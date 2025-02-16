library(tidyverse)
library(survival)
library(survminer)
library(readxl)

# Cargar los datos de la población
poblacion <- read_rds("data/processed/poblacion.RDS")
umbral <- 420 # 35 años, el máximo

#' Estimar probabilidades usando el modelo de riesgos proporcionales de Cox
#'
#' @param pob Un data frame que contiene los datos de la población
#' @return Una lista que contiene los puntos de tiempo y las probabilidades de estado
.estimar_probs <- function(pob) {
  aj_salidas <- Surv(pob$antiguedad_final, pob$cod_evento)
  cox_mod <- coxph(aj_salidas ~ sexo, data = pob, id = cedula)
  cox_fit <- survfit(cox_mod, newdata = tibble(sexo = c("f", "m")))
  
  return(list(
    t = cox_fit$time,
    pstate = cox_fit$pstate
  ))
}

#' Extraer probabilidades para un estado específico
#'
#' @param model Una lista que contiene la salida del modelo de Cox
#' @param i_estado Un entero que indica el índice del estado (1: activo, 2: cese, 3: despido, 4: pensión, 5: renuncia)
#' @return Un tibble con las probabilidades para el estado especificado
.extraer_probs <- function(model, i_estado) {
  p <- model$pstate[,,i_estado]
  colnames(p) <- c("f", "m")
  return(
    as_tibble(p) |>
      mutate(t = model$t, .before = 1)
  )
}

#' Calcular probabilidades de estado usando el modelo de Cox
#'
#' @param pob Un data frame que contiene los datos de la población
#' @return Una lista de tibbles con las probabilidades para cada estado
probs_cox <- function(pob) {
  model <- .estimar_probs(pob)
  res <- lapply(1:5, \(i) .extraer_probs(model, i))
  names(res) <- c("activo", "cese", "despido", "pension", "renuncia")
  res <- append(res, list(t=model$t), after = 0)
  return(res)
}

#' Filtrar tiempos de eventos para eliminar duplicados
#'
#' @param probs Una lista de tibbles con las probabilidades para cada estado
#' @param estado Una cadena que indica el estado a filtrar
#' @return Un tibble con tiempos de eventos distintos
.filtrar_tiempos_evento <- function(probs, estado) {
  tt <- distinct(probs[[estado]], f, m, .keep_all = T)
  return(tt)
}

#' Calcular probabilidades acumuladas usando el método UDD
#'
#' @param probs Una lista de tibbles con las probabilidades para cada estado
#' @param estado Una cadena que indica el estado para calcular las probabilidades
#' @param umbral Un entero que indica el umbral
#' @return Un tibble con las probabilidades acumuladas
probs_acumuladas_udd <- function(probs, estado, umbral) {
  p <- .filtrar_tiempos_evento(probs, estado)
  tt <- 0:umbral
  # Interpolación lineal a la CDF === UDD
  probs_f <- approx(p$t, p$f, xout=tt)$y
  probs_m <- approx(p$t, p$m, xout=tt)$y 
  return(tibble(t=tt, f=probs_f, m=probs_m))
}

#' Calcular la función de masa de probabilidad usando el método UDD
#'
#' @param model_probs Una lista de tibbles con las probabilidades para cada estado
#' @param umbral Un entero que indica el umbral
#' @return Un tibble con la función de masa de probabilidad
pmf_udd <- function(model_probs, umbral) {
  probs_acumuladas_cese <- probs_acumuladas_udd(
    model_probs,
    "cese",
    umbral
  ) |>
    rename(qf=f, qm=m)
  probs_acumuladas_activo <- probs_acumuladas_udd(
    model_probs,
    "activo",
    umbral
  ) |>
    rename(pf=f, pm=m)
  
  tabla_probs <- probs_acumuladas_cese |>
    inner_join(probs_acumuladas_activo, by = "t") |>
    mutate(
      qf_diff = c(0, diff(qf)),
      qm_diff = c(0, diff(qm))
    )
  
  return(tabla_probs)
}

# retorna P(ser cesado en el mes t | antiguedad, sexo)
prob_cese_surv <- function(probs, umbral, antiguedad_i, antiguedad_f, sexo) {

  stopifnot(sexo %in% c("f", "m"))
  stopifnot(antiguedad_i <= antiguedad_f)
  
  p <- probs |>
    filter(antiguedad_i<=t, t<=min(antiguedad_f, umbral))
  
  cese <- paste0("q",sexo,"_diff")
  activo <- paste0("p",sexo)
  
  probs_cese_sobrevivientes <- p[[cese]]/(p[[cese]][1] + p[[activo]][1])

  return(.expand_vector(probs_cese_sobrevivientes,umbral))
}

# Ejecución principal
model_probs <- probs_cox(poblacion)
tabla_probs <- pmf_udd(model_probs, umbral)

# Guardar los resultados en archivos
write.xlsx(tabla_probs, "data/processed/probs_activo_cese.xlsx")
write_rds(tabla_probs, "data/processed/probs_activo_cese.RDS")

# Graficar los resultados
g_probs <- tabla_probs |>
  pivot_longer(-1, names_to = "curva", values_to = "prob") |>
  ggplot(aes(t, prob, color = curva)) +
  geom_line() +
  theme_bw()

plotly::ggplotly(g_probs)
