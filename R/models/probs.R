library(tidyverse)
library(survival)
library(survminer)
library(readxl)

poblacion <- read_rds("data/processed/poblacion.RDS")

.estimar_probs <- function(pob) {
  aj_salidas <- Surv(pob$antiguedad_final, pob$cod_evento)
  cox_mod <- coxph(aj_salidas ~ sexo, data = pob, id = cedula)
  cox_fit <- survfit(cox_mod, newdata = tibble(sexo = c("f", "m")))
  
  return(list(
    t = cox_fit$time,
    pstate = cox_fit$pstate
  ))
}

# i = 1: activo, i = 2: cese, i = 3: despido, i = 4: pension, i = 5: renuncia
.extraer_probs <- function(model, i_estado) {
  p <- model$pstate[,,i_estado]
  colnames(p) <- c("f", "m")
  return(
    as_tibble(p) |>
      mutate(t = model$t, .before = 1)
  )
}

probs <- function(pob) {
  model <- .estimar_probs(pob)
  res <- lapply(1:5, \(i) .extraer_probs(model, i))
  names(res) <- c("activo", "cese", "despido", "pension", "renuncia")
  res <- append(res, list(t=model$t), after = 0)
  return(res)
}

.filtrar_tiempos_evento <- function(probs, estado) {
  tt <- distinct(probs[[estado]], f, m, .keep_all = T)
  return(tt)
}

probs_acumuladas_udd <- function(probs, estado, umbral) {
  p <- .filtrar_tiempos_evento(probs, estado)
  tt <- 0:umbral
  # interpolacion lineal a la CDF === udd
  probs_f <- approx(p$t, p$f, xout=tt)$y
  probs_m <- approx(p$t, p$m, xout=tt)$y 
  return(tibble(t=tt, f=probs_f, m=probs_m))
}

umbral <- 420
model_probs <- probs(poblacion)

pmf_cese <- probs_acumuladas_udd(model_probs, "cese", umbral) |>
  rename(qf=f, qm=m)

pmf_activo <- probs_acumuladas_udd(model_probs, "activo", umbral) |>
  rename(pf=f, pm=m)

probs <- p_cese |>
  left_join(p_activo) |>
  mutate(qf_diff = c(NA, diff(qf)),
         qm_diff = c(NA, diff(qm)))

g_probs <- probs |>
  pivot_longer(-1, names_to = "curva", values_to = "prob") |>
  ggplot(aes(t, prob, color = curva)) +
  geom_line() +
  theme_bw()

plotly::ggplotly(g_probs)
