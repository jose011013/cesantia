library(tidyverse)
library(survival)
library(survminer)
library(readxl)

poblacion <- read_rds("data/processed/poblacion.RDS")

estimar_probs <- function(pob) {

  # estimador de aalen-johansen para las salidas
  aj_salidas <- Surv(
    pob$antiguedad_final,
    pob$cod_evento
  )
  
  # modelo de cox de multiples estados
  cox_mod <- coxph(
    aj_poblacion ~ sexo,
    data = pob, 
    id = cedula
  )
  
  # estimacion de probabilidades por sexo
  sexo <- c("f", "m")
  cox_fit <- survfit(
    cox_mod,
    newdata = tibble(sexo=sexo)
  )
  
  p <- cox_fit$pstate # probabilidades de salida por causa y sexo
  
  p_activo <- p[,,1]
  colnames(p_activo) <- sexo
  p_activo <- as_tibble(p_activo)
  
  p_cese <- p[,,2]
  colnames(p_cese) <- sexo
  p_cese <- as_tibble(p_cese)
  
  res <- list(
    t = cox_fit$time, # tiempos de eventos
    cese = p_cese, # probabilidades de riesgos en competencia acumuladas de salida por cese (antes del tiempo t)
    activo = p_activo # probabilidades puntuales (al tiempo t) de seguir activo
  )
  
  return(res)
}

tiempos_evento <- function(probs, estado) {
  
  tt <- cbind(probs[["t"]], probs[[estado]]) |>
    as_tibble() |>
    rename(t=1) |>
    distinct(f, m, .keep_all = T)
  
  return(tt)
}

probs_udd <- function(t_evento, umbral) {
  
  tt <- 0:umbral
  probs_f <- approx(t_evento$t, t_evento$f, xout=tt)$y
  probs_m <- approx(t_evento$t, t_evento$m, xout=tt)$y
  
  return(tibble(t=tt, f=probs_f, m=probs_m))
}

fit_probs <- poblacion |>
  estimar_probs()

p_cese <- fit_probs |>
  tiempos(estado="cese") |>
  probs_udd(umbral=420) |>
  rename(qf=f, qm=m)

p_activo <- fit_probs |>
  tiempos(estado="activo") |>
  probs_udd(umbral=420) |>
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
