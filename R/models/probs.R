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
.extraer_probs <- function(model, estado) {
  p <- model$pstate[,,estado]
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

.identificar_tiempos_evento <- function(probs, estado) {
  tt <- distinct(probs[[estado]], f, m, .keep_all = T)
  return(tt)
}

probs_udd <- function(probs, estado, umbral) {
  p <- .identificar_tiempos_evento(probs, estado)
  tt <- 0:umbral
  probs_f <- approx(t_evento$t, t_evento$f, xout=tt)$y
  probs_m <- approx(t_evento$t, t_evento$m, xout=tt)$y
  return(tibble(t=tt, f=probs_f, m=probs_m))
}

fit_probs <- probs(poblacion)

p_cese <- fit_probs |>
  tiempos_evento(estado="cese") |>
  probs_udd(umbral=420) |>
  rename(qf=f, qm=m)

p_activo <- fit_probs |>
  tiempos_evento(estado="activo") |>
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
