library(tidyverse)
library(survival)
library(survminer)
library(readxl)

poblacion <- read_excel("data/processed/poblacion.xlsx")

estimarProbs <- function(pob) {
  surv <- with(pob, Surv(antiguedad_final, cod_evento))
  cox_mod <- coxph(surv ~ sexo, data = pob, id = cedula)
  nivs <- unique(pob$sexo)
  cox_fit <- survfit(cox_mod,
    newdata = tibble(sexo=nivs)
  )
  p <- with(cox_fit, pstate)
  p_activo <- as_tibble(p[,,1])
  p_cese <- as_tibble(p[,,2])
  colnames(p_activo) <- nivs
  colnames(p_cese) <- nivs
  return(with(cox_fit, list(t=time, cese=p_cese, activo=p_activo)))
}

tiempos <- function(probs, estado) {
  
  tt <- cbind(probs[["t"]], probs[[estado]]) |>
    as_tibble() |>
    rename(t=1) |>
    distinct(`F`, M, .keep_all = T)
  
  return(tt)
}

probs_udd <- function(tiempos, umbral) {
  
  tt <- 0:umbral
  probs_F <- with(tiempos, approx(t, `F`, xout=tt))$y
  probs_M <- with(tiempos, approx(t, M, xout=tt))$y
  
  return(
    tibble(t=tt, f=probs_F, m=probs_M)
  )

}

fit_probs <- poblacion |>
  estimarProbs()

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

probs |>
  pivot_longer(-1, names_to = "curva", values_to = "prob") |>
  ggplot(aes(t, prob, color = curva)) +
  geom_line() +
  theme_bw()
