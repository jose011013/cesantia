library(tidyverse)

poblacion <- read_rds("data/processed/poblacion.RDS")
poblacion_activa <- poblacion |>
  filter(cod_evento == 0)

#' Calcular el promedio salarial de la población activa
#'
#' @param poblacion_activa Un tibble con los datos de la población activa
#' @return Un tibble con los promedios salariales por antigüedad
.mediana_salarial <- function(poblacion_activa) {
  stopifnot(all(poblacion_activa$cod_evento=="0"))
  promedio_salarial <- poblacion_activa |>
    group_by(antiguedad_final) |>
    reframe(salario = median(salario))
  return(promedio_salarial)
}

#' Estimar la escala salarial
#'
#' @param poblacion_activa Un tibble con los datos de la población activa
#' @param salario_base El salario base inicial
#' @return Una lista con el salario base y el modelo lineal ajustado
.estimar_escala <- function(poblacion_activa, salario_base) {
  mediana_salarial <- .mediana_salarial(poblacion_activa)
  return(list(
    salario_base = salario_base,
    lm = lm(I(salario - salario_base) ~ 0 + antiguedad_final, data = mediana_salarial)
  ))
}

#' Calcular el factor de crecimiento salarial
#'
#' @param modelo_escala Un modelo de escala salarial
#' @param antiguedad_i La antigüedad inicial
#' @param antiguedad_f La antigüedad final
#' @return El factor de crecimiento salarial
factor_crecimiento_salarial <- function(modelo_escala, antiguedad_i, antiguedad_f) {
  salario_base <- modelo_escala$salario_base
  sal_i <- salario_base + unname(predict(
    modelo_escala$lm,
    newdata = tibble(antiguedad_final = antiguedad_i)
  ))
  sal_f <- salario_base + unname(predict(
    modelo_escala$lm,
    newdata = tibble(antiguedad_final = (antiguedad_i+1):antiguedad_f)
  ))
  return(sal_f / sal_i)
}

# Ejecución principal
modelo_escala <- .estimar_escala(poblacion_activa, salario_base=535650)