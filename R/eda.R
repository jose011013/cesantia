funcionarios <- read_xls("datos/funcionarios ultima quincena.xls")

funcionarios |>
  ggplot(aes(as_factor(ANTIGUEDAD), MOVSALARIO, fill = `ESQUEMA SALARIAL`)) +
  geom_boxplot(drop=FALSE) +
  geom_point() +
  scale_y_log10()

surv_antiguedad <- with(poblacion, Surv(antiguedad_final, cod_evento))

fit_antiguedad <- survfit(surv_antiguedad~1, data = poblacion)

fit_antiguedad$transitions

fit_antiguedad %>%
  ggcompetingrisks()

survfit(
  surv_antiguedad~tipo_de_movimiento,
  data = poblacion
) %>%
  ggcompetingrisks()

cox_antiguedad <- coxph(surv_antiguedad~edad_final+sexo, data=poblacion, id=cedula)

hist(
  log10(resultado$valor_actuarial),
  main = NULL,
  xlab = "valor actuarial",
  ylab = "frecuencia"
)

crear_ca <- function(res) {
  X <- res |>
    mutate(asociado = 1*!is.na(entidad)) |>
    select(asociado,
           edad,
           antiguedad,
           movsalario,
           valor_actuarial
    ) |>
    as.matrix()
  rownames(X) <- resultado$cedula
  FactoMineR::CA(X)
}
