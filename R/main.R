source("R/utils/expand_vector.R")
source("R/utils/preparation.R")
source("R/model/probs.R")
source("R/model/escala_salarial.R")
source("R/model/factor_descuento.R")
source("R/model/flujo_financiero.R")


calcular_cesantia <- function(poblacion_activa, probs, umbral) {
  
  edades_mat <- do.call(
    rbind,
    map(poblacion_activa$edad_inicial, \(x) x+(1:umbral))
  )
  
  prob_pago_mat <- do.call(
    rbind,
    map2(
      poblacion_activa$antiguedad_final,
      poblacion_activa$sexo,
      \(ant,sexo) prob_cese_surv(probs,umbral,ant+1,ant+umbral,sexo)
    )
  )
  
  salarios_mat <- do.call(
    rbind,
    map2(
      poblacion_activa$antiguedad_final,
      poblacion_activa$salario,
      \(ant, sal) sal * factor_crecimiento_salarial(modelo_escala, ant,ant+umbral)
    )
  )
  
  factores_antiguedad <- do.call(
    rbind,
    map(
      poblacion_activa$antiguedad_final,
      \(ant) factor_antiguedad_cesantia(ant+1,ant+umbral, umbral)
    )
  )
  
  montos_cesantia_mat <- pmin(salarios_mat * factores_antiguedad, 3302076) # salario de gerente de área
  
  montos_girados_mat <- 0.07 * salarios_mat # sumar aporte de patrono (4% obrero + 3% patrono)
  montos_girados_mat[is.na(poblacion_activa$entidad),] <- 0
  montos_girados_mat <- t(apply(montos_girados_mat, 1, cumsum))
  montos_girados_mat <- montos_girados_mat + poblacion_activa$monto_girado
  
  descuento_mat <- factor_descuento(1,umbral)
  
  valor_actuarial <- t(descuento_mat * t(
    (edades_mat<=65*12)*pmax(montos_cesantia_mat-montos_girados_mat, 0) * prob_pago_mat))
  
  return(valor_actuarial)
}

valor_actuarial <- calcular_cesantia(poblacion=poblacion_activa, probs=tabla_probs, umbral=420)

resultado <- poblacion_activa |>
  mutate(valor_actuarial = rowSums(valor_actuarial))

message(paste("El valor actuarial total es:", sum(valor_actuarial)), " CRC")

openxlsx::write.xlsx(resultado, "results/resultado.xlsx")

