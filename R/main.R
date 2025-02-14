source("libs.R")
source("preparation.R")
source("probs.R")

calcular_escala <- function(poblacion,umbral) {
  
  escala <- poblacion |>
    filter(tipo_de_movimiento=="ACTIVO") |>
    group_by(antiguedad_final) |>
    reframe(salario=mean(movsalario))
  
  p <- ggplot(escala, aes(antiguedad_final, salario))
  print(
    p + geom_point() +
      geom_smooth(method="lm") +
      theme_bw()
  )
  
  salario_inicial <- 535650 # salario de aux. de fiscalizador
  lm_escala <- lm(I(salario-salario_inicial) ~ 0+antiguedad_final, data=escala)
  print(lm_escala)
  
  escala_fit <- predict(lm_escala,newdata = tibble(antiguedad_final = 1:(2*12*45)))
  escala_fit <- escala_fit + salario_inicial
  
  s_tx <- function(t,x) {
    
    if (x==0) {
      sx <- salario_inicial
    } else {
      sx <- escala_fit[x]
    }
    
    escala <- escala_fit[x+t]/sx
    names(escala) <- NULL
    
    return(escala)
  }
  
  return(s_tx)
}

.expand_vector <- function(x, n, fill_value=0, use_last=F) {
  
  len <- length(x)
  
  if(use_last) {
    fill_value <- x[len]
    message("fill_value coerced to last value in x")
  }
  
  if(len < n) {
    return(c(x, rep(fill_value, n-length(x))))
  }
  return(x)
}

# retorna P(ser cesado en el mes t | antiguedad, sexo)
prob_pago <- function(antiguedad, sexo=c("F","M"), umbral, probs) {
  
  p <- probs |>
    filter(t>antiguedad)
  
  sexo <- tolower(sexo)
  col_cese <- paste0("q",sexo,"_diff")
  col_activo <- paste0("p",sexo)
  
  
  .expand_vector(p[[col_cese]]/(p[[col_cese]][1] + p[[col_activo]][1]),
                 umbral)
  
}

# retorna el factor de descuento
descuento <- function(t) {
  inflacion <- 0.045
  1/(1+inflacion)^(t/12)
}

# retorna el monto de cesantia unitario
monto_cesantia <- function(antiguedad,umbral) {
  
  dias_cese <- data.frame(
    cortes = 12*c(0, 1/4, 1/2, 1:6, 10:13),
    dias_pagados = c(0, 7, 14, 19.5, 20, 20.5, 21, 21.5, 22, 21.5, 21, 20.5, 20)
  )
  
  periodo <- max(antiguedad,1):umbral
  
  dias <- with(dias_cese,
               approx(cortes,
                      dias_pagados,
                      xout=periodo,
                      method="constant"),
               )$y
  dias[is.na(dias)] <- 20
  
  ans <- .expand_vector(
    1/30 * if_else(antiguedad > 12, pmin(antiguedad %/% 12, 8), 1) * dias,
    umbral,
    use_last = T
  )
  
  return(ans)
}

calcular_cesantia <- function(poblacion, probs, umbral) {
  
  escala <- calcular_escala(poblacion,umbral) # retorna s_(t+x)/s_x
  
  activos <- poblacion |>
    filter(tipo_de_movimiento=="ACTIVO") |>
    mutate(
      monto_girado = replace_na(monto_girado, 0)
    )
  
  edades_mat <- do.call(
    rbind,
    map(
      activos$edad_inicial,
      \(x) x+(1:umbral)
    )
  )
  
  prob_pago_mat <- do.call(
    rbind,
    map2(activos$antiguedad_final,
         activos$sexo,
         \(ant,sexo) prob_pago(ant,sexo,umbral,probs))
  )
  
  salarios_mat <- do.call(
    rbind,
    map2(activos$antiguedad_final,
         activos$movsalario,
         \(ant, sal) sal * escala(1:umbral, ant)
    )
  )
  
  dias_cesantia_mat <- do.call(
    rbind,
    map(activos$antiguedad_final,
        \(ant) monto_cesantia(ant, umbral))
  )
  
  montos_cesantia_mat <- salarios_mat * dias_cesantia_mat
  
  montos_girados_mat <- 0.04 * salarios_mat # sumar aporte de patrono (7%)
  montos_girados_mat[is.na(activos$entidad),] <- 0
  montos_girados_mat <- t(apply(montos_girados_mat, 1, cumsum))
  montos_girados_mat <- montos_girados_mat + activos$monto_girado
  
  descuento_mat <- descuento(1:umbral)
  
  valor_actuarial <- t(descuento_mat * t(
    (edades_mat<=65*12)*pmax(montos_cesantia_mat-montos_girados_mat, 0) * prob_pago_mat))
  
  return(valor_actuarial)
}

valor_actuarial <- calcular_cesantia(poblacion=poblacion, probs=probs, umbral=420)

resultado <- activos |>
  mutate(valor_actuarial = rowSums(valor_actuarial))

openxlsx::write.xlsx(resultado, "datos/output/resultado.xlsx")

