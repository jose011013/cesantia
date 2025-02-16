# retorna el monto de cesantia unitario

factor_antiguedad_cesantia <- function(antiguedad_i, antiguedad_f, umbral) {
  
  dias_cese <- data.frame(
    cortes = 12*c(0, 1/4, 1/2, 1:6, 10:13),
    dias_pagados = c(0, 7, 14, 19.5, 20, 20.5, 21, 21.5, 22, 21.5, 21, 20.5, 20)
  )
  
  periodo <- antiguedad_i:min(antiguedad_f, umbral)
  
  dias <- approx(
    dias_cese$cortes,
    dias_cese$dias_pagados,
    xout=periodo,
    method="constant")$y
  
  dias[is.na(dias)] <- 20
  
  ans <- .expand_vector(
    1/30 * if_else(periodo > 12, pmin(periodo %/% 12, 8), 1) * dias,
    umbral,
    use_last = T
  )
  
  return(ans)
}