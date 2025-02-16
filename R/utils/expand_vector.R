#' Expandir un vector a una longitud específica
#'
#' Esta función expande un vector a una longitud específica, rellenando con un valor de relleno o el último valor del vector.
#'
#' @param x Un vector que se desea expandir
#' @param n Un entero que indica la longitud deseada del vector
#' @param fill_value El valor con el que se rellenará el vector si es más corto que `n` (por defecto es 0)
#' @param use_last Un booleano que indica si se debe usar el último valor del vector `x` como valor de relleno (por defecto es FALSE)
#' @return Un vector de longitud `n`
#' @examples
#' .expand_vector(c(1, 2, 3), 5) # Expande el vector a longitud 5 con valor de relleno 0
#' .expand_vector(c(1, 2, 3), 5, fill_value = 9) # Expande el vector a longitud 5 con valor de relleno 9
#' .expand_vector(c(1, 2, 3), 5, use_last = TRUE) # Expande el vector a longitud 5 usando el último valor del vector como relleno
.expand_vector <- function(x, n, fill_value=0, use_last=F) {
  len <- length(x)
  if(use_last) {
    fill_value <- x[len]
  }
  if(len < n) {
    return(c(x, rep(fill_value, n-length(x))))
  }
  return(x[1:n])
}