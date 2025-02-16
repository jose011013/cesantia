#' Calcular el factor de descuento
#'
#' Esta función calcula el factor de descuento basado en la inflación promedio.
#'
#' @param tiempo_i Un entero que indica el tiempo inicial en meses
#' @param tiempo_f Un entero que indica el tiempo final en meses
#' @return Un vector con los factores de descuento para cada mes en el rango especificado
#' @examples
#' factor_descuento(0, 12) # Calcula el factor de descuento para un año
factor_descuento <- function(tiempo_i, tiempo_f) {
  inflacion <- 0.045 # promedio de la banda de inflacion del BC
  return(1/(1+inflacion)^(tiempo_i:tiempo_f/12))
}