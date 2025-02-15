library(tidyverse)
library(readxl)
library(janitor)
library(openxlsx)

#' Preparar datos de salidas
#'
#' @param path La ruta al archivo Excel que contiene los datos de salidas
#' @return Un tibble con los datos de salidas preparados
preparar_salidas <- function(path) {
  
  col_types <- list(
    CEDULA = "text",
    NOMBRE = "text",
    `TIPO DE MOVIMIENTO`= "text",
    RIGE = "date",
    `FECHA DE INGRESO` = "date",
    `FECHA DE NACIMIENTO` = "date",
    SEXO = "text",
    ENTIDAD = "text",
    CATEGORIA = "text",
    `ESQUEMA SALARIAL`= "text"
  )
  
  salidas <- read_excel(
    path,
    col_types = unname(unlist(col_types))
  )
  
  message(paste("columnas en ", path, ":", sep = ""))
  message(paste(
    colnames(salidas),
    collapse = ", "
  ))
  message("columnas esperadas (en orden):")
  message(paste(
    names(col_types),
    collapse = ", "
  ))
  stopifnot(names(col_types) == colnames(salidas))
  
  salidas <- salidas |>
    clean_names() |>
    mutate(
      cedula = str_remove(cedula, "^0"), # eliminar los 0 al inicio de las cedulas
      tipo_de_movimiento = if_else( # unir todas las descripciones de ceses bajo "CESES"
        startsWith(tipo_de_movimiento, "CESE"),
        "CESE",
        tipo_de_movimiento
      ),
      sexo = tolower(sexo) # convierte F, M a f, m respectivamente
    )
  
  return(salidas)
}

#' Preparar datos de funcionarios
#'
#' @param path La ruta al archivo Excel que contiene los datos de funcionarios
#' @return Un tibble con los datos de funcionarios preparados
preparar_funcionarios <- function(path) {
  
  col_types <- list(
    CEDULA = "text",
    NOMBRE = "text",
    `FECHA DE NACIMIENTO`= "date",
    EDAD = "numeric",
    `FECHA DE INGRESO` = "date",
    ANTIGUEDAD = "numeric",
    SEXO = "text",
    SALARIO = "numeric",
    ENTIDAD = "text",
    `MONTO GIRADO` = "numeric",
    CATEGORIA = "text",
    `ESQUEMA SALARIAL` = "text"
  )
  
  funcionarios <- read_excel(
    path,
    col_types = unname(unlist(col_types))
  )
  
  message(paste("columnas en ", path, ":", sep = ""))
  message(paste(
    colnames(funcionarios),
    collapse = ", "
  ))
  message("columnas esperadas (en orden):")
  message(paste(
    names(col_types),
    collapse = ", "
  ))
  stopifnot(names(col_types) == colnames(funcionarios))
  
  funcionarios <- funcionarios |>
    clean_names() |>
    mutate(
      cedula = str_remove(cedula, "^0"), # eliminar los 0 al inicio de las cedulas
      monto_girado = replace_na(monto_girado, 0),
      sexo = tolower(sexo) # convierte F, M a f, m respectivamente
    )
  
  return(funcionarios)
}

#' Calcular el tiempo de supervivencia en meses
#'
#' @param inicio La fecha de inicio
#' @param final La fecha final
#' @param fecha_corte La fecha de corte
#' @return El tiempo de supervivencia en meses
tiempo_sobrevivencia_meses <- function(inicio, final, fecha_corte) {
  fin <- if_else(
    is.na(final),
    fecha_corte,
    final
  )
  # en caso de censura por la derecha (activo), la fecha de corte corresponde a la ultima observacion
  
  return(interval(inicio, fin) %/% months(1))
}

#' Preparar datos de la población
#'
#' @param path_salidas La ruta al archivo Excel que contiene los datos de salidas
#' @param path_funcionarios La ruta al archivo Excel que contiene los datos de funcionarios
#' @param fecha_corte La fecha de corte
#' @return Un tibble con los datos de la población preparados
preparar_poblacion <- function(path_salidas, path_funcionarios, fecha_corte) {
  
  salidas <- preparar_salidas(path_salidas) # salidas de la cgr
  funcionarios <- preparar_funcionarios(path_funcionarios) # activos
  
  fecha_corte <- ymd(fecha_corte)
  
  poblacion <- bind_rows(salidas, funcionarios)
  
  poblacion <- poblacion |>
    mutate( # agrega la descripción de activo
      tipo_de_movimiento = replace_na(
        tipo_de_movimiento,
        "ACTIVO"
      )
    ) |>
    mutate( # codifica el estado para el modelo de sobrevivencia
      cod_evento = factor(
        tipo_de_movimiento,
        levels = c(
          "ACTIVO",
          "CESE",
          "DESPIDO POR CAUSA",
          "Pensión",
          "RENUNCIA"
        ),
        labels = 0:4 # es importante que ACTIVO corresponda al nivel base (0)
      )
    )
  
  poblacion <- poblacion |>
    mutate(
      antiguedad_final = tiempo_sobrevivencia_meses(
        fecha_de_ingreso,
        rige,
        fecha_corte
      ),
      edad_inicial = tiempo_sobrevivencia_meses(
        fecha_de_nacimiento,
        fecha_de_ingreso,
        fecha_corte
      )
    )
  
  # eliminar individuos que entraron posterior a la fecha de corte
  poblacion <- poblacion |>
    filter(antiguedad_final >= 0, !is.na(edad_inicial))
  
  return(poblacion)
}

# Ejecución principal
path_salidas <- "data/raw/salidas.xls"
path_funcionarios <- "data/raw/funcionarios_uq.xls"
fecha_corte <- "2024/12/31"

poblacion <- preparar_poblacion(path_salidas, path_funcionarios, fecha_corte)

# Guardar los resultados en archivos
write.xlsx(poblacion, "data/processed/poblacion.xlsx", rowNames = FALSE)
write_rds(poblacion, "data/processed/poblacion.RDS")