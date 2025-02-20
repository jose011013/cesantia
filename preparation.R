prepararCeses <- function(path) {
  ceses <- read_excel(path,
    col_types = c(
      "text", "text", "text", "date", "date",
      "date", "text", "text", "numeric", "text"
    )
  ) %>%
    mutate(CEDULA = str_remove(CEDULA, "^0")) %>%
    clean_names()
  return(ceses)
}

prepararFuncionarios <- function(path) {
  funcionarios <- read_excel(path) %>%
    mutate(
      CEDULA = str_remove(CEDULA, "^0")
    ) %>%
    clean_names()
  return(funcionarios)
}

prepararPoblacion <- function(path_ceses, path_funcionarios, fecha_corte) {
  
  ceses <- prepararCeses(path_ceses) # salidas de la cgr
  funcionarios <- prepararFuncionarios(path_funcionarios) # activos

  fecha_corte <- ymd(fecha_corte)

  poblacion <- bind_rows(ceses, funcionarios)

  surv_meses <- function(inicio, final, fecha_corte) {
    f <- if_else(
      is.na(final),
      fecha_corte,
      final
    ) # en caso de censura por la derecha
    interval(inicio, f) %/% months(1)
  }

  poblacion_add_movimientos <- poblacion %>%
    mutate(
      tipo_de_movimiento = if_else(
        startsWith(tipo_de_movimiento, "CESE"),
        "CESE",
        tipo_de_movimiento
      )
    ) %>%
    replace_na(list(tipo_de_movimiento = "ACTIVO")) %>%
    mutate(cod_evento = factor(tipo_de_movimiento, labels = 0:4))

  poblacion_add_sobrevivencia <- poblacion_add_movimientos %>%
    mutate(
      antiguedad_final = surv_meses(fecha_de_ingreso, rige, fecha_corte),
      edad_inicial = surv_meses(fecha_de_nacimiento, fecha_de_ingreso, fecha_corte)
    )

  poblacion_rm_invalidos <- poblacion_add_sobrevivencia %>%
    filter(
      antiguedad_final >= 0,
      !is.na(edad_inicial)
    )

  return(poblacion_rm_invalidos)
}

agregar_info_asociacion <- function(
    poblacion,
    path_info = 'datos/funcionarios ultima quincena 19-12-2024.xls'
) {
  info <- readxl::read_xls(
    path_info
  )
  info <- info %>%
    mutate(
      CEDULA = str_remove(CEDULA, "^0")
    ) %>%
    select(
      CEDULA,
      `MONTO GIRADO`
    )
  return(
    poblacion %>%
      left_join(
        info,
        join_by(
          cedula==CEDULA
        )
      ) %>%
      rename(monto_girado = `MONTO GIRADO`)
  )
}

path_ceses <- "datos/ceses 2004 a la fecha.xls"
path_funcionarios <- "datos/funcionarios ultima quincena.xls"
fecha_corte <- "2024/11/01"
poblacion <- prepararPoblacion(path_ceses, path_funcionarios, fecha_corte) |>
  agregar_info_asociacion()
