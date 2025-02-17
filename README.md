# Valuación actuarial del beneficio de cesantía

Este proyecto tiene como objetivo calcular el valor actuarial de la cesantía para una población activa utilizando modelos demográficos y financieros de la población activa.

## Estructura del Proyecto

El proyecto está organizado en los siguientes directorios y archivos:


## Descripción de los Archivos

### `R/main.R`

Este archivo contiene la función principal `calcular_cesantia` que coordina el cálculo del valor actuarial de la cesantía. También incluye la ejecución principal del script.

### `R/utils/expand_vector.R`

Contiene la función `.expand_vector` que expande un vector a una longitud específica.

### `R/utils/preparation.R`

Contiene funciones para preparar los datos de salidas y funcionarios, así como para calcular el tiempo de supervivencia en meses.

### `R/model/probs.R`

Contiene funciones para estimar probabilidades usando el modelo de riesgos proporcionales de Cox y calcular probabilidades acumuladas usando el método UDD.

### `R/model/escala_salarial.R`

Contiene funciones para calcular la escala salarial y el factor de crecimiento salarial.

### `R/model/factor_descuento.R`

Contiene la función `descuento` que calcula el factor de descuento basado en la inflación promedio.

### `R/model/flujo_financiero.R`

Contiene funciones para calcular el flujo financiero relacionado con la cesantía.

## Uso del Proyecto

### Requisitos

- R
- Paquetes de R: `tidyverse`, `readxl`, `janitor`, `openxlsx`, `survival`, `survminer`

## Estructura de los Datos

### Datos de Salidas (`salidas.xls`)

El archivo `salidas.xls` debe contener las siguientes columnas:

- `CEDULA`: Texto, cédula del empleado
- `NOMBRE`: Texto, nombre del empleado
- `TIPO DE MOVIMIENTO`: Texto, tipo de movimiento (por ejemplo, RENUNCIA)
- `RIGE`: Fecha, fecha en que rige el movimiento
- `FECHA DE INGRESO`: Fecha, fecha de ingreso del empleado
- `FECHA DE NACIMIENTO`: Fecha, fecha de nacimiento del empleado
- `SEXO`: Texto, sexo del empleado (F o M)
- `ENTIDAD`: Texto, asociación a la que pertenece el empleado
- `CATEGORIA`: Texto, categoría del puesto del empleado
- `ESQUEMA SALARIAL`: Texto, esquema salarial del empleado (PLUSES o UNICO)

### Datos de Funcionarios (`funcionarios_uq.xls`)

El archivo `funcionarios_uq.xls` debe contener las siguientes columnas:

- `CEDULA`: Texto, cédula del empleado
- `NOMBRE`: Texto, nombre del empleado
- `FECHA DE NACIMIENTO`: Fecha, fecha de nacimiento del empleado
- `EDAD`: Numérico, edad del empleado
- `FECHA DE INGRESO`: Fecha, fecha de ingreso del empleado
- `ANTIGUEDAD`: Numérico, antigüedad del empleado
- `SEXO`: Texto, sexo del empleado (F o M)
- `SALARIO`: Numérico, salario del empleado
- `ENTIDAD`: Texto, asociación a la que pertenece el empleado
- `MONTO GIRADO`: Numérico, monto girado del empleado a la asociación
- `CATEGORIA`: Texto, categoría del puesto del empleado
- `ESQUEMA SALARIAL`: Texto, esquema salarial del empleado (PLUSES o UNICO)

2. Ejecuta el script principal `R/main.R` para calcular el valor actuarial de la cesantía.