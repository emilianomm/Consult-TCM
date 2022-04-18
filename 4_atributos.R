library(tidyverse)
library(rio)


# Cargamos los datos
datos_tcm <- rio::import('TCM2022.xlsx')
datos_tcm_tibble <- tibble::as_tibble(datos_tcm)
datos_tcm_tibble

# Tamaño de la muestra
n <- nrow(datos_tcm)

# Datos agrupados 
# 0: bajo 36, 1: entre 36 y 54, 2: sobre 54
datos_tcm_tibble <- datos_tcm_tibble %>% 
  dplyr::mutate(edad_cat = case_when(
    Edad <= 35 ~ 0,
    between(Edad, 36, 54) ~ 1,
    Edad >= 55 ~ 2
  ))

head(datos_tcm_tibble$Edad)
head(datos_tcm_tibble$edad_cat)
