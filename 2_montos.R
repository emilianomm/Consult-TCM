library(tidyverse)
library(rio)


# Cargamos los datos
datos_tcm <- rio::import('TCM2022.xlsx')
datos_tcm_tibble <- tibble::as_tibble(datos_tcm)
datos_tcm_tibble

# Tamaño de la muestra
n <- nrow(datos_tcm)

# Monto compras mensuales marzo 2021
monto_promedio_marzo_2021 <- 400

## H0: monto_2022 <= monto_2021 vs H1: monto_2022 > monto_2021

# Quitamos los clientes que no gastaron
datos_tcm_compras <- datos_tcm_tibble %>% 
  dplyr::filter(UsoMarzo == 1)

# Monto compras mensuales marzo 2022
monto_promedio_marzo_2022 <- datos_tcm_compras %>% 
  dplyr::summarise(media = mean(MontoMarzo)) %>% 
  as.numeric()
monto_promedio_marzo_2022

# Queremos ocupar un t.test
# Primero tenemos que ver normalidad
## Densidad
ggplot(datos_tcm_compras) +
  geom_density(mapping = aes(x = MontoMarzo), fill = 'salmon') +
  labs(x = 'Monto', y = 'densidad',
       title = 'Montos de compra en marzo del 2022')

## QQ-plot
ggplot(datos_tcm_compras) +
  geom_qq_line(aes(sample = MontoMarzo), color = "salmon", lwd = 2) +
  geom_qq(aes(sample = MontoMarzo)) +
  labs(title = 'Supuesto de normalidad montos de compra', subtitle = 'Marzo 2022',
       x = 'Cuantiles teóricos', y = 'Cuantiles muestrales')

# Ahora vemos un test formal
## H0: viene de una población normal
shapiro.test(datos_tcm_compras$MontoMarzo)
# No podemos ocupar un test t

## Probamos igual con el t.test
t.test(x = datos_tcm_compras$MontoMarzo,
       mu = monto_promedio_marzo_2021,
       alternative = 'greater')

## Buscar un test no paramétrico para medias
wilcox.test(x = datos_tcm_compras$MontoMarzo,
            mu = monto_promedio_marzo_2021,
            alternative = 'greater')

## Se rechaza H0 (considerando alpha = 0.05), y por lo tanto aceptamos que el monto
## del 2022 es superior al del 2021
