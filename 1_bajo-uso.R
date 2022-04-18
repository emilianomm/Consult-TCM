library(tidyverse)
library(rio)


# Cargamos los datos
datos_tcm <- rio::import('TCM2022.xlsx')
datos_tcm_tibble <- tibble::as_tibble(datos_tcm)
datos_tcm_tibble

# Tamaño de la muestra
n <- nrow(datos_tcm)

# Uso de la tarjeta 2021
uso_tarjeta_2021 <- 0.62

# Uso de la tarjeta 2022
## uso_tarjeta_2022 <- mean(max(datos_tcm$Uso2022, datos_tcm$UsoMarzo))
## * Mandar correo

uso_tarjeta_2022 <- mean(datos_tcm$Uso2022)
uso_tarjeta_2022

# Comparación 2021 - 2022
ggplot(tibble(uso = c(uso_tarjeta_2021, uso_tarjeta_2022),
              año = c('2021', '2022'))) +
  geom_col(mapping = aes(x = año, y = uso), fill = c('salmon', 'turquoise')) +
  labs(title = 'Uso de tarjeta TCM años 2021 y 2022') +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))

# Vemos que ha bajado empíricamente
# Podemos ver un test (binom.test)

# H0: p_2022 >= p_2021 vs H1: p_2022 < p_2021
binom.test(x = sum(datos_tcm$Uso2022),
           n = n,
           p = uso_tarjeta_2021,
           alternative = 'less')

# A una significancia 0.05 no se rechaza la hipótesis nula
# A una significancia 0.1 se rechaza la hipótesis nula
# Tiene sentido ocupar una significancia más alta

