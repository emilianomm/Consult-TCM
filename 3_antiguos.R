library(tidyverse)
library(rio)


# Cargamos los datos
datos_tcm <- rio::import('TCM2022.xlsx')
datos_tcm_tibble <- tibble::as_tibble(datos_tcm)
datos_tcm_tibble

# Tamaño de la muestra
n <- nrow(datos_tcm)

# Segmentamos los clientes
clientes_antiguos <- datos_tcm_tibble %>% 
  dplyr::filter(Cliente < 250000) %>% 
  dplyr::filter(Uso2022 == 1)

clientes_nuevos <- datos_tcm_tibble %>% 
  dplyr::filter(Cliente >= 250000) %>% 
  dplyr::filter(Uso2022 == 1)

clientes_antiguos
clientes_nuevos

n_antiguos <- nrow(clientes_antiguos)
n_nuevos <- nrow(clientes_nuevos)

n_antiguos ; n_nuevos

mean(clientes_antiguos$MontoAcum)
mean(clientes_nuevos$MontoAcum)

# Veamos normalidad
## Densidad
ggplot(clientes_antiguos) +
  geom_density(mapping = aes(x = MontoAcum), fill = 'salmon') +
  labs(x = 'Monto', y = 'densidad',
       title = 'Montos de compra en marzo del 2022')

## QQ-plot
ggplot(clientes_antiguos) +
  geom_qq_line(aes(sample = MontoAcum), color = "salmon", lwd = 2) +
  geom_qq(aes(sample = MontoAcum)) +
  labs(title = 'Supuesto de normalidad montos de compra', subtitle = 'Marzo 2022',
       x = 'Cuantiles teóricos', y = 'Cuantiles muestrales')

## Densidad
ggplot(clientes_nuevos) +
  geom_density(mapping = aes(x = MontoAcum), fill = 'salmon') +
  labs(x = 'Monto', y = 'densidad',
       title = 'Montos de compra en marzo del 2022')

## QQ-plot
ggplot(clientes_nuevos) +
  geom_qq_line(aes(sample = MontoAcum), color = "salmon", lwd = 2) +
  geom_qq(aes(sample = MontoAcum)) +
  labs(title = 'Supuesto de normalidad montos de compra', subtitle = 'Marzo 2022',
       x = 'Cuantiles teóricos', y = 'Cuantiles muestrales')

# H0: Distribuye normal
shapiro.test(clientes_antiguos$MontoAcum)

shapiro.test(clientes_nuevos$MontoAcum)
ks.test(scale(clientes_nuevos$MontoAcum), y = 'pnorm')

# H0: monto_nuevo - m_antiguo <= 100  vs H1: monto_nuevo - m_antiguo > 100
# Estamos entre varianzas iguales y desconocidas o diferentes y desconocidas

# Para esto, vemos un test de varianzas
## H0: sigma_antiguos/sigma_nuevos = 1 vs H1: diferentes
var.test(x = clientes_antiguos$MontoAcum,
         y = clientes_nuevos$MontoAcum,
         alternative = 'two.sided')

## Rechazamos y nos quedamos con varianzas diferentes y desconocidas
t.test(x = clientes_nuevos$MontoAcum,
       y = clientes_antiguos$MontoAcum,
       mu = 100,
       alternative = 'greater',
       var.equal = FALSE)

# No podemos rechazar H0, esto es, que el area de fidelización se los cagó