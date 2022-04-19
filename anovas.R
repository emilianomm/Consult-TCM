

# frecuencia de uso 

ggplot(datos_tcm_tibble_edadcat) +
  aes(x = Uso2022) +
  geom_density(adjust = 1L, fill = "salmon") +
  theme_minimal() +
  facet_wrap(vars(edad_cat))

ggplot(datos_tcm_tibble_edadcat) +
  aes(x = Uso2022) +
  geom_density(adjust = 1L, fill = "salmon") +
  theme_minimal() +
  facet_wrap(vars(`Reg (1=RM)`))

ggplot(datos_tcm_tibble_edadcat) +
  aes(x = Uso2022) +
  geom_density(adjust = 1L, fill = "salmon") +
  theme_minimal() +
  facet_wrap(vars(`Sex(1=Fem)`))


anova1 = aov(datos_tcm_tibble_edadcat$Uso2022~datos_tcm_tibble_edadcat$edad_cat)
summary(anova1)

anova2 = aov(datos_tcm_tibble_edadcat$Uso2022~datos_tcm_tibble_edadcat$`Sex(1=Fem)`)
summary(anova2)

anova3 = aov(datos_tcm_tibble_edadcat$Uso2022~datos_tcm_tibble_edadcat$`Reg (1=RM)`)
summary(anova3)

# el valor-p de edad_cat es menor a 0.05 tenemos evidencia  


# montos transados

#limpiamos usarios que no usaron tarjeta

datos_tcm_tibble_edadcat2 = datos_tcm_tibble_edadcat %>% filter(Uso2022==1)



ggplot(datos_tcm_tibble_edadcat2) +
  aes(x = MontoAcum) +
  geom_density(adjust = 1L, fill = "salmon") +
  theme_minimal() +
  facet_wrap(vars(edad_cat))

ggplot(datos_tcm_tibble_edadcat2) +
  aes(x = MontoAcum) +
  geom_density(adjust = 1L, fill = "salmon") +
  theme_minimal() +
  facet_wrap(vars(`Reg (1=RM)`))

ggplot(datos_tcm_tibble_edadcat2) +
  aes(x = MontoAcum) +
  geom_density(adjust = 1L, fill = "salmon") +
  theme_minimal() +
  facet_wrap(vars(`Sex(1=Fem)`))


anova4 = aov(datos_tcm_tibble_edadcat2$MontoAcum~datos_tcm_tibble_edadcat2$edad_cat)
summary(anova4)

anova5 = aov(datos_tcm_tibble_edadcat2$MontoAcum~datos_tcm_tibble_edadcat2$`Sex(1=Fem)`)
summary(anova5)

anova6 = aov(datos_tcm_tibble_edadcat2$MontoAcum~datos_tcm_tibble_edadcat2$`Reg (1=RM)`)
summary(anova6)


# dado que en las 3 el valor-p es mayor a 0.05, no hay evidencia 

```