### Guardar resultados de regresiones

lm1 <- lm(y~x, datos)

sum.lm1 <- summary(lm1)


resultados <- data.frame(modelo= c('lm1', 'lm2'),
                         r2= c(sum.lm1$r2, sum.lm2$r2),
                         p.value = c(sum.lm1$p.value, sum.lm2$p.value))

write_csv(resultados, 'resultados.csv')