#### No Linearidad

require(fNonlinear)
require("NTS")
require(TSA)

library(tsDyn)
library(coda)
library(Metrics)

### Pruebas de no linearidad

data(lynx)
y <- log10(lynx)
autoplot(y)

#########################
#### No paremetricos ####
#########################

## bds Test: esta prueba se utiliza ampliamente para verificar la hipótesis nula
# de que una serie de tiempo determinada consta de variables aleatorias 
# independientes e idénticamente distribuidas (iid).
# En la practica uno necesita seleccionar

# En la práctica, es necesario seleccionar la dimensión de empotramiento m y la 
# distancia 𝜖.

# La dimensión máxima predeterminada es m = 3, lo que significa que las dimensiones
# de incrustación utilizadas en la prueba son 2 y 3. Las opciones predeterminadas 
# de 𝜖 son (0.5, 1, 1.5, 2) 𝜎̂ x, donde 𝜎̂ x denota el error estándar de muestra de

# La prueba BDS está diseñada para probar la hipótesis nula de iid, ya que es 
# necesario eliminar cualquier dependencia dinámica lineal antes de aplicar 
# la prueba para detectar la no linealidad en una serie de tiempo. 
# En otras palabras, se debe tener cuidado al utilizar la prueba BDS para detectar 
# la no linealidad. En la práctica, la prueba se aplica típicamente a los residuos 
# de un modelo de serie de tiempo lineal ajustado. Además, un rechazo de la 
# prueba BDS no aporta ninguna información específica para mejorar el modelo ajustado.
# A menudo se necesita un análisis más detallado de los residuos para buscar direcciones 
# para el refinamiento del modelo después de que la prueba rechaza la hipótesis nula.
# Como sugiere la opción predeterminada, la distancia 𝜖 utilizada en la prueba BDS 
# debe estar relacionada con el error estándar de xt.

bdsTest(y)

# La prueba de Pena – Rodríguez: se puede utilizar para la verificación del modelo
# de un modelo de serie de tiempo lineal ajustado, incluida la no linealidad en 
# los residuos.

PRnd(y,m=8)
#########################
##### Parametricos ##### 
#########################

Keenan.test(y)
Tsay.test(y)

### Modelos 

mod <- list()
# SETAR
grid <- selectSETAR(tsF, m = 3, thDelay = 1, trim = 0.15, criterion = "AIC")
print(grid)
mod[["setar"]] <- setar(tsF, m = 3, mL = 1, mH = 2, thDelay = 1)
summary(mod[["setar"]])
sapply(mod, AIC)
sapply(mod, MAPE)
summary(mod[["setar"]])
plot(mod[["setar"]])

mod.test[['lstar']] <- lstar(x.train, m=2, thDelay=1, trace=FALSE,
control=list(maxit=1e5))
mod.test[["nnet"]] <- nnetTs(x.train, m = 2, size = 3, control = list(maxit = 1e+05))
mod.test[["aar"]] <- aar(x.train, m = 2)

frc.test <- lapply(mod.test, predict, n.ahead = 100)
frc.test

plot(x.test)
lines(frc.test$linear, col = "red")
lines(frc.test$setar, col = "blue")
# lines(frc.test$lstar, col='green')
lines(frc.test$nnet, col = "orange")
lines(frc.test$aar, col = "violet")
sapply(mod.test, MAPE)

rmse(x.test, frc.test$setar)
rmse(x.test,frc.test$lstar)
rmse(x.test, frc.test$nnet)
rmse(x.test, frc.test$aar)



llynx <- log10(lynx)
l<-selectSETAR(llynx, m=4, thDelay = c(0,1,2,3), trim = 0.15, criterion = "AIC")


#Suggested model is the following:
summary(setar(llynx, m=2, thDelay=1, th=3.4))
