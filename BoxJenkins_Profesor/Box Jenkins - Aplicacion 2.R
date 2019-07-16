
# Tipo de cambio de d?lar americano - fin de periodo (anual) - S/ por US$
# Fuente: https://estadisticas.bcrp.gob.pe/estadisticas/series/anuales/resultados/PM05309PA/html/1996/2018/

Y = c(2.583, 2.716, 3.134, 3.484, 3.52, 3.435, 3.514, 3.471, 3.281, 3.424, 3.205, 2.981, 3.114, 2.877, 2.816, 2.696, 2.567, 2.785, 2.962, 3.383, 3.395, 3.246, 3.364)
Y = ts(Y, start = 1996, freq = 1)

# ============== #
# Identificaci?n #
# ============== #

library(tseries)
library(urca)
library(ggfortify)
library(ggplot2)
library(forecast)
kpss.test(Y)$p.value # e
adf.test(Y)$p.value #ne
ur.df(Y)@teststat # ne
pp.test(Y)$p.value # ne
ur.pp(Y)@teststat # e
ggtsdisplay(Y, lag = 12)
# d = 0
auto.arima(Y, d = 0) # p = 2, q = 0

kpss.test(diff(Y))$p.value # e
adf.test(diff(Y))$p.value # ne
ur.df(diff(Y))@teststat # e
pp.test(diff(Y))$p.value # ne
ur.pp(diff(Y))@teststat # e
ggtsdisplay(diff(Y),lag=12)
# d = 1
auto.arima(Y,d=1) # p = q = 0

kpss.test(diff(Y,differences = 2))$p.value # e
adf.test(diff(Y,differences = 2))$p.value # ne
ur.df(diff(Y,differences = 2))@teststat # e
pp.test(diff(Y,differences = 2))$p.value # e
ur.pp(diff(Y,differences = 2))@teststat # e
ggtsdisplay(diff(Y,differences = 2),lag=12)
# d = 2
auto.arima(Y,d=2) # p = 0, q = 1

kpss.test(diff(Y,differences = 3))$p.value # e
adf.test(diff(Y,differences = 3))$p.value # e
ur.df(diff(Y,differences = 3))@teststat # e
pp.test(diff(Y,differences = 3))$p.value # e
ur.pp(diff(Y,differences = 3))@teststat # e
ggtsdisplay(diff(Y,differences = 3),lag=18)
# d = 3
auto.arima(Y,d=3) # p = 2, q = 0 

# Modelos tentativos
# Modelo 1 => p = 1, d = 0, q = 0 # cte/sin cte
# Modelo 2 => p = 2, d = 0, q = 0 # cte/sin cte
# Modelo 3 => p = 0, d = 1, q = 0 # cte/sin cte
# Modelo 4 => p = 0, d = 2, q = 0
# Modelo 5 => p = 0, d = 2, q = 1
# Modelo 6 => p = 2, d = 3, q = 0

# ============ #
# Modelamiento #
# ============ #

modelo1 = Arima(Y, order=c(1,0,0), include.constant = T)
modelo1
3.0858*(1-.8135)

modelo2 = Arima(Y, order=c(2,0,0), include.constant = T)
modelo2
3.1136*(1-1.1316+0.4084)

modelo3a = Arima(Y, order=c(0,1,0), include.constant = F)
modelo3a

modelo3b = Arima(Y, order=c(0,1,0), include.constant = T)
modelo3b

modelo4 = Arima(Y, order=c(0,2,0))
modelo4

modelo5 = Arima(Y, order=c(0,2,1))
modelo5

modelo6 = Arima(Y, order=c(2,3,0))
modelo6

# =========== #
# Diagn?stico #
# =========== #

res1  = residuals(modelo1)
res2  = residuals(modelo2)
res3a = residuals(modelo3a)
res3b = residuals(modelo3b)
res4  = residuals(modelo4)
res5  = residuals(modelo5)
res6  = residuals(modelo6)

shapiro.test(res1)
shapiro.test(res2)
shapiro.test(res3a)
shapiro.test(res3b)
shapiro.test(res4)
shapiro.test(res5)
shapiro.test(res6)
# todos cumplen con normalidad de errores

t.test(res1)$p.value
t.test(res2)$p.value
t.test(res3a)$p.value
t.test(res3b)$p.value
t.test(res4)$p.value
t.test(res5)$p.value
t.test(res6)$p.value
# todos cumplen con media cero

ggtsdisplay(res1,lag=15)
ggtsdisplay(res2,lag=15)
ggtsdisplay(res3a,lag=15)
ggtsdisplay(res3b,lag=15)
ggtsdisplay(res4,lag=15)
ggtsdisplay(res5,lag=15)
ggtsdisplay(res6,lag=15)
# sin autocorrelaciones significativamente distintas de cero

accuracy(forecast(modelo1))
accuracy(forecast(modelo2))
accuracy(forecast(modelo3a))
accuracy(forecast(modelo3b))
accuracy(forecast(modelo4))
accuracy(forecast(modelo5))
accuracy(forecast(modelo6))

n    = length(Y)    # longitud de la serie
k    = 18           # longitud minima de la data de entrenamiento (k<n)
ho   = 2            # longitud de la data de prueba (ho<k<n)
mse1 = mse2 = mse3a = mse3b = mse4 = mse5 = mse6 = matrix(NA,n-k-ho,ho)
st   = tsp(Y)[1]    # inicio de la serie

for(i in 1:(n-k-ho)){
  train   = window(Y, 
                   end   = st + k + (i-2))
  test    = window(Y, 
                   start = st + k + (i-1),
                   end   = st + k + (i-1) + (ho-1))
  
  modelo1 = Arima(train, order = c(1,0,0), include.constant = TRUE)  
  predi1  = forecast(modelo1,h = ho)                                        
  mse1[i,1:length(test)] = ((ts(predi1$mean)-ts(test))^2)
  
  modelo2 = Arima(train, order = c(2,0,0), include.constant = TRUE)  
  predi2  = forecast(modelo2,h = ho)                                        
  mse2[i,1:length(test)] = ((ts(predi2$mean)-ts(test))^2)
  
  modelo3a= Arima(train, order = c(0,1,0), include.constant = FALSE)  
  predi3a = forecast(modelo3a,h = ho)                                        
  mse3a[i,1:length(test)] = ((ts(predi3a$mean)-ts(test))^2)
  
  modelo3b= Arima(train, order = c(0,1,0), include.constant = TRUE)  
  predi3b = forecast(modelo3b,h = ho)                                        
  mse3b[i,1:length(test)] = ((ts(predi3b$mean)-ts(test))^2)
  
  modelo4 = Arima(train, order = c(0,2,0))  
  predi4  = forecast(modelo4,h = ho)                                        
  mse4[i,1:length(test)] = ((ts(predi4$mean)-ts(test))^2)
  
  modelo5 = Arima(train, order = c(0,2,1))  
  predi5  = forecast(modelo5,h = ho)                                        
  mse5[i,1:length(test)] = ((ts(predi5$mean)-ts(test))^2)
  
  modelo6 = Arima(train, order = c(2,3,0))  
  predi6  = forecast(modelo6,h = ho)                                        
  mse6[i,1:length(test)] = ((ts(predi6$mean)-ts(test))^2)}  

plot(1:ho, 
     colMeans(sqrt(mse1),na.rm=TRUE), 
     type = "l", 
     lwd  = 3,
     col  = "red", 
     xlab = "horizonte", 
     ylab = "RMSE",
     ylim = c(0.1,0.6))
lines(1:ho, 
      colMeans(sqrt(mse2),na.rm=TRUE), 
      type = "l",
      lwd  = 3,
      col  = "darkblue")
lines(1:ho, 
      colMeans(sqrt(mse3a),na.rm=TRUE), 
      type = "l",
      lwd  = 3,
      col  = "orange2")
lines(1:ho, 
      colMeans(sqrt(mse3b),na.rm=TRUE), 
      type = "l",
      lwd  = 3,
      col  = "springgreen3")
lines(1:ho, 
      colMeans(sqrt(mse4),na.rm=TRUE), 
      type = "l",
      lwd  = 3,
      col  = "gray")
lines(1:ho, 
      colMeans(sqrt(mse5),na.rm=TRUE), 
      type = "l",
      lwd  = 3,
      col  = "pink")
lines(1:ho, 
      colMeans(sqrt(mse6),na.rm=TRUE), 
      type = "l",
      lwd  = 3,
      col  = "gold")

# Elegimos el modelo 2

# ========== #
# Pron?stico #
# ========== #

modelo2 = Arima(Y, order=c(2,0,0), include.constant = T)
forecast(modelo2, h = 2)
autoplot(forecast(modelo2, h = 2))
