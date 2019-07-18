
# ?ndice trimestral de comercio minorista en la zona del euro (17 pa?ses), 1996-2011, 
# que abarca el comercio mayorista y minorista, y la reparaci?n de veh?culos de motor y
# motocicletas. (?ndice: 2005 = 100).
# Fuente: Eurostat. https://data.is/IdKyZr
library(fpp2)

# ============== #
# Identificaci?n #
# ============== #

ggtsdisplay(euretail,lag = 32)
kpss.test(euretail)$p.value # < 0.01 # no estacionario
adf.test(euretail)$p.value # > 0.99 # no estacionario
ur.df(euretail)@teststat # 0.9775485< 3 # no estacionario
pp.test(euretail)$p.value # > 0.99 # no estacionario
ur.pp(euretail)@teststat # -2.909147< 3 # no estacionario
# observamos las gr?ficas ACF, domina la tendencia

ggtsdisplay(diff(euretail),lag = 32)
kpss.test(diff(euretail))$p.value # 0.01234317 # no estacionario
adf.test(diff(euretail))$p.value # 0.3058271 # no estacionario
ur.df(diff(euretail))@teststat # |-4.561445| > 3 # estacionario
pp.test(diff(euretail))$p.value # < 0.01 # estacionario
# para algunas pruebas, d = 1. Sin embargo para otras, observamos 
ur.pp(diff(euretail))@teststat # |-43.4743| < 3 #  estacionario
# las gr?ficas ACF, PACF. domina la estacionalidad
# cuando d = 1, se tiene que p y q toman valores distintos de cero, 
# mientras que P = 2 y Q = 0
auto.arima(euretail, d = 1, D = 0) # p = P = 2, q = Q = 0
# ARIMA(2,1,0)(2,0,0)[4] 

ggtsdisplay(diff(diff(euretail),lag=4),lag = 32)
kpss.test(diff(diff(euretail),lag=4))$p.value # > 0.1 # estacionario
adf.test(diff(diff(euretail),lag=4))$p.value # < 0.01 # estacionario
ur.df(diff(diff(euretail),lag=4))@teststat # |-3.753256| > 3 # estacionario
pp.test(diff(diff(euretail),lag=4))$p.value # < 0.01 estacionario
ur.pp(diff(diff(euretail),lag=4))@teststat # |-47.27217| < 3 #  estacionario
# finalmente conseguimos estacionariedad con d = 1 y D = 1
auto.arima(euretail, d = 1, D = 1) # p = P = 0, q = 3, Q = 1
#ARIMA(0,1,3)(0,1,1)[4] 

auto.arima(euretail) # p = 0, d = 1, q = 3, P = 0, D = 1, Q = 1
#ARIMA(0,1,3)(0,1,1)[4] 

# ============ #
# Modelamiento #
# ============ #

mod1 = Arima(euretail, order = c(2,1,0), seasonal = c(2,0,0))
summary(mod1)

mod2 = Arima(euretail, order = c(0,1,3), seasonal = c(0,1,1))
summary(mod2)

mod3 = Arima(euretail, order = c(1,1,0), seasonal = c(2,0,0))
summary(mod3)

# =========== #
# Diagn?stico #
# =========== #

res1 = residuals(mod1)
res2 = residuals(mod2)
res3 = residuals(mod3)

ggtsdisplay(res1)
ggtsdisplay(res2)
ggtsdisplay(res3)

t.test(res1)$p.value
t.test(res2)$p.value
t.test(res3)$p.value

shapiro.test(res1)$p.value
shapiro.test(res2)$p.value
shapiro.test(res3)$p.value

accuracy(forecast(mod1))
accuracy(forecast(mod2))
accuracy(forecast(mod3))

n    = length(euretail)  # longitud de la serie
k    = 28                # longitud m?nima de la data de entrenamiento (k<n)
ho   = 4                 # longitud de la data de prueba (ho<k<n)
mse1 = mse2 = mse3 = matrix(NA,n-k-ho,ho)
st   = tsp(euretail)[1]  # inicio de la serie

for(i in 1:(n-k-ho)){
  train   = window(euretail, 
                   end   = st + k/4 + (i-2)/4)
  test    = window(euretail, 
                   start = st + k/4 + (i-1)/4,
                   end   = st + k/4 + (i-1)/4 + (ho-1)/4)
  
  modelo1 = Arima(train, order = c(2,1,0), seasonal = c(2,0,0))  
  predi1  = forecast(modelo1,h = ho)                                        
  mse1[i,1:length(test)] = (ts(predi1$mean)-ts(test))^2  
  
  modelo2 = Arima(train, order = c(0,1,3), seasonal = c(0,1,1))  
  predi2  = forecast(modelo2,h = ho)                                        
  mse2[i,1:length(test)] = (ts(predi2$mean)-ts(test))^2
  
  modelo3 = Arima(train, order = c(1,1,0), seasonal = c(2,0,0))  
  predi3  = forecast(modelo3,h = ho)                                        
  mse3[i,1:length(test)] = (ts(predi3$mean)-ts(test))^2  }  

plot(1:ho, 
     colMeans(sqrt(mse1),na.rm=TRUE), 
     type = "l", 
     lwd  = 3,
     col  = "red", 
     xlab = "horizonte", 
     ylab = "RMSE",
     ylim = c(0.25,2))
lines(1:ho, 
      colMeans(sqrt(mse2),na.rm=TRUE), 
      type = "l",
      lwd  = 3,
      col  = "darkblue")
lines(1:ho, 
      colMeans(sqrt(mse3),na.rm=TRUE), 
      type = "l",
      lwd  = 3,
      col  = "springgreen3")
legend("topleft",
       legend = c("ARIMA(2,1,0)(2,0,0)[4]","ARIMA(0,1,3)(0,1,1)[4]","ARIMA(1,1,0)(2,0,0)[4]"),
       col    = c("red","darkblue","springgreen3"),
       lty    = 1,
       lwd    = 3)


# ========== #
# Predicci?n #
# ========== #

forecast(mod2, h = 4)
autoplot(forecast(mod2, h = 4))
