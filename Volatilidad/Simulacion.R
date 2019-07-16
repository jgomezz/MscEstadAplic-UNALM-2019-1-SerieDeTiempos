
# Simulación
N = 200
set.seed(2019)
library(fGarch)
Y = ts(garchSim(garchSpec(model = list(alpha=0.45,beta=0.5)), 
                n = N)[,1])

# Análisis exploratorio
library(ggfortify)
autoplot(Y)
library(tseries)
kpss.test(Y)
ggtsdisplay(Y, lag = 100)

# Box Jenkins
library(forecast)
mod.arima = auto.arima(Y)
mod.arima
res = mod.arima$residuals
ggtsdisplay(res, lag = 100)
ggtsdisplay(res^2, lag = 100)

library(fractal)
stationarity(Y)

# Modelamiento
modelo1 = garchFit(~garch(1,0), data = Y, trace= FALSE)
modelo1
resi1 = modelo1@residuals/modelo1@sigma.t

modelo2 = garchFit(~garch(2,0), data = Y, trace= FALSE)
modelo2
resi2 = modelo2@residuals/modelo2@sigma.t

modelo3 = garchFit(~garch(1,1), data = Y, trace= FALSE)
modelo3
resi3 = modelo3@residuals/modelo3@sigma.t

library(astsa)
library(TSA)
autoplot(TSA::acf(resi1,lag=100))
autoplot(TSA::acf(resi2,lag=100))
autoplot(TSA::acf(resi3,lag=100))

autoplot(TSA::acf(resi1^2,lag=100))
autoplot(TSA::acf(resi2^2,lag=100))
autoplot(TSA::acf(resi3^2,lag=100))

autoplot(ts(modelo1@h.t))
autoplot(ts(modelo2@h.t))
autoplot(ts(modelo3@h.t))
autoplot(cbind(ts(modelo1@h.t),ts(modelo2@h.t),ts(modelo3@h.t)), xlab="", ylab="")

autoplot(ts(modelo1@fitted))
autoplot(ts(modelo2@fitted))
autoplot(ts(modelo3@fitted))

summary(modelo1)
summary(modelo2)
summary(modelo3)
