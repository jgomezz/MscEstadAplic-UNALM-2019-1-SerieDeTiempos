
# RD15202DQ
# Ingresos tributarios recaudados por SUNAT - Impuesto General a las Ventas interno según departamento - Huánuco (millones de soles)

# Lectura de datos
datos = ts(read.table("datos2.txt",T),freq = 4, start = c(1994,1))

# Análisis exploratorio
library(ggfortify)
autoplot(datos, xlab = "Año", ylab = "millones S/", main = "Ingresos tributarios recaudados - IGV - Huánuco")

# Retornos
R1 = timeSeries::returns(datos, method = "discrete")[-1]
autoplot(ts(R1, freq = 4, start = c(1980,1)), xlab = "Año", ylab = "millones US$", main = "Retornos")

R2 = timeSeries::returns(datos, method = "continuous")[-1]
autoplot(ts(R2, freq = 4, start = c(1980,1)), xlab = "Año", ylab = "millones US$", main = "Retornos")
qplot(R2, geom = "histogram", fill = I("darkblue"), bins = 1+3.3*log10(length(R2)))
library(e1071)
kurtosis(R2)
shapiro.test(R2)
library(fractal)
stationarity(R2)

# ARIMA
library(forecast)
modelo.arima = auto.arima(R2)
res.arima    = residuals(modelo.arima)
t.test(res.arima)
shapiro.test(res.arima)
ggtsdisplay(res.arima)

# 1. Identificación
ggtsdisplay(res.arima^2)
# ARIMA no es un buen modelo, hay heterocedasticidad condicional

# 2. Modelamiento
# ARCH(1)
library(fGarch)
mod.arch1 = garchFit(~arma(3,2)+garch(1,0), 
                      data  = R2, 
                      trace = FALSE)
mod.arch1

# ARCH(2)
mod.arch2 = garchFit(~arima(3,2)+garch(2,0), 
                     data  = R2,
                     trace = FALSE)
mod.arch2

# GARCH(1,1)
mod.garch11 = garchFit(~arma(3,2)+garch(1,1), 
                     data  = R2, 
                     trace = FALSE)
mod.garch11

# 3. Diagnóstico
res1  = mod.arch1@residuals/mod.arch1@sigma.t
res2  = mod.arch2@residuals/mod.arch2@sigma.t
res11 = mod.garch11@residuals/mod.garch11@sigma.t

t.test(res1)$p.value
t.test(res2)$p.value
t.test(res11)$p.value

shapiro.test(res1)$p.value
shapiro.test(res2)$p.value
shapiro.test(res11)$p.value

summary(mod.arch1)
summary(mod.arch2)
summary(mod.garch11)

ggtsdisplay(res1)
ggtsdisplay(res2)
ggtsdisplay(res11)

plot(mod.arch1)

# 4. Pronóstico

autoplot(ts(mod.arch1@sigma.t))
autoplot(ts(mod.arch2@sigma.t))
autoplot(ts(mod.garch11@sigma.t))

predict(mod.arch2, n.ahead = 2)


