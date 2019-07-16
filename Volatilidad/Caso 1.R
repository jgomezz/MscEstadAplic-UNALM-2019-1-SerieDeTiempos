
# RD14970DQ
# Depósitos en el sistema financiero por departamentos - fin de periodo (millones S/) - Cusco (moneda nacional)

# Lectura de datos
datos = ts(read.table("datos.txt",T),freq = 4, start = c(1980,1))

# Análisis exploratorio
library(ggfortify)
autoplot(datos, xlab = "Año", ylab = "millones US$", main = "Cuenta financiera del sector privado")

# Retornos
R = timeSeries::returns(datos, method = "discrete")[-1]
autoplot(ts(R, freq = 4, start = c(1980,1)), xlab = "Año", ylab = "millones US$", main = "Retornos")
qplot(R, geom = "histogram", fill = I("darkblue"), bins = 1+3.3*log10(length(R)))
library(e1071)
kurtosis(R)
shapiro.test(R)
library(fractal)
stationarity(R)

# ARIMA
library(forecast)
modelo.arima = auto.arima(R)
res.arima    = residuals(modelo.arima)
t.test(res.arima)
shapiro.test(res.arima)
ggtsdisplay(res.arima)

# 1. Identificación
ggtsdisplay(res.arima^2)
# ARIMA es un buen modelo, no hay heterocedasticidad condicional
