#install.packages("tseries")
#install.packages("forecast")
#install.packages("TSstudio")

library("readxl")

library(tseries)
library(forecast)
library(dplyr)
library(TSstudio)


rm(list = ls(all = T))
options(digits=9)

setwd("C:/Users/juapai/OneDrive - VRIO DIRECTV/Escritorio/Maestria/MIT_Course/multiplica")
list.files("C:/Users/juapai/OneDrive - VRIO DIRECTV/Escritorio/Maestria/MIT_Course/multiplica")

my_data <- read_excel("flujo_vehicular_wcm.xlsx")


getwd()

#DatosEnergia <- read.csv("C:/Users/Juan Paiba/Desktop/Datos_historicos_ELE.csv", header = T,sep = ";")
my_data$V1=as.character(my_data$cantidad)
my_data$V1=as.numeric(my_data$V1)


data <- filter(my_data, disp_nombre == "RD169 Monroe" & WCM >= '2020-03-01' & WCM < '2022-02-27')
data <- data[order(data$WCM),]

flujo <- ts(data$V1,start=c(2020,03,08),frequency = 52)
m=data.frame(flujo)

plot(flujo, main="flujo vehicular en radar Monroe", xlab="Tiempo", ylab="Cant. Vehículos")

par(mfrow=c(1,2))
acf(flujo,main="ACF serie flujos", lag.max=36)
pacf(flujo,main="PACF serie flujos", lag.max=36)

adf.test(flujo)

#se realiza autoarima  
auto.arima(flujo, trace=T)
#Mejor Modelo Best model: ARIMA(3,0,3) with non-zero mean 

#Se estiman posibles modelos
mod1<-arima(flujo,order=c(4,1,0))
accuracy(mod1)
mod1
summary(mod1)

at_est <- residuals(mod1)
windows()
plot(at_est, main = "Reisudos")


Ljung_Box <- NULL
x <- at_est
for(i in 1:18){
  Ljung_Box1 <- Box.test(x, lag = i, type="Ljung")$p.value
  Ljung_Box <- rbind(Ljung_Box, cbind(i,Ljung_Box1))
}
colnames(Ljung_Box) <- c("Rezago","p-valor");
cat(" \n \n")
cat("Prueba Ljung_Box Hipótesis nula independencia \n \n")
Ljung_Box
plot(Ljung_Box, main="Prueba Ljung and Box \n H0: Independencia",ylim=c(0,1))
abline(h=0.05,col="red")


#SE CALCULA LA MEDIA DE LOS ERRORES
mean(at_est)

### pruebas de hipótesis de normalidad
ad.test(at_est) #test de normalidad (nortest)
shapiro.test(at_est) #test de normalidad (stats)

win.graph(width=2.5,height=2.5,pointsize=4)
qqnorm(at_est); qqline(at_est,col=2)



win.graph(width=2.5,height=2.5,pointsize=8)
acf(at_est, main="Retornos acción Energia")
pacf(at_est)

ajuste <- auto.arima(y = flujo)
summary(ajuste)

ts_decompose(flujo)

predicciones <- forecast(ajuste,h=4)
autoplot(predicciones)





fit <- ets(flujo)
fit <- auto.arima(flujo)
plot(forecast(fit, 5))
