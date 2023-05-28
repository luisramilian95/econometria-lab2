install.packages("magrittr") 
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")

library(magrittr) 
library(dplyr)
library(lubridate, warn.conflicts = FALSE)
library(ggplot2)


dataset = read.csv("dataset.csv")

dataset$cnt <- as.integer(dataset$cnt)
dataset$registered <- as.integer(dataset$registered)
dataset$casual <- as.integer(dataset$casual)

dataset$date<- lubridate::ymd(dataset$dteday)
dataset$year <- lubridate::year(date)
dataset$month <- lubridate::month(date)
dataset$day <- lubridate::day(date)


# 2.  ¿Qué mes es el que tiene la mayor demanda? Muestre una tabla y una gráfica
count_by_month <- aggregate(dataset$cnt, by=list(dataset$month), FUN=sum)
colnames(count_by_month)[1] = "month"
colnames(count_by_month)[2] = "sum_count"
print(count_by_month)
plot(count_by_month$month, count_by_month$sum_count, ylab = "Total", xlab = "Mes")

# 3. ¿Qué rango de hora es la de mayor demanda? Muestre una tabla y una gráfica
count_by_hours <- aggregate(dataset$cnt, by=list(dataset$hr), FUN=sum)
colnames(count_by_hours)[1] = "hour"
colnames(count_by_hours)[2] = "sum_count"
plot(count_by_hours$hour, count_by_hours$sum_count, ylab = "Total", xlab = "Hora")


# 4. ¿Qué temporada es la mas alta? Muestre una tabla.
count_by_season <- aggregate(dataset$cnt, by=list(dataset$season), FUN=sum)
colnames(count_by_season)[1] = "season"
colnames(count_by_season)[2] = "sum_count"
count_by_season <- count_by_season[order(-count_by_season$sum_count), ]
print(count_by_season) 


#5. ¿A que temperatura disminuye la demanda? Muestre una gráfica para analizar y dar su respuesta.
mean_by_temp <- aggregate(dataset$cnt, by=list(dataset$temp), FUN=mean)
colnames(mean_by_temp)[1] = "temperature"
colnames(mean_by_temp)[2] = "mean"
plot(mean_by_temp$temperature, mean_by_temp$mean)
hist(dataset$temp, freq = FALSE, main = "Histograma y densidad",
     ylab = "Densidad", xlab="Temperatura")
dtemp <- density(dataset$temp)
lines(dtemp, lwd = 2, col = "red")
print("La demanda diminuye cuando es la temperatura es menor a 0.6 y mayor a 0.9")



#6. ¿A que humedad disminuye la demanda? Muestre una gráfica para analizar y dar su respuesta.
mean_by_hum <- aggregate(dataset$cnt, by=list(dataset$hum), FUN=mean)
colnames(mean_by_hum)[1] = "humedity"
colnames(mean_by_hum)[2] = "mean"
plot(mean_by_hum$humedity, mean_by_hum$mean)
hist(dataset$hum, freq = FALSE, main = "Histograma y densidad",
     ylab = "Densidad", xlab="Humedad")
dhum <- density(dataset$hum)
lines(dhum, lwd = 2, col = "red")
print("La demanda diminuye cuando es la humedad es menor a 0.2 y mayor a 0.4")


# 7. ¿Que condiciones climáticas serian ideales para nuestra demanda? (considere una función de densidad bivariable para la temperatura y la humedad)
print("Las condiciones ideales son con una temperatura entre (0.7 a 0.9) con una humedad entre (0.2 a 0.4).")

#8. Mueste una gráfica de la densidad de rentas.
hist(dataset$cnt, freq = FALSE, main = "Histograma y densidad",
     ylab = "Densidad", xlab="Renta")
drent <- density(dataset$cnt)
lines(drent, lwd=2, col="red")

#9. ¿En promedio de personas que rentan bicicletas y están registradas?
percent <-   sum(dataset$registered) / sum(dataset$cnt) * 100
print(percent)
mean(dataset$registered)

#10. Determine la mediana de personas que rentan bicicletas y no están registradas.
median(dataset$casual)

#11. Deterimne la renta total, renta promedio por cada tipo de estación.
sum_mean_by_season <- aggregate(dataset$cnt, by=list(dataset$season), function(x) cbind( sum(x),mean(x)))
colnames(sum_mean_by_season)[1] = "season"
sum_mean_by_season

#12. Determine y muestre una gráfica de barras la cantidad de rentas por tipo de temporada.
plot(sum_mean_by_season$season, sum_mean_by_season$x[,1], ylab = "Total", xlab = "Temporada")

#13. Muestre una gráfica de la densidad por hora.
hist(dataset$hr, freq = FALSE, main = "Histograma y densidad",
     ylab = "Densidad", xlab="Hora")
dhr <- density(dataset$hr)
lines(dhr, lwd=2, col="red")


#14. Muestre una gráfica de barras por día del mes como eje x y la cantidad total de alquileres como el eje Y.
count_by_day <- aggregate(dataset$cnt, by=list(dataset$day), FUN=sum)
colnames(count_by_day)[1] = "day"
colnames(count_by_day)[2] = "sum_count"
count_by_day
options(scipen=999)
barplot(count_by_day$sum_count, main = "Distribucion por dia", names.arg =   count_by_day$day, ylab = "Total", xlab = "dia")


# 15. Muestre una serie temporal (gráfica) con el tiempo (mes-dia-año) como eje de tiempo y la cantidad de alquieleres como eje Y.
count_by_date <- aggregate(dataset$cnt, by=list(dataset$date), FUN=sum)
colnames(count_by_date)[1] = "date"
colnames(count_by_date)[2] = "sum_count"
count_by_date

# 16. Muestre una gráfica de puntos que relaciones la temperatura comoejer X y la humedad como eje Y.
plot(dataset$temp, dataset$hum, ylab = "Humedad", xlab = "Temperatura")