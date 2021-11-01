##################
### Curso de Doctorado
### Análisis de datos para cargas energéticas en edificios. Contexto, Métodos de análisis, Herramientas y Aplicaciones
### UPV/EHU, 2021
### Roberto GARAY MARTINEZ, roberto@robertogaray.com / roberto.garay@Tecnalia.com
### Mikel LUMBRERAS MUGAGUREN
##################

##################
### Práctica 3
### Script 3
###
### 0. Gestión de directorios
### 1. Instalación y activación de librerías, carga de funciones
### 2. Apertura de archivos CSV
###
### referencia
###
### 3. Agregados diarios
### 4. Definición de función changepoint
### 5. Uso manual de la función
### 6. Optimización
### 7. Formulación alternativa
##################

##################
### 0. Gestión de directorios
##################
# Gestión de directorios
# Directorio de trabajo
wd<-getwd()
# NOTA: Revísese que wd apunta al directorio en el que se ha dejado la información descargada
# por ejemplo, "C:/.../PhD_Course_Practice_01_R"
# en caso contrario, ejecutar la instancia de más abajo (reemplazando la ruta por la que corresponda)
# wd<-"C:/.../PhD_Course_Practice_03_R"


##################
### 1. Instalación y activación de librerías, carga de funciones
##################
  
# # Carga de funciones
# # Archivo de funciones
# file<-""
# # Ruta completa
# ruta<-paste(wd,file, sep="")
# # Carga de funciones
# source(ruta)
  
##################
### 2. Apertura de 1 archivo CSV
##################

# Archivo de datos
file<-"/2_Data/Data_Pr01.csv"

# Ruta completa
ruta<-paste(wd,file, sep="")
  
# Lectura de un archivo csv
dataset <- read.csv(ruta, sep=";")
# View(dataset)
  

##################
### referencia
##################
###
### aggregate()
### sintaxis de funciones
### R2 & MAE
### quantile y percentiles

##################
### 3. Agregados diarios
##################
mean_T_day <- aggregate(dataset$Temperature, list(dataset$Day_Year), mean)
names(mean_T_day)<- c("Day_Year","Mean_Temperature")

load_day <- aggregate(dataset$Power.kW., list(dataset$Day_Year), sum)
names(load_day)<- c("Day_Year","Load")

daily_summary<-cbind(mean_T_day,load_day$Load)
names(daily_summary)<- c("Day_Year","Mean_Temperature","Load")

##################
### 4. Definición de función changepoint
##################
changepoint<- function(Temperature,slope, change_T, horizontal){
 load<-horizontal+ (Temperature<change_T)*slope*(change_T-Temperature)
}

##################
### 5. Uso manual de la función
##################
plot(changepoint(daily_summary$Mean_Temperature,10,15,3))
points(changepoint(daily_summary$Mean_Temperature,5,15,3), col="red")
points(changepoint(daily_summary$Mean_Temperature,10,10,3), col="blue")
points(changepoint(daily_summary$Mean_Temperature,10,15,20), col="green")

##################
### 6. Optimización
##################
# Funciones para calcular el error (R2 y MAE)
r2 <- function (observacion, modelo) cor(observacion, modelo) ^ 2
MAE <- function (observacion, modelo) (sum((observacion>modelo)*(observacion-modelo)-(observacion<modelo)*(observacion-modelo)))/length(observacion)

# Resolución por barrido
# (no es muy óptima)
# (approx 1-30', según la combinatoria)
# (~1' para ~10^4-5 combinaciones)

# combinatoria de casos
rango_slope<-seq(0,50,1)
rango_change_T<-seq(5,20,0.25)
rango_horizontal<-seq(0,100,10)
combinatoria<-as.data.frame(expand.grid(rango_slope, rango_change_T, rango_horizontal))
combinatoria<-cbind(combinatoria,
                    rep(0,dim(combinatoria)[1]),
                    rep(0,dim(combinatoria)[1]))
names(combinatoria)<-c("slope", "change_T", "horizontal","R2","MAE")

# cálculo de la resolución
observacion<-daily_summary$Load
for (i in 1: dim(combinatoria)[1])
{
  modelo<-changepoint(daily_summary$Mean_Temperature,
                   combinatoria$slope[i],
                   combinatoria$change_T[i],
                   combinatoria$horizontal[i])
  combinatoria$R2[i]<-r2(observacion,modelo)
  combinatoria$MAE[i]<-MAE(observacion,modelo)
}

# Eliminar combinaciones con R2 ==NA
combinatoria<-combinatoria[!is.na(combinatoria$R2),]

# Identificar combinatorias buenas
# Percentil R2>0.75
R2_75<-quantile(combinatoria$R2,0.75)
opt_R2<-combinatoria$R2>R2_75
# Percentil MAE<.025
MAE_25<-quantile(combinatoria$MAE,0.25)
opt_MAE<-combinatoria$MAE<MAE_25

# combinado
opt_glob<-opt_R2*opt_MAE
optimo<-combinatoria[opt_glob==1,]
summary(optimo)


# 2a vuelta

# tomar los intercuartiles 1-3 y barrer más fino
rango_slope<-seq(quantile(optimo$slope, .25),
                 quantile(optimo$slope, .75),
                 0.25)
rango_change_T<-seq(quantile(optimo$change_T, .25),
                    quantile(optimo$change_T, .75),
                    0.0625)
rango_horizontal<-seq(quantile(optimo$horizontal, .25),
                      quantile(optimo$horizontal, .75),
                      1)
combinatoria<-as.data.frame(expand.grid(rango_slope, rango_change_T, rango_horizontal))
combinatoria<-cbind(combinatoria,
                    rep(0,dim(combinatoria)[1]),
                    rep(0,dim(combinatoria)[1]))
names(combinatoria)<-c("slope", "change_T", "horizontal","R2","MAE")

# cálculo de la resolución
observacion<-daily_summary$Load
for (i in 1: dim(combinatoria)[1])
{
  modelo<-changepoint(daily_summary$Mean_Temperature,
                      combinatoria$slope[i],
                      combinatoria$change_T[i],
                      combinatoria$horizontal[i])
  combinatoria$R2[i]<-r2(observacion,modelo)
  combinatoria$MAE[i]<-MAE(observacion,modelo)
}

# Eliminar combinaciones con R2 ==NA
combinatoria<-combinatoria[!is.na(combinatoria$R2),]

# Identificar combinatorias buenas
# Percentil R2>0.95
R2_75<-quantile(combinatoria$R2,0.95)
opt_R2<-combinatoria$R2>R2_75

# Percentil MAE<0.05
MAE_25<-quantile(combinatoria$MAE,0.05)
opt_MAE<-combinatoria$MAE<MAE_25

# combinado
opt_glob<-opt_R2*opt_MAE
optimo<-combinatoria[opt_glob==1,]
summary(optimo)

#Coeficientes optimizados
# Tomar el valor promedio
opt_slope     <-as.numeric(quantile(optimo$slope     ,0.5))
opt_change_T  <-as.numeric(quantile(optimo$change_T  ,0.5))
opt_horizontal<-as.numeric(quantile(optimo$horizontal,0.5))


# Graficar
plot(daily_summary$Mean_Temperature,daily_summary$Load, xlab="Temperature [ºC]", ylab="Load [kWh]")
points(daily_summary$Mean_Temperature,
       changepoint(daily_summary$Mean_Temperature,
                   opt_slope,
                   opt_change_T,
                   opt_horizontal),
       col="blue")

plot(daily_summary$Day_Year,daily_summary$Load, xlab="time", ylab="Load [kWh]")
points(daily_summary$Day_Year,
       changepoint(daily_summary$Mean_Temperature,
                   opt_slope,
                   opt_change_T,
                   opt_horizontal),
       col="blue")

##################
### 7. Formulación alternativa
##################
Qbase<- function(Temperature, slope, horizontal,intercept){
  load<-pmax(horizontal,
             Temperature*slope+intercept)
}

# Graficar Q-t
plot(Qbase(daily_summary$Mean_Temperature,-100,0,500))
points(Qbase(daily_summary$Mean_Temperature,-50,0,500), col="red")
points(Qbase(daily_summary$Mean_Temperature,-100,100,500), col="blue")
points(Qbase(daily_summary$Mean_Temperature,-100,100,200), col="green")

# Graficar Q-T
plot(seq(-15,30,1),Qbase(seq(-15,30,1),-100,0,500), type="l")
lines(seq(-15,30,1),Qbase(seq(-15,30,1),-50,0,500), col="red")
lines(seq(-15,30,1),Qbase(seq(-15,30,1),-100,100,500), col="blue")
lines(seq(-15,30,1),Qbase(seq(-15,30,1),-100,100,200), col="green")


