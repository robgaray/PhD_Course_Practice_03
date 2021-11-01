##################
### Curso de Doctorado
### Análisis de datos para cargas energéticas en edificios. Contexto, Métodos de análisis, Herramientas y Aplicaciones
### UPV/EHU, 2021
### Roberto GARAY MARTINEZ, roberto@robertogaray.com / roberto.garay@Tecnalia.com
### Mikel LUMBRERAS MUGAGUREN
##################

##################
### Práctica 3
### Script 4
###
### 0. Gestión de directorios
### 1. Instalación y activación de librerías, carga de funciones
### 2. Apertura de archivos CSV
###
### referencia
###
### 3. Agregados diarios
### 4. Definición de función Qbase & Optimización
### 5. Definición función Qbase multidimensional & Optimización
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


##################
### 3. Agregados diarios
##################
mean_T_day <- aggregate(dataset$Temperature, list(dataset$Day_Year), mean)
names(mean_T_day)<- c("Day_Year","Mean_Temperature")

load_day <- aggregate(dataset$Power.kW., list(dataset$Day_Year), sum)
names(load_day)<- c("Day_Year","Load")

Isol_day<- aggregate(dataset$Irradiation.flux, list(dataset$Day_Year), sum)
names(Isol_day)<- c("Day_Year","Solar_Radiation")

daily_summary<-cbind(mean_T_day,Isol_day$Solar_Radiation,load_day$Load)
names(daily_summary)<- c("Day_Year","Mean_Temperature","Solar_Radiation","Load")

##################
### 4. Definición de función Qmax & Optimización
##################
# Funciones para calcular el error (R2 y MAE)
r2 <- function (observacion, modelo) cor(observacion, modelo) ^ 2
MAE <- function (observacion, modelo) (sum((observacion>modelo)*(observacion-modelo)-(observacion<modelo)*(observacion-modelo)))/length(observacion)


# Función Qbase
Qbase<- function(Temperature, slope, horizontal,intercept){
  load<-pmax(horizontal,
             Temperature*slope+intercept)
}

# Resolución por barrido
# (no es muy óptima)
# (approx 1-30', según la combinatoria)
# (~1' para ~10^4-5 combinaciones)

# combinatoria de casos
rango_slope<-seq(-100,0,1)
rango_intercept<-seq(0,1000,25)
rango_horizontal<-seq(0,200,20)
combinatoria<-as.data.frame(expand.grid(rango_slope, rango_horizontal, rango_intercept))
combinatoria<-cbind(combinatoria,
                    rep(0,dim(combinatoria)[1]),
                    rep(0,dim(combinatoria)[1]))
names(combinatoria)<-c("slope", "horizontal", "intercept","R2","MAE")

# cálculo de la resolución
observacion<-daily_summary$Load
for (i in 1: dim(combinatoria)[1])
{
  modelo<-Qbase(daily_summary$Mean_Temperature,
                combinatoria$slope[i],
                combinatoria$horizontal[i],
                combinatoria$intercept[i])
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
rango_intercept<-seq(quantile(optimo$intercept, .25),
                     quantile(optimo$intercept, .75),
                     5)
rango_horizontal<-seq(quantile(optimo$horizontal, .25),
                      quantile(optimo$horizontal, .75),
                      2.5)
combinatoria<-as.data.frame(expand.grid(rango_slope, rango_horizontal, rango_intercept))
combinatoria<-cbind(combinatoria,
                    rep(0,dim(combinatoria)[1]),
                    rep(0,dim(combinatoria)[1]))
names(combinatoria)<-c("slope", "horizontal", "intercept","R2","MAE")


# cálculo de la resolución
observacion<-daily_summary$Load
for (i in 1: dim(combinatoria)[1])
{
  modelo<-Qbase(daily_summary$Mean_Temperature,
                combinatoria$slope[i],
                combinatoria$horizontal[i],
                combinatoria$intercept[i])
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
opt_horizontal<-as.numeric(quantile(optimo$horizontal,0.5))
opt_intercept <-as.numeric(quantile(optimo$intercept,0.5))


# Graficar
par(mfrow=c(3,1))
# Graficar Q-T
plot(daily_summary$Mean_Temperature,daily_summary$Load, xlab="Temperature [ºC]", ylab="Load [kWh]")
points(daily_summary$Mean_Temperature,
       Qbase(daily_summary$Mean_Temperature,
             opt_slope,
             opt_horizontal,
             opt_intercept),
       col="blue")

# Graficar Q-t
plot(daily_summary$Load, xlab="time [Days]", ylab="Load [kWh]")
points(Qbase(daily_summary$Mean_Temperature,
             opt_slope,
             opt_horizontal,
             opt_intercept),
       col="blue")

# Graficar Q-Q
plot(daily_summary$Load,
     Qbase(daily_summary$Mean_Temperature,
           opt_slope,
           opt_horizontal,
           opt_intercept),
     col="blue", , xlab="Load, data [kWh]", ylab="Load, modeled [kWh]")
abline(0,1)
par(mfrow=c(1,1))


##################
### 5. Definición función Qmax multidimensional & Optimización
##################
# Función Qbase
Qbase_multi<- function(Temperature, Solar_Rad, slope_T, horizontal,intercept, Slope_I){
  load<-pmax(horizontal,
             Temperature*slope_T+intercept+Solar_Rad*Slope_I)
}

# Resolución por barrido
# (no es muy óptima)
# (approx 1-30', según la combinatoria)
# (~1' para ~10^4-5 combinaciones)

# combinatoria de casos
rango_slope_T<-seq(-100,0,10)
rango_intercept<-seq(500,1000,50)
rango_horizontal<-seq(0,300,25)
rango_slope_I<-seq(-5,0,0.1)
combinatoria<-as.data.frame(expand.grid(rango_slope_T, rango_horizontal, rango_intercept,rango_slope_I))
combinatoria<-cbind(combinatoria,
                    rep(0,dim(combinatoria)[1]),
                    rep(0,dim(combinatoria)[1]))
names(combinatoria)<-c("slope_T", "horizontal", "intercept","slope_I","R2","MAE")

# cálculo de la resolución
observacion<-daily_summary$Load
for (i in 1: dim(combinatoria)[1])
{
  modelo<-Qbase_multi(daily_summary$Mean_Temperature,
                daily_summary$Solar_Radiation,
                combinatoria$slope_T[i],
                combinatoria$horizontal[i],
                combinatoria$intercept[i],
                combinatoria$slope_I[i])
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
rango_slope_T<-seq(quantile(optimo$slope_T, .25),
                 quantile(optimo$slope_T, .75),
                 5)
rango_intercept<-seq(quantile(optimo$intercept, .25),
                     quantile(optimo$intercept, .75),
                     20)
rango_horizontal<-seq(quantile(optimo$horizontal, .25),
                      quantile(optimo$horizontal, .75),
                      10)
rango_slope_I<-seq(quantile(optimo$slope_I, .25),
                   quantile(optimo$slope_I, .75),
                   0.01)
combinatoria<-as.data.frame(expand.grid(rango_slope_T, rango_horizontal, rango_intercept,rango_slope_I))
combinatoria<-cbind(combinatoria,
                    rep(0,dim(combinatoria)[1]),
                    rep(0,dim(combinatoria)[1]))
names(combinatoria)<-c("slope_T", "horizontal", "intercept","slope_I","R2","MAE")



# cálculo de la resolución
observacion<-daily_summary$Load
for (i in 1: dim(combinatoria)[1])
{
  modelo<-Qbase_multi(daily_summary$Mean_Temperature,
                      daily_summary$Solar_Radiation,
                      combinatoria$slope_T[i],
                      combinatoria$horizontal[i],
                      combinatoria$intercept[i],
                      combinatoria$slope_I[i])
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
opt_multi_slope_T     <-as.numeric(quantile(optimo$slope_T     ,0.5))
opt_multihorizontal   <-as.numeric(quantile(optimo$horizontal  ,0.5))
opt_multi_intercept   <-as.numeric(quantile(optimo$intercept   ,0.5))
opt_multi_slope_I     <-as.numeric(quantile(optimo$slope_I     ,0.5))

# Graficar
# Graficar Q-T
plot(daily_summary$Mean_Temperature,daily_summary$Load)
points(daily_summary$Mean_Temperature,
       Qbase(daily_summary$Mean_Temperature,
             opt_slope,
             opt_horizontal,
             opt_intercept),
       col="blue")
points(daily_summary$Mean_Temperature,
       Qbase_multi(daily_summary$Mean_Temperature,
                   daily_summary$Solar_Radiation,
                   opt_multi_slope_T,
                   opt_multihorizontal,
                   opt_multi_intercept,
                   opt_multi_slope_I),
       col="red")

# Graficar Q-t
plot(daily_summary$Load)
points(Qbase(daily_summary$Mean_Temperature,
             opt_slope,
             opt_horizontal,
             opt_intercept),
       col="blue")
points(Qbase_multi(daily_summary$Mean_Temperature,
                   daily_summary$Solar_Radiation,
                   opt_multi_slope_T,
                   opt_multihorizontal,
                   opt_multi_intercept,
                   opt_multi_slope_I),
       col="red")


# Graficar Q-Q
plot(daily_summary$Load,
     Qbase(daily_summary$Mean_Temperature,
           opt_slope,
           opt_horizontal,
           opt_intercept),
     col="blue")
points(daily_summary$Load,
       Qbase_multi(daily_summary$Mean_Temperature,
                   daily_summary$Solar_Radiation,
                   opt_multi_slope_T,
                   opt_multihorizontal,
                   opt_multi_intercept,
                   opt_multi_slope_I),
       col="red")
abline(0,1)
