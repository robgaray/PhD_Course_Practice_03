##################
### Curso de Doctorado
### Análisis de datos para cargas energéticas en edificios. Contexto, Métodos de análisis, Herramientas y Aplicaciones
### UPV/EHU, 2021
### Roberto GARAY MARTINEZ, roberto@robertogaray.com / roberto.garay@Tecnalia.com
### Mikel LUMBRERAS MUGAGUREN
##################

##################
### Práctica 3
### Script 1
###
### 0. Gestión de directorios
### 1. Instalación y activación de librerías, carga de funciones
### 2. Apertura de archivos CSV
### 
### referencia
###
### 3. Cálculo de GD
### 4. Agregación mensual
### 5. Graficar
### 6. Regresión
### 7. Proyección
###
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
### Referencia
##################

### aggregate

##################
### 3. Cálculo de GD
##################
### 3.1 Calcular el promedio de temperatura diario
### 3.2 Diferenciar por la temperatura de referencia
### 3.3 Tomar valores negativos e invertir el signo
##################

### 3.1 Calcular el promedio de temperatura diario
mean_T_day <- aggregate(dataset$Temperature, list(dataset$Day_Year), mean)
names(mean_T_day)<- c("Day_Year","Mean_Temperature")

### 3.2 Diferenciar por la temperatura de referencia
T_ref<- 15
mean_T_day$HDD<-mean_T_day$Mean_Temperature-15

### 3.3 Tomar valores negativos e invertir el signo
mean_T_day$HDD<-  pmin(mean_T_day$HDD,0)
mean_T_day$HDD<- -mean_T_day$HDD

##################
### 4. Agregación mensual
##################
### 4.1 Calcular calendario
### 4.2 Agregar HDD
### 4.3 Agregar cargas
##################

### 4.1 Calcular calendario
# Tabla índice con los días del mes
calendar_table<- data.frame(c(1:12),
           c(31,28,31,30,31,30,31,31,30,31,30,31),
           rep(0,12))
names(calendar_table)<-c("month","days", "end_day")
calendar_table$end_day[1]<-calendar_table$days[1]
for (i in 2:dim(calendar_table)[1])
{
  calendar_table$end_day[i]<-calendar_table$end_day[i-1]+calendar_table$days[i]
}

# Tabla unívoca día-mes
calendar<- data.frame(1:365,rep(0,365))
names(calendar)<-c("Day_Year","Month")
for (i in 1:max(calendar_table$month))
{
  month<-i
  end_day<-calendar_table[calendar_table$month==month,]$end_day
  for (j in 1:max(calendar$Day_Year))
  {
    if (calendar$Month[j]==0)
    {
      if (calendar$Day_Year[j]<=end_day)
      {
        calendar$Month[j]<-month
      }
    }
  }
}

### 4.2 Agregar HDD
### 4.3 Agregar cargas
### 4.2.1 Añadir el mes correspondiente a cada día al DF
mean_T_day$Month<-rep(0,dim(mean_T_day)[1])
for (i in 1:dim(mean_T_day)[1])
{
  day<- mean_T_day$Day_Year[i]
  month<-calendar[calendar$Day_Year==day,]$Month
  mean_T_day$Month[i]<-month
}

### 4.2.2. Agregar
HDD_month <- aggregate(mean_T_day$HDD, list(mean_T_day$Month), sum)
names(HDD_month)<- c("Month","HDD")

### 4.3 Agregar cargas
Load_month <- aggregate(dataset$Power.kW., list(dataset$Month), sum)
names(Load_month)<- c("Month","Load.kWh")

### 4.4 Tabla sumario
Summary_month <- cbind(HDD_month,Load_month$Load.kWh)
names(Summary_month)[3]<- "Load.kWh"

##################
### 5. Graficar
##################
par(mfrow=c(3,1)) 
plot (Summary_month[,1:2]   , type="l")
plot (Summary_month[,c(1,3)], type="l")
plot (Summary_month[,2:3])
par(mfrow=c(1,1)) 

# Verificar que se está graficando toda la escala
lim_HDD <- c(0, max(Summary_month$HDD))
lim_Load <- c(0, max(Summary_month$Load.kWh))

par(mfrow=c(3,1)) 
plot (Summary_month[,1:2]   , type="l", ylim=lim_HDD)
plot (Summary_month[,c(1,3)], type="l", ylim=lim_Load)
plot (Summary_month[,2:3], xlim=lim_HDD, ylim=lim_Load)
par(mfrow=c(1,1)) 


##################
### 6. Regresión
##################

RL_I <- lm(Summary_month$Load.kWh ~ Summary_month$HDD)

par(mfrow=c(2,1))
plot(Summary_month$Load.kWh, RL_I[["fitted.values"]], xlim=lim_Load, ylim=lim_Load)
abline(a=0, b=1)

plot (Summary_month[,c(1,3)], type="l", ylim=lim_Load)
points(Summary_month$Month, RL_I[["fitted.values"]], type="l", col="red")
points(Summary_month$Month, RL_I[["residuals"]], type="l", col="blue")
par(mfrow=c(1,1))

# Pasando por 0
RL_nI <- lm(Summary_month$Load.kWh ~ Summary_month$HDD+0)

par(mfrow=c(2,1))
plot(Summary_month$Load.kWh, RL_nI[["fitted.values"]], xlim=lim_Load, ylim=lim_Load)
abline(a=0, b=1)

plot (Summary_month[,c(1,3)], type="l", ylim=lim_Load)
points(Summary_month$Month, RL_nI[["fitted.values"]], type="l", col="red")
points(Summary_month$Month, RL_nI[["residuals"]], type="l", col="blue")
par(mfrow=c(1,1))




##################
### 7. Proyección
##################
# Nuevo archivo de HDD tomado de degredays.net
# Aeropuerto de Tartu
# Formato modificado para compatibiliar con Script
# (original en la misma ruta)

# Archivo de datos
file<-"/2_Data/EETU_HDD_15C_mod_RG.csv"

# Ruta completa
ruta<-paste(wd,file, sep="")

# Lectura de un archivo csv
HDD_month_2020 <- read.csv(ruta, sep=";")
# View(dataset)





Load_month_2020<-as.data.frame(predict(RL_I, HDD_month))
Summary_month_2020 <- cbind(HDD_month_2020,Load_month_2020)
names(Summary_month_2020)[3]<- "Load.kWh"

##################