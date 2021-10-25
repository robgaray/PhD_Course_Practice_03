# Análisis de datos para cargas energéticas en edificios. Contexto, Métodos de análisis, Herramientas y Aplicaciones

# Módulo 3. Práctica (4h)

# Bloque 1.
# Objetivos y Herramientas
*(30')*
## Datos a utilizar
*(Teoría 10')*

## Conocimientos prácticos 
*(Teoría 15')*
- Teoría 01
  - Heating Degree Day
- Práctica 01
  - Subseteado & Agregado
  - Ploteado
- Práctica 02
  - Métricas de error. R2 y MAE
- Teoría 3
  - PRISM
  - ASHRAE Changepoint (3 parámetros)
- Adicional
  - Regresión *lm()*

## Puesta a punto
*(Práctica 5')*

# Bloque 2
*(Práctica 90')*
# PRISM
*(Práctica 15`)*
## [Script 1]

- Calcular HDD
- Agregar datos mensuales
- Graficar
- Regresionar
- Proyectar sobre otros datos climáticos
- Métricas de error 

## [Script 1, Resuelto]
*(hasta aquí, 35')*

# ASHRAE Changepoint
*(Práctica 60`)*
## [Script 2]
- Aproximaciones posibles
  - T corte
  - Q corte
  - Elegir Q corte

- Proceso
  - Definir rango y paso de tiempo de variación variable de segmentación
  - Calcular parámetros correspondiente a cada segmento
  - Simular
  - Graficar
  - Calcular métricas de error
  - Seleccionar modelo óptimo
- Estructura de datos
  - Serie temporal
  - Archivo de registro información

- Escribir una función para cada caso

- Generar un proceso automático

## [Script 2, Resuelto]

*(hasta aquí, 1h 30')*

*(Descanso, 15')*

# Variables Relevantes
*(Práctica 60`)*
## [Script 3]
- Definir sensibilidad
  - Gráfico de correlación
  - Boxplot del output por variables de calendario
  - Gráfico de correlación vs error de modelo simple
  - Boxplot del error del modelo simple por variables de calendario

- ·Generar variables auxiliares
  - Gráfico de correlación
  - Gráfico de correlación vs error de modelo simple

- Extender el proceso de ASHRAE
  - Archivo de registro información
  - Calcular parámetros correspondiente a cada segmento
  - Simular

- Identificar mejoras al modelo
  - Métricas de error del modelo simple vs sobreparametrizado

- Selección de variables
  - Significancia variables [¿Cómo se calcula a mano?]
  - Selección de las más relevantes
  - Generar subset con variables relevantes
  - Nuevo proceso de identificación
  - Métricas de error y comparación con modelos simple y sobreparametrizado
## [Script 3, Resuelto]
*(hasta aquí, 2h 45')*
*(Meter algo más??)*