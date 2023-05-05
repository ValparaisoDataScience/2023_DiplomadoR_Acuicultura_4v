# -------------------------------------------------------------
# Clase 02 - Importar y simular variables aleatorias continuas
# Dr. José Gallardo y Dra. María Angélica Rueda Calderón
# 11 abril 2023
# Diplomado en Análisis de Datos y Modelamiento Predictivo
# con Aprendizaje Automático para la Acuicultura.
# -------------------------------------------------------------

# ¿Cómo habilitar paquetes?

library(xlsx)
library(readr)
library(readxl)

# ¿Qué tipos de distribuciones hay en R?

help(Distributions)

# ¿Cómo importar datos datos a R? (Formatos .txt, .csv, .xlsx)

# Importa base de datos en formato .txt

datos_txt <- read.delim("datos.txt")

# Importa base de datos en formato .csv

datos_csv <- read_delim("datos.csv")

# Importa base de datos en formato .xlsx

datos_xlsx <- read_excel("datos.xlsx")

# Simular base de datos con variable aleatoria continua y distribución normal paso a paso
set.seed(1) #semilla para fijar resultados cada vez que se corre la simulación

# Simula variable aleatoria peso salmón
help(rnorm)
set.seed(1)
rnorm(10, 4.5, 0.5) # 10 animales, media=4,5 Kg, ds=1 Kg

# Simula variable aleatoria Sexo
help(rep)
rep(c("Hembra","Macho"), each=100)

# Unimos
set.seed(1)
datos <- data.frame(Sexo=(rep(c("Hembra","Macho"), each=100)),
Peso=c(rnorm(100, 4, 0.5), rnorm(100, 5.5, 0.5)))

# Ejercicio 1 - Explore el objeto datos con summary()
# Observe la variable peso con hist(), boxplot() y density()
# respuestas al final de este script




# Debido a distribución bimodal es conveniente filtrar por sexo antes de predecir
datos_hembras <- datos[datos$Sexo=="Hembra",]

# Histograma y densidad de peso de hembras
hist(datos_hembras$Peso)
plot(density(datos_hembras$Peso))

# Crear una función para predecir datos
Fn <- ecdf(datos_hembras$Peso)
Fn
plot(Fn)

# Proporción de hembras bajo 5 kg
Fn(5)*100

# Proporción de hembras sobre 5 kg
(1-Fn(5))*100

# Ejercicio 2
# a) Calcule proporción de datos 1 desviación estándar sobre la media.
# a) Use el argumento  mean()+sd() dentro de fn
# b) Proporción de datos 1 desviación estándar bajo la media
# c) Calcule proporción de datos 1 desviación en torno a la media
# respuestas al final de este script




























# Respuesta a ejercicio 1
summary(datos)
hist(datos$Peso)
boxplot(datos$Peso ~ datos$Sexo)
densidad <- density(datos$Peso)
plot(densidad)

# Respuesta Ejercicio 2
# Calcule proporción de datos 1 desviación estándar sd() sobre la media mean()
(1-Fn(mean(datos_hembras$Peso) + sd(datos_hembras$Peso)))*100

# Proporción de datos 1 desviación estándar bajo la media
Fn(mean(datos_hembras$Peso) - sd(datos_hembras$Peso))
   

# Calcule proporción de datos 1 desviación en torno a la media
# Crea objeto con proporción sobre la media
ds_sobre <- (1-Fn(mean(datos_hembras$Peso) + sd(datos_hembras$Peso)))*100
ds_sobre
# Crea objeto con proporción bajo la media
ds_bajo <- Fn(mean(datos_hembras$Peso) - sd(datos_hembras$Peso))*100
ds_bajo
# Calcula proporción en torno a la media
100-(ds_sobre+ds_bajo)
