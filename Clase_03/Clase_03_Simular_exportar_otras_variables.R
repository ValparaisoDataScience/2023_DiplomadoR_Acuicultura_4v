# ----------------------------------------------------------
# Clase 03 - Variables Aleatorias discretas y cualitativas
# Dr. José Gallardo y Dra. María Angélica Rueda Calderón
# 15 abril 2023
# Diplomado en Análisis de Datos y Modelamiento Predictivo con Aprendizaje Automático para la Acuicultura.
# ----------------------------------------------------------

# Habilita paquetes para importar/exportar datos
library(readr)
library(xlsx)

# Habilita paquetes para simular algunas variables aletaorias
library(Rlab)
library(MASS)

# ¿Qué tipos de distribuciones hay en R?

help(Distributions)


# Simular variable con distribucion Bernoulli
# Hasan, N.A., Haque, M.M., Hinchliffe, S., & Guilder, J.A. (2020).
# A sequential assessment of WSD risk factors of shrimp farming in Bangladesh:
# Looking for a sustainable farming system. Aquaculture, 526, 735348.

# La prevalencia en las granjas de Bangladesh para el virus de mancha blanca puede ser tan alta como el 70-80%.

#semilla para fijar resultados cada vez que se corre la simulación
set.seed(1) 
# Simula experimento de colectar un camarón desde una granja en Bangladesh.
# Resultados posible: 0= sin virus; 1= con virus
rbern(1, 0.80)

# Simular 100 ensayos Bernoulli.
# Muestreo aleatorio de 100 camarones desde una granja en Bangladesh.
set.seed(1)
rbern(100, 0.80)
# Tabula resultado del muestreo
table(rbern(100, 0.80))

# Ejercicio 1. Simula y tabula muestreo aleatorio de 100 camarones con una prevalencia de 20%.


# Simular variable que distribuye Binomial negativa
# Jeong, J., & Revie, C.W. (2020). Appropriate sampling strategies to estimate sea lice prevalence
# on salmon farms with low infestation levels. Aquaculture 518: 734858

# n = 1000 peces
# mu = media de parásitos por pez = 10
# theta = 30 (número arbitrario para estimar la varianza)

Caligus <- data.frame(rnegbin(1000, 10, 30))
dat <- as.data.frame(table(Caligus))
colnames(dat) <- c("Abundancia parasitos","Frecuencia")
barplot(dat$Frecuencia, col = "coral", names.arg=dat$`Abundancia parasitos`, main="Abundancia de parásitos.")

# Ejercicio 2. Simula y grafica la distribución de un muestreo aleatorio de parásitos.
# n= 500 peces ; mu = 3 ; Reduce la varianza con theta = 10


# Simular base de datos con variable aleatoria discretas y variables categóricas paso a paso
set.seed(1)
Animal <- seq(1:100)
Madurez <- rbern(100, 0.65)
inf_caligus <- rbinom(100,8,0.6) 
Sexo <- sample(c("Hembra","Macho"), size = 100, replace = TRUE)
Nivel_cataratas <- sample(c("Alto","Medio","Bajo"), size = 100, replace = TRUE)

datos_all <- data.frame(Animal,Madurez, inf_caligus, Sexo, Nivel_cataratas)

# Exportar objeto datos_all en formato .txt, .csv y .xlsx

write_delim(x = datos_all, file = "datos_all.txt", delim = ";")

write_csv(x = datos_all, file = "datos_all.csv")

write.xlsx(datos_all, "datos_all.xlsx", sheetName = "Base_datos", col.names = TRUE, row.names = FALSE)


## Ejercicio 3: En grupos
## Explore y grafique las variables del objeto datos_all.
# Use as.factor(), summary().
# Use table() para dos variables categóricas a la vez (ej. madurez y Sexo).
# Use barplot(), debe crear objeto con tabla resumen de la variable antes de graficar.




















# Solución ejercicio 3
datos_all$Madurez <- as.factor(datos_all$Madurez)
datos_all$inf_caligus <- as.factor(datos_all$inf_caligus)
datos_all$Sexo <- as.factor(datos_all$Sexo)
datos_all$Nivel_cataratas <- as.factor(datos_all$Nivel_cataratas)
summary(datos_all)

table(datos_all$Madurez, datos_all$Sexo)
table(datos_all$inf_caligus, datos_all$Sexo)
table(datos_all$Nivel_cataratas, datos_all$Sexo)

madurez_resumen <- table(datos_all$Madurez)
barplot(madurez_resumen)

caligus_resumen <- table(datos_all$inf_caligus)
barplot(caligus_resumen)

cataratas_resumen <- table(datos_all$Nivel_cataratas)
barplot(cataratas_resumen)
