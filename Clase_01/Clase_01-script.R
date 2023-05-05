# ----------------------------------------------------------
# Clase 01 - Programación con R
# Dr. José Gallardo Matus
# 04 abril 2023
# Diplomado en Análisis de Datos y Modelamiento Predictivo con
# Aprendizaje Automático para la Acuicultura.
# ----------------------------------------------------------

#R reconoce funciones matemáticos
29+29
29*29
29==29

# Error en R
29 + diez

# Crear un objeto
diez <- 10
29 + diez

# Crea un objeto llamado parásitos.
# Agrega en el objeto parásitos el número 10, para representar 10 parásitos moviles

# Version R
version
R.version.string

# ¿Como citar R?
citation()

# En que directorio estoy
getwd()

# Listar librerías o packages disponibles en mi entorno de trabajo
search()

# Listar archivos en el directorio actual
list.files()

# Crear un objeto
ID <- c("Pez 1", "Pez 2")

# Características de un objeto
class(ID)
colnames(ID)

Sexo <- c(1,2) # Codificados 1= macho ; 2= Hembra
Longitud <- c(70, 78) 
Peso <- c(5500, 6000)
Peces <- data.frame(ID,Sexo,Longitud,Peso)

# Use los comandos class(), dim(), str() y summary() para identificar atributos de Sexo, Peces


# Explorar un objeto con [] y con $
Peces[1,1]
Peces[1,]
Peces[2,6] # da NULL
Peces[c(1:2),"Peso"]
Peces$ID
Peces$ID[-1] # excluye datos


# Usando [] y $ extraer y excluir datos de Sexo desde el data.frame Peces


# Listar objetos 
ls()

# Obtener ayuda de un comando
help("mean")
mean(Peces$Longitud)
mean(Peces$ID) # da error porque nombres no es numérico.
Peces$Sexo <- as.factor(Peces$Sexo)
mean(Peces$Sexo)
class(Peces$Sexo)
Peces$Sexo

# Ejercicio 1
# En grupos de tres personas cree una data.frame con datos de los compañeros.
# Primero: Cree 3 vectores con información de nombre, edad y empresa de los integrantes del grupo.
# Segundo: Elabore un data.frame uniendo los vectores.
# Tercero: Tranforme nombre y empresa a factor usando el comando as.factor().
# Cuarto: Evalue atributos del objeto data.frame creado usando class(), summary().

# Trabajando con matrices.
# Simularemos y exploraremos abundancia de 3 especies de bacterias de la microbiota
# que se han asociado a la pigmentación del salmón y como estan varían en el tiempo.
abundancia=c(1:21)
dim(abundancia)
M  = matrix(abundancia, ncol=3)
M
class(M)
dim(M) # dimensiones de una matriz
colnames(M) <- c("Bacillacea", "Mycoplasmataceae", "Pseudomonas")
M
rownames(M) <- paste("day",c(seq(1:7)))
M

M[3,]
M[,c(1,2)]

mean(M)
summary(M)
M>=4 # greater than or equal to
M!=12 # not equal to

# Trabajando con listas
proyecto <- list(Peces, M)
proyecto
# agregar nombres a una lista
proyecto <- list(Datos=Peces, Bacterias=M)
proyecto
str(proyecto)

# Acceso a componentes de una lista
proyecto$Datos
proyecto$Bacterias
proyecto[[2]] 

# Librerías, gráficas y funciones
help("datasets")
help(BOD)
summary(BOD)
hist(BOD$demand, main = "Demanda bioquimica de oxígeno", col = "red")
plot(BOD$Time, BOD$demand)
cor(BOD$Time, BOD$demand)

# Remover objetos de la sesión de trabajo
rm(list = ls())

# Tarea 2
# Investigue el paquete **fishdata** y habilite con la función library().
# Explore los datos de juvenile_metrics con summary().
# Realice un histograma de la variable standard_length y de la variable age con la función hist().
# Realice un gráfico de dispersión con las variables age (eje x) y standard_length (eje y).
# Finalmente calcule la correlación entre ambas variables con la función cor().


# Referencias
# Nguyen, C.D.H., Amoroso, G., Ventura, T. et al.
# Atlantic Salmon (Salmo salar L., 1758) Gut Microbiota Profile Correlates with Flesh Pigmentation: Cause or Effect?.
# Mar Biotechnol 22, 786–804 (2020). https://doi.org/10.1007/s10126-019-09939-1
