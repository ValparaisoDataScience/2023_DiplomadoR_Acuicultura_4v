# ------------------------------------------------------------------------------------------------------------
# Clase 05 - Script Manipulación de datos con tidyr y dplyr
# Dr. José Gallardo Matus, Dra. María Angélica Rueda, Ing. Carlos Gutierrez
# 18 abril 2023
# Diplomado en Análisis de Datos y Modelamiento Predictivo con Aprendizaje Automático para la Acuicultura.
# ------------------------------------------------------------------------------------------------------------


# Lectura recomendada https://es.r4ds.hadley.nz/
  
# Habilita paquetes
library(readxl) # Para importar datos a R

library(dplyr) # Para manipular datos

library(ggplot2) # Para hacer gráficos

library(tidyr) # Para manipular datos

# PAQUETE DPLYR: EL OPERADOR PIPE (TUBERÍA).

# dplyr usa el operador pipe %>% como una tubería para enlazar un data.frame con una o más funciones.

x <- rnorm(5)
y <- rnorm(5)
dat <- data.frame(x,y)
dat
max(dat) 
dat %>% max
dat %>% arrange(y) # Ordena filas de un data.frame por el valor de alguna columna

# Importar messy datos
messy <- read_excel("Peces.xlsx") # Carga el set de datos
summary(messy)

# Importar messy datos con datos faltantes
messy <- read_excel("Peces.xlsx", na="NA")
summary(messy)
head(messy)  # Muestra los primeros datos del data set

# Filtrar variable peso con tubería
messy %>% filter(Variable == "peso")

# Colapsar columna Especie
messy %>% filter(Variable == "peso") %>% gather("Especie","Peso",2:4)

# Crear objeto Peso
Datos <- messy %>% filter(Variable == "peso") %>% gather("Especie","Peso",2:4)

# Agregar indice peces
Datos %>% mutate(peces=1:9)
Datos <- Datos %>% mutate(peces=1:9)

# EJERCICIO 1 - ver script_ejercicios.R - grupal

# Unir Objetos Datos y Sexo.
sexo <- read_excel("Peces.xlsx", sheet = 2)
head(sexo)
tidy_data <- left_join(Datos, sexo, "peces")
head(tidy_data)

# Eliminar y ordena columnas.
tidy_data <- tidy_data[,-1] 
tidy_data <- tidy_data[, c(3, 1, 4, 2)]
head(tidy_data)

# EJERCICIO 2 - ver script_ejercicios.R - grupal

# FUNCIÓN SELECT()
# Permite extraer o seleccionar variables/columnas específicas de un data.frame.
select(tidy_data, Especie, Sexo)

# FUNCIÓN SELECT() CON PIPE
tidy_data %>% select(Especie, Sexo)

# FUNCIÓN FILTER() CON PIPE
# **filter()**: Para filtrar desde una tabla de datos un subconjunto de filas.
# Ej. solo un nivel de de un factor, observaciones que cumplen algún criterio (ej. > 20).
tidy_data %>% filter(Sexo == "Macho")

# MÚLTIPLES FUNCIONES Y TUBERÍAS
tidy_data %>% select(Especie, Sexo, Peso) %>% 
  filter(Sexo == "Macho")

# FUNCIÓN SUMMARIZE()
tidy_data %>% select(Especie, Sexo, Peso) %>% 
          summarize(n = n(), 
                    Promedio_Peso = mean(Peso), 
                    Maximo_Peso = max(Peso))

# FUNCIÓN SUMMARIZE() removiendo NA
tidy_data %>% select(Especie, Sexo, Peso) %>% 
  summarize(n = n(), 
            Promedio_Peso = mean(Peso, na.rm=T), 
            Maximo_Peso = max(Peso, na.rm=T))

# FUNCIÓN SUMMARIZE() + GROUP_BY()
# Permite agrupar filas con base a los niveles de alguna variable o factor.
tidy_data %>% group_by(Especie) %>% 
          summarize(n = n(), 
            Promedio_Peso = mean(Peso, na.rm=T), 
            Maximo_Peso = max(Peso, na.rm=T))

# EJERCICIO 3 - ver script_ejercicios.R - grupal

# FUNCIÓN MUTATE()
# Permite calcular nuevas variables "derivadas", ej. proporciones, tasas, log.
tidy_data <- read_excel("Peces.xlsx", sheet = 3, na="NA") 

tidy_data %>% select(Especie, Peso, Parasitos) %>%
  mutate(Densidad_parasitos = Parasitos/Peso)
tidy_data

# EJERCICIO 4 - ver script_ejercicios.R - individual

# GRÁFICA DE DISPERSIÓN CON GGPLOT2

tidy_data %>% ggplot(aes(x=Peso, y=Parasitos, color=Sexo))+
  geom_point(size=2)+
  labs(x= "Peso(g)", y= "Número de parásitos")+
  theme_bw()

# EJERCICIO 5 - ver script_ejercicios.R - individual
