# ------------------------------------------------------------------------------------------------------------------
# Clase 05 - Script Solución ejercicios para manipulación de datos con tidyr y dplyr
# Dr. José Gallardo Matus
# 18 abril 2023
# Diplomado en Análisis de Datos y Modelamiento Predictivo con Aprendizaje Automático para la Acuicultura.

# ------------------------------------------------------------------------------------------------------------------

# Habilite paquetes
library(readxl) # Para importar datos a R

library(dplyr) # Para manipular datos

library(ggplot2) # Para hacer gráficos

library(tidyr) # Para manipular datos

# EJERCICIO 1
# a) Importe la hoja 1 del set de datos Parasitos a R.
# b) Filtre la columna "Variable" por el factor "parasitos" y ordene los datos a formato TIDY.
# c) Agregue una columna indice llamada peces con valores 1-9 con el comando mutate(). 

Parasitos <-read_excel("Parasitos.xlsx",na="NA")
ParaTidy<-Parasitos %>% filter(Variable == "parasitos") %>% gather("Especie","Parasitos",2:4)
ParaTidy <- ParaTidy %>% mutate(Peces=1:9)
ParaTidy

# EJERCICIO 2
# a) Importe y explore la hoja 2 del set de datos Parasitos a R.
# b) Una sus datos ordenados de parásitos con la variable sexo. 
# c) Elimine las columna "Variable" y reordene su objeto de la siguiente manera
# peces, especie, sexo, parasitos

Parasitos2 <-read_excel("Parasitos.xlsx",sheet = "sexo")
names(Parasitos2)[1]<-"Peces"
Tidy_Parasitos <- left_join(ParaTidy, Parasitos2, "Peces")
Tidy_Parasitos <- Tidy_Parasitos[,-1]
Tidy_Parasitos <- Tidy_Parasitos[, c(3, 1, 4, 2)]
Tidy_Parasitos

# EJERCICIO 3
# a) Agrupe por Especie su objeto ordenado de parásitos y calcule n, promedio y desviación estándar.

Mean_sd_par<-Tidy_Parasitos %>% group_by(Especie) %>% 
  summarize(N = n(), 
            Promedio_Parasitos = mean(Parasitos, na.rm=T), 
            SD_Parasitos = sd(Parasitos, na.rm=T))
Mean_sd_par

# EJERCICIO 4
# a) Usando mutate() genere UNA variable derivada llamada log_par con el logaritmo del número de parásitos+1.

Log_par_tidy<-Tidy_Parasitos %>% mutate(log_par = log10(Parasitos+1))
Log_par_tidy

# EJERCICIO 5
# a) Importe y explore la hoja 3 del set de datos Parasitos a R.
# b) Usando mutate() genere una variable derivada llamada log_par con el logaritmo del número de parásitos+1.
# c) Usando mutate() genere una variable derivada llamada log_peso con el logaritmo del peso.
# d) Realice un gráfico de puntos para visualizar la relación entre el log+1 de parásitos en función del logaritmo del peso.

tidy_all<-read_excel("Parasitos.xlsx",sheet = "tidy",na="NA")
tidy_all
Tidy_All<-tidy_all %>% mutate(log_par = log10(Parasitos+1)) %>% mutate(log_peso = log10(Peso))

ggplot(Tidy_All, aes(x=log_peso, y=log_par, color=Sexo))+
  geom_point(size=2)+
  labs(x= "Log(10) Peso(g)", y= "Log(10) Número de parásitos")+
  theme_gray()

