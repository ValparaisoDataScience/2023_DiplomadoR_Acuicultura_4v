# ---------------------------------------------------------------------------------------------------------------
# Clase 05 - Script ejercicios para manipulación de datos con tidyr y dplyr
# Dr. José Gallardo Matus
# 18 abril 2023
# Diplomado en Análisis de Datos y Modelamiento Predictivo con Aprendizaje Automático para la Acuicultura.
# ---------------------------------------------------------------------------------------------------------------

# Habilite paquetes

library(readxl) # Para importar datos a R

library(dplyr) # Para manipular datos

library(ggplot2) # Para hacer gráficos

library(tidyr) # Para manipular datos

# EJERCICIO 1
# a) Importe la hoja 1 del set de datos Parasitos a R.
# b) Filtre la columna "Variable" por el factor "parasitos" y ordene los datos a formato TIDY.
# c) Agregue una columna indice llamada peces con valores 1-9 con el comando mutate(). 

# EJERCICIO 2
# a) Importe y explore la hoja 2 del set de datos Parasitos a R.
# b) Una sus datos ordenados de parásitos con la variable sexo. 
# c) Elimine las columna "Variable" y reordene su objeto de la siguiente manera
# peces, especie, sexo, parásitos

# EJERCICIO 3
# a) Agrupe por Especie su objeto ordenado de parásitos y calcule n, promedio y desviación estándar.

# EJERCICIO 4
# a) Usando mutate() genere una variable derivada llamada log_par con el logaritmo del número de parásitos+1.

# EJERCICIO 5
# a) Importe y explore la hoja 3 del set de datos Parasitos a R.
# b) Usando mutate() genere una variable derivada llamada log_par con el logaritmo del número de parásitos+1.
# c) Usando mutate() genere una variable derivada llamada log_peso con el logaritmo del peso.
# d) Realice un gráfico de puntos para visualizar la relación entre el log+1 de parásitos en función del logaritmo del peso.