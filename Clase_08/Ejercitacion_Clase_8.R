# ------------------------------------------------------------------------------------------------------------------
# Clase 08 - Inteligencia Artificial
# Dr. José Gallardo Matus & Dra. María Angélica Rueda Calderón
# 29 abril 2023
# Diplomado en Análisis de Datos y Modelamiento Predictivo con Aprendizaje Automático para la Acuicultura.
# ------------------------------------------------------------------------------------------------------------------

# Habilita paquetes

library(readxl) # Para importar datos a R

library(dplyr) # Para manipular datos

library(tidyr) # Para manipular datos

library(ggplot2) # Para hacer gráficos

library(gridExtra) # Para que salgan varios graficos en simultaneo

library(tidyverse) # Para manipular datos

# Realice las siguientes preguntas al Chat de Rstudio

# 1). Simule un dataframe con 100 observaciones y que tenga las variables sexo, 
#     peso, talla, longitud, dosis (que sean 4 dosis), estado de madurez (0 inmaduro y 1 maduro) 
#     usar semilla para fijar datos simulados para salmones del atlantico

set.seed(123) # fijamos la semilla

datos_simulados <- tibble(
  sexo = sample(c("M", "H"), 100, replace=TRUE),
  peso = rnorm(100, mean=5, sd=2),
  talla = rnorm(100, mean=10, sd=3),
  longitud = rnorm(100, mean=15, sd=4),
  dosis = rep(c("Dosis1", "Dosis2", "Dosis3", "Dosis4"), each=25),
  estado_madurez = sample(0:1, 100, replace=TRUE)
)

# Visualizar los primeros registros
head(datos_simulados)

# 2). Realice un histogramas de la variable peso con color coral y longitud con color lightblue
#     y use la función grid.arrange(). Quiero que los histogramas se hagan con ggplot2 

# Histograma de peso
histograma_peso <- ggplot(datos_simulados, aes(x = peso, fill = "peso")) +
  geom_histogram(binwidth = 1, alpha = 0.8) +
  scale_fill_manual(values = "coral", guide = "none") +
  labs(x = "Peso")

# Histograma de longitud
histograma_longitud <- ggplot(datos_simulados, aes(x = longitud, fill = "longitud")) +
  geom_histogram(binwidth = 1, alpha = 0.8) +
  scale_fill_manual(values = "lightblue", guide = "none") +
  labs(x = "Longitud")

# Combinamos los gráficos
grid.arrange(histograma_peso, histograma_longitud, nrow = 1)

# 3). Genere un boxplot de la talla versus sexo  y que coloree para cada sexo usando color
#     darkolivegreen2 y darkslategray1 de en ggplot2

# Boxplot
boxplot_talla_sexo <- ggplot(datos_simulados, aes(x = sexo, y = talla, fill = sexo)) +
  geom_boxplot() +
  scale_fill_manual(values = c("darkolivegreen2", "darkslategray1")) +
  labs(x = "Sexo", y = "Talla") +
  theme_classic()

boxplot_talla_sexo

# 4). Genere un boxplot de peso vs dosis por sexo use face_wrap() usando ggplot2

# Crear boxplot con facet_wrap() para los paneles de los valores de sexo
boxplot_peso_dosis <- ggplot(datos_simulados, aes(x = dosis, y = peso)) +
  geom_boxplot() +
  facet_wrap(~sexo, ncol = 2) +
  labs(
    title = "Boxplot de Peso por Dosis y Sexo",
    y = "Peso",
    x = "Dosis",
    caption = "Fuente: datos simulados"
  )

boxplot_peso_dosis


# 5). Realice un grafico de dispersión de peso vs longitud y 
#     que colore los puntos por sexo usando ggplot2

# Gráfico de dispersión
ggplot(datos_simulados, aes(x = longitud, y = peso, color = sexo)) + 
  geom_point() +
  labs(x = "Longitud", y = "Peso", color = "Sexo")

# Actividad grupal

# Use la base de datos salmon.xlsx y pida al chat que le recomiende pasos para el proceso
# de limpieza de datos usando los paquetes dplyr y tidyr (imputar datos faltantes, 
# unificar duplicados, identificar datos outliers y quitarlos)
# Realice después de tener lista la base de datos un análisis exploratorio de datos
# que le recomiende el chat usando ggplot2 para los graficos
