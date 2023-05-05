# ----------------------------------------------------------
# Clase 04 - Script exploratorio de datos con ggplot2
# Dr. José Gallardo Matus
# 15 abril 2023
# Diplomado en Análisis de Datos y Modelamiento Predictivo con Aprendizaje Automático para la Acuicultura.
# ----------------------------------------------------------

# habilitar paquete ggplot2
library(ggplot2)

# Explore el objeto CO2 con el comando help
help(CO2)

# Intento de gráfica con función ggplot.
# La gráfica queda vacía pues falta indicar el tipo de gráfica que deseamos
ggplot(CO2, aes(uptake))

# Histrograma con ggplot. 
ggplot(CO2, aes(uptake))+
 geom_histogram()

# Agregamos titulo y nombre de los ejes
ggplot(CO2, aes(uptake))+
  geom_histogram()+
  labs(title="Histograma", x="Consumo de CO2", 
       y="Frecuencia")

# Modificamos tamaño de etiquetas
My_Theme = theme(
  axis.title.x = element_text(size = 18),
  axis.text.x = element_text(size = 18),
  axis.title.y = element_text(size = 18),
  axis.text.y = element_text(size = 18))

ggplot(CO2, aes(uptake))+
  geom_histogram()+
  labs(title="Histograma", x="Consumo de CO2", 
       y="Frecuencia") +
  My_Theme

# Gráfica con dos ejes y tratamiento pero incompleta
ggplot(CO2, aes(x=Treatment, y=uptake))

# Gráfica de boxplot
ggplot(CO2, aes(x=Treatment, y=uptake))+
geom_boxplot()

# Agrega diferentes colores a un tratamiento
ggplot(CO2, aes(x=Treatment, y=uptake, fill=Treatment))+
  geom_boxplot()

# Agrega mismo color a todos los tratamientos
ggplot(CO2, aes(x=Treatment, y=uptake))+
  geom_boxplot(color="blue4", fill="blue")

# Agrega transparencia
ggplot(CO2, aes(x=Treatment, y=uptake))+
  geom_boxplot(color="blue4", fill="blue", alpha=0.2)

# Intente otros colores
# http://sape.inf.usi.ch/quick-reference/ggplot2/colour
ggplot(CO2, aes(x=Treatment, y=uptake))+
  geom_boxplot(color="blue4", fill="blue", alpha=0.2)


# Cambia colores de forma manual en gráfica de boxplot
ggplot(CO2, aes(x=Treatment, y=uptake))+
  geom_boxplot(aes(color = Treatment, fill = Treatment), alpha=0.2)+
  scale_color_manual(values = c("blue4", "red4")) +
  scale_fill_manual(values = c("blue", "red"))

# Crea objeto

g <- ggplot(CO2, aes(x=Treatment, y=uptake))+
  geom_boxplot(aes(color = Treatment, fill = Treatment), alpha=0.2)+
  scale_color_manual(values = c("blue4", "red4")) +
  scale_fill_manual(values = c("blue", "red"))

class(g)

# Cambia tema de fondo
g+My_Theme

# Seleccione otros temas
# https://ggplot2.tidyverse.org/reference/ggtheme.html
