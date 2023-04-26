# ------------------------------------------------------------------------------------------------------------------
# Clase 06 - Script de manipulación avanzada de datos con tidyr y dplyr
# Dr. José Gallardo Matus & Dra. María Angélica Rueda Calderón
# 25 abril 2023
# Diplomado en Análisis de Datos y Modelamiento Predictivo con Aprendizaje Automático para la Acuicultura.
# ------------------------------------------------------------------------------------------------------------------

# Habilita paquetes
library(readxl) # Para importar datos a R

library(dplyr) # Para manipular datos

library(tidyr) # Para manipular datos

library(ggplot2) # Para hacer gráficos

library(gridExtra)

# Importar base de datos
salmon <- read_excel("salmon.xlsx", na = "NA")

# Ordenar la variable Sample de menor a mayor usando arrange
salmon <- salmon%>% 
          arrange(Sample)

# Transformar variables tipo chr a factor
salmon$Sample <- as.factor(salmon$Sample)
salmon$Ploidy <- as.factor(salmon$Ploidy)
salmon$Family <- as.factor(salmon$Family)
salmon$Tank <- as.factor(salmon$Tank)

# Identificar datos atípicos, duplicados o faltantes (NA) con summary
summary(salmon)

# Ver la dimensión de la nueva base de datos con datos faltantes
dim(salmon)

# Omitir/quitar datos faltantes na.omit()
salmon_new <- na.omit(salmon)

# Ver la dimensión de la nueva base de datos sin datos faltantes 
dim(salmon_new)

# Reemplazar datos faltantes por la media, mediana con replace_na()
  
salmon <- salmon%>% mutate(Weight = replace_na(Weight,median(Weight, na.rm = TRUE)),
                           Length = replace_na(Length,median(Length, na.rm = TRUE)))


# Identificar información duplicada en todas las columnas usando duplicated()
dups_all <- salmon %>% filter(duplicated(.))


# Identificar observaciones que están duplicadas para el mismo individuo (Sample) usando duplicated()
dups_id <- salmon %>% filter(duplicated(Sample))

# Unificar observaciones duplicadas usando distinct()
salmon_unified <- salmon %>% distinct(Sample, .keep_all = TRUE)

# Muestra las diez primeras observaciones de la nueva base de datos con head()

head(salmon_unified,10)

# Identificar valores atípicos (outliers)
datos_outliers_bajos <- salmon_unified %>%
  filter(Weight < (quantile(Weight, 0.25) - 1.5*IQR(Weight)) | Length < (quantile(Length, 0.25) - 1.5*IQR(Length))) 

datos_outliers_altos <- salmon_unified %>%
  filter(Weight > (quantile(Weight, 0.75) + 1.5*IQR(Weight)) | Length > (quantile(Length, 0.75) + 1.5*IQR(Length)))

# Unir las observaciones de datos_outliers_bajos y datos_outliers_altos con rbind
datos_outliers <- rbind(datos_outliers_bajos,datos_outliers_altos)

# Generar base de datos sin valores atípicos (outliers)
datos_sin_outliers <- anti_join(salmon_unified, datos_outliers, by = "Sample")

# Modificar dato atípico usando mutate()
salmon_mod <- salmon_unified %>%
  mutate(Weight = ifelse(Sample == "M12", 54, Weight))

# facet_wrap()```**: Permite dividir una gráfica en paneles o subgráficos basados en una o varias variables categóricas.  

ggplot(salmon_unified, aes(x = Family, y = Weight, fill = Ploidy)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("turquoise", "coral")) +
  facet_wrap(~ Ploidy) +
  labs(title = "Weight distribution by family and ploidy level",
       x = "Family",
       y = "Weight (g)") +
  theme_bw() +
  geom_text(data = filter(salmon_unified, Weight > quantile(Weight, 0.75) + 1.5*IQR(Weight)),
            aes(label = Sample), hjust = -0.2, vjust = 0.5)


# grid.arrange(p, q, r, ncol = 3) : se utiliza para combinar varios gráficos de ggplot en una sola figura.

# Hacer tema con características a aplicar en los gráficos 
theme_customizado <-  theme(axis.text.x = element_text(size = 12, face = "bold", hjust = 0.5, vjust = 0.5), 
                            plot.margin = unit(c(1,0.5,1,1), "cm"))

# Hacer gráficos con ggplot2 y mostrarlos en una sola figura con grid.arrange()
p <- ggplot(salmon_unified, aes(x = Weight)) + 
  geom_histogram(fill="coral", bins=30) +
  theme_bw()+
  theme_customizado

q <- ggplot(data = salmon_unified, aes(x = reorder(Family, Weight, FUN = mean), y = Weight))+
  geom_boxplot(fill="darkolivegreen1")+
  labs(x="Families", y="Weight (g)")+ 
  geom_hline(yintercept = mean(salmon_unified$Weight), linetype = "solid", color = "blue")+
  theme_bw()+
  theme_customizado


r <- ggplot(data = salmon_mod, aes(x = reorder(Family, Weight, FUN = mean), y = Weight))+
  geom_boxplot(fill="cyan3")+
  labs(x="Families", y="Weight (g)")+ 
  geom_hline(yintercept = mean(salmon_mod$Weight), linetype = "solid", color = "blue")+
  theme_bw()+
  theme_customizado


grid.arrange(p, q, r, ncol = 3)

# Ejercicio individual
# Use el objeto datos_sin_outliers creado en la clase para:  
# 1) Hacer histograma de Length,
# 2) Hacer boxplot de Length por Family con la recta horizontal que indica el promedio de Lenght 
# 3) Hacer boxplot de Lenght por Family con la recta horizontal que indica el promedio de Lenght y por Tank use facet_wrap(~ Tank)
# 4) Coloque en una sola figura los graficos de los items 1, 2 y 3 usando grid.arrange()





# Respuesta del ejercicio
# Hacer gráficos con ggplot2 y mostrarlos en una sola figura con grid.arrange()
#1). 
s <- ggplot(datos_sin_outliers, aes(x = Length)) + 
  geom_histogram(color="black",fill="cadetblue1", bins=30) +
  theme_bw()+
  theme_customizado

#2).
t <- ggplot(data = datos_sin_outliers, aes(x = reorder(Family, Length, FUN = mean), y = Length))+
  geom_boxplot(fill="deeppink2")+
  labs(x="Families", y="Length (cm)")+ 
  geom_hline(yintercept = mean(datos_sin_outliers$Length), linetype = "solid", color = "blue")+
  theme_bw()+
  theme_customizado

#3).
u <- ggplot(data = datos_sin_outliers, aes(x = reorder(Family, Length, FUN = mean), y = Length))+
  geom_boxplot(fill="aquamarine")+
  labs(x="Families", y="Weight (g)")+ 
  geom_hline(yintercept = mean(datos_sin_outliers$Length), linetype = "solid", color = "blue")+
  theme_bw()+ facet_wrap(~ Tank)
  theme_customizado

#4).
  grid.arrange(s, t, u, ncol = 3)
