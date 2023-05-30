# ------------------------------------------------------------------------------------------------------------
# Clase 12 - Script Evaluación de supuestos ANOVA
# Dr. José Gallardo Matus, Dra. María Angélica Rueda Calderón
# 30 mayo 2023
# Diplomado en Análisis de Datos y Modelamiento Predictivo con Aprendizaje Automático para la Acuicultura.
# ------------------------------------------------------------------------------------------------------------

# Habilita paquetes

library(dplyr) # Para manipular datos

library(ggplot2) # Para hacer gráficos

library(tidyr) # Para manipular datos

library(car) # Para el análisis de regresión y diagnóstico

library(lmtest) # Para realizar pruebas de hipótesis sobre los coeficientes de regresión

library(knitr) # Para hacer tablas

#install.packages("equatiomatic")

#remotes::install_github("datalorax/equatiomatic")

library(equatiomatic) # Para hacer la ecuación del modelo

library(texPreview) # Para visualizar en la solapa Viewer la ecuación del modelo

library(sjPlot) # Para generar la tabla con la información del modelo ajustado en la solapa Viewer

library(readxl) # para cargar archivos de excell

# Cargar la base de datos
my_data <- read_excel("my_data.xlsx")

# Transformar la variable Tratamiento de character a factor
my_data$Tratamiento <- as.factor(my_data$Tratamiento)

# Hacer tema con características a aplicar en los gráficos 
My_Theme = theme(
  title = element_text(size = 12,face = "bold"),
  axis.title.x = element_text(size = 12,face = "bold"),
  axis.text.x = element_text(size = 12,face = "bold"),
  axis.title.y = element_text(size = 12,face = "bold"),
  axis.text.y = element_text(size = 12,face = "bold"))

# Crear el boxplot utilizando ggplot2
ggplot(my_data, aes(x = Tratamiento, y = Peso, fill=Tratamiento)) +
  geom_boxplot() +
  scale_fill_manual(values = c("coral", "turquoise"))+
  labs(x = "Tratamiento", y = "Peso") +
  ggtitle("Boxplot de Peso por Tratamiento")+
  theme_bw()+My_Theme

# ANOVA en la forma de modelo lineal
lm.aov <- lm(Peso ~ Tratamiento, data = my_data)

# Genera la ecuación del modelo
tex_preview(extract_eq(lm.aov))

# Muestra el modelo con los coeficientes estimados
tex_preview(extract_eq(lm.aov,use_coefs = TRUE))

# Muestra la información del modelo ajustado en formato tabla
tab_model(lm.aov, show.se = TRUE, show.aic=TRUE)

# Calcular el promedio por cada tratamiento 

media_tratamiento <- my_data%>%
  select(Peso, Tratamiento)%>%
  group_by(Tratamiento)%>%
  summarise(Promedio= mean(Peso))

merged_data0 <- inner_join(my_data, media_tratamiento, by = "Tratamiento")

# Mostrar la tabla formateada con kable
kable(merged_data0, digits = 2)

# Generar base de datos con valores predichos y residuos del modelo 
merged_data <- cbind(my_data, Residuos = residuals(lm.aov), Predichos= fitted.values(lm.aov))

# Filtrar las siguientes cinco observaciones de "Control"
control_data <- merged_data %>%
  filter(Tratamiento == "Control") %>%
  slice(1:5)

# Filtrar las primeras cinco observaciones de "Dieta 1"
dieta1_data <- merged_data %>%
  filter(Tratamiento == "Dieta 1") %>%
  slice(1:5)

# Unir los datos de "Dieta 1" y "Control"
final_data <- rbind(control_data,dieta1_data)

# Mostrar la tabla formateada con kable
kable(final_data, digits = 2)

# Hace el ANOVA del modelo ajustado
anova(lm.aov) %>% kable(digits = 3)

# Supuesto de independencia método gráfico: análsis de residuales
plot(lm.aov$residuals, pch=20, col = "blue",
     cex.lab=1.25, cex.axis=1.25)

# Supuesto de independencia: Prueba de Durbin-Watson
dwtest(Peso ~ Tratamiento, data = my_data,
       alternative = c("two.sided"), 
       iterations = 15) #library(lmtest)

# Supuesto de Homogeneidad de varianzas: análisis de residuales
plot(lm.aov, 1, pch=20, col = "blue",
     cex.lab=1.5, cex.axis=1.5, sub = "")

# Supuesto de homogeneidad de varainzas: Prueba de Levene
lv <- leveneTest(Peso ~ Tratamiento, data = my_data,
                 center = "median") # library(car)  
lv %>% kable(digits = 3)

# Supuesto de normalidad: análisis de residuales 
plot(lm.aov, 2, pch=20, col = "blue")

# Supuesto de normalidad: análisis de residuales (2)
qqPlot(lm.aov) # library(car)

# Supuesto de normalidad: Prueba de Shapiro-Wilks
aov_residuals <- residuals(object = lm.aov)
shapiro.test(x= aov_residuals)

