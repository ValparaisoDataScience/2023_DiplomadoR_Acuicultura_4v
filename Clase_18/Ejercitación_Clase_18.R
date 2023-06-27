# ------------------------------------------------------------------------------------------------------------
# Clase 18 - Script Modelamiento predictivo: Regresión logistica
# Dr. José Gallardo Matus, Dra. María Angélica Rueda Calderón
# 20 de junio 2023
# Diplomado en Análisis de Datos y Modelamiento Predictivo con Aprendizaje Automático para la Acuicultura
# ------------------------------------------------------------------------------------------------------------

# Cargar paquetes necesarios
library(car)
library(lmtest)
library(psych)
library(readxl)
library(nlme)
library(lme4)
library(stats)
library(boot)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(knitr)
library(gridExtra)
library(nortest)
library(reshape2)
library(pROC)

# Generar objeto My_Theme para personalizar el gráfico 
My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 20),
  axis.title.y = element_text(size = 20),
  axis.text.y = element_text(size = 20))


# Regresión logistica simple
maduracion <- read_excel("Maturation.xlsx")

maduracion$Genotype <- as.factor(maduracion$Genotype)

maduracion <- maduracion%>% 
  select("Fish","Genotype","Gonad","Mass","Maturation")


# División del conjunto de datos en entrenamiento y prueba
set.seed(123)  # Para reproducibilidad
indice_entrenamiento <- sample(nrow(maduracion), floor(0.7 * nrow(maduracion)))
datos_entrenamiento <- maduracion[indice_entrenamiento, ]
datos_prueba <- maduracion[-indice_entrenamiento, ]

# Ajuste del modelo de regresión logística en los datos de entrenamiento
mod_logit <- glm(Maturation ~ Mass, family = binomial, data = datos_entrenamiento)

# Predicción en los datos de prueba
predicciones <- predict(mod_logit, newdata = datos_prueba, type = "response")

# Conversión de las probabilidades en clases predichas
clases_predichas <- ifelse(predicciones > 0.5, 1, 0)

# Evaluación del modelo
matriz_confusion <- table(clases_predichas, datos_prueba$Maturation)

# Convertir la matriz de confusión en un data frame
matriz_confusion_df <- as.data.frame.matrix(matriz_confusion)

rownames(matriz_confusion_df) <- c("Predicho 0", "Predicho 1")
colnames(matriz_confusion_df) <- c("Observado 0", "Observado 1")

# Mostrar la matriz de confusión
kable(matriz_confusion_df)
table(datos_prueba$Maturation)

exactitud <- sum(diag(matriz_confusion)) / sum(matriz_confusion)
precision <- matriz_confusion[2, 2] / sum(matriz_confusion[, 2])
sensibilidad <- matriz_confusion[2, 2] / sum(matriz_confusion[2, ])
especificidad <- matriz_confusion[1, 1] / sum(matriz_confusion[1, ])

# Imprimir métricas de evaluación
cat("Matriz de confusión:\n")
print(matriz_confusion)
cat("\nExactitud:", round(exactitud,2))
cat("\nPrecisión:", precision)
cat("\nSensibilidad (Recall):", sensibilidad)
cat("\nEspecificidad:", especificidad)

# Agrego la columnas llamas Probabilidad y Clases_predichas a la base de datos de prueba
datos_prueba$Probabilidad <- predict(mod_logit, newdata = datos_prueba, type = "response")
datos_prueba$clases_predichas <- clases_predichas

# Crear un gráfico de dispersión con puntos coloreados según la probabilidad
ggplot(datos_prueba, aes(x = Mass, y = Maturation, color = Probabilidad)) +
  geom_point() +
  stat_function(fun = function(x){predict(mod_logit,
                                          newdata = data.frame(Mass = x),
                                          type = "response")})+
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Peso de cuerpo", y = "Probabilidad de Maduración", color = "Probabilidad") +
  theme_minimal()+ My_Theme

# Crear objeto de curva ROC
roc_obj <- roc(datos_prueba$Maturation, datos_prueba$clases_predichas)

# Obtener datos de sensibilidad y especificidad
roc_data <- coords(roc_obj, "all")

# Calcular el área bajo la curva ROC
auc <- auc(roc_obj)

# Crear gráfico de la curva ROC
ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity)) +
  geom_path(color = "darkblue", size = 1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Tasa de Falsos Positivos (1 - Especificidad)",
       y = "Tasa de Verdaderos Positivos (Sensibilidad)",
       title = "Curva ROC",
       subtitle = paste("AUC =", round(auc, 2))) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none")

