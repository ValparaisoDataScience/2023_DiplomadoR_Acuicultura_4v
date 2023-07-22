# ------------------------------------------------------------------------------------------------------------
# Clase 25 Modelamiento predictivo: Máquinas de Soporte Vectorial (SVM) y Sobrevivencia
# Dr. José Gallardo Matus, Dra. María Angélica Rueda Calderón
# 22 de julio 2023
# Diplomado en Análisis de Datos y Modelamiento Predictivo con Aprendizaje Automático para la Acuicultura
# ------------------------------------------------------------------------------------------------------------

# Cargar paquetes necesarios
library(readxl) # Proporciona funciones para leer datos en formatos de archivos de Excel
library(dplyr) # Ofrece una serie de funciones para manipulación y transformación de datos
library(ggplot2) # Permite crear visualizaciones gráficas de alta calidad 
library(ggpmisc) # Proporciona complementos y funciones adicionales para gráficos de ggplot2, como etiquetas de ecuaciones y líneas de ajuste
library(pROC) # Permite calcular y visualizar curvas ROC (Receiver Operating Characteristic) y otras métricas relacionadas con la evaluación de modelos de clasificación.
library(randomForest) # Implementa el algoritmo Random Forest para construir y analizar modelos de ensamble basados en árboles de decisión
library(caret) # Proporciona herramientas para el preprocesamiento de datos, selección de características y ajuste de modelos de aprendizaje automático.
library(rpart.plot) #Permite visualizar árboles de decisión generados con el paquete rpart
library(rpart) # Implementa el algoritmo CART (Classification and Regression Trees) para construir árboles de decisión.
library(pander) # Facilita la generación de tablas
library(e1071) # Implementa el algoritmo de maquinas de soporte vectorial tanto para regresión como para clasificación
library(gridExtra) # Permite crear fácilmente gráficos de alto nivel y organizarlos en diseños de cuadrícula.
library(kernlab) # Proporciona implementaciones para SVM, regresión ridge, regresión logística, clustering y más.
library(plotly) #  Gennera gráficos interactivos.
library(drda) # Fit logistic functions to observed dose-response continuous

# Generar objeto My_Theme para personalizar el gráfico 
My_Theme <- theme(axis.title.x = element_text(size = 20, face = "bold"),
                  axis.text.x = element_text(size = 20,face = "bold"),
                  axis.title.y = element_text(size = 20, face = "bold"),
                  axis.text.y = element_text(size = 20,face = "bold"))

# Importar la base de datos
datos <- read_excel("Maturation.xlsx")


# Transformar la variable Maturation a factor
datos$Maturation <- as.factor(datos$Maturation)

# Cambiar la clase 1 como la clase positiva
datos$Maturation <- relevel(datos$Maturation, ref = "1")

# Generar base de datos con variables de interes
Datos_def <- datos%>% 
  select("Mass","SGR", "Length","GSI","Maturation")

# División del conjunto de datos en entrenamiento y prueba
set.seed(123)
train_indices_all <- createDataPartition(Datos_def$Maturation, p = 0.7, list = FALSE)
train_data_all <- Datos_def[train_indices_all, ]
test_data_all <- Datos_def[-train_indices_all, ]

# Ajustar el modelo SVM radial con búsqueda de parámetros y validación cruzada
set.seed(123)
tune_grid_svm <- expand.grid(sigma = c(0.1, 1, 10), C = c(0.1, 1, 10))

modelo_train <- train(Maturation ~ Mass + SGR + GSI, data = Datos_def, method = "svmRadial",
                      trControl = trainControl(method = "cv", number = 5),
                      tuneGrid = tune_grid_svm)

# Ajustar el modelo SVM con los mejores parámetros encontrados
set.seed(123)
modelo_svm <- svm(Maturation ~ Mass + SGR + GSI, data = train_data_all, kernel = "radial",
                  gamma = modelo_train$bestTune$sigma, cost = modelo_train$bestTune$C)

# Predecir las etiquetas para los datos de prueba
predicciones_SVM <- predict(modelo_svm, test_data_all)

# Agregar las etiquetas predichas al conjunto de datos de prueba
test_data_all$pred_SVM <- predicciones_SVM

# Crear una matriz de datos que combine las características y las predicciones
datos_grafico <- cbind(test_data_all[, c("Mass", "SGR", "GSI")], Predicciones = predicciones_SVM)

# Graficar los puntos de datos coloreados según las predicciones
grafico_SVM <- plot_ly(data = datos_grafico, x = ~Mass, y = ~SGR, z = ~GSI, color = ~Predicciones,
                   colors = c("#1f78b4", "#e31a1c"), 
                   marker = list(size = 5, opacity = 0.8),
                   type = "scatter3d", mode = "markers")

# Agregar etiquetas a los ejes
grafico_SVM <- layout(grafico_SVM, scene = list(xaxis = list(title = "Mass"),
                                        yaxis = list(title = "SGR"),
                                        zaxis = list(title = "GSI")))

# Mostrar el gráfico
print(grafico_SVM)

# Matriz de confusión - SVM
confusion_SVM <- confusionMatrix(predicciones_SVM, test_data_all$Maturation)

# Obtener la matriz de confusión con las etiquetas modificadas
confusion_SVM_mod  <- confusion_SVM[["table"]]
rownames(confusion_SVM_mod) <- c("Maduro","Inmaduro")
colnames(confusion_SVM_mod) <- c("Maduro","Inmaduro")

pander(confusion_SVM_mod, caption = "Matriz de Confusión SVM",
       split.cells = 20, style = "grid")

# Extraer la metrica accuracy del objeto confusion_SVM
ACC_SVM<- round(confusion_SVM$overall['Accuracy'],2);ACC_SVM

# Extraer la metrica sensibilidad del objeto confusion_SVM
SEN_SVM <- round(confusion_SVM$byClass['Sensitivity'],2);SEN_SVM

# Extraer la metrica especificidad del objeto confusion_SVM
SPE_SVM <- round(confusion_SVM$byClass['Specificity'],2);SPE_SVM

# Crear objeto de curva ROC
roc_obj_SVM <- roc(as.numeric(test_data_all$Maturation), as.numeric(test_data_all$pred_SVM))

# Obtener datos de sensibilidad y especificidad
roc_data_SVM <- coords(roc_obj_SVM, "all")

# Calcular el área bajo la curva ROC
auc_SVM <- auc(roc_obj_SVM)

# Crear gráfico de la curva ROC
ggplot(roc_data_SVM, aes(x = 1 - specificity, y = sensitivity)) +
  geom_path(color = "darkblue", size = 1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Tasa de Falsos Positivos (1 - Especificidad)",
       y = "Tasa de Verdaderos Positivos (Sensibilidad)",
       title = "Curva ROC",
       subtitle = paste("AUC =", round(auc_SVM, 2))) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none")+My_Theme

# Create a data frame Dose - Response.
data <- data.frame(Dose = log(rep(c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100), each = 3)),
                   Relative_Viability = c(0.877362, 0.812841, 0.883113, 0.973494, 0.945769, 0.999422,
                                          0.888961, 0.735539, 0.842040, 0.518041, 0.519261, 0.501252,
                                          0.253209, 0.183937, 0.170719, 0.049249, 0.070804, 0.091425,
                                          0.041096, 0.050012, 0.092564))


set.seed(123) # Establecer una semilla para reproducibilidad
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Realizar una búsqueda en cuadrícula para seleccionar el mejor valor de C
tune_result <- tune(svm, Relative_Viability ~ Dose, data = data,
                    ranges = list(C = c(0.01, 0.1, 1, 10, 100)))


# Obtener el mejor modelo SVM con el valor óptimo de C
best_svm_model <- tune_result$best.model
best_C <- best_svm_model$cost

# Ajustar el modelo SVM con el valor óptimo de C
best_svm_model <- svm(Relative_Viability ~ Dose, data = train_data, cost = best_C)

# Realizar predicciones en el conjunto de prueba
predicted_values <- predict(best_svm_model, newdata = test_data)

# Agregar las etiquetas predichas al conjunto de datos de prueba
test_data$pred_SVM_reg <- predicted_values

# Calcular el error cuadrático medio (MSE)
mse <- mean((predicted_values - test_data$Relative_Viability)^2);mse

# Crear un gráfico interactivo con las predicciones
plot <- plot_ly() %>%
  # Agregar los datos observados en el conjunto de prueba
  add_trace(x = test_data$Dose, y = test_data$Relative_Viability,
            type = "scatter", mode = "markers", name = "Observados", marker = list(color = "blue")) %>%
  # Agregar las predicciones del modelo SVM en el conjunto de prueba
  add_trace(x = test_data$Dose, y = test_data$pred_SVM_reg,
            type = "scatter", mode = "lines", name = "Predichos", line = list(color = "red")) %>%
  # Configurar el diseño del gráfico
  layout(title = "Predicciones del modelo SVM de regresión",
         xaxis = list(title = "Dose"),
         yaxis = list(title = "Relative Viability"))

# Mostrar el gráfico interactivo
plot


# Create a data frame Dose - Response.
data <- read_excel("Datos_Geno.xlsx")

# Reemplazar los valores faltantes (NA) por la moda de cada columna
data <- data %>%
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)) %>%
  mutate_if(is.factor, function(x) ifelse(is.na(x), names(sort(table(x)))[length(names(sort(table(x))))], x))

# Transformar la variable survival a factor
data$survival <- as.factor(data$survival)

# Cambiar la clase 1 como la clase positiva
data$survival <- relevel(data$survival, ref = "1")

set.seed(123) # Establecer una semilla para reproducibilidad
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Submuestreo de la clase mayoritaria
class_mayoritaria <- filter(train_data, survival == "0")
class_minoritaria <- filter(train_data, survival == "1")

# Obtener el número de muestras de la clase mayoritaria y menor
n_mayoritaria <- nrow(class_mayoritaria)
n_minoritaria <- nrow(class_minoritaria)

# Realizar el submuestreo duplicando aleatoriamente muestras de la clase minoritaria
class_minoritaria_upsampled <- class_minoritaria[sample(1:n_minoritaria, n_mayoritaria, replace = TRUE), ]

# Combinar los datos submuestreados de ambas clases
train_data_balanced <- rbind(class_mayoritaria, class_minoritaria_upsampled)

# Ajustar el modelo SVM radial con búsqueda de parámetros y validación cruzada
set.seed(123)
tune_grid_svm <- expand.grid(sigma = c(0.1, 1, 10), C = c(0.1, 1, 10))

modelo_train <- train(survival ~ SNP1+SNP2+SNP3+SNP4+SNP5+SNP6+SNP7+SNP8+SNP9, data = data, method = "svmRadial",
                      trControl = trainControl(method = "cv", number = 5),
                      tuneGrid = tune_grid_svm)

# Ajustar el modelo SVM con los mejores parámetros encontrados en los datos balanceados
modelo_svm_sur <- svm(survival ~ SNP1 + SNP2 + SNP3 + SNP4 + SNP5 + SNP6 + SNP7 + SNP8 + SNP9, 
                      data = train_data_balanced, kernel = "radial",
                      gamma = modelo_train$bestTune$sigma, cost = modelo_train$bestTune$C)

# Predecir las etiquetas para los datos de prueba
predicciones_SVM_sur <- predict(modelo_svm_sur, test_data, type = "class")

# Agregar las etiquetas predichas al conjunto de datos de prueba
test_data$pred_SVM_sur <- predicciones_SVM_sur

# Matriz de confusión - SVM
confusion_SVM_sur <- confusionMatrix(predicciones_SVM_sur, test_data$survival)

# Obtener la matriz de confusión con las etiquetas modificadas
confusion_SVM_sur_mod  <- confusion_SVM_sur[["table"]]
rownames(confusion_SVM_sur_mod) <- c("Muerto","Vivo")
colnames(confusion_SVM_sur_mod) <- c("Muerto","Vivo")

pander(confusion_SVM_sur_mod, caption = "Matriz de Confusión SVM Sobrevivencia",
       split.cells = 20, style = "grid")

# Extraer la métrica accuracy del objeto confusion_SVM
ACC_SVM_Sur <- round(confusion_SVM_sur$overall['Accuracy'], 2)

# Extraer la métrica sensibilidad del objeto confusion_SVM
SEN_SVM_Sur <- round(confusion_SVM_sur$byClass['Sensitivity'], 2)

# Extraer la métrica especificidad del objeto confusion_SVM
SPE_SVM_Sur <- round(confusion_SVM_sur$byClass['Specificity'], 2)