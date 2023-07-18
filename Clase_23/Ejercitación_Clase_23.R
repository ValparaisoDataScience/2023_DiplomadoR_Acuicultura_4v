# ------------------------------------------------------------------------------------------------------------
# Clase 23 Técnicas avanzadas de modelamiento predictivo: Árboles de decisión y Random Forest
# Dr. José Gallardo Matus, Dra. María Angélica Rueda Calderón
# 11 de julio 2023
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

# **EJERCICIO: ÁRBOL DE DECISIÓN MADURACIÓN TEMPRANA**

datos <- read_excel("Maturation.xlsx")

# Generar objeto My_Theme para personalizar el gráfico 
My_Theme <- theme(axis.title.x = element_text(size = 20, face = "bold"),
                  axis.text.x = element_text(size = 20,face = "bold"),
                  axis.title.y = element_text(size = 20, face = "bold"),
                  axis.text.y = element_text(size = 20,face = "bold"))

# Transformar la variable Maturation como factor
datos$Maturation <- as.factor(datos$Maturation)

# Cambiar la clase 1 como la clase positiva
datos$Maturation <- relevel(datos$Maturation, ref = "1")

# Seleccionar variables de la base de datos
datos_1 <- datos%>% 
  select("Mass","SGR", "Length","GSI","Maturation")

# Realiza partición de los datos en conjuntos de entrenamiento y prueba
set.seed(123)
train_indices <- createDataPartition(datos_1$Maturation, p = 0.7, list = FALSE)
train_data <- datos_1[train_indices, ]
test_data <- datos_1[-train_indices, ] 

# Crear el gráfico de dispersión con etiquetas invertidas
ggplot(datos_1, aes(x = Mass, y = reorder(Maturation, desc(Maturation)))) +
  geom_point(aes(color = factor(Maturation)),size=3) +
  scale_y_discrete(breaks = c(1, 0), labels = c("Si Madura", "No Madura")) +
  scale_color_manual(values = c("coral", "darkblue"),
                     labels = c("Si Madura", "No Madura")) +
  labs(x = "Peso Cuerpo", y = "Estado de Maduración") +
  theme_classic() +
  theme(legend.position = "none")+My_Theme+
  scale_x_continuous(breaks = seq(100, 600, 50))


# Entrenar el modelo de árbol de decisión
modelo_arbol <- rpart(Maturation ~ Mass, data = train_data,parms = list(split = 'gini'))

# Extraer indice de impureza Gini (Partición)
Splits <- modelo_arbol[["splits"]]

# Realizar predicciones en los datos de prueba
predicciones <- predict(modelo_arbol, newdata = test_data, type = "class")

# Agrega la variable con las predicciones a la base de testeo
test_data$predicciones <- predicciones

# Mostrar el objeto split en formato tabla usando pander()
pander(Splits)

# Crear el gráfico de dispersión con el valor del split
ggplot(datos_1, aes(x = Mass, y = reorder(Maturation, desc(Maturation)))) +
  geom_point(aes(color = factor(Maturation)), size = 3) +
  geom_vline(xintercept = modelo_arbol[["splits"]][4], linetype = "dashed", color = "red") +
  scale_y_discrete(breaks = c(1, 0), labels = c("Si Madura", "No Madura")) +
  scale_color_manual(values = c("coral", "darkblue"),
                     labels = c("Si Madura", "No Madura")) +
  labs(x = "Peso Cuerpo", y = "Estado de Maduración") +
  theme_classic() +
  theme(legend.position = "none") + My_Theme +
  scale_x_continuous(breaks = seq(100, 600, 50))

# Visualizar el árbol de decisión con rpart.plot
rpart.plot(modelo_arbol, type=4,extra = 1)

rpart.plot(modelo_arbol, type=4,extra = 2)

rpart.plot(modelo_arbol, type=4,extra = 106)

# Crear la matriz de confusión
matriz_confusion <- confusionMatrix(predicciones, test_data$Maturation)

# Obtener la matriz de confusión con las etiquetas modificadas
matriz_confusion_modificada <- matriz_confusion[["table"]]
rownames(matriz_confusion_modificada) <- c("Maduro","Inmaduro")
colnames(matriz_confusion_modificada) <- c("Maduro","Inmaduro")

# Muestra lamatriz de confusión como tabla
pander(matriz_confusion_modificada, caption = "Matriz de Confusión",
       split.cells = 20, style = "grid")

ACC<- round(matriz_confusion$overall['Accuracy'],2)


SEN <- round(matriz_confusion$byClass['Sensitivity'],2)


SPE <- round(matriz_confusion$byClass['Specificity'],2)

# Crear objeto de curva ROC
roc_obj_D <- roc(as.numeric(test_data$Maturation), as.numeric(test_data$predicciones))

# Obtener datos de sensibilidad y especificidad
roc_data_D <- coords(roc_obj_D, "all")

# Calcular el área bajo la curva ROC
auc_D <- auc(roc_obj_D)

# Crear gráfico de la curva ROC
ggplot(roc_data_D, aes(x = 1 - specificity, y = sensitivity)) +
  geom_path(linetype = "solid", color = "darkblue", size = 1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Tasa de Falsos Positivos (1 - Especificidad)",
       y = "Tasa de Verdaderos Positivos (Sensibilidad)",
       title = "Curva ROC",
       subtitle = paste("AUC =", round(auc_D, 2))) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none")+My_Theme


# **EJEMPLO: MADURACIÓN TEMPRANA CON ÁRBOL DE DECISIÓN**

# **ÁRBOL DE DECISIÓN**

# Entrenar el modelo de árbol de decisión
modelo_arbol_all <- rpart(Maturation ~ Mass + SGR + Length + GSI, data = train_data,parms = list(split = 'gini'))

# Extraer indice de impureza Gini (Partición)
Splits_all <- modelo_arbol_all[["splits"]]

# Realizar predicciones en los datos de prueba
predicciones_AD <- predict(modelo_arbol_all, newdata = test_data, type = "class")

test_data$predicciones_AD <- predicciones_AD

# Visualizar el árbol de decisión con rpart.plot
rpart.plot(modelo_arbol_all,type=4,extra = 1)

# **MATRIZ DE CONFUSIÓN CONSIDERANDO TODAS LA VARIABLES PREDICTORAS**

# Crear la matriz de confusión
matriz_confusion_AD <- confusionMatrix(predicciones_AD, test_data$Maturation)

# Obtener la matriz de confusión con las etiquetas modificadas
matriz_confusion_modificada_AD <- matriz_confusion_AD[["table"]]
rownames(matriz_confusion_modificada_AD) <- c("Maduro","Inmaduro")
colnames(matriz_confusion_modificada_AD) <- c("Maduro","Inmaduro")

pander(matriz_confusion_modificada_AD, caption = "Matriz de Confusión AD",
       split.cells = 20, style = "grid")

ACC_AD<- round(matriz_confusion_AD$overall['Accuracy'],2)


SEN_AD <- round(matriz_confusion_AD$byClass['Sensitivity'],2)


SPE_AD <- round(matriz_confusion_AD$byClass['Specificity'],2)


# **AREA BAJO LA CURVA ROC (AUC)**

# Crear objeto de curva ROC
roc_obj_AD <- roc(as.numeric(test_data$Maturation), as.numeric(test_data$predicciones_AD))

# Obtener datos de sensibilidad y especificidad
roc_data_AD <- coords(roc_obj_AD, "all")

# Calcular el área bajo la curva ROC
auc_AD <- auc(roc_obj_AD)

# Crear gráfico de la curva ROC
ggplot(roc_data_AD, aes(x = 1 - specificity, y = sensitivity)) +
  geom_path(color = "darkblue", size = 1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Tasa de Falsos Positivos (1 - Especificidad)",
       y = "Tasa de Verdaderos Positivos (Sensibilidad)",
       title = "Curva ROC",
       subtitle = paste("AUC =", round(auc_AD, 2))) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none")+My_Theme


# **ESTUDIO DE CASO: MADURACIÓN TEMPRANA CON RF**

# Realiza partición de los datos en conjuntos de entrenamiento y prueba

# Definir la rejilla de parámetros para la búsqueda
tune_grid_rf <- expand.grid(mtry = c(2, 3, 4))

# Ajustar el modelo Random Forest con búsqueda de parámetros y validación cruzada
set.seed(123)
modelo_rf <- train(Maturation ~  Mass + SGR + Length + GSI, data = datos_1, method = "rf",
                   trControl = trainControl(method = "cv", number = 5),
                   tuneGrid = tune_grid_rf, importance=TRUE,ntree=1000)

# Obtener los mejores parámetros encontrados
best_mtry <- modelo_rf$bestTune$mtry

# Ajustar el modelo Random Forest con los mejores parámetros
set.seed(123)
modelo_rf_final <- randomForest(Maturation ~ Mass + SGR + Length + GSI, data = train_data, mtry = best_mtry, importance=TRUE,ntree=1000)

# Realizar predicciones con los datos de prueba
predicciones_rf <- predict(modelo_rf_final, newdata = test_data)

# Visualiza la importancia de las variables predictoras en un modelo Random Forest
varImpPlot(modelo_rf_final, pch = 19, main = "Importancia de Variables Predictoras", type=2) 

# Agrega las predicciones al data frame test_data
test_data$pred_rf <- predicciones_rf

# Matriz de confusión - Random Forest
confusion_rf <- confusionMatrix(predicciones_rf, test_data$Maturation)

# Obtener la matriz de confusión con las etiquetas modificadas
matriz_confusion_modificada_rf <- confusion_rf[["table"]]
rownames(matriz_confusion_modificada_rf) <- c("Maduro","Inmaduro")
colnames(matriz_confusion_modificada_rf) <- c("Maduro","Inmaduro")

pander(matriz_confusion_modificada_rf, caption = "Matriz de Confusión RF.",
       split.cells = 20, style = "grid")

ACC<- round(confusion_rf$overall['Accuracy'],2)


SEN <- round(confusion_rf$byClass['Sensitivity'],2)


SPE <- round(confusion_rf$byClass['Specificity'],2)


# **AREA BAJO LA CURVA ROC (AUC) PARA RF**
  

# Crear objeto de curva ROC
roc_obj <- roc(as.numeric(test_data$Maturation), as.numeric(test_data$pred_rf))

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
        legend.position = "none")+My_Theme

