# ------------------------------------------------------------------------------------------------------------
# Clase 16 - Script Modelamiento predictivo: Regresión lineal
# Dr. José Gallardo Matus, Dra. María Angélica Rueda Calderón
# 17 de junio 2023
# Diplomado en Análisis de Datos y Modelamiento Predictivo con Aprendizaje Automático para la Acuicultura
# ------------------------------------------------------------------------------------------------------------

# Cargar paquetes necesarios
library(caret)
library(readxl)
library(ggplot2)
library(car)
library(lmtest)
library(pander)
library(dplyr)
library(ggpmisc)
library(gridExtra)
library(ggpubr)

# Regresión lineal simple

# Importa base de datos
Global_warming <- read_excel("Global_Warming.xlsx")

# Formato formula para considerar en el grafico
formula1 <- y ~ x

# Transforma la variable Year a factor
Global_warming$Year <- as.factor(Global_warming$Year)

# Transforma la variable CO2_ppm a númerica
Global_warming$CO2_ppm <- as.numeric(Global_warming$CO2_ppm)

# Transforma la variable Global Temperature Anomalies a númerica
Global_warming$`Global Temperature Anomalies` <- as.numeric(Global_warming$`Global Temperature Anomalies`)

# Validación k-fold (k=5)
# Establecer la semilla aleatoria para reproducibilidad
set.seed(12345)

# Crear los índices para la validación cruzada
folds <- sample(1:5, size = nrow(Global_warming), replace = TRUE)

# Crear un data frame vacío
results_df <- data.frame(True = numeric(), Predicted = numeric(), Fold = integer(), Data = character(), stringsAsFactors = FALSE)

# Bucle para agregar los valores True, Predicted, Fold y Data de cada fold al dataframe
for (i in 1:max(folds)) {
  tst <- which(folds == i)
  train <- which(folds != i)
  train_data <- Global_warming[train, ]
  test_data <- Global_warming[tst, ]
  
  # Ajustar el modelo de regresión lineal simple en el conjunto de entrenamiento
reg <- lm(`Global Temperature Anomalies` ~ CO2_ppm, data = train_data)
  
  # Realizar predicciones en el conjunto de prueba
predictions <- predict(reg, newdata = test_data)
  
  # Calcular el error de prueba (RMSE)
rmse <- sqrt(mean((predictions - test_data$`Global Temperature Anomalies`)^2))
  
  # Calcular el error de prueba (MSE)
mse <- mean((predictions - test_data$`Global Temperature Anomalies`)^2)
  
# Agregar los valores True, Predicted, Fold y Data del conjunto de entrenamiento al data frame
  train_df <- data.frame(True = train_data$`Global Temperature Anomalies`, Predicted = predict(reg, newdata = train_data), Fold = i, Data = rep("Train", length(train)), stringsAsFactors = FALSE)
  results_df <- rbind(results_df, train_df)
  
# Agregar los valores True, Predicted, Fold y Data del conjunto de prueba al data frame
  tst_df <- data.frame(True = test_data$`Global Temperature Anomalies`, Predicted = predictions, Fold = i, Data = rep("Tst", length(tst)), stringsAsFactors = FALSE)
  results_df <- rbind(results_df, tst_df)
}

# Seleccionar 5 observaciones del conjunto de entrenamiento
data_train <- results_df %>%
  filter(Data=="Train")%>%
  slice(1:5)

# Seleccionar 5 observaciones del conjunto de testeo
data_test <- results_df %>%
  filter(Data=="Tst")%>%
  slice(1:5)

# Generar una base de datos con los objetos data_train y data_test
combined_data <- bind_rows(data_train, data_test)

# Mostrar el dataframe en formato tabla
kable(head(combined_data, 10), digits = 2)

# Agrupar los datos por Fold y calcular el R2, MSE, RMSE y el n para cada grupo
R_R2_RMSE_by_fold <- results_df %>%
  group_by(Fold, Data) %>%
  summarize(R = cor(True, Predicted),
            R2 = summary(lm(True ~ Predicted))$r.squared, 
            MSE= mean((True - Predicted)^2),
            RMSE = sqrt(mean((True - Predicted)^2)),
            n=n())

# Mostrar los resultados
kable(head(R_R2_RMSE_by_fold,10), digits = 2)

# Calcular el promedio del MSE
MSE_average <- R_R2_RMSE_by_fold%>%
  group_by(Data)%>%
  summarise(MSE_PROMEDIO=mean(MSE))

# Calcular el promedio del RMSE
RMSE_average <- R_R2_RMSE_by_fold%>%
  group_by(Data)%>%
  summarise(RMSE_PROMEDIO=mean(RMSE))

# Hacer sub-bases para cada fold
df_K1 <- results_df%>%
  filter(Fold==1)

df_K2 <- results_df%>%
  filter(Fold==2)

df_K3 <- results_df%>%
  filter(Fold==3)

df_K4 <- results_df%>%
  filter(Fold==4)

df_K5 <- results_df%>%
  filter(Fold==5)

# Generar objeto My_Theme para personalizar el gráfico 
My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 20),
  axis.title.y = element_text(size = 20),
  axis.text.y = element_text(size = 20))

# Generar figura .jpg con los 5-folds (Train y Test)
jpeg(file = "5_FOLD_RL.jpg", 
     width = 40, height = 40,  
     units = "cm",             
     quality = 95,            
     res = 300)
K1 <- ggscatter(df_K1, x = "True", y = "Predicted", color = "Data",
                add = "reg.line", conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Real", ylab = "Predicción",
                palette = c("red","blue")) + theme(legend.position = "none")+
  facet_wrap(~Fold+Data)+My_Theme


K2 <- ggscatter(df_K2, x = "True", y = "Predicted", color = "Data",
                add = "reg.line", conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Real", ylab = "Predicción",
                palette = c("red","blue")) + theme(legend.position = "none")+
  facet_wrap(~Fold+Data)+My_Theme

K3 <- ggscatter(df_K3, x = "True", y = "Predicted", color = "Data",
                add = "reg.line", conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Real", ylab = "Predicción",
                palette = c("red","blue")) + theme(legend.position = "none")+
  facet_wrap(~Fold+Data)+My_Theme

K4 <- ggscatter(df_K4, x = "True", y = "Predicted", color = "Data",
                add = "reg.line", conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Real", ylab = "Predicción",
                palette = c("red","blue")) + theme(legend.position = "none")+
  facet_wrap(~Fold+Data)+My_Theme

K5 <- ggscatter(df_K5, x = "True", y = "Predicted", color = "Data",
                add = "reg.line", conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Real", ylab = "Predicción",
                palette = c("red","blue")) + theme(legend.position = "none")+
  facet_wrap(~Fold+Data)+My_Theme

grid.arrange(K1,K2,K3,K4,K5, nrow = 5)
dev.off()

# Unir la información de las dos bases de datos anteriores por la variable Data
datos_all <- left_join(MSE_average,RMSE_average, by="Data")
datos_all

# Generar figura .jpg con la correlación entre los valores observados y predichos

jpeg(file = "R_promedio.jpg", 
     width = 40, height = 20,  
     units = "cm",             
     quality = 95,            
     res = 300)
ggscatter(results_df, x = "True", y = "Predicted", color = "Data",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Real", ylab = "Predicción",
          palette = c("red", "blue"))+  theme(legend.position = "none")+
  facet_wrap(~Data)
dev.off()


