# ------------------------------------------------------------------------------------------------------------
# Clase 15 - Script Regresión Lineal Múltiple
# Dr. José Gallardo Matus, Dra. María Angélica Rueda Calderón
# 30 mayo 2023
# Diplomado en Análisis de Datos y Modelamiento Predictivo con Aprendizaje Automático para la Acuicultura.
# ------------------------------------------------------------------------------------------------------------

# Habilita paquetes

library(ggpmisc) # Para mejorar y personalizar los gráficos creados con ggplot2

library(psych) # Para calcular y visualizar matrices de correlación

library(dplyr) # Para manipular datos

library(ggplot2) # Para hacer gráficos

library(tidyr) # Para manipular datos

library(car) # Para el análisis de regresión y diagnóstico

library(lmtest) # Para realizar pruebas de hipótesis sobre los coeficientes de regresión

library(knitr) # Para hacer tablas

library(sjPlot) # Para generar la tabla con la información del modelo ajustado en la solapa Viewer

library(readxl) # para cargar archivos de excell

# Cargar la base de datos
clearance <-  read_excel("ParticleClearance.xlsx", sheet = 1)

# Generar sub-bases de datos (Filtrar por sample)

mussel <- filter(clearance, sample == "mussel")

control <- filter(clearance, sample == "control")

# Generar objeto My_Theme para personalizar el gráfico 
My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 20),
  axis.title.y = element_text(size = 20),
  axis.text.y = element_text(size = 20))

# Graficar microparticle_concentration vs time
microplot <- ggplot(data = mussel, aes(x = time, y = microparticle_concentration)) +
  geom_point(position = position_jitter(w = 0, h = 0.1) ) +
  labs(x = "Time (minutes)", y = expression(Concentration~microparticles~ml^-1)) +
  scale_shape_manual(values=c(1,2)) +
  stat_smooth(method='loess',formula=y~x, se=T)+
  scale_color_brewer(palette="Set1") + 
  theme(legend.position="none") +
  theme(panel.border=element_blank(), axis.line=element_line())
microplot+My_Theme

# Ajustar modelo de regresión lineal simple
reg_mussel <- lm(microparticle_concentration ~ time, 
                 data=mussel)

# Graficar Residuos vs Valores Predichos (Ajustados)
plot(reg_mussel, which = 1)

# Ajustar regresión lineal simple para el sub-conjunto mussel
reg_mussel <- lm(log_microparticle_concentration ~ time, data=mussel)

# Ajustar regresión lineal simple para el sub-conjunto control
reg_control <- lm(log_microparticle_concentration ~ time, data=control)

# Graficar log_microparticle_concentration vs time
microplot1 <- ggplot(data = clearance, aes(x = time, y = log_microparticle_concentration, color = sample, shape = sample)) +
  geom_point(position = position_jitter(w = 0, h = 0.1) ) +
  labs(x = "Time (minutes)", y = expression(log[10]~(Concentration~microparticles~ml^-1))) +
  stat_smooth(method='lm',formula=y~x, se=F) +
  scale_shape_manual(values=c(1,2)) +
  scale_color_brewer(palette="Set1") + 
  theme(legend.position= c(0.2, 0.2)) +
  theme(panel.border=element_blank(), axis.line=element_line())
microplot1+My_Theme

# Crea modelo de regresión múltiple (RM) con lm()
lm.full <- lm(log_microparticle_concentration 
              ~ time*sample + time + sample, 
              data = clearance)

# Muestra la información del modelo ajustado en formato tabla
tab_model(lm.full, show.se = TRUE, show.aic=TRUE)

# Imprime resultado RM con función summary()
summary(lm.full)$coef %>% kable(digits=2)

# Realizar Anova del modelo ajustado
anova(lm.full) %>% kable(digits=2)

# Simular datos
set.seed(50)
X1=rnorm(100,0,1)
X2=rnorm(100,0,1)+(3.1*X1)
Y= 2 + 0.5 * X1 + 0.1 * X2 + rnorm(100,0,0.4)
lm1<- lm(Y~X1+X2)
lm2<- lm(Y~X1)
sim_dat<-cbind(Y,X1,X2)
head(sim_dat) %>% kable(digits=2)

# Grafico de Matriz de correlacciones
pairs.panels(sim_dat)

# Ajustar regresión lineal múltiple
lm1<- lm(Y~X1+X2)

# Calcular el VIF del modelo lm1
vif(lm1) %>% 
  kable(digits=2, col.names = c("VIF"))

# Ajustar el modelo completo (con todas las variables predictoras)
lm0<- lm(Y~X1+X2)

# Muestra la información del modelo ajustado en formato tabla
tab_model(lm0, show.se = TRUE, show.aic=TRUE)

# Imprime resultado de modelo de regresión múltiple
summary(lm0)$coef %>% kable(digits=2)

# Crea modelo de regresión simple variable X1
lm1<- lm(Y~X1)

# Muestra la información del modelo ajustado en formato tabla
tab_model(lm1, show.se = TRUE, show.aic=TRUE)

# Imprime resultado de modelo de regresión simple
summary(lm1)$coef %>% kable(digits=2)

# Crea modelo de regresión simple variable X2
lm2<- lm(Y~X2)

# Muestra la información del modelo ajustado en formato tabla
tab_model(lm2, show.se = TRUE, show.aic=TRUE)

# Imprime resultado de modelo de regresión simple
summary(lm2)$coef %>% kable(digits=2)

# Comparar los modelos usando residuales
anova(lm0, lm1, lm2) %>% kable(digits=2)

# Comparar los modelos usando los criterios AIC y BIC
AIC <- AIC(lm0, lm1, lm2)
BIC <- BIC(lm0, lm1, lm2)

# Mostrar en formato tabla la comparación de modelos usando AIC
AIC(lm0, lm1, lm2) %>% kable(digits=2)

# Mostrar en formato tabla la comparación de modelos usando BIC
BIC(lm0, lm1, lm2) %>% kable()
