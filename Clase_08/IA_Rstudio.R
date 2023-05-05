# ------------------------------------------------------------------------------------------------------------------
# Clase 08 - PASOS PARA INSTALAR IA EN R STUDIO
# Dr. José Gallardo Matus & Dra. María Angélica Rueda Calderón
# 29 abril 2023
# Diplomado en Análisis de Datos y Modelamiento Predictivo con Aprendizaje Automático para la Acuicultura.
# ------------------------------------------------------------------------------------------------------------------

# Generar cuenta y clave en Platform https://platform.openai.com/

# Generar la API Key https://platform.openai.com/account/api-keys

# Guardar la API KEY en un block de notas

# Ir a RStudio y seguir los siguientes pasos:

# Instalar paquete
install.packages("devtools")

# Habilitar paquete
library(devtools)

# Instalar paquete gptstudio desde github
devtools::install_github("MichelNivard/gptstudio")

# Vincular la Api key https://platform.openai.com/account/api-keys
Sys.setenv(OPENAI_API_KEY = "COLOCAR SU API KEY")
