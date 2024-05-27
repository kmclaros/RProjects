# app.R

# Cargar las bibliotecas necesarias
library(tidyverse)
library(lubridate)
library(forecast)
library(ggplot2)

# Establecer el directorio de trabajo
setwd("c:/data")
getwd()

# Leer el archivo CSV
raw_data <- read_csv(str_c(getwd(), "/2020-02.csv"))

# Convertir columnas de fecha y hora a formato datetime
viajes_diarios <- raw_data %>%
  mutate(fecha_hora = dmy_hms(paste(Fecha_Retiro, Hora_Retiro, sep=" ")))

# Filtrar datos entre el 24 y 27 de febrero
viajes_diarios <- viajes_diarios %>%
  filter(fecha_hora >= as.Date('2020-02-24') & fecha_hora <= as.Date('2020-02-27')) %>%
  group_by(horas = floor_date(fecha_hora, unit = 'hour')) %>%
  summarise(conteo = n())

# Crear estructura de datos con horario completo
horas_completas <- data.frame(
  horas = seq(
    floor_date(min(viajes_diarios$horas), unit='hour'),
    floor_date(max(viajes_diarios$horas), unit='hour'),
    by = 'hour'
  )
)

# Completar los datos con conteo y asignar ceros donde no hay registros
viajes_horas <- horas_completas %>%
  left_join(viajes_diarios, by = c("horas" = "horas")) %>%
  mutate(conteo = ifelse(is.na(conteo), 0, conteo))

# Graficar el patrón generado para los tres días
ggplot(data = viajes_horas, aes(x = horas, y = conteo)) + 
  geom_line() + 
  geom_point()

# Crear una serie temporal de los datos
conteo_ts <- ts(viajes_horas$conteo, start = 1, frequency = 24)

# Aplicar el modelo ARIMA
ajuste <- auto.arima(y = conteo_ts)

# Interpretar los datos
summary(ajuste)
predicciones <- forecast(ajuste)

min(predicciones[['lower']])
max(predicciones[['upper']])

# Graficar la predicción del modelo
autoplot(predicciones)

# Repetir desde el paso 8 para el periodo del 24 al 29 de febrero
viajes_diarios <- raw_data %>%
  mutate(fecha_hora = dmy_hms(paste(Fecha_Retiro, Hora_Retiro, sep=" "))) %>%
  filter(fecha_hora >= as.Date('2020-02-24') & fecha_hora <= as.Date('2020-02-29')) %>%
  group_by(horas = floor_date(fecha_hora, unit = 'hour')) %>%
  summarise(conteo = n())

# Crear estructura de datos con horario completo
horas_completas <- data.frame(
  horas = seq(
    floor_date(min(viajes_diarios$horas), unit='hour'),
    floor_date(max(viajes_diarios$horas), unit='hour'),
    by = 'hour'
  )
)

# Completar los datos con conteo y asignar ceros donde no hay registros
viajes_horas <- horas_completas %>%
  left_join(viajes_diarios, by = c("horas" = "horas")) %>%
  mutate(conteo = ifelse(is.na(conteo), 0, conteo))

# Graficar los datos reales del periodo de análisis
ggplot(data = viajes_horas, aes(x = horas, y = conteo)) +
  geom_line() + 
  ylim(-551.5396, 4103.783) +
  labs(title = "Realidad")
