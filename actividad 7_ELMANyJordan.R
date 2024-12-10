# Instalar el paquete si no lo tienes
install.packages("readxl")

# Cargar el paquete
library(readxl)

# Leer el archivo Excel
data <- MUESTRA_SERIE_TIEMPO


# Verificar los datos cargados
head(data)
str(data)

#convertir la variable "quantity" en una serie de tiempo diaria"

data$transaction_date <- as.Date(data$transaction_date, format = "%Y-%m-%d") # Ajusta el formato si es necesario

#agrupar los datos por dia 

library(dplyr)
library(tidyr)

# Crear una serie diaria completa

daily_data <- data %>%
  group_by(transaction_date) %>%
  summarise(quantity = sum(quantity, na.rm = TRUE)) %>%
  complete(transaction_date = seq(min(transaction_date), max(transaction_date), by = "day"),
           fill = list(quantity = 0))


# Crear la serie temporal 

quantity_ts <- ts(daily_data$quantity, start = c(as.numeric(format(min(daily_data$transaction_date), "%Y")),
                                                 as.numeric(format(min(daily_data$transaction_date), "%j"))),
                  frequency = 365)


#dividir los datos en entrenamiento y prueba 

train_size <- floor(0.8 * length(quantity_ts))
train <- quantity_ts[1:train_size]
test <- quantity_ts[(train_size + 1):length(quantity_ts)]

#Normalizar los datos 

normalize <- function(x) (x - min(x)) / (max(x) - min(x))
denormalize <- function(x, min_val, max_val) x * (max_val - min_val) + min_val

train_norm <- normalize(train)
test_norm <- normalize(test)


#ELMAN MODEL

library(RSNNS)

# Preparar datos para el modelo Elman
inputs <- embed(train_norm, 2)[, 2, drop = FALSE]
outputs <- embed(train_norm, 2)[, 1]

# Entrenar el modelo
elman_model <- elman(
  inputs,
  outputs,
  size = c(5),             # Número de neuronas en la capa oculta
  learnFuncParams = c(0.1),# Tasa de aprendizaje
  maxit = 1000             # Iteraciones máximas
)

# Predecir en el conjunto de prueba
inputs_test <- embed(test_norm, 2)[, 2, drop = FALSE]
predictions_elman <- predict(elman_model, inputs_test)

# Desnormalizar las predicciones
predictions_elman <- denormalize(predictions_elman, min(train), max(train))

# Grafica de predicción vrs realidad
# Graficar predicciones frente a los valores reales
plot(test, type = "l", col = "black", main = "Predicciones vs Realidad", xlab = "Tiempo", ylab = "Cantidad")
lines(predictions_elman, col = "blue", lty = 2)  # Predicciones del modelo Elman
legend("topright", legend = c("Realidad", "Elman"), col = c("black", "blue"), lty = 1:2)

# GRafica de residuos
# Asegurarte de que 'predictions_elman' tenga el mismo tamaño que 'test'
predictions_elman <- predictions_elman[1:length(test)]

# Calcular los residuos
residuals <- test - predictions_elman

# Graficar los residuos
plot(residuals, type = "o", col = "red", main = "Residuos del Modelo Elman", xlab = "Tiempo", ylab = "Residuo")

# Metricas
# Calcular el RMSE
rmse <- function(real, predicted) sqrt(mean((real - predicted)^2))
rmse_value <- rmse(test, predictions_elman)
print(paste("RMSE:", rmse_value))


# Verificar valores NA en 'test' y 'predictions_elman'
sum(is.na(test))  # Ver cuántos valores faltan en el conjunto de prueba
sum(is.na(predictions_elman))  # Ver cuántos valores faltan en las predicciones

# Eliminar valores NA de 'test' y 'predictions_elman'
valid_indices <- complete.cases(test, predictions_elman)
test <- test[valid_indices]
predictions_elman <- predictions_elman[valid_indices]

# Recalcular el RMSE
rmse_value <- sqrt(mean((test - predictions_elman)^2))
print(paste("RMSE:", rmse_value))

# Calcular el MAE
mae <- function(real, predicted) mean(abs(real - predicted))
mae_value <- mae(test, predictions_elman)
print(paste("MAE:", mae_value))

# Calcular el R²
r2_value <- 1 - sum((test - predictions_elman)^2) / sum((test - mean(test))^2)
print(paste("R²:", r2_value))


##JORDAN

# Entrenar el modelo Jordan
jordan_model <- jordan(
  inputs,
  outputs,
  size = c(5),             # Número de neuronas en la capa oculta
  learnFuncParams = c(0.1),# Tasa de aprendizaje
  maxit = 1000             # Iteraciones máximas
)

# Predecir en el conjunto de prueba
predictions_jordan <- predict(jordan_model, inputs_test)

# Desnormalizar las predicciones
predictions_jordan <- denormalize(predictions_jordan, min(train), max(train))

# Calcular RMSE
rmse <- function(real, predicted) sqrt(mean((real - predicted)^2))

rmse_elman <- rmse(test, predictions_elman)
rmse_jordan <- rmse(test, predictions_jordan)

# Graficar resultados


plot(test, type = "l", col = "black", main = "Predicciones vs Realidad")
lines(predictions_elman, col = "blue", lty = 2)  # Elman
lines(predictions_jordan, col = "red", lty = 2)  # Jordan
legend("topright", legend = c("Realidad", "Elman", "Jordan"),
       col = c("black", "blue", "red"), lty = 1:2)


#Comparación de métricas
# Cálculo de RMSE para Elman
rmse_elman <- sqrt(mean((test - predictions_elman)^2))

# Cálculo de RMSE para Jordan
rmse_jordan <- sqrt(mean((test - predictions_jordan)^2))

# Imprimir los resultados
print(paste("RMSE Elman:", rmse_elman))
print(paste("RMSE Jordan:", rmse_jordan))

# Cálculo de MAE para Elman
mae_elman <- mean(abs(test - predictions_elman))

# Cálculo de MAE para Jordan
mae_jordan <- mean(abs(test - predictions_jordan))

# Imprimir los resultados
print(paste("MAE Elman:", mae_elman))
print(paste("MAE Jordan:", mae_jordan))

# Cálculo de R² para Elman
r_squared_elman <- 1 - sum((test - predictions_elman)^2) / sum((test - mean(test))^2)

# Cálculo de R² para Jordan
r_squared_jordan <- 1 - sum((test - predictions_jordan)^2) / sum((test - mean(test))^2)

# Imprimir los resultados
print(paste("R² Elman:", r_squared_elman))
print(paste("R² Jordan:", r_squared_jordan))

# Graficar las predicciones de ambos modelos
plot(test, type = "l", col = "black", main = "Comparación de Predicciones: Elman vs. Jordan", 
     xlab = "Tiempo", ylab = "Cantidad")
lines(predictions_elman, col = "blue", lty = 2)
lines(predictions_jordan, col = "red", lty = 3)
legend("topright", legend = c("Realidad", "Predicciones Elman", "Predicciones Jordan"), 
       col = c("black", "blue", "red"), lty = 1:3)





