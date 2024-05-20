# Cargar el paquete DatasuRus
library(datasauRus)

# Cargamos dplyr
library(dplyr)

# Mostramos el resumen de los datos 
datasaurus_dozen %>%
    group_by(dataset) %>%
    summarize(
        mean_x    = mean(x),
        mean_y    = mean(y),
        std_dev_x = sd(x),
        std_dev_y = sd(y),
        corr_x_y  = cor(x, y)
    )

# Cargamos ggplot para graficar
library(ggplot2)

# Graficamos con el siguiente codigo
ggplot(datasaurus_dozen, aes(x = x, y = y, colour = dataset))+
    geom_point()+
    theme_void()+
    theme(legend.position = "none")+
    facet_wrap(~dataset, ncol = 3)
    
# Separamos los datos del conjunto Dino
dino_data <- datasaurus_dozen %>%
    filter(dataset == "dino")

# Graficar solo el conjunto de datos "dino"
ggplot(dino_data, aes(x = x, y = y)) +
  geom_point() +
  labs(title = "Conjunto de datos Dino") +
  theme_minimal()
  
# Ajustar un modelo de regresión lineal
modelo <- lm(y ~ x, data = dino_data)

# Graficar los datos de "dino" con la línea de regresión
ggplot(dino_data, aes(x = x, y = y)) +
  geom_point() +  # Agrega los puntos de datos
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +  # Agrega la línea de regresión
  labs(title = "Conjunto de datos Dino - Regresión Lineal") +
  theme_minimal()
  
# Mostrar resumen del modelo
summary(modelo)

# Resumen del modelo para obtener el R^2
summary_modelo <- summary(modelo)
r_squared <- summary_modelo$r.squared

# Predicciones del modelo
predicciones <- predict(modelo, dino_data)

# Calcular el MSE
mse <- mean((dino_data$y - predicciones)^2)

# Calcular el MAE
mae <- mean(abs(dino_data$y - predicciones))

# Calcular el RMSE
rmse <- sqrt(mse)

# Mostrar las métricas de calidad
cat("R^2:", r_squared, "\n")
cat("MSE:", mse, "\n")
cat("MAE:", mae, "\n")
cat("RMSE:", rmse, "\n")

# Ajustar un modelo de regresión cuadrática (polinomial de segundo grado)
modelo_cuadratico <- lm(y ~ poly(x, 2), data = dino_data)

# Generar valores predichos para la curva de regresión cuadrática
x_vals <- seq(min(dino_data$x), max(dino_data$x), length.out = 100)
y_vals <- predict(modelo_cuadratico, newdata = data.frame(x = x_vals))

# Crear un data frame con los valores predichos
predicciones <- data.frame(x = x_vals, y = y_vals)

# Graficar los datos de "dino" con la curva de regresión cuadrática
ggplot() +
  geom_point(data = dino_data, aes(x = x, y = y)) +  # Agrega los puntos de datos
  geom_line(data = predicciones, aes(x = x, y = y), color = "red") +  # Agrega la curva de regresión cuadrática
  labs(title = "Conjunto de datos Dino - Regresión Cuadrática") +
  theme_minimal()
  
# Mostrar resumen del modelo
summary(modelo_cuadratico)

# Resumen del modelo para obtener el R^2
summary_modelo_cuadratico <- summary(modelo_cuadratico)
r_squared_cuadratico <- summary_modelo_cuadratico$r.squared

# Predicciones del modelo cuadrático en los datos originales
predicciones_originales <- predict(modelo_cuadratico, dino_data)

# Calcular el MSE para el modelo cuadrático
mse_cuadratico <- mean((dino_data$y - predicciones_originales)^2)

# Calcular el MAE para el modelo cuadrático
mae_cuadratico <- mean(abs(dino_data$y - predicciones_originales))

# Calcular el RMSE para el modelo cuadrático
rmse_cuadratico <- sqrt(mse_cuadratico)

# Mostrar las métricas de calidad para el modelo cuadrático
cat("R^2:", r_squared_cuadratico, "\n")
cat("MSE:", mse_cuadratico, "\n")
cat("MAE:", mae_cuadratico, "\n")
cat("RMSE:", rmse_cuadratico, "\n")

# Ajustar un modelo de regresión polinomial (por ejemplo, polinomio de grado 3)
modelo_polinomial <- lm(y ~ poly(x, 3), data = dino_data)

# Generar valores predichos para la curva de regresión polinomial
x_vals <- seq(min(dino_data$x), max(dino_data$x), length.out = 100)
y_vals <- predict(modelo_polinomial, newdata = data.frame(x = x_vals))

# Crear un data frame con los valores predichos
predicciones <- data.frame(x = x_vals, y = y_vals)

# Graficar los datos de "dino" con la curva de regresión polinomial
ggplot() +
  geom_point(data = dino_data, aes(x = x, y = y)) +  # Agrega los puntos de datos
  geom_line(data = predicciones, aes(x = x, y = y), color = "red") +  # Agrega la curva de regresión polinomial
  labs(title = "Conjunto de datos Dino - Regresión Polinomial") +
  theme_minimal()

# Mostrar resumen del modelo polinomial
summary(modelo_polinomial)

# Resumen del modelo polinomial para obtener el R^2
summary_modelo_polinomial <- summary(modelo_polinomial)
r_squared_polinomial <- summary_modelo_polinomial$r.squared

# Predicciones del modelo polinomial en los datos originales
predicciones_originales_polinomial <- predict(modelo_polinomial, dino_data)

# Calcular el MSE para el modelo polinomial
mse_polinomial <- mean((dino_data$y - predicciones_originales_polinomial)^2)

# Calcular el MAE para el modelo polinomial
mae_polinomial <- mean(abs(dino_data$y - predicciones_originales_polinomial))

# Calcular el RMSE para el modelo polinomial
rmse_polinomial <- sqrt(mse_polinomial)

# Mostrar las métricas de calidad para el modelo polinomial
cat("R^2:", r_squared_polinomial, "\n")
cat("MSE:", mse_polinomial, "\n")
cat("MAE:", mae_polinomial, "\n")
cat("RMSE:", rmse_polinomial, "\n")

