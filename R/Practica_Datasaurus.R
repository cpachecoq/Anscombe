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
  

