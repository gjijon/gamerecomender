# Cargar las librerías necesarias
# https://www.kaggle.com/code/gabrieljijon/relationship-between-playtime-and-recommendations
library(ggplot2)
library(dplyr)

data <- read.csv('/kaggle/input/steam-reviews-dataset/dataset_steam_games.csv')

# Crear una nueva columna 'likes' que indica si el juego tiene más recomendaciones positivas que negativas
data <- data %>%
  group_by(nombre) %>%
  mutate(likes = ifelse(recomendado == 1, 1, 0))

# Resumir los datos por juego para obtener total de likes y tiempo de juego promedio
data_summary <- data %>%
  group_by(nombre) %>%
  summarise(
    total_likes = sum(likes),
    avg_playtime = mean(tiempoJuego)
  )

# Calcular la correlación entre el tiempo de juego promedio y los likes
correlation <- cor(data_summary$avg_playtime, data_summary$total_likes, use = "complete.obs")

# Mostrar la correlación
print(paste("La correlación entre el tiempo de juego y los likes es:", round(correlation, 2)))

# Visualizar la relación entre tiempo de juego y likes con un gráfico de dispersión
ggplot(data_summary, aes(x = avg_playtime, y = total_likes)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Relación entre Tiempo de Juego y Likes",
       x = "Tiempo de Juego Promedio (horas)",
       y = "Total de Likes") +
  theme_minimal()
