# Cargar las librerías necesarias
# https://www.kaggle.com/code/gabrieljijon/average-playtime-and-likes-by-genre
library(ggplot2)
library(dplyr)
library(tidyr)

data <- read.csv('/kaggle/input/steam-reviews-dataset/dataset_steam_games.csv')

# Crear una columna que indique si el juego pertenece a cada género basado en los valores en las columnas "historia", "rapido", "ajustable"
data_long <- data %>%
  mutate(historia = ifelse(historia == 1, "historia", NA),
         rapido = ifelse(rapido == 1, "rapido", NA),
         ajustable = ifelse(ajustable == 1, "ajustable", NA)) %>%
  pivot_longer(cols = c("historia", "rapido", "ajustable"), names_to = "genre_type", values_to = "genre") %>%
  filter(!is.na(genre))

# Crear una columna 'likes' que indica si el juego tiene recomendaciones positivas (1) o no (0)
data_long <- data_long %>%
  mutate(likes = ifelse(recomendado == 1, 1, 0))

# Agrupar datos por género y calcular el promedio de horas jugadas y el total de likes
genre_summary <- data_long %>%
  group_by(genre) %>%
  summarise(
    avg_playtime = mean(tiempoJuego, na.rm = TRUE),
    total_likes = sum(likes),
    total_reviews = n()
  )

# Visualizar la relación entre género, horas de juego promedio y total de likes
ggplot(genre_summary, aes(x = reorder(genre, -total_likes), y = avg_playtime, fill = total_likes)) +
  geom_bar(stat = "identity") +
  labs(title = "Horas de Juego Promedio y Likes por Género",
       x = "Género",
       y = "Horas de Juego Promedio (horas)",
       fill = "Total de Likes") +
  theme_minimal()
