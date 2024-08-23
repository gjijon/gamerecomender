# Cargar las librerías necesarias
# https://www.kaggle.com/code/gabrieljijon/popularity-by-genre-and-price-hours-relation
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

# Agrupar datos por género y sumar votos positivos y negativos
genre_popularity <- data_long %>%
  group_by(genre) %>%
  summarise(
    total_positive = sum(recomendado == 1),
    total_negative = sum(recomendado == 0),
    total_reviews = n()
  )

# Visualizar la popularidad por género
ggplot(genre_popularity, aes(x = reorder(genre, total_positive), y = total_positive)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Popularidad de Géneros por Votos Positivos", x = "Género", y = "Total de Votos Positivos")

# Filtrar datos para eliminar precios y horas jugadas extremadamente altos o bajos (opcional)
filtered_data <- data %>%
  filter(precio > 0 & precio < 100, tiempoJuego > 0 & tiempoJuego < 500)

# Crear un gráfico de dispersión para ver la relación entre precio y horas jugadas
ggplot(filtered_data, aes(x = precio, y = tiempoJuego)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre Precio y Horas Jugadas", x = "Precio ($)", y = "Horas Jugadas")
