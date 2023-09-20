
# Filtrar os top 10 países com mais hóspedes
top_countries <- hotel_stays %>%
  count(country) %>%
  arrange(desc(n)) %>%
  head(10)

# Gráfico
ggplot(top_countries, aes(x = reorder(country, n), y = n)) +
  geom_bar(stat = "identity", fill = "#69b3a2") +
  coord_flip() +
  labs(title = "Top 10 Países com Mais Hóspedes",
       x = "País",
       y = "Número de Hóspedes") +
  theme_minimal()


# Contar hóspedes repetidos vs. novos hóspedes
repeated_guests <- hotel_stays %>%
  count(is_repeated_guest) %>%
  mutate(type = ifelse(is_repeated_guest == 0, "Novos Hóspedes", "Hóspedes Repetidos"))

# Gráfico
ggplot(repeated_guests, aes(x = type, y = n, fill = type)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribuição de Novos Hóspedes vs. Hóspedes Repetidos",
       x = "",
       y = "Número de Hóspedes") +
  theme_minimal() +
  theme(legend.position = "none")



###################################
# Carregue os pacotes necessários
library(dplyr)
library(tidyr)
library(fmsb)

df_processed <- df_hotels %>%
  filter(is_canceled == 0) %>%
  mutate(
    parents_detailed = case_when(children > 0 ~ "pais",
                                 babies > 0 ~ "pais_jovens",
                                 TRUE ~ "sem_filhos"),
    stay_duration = stays_in_weekend_nights + stays_in_week_nights,
    special_requests = if_else(total_of_special_requests > 0, 1, 0)
  ) %>%
  group_by(parents_detailed) %>%
  summarize(
    stay_duration = mean(stay_duration, na.rm = TRUE),
    adr = mean(adr, na.rm = TRUE),
    special_requests = mean(special_requests, na.rm = TRUE),
    meal_BB = mean(meal == "BB", na.rm = TRUE)
  ) %>%
  ungroup()

# Identificar valores máximos e mínimos
max_values <- df_processed %>%
  summarise(across(everything(), max, na.rm = TRUE))

min_values <- df_processed %>%
  summarise(across(everything(), min, na.rm = TRUE))

# Renomeando as linhas para aderir ao formato esperado
rownames(max_values) <- "max"
rownames(min_values) <- "min"
df_radar <- rbind(max_values, min_values, df_processed)


radarchart(df_radar[,-1], seg=4,
           axistype = 1, 
           #pcol = c("pais" = "#FC4E07", "pais_jovens" = "#E7B800", "sem_filhos" = "#00AFBB", "min" = "#FFFFFF", "max" = "#FFFFFF"), 
           plwd = 1,
           plty = 1)


radarchart(dat, axistype=1, seg=5, plty=1, vlabels=c("Total\nQOL", "Physical\naspects", 
                                                     "Phychological\naspects", "Social\naspects", "Environmental\naspects"), 
           title="(axis=1, 5 segments, with specified vlabels)", vlcex=0.5)



library(ggplot2)
library(tidyr)
library(dplyr)

# Transformação dos dados
df_long <- df_processed %>%
  pivot_longer(cols = -parents_detailed, names_to = "variable", values_to = "value")

# Desenhar o gráfico de radar
ggplot(df_long, aes(x = variable, y = value, group = parents_detailed, color = parents_detailed)) +
  geom_polygon(fill = NA, size = 1.5) + 
  geom_line(size = 1) +
  coord_polar() +
  theme_minimal() +
  labs(title = "Gráfico de Radar para Características de Hospedagem") +
  theme(legend.position = "bottom")




# Calculando o valor máximo dos dados
max_value <- max(df_radar[,-1], na.rm = TRUE)

# Ajustando os dados para o gráfico de radar
df_radar <- df_long %>%
  group_by(parents_detailed, variable) %>%
  summarize(value = mean(value, na.rm = TRUE)) %>%
  spread(key = variable, value = value) %>%
  arrange(desc(parents_detailed))

# Normalizando os dados para valores entre 0 e 1
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

df_normalized <- df_radar
df_normalized[,-1] <- as.data.frame(lapply(df_normalized[,-1], normalize))

# Gráfico de radar com dados normalizados
ggradar(df_normalized, grid.max = 1) + 
  scale_fill_manual(values = c("pais" = "#FC4E07", "pais_jovens" = "#E7B800", "sem_filhos" = "#00AFBB"))
