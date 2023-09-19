
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
