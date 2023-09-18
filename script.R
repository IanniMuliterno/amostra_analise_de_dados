library(tidyverse)
library(skimr)
library(GGally)
library(tidymodels)
library(recipes)
library(workflows)
#funções
# Função para plotar histogramas em massa para variáveis numéricas
plot_histograms <- function(data) {
  for(col in names(data)) {
    if(is.numeric(data[[col]])) {
      print(ggplot(data, aes_string(col)) + geom_histogram(binwidth=30) +
              labs(title=paste("Histograma de", col), x=col, y="Frequência"))
    }
  }
}

# Função para plotar gráficos de barra em massa para variáveis categóricas
plot_bargraphs <- function(data) {
  for(col in names(data)) {
    if(!is.numeric(data[[col]])) {
      print(ggplot(data, aes_string(col)) + geom_bar() +
              labs(title=paste("Gráfico de Barras de", col), x=col, y="Contagem"))
    }
  }
}


hotels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv")

head(hotels)

# 4. Limpeza e Pré-processamento de Dados
# 
# Tratamento de dados faltantes
sapply(hotels, function(x)sum(is.na(x)))
sapply(hotels, function(x)sum(x == ''))

# Identificação e tratamento de outliers
plot_histograms(hotels)

hotels |> 
  select_if(is.numeric) |> 
  pivot_longer(1:18) |> 
ggplot(aes(x=value)) +
  geom_histogram() +
  labs(title="Histograma") +
  facet_wrap(~name,scales = "free")


plot_bargraphs(hotels)

hotels |> 
  select_if(is.character) |> 
  pivot_longer(1:13) |> 
  ggplot(aes(x=value)) +
  geom_bar() +
  labs(title="Gráfico de barras") +
  facet_wrap(~name,scales = "free")
# Normalização e padronização
  

# 5. Análise Exploratória de Dados (AED)
# 
# Histogramas, scatter plots, box plots
# Medidas de tendência central e dispersão
# Correlações e causualidade



hotel_stays <- hotels |> 
  filter(is_canceled == 0) |> 
  mutate(
    children = case_when(
      children + babies > 0 ~ "children",
      TRUE ~ "none"
    ),
    required_car_parking_spaces = case_when(
      required_car_parking_spaces > 0 ~ "parking",
      TRUE ~ "none"
    )
  ) |>   
  select(-is_canceled, -reservation_status)

hotel_stays


hotel_stays |> 
  count(children)


skim(hotel_stays)

# VARIAÇÃO DE ESTADIA AO LONGO DO ANO DE ACORDO COM TER OU NÃO CRIANÇAS
# E TAMBÉM DE ACORDO COM O TIPO DO HOTEL
hotel_stays |> 
  mutate(arrival_date_month = factor(arrival_date_month,
                                     levels = month.name
  )) |> 
  count(hotel, arrival_date_month, children) |> 
  group_by(hotel, children) |> 
  mutate(proportion = n / sum(n)) |> 
  ggplot(aes(arrival_date_month, proportion, fill = children)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~hotel, nrow = 2) +
  labs(
    x = NULL,
    y = "Proporção de estadia ao longo do ano",
    fill = NULL
  )

#CLIENTES COM CRIANÇAS TEM MAIS CHANCE DE PREFERIR VAGAS DE CARRO?
hotel_stays |> 
  count(hotel, required_car_parking_spaces, children) |> 
  group_by(hotel, children) |> 
  mutate(proportion = n / sum(n)) |> 
  ggplot(aes(required_car_parking_spaces, proportion, fill = children)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~hotel, nrow = 2) +
  labs(
    x = NULL,
    y = "Proportion of hotel stays",
    fill = NULL
  )



hotel_stays %>%
  select(
    children, adr,
    required_car_parking_spaces,
    total_of_special_requests
  ) %>%
  ggpairs(mapping = aes(color = children))
# 6. Modelagem e Machine Learning
# 
# Regressão linear e logística
# Árvores de decisão e florestas aleatórias
# Validando e avaliando modelos

hotels_df <- hotel_stays %>%
  select(
    children, hotel, arrival_date_month, meal, adr, adults,
    required_car_parking_spaces, total_of_special_requests,
    stays_in_week_nights, stays_in_weekend_nights
  ) %>%
  mutate_if(is.character, factor)


set.seed(2023)
hotel_split <- initial_split(hotels_df)

hotel_train <- training(hotel_split)
hotel_test <- testing(hotel_split)

hotel_rec <- recipe(children ~ ., data = hotel_train) %>%
  themis::step_downsample(children) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_numeric()) %>%
  step_normalize(all_numeric()) %>%
  prep()

hotel_rec

test_proc <- bake(hotel_rec, new_data = hotel_test)

knn_spec <- nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("classification")

knn_fit <- knn_spec %>%
  fit(children ~ ., data = juice(hotel_rec))

knn_fit

tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

tree_fit <- tree_spec %>%
  fit(children ~ ., data = juice(hotel_rec))

tree_fit
# 7. Comunicação dos Resultados
# (falar sobre)
# Como apresentar insights
# Storytelling com dados
# Relatórios e dashboards
set.seed(1234)
validation_splits <- mc_cv(juice(hotel_rec), prop = 0.9, strata = children)
validation_splits

knn_wf <- workflow() |> 
  add_model(knn_spec) |> 
  add_formula(children ~.)

tree_wf <- workflow() |> 
  add_model(tree_spec) |> 
  add_formula(children ~.)

knn_res <- fit_resamples(
  knn_wf,
  validation_splits,
  control = control_resamples(save_pred = TRUE)
)

tree_res <- fit_resamples(
  tree_wf,
  validation_splits,
  control = control_resamples(save_pred = TRUE)
)

tree_res %>%
  collect_metrics()

knn_res %>%
  collect_metrics()

knn_res %>%
  unnest(.predictions) %>%
  mutate(model = "kknn") %>%
  bind_rows(tree_res %>%
              unnest(.predictions) %>%
              mutate(model = "rpart")) %>%
  group_by(model) %>%
  roc_curve(children, .pred_children) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_line(size = 1.5) +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  )


# 9. Ética em Ciência de Dados
#  ( se sobrer tempo, poderiamos falar de)
# Privacidade e segurança dos dados
# Bias e equidade em modelagem
