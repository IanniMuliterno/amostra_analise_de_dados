library(tidyverse)
library(skimr)
library(GGally)
library(tidymodels)
library(recipes)
library(workflows)
library(RColorBrewer)
library(ggcorrplot)
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
str(hotel_stays)

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

#comparação de característica de hospedagem
# Preprocessamento e transformação de dados
df_processed <- df_hotels |> 
  filter(is_canceled == 0) |> 
  mutate(
    parents = if_else(children > 0 | babies > 0, 1, 0),
    parents_detailed = case_when(children > 0 ~ "pais",
                                 babies > 0 ~ "pais_jovens",
                                 TRUE ~ "sem_filhos"),
    meal_num = if_else(meal %in% c("HB", "FB"), 1, 0)
  ) |> 
  dplyr::select(parents_detailed,# not_canceled,
                stays_in_weekend_nights, 
                meal_num, total_of_special_requests) |> 
  pivot_longer(cols = stays_in_weekend_nights:total_of_special_requests,
               names_to = "variable",
               values_to = "value") |> 
  group_by(parents_detailed, variable) |>
  summarize(
    yes = sum(value > 0) / n(),
    no = sum(value == 0) / n()
  ) |> 
  pivot_longer(cols = c(yes, no),
               names_to = "group",
               values_to = "value")|>
  filter(group == "yes")

# Paletas de cores simplificadas
color_fill <- c("pais_jovens" = "#66c2a5", "pais" = "#fc8d62", "sem_filhos" = "#8da0cb")

  ggplot(df_processed , aes(x = variable, y = value, fill = parents_detailed)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = color_fill) +
  scale_alpha_manual(0.6) +
  labs(title = "Comparação de característica de hospedagem",
       x = "Características",
       y = "%",
       fill = "tem filhos?",
       alpha = "Type of Parent") +
  theme_minimal()
# stays , previous, adr
correlacao <-   hotel_stays |> 
    select(starts_with('stays'),starts_with('previous'),adr) |> 
  janitor::clean_names() |>  
  cor()   
    corrplot::corrplot(correlacao, tl.col = "red", bg = "White", tl.srt = 45,
                        addCoef.col = "black", type = "lower")

# mas...

    
 # Dividir as variáveis pelo tipo
numeric_vars <- names(hotel_stays)[sapply(hotel_stays, is.numeric)]
categorical_vars <- names(hotel_stays)[sapply(hotel_stays, is.character)]

# Correlação de Pearson para numéricas
numeric_cor <- cor(hotel_stays[, numeric_vars], method = "pearson")
    
# Correlação de Spearman para ordinais (usando numéricas como exemplo)
ordinal_cor <- cor(hotel_stays[, numeric_vars], method = "spearman")
    
# Qui-quadrado para categóricas
chi_results <- list()
for(i in 1:(length(categorical_vars)-1)) {
  for(j in (i+1):length(categorical_vars)) {
    var1 <- as.factor(hotel_stays[[categorical_vars[i]]])
    var2 <- as.factor(hotel_stays[[categorical_vars[j]]])
    chi_table <- table(var1, var2)
    chi_results[[paste0(categorical_vars[i], " vs ", categorical_vars[j])]] <- chisq.test(chi_table)
  }
}
    
# Mostrar resultados
    numeric_cor
    ordinal_cor
    chi_results
    
  
# GGPAIRS

hotel_stays |> 
  select(
    children, adr,
    required_car_parking_spaces,
    total_of_special_requests
  ) |> 
  ggpairs(mapping = aes(color = children))
# 6. Modelagem e Machine Learning
# 
# Regressão linear e logística
# Árvores de decisão e florestas aleatórias
# Validando e avaliando modelos

hotels_df <- hotel_stays |> 
  select(
    children, hotel, arrival_date_month, meal, adr, adults,
    required_car_parking_spaces, total_of_special_requests,
    stays_in_week_nights, stays_in_weekend_nights
  ) |> 
  mutate_if(is.character, factor)

#### Data prep manual
set.seed(2023)

# Divisão de Dados
hotel_split <- initial_split(hotels_df)
hotel_train <- training(hotel_split)
hotel_test <- testing(hotel_split)

# Downsampling
minority_class_size <- sum(hotel_train$children == "children")
majority_class_indices <- which(hotel_train$children == "none")
selected_majority_indices <- sample(majority_class_indices, minority_class_size)
downsampled_indices <- c(which(hotel_train$children == "children"), selected_majority_indices)
hotel_train_downsampled <- hotel_train[downsampled_indices, ]

# Separar colunas numéricas e categóricas
num_vars <- names(hotel_train_downsampled)[sapply(hotel_train_downsampled, is.numeric)]
cat_vars <- setdiff(names(hotel_train_downsampled), c(num_vars, "children"))

# Criar dummies para as colunas categóricas
dummy_train_cat <- model.matrix(~ . + 0, data = hotel_train_downsampled[, cat_vars, drop=FALSE])
dummy_test_cat <- model.matrix(~ . + 0, data = hotel_test[, cat_vars, drop=FALSE])

# Remover colunas com Variância Zero nas dummies categóricas
zv_cols_cat <- which(apply(dummy_train_cat, 2, var) == 0)
dummy_train_cat <- dummy_train_cat[, -zv_cols_cat]
dummy_test_cat <- dummy_test_cat[, -zv_cols_cat]

# Normalizar as colunas numéricas
mean_train_num <- apply(hotel_train_downsampled[, num_vars, drop=FALSE], 2, mean)
std_train_num <- apply(hotel_train_downsampled[, num_vars, drop=FALSE], 2, sd)

#sweep:
# A função sweep é usada para "varrer" uma matriz ou array por suas margens. No contexto do código, ela é usada para subtrair a média (mean_train_num) e depois dividir pelo desvio padrão (std_train_num) para cada coluna. Esta é a operação de normalização.
# 
# O primeiro argumento é a matriz/array.
# O segundo argumento é a margem (1 = linhas, 2 = colunas).
# O terceiro argumento é o vetor de estatísticas (média ou desvio padrão).
# O quarto argumento é a função a ser aplicada (FUN = "-" para subtração e FUN = "/" para divisão).

hotel_train_norm <- sweep(hotel_train_downsampled[, num_vars, drop=FALSE], 2, mean_train_num, FUN = "-")
hotel_train_norm <- sweep(hotel_train_norm, 2, std_train_num, FUN = "/")

hotel_test_norm <- sweep(hotel_test[, num_vars, drop=FALSE], 2, mean_train_num, FUN = "-")
hotel_test_norm <- sweep(hotel_test_norm, 2, std_train_num, FUN = "/")

# Juntar dados normalizados e variáveis dummy
dummy_train <- cbind(dummy_train_cat, hotel_train_norm)
dummy_test <- cbind(dummy_test_cat, hotel_test_norm)

# Resultado Final
test_proc <- as.data.frame(dummy_test)


#### Como fica com recipes
set.seed(2023)
hotel_split <- initial_split(hotels_df)

hotel_train <- training(hotel_split)
hotel_test <- testing(hotel_split)

hotel_rec <- recipe(children ~ ., data = hotel_train) |> 
  themis::step_downsample(children) |> 
  step_dummy(all_nominal(), -all_outcomes()) |> 
  step_zv(all_numeric()) |> 
  step_normalize(all_numeric()) |> 
  prep()

hotel_rec

test_proc <- bake(hotel_rec, new_data = hotel_test)

knn_spec <- nearest_neighbor() |> 
  set_engine("kknn") |> 
  set_mode("classification")

knn_fit <- knn_spec |> 
  fit(children ~ ., data = juice(hotel_rec))

knn_fit

tree_spec <- decision_tree() |> 
  set_engine("rpart") |> 
  set_mode("classification")

tree_fit <- tree_spec |> 
  fit(children ~ ., data = juice(hotel_rec))

tree_fit


# Especificar o modelo GLM
glm_spec <- logistic_reg() |>  
  set_engine("glm") |>  
  set_mode("classification")

# Ajuste o modelo
glm_fit <- glm_spec |>  
  fit(children ~ ., data = juice(hotel_rec))
glm_fit
# 7. Comunicação dos Resultados
# (falar sobre)
# Como apresentar insights
# Storytelling com dados
# Relatórios e dashboards
set.seed(1234)
validation_splits <- mc_cv(juice(hotel_rec), prop = 0.9, strata = children)
validation_splits

# Definir o fluxo de trabalho para o GLM
glm_wf <- workflow() |>  
  add_model(glm_spec) |>  
  add_formula(children ~ .)

knn_wf <- workflow() |> 
  add_model(knn_spec) |> 
  add_formula(children ~.)

tree_wf <- workflow() |> 
  add_model(tree_spec) |> 
  add_formula(children ~.)

# Ajustar o modelo usando validação cruzada


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


glm_res <- fit_resamples(
  glm_wf,
  validation_splits,
  control = control_resamples(save_pred = TRUE)
)


# Coletar métricas

tree_res |> 
  collect_metrics()

knn_res |> 
  collect_metrics()

glm_res |>  
  collect_metrics()


knn_res |> 
  unnest(.predictions) |> 
  mutate(model = "kknn") |> 
  bind_rows(tree_res |> 
              unnest(.predictions) |> 
              mutate(model = "rpart")) |> 
  bind_rows(glm_res |> 
              unnest(.predictions) |> 
              mutate(model = "glm")) |> 
  group_by(model) |> 
  roc_curve(children, .pred_children) |> 
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_line(size = 1.5) +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  )

glm_fit |> 
  predict(new_data = test_proc, type = "prob") |> 
  mutate(truth = hotel_test$children) |> 
  roc_auc(truth, .pred_children)
##################
# provando que o recipes pode pre processar para que vc use em modelos comuns
# Definindo a receita
rec <- recipe(mpg ~ ., data = mtcars)  |> 
  step_center(all_predictors())  |> 
  step_scale(all_predictors())

rec_prepared <- prep(rec, training = mtcars)

data_transformed <- bake(rec_prepared, new_data = mtcars)

model <- lm(mpg ~ ., data = data_transformed)
summary(model)
