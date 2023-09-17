library(tidyverse)
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
sapply(hotels, function(x)sum(is.na(x)))]
sapply(hotels, function(x)sum(x == ''))

# Identificação e tratamento de outliers
plot_histograms(hotels)

hotels |> 
  select_if(is.numeric) |> 
  pivot_longer(1:18) |> 
ggplot(aes(x=value)) +
  geom_histogram(binwidth=30) +
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
# 6. Modelagem e Machine Learning
# 
# Regressão linear e logística
# Árvores de decisão e florestas aleatórias
# Validando e avaliando modelos

# 7. Comunicação dos Resultados
# (falar sobre)
# Como apresentar insights
# Storytelling com dados
# Relatórios e dashboards
# 9. Ética em Ciência de Dados
#  ( se sobrer tempo, poderiamos falar de)
# Privacidade e segurança dos dados
# Bias e equidade em modelagem
