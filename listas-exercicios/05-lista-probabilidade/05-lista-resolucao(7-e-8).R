# Arquivo: exercícios_7-e-8.R
# Autor: Jonathan Vargas Silva
# Data: 04/05/2026
# Objetivo: Resolução da lista de exercícios da aula 8 (30/04/2026)

# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(tidyverse)

# Exercício 7: Simulação: frequência relativa -------------------------------

# define semente
set.seed(2026)

# tamanhos de amostra para simulação
tamanhos_amostra <- c(1000, 10000, 100000)

# simula a frequência relativa para cada amostra
resultados_simulacao <- map_dbl(tamanhos_amostra, \(n) {
  amostra <- sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5))
  return(mean(amostra))
})

# organiza os resultados em uma tibble para análise e comparação
analise_simulacao <- tibble(
  n = tamanhos_amostra,
  frequencia_simulada = resultados_simulacao) |> 
  mutate(probabilidade_teorica = 0.5,
    desvio = frequencia_simulada - probabilidade_teorica)

# visão geral dos resultados
glimpse(analise_simulacao)


# Exercício 8: Simulação: probabilidade total e Bayes -----------------------

# define semente
set.seed(2026)

# tamanhos de amostra para simulação
n_empresas <- 100000

# simula a situação e classificação das empresas
simulacao_empresas <- tibble(id = 1:n_empresas) |> 
  mutate(situacao_real = sample(c("Dificuldade", "Saudável"), 
                           size = n(), 
                           replace = TRUE, 
                           prob = c(0.08, 0.92)), classificacao = case_when(
      situacao_real == "Dificuldade" ~ sample(c("Risco Elevado", "Normal"), 
                                              size = n(), 
                                              replace = TRUE, 
                                              prob = c(0.85, 0.15)),
      situacao_real == "Saudável"    ~ sample(c("Risco Elevado", "Normal"), 
                                              size = n(), 
                                              replace = TRUE, 
                                              prob = c(0.25, 0.75)))
  )

# estima a probabilidade condicional e compara os resultados
analise_bayes <- simulacao_empresas |> 
  summarise(proporcao_risco_elevado = mean(classificacao == "Risco Elevado"),
            probabilidade_dificuldade
            = mean(situacao_real[classificacao == "Risco Elevado"] == "Dificuldade")) |> 
  mutate(
    teorico_total_ex6 = 0.298,
    teorico_bayes_ex6 = 0.2282)

# exibe a comparação final
glimpse(analise_bayes)