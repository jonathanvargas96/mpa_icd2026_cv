# Arquivo: 01-avaliacao-resolucao.R
# Autor: Jonathan Vargas Silva
# Data: 16/04/2026
# Objetivo: 
# Resolução da Avaliação 1 - Introdução à Ciência de Dados


# Configurações globais  ----------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here)
library(tidyverse)


# Resolução da Questão 1


# 1.a) --------------------------------------------------------

# importa os arquivos

caminho_agencias <- here("data/raw/agencias.csv")
agencias <- read_csv(caminho_agencias)

caminho_credito_trimestral <- here("data/raw/credito_trimestral.csv")
credito_trimestral <- read_csv(caminho_credito_trimestral)

caminho_inadimplencia <- here("data/raw/inadimplencia.csv")
inadimplencia <- read_csv(caminho_inadimplencia)


# 1.b) ---------------------------------------------------------

# transforma o formato amplo em longo

credito_long <- credito_trimestral |>
  pivot_longer(
    cols = c("T1", "T2", "T3", "T4"),
    names_to = "trimestre",
    values_to = "volume_credito"
  )


# 1.c) ---------------------------------------------------------

# reune todas as informações em um único objeto

dados_completos <- credito_long |>
  left_join(agencias, by = "codigo_agencia") |>
  left_join(inadimplencia, by = c ("codigo_agencia", "trimestre"))


# 1.d) ---------------------------------------------------------

# analise dos dados completos

dados_analise <- dados_completos |> 
  mutate(
    credito_por_cooperado = (volume_credito * 1000) / num_cooperados,
    risco = case_when(
  taxa_inadimplencia < 3.0 ~ "Baixo",
  taxa_inadimplencia >= 3.0 & taxa_inadimplencia < 4.5 ~ "Moderado",
  taxa_inadimplencia >= 4.5 ~ "Alto")
)


# 1.e) ---------------------------------------------------------

# cria relatório para a diretoria

analise_diretoria <- dados_analise |> 
  group_by(cidade) |> 
  summarise(
    volume_total = sum(volume_credito),
    media_inadimplencia = mean(taxa_inadimplencia)) |> 
  arrange(desc(volume_total))


# Resolução da Questão 2


# 2.a) ---------------------------------------------------------

# cria função para calcular prestação

calcular_prestacao <- function(valor, taxa_anual, num_meses) {
  taxa_anual <- taxa_anual / 12
  prestacao <- valor * ((taxa_anual * (1 + taxa_anual)^num_meses) / 
                          ((1 + taxa_anual)^num_meses - 1))
  return(prestacao)
}

resultado_teste <- calcular_prestacao(valor = 120000, 
                                      taxa_anual = 0.12, 
                                      num_meses = 60)

# 2.b) ---------------------------------------------------------

# define as taxas de juros anuais

taxas_anuais <- c(0.08, 0.10, 0.12, 0.14, 0.16)

# calcula prestacao mensal por 60 meses

prestacoes <- map_dbl(taxas_anuais, \(x) {
  calcular_prestacao(valor = 120000, taxa_anual = x, num_meses = 60)
})

# organiza os resultados em uma tibble

resultado <- tibble(
  taxa_anual = taxas_anuais,
  prestacao_mensal = prestacoes
)

# 2.c) ---------------------------------------------------------

# adiciona colunas à tibble resultado

resultado <- resultado |> 
  mutate(
    custo_total = (prestacao_mensal * 60),
    juros_totais = (custo_total - 120000),
    acessibilidade = case_when(
      prestacao_mensal < 2600 ~ "Acessível",
      prestacao_mensal >= 2600 & prestacao_mensal < 2800 ~ "Moderado",
      prestacao_mensal >= 2800 ~ "Elevado")
    )