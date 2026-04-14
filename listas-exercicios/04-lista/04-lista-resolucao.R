# Arquivo: 04-lista-resolucao.R
# Autor: Jonathan Vargas Silva
# Data: 13/04/2026
# Objetivo: Resolução da lista de exercícios 4

# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here)      # para usar caminhos relativos
library(tidyverse) # inclui readr, dplyr, tidyr, ggplot2 etc.


# Exercício 1 ------------------------------------------------

# cria a função para calcular o montante mensal
calcular_montante_mensal <- function(capital, taxa_anual, meses) {
  taxa_mensal <- taxa_anual / 12
  montante <- capital * (1 + taxa_mensal)^meses
  return(montante)
}

# capital de R$5.000, a taxa anual de 10%, por 3 meses
calcular_montante_mensal(capital = 5000, taxa_anual = 0.10, meses = 36)

# Exercício 2 ------------------------------------------------

# cria funcao para classificar o retorno de um investimento
avaliar_investimento <- function(retorno) {
  if (retorno > 0.15) {
    return("Excelente")
  } else if (retorno > 0.05) {
    return("Bom")
  } else if (retorno > 0) {
    return("Fraco")
  } else {
    return("Negativo")
  }
}

# retorno de 20%
avaliar_investimento(0.20)
# retorno de 8%
avaliar_investimento(0.08)
# retorno de 2%
avaliar_investimento(0.02)
# retorno de -5%
avaliar_investimento(-0.05)

# Exercício 3 ------------------------------------------------

# cria função que adiciona coluna de análise de carteira a uma tibble
analisar_carteira <- function(dados) {
  resultado_df <- dados |> 
    mutate(
      retorno = (preco_atual - preco_compra) / preco_compra,
      valor_investido = preco_compra * quantidade,
      valor_atual = preco_atual * quantidade,
      resultado = valor_atual - valor_investido,
      situacao = ifelse(resultado > 0, "Ganho", "Perda")
    )
  return(resultado_df)
}

# cria uma tibble com dados de preços de ativos
carteira <- tibble(
  ativo        = c("PETR4", "VALE3", "ITUB4", "WEGE3"),
  preco_compra = c(28.50, 68.20, 32.00, 45.00),
  preco_atual  = c(31.00, 65.40, 33.60, 48.50),
  quantidade   = c(200, 100, 300, 150)
)

# aplica a função
carteira_final <- analisar_carteira(carteira)

# Exercício 4 ------------------------------------------------

# cria função para calcular valor futuro
calcular_valor_futuro <- function(taxa, capital = 10000, anos = 20) {
  valor <- capital * (1 + taxa)^anos
  return(valor)
}

# define as taxas de juros anuais
taxas_anuais <- c(0.04, 0.06, 0.08, 0.10, 0.12, 0.14, 0.16)

# cria vetor de valor futuro por 20 anos
vf_20_anos <- map_dbl(taxas_anuais, calcular_valor_futuro)

# cria uma tibble comparando cenarios
comparacao_cenarios <- tibble(
  taxa = taxas_anuais,
  valor_futuro = vf_20_anos,
  ganho_liquido = valor_futuro - 10000
)

# exibe a tabela
comparacao_cenarios

# Exercício 5 ------------------------------------------------

# Calcula o VPL de um projeto de investimento
calcular_vpl <- function(investimento, fluxos, taxa, valor_residual = 0) {
  n <- length(fluxos)
  t <- seq_along(fluxos)
  
  vpl <- -investimento + 
    sum(fluxos / (1 + taxa)^t) + 
    valor_residual / (1 + taxa)^n
  
  return(vpl)
}

# projeto: investimento inicial de R$300 mil
# valor residual da máquina: R$30 mil
# taxaS de desconto: 08%, 10%, 12%, 14%, 16% e 18%
investimento_inicial <- 300000
fluxos_caixa <- c(80000, 95000, 110000, 100000)
valor_residual_maquina <- 30000
taxas_desconto <- c(0.08, 0.10, 0.12, 0.14, 0.16, 0.18)

vpl_por_taxa <- map_dbl(taxas_desconto, calcular_vpl, 
                        investimento = investimento_inicial, 
                        fluxos = fluxos_caixa, 
                        valor_residual = valor_residual_maquina)

# organiza os resultados e classifica a viabilidade em cada taxa
analise_projeto <- tibble(
  taxa_pct = taxas_desconto * 100,
  vpl = vpl_por_taxa,
  decisao = ifelse(vpl_por_taxa > 0, "Viável", "Inviável")
)

# Exercício 6 ------------------------------------------------
# (resolver em arquivo .qmd separado)
# caminho para o arquivo Quarto criado
caminho_quarto <- here("doc", "01-report", "02-exercicio.qmd")

# Exercício 7 (Desafio) --------------------------------------

# Calcula o VPL de um projeto de investimento
# Argumentos:
#   investimento   - valor do investimento inicial
#   fluxos         - vetor de fluxos de caixa futuros
#   taxa           - taxa de desconto por período
#   valor_residual - valor residual ao final (padrão = 0)
# Retorna:
#   valor numérico correspondente ao VPL
calcular_vpl <- function(investimento, fluxos, taxa, valor_residual) {
  n <- length(fluxos) 
  t <- seq_along(fluxos)
  
  vpl <- -investimento +
    sum(fluxos / (1 + taxa)^t) +
    valor_residual / (1 + taxa)^n
  
  return(vpl)
}

# define semente para reprodutibilidade
set.seed(123)

# simula 1.000 cenários possíveis para os fluxos de caixa do projeto
# e calcula o VPL correspondente em cada cenário
vpl_sim <- map_dbl(1:1000, \(i) {
  # Sorteia 3 fluxos, um para cada ano
  fluxos_sorteados <- rnorm(3, mean = 80000, sd = 15000)
    # calcula o VPL correspondente a esse cenário sorteado
  vpl_resultado <- calcular_vpl(
    investimento = 200000, 
    fluxos = fluxos_sorteados, 
    taxa = 0.10, 
    valor_residual = 20000
  )
    return(vpl_resultado)
})

# visão geral do resultado da simulação
glimpse(vpl_sim)

# resume a distribuição dos VPLs com métricas úteis para decisão
analise_monte_carlo <- tibble(vpl = vpl_sim) |>
  summarise(
    vpl_medio = mean(vpl),
    prob_vpl_positivo = mean(vpl > 0),
    percentil_5 = quantile(vpl, 0.05),
    percentil_95 = quantile(vpl, 0.95)
  )