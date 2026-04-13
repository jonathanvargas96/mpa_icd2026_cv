# Arquivo: 03-funcoes-iteracao.R
# Autor: Jonathan Vargas Silva
# Data: 09/04/2026
# Objetivo: Criar funções e introdução ao pacote purrr


# Configuracoes globais  --------------------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(tidyverse)


# Criando funções ---------------------------------------------------------  

# Exemplo 1

# cria a função para calcular o retorno simples
calcular_retorno <- function(preco_atual, preco_anterior) {
  retorno <- (preco_atual - preco_anterior) / preco_anterior
  return(retorno)
}

# preço de uma ação: R$45,00 ontem, R$47,50 hoje
calcular_retorno(preco_atual = 47.50, preco_anterior = 45.00)

# preço de outra ação: R$32,00 ontem, R$30,80 hoje
calcular_retorno(preco_atual = 30.80, preco_anterior = 32.00)

# Exemplo 2

# cria a função para calcular o valor futuro
calcular_valor_futuro <- function(valor_presente, taxa, periodos = 1) {
  valor_futuro <- valor_presente * (1 + taxa)^periodos
  return(valor_futuro)
}

# investimento de R$10.000 a 8% ao ano por 5 anos
calcular_valor_futuro(valor_presente = 10000, taxa = 0.08, periodos = 5)

# usando o valor padrão de períodos (1 período)
calcular_valor_futuro(valor_presente = 10000, taxa = 0.08)

# investimento de R$50.000 a 12% ao ano por 10 anos
calcular_valor_futuro(50000, 0.12, 10)

# Exemplo 3

# cria funcao para classificar o retorno de um ativo
classificar_retorno <- function(retorno) {
  if (retorno > 0.05) {
    return("Retorno Alto")
  } else if (retorno > 0) {
    return("Retorno Positivo")
  } else if (retorno == 0) {
    return("Sem Retorno")
  } else {
    return("Retorno Negativo")
  }
}

# retorno de 8%
classificar_retorno(0.08)

# retorno de 2%
classificar_retorno(0.02)

# retorno de -3%
classificar_retorno(-0.03)

# Exemplo 4

# cria função que retorna resumo estatístico em uma tibble
resumo_financeiro <- function(x) {
  tibble(
    media       = mean(x, na.rm = TRUE),
    desvio_padrao = sd(x, na.rm = TRUE),
    minimo      = min(x, na.rm = TRUE),
    maximo      = max(x, na.rm = TRUE)
  )
}

# retornos mensais fictícios de um fundo de investimento
retornos_fundo <- c(0.012, -0.008, 0.025, 0.003, -0.015, 
                    0.018, 0.007, -0.002, 0.031, 0.009,
                    -0.011, 0.022)

# aplica a função
resumo_financeiro(retornos_fundo)

# Exemplo 5

# cria função que adiciona coluna de retorno a uma tibble
adicionar_retorno <- function(dados) {
  dados |>
    mutate(retorno = (preco_atual - preco_anterior) / preco_anterior)
}

# cria uma tibble com dados de preços de ativos
precos_ativos <- tribble(
  ~ativo,       ~preco_anterior, ~preco_atual,
  "PETR4",      28.50,           29.10,
  "VALE3",      68.20,           66.80,
  "ITUB4",      32.00,           32.80,
  "BBDC4",      14.50,           14.20
)

# aplica a função
adicionar_retorno(precos_ativos)

# Exemplo 6

# Calcula o VPL de um projeto de investimento
# Argumentos:
#   investimento   - valor do investimento inicial
#   fluxos         - vetor de fluxos de caixa futuros
#   taxa           - taxa de desconto por período
#   valor_residual - valor residual ao final (padrão = 0)
# Retorna:
#   valor numérico correspondente ao VPL
calcular_vpl <- function(investimento, fluxos, taxa, valor_residual = 0) {
  n <- length(fluxos)     # número de períodos
  t <- seq_along(fluxos)  # sequência 1, 2, ..., n
  
  vpl <- -investimento +
    sum(fluxos / (1 + taxa)^t) +
    valor_residual / (1 + taxa)^n
  
  return(vpl)
}

# projeto: máquina industrial de R$500 mil
# fluxos de caixa anuais estimados por 5 anos
# valor residual da máquina: R$50 mil
# taxa de desconto: 12% ao ano (custo de capital)
calcular_vpl(
  investimento = 500000,
  fluxos = c(120000,135000, 150000, 140000, 130000),
  taxa = 0.12,
  valor_residual = 50000
)

# Exemplo 7

# importa dados contábeis simplificados de empresas
demonstracoes <- read_csv(here::here("data/raw/demonstracoes.csv"))

# visualiza os dados
glimpse(demonstracoes)

# cria função para diagnóstico financeiro de empresas
diagnostico_financeiro <- function(dados) {
  dados |>
    mutate(
      margem_liquida = lucro_liquido / receita_liquida,
      roa            = lucro_liquido / ativo_total,
      roe            = lucro_liquido / patrimonio_liquido,
      endividamento  = divida_total / ativo_total,
      situacao = case_when(
        margem_liquida > 0.08 & endividamento < 0.40 ~ "Saudável",
        margem_liquida > 0    & endividamento < 0.60 ~ "Atenção",
        TRUE ~ "Crítica"
      )
    )
}

# aplica o diagnostico financeiro
resultado <- diagnostico_financeiro(demonstracoes)

# seleciona colunas para visualização
resultado |>
  select(empresa, situacao)

# Iterações com o pacote purrr --------------------------------------------

# Lista - Exemplo 1

# lista: reúne diferentes objetos de uma análise
analise_empresa <- list(
  empresas = c("Alfa S.A.", "Beta Ltda."),
  retornos = c(0.02, -0.01, 0.03, 0.01),
  indicadores = tibble(
    liquidez = 1.8,
    rentabilidade = 0.12
  ),
  aprovada = TRUE
)

# exibe a lista
analise_empresa

# Lista - Exemplo 2

projetos <- list(
  projeto_a = list(
    fluxos = c(100000, 120000, 130000),
    taxa = 0.10,
    nome = "Expansão"
  ),
  projeto_b = list(
    fluxos = c(80000, 90000, 110000, 115000),
    taxa = 0.12,
    nome = "Nova filial"
  )
)

# exibe a lista
projetos

# map_dbl - Exemplo 1

resultado <- map_dbl(entrada, funcao_a_aplicar)

#aplica a raiz quadrada a cada elemento do vetor
map_dbl(c(4, 9, 16, 25), sqrt)

# Exemplo 8

# define as taxas de juros anuais
taxas <- c(0.02, 0.05, 0.08, 0.10, 0.12, 0.15)

# aplica calcular_valor_futuro para cada taxa
# investindo R$ 10.000 por 10 anos
valores_futuros <- map_dbl(
  taxas, 
  \(taxa) calcular_valor_futuro(10000, taxa, 10)
)

# exibe o resultado
valores_futuros

# Função anônima ----------------------------------------------------------


# estas duas formas são equivalentes:

# forma completa
map_dbl(taxas, function(taxa) calcular_valor_futuro(10000, taxa, 10))

# forma abreviada
map_dbl(taxas, \(taxa) calcular_valor_futuro(10000, taxa, 10))


# Organizando resultados em uma tibble ------------------------------------

# Exemplo 9


# 1. calcula os valores futuros para cada taxa
valor_futuro_por_taxa <- map_dbl(
  taxas,
  \(taxa) calcular_valor_futuro(10000, taxa, 10)
)

# 2. cria uma tibble comparando taxas e valores futuros
cenarios_investimento <- tibble(
  taxa_percentual = taxas * 100,
  valor_futuro = valor_futuro_por_taxa,
  ganho = valor_futuro_por_taxa - 10000
)

# exibe a tabela
cenarios_investimento

# Exemplo 10

# fluxos de caixas anuais do projeto
fluxos_projeto <- c(120000, 135000, 150000, 140000, 130000)

# taxas de desconto para comparação
taxas_desconto <- c(0.08, 0.10, 0.12, 0.15, 0.18, 0.20)

# calcula um VPL para cada taxa, mantendo o mesmo projeto
vpl_por_taxa <- map_dbl(
  taxas_desconto,
  \(taxa) calcular_vpl(500000, fluxos_projeto, taxa, 50000)
)

# organiza os resultados e classifica a viabilidade em cada taxa
analise_vpl <- tibble(
  taxa_pct = taxas_desconto * 100,
  vpl = vpl_por_taxa,
  decisao = ifelse(vpl_por_taxa > 0, "Viável", "Inviável")
)

# exibe a tibble
analise_vpl

# Exemplo 11

# função auxiliar baseada em rlnorm()
# converte media e desvio na escala original para a escala log-normal
simular_fluxos_lognormais <- function(medias, desvios) {
  variancias <- desvios^2
  meanlog <- log((medias^2) / sqrt(variancias + medias^2))
  sdlog <- sqrt(log(1 + variancias / medias^2))
  
  rlnorm(length(medias), meanlog = meanlog, sdlog = sdlog)
}

# define semente para reprodutibilidade
set.seed(42)

# fluxos esperados e incerteza de cada ano
fluxos_esperados <- c(120000, 135000, 150000, 140000, 130000)
desvios_fluxos   <- c(20000, 25000, 30000, 25000, 20000)

# simula 5.000 cenários possíveis para os fluxos de caixa do projeto
# e calcula o VPL correspondente em cada cenário
vpl_simulado <- map_dbl(
  1:5000,
  \(i) {
    # sorteia 5 fluxos, um para cada ano do projeto
    fluxos_sorteados <- simular_fluxos_lognormais(
      fluxos_esperados,
      desvios_fluxos
    )
    
    # calcula o VPL correspondente a esse cenário sorteado
    calcular_vpl(500000, fluxos_sorteados, 0.12, 50000)
  }
)

# visão geral do resultado da simulação 
glimpse(vpl_simulado)

vpl_simulado <- map_dbl(
  1:5000,
  \(i) {
    fluxos_sorteados <- simular_fluxos_lognormais(
      fluxos_esperados,
      desvios_fluxos
    )
    
    calcular_vpl(500000, fluxos_sorteados, 0.12, 50000)
  }
)

# resume a distribuição dos VPLs com métricas úteis para decisão
analise_monte_carlo <- tibble(vpl = vpl_simulado) |>
  summarise(
    vpl_medio = mean(vpl),
    prob_vpl_positivo = mean(vpl > 0),
    pior_cenario = min(vpl),
    melhor_cenario = max(vpl),
    percentil_5 = quantile(vpl, 0.05),
    percentil_95 = quantile(vpl, 0.95)
  )

# exibe o objeto
View(analise_monte_carlo)