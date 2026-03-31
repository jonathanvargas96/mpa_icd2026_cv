# Arquivo: 03-lista-resolucao.R
# Autor(a): Jonathan Vargas Silva
# Data: 26/03/2026
# Objetivo: Resolução da lista de exercícios 3

# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here) # para usar caminhos relativos
library(tidyverse) # meta-pacote que inclui readr, dplyr, tidyr...


# Exercício 1 ---------------------------------------------------------------


# importa os arquivos

caminho_produtos <- here("data/raw/produtos.csv")
dados_produtos <- read.csv(caminho_produtos)

caminho_vendas <- here("data/raw/vendas.csv")
dados_vendas <- read.csv(caminho_vendas)

caminho_clientes <- here("data/raw/clientes.csv")
dados_clientes <- read.csv(caminho_clientes)

# analisa os objetos importados

glimpse(dados_produtos)
glimpse(dados_vendas)
glimpse(dados_clientes)

# combina vendas com produtos e clientes

vendas_produto <- dados_vendas |>
  left_join(dados_produtos, by = "codigo_produto")

relatorio_vendas <- vendas_produto |>
  left_join(dados_clientes, by = "id_cliente")

# exibe a estrutura do resultado

glimpse(relatorio_vendas)


# Exercício 2 ---------------------------------------------------------------


# importa os arquivos

caminho_governanca <- here("data/raw/governanca.csv")
dados_governanca <- read.csv(caminho_governanca)

caminho_risco <- here("data/raw/risco.csv")
dados_risco <- read.csv(caminho_risco)

caminho_contabeis <- here("data/raw/contabeis.csv")
dados_contabeis <- read.csv(caminho_contabeis)

# analisa os objetos importados

glimpse(dados_governanca)
glimpse(dados_risco)
glimpse(dados_contabeis)

# combina governança, risco e dados contábeis


# exibe a estrutura do resultado


# Exercício 3 ---------------------------------------------------------------


# importa os arquivos

caminho_acoes <- here("data/raw/acoes.csv")
dados_acoes <- read.csv(caminho_acoes)

caminho_eventos_corporativos <- here("data/raw/eventos_corporativos.csv")
dados_eventos_corporativos <- read.csv(caminho_eventos_corporativos)

# analisa os objetos importados

glimpse(dados_acoes)
glimpse(dados_eventos_corporativos)

# constrói a base do estudo de eventos


# exibe o objeto final


# ------------------------- FIM ---------------------------------------------#
