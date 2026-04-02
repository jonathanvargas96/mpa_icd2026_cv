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
dados_produtos <- read_csv(caminho_produtos)

caminho_vendas <- here("data/raw/vendas.csv")
dados_vendas <- read_csv(caminho_vendas)

caminho_clientes <- here("data/raw/clientes.csv")
dados_clientes <- read_csv(caminho_clientes)

# analisa os objetos importados

glimpse(dados_produtos)
glimpse(dados_vendas)
glimpse(dados_clientes)

# combina vendas, produtos e clientes

vendas_produto <- dados_vendas |>
  left_join(dados_produtos, by = "codigo_produto")

relatorio_vendas <- vendas_produto |>
  left_join(dados_clientes, by = "id_cliente")
  
#selecionar variáveis específicas

vendas_produto |>
  select(-codigo_produto, -id_cliente, -preco_unitario)

# exibe a estrutura do resultado

glimpse(relatorio_vendas)

# identificação de NA's e comentários explicativos

# Ocorreram quatro NA's (linha "5", coluna "nome_produto"; linha "5", coluna "categoria";
# linha "6", coluna "nome_cliente"; linha "6", coluna "cidade"), pelo seguinte motivo:
# Os NA's surgiram devido a inconsistências nas chaves estrangeiras. O left_join manteve 
# as transações de venda, mas como os identificadores de produto (P006) e cliente (C004) 
# não possuíam chaves primárias correspondentes nas tabelas de cadastro, portanto as 
# variáveis de atributos (nome, categoria, cidade) não puderam ser recuperadas.

# refazer a combinação entre dados_vendas e dados_produtos e comparar o resultado obtido

comparacao <- dados_vendas |> 
  full_join(dados_produtos, by = "codigo_produto")

glimpse(comparacao)

# Ao fazer a comparação, observamos que, em ambos os joins, a linha da venda "5" aparece.
# Isso ocorre porque o código de produto P006 está em "dados_vendas". Como ele não existe
# em dados_produtos, as informações de nome e preço serão NA em ambos os casos.


# Exercício 2 ---------------------------------------------------------------


# importa os arquivos

caminho_governanca <- here("data/raw/governanca.csv")
dados_governanca <- read_csv(caminho_governanca)

caminho_risco <- here("data/raw/risco.csv")
dados_risco <- read_csv(caminho_risco)

caminho_contabeis <- here("data/raw/contabeis.csv")
dados_contabeis <- read_csv(caminho_contabeis)

# analisa os objetos importados

glimpse(dados_governanca)
glimpse(dados_risco)
glimpse(dados_contabeis)

# combina governança, risco e dados contábeis

analise_integrada <- dados_governanca |>
  left_join(dados_risco, by = "codigo_negociacao")

analise_geral <- analise_integrada |>
  left_join(dados_contabeis, by = c("codigo_negociacao", "ano"))

# exibe a estrutura do resultado

glimpse(analise_integrada)
glimpse(analise_geral)

#selecionar variáveis específicas

analise_geral |>
  select(empresa, codigo_negociacao, ano, indice_governanca, tipo_controlador,
         comite_auditoria, retorno_anual, volatilidade, beta, roa, alavancagem,
         tamanho_ativo)

# Explicações teóricas e comentários

# - por que algumas empresas podem aparecer com valores NA após os joins?:
# Os NA's surgem quando uma empresa presente na tabela de 'governanca' não possui 
# dados correspondentes nas tabelas de 'risco' ou 'contabeis'. 
# Exemplo: A empresa MGLU3 aparece na base de governança e contábeis, mas não 
# está na base de risco. Assim, suas colunas de retorno e beta se tornam NA's.

# - por que o left_join() é adequado para preservar a base principal?:
# O left_join() garante que nenhuma empresa do objeto dados_governanca seja
# excluída da análise. Mesmo que não tenha dados financeiros em um determinado ano,
# ela continua no relatório, permitindo identificar lacunas na coleta de dados.
# Se fosse utilizado inner_join(), por exemplo, as empresas com dados incompletos
# seriam deletadas do objeto "análise_integrada".


# Exercício 3 ---------------------------------------------------------------


# importa os arquivos

caminho_acoes <- here("data/raw/acoes.csv")
dados_acoes <- read_csv(caminho_acoes)

caminho_eventos_corporativos <- here("data/raw/eventos_corporativos.csv")
dados_eventos_corporativos <- read_csv(caminho_eventos_corporativos)

# analisa os objetos importados

glimpse(dados_acoes)
glimpse(dados_eventos_corporativos)

# combina ações e eventos corporativos

dados_estudo_eventos <- dados_acoes |>
  inner_join(dados_eventos_corporativos, by = c("ticker", "data" = "data_anuncio"))

#selecionar variáveis específicas

dados_estudo_eventos |>
  select(ticker, data, tipo_evento, valor, retorno_diario, volume)

# exibe o objeto final

glimpse(dados_estudo_eventos)

# Explicações teóricas e comentários

# - por que o objeto final possui menos linhas que 'dados_acoes'?:
# O objeto 'dados_acoes' contém o histórico diário de preços. O objeto 
# 'dados_estudo_eventos' contém apenas as datas em que houve um evento
# correspondente na tabela de eventos. Como os eventos corporativos são
# esporádicos, a grande maioria dos dias de negociação é descartada.

# - por que o inner_join() não mantém observações sem correspondência?:
# Para o inner_join() funcionar, a chave selecionada deve existir em AMBAS as 
# tabelas, simultaneamente. Ele atua como uma interseção de conjuntos.
# Por exemplo, se a variável "ticker" ou "data" existe no objeto 'dados_acoes'
# mas não em 'dados_eventos_corporativos' ela é removida.


# ------------------------- FIM ---------------------------------------------#
