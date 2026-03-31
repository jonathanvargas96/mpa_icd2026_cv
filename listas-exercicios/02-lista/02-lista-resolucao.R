# ============================================================
# Disciplina: Introdução à Ciência de Dados
# ============================================================
# Arquivo: 02-lista-resolucao.R
# Autor(a): Jonathan Vargas Silva
# Data: 2026/03/19
# Objetivo: Resolução da lista de exercícios 2

# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here)      # para usar caminho relativo
library(tidyverse) # meta-pacote que inclui readr, dplyr..
library(gapminder) # contém os dados gapminder

# carrega os dados do pacote gapminder
data(gapminder)


## Exercício 1

caminho_csv <- here("data/raw/productionlog_sample.csv")
dados_csv <- read.csv(caminho_csv)
glimpse(dados_csv)

## Exercício 2

dados_expectativa <- gapminder |>
  select(country, year, lifeExp)


## Exercício 3

variaveis_exc_pop_e_gdppercap <- gapminder |>
  select(-pop, -gdpPercap)

## Exercício 4

variaveis_com_c <- gapminder |>
  select(starts_with("c"))

## Exercício 5

variaveis_sequencia <- gapminder |>
  select(country:pop)

# Exercício 6

variaveis_com_p <- gapminder |>
  select(contains("p") | ends_with("p"))

# Exercício 7

paises_america_2007 <- gapminder |>
  filter(continent == "Americas" & year == "2007")

# Exercício 8

dados_brasil <- gapminder |> 
  filter(country == "Brazil")

# Exercício 9

asia_pop_2007 <- gapminder |>
  filter(continent == "Asia", pop > 50000000, year == "2007")
  
# Exercício 10

paises_expVida_ppc <- gapminder |>
  filter(lifeExp > 75, gdpPercap < 10000, year == "2007")

# Exercício 11

populacao_em_mi <- gapminder |>
  mutate(pop_em_milhoes = pop / 1000000)

# Exercício 12

receita_total <- gapminder |>
  mutate(gdp_total = gdpPercap * pop)

# Exercício 13

maiores_populacoes <- gapminder |> 
  mutate(economia_grande = ifelse(pop > 50000000, "Sim", "Não"))

# Exercício 14

paises_expVida_ranking <- gapminder |> 
  filter(year == 2007) |> 
  mutate(lifeExp_ranking = case_when(
      lifeExp < 60 ~ "Baixa",
      lifeExp >= 60 & lifeExp <= 75 ~ "Média",
      lifeExp > 75 ~ "Alta"))

# Exercício 15

expectativa_por_continente <- gapminder |>
  group_by(continent) |>
  summarise(expectativa_media = mean(lifeExp))

# Exercício 16

populacao_por_continente <- gapminder |>
  group_by(continent) |>
  summarise(populacao_total = sum(pop))

# Exercício 17

desempenho_filiais <- gapminder |> 
  filter(year == 2007) |> 
  group_by(continent) |> 
  summarise(n_filiais = n(), pib_medio = mean(gdpPercap),
            pib_maximo = max(gdpPercap))

# Exercício 18

evolucao_expVida_americas <- gapminder |> 
  filter(continent == "Americas") |> 
  group_by(year) |> 
  summarise(expectativa_media = mean(lifeExp))

# Exercício 19

ranking_expVida_2007 <- gapminder |> 
  filter(year == 2007) |> 
  arrange(desc(lifeExp))

# Exercício 20

menores_pib_2007 <- gapminder |> 
  filter(year == 2007) |> 
  arrange(gdpPercap) |> 
  head(5)

# Exercício 21

ranking_americas_2007 <- gapminder |> 
  filter(year == 2007, continent == "Americas") |> 
  arrange(desc(pop))

# Exercício 22

ranking_continentes_2007 <- gapminder |> 
  filter(year == 2007) |> 
  group_by(continent) |> 
  summarise(expectativa_media = mean(lifeExp)) |> 
  arrange(desc(expectativa_media))