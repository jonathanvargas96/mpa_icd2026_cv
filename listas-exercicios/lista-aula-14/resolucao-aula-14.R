# Arquivo: resolucao-aula-14.R
# Autor: Jonathan Vargas Silva
# Data: 18/06/2026
# Objetivo: Resolução da lista de exercícios da aula 14

# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here)      # para usar caminhos relativos
library(tidyverse) # inclui readr, dplyr, tidyr, ggplot2, etc.
library(infer) # inclui specify, generate, calculate e get_confidence_interval, etc.

### Exercício 1 — Bootstrap

# define a semente
set.seed(2026)

# define a ordem esperada dos grupos
ordem_incentivos <- c("Incentivo monetário", "Incentivo não monetário")

# fluxo de Inferência
dist_bootstrap <- dados_incentivos |> 
  # 1. declarar a relação entre produtividade e grupo
  specify(produtividade ~ grupo) |> 
  # 2. gerar as amostras de bootstrap
  generate(reps = 5000, type = "bootstrap") |> 
  # 3. calcular a diferença de médias
  calculate(stat = "diff in means", order = ordem_incentivos)

# 4. intervalo de confiança de 95%
ic_95 <- dist_bootstrap |> 
  get_confidence_interval(level = 0.95)

# exibe o intervalo de confiança
ic_95

# 5. interpretação do resultado

# Interpretação: com 95% de confiança, a verdadeira diferença média de 
# produtividade entre os funcionários que receberam incentivo monetário e não
# monetário está contida entre 0.68 e 4.97 unidades de produtividade.
# Como o intervalo é totalmente positivo e não inclui o valor zero, há evidências 
# estatísticas de que o incentivo monetário de fato gera uma produtividade média 
# superior à do incentivo não monetário.


# Exercício 2: Teste por Permutação ------------------------------------------

# define a semente
set.seed(2026)
ordem_incentivos <- c("Incentivo monetário", "Incentivo não monetário")

# calcula a estatística observada
diff_observada <- dados_incentivos |> 
  specify(produtividade ~ grupo) |> 
  calculate(stat = "diff in means", order = ordem_incentivos)

# - exibe a diferença observada no console
diff_observada

# construção da distribuição nula via permutação
dist_nula_permutacao <- dados_incentivos |> 
  specify(produtividade ~ grupo) |> 
  hypothesize(null = "independence") |> 
  generate(reps = 5000, type = "permute") |> 
  calculate(stat = "diff in means", order = ordem_incentivos)

# calcula o valor-P simulado
valor_p <- dist_nula_permutacao |> 
  get_p_value(obs_stat = diff_observada, direction = "both")

# - exibe o valor-p no console
valor_p

# - o que o teste permite concluir:
# Como o valor-p é menor que o nível de significância padrão de 5% (p < 0,05), 
# rejeitamos a hipótese nula. O teste permite concluir que o tipo de incentivo
# altera de forma estatisticamente significante a produtividade média, sendo o 
# incentivo monetário superior.

# - que condição do desenho experimental sustenta a leitura causal: 
# A condição que sustenta a leitura causal é a aleatorização (randomização) na 
# distribuição dos funcionários entre os grupos. Ela garante que os grupos 
# fossem homogêneos antes do experimento, isolando o tipo de incentivo como a 
# única causa provável para a diferença de produtividade observada.

# Exercício 3: Ponte para Regressão ------------------------------------------

# ajuste do modelo de regressão linear simples
modelo_ab <- lm(tempo_min ~ versao, data = dados_ab)

# exibição detalhada dos coeficientes, erros-padrão e estatísticas
summary(modelo_ab)

# 1. estimativa de β1?
# A estimativa de β1 é 0,485. Ela indica que a versão B aumentou o tempo médio
# em 0,485 minutos em relação à versão A.

# 2. relação da estimativa de β1 com Y¯B − Y¯A?
# A estimativa é exatamente igual a essa diferença.

# 3. erro padrão da estimativa de β1?
# O erro-padrão da estimativa é 0,170

# 4. por que, neste caso, o coeficiente pode receber a interpretação causal?
# Porque os dados vêm de um experimento controlado aleatorizado (Teste A/B). 
# A randomização garante que os grupos sejam comparáveis e isola a versão da
# página como a única causa para a variação no tempo médio observado.