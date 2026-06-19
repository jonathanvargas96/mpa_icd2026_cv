# Arquivo: resolucao-aula-14.R
# Autor: Jonathan Vargas Silva
# Data: 19/06/2026
# Objetivo: Resolução da lista de exercícios da aula 14

# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here)      # para usar caminhos relativos
library(tidyverse) # inclui readr, dplyr, tidyr, ggplot2, etc.
library(infer) # inclui specify, generate, calculate e get_confidence_interval, etc.

# Exercício 1: Bootstrap -----------------------------------------------------

# 1. Configuração inicial e semente para reprodutibilidade
# (Sempre use uma semente antes de funções de amostragem/bootstrap)
set.seed(2026)

# Definindo a ordem esperada dos grupos conforme o enunciado (monetário - não monetário)
ordem_incentivos <- c("monetário", "não monetário")

# 2. Fluxo de Inferência com o pacote 'infer'
# Passos 1 a 3: Especificar, gerar as réplicas e calcular a estatística
dist_bootstrap <- dados_incentivos |> 
  # Passo 1: declarar a relação (produtividade em função do grupo de incentivo)
  specify(produtividade ~ grupo) |> 
  # Passo 2: gerar as amostras de bootstrap (geralmente 1000 ou 5000 réplicas)
  generate(reps = 5000, type = "bootstrap") |> 
  # Passo 3: calcular a diferença de médias na ordem correta
  calculate(stat = "diff in means", order = ordem_incentivos)

# Passo 4: Obter o intervalo de confiança de 95% (Método do Percentil)
ic_95 <- dist_bootstrap |> 
  get_confidence_interval(level = 0.95)

# Exibe o intervalo de confiança no console (limites inferior e superior)
ic_95


# Passo 5: Interpretação do resultado (Item 5) -------------------------------

# [Substitua os valores abaixo (X.XX e Y.YY) pelos números que saírem no seu console]
# Interpretação: Com 95% de confiança, a verdadeira diferença média de produtividade 
# entre os funcionários que receberam incentivo monetário e não monetário está contida 
# entre X.XX e Y.YY unidades de produtividade. 

# DICA DE PROVA: Se o intervalo conter o valor ZERO (ex: de -1.5 a 3.2), significa que
# não há evidência estatística de que um incentivo seja melhor que o outro. Se o 
# intervalo for totalmente POSITIVO (ex: de 1.2 a 5.4), o incentivo monetário é 
# significativamente superior.


# Exercício 2: Teste por Permutação ------------------------------------------

# 1. Configuração de semente para garantir a reprodutibilidade da permutação
set.seed(2026)
ordem_incentivos <- c("monetário", "não monetário")

# 2. Cálculo da Estatística Observada (Diferença de médias real dos dados)
diff_observada <- dados_incentivos |> 
  specify(produtividade ~ grupo) |> 
  calculate(stat = "diff in means", order = ordem_incentivos)

# Exibe a diferença observada no console
diff_observada


# 3. Construção da Distribuição Nula via Permutação
# Sequência obrigatória: specify -> hypothesize -> generate -> calculate
dist_nula_permutacao <- dados_incentivos |> 
  # Passo 1: Especificar a relação
  specify(produtividade ~ grupo) |> 
  # Passo 2: Declarar a hipótese nula (independência entre as variáveis)
  hypothesize(null = "independence") |> 
  # Passo 3: Gerar as permutações (embaralhar os rótulos de grupo)
  generate(reps = 5000, type = "permute") |> 
  # Passo 4: Calcular a estatística para cada cenário sob a hipótese nula
  calculate(stat = "diff in means", order = ordem_incentivos)


# 4. Cálculo do Valor-P Simulado
# Passo 5: Comparar a diff_observada com a distribuição nula (teste bicaudal)
valor_p <- dist_nula_permutacao |> 
  get_p_value(obs_stat = diff_observada, direction = "both")

# Exibe o valor-p no console
valor_p


# 5. Interpretação dos Resultados (Respostas aos Itens do Enunciado) --------

# - DIFERENÇA OBSERVADA: 
# [Substitua o valor abaixo pelo número gerado no objeto 'diff_observada']
# A diferença observada de X.XX indica a distância bruta entre a produtividade 
# média do grupo monetário e do grupo não monetário na amostra original.

# - VALOR-P SIMULADO:
# [Substitua pelo valor impresso no objeto 'valor_p']
# O valor-p simulado foi de Y.YY. Ele representa a probabilidade de encontrarmos 
# uma diferença de médias tão ou mais extrema que a observada (X.XX), assumindo 
# que o tipo de incentivo não possui efeito nenhum sobre a produtividade.

# - O QUE O TESTE PERMITE CONCLUIR:
# [Regra de decisão baseado no valor-p padrão de 5% / 0.05]
# Se valor-p < 0.05: Rejeitamos a hipótese nula. O teste permite concluir que 
# o tipo de incentivo altera de forma estatisticamente significante a produtividade.
# Se valor-p >= 0.05: Não rejeitamos a hipótese nula. Não há evidências de que 
# o tipo de incentivo altere a produtividade média.

# - CONDIÇÃO DO DESENHO EXPERIMENTAL QUE SUSTENTA A LEITURA CAUSAL:
# A condição indispensável que sustenta a interpretação causal (afirmar que o 
# incentivo X CAUSOU o aumento na produtividade) é a **ALEATORIZAÇÃO** (Randomização). 
# O fato de os funcionários terem sido distribuídos nos grupos de incentivo de forma 
# completamente aleatória garante que os grupos sejam estatisticamente homogêneos 
# antes do experimento, eliminando variáveis de confusão (vieses) e isolando o 
# efeito puramente causal do tipo de incentivo.


# Exercício 3: Ponte para Regressão (Teste A/B) ------------------------------

# 1. Ajuste do modelo de regressão linear simples
# Nota: O R converte automaticamente variáveis categóricas/fatores (como Versão A/B)
# em variáveis indicadoras (dummies 0 e 1) internamente no modelo lm().
modelo_ab <- lm(tempo ~ versao, data = dados_ab)

# 2. Exibição detalhada dos coeficientes, erros-padrão e estatísticas
summary(modelo_ab)


# Respostas Conceituais (Itens 1 a 4) ----------------------------------------

# 1. QUAL É A ESTIMATIVA DE β1?
# [Substitua pelo valor encontrado na linha correspondente à 'versaoB' na coluna 'Estimate']
# A estimativa de β1 é o coeficiente associado à dummy 'versaoB' (ex: X.XX). Ela representa
# o efeito estimado de mudar da versão de referência (Versão A) para a nova versão (Versão B)
# sobre a variável dependente 'tempo'.

# 2. COMO ELA SE RELACIONA COM Y¯B − Y¯A?
# A estimativa de β1 é **exatamente igual** à diferença entre a média amostral do grupo B 
# e a média amostral do grupo A (Y¯B − Y¯A). Em termos matemáticos, em uma regressão com 
# uma única variável explicativa binária, o intercepto (β0) assume o valor da média do 
# grupo base (Y¯A) e o coeficiente de inclinação (β1) captura o deslocamento exato para 
# a média do grupo de tratamento (β1 = Y¯B − Y¯A).

# 3. QUAL É O ERRO-PADRÃO DESTA ESTIMATIVA?
# [Substitua pelo valor encontrado na linha 'versaoB' na coluna 'Std. Error']
# O erro-padrão da estimativa de β1 é Y.YY. Ele mede a variabilidade ou incerteza associada 
# ao coeficiente devido ao erro amostral. Sob homocedasticidade, ele se equivale rigorosamente 
# ao erro-padrão combinado usado no teste-t clássico de comparação de duas médias independentes.

# 4. POR QUE, NESTE CASO, O COEFICIENTE PODE RECEBER INTERPRETAÇÃO CAUSAL?
# O coeficiente β1 pode receber interpretação causal porque os dados provêm de um **experimento 
# controlado aleatorizado (Teste A/B)**. A atribuição estocástica das versões (A ou B) garante 
# que o termo de erro (ui) — que guarda as características não observadas dos usuários — seja 
# completamente independente da versão recebida (E[ui | Versão] = 0). Isso elimina o viés de 
# seleção e a endogenidade, permitindo afirmar que qualquer mudança observada em 'tempo' foi 
# de fato provocada (causada) pela alteração da 'Versão'.