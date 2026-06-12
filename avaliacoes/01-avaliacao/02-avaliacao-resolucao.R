  # Arquivo: 02-avaliacao-resolucao.R
  # Autor(a): Jonathan Vargas Silva
  # Data: 11/06/2026
  # Objetivo: Resolução da Avaliação 2 - Introdução à Ciência de Dados
  
  
  # Configurações globais  ----------------------------------------
  
  # define opções globais para exibição de números
  options(digits = 5, scipen = 999)
  
  # Carrega os pacotes necessários
  library(tidyverse) # dplyr, purrr::pmap_dbl() etc.
  library(EnvStats)  # distribuição triangular: rtri()
  library(tidyquant) # tq_get()
  
  
  # Resolução da Questão 1 ----------------------------------------
  
  # fixa a semente para reprodutibilidade
  set.seed(2026)
  
  # número de cenários simulados
  n_sim <- 20000
  
  # duração do projeto, em anos (conhecida)
  duracao <- 5
  
  # (a.1) Simula os parâmetros incertos de cada cenário, em R$ mil.
  cenarios <- tibble(
    cenario = seq_len(n_sim),
    investimento = rtri(n_sim, min = 850, max = 1200, mode = 1000),
    receita_anual = rtri(n_sim, min = 230, max = 350, mode = 290),
    valor_residual = rtri(n_sim, min = 100, max = 200, mode = 150),
    taxa_desconto = rtri(n_sim, min = 0.11, max = 0.15, mode = 0.13)
  )
  
  # (a.2) Função que calcula  VPL de UM cenário.
  # Traduza a fórmula do enunciado
  calcular_vpl <- function(investimento, receita_anual,
                           valor_residual, taxa_desconto,
                           duracao = 5) {
    
    # cria o vetor 1, 2, ..., duracao
    anos <- seq_len(duracao)
  
    # valor presente das receitas (constantes) ao longo dos anos
    vp_receitas <- sum(receita_anual / (1 + taxa_desconto)^anos)
    
    # valor presente do valor residual, recebido no final
    vp_residual <- valor_residual / (1 + taxa_desconto)^duracao
    
    # VPL do cenário (combine os três componentes acima)
    vpl <- -investimento + vp_receitas + vp_residual
    return(vpl)
  }
  
  # (a.3) Aplica a função a todos os cenários com
  # a funçao apropriada do pacote purr e armazene em vpl
  simulacoes <- cenarios |> 
    mutate(
      vpl = pmap_dbl(
      list(
        investimento = investimento,
        receita_anual = receita_anual,
        valor_residual = valor_residual,
        taxa_desconto = taxa_desconto
        ),
      # nome da função a aplicar
      calcular_vpl
      )
    )
  
  # vetor de VPLs simulados (uso nos itens b, c e d)
  vpl_sim <- simulacoes$vpl
  
  # (b) Probabilidade de o projeto destruir valor: P(VPL < 0).
  prob_vpl_neg <- mean(vpl_sim < 0)
  
  # exibe a probabilidade em percentual
  prob_vpl_neg * 100
  
  # Interpretação (item b)
  
  # A probabilidade estimada de o projeto destruir valor é de aproximadamente 23,21%.
  # No contexto de decisão de investimentos da empresa, esse resultado indica que
  # em cerca de 23 em cada 100 cenários simulados de flutuação de mercado, o 
  # projeto não cobrirá o investimento inicial e o custo de oportunidade.
  # Essa probabilidade de insucesso na casa de 23% indica um nível de risco
  # moderado, exigindo que a empresa avalie se possui margem de liquidez para
  # absorver cenários desfavoráveis antes de homologar o projeto.
  
  # (c) VPL determinístico (usando as MODAS de cada parâmetro)
  # e comparação
  vpl_det <- calcular_vpl(
    investimento = 1000,
    receita_anual = 290,
    valor_residual = 150,
    taxa_desconto = 0.13,
  )
  
  # média dos VPLs simulados (comparada com o VPL determinístico)
  vpl_medio <- mean(vpl_sim)
  
  # desvio padrão dos VPLs simulados
  vpl_desvio <- sd(vpl_sim)
  
  # exibe os valores para comparação
  vpl_det
  vpl_medio
  vpl_desvio
  
  # Interpretação (item c): 
  
  # Os valores não coincidem pelo fato de que a fórmula do VPL não é linear em 
  # relação à taxa de desconto e porque a simulação de Monte Carlo captura a 
  # assimetria e a variabilidade total dos extremos das distribuições triangulares,
  # enquanto o modelo determinístico ignora esses riscos ao analisar apenas um 
  # único ponto estático (as modas).
  
  # (d) Histograma da distribuição simulada
  hist(vpl_sim,
       breaks = 50,
       col = "lightblue",
       main = "Distribuição simulada do VPL",
       xlab = "VPL (R$ mil)")
  
  # linha vertical em VPL = 0
  abline(v=0, col = "red", lwd = 2, lty = 2)
  
  # linha vertical no VPL médio simulado
  abline(v = mean(vpl_sim), col = "blue", lwd = 2)
  
  # legenda das duas linhas
  legend("topright",
         legend = c("VPL = 0", "VPL médio simulado"),
         col = c("red", "blue"),
         lwd = 2, lty = c(2,1), bty = "n")
  
  
  # Resolução da Questão 2 ----------------------------------------
  
  # (a) Importe os preços (ajustados) e calcule-os
  # retorno log diários .
  precos_vale3 <- "VALE3.SA" |>
    tq_get(get = "stock.prices",
           from = "2024-01-01",
           to = "2026-06-08") |>
    select(date, adjusted)
  
  retornos_vale3 <- precos_vale3 |>
    # fórmula do retorno log diário (usando preço ajustado)
    mutate(ret = log(adjusted / dplyr::lag(adjusted))) |>
    drop_na()
  
  # extrai a coluna ret como um vetor
  ret_vale3 <- retornos_vale3 |> pull(ret)
  
  # (b) Parâmetros do problema (de acordo com o enunciado)
  valor_carteira <- 25000
  p <- 0.01
  
  # (c) VaR histórico: quantil da cauda esquerda
  
  # ordena os retornos do pior para o melhor.
  ret_ordenado <- sort(ret_vale3)
  
  # posicao do quantil de p
  k <- ceiling(length(ret_ordenado) * p)
  
  # retorno no ponto de corte
  retorno_var <- ret_ordenado[k]
  
  # converte o retorno em perda positiva (%)
  var_percentual <- -ret_ordenado[k] * 100
  
  # VaR em reais
  var_monetario  <- -ret_ordenado[k] * valor_carteira
  
  # exibe os valores de VaR
  var_percentual
  var_monetario
  
  # (d) Expected Shortfall
  
  # média dos retornos nas posições 1 até k
  retorno_medio_cauda <- mean(ret_ordenado[1:k])
    
  # ES em %, como perda positiva
  es_percentual  <- -retorno_medio_cauda * 100
  
  # ES em reais
  es_monetario   <- -retorno_medio_cauda * valor_carteira
  
  # exibe os valores do ES
  es_percentual
  es_monetario
  
  # (e) Interpretação:
  
  # Ao interpretar os resultados, pode-se concluir que a divergência existente 
  # entre as duas métricas, ocorre porque, enquanto o VaR informa o ponto de corte
  # da cauda de perdas, o Expected Shortfall informa a perda média nos casos em 
  # que esse ponto de corte é ultrapassado. Ou seja, o ES não apenas identifica 
  # onde começa a cauda extrema, mas também resume a severidade média das perdas 
  # dentro dessa cauda. 
  
  
  # Resolução da Questão 3 ----------------------------------------
  
  # (a) Importe os preços (ajustados), organize em formato largo
  # e calcule os retornos SIMPLES diários de cada ação.
  serie_precos <- c("ITUB4.SA", "VALE3.SA", "WEGE3.SA") |>
    tq_get(get = "stock.prices",
           from = "2024-01-01",
           to = "2026-06-08") |>
    select(symbol, date, adjusted) |>
    pivot_wider(names_from = symbol,
                values_from = adjusted) |>
    rename(dia = date,
           itub4 = ITUB4.SA,
           vale3 = VALE3.SA,
           wege3 = WEGE3.SA)
  
  # retorno simples = preço / preço anterior -1
  retornos <- serie_precos |>
    mutate(
      # exemplo: retorno simples do ITUB4
      ret_itub4 = itub4 / dplyr::lag(itub4) - 1,
      # aplique a mesma fórmula
      ret_vale3 = vale3 / dplyr::lag(vale3) - 1,
      ret_wege3 = wege3 / dplyr::lag(wege3) - 1
    ) |>
    # remove os dados faltantes
    drop_na()
  
  # (b) Calcule o retorno diário da carteira.
  
  # pesos de carteira, na ordem: itub4, vale3, wege3
  pesos <- c(0.40, 0.35, 0.25)
  
  retornos <- retornos |>
    # combine os três retornos com os pesos
    mutate(ret_carteira = (pesos[1] * ret_itub4) + (pesos[2] * ret_vale3) + (pesos[3] * ret_wege3))
  
  # (c)-(d) Parâmetros e medidas de risco (VaR e ES)
  
  # parâmetros do problemas
  valor_carteira <- 100000
  p <- 0.01
  
  # ordena os retornos do pior para o melhor
  ret_ordenado <- sort(retornos$ret_carteira)
  
  # posição do quantil de p
  k <- ceiling(length(ret_ordenado) * p)
  
  # VaR em %, como perda positiva
  var_percentual <- -ret_ordenado[k] * 100
  
  # VaR em reais
  var_monetario  <- -ret_ordenado[k] * valor_carteira
  
  # ES em %, como perda positiva
  es_percentual  <- -mean(ret_ordenado[1:k]) * 100
  
  # ES em reais
  es_monetario   <- -mean(ret_ordenado[1:k]) * valor_carteira
  
  # exibe os valores do ES
  var_percentual
  var_monetario
  es_percentual
  es_monetario
  
  # (e) Interpretação:
  
  # Ao interpretar os resultados, podemos concluir que O Expected Shortfall (ES) é
  # maior, em magnitude, porque Como o ES resume perdas ainda mais severas do que 
  # o ponto de corte usado no VaR. O VaR é a porta de entrada para o pior 1% de 
  # dias de crise do mercado. Já o ES calcula a média de todas as perdas que 
  # ultrapassaram a linha do VaR, resultando sempre em uma métrica de risco mais 
  # conservadora e severa.