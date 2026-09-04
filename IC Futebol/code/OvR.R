
# =======================
# Código Futebol
# Classificador OvR 
# Essa versão: 01/09/2026      
# =======================


# ############ #
# Preliminares #
# ############ #

## Limpando o workspace
rm(list = ls())

## Carregando pacotes necessários
library(car)           
library(caret)     
library(corrplot)
library(DHARMa)
library(dplyr)
library(ggplot2)       
library(glmtoolbox)    
library(haven)
library(openxlsx)
library(performance)   
library(pROC)          
library(see)           
library(tidyverse)
library(writexl)


## Diretório de trabalho
# setwd('C:/Users/Uriel/Desktop/IC Futebol/code')
setwd('/home/uriel/Desktop/IC Futebol/code')


## Carregando dados: Brasileirão Série A, 2025
df_jogos = read.xlsx('_dta/BRA.xlsx')
glimpse(df_jogos)


## filtrando por 2025
df_jogos = df_jogos %>% subset(Season == 2025)
table(df_jogos$Home)


## criando uma variável de rodadas
## NOTA: aqui usamos uma .csv externa
# lendo a base
df_rodadas = read.csv(
  "_dta/campeonato_brasileiro_serie_A_2003_2025 - campeonato-brasileiro-full.csv"
)
# transformando os nomes
df_rodadas = df_rodadas %>% rename(
  Home = mandante,
  Away = visitante
)
# criando o ano
df_rodadas$Season = as.numeric(substr(df_rodadas$data, start = 7, stop = 11))
# filtrando por 2025
df_rodadas = df_rodadas %>% subset(Season == 2025)
# trocando o dicionário de times (Home)
df_rodadas =  df_rodadas %>%  mutate(Home = case_when(
  Home == "america_mg" ~ "America MG",
  Home == "athletico_pr" ~ "Athletico-PR",
  Home == "atletico_go" ~ "Atletico GO",
  Home == "atletico_mg" ~ "Atletico-MG",
  Home == "avai" ~ "Avai",
  Home == "bahia" ~ "Bahia",
  Home == "botafogo_rj" ~ "Botafogo RJ",
  Home == "bragantino" ~ "Bragantino",
  Home == "csa" ~ "CSA",
  Home == "ceara" ~ "Ceara",
  Home == "chapecoense" ~ "Chapecoense-SC",
  Home == "corinthians" ~ "Corinthians",
  Home == "coritiba" ~ "Coritiba",
  Home == "criciuma" ~ "Criciuma",
  Home == "cruzeiro" ~ "Cruzeiro",
  Home == "cuiaba" ~ "Cuiaba",
  Home == "figueirense" ~ "Figueirense",
  Home == "flamengo" ~ "Flamengo RJ",
  Home == "fluminense" ~ "Fluminense",
  Home == "fortaleza" ~ "Fortaleza",
  Home == "goias" ~ "Goias",
  Home == "gremio" ~ "Gremio",
  Home == "internacional" ~ "Internacional",
  Home == "joinville" ~ "Joinville",
  Home == "juventude" ~ "Juventude",
  Home == "mirassol" ~ "Mirassol",
  Home == "nautico" ~ "Nautico",
  Home == "palmeiras" ~ "Palmeiras",
  Home == "parana" ~ "Parana",
  Home == "ponte_preta" ~ "Ponte Preta",
  Home == "portuguesa" ~ "Portuguesa",
  Home == "remo" ~ "Remo",
  Home == "santa_cruz" ~ "Santa Cruz",
  Home == "santos" ~ "Santos",
  Home == "sao_paulo" ~ "Sao Paulo",
  Home == "sport" ~ "Sport Recife",
  Home == "vasco" ~ "Vasco",
  Home == "vitoria" ~ "Vitoria" 
))
df_rodadas =  df_rodadas %>%  mutate(Away = case_when(
  Away == "america_mg" ~ "America MG",
  Away == "athletico_pr" ~ "Athletico-PR",
  Away == "atletico_go" ~ "Atletico GO",
  Away == "atletico_mg" ~ "Atletico-MG",
  Away == "avai" ~ "Avai",
  Away == "bahia" ~ "Bahia",
  Away == "botafogo_rj" ~ "Botafogo RJ",
  Away == "bragantino" ~ "Bragantino",
  Away == "csa" ~ "CSA",
  Away == "ceara" ~ "Ceara",
  Away == "chapecoense" ~ "Chapecoense-SC",
  Away == "corinthians" ~ "Corinthians",
  Away == "coritiba" ~ "Coritiba",
  Away == "criciuma" ~ "Criciuma",
  Away == "cruzeiro" ~ "Cruzeiro",
  Away == "cuiaba" ~ "Cuiaba",
  Away == "figueirense" ~ "Figueirense",
  Away == "flamengo" ~ "Flamengo RJ",
  Away == "fluminense" ~ "Fluminense",
  Away == "fortaleza" ~ "Fortaleza",
  Away == "goias" ~ "Goias",
  Away == "gremio" ~ "Gremio",
  Away == "internacional" ~ "Internacional",
  Away == "joinville" ~ "Joinville",
  Away == "juventude" ~ "Juventude",
  Away == "mirassol" ~ "Mirassol",
  Away == "nautico" ~ "Nautico",
  Away == "palmeiras" ~ "Palmeiras",
  Away == "parana" ~ "Parana",
  Away == "ponte_preta" ~ "Ponte Preta",
  Away == "portuguesa" ~ "Portuguesa",
  Away == "remo" ~ "Remo",
  Away == "santa_cruz" ~ "Santa Cruz",
  Away == "santos" ~ "Santos",
  Away == "sao_paulo" ~ "Sao Paulo",
  Away == "sport" ~ "Sport Recife",
  Away == "vasco" ~ "Vasco",
  Away == "vitoria" ~ "Vitoria" 
))


## fazendo o merge das rodadas
dta = merge(df_jogos, df_rodadas, by = c('Home', 'Away', 'Season'))


## renomeando variáveis
dta = dta %>% rename(
  Team_A = 'Home', Score_A = 'HG', 
  Team_B = 'Away', Score_B = 'AG',
  Week = 'rodata',
  Odds_L = 'MaxCA', Odds_D = 'MaxCD', Odds_W = 'MaxCH')


## diltrando colunas
dta = dta %>% dplyr::select(Team_A, Team_B, 
                            Score_A, Score_B, 
                            Week,
                            Odds_L, Odds_D, Odds_W)

## criando variável de resultado
dta = dta %>%
  mutate(dg = Score_A - Score_B) %>%
  mutate(Y = case_when(
    dg < 0 ~ 'L',
    dg == 0 ~ 'D',
    dg > 0 ~ 'W'
  ))


## convertendo para fator ordenado
dta$Y = factor(dta$Y, levels = c('L', 'D', 'W'))
contrasts(dta$Y)


## carregando objetos dos modelos binários
load('_out/out_w.RData') # vitórias
load('_out/out_d.RData') # empates
load('_out/out_l.RData') # derrotas








# ===========================================================================
# ===========================================================================
# Classificador OvR (banco completo)
# ===========================================================================
# ===========================================================================

# ## classificador OvR: maioria simples
# df_OvR = data.frame(
#   prob_hat_w = pi.hat_w,
#   prob_hat_d = pi.hat_d,
#   prob_hat_l = pi.hat_l
# )


## classificador OvR: max. acurácia balanceada
prob = table(dta$Y)
prob = prob/sum(prob)
df_OvR = data.frame(
  prob_hat_w = pi.hat_w/prob[3],
  prob_hat_d = pi.hat_d/prob[2],
  prob_hat_l = pi.hat_l/prob[1]
)


## classificador OvR
Y.hat_OvR = apply(df_OvR, 1, function(x) {
  c("W", "D", "L")[which.max(x)]
})
Y.hat_OvR = factor(Y.hat_OvR, levels = c('L', 'D', 'W'))


## compondo o df de classificação
df_OvR$Y = dta$Y
df_OvR$Y_hat_OvR = Y.hat_OvR


## matriz de confusão (manual)
tab = table(Y = df_OvR$Y, Y.hat = df_OvR$Y_hat_OvR)
tab
round(1e2*sum(diag(tab/sum(tab))), 2)


## frequências dos resultados
table(dta$Y)
round(1e2 * table(dta$Y) / NROW(dta), 2)


## matriz de confusão via caret
# predito + referência caret
tab = confusionMatrix(data = df_OvR$Y_hat_OvR, reference = df_OvR$Y)
tab
# salvando em .txt
sink(file = '_out/output/tabConfMatrix_OvR (full).txt')
cat('\n')
print(tab)
sink()


## incluindo as odds 
## -- (para ponderar o classificador)
# total de jogos
n = NROW(df_OvR)
n_L = length(which(dta$Y == 'L'))
n_D = length(which(dta$Y == 'D'))
n_W = length(which(dta$Y == 'W'))
n
n_L + n_D + n_W
# acertos
games_won = 1*(df_OvR$Y == df_OvR$Y_hat_OvR)
cash_L = sum(games_won[which(dta$Y == 'L')]*dta$Odds_L[which(dta$Y == 'L')])
cash_D = sum(games_won[which(dta$Y == 'D')]*dta$Odds_D[which(dta$Y == 'D')])
cash_W = sum(games_won[which(dta$Y == 'W')]*dta$Odds_W[which(dta$Y == 'W')])
# receita
revenue = cash_L + cash_D + cash_W
revenue_L = cash_L
revenue_D = cash_D
revenue_W = cash_W
revenue
revenue_L + revenue_D + revenue_W
# lucro
profit = revenue - n
profit_L = cash_L - n_L
profit_D = cash_D - n_D
profit_W = cash_W - n_W
profit
profit_L + profit_D + profit_W








# ===========================================================================
# ===========================================================================
# Classificador OvR (banco treino)
# ===========================================================================
# ===========================================================================

## treino/teste split (p/ previsão)
dta_treino = dta %>% filter(Week <= 34)
dta_teste  = dta %>% filter(Week > 34)


# ## classificador OvR: maioria simples
# df_OvR_treino = data.frame(
#   prob_hat_w = pi.hat_w_treino,
#   prob_hat_d = pi.hat_d_treino,
#   prob_hat_l = pi.hat_l_treino
# )


## classificador OvR: max. acurácia balanceada
prob = table(dta_treino$Y)
prob = prob/sum(prob)
df_OvR_treino = data.frame(
  prob_hat_w = pi.hat_w_treino/prob[3],
  prob_hat_d = pi.hat_d_treino/prob[2],
  prob_hat_l = pi.hat_l_treino/prob[1]
)


## classificador OvR
Y.hat_OvR_treino = apply(df_OvR_treino, 1, function(x) {
  c("W", "D", "L")[which.max(x)]
})
Y.hat_OvR_treino = factor(Y.hat_OvR_treino, levels = c('L', 'D', 'W'))


## compondo o df de classificação
df_OvR_treino$Y = dta_treino$Y
df_OvR_treino$Y_hat_OvR_treino = Y.hat_OvR_treino


## matriz de confusão (manual)
tab_treino = table(Y = df_OvR_treino$Y, Y.hat = df_OvR_treino$Y_hat_OvR_treino)
tab_treino
round(1e2*sum(diag(tab_treino/sum(tab_treino))), 2)


## frequências dos resultados
table(dta_treino$Y)
round(1e2 * table(dta_treino$Y) / NROW(dta_treino), 2)


## matriz de confusão via caret
# predito + referência caret
tab_treino = confusionMatrix(data = df_OvR_treino$Y_hat_OvR_treino, reference = df_OvR_treino$Y)
tab_treino
# salvando em .txt
sink(file = '_out/output/tabConfMatrix_OvR (train).txt')
cat('\n')
print(tab_treino)
sink()


## incluindo as odds 
## -- (para ponderar o classificador)
# total de jogos
n = NROW(df_OvR_treino)
n_L = length(which(dta_treino$Y == 'L'))
n_D = length(which(dta_treino$Y == 'D'))
n_W = length(which(dta_treino$Y == 'W'))
n
n_L + n_D + n_W
# acertos
games_won = 1*(df_OvR_treino$Y == df_OvR_treino$Y_hat_OvR)
cash_L = sum(games_won[which(dta_treino$Y == 'L')]*dta_treino$Odds_L[which(dta_treino$Y == 'L')])
cash_D = sum(games_won[which(dta_treino$Y == 'D')]*dta_treino$Odds_D[which(dta_treino$Y == 'D')])
cash_W = sum(games_won[which(dta_treino$Y == 'W')]*dta_treino$Odds_W[which(dta_treino$Y == 'W')])
# receita
revenue = cash_L + cash_D + cash_W
revenue_L = cash_L
revenue_D = cash_D
revenue_W = cash_W
revenue
revenue_L + revenue_D + revenue_W
# lucro
profit = revenue - n
profit_L = cash_L - n_L
profit_D = cash_D - n_D
profit_W = cash_W - n_W
profit
profit_L + profit_D + profit_W








# ===========================================================================
# ===========================================================================
# Classificador OvR (banco teste)
# ===========================================================================
# ===========================================================================

## classificador OvR: maioria simples
df_OvR_teste = data.frame(
  prob_hat_w = pi.hat_w_teste,
  prob_hat_d = pi.hat_d_teste,
  prob_hat_l = pi.hat_l_teste
)


## classificador OvR: max. acurácia balanceada
prob = table(dta_treino$Y)
prob = prob/sum(prob) # NOTA: aqui as prob são do treino!
df_OvR_teste = data.frame(
  prob_hat_w = pi.hat_w_teste/prob[3],
  prob_hat_d = pi.hat_d_teste/prob[2],
  prob_hat_l = pi.hat_l_teste/prob[1]
)


## classificador OvR
Y.hat_OvR_teste = apply(df_OvR_teste, 1, function(x) {
  c("W", "D", "L")[which.max(x)]
})
Y.hat_OvR_teste = factor(Y.hat_OvR_teste, levels = c('L', 'D', 'W'))


## compondo o df de classificação
df_OvR_teste$Y = dta_teste$Y # pode ser também em dta_d ou dta_l
df_OvR_teste$Y_hat_OvR_teste = Y.hat_OvR_teste


## matriz de confusão (manual)
tab_teste = table(Y = df_OvR_teste$Y, Y.hat = df_OvR_teste$Y_hat_OvR_teste)
tab_teste
round(1e2*sum(diag(tab_teste/sum(tab_teste))), 2)


## frequências dos resultados
table(dta_teste$Y)
round(1e2 * table(dta_teste$Y) / NROW(dta_teste), 2)


## matriz de confusão via caret
# predito + referência caret
tab_teste = confusionMatrix(data = df_OvR_teste$Y_hat_OvR_teste, reference = df_OvR_teste$Y)
tab_teste
# salvando em .txt
sink(file = '_out/output/tabConfMatrix_OvR (test).txt')
cat('\n')
print(tab_teste)
sink()


## incluindo as odds 
## -- (para ponderar o classificador)
# total de jogos
n = NROW(df_OvR_teste)
n_L = length(which(dta_teste$Y == 'L'))
n_D = length(which(dta_teste$Y == 'D'))
n_W = length(which(dta_teste$Y == 'W'))
n
n_L + n_D + n_W
# acertos
games_won = 1*(df_OvR_teste$Y == df_OvR_teste$Y_hat_OvR)
cash_L = sum(games_won[which(dta_teste$Y == 'L')]*dta_teste$Odds_L[which(dta_teste$Y == 'L')])
cash_D = sum(games_won[which(dta_teste$Y == 'D')]*dta_teste$Odds_D[which(dta_teste$Y == 'D')])
cash_W = sum(games_won[which(dta_teste$Y == 'W')]*dta_teste$Odds_W[which(dta_teste$Y == 'W')])
# receita
revenue = cash_L + cash_D + cash_W
revenue_L = cash_L
revenue_D = cash_D
revenue_W = cash_W
revenue
revenue_L + revenue_D + revenue_W
# lucro
profit = revenue - n
profit_L = cash_L - n_L
profit_D = cash_D - n_D
profit_W = cash_W - n_W
profit
profit_L + profit_D + profit_W

