
# =======================
# Código Futebol
# Classificador OvR 
# Essa versão: 20/08/2026      
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
prob = table(dta_w$Y) # pode ser também em dta_d ou dta_l
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
df_OvR$Y = dta_w$Y # pode ser também em dta_d ou dta_l
df_OvR$Y_hat_OvR = Y.hat_OvR


## matriz de confusão (manual)
tab = table(Y = df_OvR$Y, Y.hat = df_OvR$Y_hat_OvR)
tab
round(1e2*sum(diag(tab/sum(tab))), 2)


## frequências dos resultados
table(dta_w$Y)
round(1e2 * table(dta_w$Y) / NROW(dta_w), 2)


## matriz de confusão via caret
# predito + referência caret
tab = confusionMatrix(data = df_OvR$Y_hat_OvR, reference = df_OvR$Y)
tab
# salvando em .txt
sink(file = '_out/output/tabConfMatrix_OvR (full).txt')
cat('\n')
print(tab)
sink()








# ===========================================================================
# ===========================================================================
# Classificador OvR (banco treino)
# ===========================================================================
# ===========================================================================

# ## classificador OvR: maioria simples
# df_OvR_treino = data.frame(
#   prob_hat_w = pi.hat_w_treino,
#   prob_hat_d = pi.hat_d_treino,
#   prob_hat_l = pi.hat_l_treino
# )


## classificador OvR: max. acurácia balanceada
prob = table(dta_w_treino$Y) # pode ser também em dta_d ou dta_l
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
df_OvR_treino$Y = dta_w_treino$Y # pode ser também em dta_d ou dta_l
df_OvR_treino$Y_hat_OvR_treino = Y.hat_OvR_treino


## matriz de confusão (manual)
tab_treino = table(Y = df_OvR_treino$Y, Y.hat = df_OvR_treino$Y_hat_OvR_treino)
tab_treino
round(1e2*sum(diag(tab_treino/sum(tab_treino))), 2)


## frequências dos resultados
table(dta_w_treino$Y)
round(1e2 * table(dta_w_treino$Y) / NROW(dta_w_treino), 2)


## matriz de confusão via caret
# predito + referência caret
tab_treino = confusionMatrix(data = df_OvR_treino$Y_hat_OvR_treino, reference = df_OvR_treino$Y)
tab_treino
# salvando em .txt
sink(file = '_out/output/tabConfMatrix_OvR (train).txt')
cat('\n')
print(tab_treino)
sink()








# ===========================================================================
# ===========================================================================
# Classificador OvR (banco teste)
# ===========================================================================
# ===========================================================================

# ## classificador OvR: maioria simples
# df_OvR_teste = data.frame(
#   prob_hat_w = pi.hat_w_teste,
#   prob_hat_d = pi.hat_d_teste,
#   prob_hat_l = pi.hat_l_teste
# )


## classificador OvR: max. acurácia balanceada
prob = table(dta_w_treino$Y) # pode ser também em dta_d ou dta_l
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
df_OvR_teste$Y = dta_w_teste$Y # pode ser também em dta_d ou dta_l
df_OvR_teste$Y_hat_OvR_teste = Y.hat_OvR_teste


## matriz de confusão (manual)
tab_teste = table(Y = df_OvR_teste$Y, Y.hat = df_OvR_teste$Y_hat_OvR_teste)
tab_teste
round(1e2*sum(diag(tab_teste/sum(tab_teste))), 2)


## frequências dos resultados
table(dta_w_teste$Y)
round(1e2 * table(dta_w_teste$Y) / NROW(dta_w_teste), 2)


## matriz de confusão via caret
# predito + referência caret
tab_teste = confusionMatrix(data = df_OvR_teste$Y_hat_OvR_teste, reference = df_OvR_teste$Y)
tab_teste
# salvando em .txt
sink(file = '_out/output/tabConfMatrix_OvR (test).txt')
cat('\n')
print(tab_teste)
sink()









##
sum(df_OvR_teste$Y_hat_OvR == df_OvR_teste$Y)/40
dta = dta %>% arrange(ID) 
# dta[341:380, 'rodata']
df_OvR_teste$Y_hat_OvR == df_OvR_teste$Y
df_OvR_teste$Y
