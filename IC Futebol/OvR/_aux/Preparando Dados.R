
# ==================================
# Código Futebol
# Preparando os Dados para Modelagem 
# Essa versão: 15/10/2026      
# ==================================


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
library(haven)
library(openxlsx)
library(performance)   
library(pROC)          
library(see)           
library(tidyverse)
library(writexl)


## Diretório de trabalho
# setwd('C:/Users/Uriel/Desktop/IC Futebol/OvR')
setwd('/home/uriel/Desktop/IC Futebol/OvR')


## Carregando dados: Brasileirão Série A, 2003 – 2025
df_jogos = read.xlsx('_dta/BRA.xlsx')
glimpse(df_jogos)


# ## filtrando por 2025
# df_jogos = df_jogos %>% subset(Season == 2025)
# table(df_jogos$Home)


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
# # filtrando por 2025
# df_rodadas = df_rodadas %>% subset(Season == 2025)
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




# ################# #
# Duplicando a base #
# ################# #

## Renomeando variáveis
dta = dta %>% rename(
  Team_A = 'Home', Score_A = 'HG', 
  Team_B = 'Away', Score_B = 'AG',
  Week = 'rodata',
  Odds_L = 'MaxCA', Odds_D = 'MaxCD', Odds_W = 'MaxCH'
)


## Filtrando colunas
dta = dta %>% dplyr::select(Team_A, Team_B, 
                            Score_A, Score_B, 
                            Week,
                            Season,
                            Odds_L, Odds_D, Odds_W)


## Criando base espelhada
dta.2 = dta
dta.2$Team_A = dta$Team_B
dta.2$Score_A = dta$Score_B
dta.2$Team_B = dta$Team_A
dta.2$Score_B = dta$Score_A


## Indicando quem jogou em casa
dta$Home = "Home"
dta.2$Home = "Away"


## Combinando as bases
dta_duplicado = dta %>%
  bind_rows(dta.2) %>%
  arrange(Week)


## Criando variável de resultado (base original)
dta = dta %>%
  mutate(dg = Score_A - Score_B) %>%
  mutate(Y = case_when(
    dg < 0 ~ 'L',
    dg == 0 ~ 'D',
    dg > 0 ~ 'W'
  ))


## Convertendo para fator ordenado (base original)
dta$Y = factor(dta$Y, levels = c('L', 'D', 'W'))
contrasts(dta$Y)


## Criando variável de resultado (base duplicada)
dta_duplicado = dta_duplicado %>%
  mutate(dg = Score_A - Score_B) %>%
  mutate(Y = case_when(
    dg < 0 ~ 'L',
    dg == 0 ~ 'D',
    dg > 0 ~ 'W'
  ))


## Convertendo para fator ordenado
dta_duplicado$Y = factor(dta_duplicado$Y, levels = c('L', 'D', 'W'))
contrasts(dta_duplicado$Y)


## Visualizando os dados
glimpse(dta_duplicado)
glimpse(dta)








# ===========================================================================
# ===========================================================================
# Limpeza e encerramento
# ===========================================================================
# ===========================================================================

## exportando modelos e datasets
save(
  ## objetos
  dta,
  dta_duplicado,
  ## arquivo
  file = '_out/data.RData'
)


