
# ===========================
# Código Futebol
# Estatísticas Descritivas 
# Essa versão: 15/10/2026      
# ===========================


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
load('_out/data.RData') # vitórias


## filtrando por 2025
dta_duplicado = dta_duplicado %>% subset(Season == 2025)
dta = dta %>% subset(Season == 2025)
table(dta_duplicado$Home)
table(dta$Home)
table(dta_duplicado$Team_A)
table(dta_duplicado$Team_B)
table(dta$Team_A)
table(dta$Team_B)




# ################## #
# Análise descritiva #
# ################## #

## Frequência dos resultados
table(dta$Y)
round(1e2 * table(dta$Y) / NROW(dta), 2)
# salvando em .txt
sink(file = '_out/output/tabFreqResults_all.txt')
cat('\n')
print(round(1e2 * table(dta$Y) / NROW(dta), 2))
sink()


## Gols marcados por Team_A
goals = dta_duplicado %>% 
  group_by(Team_A) %>%
  summarise(Media_Gols = mean(Score_A),
            Total_Jogos = n()) %>%
  arrange(desc(Media_Gols))
goals
# salvando em .txt
sink(file = '_out/output/tabGoals_all.txt')
cat('\n')
print(goals)
sink()


## Diferença média de gols por Team_A
diff_goals = dta_duplicado %>%
  mutate(dg = Score_A - Score_B) %>%
  group_by(Team_A) %>%
  summarise(Dif_Gol_Med = mean(dg)) %>%
  arrange(desc(Dif_Gol_Med))
diff_goals
# salvando em .txt
sink(file = '_out/output/tabDiffGoals_all.txt')
cat('\n')
print(diff_goals)
sink()


## Distribuição de gols marcados (ambos os times)
ggplot(dta_duplicado, aes(x = Score_A)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  theme_bw() +
  labs(title = "Distribuição dos Gols Marcados",
       x = "Gols Marcados (Ambos Times)",
       y = "Frequência")
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figGoalsBoth_all.png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## Distribuição de gols marcados (mandante)
ggplot(dta_duplicado, aes(x = Score_A)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  theme_bw() +
  labs(title = "Distribuição dos Gols Marcados",
       x = "Gols Marcados (Time A)",
       y = "Frequência")
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figGoalsTeamA_all.png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## Distribuição de gols marcados (visitante)
ggplot(dta_duplicado, aes(x = Score_B)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  theme_bw() +
  labs(title = "Distribuição dos Gols Marcados",
       x = "Gols Marcados (Time B)",
       y = "Frequência")
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figGoalsTeamB_all.png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')

