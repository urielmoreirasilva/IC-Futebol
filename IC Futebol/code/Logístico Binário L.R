
# ===========================
# Código Futebol
# Modelo Logístico Binário: L 
# Essa versão: 01/09/2026      
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




# ################# #
# Duplicando a base #
# ################# #

## Renomeando variáveis
dta_l = dta %>% rename(
  Team_A = 'Home', Score_A = 'HG', 
  Team_B = 'Away', Score_B = 'AG',
  Week = 'rodata'
)


## Filtrando colunas
dta_l = dta_l %>% dplyr::select(Team_A, Team_B, 
                                Score_A, Score_B, 
                                Week)


## Criando base espelhada
dta_l.2 = dta_l
dta_l.2$Team_A = dta_l$Team_B
dta_l.2$Score_A = dta_l$Score_B
dta_l.2$Team_B = dta_l$Team_A
dta_l.2$Score_B = dta_l$Score_A


## Indicando quem jogou em casa
dta_l$Home = "Home"
dta_l.2$Home = "Away"


## Combinando as bases
df_l_completo = dta_l %>%
  bind_rows(dta_l.2) %>%
  arrange(Week)


## Visualizando os dados
View(df_l_completo)




# ################## #
# Análise descritiva #
# ################## #

## Criando variável de resultado
dta_l = dta_l %>%
  mutate(dg = Score_A - Score_B) %>%
  mutate(Y = case_when(
    dg < 0 ~ 'L',
    dg == 0 ~ 'D',
    dg > 0 ~ 'W'
  ))


## Convertendo para fator ordenado
dta_l$Y = factor(dta_l$Y, levels = c('L', 'D', 'W'))
contrasts(dta_l$Y)


## Frequência dos resultados
table(dta_l$Y)
round(1e2 * table(dta_l$Y) / NROW(dta_l), 2)
# salvando em .txt
sink(file = '_out/output/tabFreqResults_all.txt')
cat('\n')
print(round(1e2 * table(dta_l$Y) / NROW(dta_l), 2))
sink()


## Re-categorizando os resultados (binário)
dta_l = dta_l %>% mutate(Y_bin_l = case_when(
  Y == 'L' ~ 1,
  Y == 'D' ~ 0,
  Y == 'W' ~ 0
))
dta_l$Y_bin_l = factor(dta_l$Y_bin_l, levels = c('0', '1'))
# frequências
round(1e2 * table(dta_l$Y_bin_l) / NROW(dta_l), 2)
# salvando em .txt
sink(file = '_out/output/tabFreqResults_l.txt')
cat('\n')
print(round(1e2 * table(dta_l$Y_bin_l) / NROW(dta_l), 2))
sink()


## Gols marcados por Team_A
goals = df_l_completo %>% 
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
## ---- Elaborar um gráfico desse e comparar com fator ataque e defesa estimados [!]
diff_goals = df_l_completo %>%
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
ggplot(df_l_completo, aes(x = Score_A)) +
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
ggplot(dta_l, aes(x = Score_A)) +
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
## ---- Sobrepor essas figuras? [!]
ggplot(dta_l, aes(x = Score_B)) +
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








# ===========================================================================
# ===========================================================================
# Estudo preditivo (banco completo)
# ===========================================================================
# ===========================================================================

# ##################################################### #
# Estimando Modelo Logístico Binário (dataset completo) #
# ##################################################### #

## Criando variável de resultado
df_l_completo = df_l_completo %>%
  mutate(dg = Score_A - Score_B) %>%
  mutate(Y = case_when(
    dg < 0 ~ 'L',
    dg == 0 ~ 'D',
    dg > 0 ~ 'W'
  ))

## Convertendo para fator ordenado
df_l_completo$Y = factor(df_l_completo$Y, levels = c('L', 'D', 'W'))
contrasts(df_l_completo$Y)

## Re-categorizando os resultados (binário)
df_l_completo = df_l_completo %>% mutate(Y_bin_l = case_when(
  Y == 'L' ~ 1,
  Y == 'D' ~ 0,
  Y == 'W' ~ 0
))
df_l_completo$Y_bin_l = factor(df_l_completo$Y_bin_l, levels = c('0', '1'))

## Estimando regressão logística
out_l = glm(Y_bin_l ~ 1 + factor(Team_A) + factor(Team_B) + factor(Home),
            family = binomial(link = logit),
            data = df_l_completo)
summary(out_l)
# salvando em .txt
sink(file = '_out/output/tabSummaryLogit_l (full).txt')
cat('\n')
print(summary(out_l))
sink()




# ####################################### #
# Classificação com ponto de corte padrão #
# ####################################### #

## Previsões de probabilidade
pi.hat_l = predict(out_l, newdata = dta_l, type = 'response')
head(pi.hat_l)


## Classificação com cutoff padrão
cutoff = 0.5
pred.Y_bin_l = factor(ifelse(pi.hat_l > cutoff, '1', '0'), levels = c('0', '1'))


## Referência ajustada
Y_bin_l = factor(dta_l$Y_bin_l, levels = c("0", "1"))
Y_bin_l = relevel(dta_l$Y_bin_l, ref = '0')


## Matriz de confusão base::table
table(pred.Y_bin_l, Y_bin_l)
table(pred.Y_bin_l, Y_bin_l) / NROW(dta_l)


## Matriz de confusão via caret
# predito + referência caret
pred.Y_bin_caret_l = relevel(pred.Y_bin_l, ref = '1')
Y_bin_caret_l = relevel(Y_bin_l, ref = '1')
tab_l = confusionMatrix(data = pred.Y_bin_caret_l, reference = Y_bin_caret_l)
tab_l
# salvando em .txt
sink(file = '_out/output/tabConfMatrixNonOptimal_l (full).txt')
cat('\n')
print(tab_l)
sink()


## Plot da matriz de confusão
## ---- Buscar outra maneira de representar a matriz de confusão (sklearn?) [!]
p_l = ggplot(as.data.frame(1e2 * round(tab_l$table / NROW(dta_l), 4)), 
             aes(x = Prediction, y = Freq, fill = Reference)) + 
  geom_bar(stat = "identity") +
  xlab('\n Predito') +
  ylab('Frequência (%) \n') +
  scale_fill_manual('Referência',
                    values = c('1' = 'blue2', '0' = 'red2')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(n.breaks = 6) +
  ggtitle('Matriz de Confusão')
p_l
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figConfMatrixNonOptimal_l (full).png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## Curva ROC inicial
ROC_l = roc(Y_bin_l ~ pi.hat_l)
plot(ROC_l, print.auc = TRUE, main = "Curva ROC – Dataset Completo (Vitórias)")
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figROCCurve_l (full).png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# ###################################### #
# Classificação com ponto de corte ótimo #
# ###################################### #

## Cutoff ótimo via Youden
C = coords(ROC_l, x = 'best')
C


## Classificação com cutoff ótimo
cutoff = C$threshold
pred.Y_bin_l = factor(ifelse(pi.hat_l > cutoff, '1', '0'), levels = c('0', '1'))


## Nova matriz de confusão
# predito + referência caret
pred.Y_bin_caret_l = relevel(pred.Y_bin_l, ref = '1')
Y_bin_caret_l = relevel(Y_bin_l, ref = '1')
tab_l = confusionMatrix(data = pred.Y_bin_caret_l, reference = Y_bin_caret_l)
tab_l
# salvando em .txt
sink(file = '_out/output/tabConfMatrixOptimal_l (full).txt')
cat('\n')
print(tab_l)
sink()


## Plot da nova matriz de confusão
p_l = ggplot(as.data.frame(1e2 * round(tab_l$table / NROW(dta_l), 4)), 
             aes(x = Prediction, y = Freq, fill = Reference)) + 
  geom_bar(stat = "identity") +
  xlab('\n Predito') +
  ylab('Frequência (%) \n') +
  scale_fill_manual('Referência',
                    values = c('1' = 'blue2', '0' = 'red2')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(n.breaks = 6) +
  ggtitle('Matriz de Confusão')
p_l
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figConfMatrixOptimal_l (full).png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# ################### #
# Análise de Resíduos #
# ################### #

## Resíduos quantílicos
res_l = simulateResiduals(out_l, method = "PIT")
eps_l = res_l$scaledResiduals


## PIT plot (teórica: distribuição uniforme)
H = 10
hist(eps_l, breaks = H, probability = TRUE)
abline(h = 1, lty = 2, col = 'red')
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figResPIT_l (full).png',
          device = png,
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## QQ-plot (teórica: distribuição Normal)
q_eps_l = qnorm(eps_l)
q_eps_l = q_eps_l[which(q_eps_l != Inf)]
q_eps_l = q_eps_l[which(q_eps_l != -Inf)]
qqnorm(q_eps_l)
qqline(q_eps_l)
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figResQQPlot_l (full).png',
          device = png,
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')








# ===========================================================================
# ===========================================================================
# Estudo preditivo (banco treino)
# ===========================================================================
# ===========================================================================

# #################################################### #
# Estimando Modelo Logístico Binário (banco de treino) #
# #################################################### #

## treino/teste split
df_l_treino = df_l_completo %>% filter(Week <= 34) # p/ estimação
dta_l_treino = dta_l %>% filter(Week <= 34) # p/ previsão
dta_l_teste  = dta_l %>% filter(Week > 34)
# checando split: bancos p/ estimação
NROW(df_l_completo)
NROW(df_l_treino)
# checando split: bancos p/ previsão
NROW(dta_l)
NROW(dta_l_treino)
NROW(dta_l_teste)


## Estimando regressão logística
out_l_treino = glm(Y_bin_l ~ 1 + factor(Team_A) + factor(Team_B) + factor(Home),
                   family = binomial(link = logit),
                   data = df_l_treino)
summary(out_l_treino)
# salvando em .txt
sink(file = '_out/output/tabSummaryLogit_l (train).txt')
cat('\n')
print(summary(out_l_treino))
sink()




# ######################################################### #
# Classificação com ponto de corte padrão (banco de treino) #
# ######################################################### #

## Previsões de probabilidade
pi.hat_l_treino = predict(out_l, newdata = dta_l_treino, type = 'response')
head(pi.hat_l_treino)


## Classificação com cutoff padrão
cutoff = 0.5
pred.Y_bin_l_treino = factor(ifelse(pi.hat_l_treino > cutoff, '1', '0'), levels = c('0', '1'))


## Referência ajustada
Y_bin_l_treino <- factor(dta_l_treino$Y_bin_l, levels = c("0", "1"))
Y_bin_l_treino = relevel(dta_l_treino$Y_bin_l, ref = '0')


## Matriz de confusão base::table
table(pred.Y_bin_l_treino, Y_bin_l_treino)
table(pred.Y_bin_l_treino, Y_bin_l_treino) / NROW(dta_l_treino)


## Matriz de confusão via caret
# predito + referência caret
pred.Y_bin_caret_l_treino = relevel(pred.Y_bin_l_treino, ref = '1')
Y_bin_caret_l_treino = relevel(Y_bin_l_treino, ref = '1')
tab_l_treino = confusionMatrix(data = pred.Y_bin_caret_l_treino, reference = Y_bin_caret_l_treino)
tab_l_treino
# salvando em .txt
sink(file = '_out/output/tabConfMatrixNonOptimal_l (train).txt')
cat('\n')
print(tab_l_treino)
sink()


## Plot da matriz de confusão
## ---- Buscar outra maneira de representar a matriz de confusão (sklearn?) [!]
p_l_treino = ggplot(as.data.frame(1e2 * round(tab_l_treino$table / NROW(dta_l_treino), 4)), 
                    aes(x = Prediction, y = Freq, fill = Reference)) + 
  geom_bar(stat = "identity") +
  xlab('\n Predito') +
  ylab('Frequência (%) \n') +
  scale_fill_manual('Referência',
                    values = c('1' = 'blue2', '0' = 'red2')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(n.breaks = 6) +
  ggtitle('Matriz de Confusão')
p_l_treino
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figConfMatrixNonOptimal_l (train).png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## Curva ROC inicial
ROC_l_treino = roc(Y_bin_l_treino ~ pi.hat_l_treino)
plot(ROC_l_treino, print.auc = TRUE, main = "Curva ROC – Dataset Treino (Vitórias)")
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figROCCurve_l (train).png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# ##################################################### #
# Classificação com ponto de corte ótimo (banco treino) #
# ##################################################### #

## Cutoff ótimo via Youden
C_treino = coords(ROC_l_treino, x = 'best')
C_treino


## Classificação com cutoff ótimo
cutoff_treino = C_treino$threshold
pred.Y_bin_l_treino = factor(ifelse(pi.hat_l_treino > cutoff_treino, '1', '0'), levels = c('0', '1'))


## Nova matriz de confusão
# predito + referência caret
pred.Y_bin_caret_l_treino = relevel(pred.Y_bin_l_treino, ref = '1')
Y_bin_caret_l_treino = relevel(Y_bin_l_treino, ref = '1')
tab_l_treino = confusionMatrix(data = pred.Y_bin_caret_l_treino, reference = Y_bin_caret_l_treino)
tab_l_treino
# salvando em .txt
sink(file = '_out/output/tabConfMatrixOptimal_l (train).txt')
cat('\n')
print(tab_l_treino)
sink()


## Plot da nova matriz de confusão
p_l_treino = ggplot(as.data.frame(1e2 * round(tab_l_treino$table / NROW(dta_l_treino), 4)), 
                    aes(x = Prediction, y = Freq, fill = Reference)) + 
  geom_bar(stat = "identity") +
  xlab('\n Predito') +
  ylab('Frequência (%) \n') +
  scale_fill_manual('Referência',
                    values = c('1' = 'blue2', '0' = 'red2')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(n.breaks = 6) +
  ggtitle('Matriz de Confusão')
p_l_treino
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figConfMatrixOptimal_l (train).png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# ##################################### #
# Análise de Resíduos (banco de treino) #
# ##################################### #

## Resíduos quantílicos
res_l_treino = simulateResiduals(out_l_treino, method = "PIT")
eps_l_treino = res_l_treino$scaledResiduals


## PIT plot (teórica: distribuição uniforme)
H = 10
hist(eps_l_treino, breaks = H, probability = TRUE)
abline(h = 1, lty = 2, col = 'red')
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figResPIT_l (train).png',
          device = png,
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## QQ-plot (teórica: distribuição Normal)
q_eps_l_treino = qnorm(eps_l_treino)
q_eps_l_treino = q_eps_l_treino[which(q_eps_l_treino != Inf)]
q_eps_l_treino = q_eps_l_treino[which(q_eps_l_treino != -Inf)]
qqnorm(q_eps_l_treino)
qqline(q_eps_l_treino)
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figResQQPlot_l (train).png',
          device = png,
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')






# ===========================================================================
# ===========================================================================
# Estudo preditivo (banco teste)
# ===========================================================================
# ===========================================================================

# ######################################################### #
# Classificação com ponto de corte padrão (banco de teste) #
# ######################################################### #

## Previsões de probabilidade
pi.hat_l_teste = predict(out_l, newdata = dta_l_teste, type = 'response')
head(pi.hat_l_teste)


## Classificação com cutoff padrão
cutoff = 0.5
pred.Y_bin_l_teste = factor(ifelse(pi.hat_l_teste > cutoff, '1', '0'), levels = c('0', '1'))


## Referência ajustada
Y_bin_l_teste <- factor(dta_l_teste$Y_bin_l, levels = c("0", "1"))
Y_bin_l_teste = relevel(dta_l_teste$Y_bin_l, ref = '0')


## Matriz de confusão base::table
table(pred.Y_bin_l_teste, Y_bin_l_teste)
table(pred.Y_bin_l_teste, Y_bin_l_teste) / NROW(dta_l_teste)


## Matriz de confusão via caret
# predito + referência caret
pred.Y_bin_caret_l_teste = relevel(pred.Y_bin_l_teste, ref = '1')
Y_bin_caret_l_teste = relevel(Y_bin_l_teste, ref = '1')
tab_l_teste = confusionMatrix(data = pred.Y_bin_caret_l_teste, reference = Y_bin_caret_l_teste)
tab_l_teste
# salvando em .txt
sink(file = '_out/output/tabConfMatrixNonOptimal_l (test).txt')
cat('\n')
print(tab_l_teste)
sink()


## Plot da matriz de confusão
## ---- Buscar outra maneira de representar a matriz de confusão (sklearn?) [!]
p_l_teste = ggplot(as.data.frame(1e2 * round(tab_l_teste$table / NROW(dta_l_teste), 4)), 
                   aes(x = Prediction, y = Freq, fill = Reference)) + 
  geom_bar(stat = "identity") +
  xlab('\n Predito') +
  ylab('Frequência (%) \n') +
  scale_fill_manual('Referência',
                    values = c('1' = 'blue2', '0' = 'red2')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(n.breaks = 6) +
  ggtitle('Matriz de Confusão')
p_l_teste
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figConfMatrixNonOptimal_l (test).png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# #################################################### #
# Classificação com ponto de corte ótimo (banco teste) #
# #################################################### #

## Cutoff ótimo via Youden
# NOTA: o cutoff ótimo vem do banco de treino!
# ---- não fazemos curva ROC no teste, então
# ---- o objeto "ROC_l_teste" não existe!
C_teste = coords(ROC_l_treino, x = 'best')
C_teste


## Classificação com cutoff ótimo
cutoff_teste = C_teste$threshold
pred.Y_bin_l_teste = factor(ifelse(pi.hat_l_teste > cutoff_teste, '1', '0'), levels = c('0', '1'))


## Nova matriz de confusão
# predito + referência caret
pred.Y_bin_caret_l_teste = relevel(pred.Y_bin_l_teste, ref = '1')
Y_bin_caret_l_teste = relevel(Y_bin_l_teste, ref = '1')
tab_l_teste = confusionMatrix(data = pred.Y_bin_caret_l_teste, reference = Y_bin_caret_l_teste)
tab_l_teste
# salvando em .txt
sink(file = '_out/output/tabConfMatrixOptimal_l (test).txt')
cat('\n')
print(tab_l_teste)
sink()


## Plot da nova matriz de confusão
p_l_teste = ggplot(as.data.frame(1e2 * round(tab_l_teste$table / NROW(dta_l_teste), 4)), 
                   aes(x = Prediction, y = Freq, fill = Reference)) + 
  geom_bar(stat = "identity") +
  xlab('\n Predito') +
  ylab('Frequência (%) \n') +
  scale_fill_manual('Referência',
                    values = c('1' = 'blue2', '0' = 'red2')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(n.breaks = 6) +
  ggtitle('Matriz de Confusão')
p_l_teste
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figConfMatrixOptimal_l (test).png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')








# ===========================================================================
# ===========================================================================
# Limpeza e encerramento
# ===========================================================================
# ===========================================================================

## exportando modelos e datasets
save(
  ## objetos
  dta_l,
  out_l,
  out_l_treino,
  dta_l_treino,
  dta_l_teste,
  df_l_completo,
  pi.hat_l,
  pi.hat_l_teste,
  pi.hat_l_treino,
  ## arquivo
  file = '_out/out_l.RData'
)

