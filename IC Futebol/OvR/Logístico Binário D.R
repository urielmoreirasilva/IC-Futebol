
# ===========================
# Código Futebol
# Modelo Logístico Binário: D 
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








# ===========================================================================
# ===========================================================================
# Estudo preditivo (banco completo)
# ===========================================================================
# ===========================================================================

# ##################################################### #
# Estimando Modelo Logístico Binário (dataset completo) #
# ##################################################### #

## Re-categorizando os resultados (binário) [banco duplicado]
dta_duplicado = dta_duplicado %>% mutate(Y_bin_d = case_when(
  Y == 'L' ~ 0,
  Y == 'D' ~ 1,
  Y == 'W' ~ 0
))
dta_duplicado$Y_bin_d = factor(dta_duplicado$Y_bin_d, levels = c('0', '1'))


## Re-categorizando os resultados (binário) [banco original]
dta = dta %>% mutate(Y_bin_d = case_when(
  Y == 'L' ~ 0,
  Y == 'D' ~ 1,
  Y == 'W' ~ 0
))
dta$Y_bin_d = factor(dta$Y_bin_d, levels = c('0', '1'))


## Estimando regressão logística
out_d = glm(Y_bin_d ~ 1 + factor(Team_A) + factor(Team_B) + factor(Home),
            family = binomial(link = logit),
            data = dta_duplicado)
summary(out_d)
# salvando em .txt
sink(file = '_out/output/tabSummaryLogit_d (full).txt')
cat('\n')
print(summary(out_d))
sink()




# ####################################### #
# Classificação com ponto de corte padrão #
# ####################################### #

## Previsões de probabilidade [corrigir]
pi.hat_d = predict(out_d, newdata = dta, type = 'response')
head(pi.hat_d)


## Classificação com cutoff padrão
cutoff = 0.5
pred.Y_bin_d = factor(ifelse(pi.hat_d > cutoff, '1', '0'), levels = c('0', '1'))


## Referência ajustada
Y_bin_d <- factor(dta$Y_bin_d, levels = c("0", "1"))
Y_bin_d = relevel(dta$Y_bin_d, ref = '0')


## Matriz de confusão base::table
table(pred.Y_bin_d, Y_bin_d)
table(pred.Y_bin_d, Y_bin_d) / NROW(dta)


## Matriz de confusão via caret
# predito + referência caret
pred.Y_bin_caret_d = relevel(pred.Y_bin_d, ref = '1')
Y_bin_caret_d = relevel(Y_bin_d, ref = '1')
tab_d = confusionMatrix(data = pred.Y_bin_caret_d, reference = Y_bin_caret_d)
tab_d
# salvando em .txt
sink(file = '_out/output/tabConfMatrixNonOptimal_d (full).txt')
cat('\n')
print(tab_d)
sink()


## Plot da matriz de confusão
p_d = ggplot(as.data.frame(1e2 * round(tab_d$table / NROW(dta), 4)), 
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
p_d
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figConfMatrixNonOptimal_d (full).png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## Curva ROC inicial
ROC_d = roc(Y_bin_d ~ pi.hat_d)
plot(ROC_d, print.auc = TRUE, main = "Curva ROC – Dataset Completo (Vitórias)")
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figROCCurve_d (full).png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# ###################################### #
# Classificação com ponto de corte ótimo #
# ###################################### #

## Cutoff ótimo via Youden
C = coords(ROC_d, x = 'best')
C


## Classificação com cutoff ótimo
cutoff = C$threshold
pred.Y_bin_d = factor(ifelse(pi.hat_d > cutoff, '1', '0'), levels = c('0', '1'))


## Nova matriz de confusão
# predito + referência caret
pred.Y_bin_caret_d = relevel(pred.Y_bin_d, ref = '1')
Y_bin_caret_d = relevel(Y_bin_d, ref = '1')
tab_d = confusionMatrix(data = pred.Y_bin_caret_d, reference = Y_bin_caret_d)
tab_d
# salvando em .txt
sink(file = '_out/output/tabConfMatrixOptimal_d (full).txt')
cat('\n')
print(tab_d)
sink()


## Plot da nova matriz de confusão
p_d = ggplot(as.data.frame(1e2 * round(tab_d$table / NROW(dta), 4)), 
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
p_d
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figConfMatrixOptimal_d (full).png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# ################### #
# Análise de Resíduos #
# ################### #

## Resíduos quantílicos
res_d = simulateResiduals(out_d, method = "PIT")
eps_d = res_d$scaledResiduals


## PIT plot (teórica: distribuição uniforme)
H = 10
hist(eps_d, breaks = H, probability = TRUE)
abline(h = 1, lty = 2, col = 'red')
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figResPIT_d (full).png',
          device = png,
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## QQ-plot (teórica: distribuição Normal)
q_eps_d = qnorm(eps_d)
q_eps_d = q_eps_d[which(q_eps_d != Inf)]
q_eps_d = q_eps_d[which(q_eps_d != -Inf)]
qqnorm(q_eps_d)
qqline(q_eps_d)
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figResQQPlot_d (full).png',
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
dta_duplicado_treino = dta_duplicado %>% filter(Week <= 34) # p/ estimação
dta_treino = dta %>% filter(Week <= 34) # p/ previsão
dta_teste  = dta %>% filter(Week > 34)
# checando split: bancos duplicados
NROW(dta_duplicado)
NROW(dta_duplicado_treino)
# checando split: bancos originais
NROW(dta)
NROW(dta_treino)
NROW(dta_teste)


## Estimando regressão logística
out_d_treino = glm(Y_bin_d ~ 1 + factor(Team_A) + factor(Team_B) + factor(Home),
                   family = binomial(link = logit),
                   data = dta_duplicado_treino)
summary(out_d_treino)
# salvando em .txt
sink(file = '_out/output/tabSummaryLogit_d (train).txt')
cat('\n')
print(summary(out_d_treino))
sink()




# ######################################################### #
# Classificação com ponto de corte padrão (banco de treino) #
# ######################################################### #

## Previsões de probabilidade
pi.hat_d_treino = predict(out_d, newdata = dta_treino, type = 'response')
head(pi.hat_d_treino)


## Classificação com cutoff padrão
cutoff = 0.5
pred.Y_bin_d_treino = factor(ifelse(pi.hat_d_treino > cutoff, '1', '0'), levels = c('0', '1'))


## Referência ajustada
Y_bin_d_treino <- factor(dta_treino$Y_bin_d, levels = c("0", "1"))
Y_bin_d_treino = relevel(dta_treino$Y_bin_d, ref = '0')


## Matriz de confusão base::table
table(pred.Y_bin_d_treino, Y_bin_d_treino)
table(pred.Y_bin_d_treino, Y_bin_d_treino) / NROW(dta_treino)


## Matriz de confusão via caret
# predito + referência caret
pred.Y_bin_caret_d_treino = relevel(pred.Y_bin_d_treino, ref = '1')
Y_bin_caret_d_treino = relevel(Y_bin_d_treino, ref = '1')
tab_d_treino = confusionMatrix(data = pred.Y_bin_caret_d_treino, reference = Y_bin_caret_d_treino)
tab_d_treino
# salvando em .txt
sink(file = '_out/output/tabConfMatrixNonOptimal_d (train).txt')
cat('\n')
print(tab_d_treino)
sink()


## Plot da matriz de confusão
p_d_treino = ggplot(as.data.frame(1e2 * round(tab_d_treino$table / NROW(dta_treino), 4)), 
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
p_d_treino
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figConfMatrixNonOptimal_d (train).png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## Curva ROC inicial
ROC_d_treino = roc(Y_bin_d_treino ~ pi.hat_d_treino)
plot(ROC_d_treino, print.auc = TRUE, main = "Curva ROC – Dataset Treino (Vitórias)")
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figROCCurve_d (train).png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# ##################################################### #
# Classificação com ponto de corte ótimo (banco treino) #
# ##################################################### #

## Cutoff ótimo via Youden
C_treino = coords(ROC_d_treino, x = 'best')
C_treino


## Classificação com cutoff ótimo
cutoff_treino = C_treino$threshold
pred.Y_bin_d_treino = factor(ifelse(pi.hat_d_treino > cutoff_treino, '1', '0'), levels = c('0', '1'))


## Nova matriz de confusão
# predito + referência caret
pred.Y_bin_caret_d_treino = relevel(pred.Y_bin_d_treino, ref = '1')
Y_bin_caret_d_treino = relevel(Y_bin_d_treino, ref = '1')
tab_d_treino = confusionMatrix(data = pred.Y_bin_caret_d_treino, reference = Y_bin_caret_d_treino)
tab_d_treino
# salvando em .txt
sink(file = '_out/output/tabConfMatrixOptimal_d (train).txt')
cat('\n')
print(tab_d_treino)
sink()


## Plot da nova matriz de confusão
p_d_treino = ggplot(as.data.frame(1e2 * round(tab_d_treino$table / NROW(dta_treino), 4)), 
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
p_d_treino
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figConfMatrixOptimal_d (train).png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# ##################################### #
# Análise de Resíduos (banco de treino) #
# ##################################### #

## Resíduos quantílicos
res_d_treino = simulateResiduals(out_d_treino, method = "PIT")
eps_d_treino = res_d_treino$scaledResiduals


## PIT plot (teórica: distribuição uniforme)
H = 10
hist(eps_d_treino, breaks = H, probability = TRUE)
abline(h = 1, lty = 2, col = 'red')
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figResPIT_d (train).png',
          device = png,
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## QQ-plot (teórica: distribuição Normal)
q_eps_d_treino = qnorm(eps_d_treino)
q_eps_d_treino = q_eps_d_treino[which(q_eps_d_treino != Inf)]
q_eps_d_treino = q_eps_d_treino[which(q_eps_d_treino != -Inf)]
qqnorm(q_eps_d_treino)
qqline(q_eps_d_treino)
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figResQQPlot_d (train).png',
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
pi.hat_d_teste = predict(out_d, newdata = dta_teste, type = 'response')
head(pi.hat_d_teste)


## Classificação com cutoff padrão
cutoff = 0.5
pred.Y_bin_d_teste = factor(ifelse(pi.hat_d_teste > cutoff, '1', '0'), levels = c('0', '1'))


## Referência ajustada
Y_bin_d_teste = factor(dta_teste$Y_bin_d, levels = c("0", "1"))
Y_bin_d_teste = relevel(dta_teste$Y_bin_d, ref = '0')


## Matriz de confusão base::table
table(pred.Y_bin_d_teste, Y_bin_d_teste)
table(pred.Y_bin_d_teste, Y_bin_d_teste) / NROW(dta_teste)


## Matriz de confusão via caret
# predito + referência caret
pred.Y_bin_caret_d_teste = relevel(pred.Y_bin_d_teste, ref = '1')
Y_bin_caret_d_teste = relevel(Y_bin_d_teste, ref = '1')
tab_d_teste = confusionMatrix(data = pred.Y_bin_caret_d_teste, reference = Y_bin_caret_d_teste)
tab_d_teste
# salvando em .txt
sink(file = '_out/output/tabConfMatrixNonOptimal_d (test).txt')
cat('\n')
print(tab_d_teste)
sink()


## Plot da matriz de confusão
p_d_teste = ggplot(as.data.frame(1e2 * round(tab_d_teste$table / NROW(dta_teste), 4)), 
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
p_d_teste
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figConfMatrixNonOptimal_d (test).png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# #################################################### #
# Classificação com ponto de corte ótimo (banco teste) #
# #################################################### #

## Cutoff ótimo via Youden
# NOTA: o cutoff ótimo vem do banco de treino!
# ---- não fazemos curva ROC no teste, então
# ---- o objeto "ROC_d_teste" não existe!
C_teste = coords(ROC_d_treino, x = 'best')
C_teste


## Classificação com cutoff ótimo
cutoff_teste = C_teste$threshold
pred.Y_bin_d_teste = factor(ifelse(pi.hat_d_teste > cutoff_teste, '1', '0'), levels = c('0', '1'))


## Nova matriz de confusão
# predito + referência caret
pred.Y_bin_caret_d_teste = relevel(pred.Y_bin_d_teste, ref = '1')
Y_bin_caret_d_teste = relevel(Y_bin_d_teste, ref = '1')
tab_d_teste = confusionMatrix(data = pred.Y_bin_caret_d_teste, reference = Y_bin_caret_d_teste)
tab_d_teste
# salvando em .txt
sink(file = '_out/output/tabConfMatrixOptimal_d (test).txt')
cat('\n')
print(tab_d_teste)
sink()


## Plot da nova matriz de confusão
p_d_teste = ggplot(as.data.frame(1e2 * round(tab_d_teste$table / NROW(dta_teste), 4)), 
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
p_d_teste
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figConfMatrixOptimal_d (test).png',
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
  out_d,
  out_d_treino,
  pi.hat_d,
  pi.hat_d_teste,
  pi.hat_d_treino,
  ## arquivo
  file = '_out/out_d.RData'
)

