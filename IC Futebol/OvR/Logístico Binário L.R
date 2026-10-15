
# ===========================
# Código Futebol
# Modelo Logístico Binário: L 
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
dta_duplicado = dta_duplicado %>% mutate(Y_bin_l = case_when(
  Y == 'L' ~ 1,
  Y == 'D' ~ 0,
  Y == 'W' ~ 0
))
dta_duplicado$Y_bin_l = factor(dta_duplicado$Y_bin_l, levels = c('0', '1'))


## Re-categorizando os resultados (binário) [banco original]
dta = dta %>% mutate(Y_bin_l = case_when(
  Y == 'L' ~ 1,
  Y == 'D' ~ 0,
  Y == 'W' ~ 0
))
dta$Y_bin_l = factor(dta$Y_bin_l, levels = c('0', '1'))


## Estimando regressão logística
out_l = glm(Y_bin_l ~ 1 + factor(Team_A) + factor(Team_B) + factor(Home),
            family = binomial(link = logit),
            data = dta_duplicado)
summary(out_l)
# salvando em .txt
sink(file = '_out/output/tabSummaryLogit_l (full).txt')
cat('\n')
print(summary(out_l))
sink()




# ####################################### #
# Classificação com ponto de corte padrão #
# ####################################### #

## Previsões de probabilidade [corrigir]
pi.hat_l = predict(out_l, newdata = dta, type = 'response')
head(pi.hat_l)


## Classificação com cutoff padrão
cutoff = 0.5
pred.Y_bin_l = factor(ifelse(pi.hat_l > cutoff, '1', '0'), levels = c('0', '1'))


## Referência ajustada
Y_bin_l <- factor(dta$Y_bin_l, levels = c("0", "1"))
Y_bin_l = relevel(dta$Y_bin_l, ref = '0')


## Matriz de confusão base::table
table(pred.Y_bin_l, Y_bin_l)
table(pred.Y_bin_l, Y_bin_l) / NROW(dta)


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
p_l = ggplot(as.data.frame(1e2 * round(tab_l$table / NROW(dta), 4)), 
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
p_l = ggplot(as.data.frame(1e2 * round(tab_l$table / NROW(dta), 4)), 
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
out_l_treino = glm(Y_bin_l ~ 1 + factor(Team_A) + factor(Team_B) + factor(Home),
                   family = binomial(link = logit),
                   data = dta_duplicado_treino)
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
pi.hat_l_treino = predict(out_l, newdata = dta_treino, type = 'response')
head(pi.hat_l_treino)


## Classificação com cutoff padrão
cutoff = 0.5
pred.Y_bin_l_treino = factor(ifelse(pi.hat_l_treino > cutoff, '1', '0'), levels = c('0', '1'))


## Referência ajustada
Y_bin_l_treino <- factor(dta_treino$Y_bin_l, levels = c("0", "1"))
Y_bin_l_treino = relevel(dta_treino$Y_bin_l, ref = '0')


## Matriz de confusão base::table
table(pred.Y_bin_l_treino, Y_bin_l_treino)
table(pred.Y_bin_l_treino, Y_bin_l_treino) / NROW(dta_treino)


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
p_l_treino = ggplot(as.data.frame(1e2 * round(tab_l_treino$table / NROW(dta_treino), 4)), 
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
p_l_treino = ggplot(as.data.frame(1e2 * round(tab_l_treino$table / NROW(dta_treino), 4)), 
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
pi.hat_l_teste = predict(out_l, newdata = dta_teste, type = 'response')
head(pi.hat_l_teste)


## Classificação com cutoff padrão
cutoff = 0.5
pred.Y_bin_l_teste = factor(ifelse(pi.hat_l_teste > cutoff, '1', '0'), levels = c('0', '1'))


## Referência ajustada
Y_bin_l_teste = factor(dta_teste$Y_bin_l, levels = c("0", "1"))
Y_bin_l_teste = relevel(dta_teste$Y_bin_l, ref = '0')


## Matriz de confusão base::table
table(pred.Y_bin_l_teste, Y_bin_l_teste)
table(pred.Y_bin_l_teste, Y_bin_l_teste) / NROW(dta_teste)


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
p_l_teste = ggplot(as.data.frame(1e2 * round(tab_l_teste$table / NROW(dta_teste), 4)), 
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
p_l_teste = ggplot(as.data.frame(1e2 * round(tab_l_teste$table / NROW(dta_teste), 4)), 
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
  out_l,
  out_l_treino,
  pi.hat_l,
  pi.hat_l_teste,
  pi.hat_l_treino,
  ## arquivo
  file = '_out/out_l.RData'
)

