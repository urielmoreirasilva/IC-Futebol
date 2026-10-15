
# ===========================
# Código Futebol
# Modelo Logístico Binário: W 
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
dta_duplicado = dta_duplicado %>% mutate(Y_bin_w = case_when(
  Y == 'L' ~ 0,
  Y == 'D' ~ 0,
  Y == 'W' ~ 1
))
dta_duplicado$Y_bin_w = factor(dta_duplicado$Y_bin_w, levels = c('0', '1'))


## Re-categorizando os resultados (binário) [banco original]
dta = dta %>% mutate(Y_bin_w = case_when(
  Y == 'L' ~ 0,
  Y == 'D' ~ 0,
  Y == 'W' ~ 1
))
dta$Y_bin_w = factor(dta$Y_bin_w, levels = c('0', '1'))


## Estimando regressão logística
out_w = glm(Y_bin_w ~ 1 + factor(Team_A) + factor(Team_B) + factor(Home),
            family = binomial(link = logit),
            data = dta_duplicado)
summary(out_w)
# salvando em .txt
sink(file = '_out/output/tabSummaryLogit_w (full).txt')
cat('\n')
print(summary(out_w))
sink()




# ####################################### #
# Classificação com ponto de corte padrão #
# ####################################### #

## Previsões de probabilidade [corrigir]
pi.hat_w = predict(out_w, newdata = dta, type = 'response')
head(pi.hat_w)


## Classificação com cutoff padrão
cutoff = 0.5
pred.Y_bin_w = factor(ifelse(pi.hat_w > cutoff, '1', '0'), levels = c('0', '1'))


## Referência ajustada
Y_bin_w <- factor(dta$Y_bin_w, levels = c("0", "1"))
Y_bin_w = relevel(dta$Y_bin_w, ref = '0')


## Matriz de confusão base::table
table(pred.Y_bin_w, Y_bin_w)
table(pred.Y_bin_w, Y_bin_w) / NROW(dta)


## Matriz de confusão via caret
# predito + referência caret
pred.Y_bin_caret_w = relevel(pred.Y_bin_w, ref = '1')
Y_bin_caret_w = relevel(Y_bin_w, ref = '1')
tab_w = confusionMatrix(data = pred.Y_bin_caret_w, reference = Y_bin_caret_w)
tab_w
# salvando em .txt
sink(file = '_out/output/tabConfMatrixNonOptimal_w (full).txt')
cat('\n')
print(tab_w)
sink()


## Plot da matriz de confusão
p_w = ggplot(as.data.frame(1e2 * round(tab_w$table / NROW(dta), 4)), 
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
p_w
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figConfMatrixNonOptimal_w (full).png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## Curva ROC inicial
ROC_w = roc(Y_bin_w ~ pi.hat_w)
plot(ROC_w, print.auc = TRUE, main = "Curva ROC – Dataset Completo (Vitórias)")
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figROCCurve_w (full).png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# ###################################### #
# Classificação com ponto de corte ótimo #
# ###################################### #

## Cutoff ótimo via Youden
C = coords(ROC_w, x = 'best')
C


## Classificação com cutoff ótimo
cutoff = C$threshold
pred.Y_bin_w = factor(ifelse(pi.hat_w > cutoff, '1', '0'), levels = c('0', '1'))


## Nova matriz de confusão
# predito + referência caret
pred.Y_bin_caret_w = relevel(pred.Y_bin_w, ref = '1')
Y_bin_caret_w = relevel(Y_bin_w, ref = '1')
tab_w = confusionMatrix(data = pred.Y_bin_caret_w, reference = Y_bin_caret_w)
tab_w
# salvando em .txt
sink(file = '_out/output/tabConfMatrixOptimal_w (full).txt')
cat('\n')
print(tab_w)
sink()


## Plot da nova matriz de confusão
p_w = ggplot(as.data.frame(1e2 * round(tab_w$table / NROW(dta), 4)), 
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
p_w
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figConfMatrixOptimal_w (full).png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# ################### #
# Análise de Resíduos #
# ################### #

## Resíduos quantílicos
res_w = simulateResiduals(out_w, method = "PIT")
eps_w = res_w$scaledResiduals


## PIT plot (teórica: distribuição uniforme)
H = 10
hist(eps_w, breaks = H, probability = TRUE)
abline(h = 1, lty = 2, col = 'red')
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figResPIT_w (full).png',
          device = png,
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## QQ-plot (teórica: distribuição Normal)
q_eps_w = qnorm(eps_w)
q_eps_w = q_eps_w[which(q_eps_w != Inf)]
q_eps_w = q_eps_w[which(q_eps_w != -Inf)]
qqnorm(q_eps_w)
qqline(q_eps_w)
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figResQQPlot_w (full).png',
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
out_w_treino = glm(Y_bin_w ~ 1 + factor(Team_A) + factor(Team_B) + factor(Home),
                   family = binomial(link = logit),
                   data = dta_duplicado_treino)
summary(out_w_treino)
# salvando em .txt
sink(file = '_out/output/tabSummaryLogit_w (train).txt')
cat('\n')
print(summary(out_w_treino))
sink()




# ######################################################### #
# Classificação com ponto de corte padrão (banco de treino) #
# ######################################################### #

## Previsões de probabilidade
pi.hat_w_treino = predict(out_w, newdata = dta_treino, type = 'response')
head(pi.hat_w_treino)


## Classificação com cutoff padrão
cutoff = 0.5
pred.Y_bin_w_treino = factor(ifelse(pi.hat_w_treino > cutoff, '1', '0'), levels = c('0', '1'))


## Referência ajustada
Y_bin_w_treino <- factor(dta_treino$Y_bin_w, levels = c("0", "1"))
Y_bin_w_treino = relevel(dta_treino$Y_bin_w, ref = '0')


## Matriz de confusão base::table
table(pred.Y_bin_w_treino, Y_bin_w_treino)
table(pred.Y_bin_w_treino, Y_bin_w_treino) / NROW(dta_treino)


## Matriz de confusão via caret
# predito + referência caret
pred.Y_bin_caret_w_treino = relevel(pred.Y_bin_w_treino, ref = '1')
Y_bin_caret_w_treino = relevel(Y_bin_w_treino, ref = '1')
tab_w_treino = confusionMatrix(data = pred.Y_bin_caret_w_treino, reference = Y_bin_caret_w_treino)
tab_w_treino
# salvando em .txt
sink(file = '_out/output/tabConfMatrixNonOptimal_w (train).txt')
cat('\n')
print(tab_w_treino)
sink()


## Plot da matriz de confusão
p_w_treino = ggplot(as.data.frame(1e2 * round(tab_w_treino$table / NROW(dta_treino), 4)), 
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
p_w_treino
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figConfMatrixNonOptimal_w (train).png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## Curva ROC inicial
ROC_w_treino = roc(Y_bin_w_treino ~ pi.hat_w_treino)
plot(ROC_w_treino, print.auc = TRUE, main = "Curva ROC – Dataset Treino (Vitórias)")
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figROCCurve_w (train).png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# ##################################################### #
# Classificação com ponto de corte ótimo (banco treino) #
# ##################################################### #

## Cutoff ótimo via Youden
C_treino = coords(ROC_w_treino, x = 'best')
C_treino


## Classificação com cutoff ótimo
cutoff_treino = C_treino$threshold
pred.Y_bin_w_treino = factor(ifelse(pi.hat_w_treino > cutoff_treino, '1', '0'), levels = c('0', '1'))


## Nova matriz de confusão
# predito + referência caret
pred.Y_bin_caret_w_treino = relevel(pred.Y_bin_w_treino, ref = '1')
Y_bin_caret_w_treino = relevel(Y_bin_w_treino, ref = '1')
tab_w_treino = confusionMatrix(data = pred.Y_bin_caret_w_treino, reference = Y_bin_caret_w_treino)
tab_w_treino
# salvando em .txt
sink(file = '_out/output/tabConfMatrixOptimal_w (train).txt')
cat('\n')
print(tab_w_treino)
sink()


## Plot da nova matriz de confusão
p_w_treino = ggplot(as.data.frame(1e2 * round(tab_w_treino$table / NROW(dta_treino), 4)), 
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
p_w_treino
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figConfMatrixOptimal_w (train).png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# ##################################### #
# Análise de Resíduos (banco de treino) #
# ##################################### #

## Resíduos quantílicos
res_w_treino = simulateResiduals(out_w_treino, method = "PIT")
eps_w_treino = res_w_treino$scaledResiduals


## PIT plot (teórica: distribuição uniforme)
H = 10
hist(eps_w_treino, breaks = H, probability = TRUE)
abline(h = 1, lty = 2, col = 'red')
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figResPIT_w (train).png',
          device = png,
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')


## QQ-plot (teórica: distribuição Normal)
q_eps_w_treino = qnorm(eps_w_treino)
q_eps_w_treino = q_eps_w_treino[which(q_eps_w_treino != Inf)]
q_eps_w_treino = q_eps_w_treino[which(q_eps_w_treino != -Inf)]
qqnorm(q_eps_w_treino)
qqline(q_eps_w_treino)
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figResQQPlot_w (train).png',
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
pi.hat_w_teste = predict(out_w, newdata = dta_teste, type = 'response')
head(pi.hat_w_teste)


## Classificação com cutoff padrão
cutoff = 0.5
pred.Y_bin_w_teste = factor(ifelse(pi.hat_w_teste > cutoff, '1', '0'), levels = c('0', '1'))


## Referência ajustada
Y_bin_w_teste = factor(dta_teste$Y_bin_w, levels = c("0", "1"))
Y_bin_w_teste = relevel(dta_teste$Y_bin_w, ref = '0')


## Matriz de confusão base::table
table(pred.Y_bin_w_teste, Y_bin_w_teste)
table(pred.Y_bin_w_teste, Y_bin_w_teste) / NROW(dta_teste)


## Matriz de confusão via caret
# predito + referência caret
pred.Y_bin_caret_w_teste = relevel(pred.Y_bin_w_teste, ref = '1')
Y_bin_caret_w_teste = relevel(Y_bin_w_teste, ref = '1')
tab_w_teste = confusionMatrix(data = pred.Y_bin_caret_w_teste, reference = Y_bin_caret_w_teste)
tab_w_teste
# salvando em .txt
sink(file = '_out/output/tabConfMatrixNonOptimal_w (test).txt')
cat('\n')
print(tab_w_teste)
sink()


## Plot da matriz de confusão
p_w_teste = ggplot(as.data.frame(1e2 * round(tab_w_teste$table / NROW(dta_teste), 4)), 
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
p_w_teste
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figConfMatrixNonOptimal_w (test).png',
          device = png, 
          width = 1280*c.width, height = 720*c.height, res = 96*c.res, units = 'px')




# #################################################### #
# Classificação com ponto de corte ótimo (banco teste) #
# #################################################### #

## Cutoff ótimo via Youden
# NOTA: o cutoff ótimo vem do banco de treino!
# ---- não fazemos curva ROC no teste, então
# ---- o objeto "ROC_w_teste" não existe!
C_teste = coords(ROC_w_treino, x = 'best')
C_teste


## Classificação com cutoff ótimo
cutoff_teste = C_teste$threshold
pred.Y_bin_w_teste = factor(ifelse(pi.hat_w_teste > cutoff_teste, '1', '0'), levels = c('0', '1'))


## Nova matriz de confusão
# predito + referência caret
pred.Y_bin_caret_w_teste = relevel(pred.Y_bin_w_teste, ref = '1')
Y_bin_caret_w_teste = relevel(Y_bin_w_teste, ref = '1')
tab_w_teste = confusionMatrix(data = pred.Y_bin_caret_w_teste, reference = Y_bin_caret_w_teste)
tab_w_teste
# salvando em .txt
sink(file = '_out/output/tabConfMatrixOptimal_w (test).txt')
cat('\n')
print(tab_w_teste)
sink()


## Plot da nova matriz de confusão
p_w_teste = ggplot(as.data.frame(1e2 * round(tab_w_teste$table / NROW(dta_teste), 4)), 
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
p_w_teste
# salvando figura [caixa quadrada] (png)
c.width = 1; c.height = 1; c.res = 1.5
dev.print(file = '_out/figures/figConfMatrixOptimal_w (test).png',
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
  out_w,
  out_w_treino,
  pi.hat_w,
  pi.hat_w_teste,
  pi.hat_w_treino,
  ## arquivo
  file = '_out/out_w.RData'
)

