## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##
##       R PROGRAM: sessao2_adicional.R
##
##          AUTHOR: Peter Young - CDC Mozambique
##           EMAIL: fqm1@cdc.gov
##
##         PROJECT: INS Survey Data Analysis Training
##
##     Description: Teaching code for Session 2, Basic R additional material
##
##            NOTE: This file is used to prepare a practical training session on
##                  on basic R usage for those not familiar with R or R Studio.
##                  It is part of a sequence of modules on practical survey data
##                  analysis with R prepared by CDC for INS staff. This module 
##                  relies heavily on base R to avoid dependency on knowledge of
##                  tidyverse.
##
##    REFERENCE(S): Alan Agresti. An Introduction to Categorical Data Analysis. 
##                  Wiley & Sons, USA. 1996.
##
##                  The US Centers for Disease Control and Prevention. The National
##                  Health and Nutrition Examination Survey (NHANES) 2015-2016.
##                  https://www.cdc.gov/nchs/nhanes/new_nhanes.htm
##
##           INPUT: nhanes.dta
##
##          OUTPUT: N/A
##
##      DISCLAIMER: Although this program has been used by the Centers
##                  for Disease Control & Prevention (CDC), no warranty,
##                  expressed or implied, is made by the CDC or the U.S.
##                  Government as to the accuracy and functioning of the
##                  program and related program material nor shall the
##                  fact of distribution constitute any such warranty,
##                  and no responsibility is assumed by the CDC in
##                  connection therewith.
##
##            Date: Tue Jun 17 2025
##
##      CHANGE LOG: Date        Change
##                  ---         ---
##                  Tue Jun  17 2025  Initial version refactored from sessao2_base.R    
##                  ---         ---
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

library(haven)
library(epitools)

nhanes_old <- nhanes
nhanes <- read_dta("data/nhanes_recode.dta")

## Ajudantes de haven --------------------------------------------------------

# as_factor() puxa os atributos do Stata para R
nhanes$sex <- as_factor(nhanes$sex)
nhanes$marital <- as_factor(nhanes$marital)
nhanes$hhref_edu <- as_factor(nhanes$hhref_edu)
nhanes$hh_inc <- as_factor(nhanes$hh_inc)
nhanes$race <- as_factor(nhanes$race)
nhanes$hhref_edu3 <- as_factor(nhanes$hhref_edu3)
nhanes$hh_inc_high <- zap_labels(nhanes$hh_inc_high)
nhanes$hh_inc_high <- zap_formats(nhanes$hh_inc_high)

# como all.equal() funciona com numeros muito pequenos?

# Numeros decimais ----------------------------------------------------------
decimais <- c(10 + 1/10000000, 10 + 2/10000000)
decimais
decimais[1] == decimais[2]  
all.equal(decimais[1], decimais[2]) 
all.equal(10 + 1/1000000, 10 + 2/1000000) 

# Obs - tenha cuidado como a comparacao com numeros decimais

# Tabelas parciais --------------------------------------------------

# tabela parcial para controlar um terceiro factor


epitable(nhanes$hh_inc_high, nhanes$hhref_edu3, rev = "neither")


with(subset(nhanes, race_white == 1),
     oddsratio(hh_inc_high, hhref_edu3))

with(subset(nhanes, race_white == 0),
     oddsratio(hh_inc_high, hhref_edu3))

# another way, that allows us to calculate a common OR
tab3 <- with(nhanes, table(hhref_edu3, hh_inc_high, race_white))
partial_tables <- margin.table(tab3, c(1,2,3))/1.0
partial_tables

# Razao de chance comum -------------------------------------------------

mantelhaen.test(partial_tables)

# podemos criar funcoes... ver help(mantelhaen.test)
woolf <- function(x) {
  x <- x + 1 / 2
  k <- dim(x)[3]
  or <- apply(x, 3, function(x) (x[1,1]*x[2,2])/(x[1,2]*x[2,1]))
  w <-  apply(x, 3, function(x) 1 / sum(1 / x))
  1 - pchisq(sum(w * (log(or) - weighted.mean(log(or), w)) ^ 2), k - 1)
}

woolf(partial_tables)                           # talvez nao devemos usar

# Transformacao matematica  ------------------------------------------------------

lmod <- lm(hh_inc_num ~ hhref_edu, data = nhanes) # salvar o resultado

summary(lmod)                                   # qual e o grupo de referencia?

# Transformar o resultado (income)

lmod3 <- lm(log(hh_inc_num) ~ hhref_edu, 
            data = nhanes)
summary(lmod3)

par(mfrow=c(2,2))                      
plot(lmod3)

lmod4 <- lm(sqrt(hh_inc_num) ~ hhref_edu,
            data = nhanes)
summary(lmod4)
plot(lmod4)

par(mfrow=c(1,1))                      # restaurar

# Pronostico (prediction) --------------------------------------------------------

boxplot(nhanes$hh_inc_num ~ nhanes$hhref_edu, xlab = "education", ylab = "income",
        weights = nhanes$WTINT2YR,
        main = "Educação versus renda, NHANES, 2015-16")

# points acrescenta dados na janela actual (para fazer novo x-y plot usar plot())
points(predict(lmod, data.frame(hhref_edu = levels(nhanes$hhref_edu))), col = "red")
points(exp(predict(lmod3, data.frame(hhref_edu = levels(nhanes$hhref_edu)))), col = "blue")
points(predict(lmod4, data.frame(hhref_edu = levels(nhanes$hhref_edu)))^2, col = "green")

# Modelos log-lineares  -------------------------------------------------------

# lembrar que o OR e apropriado quando o resultado e incomum (~ <10% em ambos os
# grupos)

tab <- with(nhanes, 
            prop.table(table(hh_inc_high, hhref_edu3), 
                       margin = 2))
tab
pr1 <- tab[2,2]         # probabilidade de ter maior renda com maior educacao
pr2 <- tab[2,1]         # probabilidade de ter maior renda com menor educacao
or1 <- pr1/(1-pr1)      # chance 1
or2 <- pr2/(1-pr2)      # chance 2

# neste caso, o desfecho e comum (56% e 23%), o OR vai ser enviesado em comparacao
# com o PR
list(P1 = pr1, P2 = pr2, PR = pr1/pr2, OR = or1/or2) # grande diferenca!

# modelar o racio das prevalencias em vez do racio de chances

# for estimating prevalence ratios we can use log-linear models

gmod3 <- glm(hh_inc_high ~ hhref_edu3, data = nhanes, 
             family = binomial(link = "log"))
summary(gmod3)
exp(coefficients(gmod3))      # lembrar, coeficientes estao na escala logaritmico
exp(confint(gmod3))           # os CIs tambem

# o PR para maior educacao e 2.45, igual ao PR que calculamos acima (nao ajustado)

# agora, ajustar por sexo e raca

gmod4 <- glm(hh_inc_high ~ hhref_edu3 + sex + race_white, data = nhanes, 
             family = binomial(link = "log"))
summary(gmod4)
exp(coefficients(gmod4))      # lembrar, coeficientes estao na escala logaritmico
exp(confint(gmod4))           # os CIs tambem

# o PR para educacao passou de 2.45 para 2.32, uma reducao de 3% (podemos considerar
# que nao haja evidencia forte de confusao entre estes factores)

# Fim!
