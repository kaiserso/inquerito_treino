## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##
##       R PROGRAM: sessao3_tidy.R
##
##          AUTHOR: Peter Young - CDC Mozambique
##           EMAIL: fqm1@cdc.gov
##
##         PROJECT: INS Survey Data Analysis Training
##
##     Description: Teaching code for Session 3, Basic R with Tidy
##
##            NOTE: This file is used to prepare a practical training session on
##                  basic R usage for those not familiar with R or R Studio.
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
##           INPUT: nhanes.csv, nhanes.dta
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
##            Date: Mon Jun 30 10:03:01 2025
##
##      CHANGE LOG: Date        Change
##                  ---         ---
##                  Mon Jun 30, 2025    Initial Version
##                  ---         ---
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

# Libraries --------------------------------------------------------------------

#install.packages(c("tidyverse", "gtsummary"))

library(tidyverse) # esta libraria inclui varios outros pacotes (dplyr, ggplot2, etc)
library(haven)

# Tibbles ----------------------------------------------------------------------

# similar a um 'data frame'

# Neste exemplo, I = interviewed, D = destroyed, R = refused

afs <- tibble(householdid = c(1000, 1001, 2002, 2003, 2004), 
              residency = c("Urban", "Urban", "Rural", "Rural", "Rural"),
              size = c(2, 2, 1, 3, 0),
              status = c(rep("I", 4), "D"))

pers <- tibble(personid = c(5001:5009),
               idade = c(rep(c(20,34), 4), 55),
               householdid = c(rep(c(1000, 1001), each=2),
                               2002, rep(2003, 3), 2005),
               edu_yrs = seq(1, 18, by=2),
               status = c(rep("I", 7), rep("R", 2)))

# leva alguns minutos para explorar e entender as duas tabelas em termos de variaveis,
# numero de casos, o que cada caso representa, e quais variaveis podem servir de 
# identificador unico de cada caso

afs
pers

# podemos resumir como um data frame
summary(pers$edu_yrs)

# podemos converter em data frame
as.data.frame(pers)

# Exercicio: a) usar a indexacao para identificar a idade da segunda pessoa, b) listar as idades
# de todos que tem > 20 anos de idade.

# alguns comandos do tidyverse -------------------------------------------------

# filter - selecionar linhas com criterios
filter(pers, status == "R")
filter(pers, idade > 20)

# select - seleccionar colunas com criterios
select(pers, householdid)
select(pers, c(personid, status))

select(pers, where(is.numeric))  # com alguma magia de 'where'

# mutate - alterar o criar colunas 
mutate(pers, idade_meses = idade *12)

# summarise - agregar
summarise(pers, total = sum(idade))  # uma fila em vez de varias

# Exercicio: calcular a idade media das pessoas que recusaram em
# participar no inquerito, usando estas funcoes

# Dica - pode criar um tibble com resultados interinos (<-)

# Pipes (tubos) -------------------------------------------------------

# normalmente, quando queremos selecionar um subconjunto de um data
# frame, e actualiza-lo, resulta em codigo um pouco 'feio' ou inelegante, 
# no base R:

pers$idade[pers$status == "I"]

# entra o pipe operator (|>)

pers |> filter(status == "I") |> 
  select(idade)

# podem ser extendida de forma indefinida, e permitem ler a logica
# de izquerda a direita (ou acima para abaixo)

pers |> filter(status == "I") |> 
  select(idade) |>
  mutate(idade_meses = idade * 12) |> 
  summarise(total = sum(idade_meses))

# summarize by group -----------------------------------------------------------

# tambem podemos agregar por subgrupos
pers |> group_by(status) |> 
  summarise(idade_media = mean(idade))

# Exercicio: a) calcular o tamanho medio dos agregados familiares por residencia, 
# sao maiores os agregados na zona urbana ou rural? b) refazer o calculo, somente
# incluindo as agregados que foram entrevistadas. Muda a resposta?

# conditional updates ---------------------------------------------------------

# corregir o caso 5009 - como o inquerito era para adultos de 15-49 anos, era 
# ineligivel, portanto nao podia 'recusar' - usar ifelse

pers |> mutate(status2 = if_else(idade > 49, "U",
                                         status))

# como salvar o novo tibble?

pers <- 
  pers |> mutate(status2 = if_else(idade > 49, "U",
                                           status))

# uma maneira mais flexivel - case_match

# substituir "I" com "Entrevistado" e "R" com "Recusa" no status
# o tilde (~) separe o original pelo novo codigo:

pers |> mutate(status3 = case_match(status2,
                                            "I" ~ "Entrevistado",
                                            "R" ~ "Recusa"))

# oops, esquecemos ineligivel, U (ineligivel) ficou NA na nova variavel!
pers |> mutate(status3 = case_match(status2,
                                            "I" ~ "Entrevistado",
                                            "R" ~ "Recusa", 
                                            .default = "Ineligivel")) # nao ideal, presume muito

pers |> mutate(status3 = case_match(status2,
                                            "I" ~ "Entrevistado",
                                            "R" ~ "Recusa", 
                                            "U" ~ "Ineligivel")) # um pouco melhor

# ainda mais flexivel - usar condicoes - case_when

pers <-
  pers |> mutate(edu_cat = case_when(edu_yrs < 5 ~ "1-6",
                                      edu_yrs < 13 ~ "7-12",
                                      TRUE ~ "13+"))
pers

# Obs, notar uma diferenca entre case_match e case_when, onde fica a variavel que
# e base na transformacao?

# Exercicio: usar case_when para recategorizar idade em dois grupos, <25 e 25+

# Merging ----------------------------------------------------------------------

# Eercicio: a) o merge destes dados seria 1 a N, N a 1, 1 a 1 ou N a N? b) qual seria a 'chave' em cada
# tabela para o merge?  c) quantos casos devem resultar da merge?

# com base R, podemos usar 'merge' para juntar dados de 1:n, 1:1 ou n:n

combinado1 <- merge(afs, pers)  # sem opcoes, faz match em todos as colunas comuns (householdid e status)
combinado1

combinado2 <- merge(afs, pers, by = "householdid")  # agora sim - mas o que acontece com status?
combinado2

# outra alternativa, talvez mais clara

pers$person_status <- pers$status
pers$status <- NULL
pers

# quem ficou de fora?

combinado3 <- merge(afs, pers)
combinado4 <- merge(afs, pers, all.x = TRUE)  # o que acontece com os nao-matches?
combinado5 <- merge(afs, pers, all.y = TRUE)
combinado6 <- merge(afs, pers, all = TRUE)

# como identificar os nao-matches? (base R)

combinado6[is.na(combinado6$personid),]       # missing no pers
combinado6[is.na(combinado6$residency),]      # missing no afs

# tidyverse

combinado7 <- inner_join(afs, pers)  # sem opcoes, similar a merge, usa as colunas comuns. Salientar que
combinado7

all.equal(combinado3, as.data.frame(combinado7)) # quase - ordem difere

combinado8 <- left_join(afs, pers)  # all.x = TRUE
combinado9 <- right_join(afs, pers)  # all.y = TRUE
combinado10 <- full_join(afs, pers)  # all = TRUE

combinado10 |> filter(is.na(personid))
combinado10 |> filter(is.na(residency))

# Pergunta: porque olhamos para 'personid' para identificar os casos em falta no tibble pers?

# Skip patterns ----------------------------------------------------------------

# os dados de inquerito frequentemente sao distribuidos em formato de Stata ou 
# CSV. Stata tem ventagens como uma boa estruturacao de dados (labels, missing, etc)

nhanes <- read_dta("data/nhanes_recode.dta")
str(nhanes)
names(nhanes)
nhanes

# vamos converter alguns variaveis com labels em factores para R:
nhanes$marital <- as_factor(nhanes$marital)
nhanes$sex <- as_factor(nhanes$sex)
nhanes$marital <- as_factor(nhanes$marital)
nhanes$hhref_edu <- as_factor(nhanes$hhref_edu)
nhanes$hh_inc <- as_factor(nhanes$hh_inc)
nhanes$race <- as_factor(nhanes$race)
nhanes$hhref_edu2 <- as_factor(nhanes$hhref_edu2)
nhanes$hhref_edu3 <- as_factor(nhanes$hhref_edu3)
nhanes$hh_inc_high <- as_factor(nhanes$hh_inc_high)

## missing ---------------------------------------------------------------------

# estado marital tem missing? porque?
sum(is.na(nhanes$marital))

# Exercicio: consultar o dicionario de dados e ver porque sao missings

## Acrescentando um nivel de factor ----------------------------------------------

nhanes |> filter(RIDAGEYR < 20) |> select(marital) |> summary()

nhanes$marital2 <- nhanes$marital
nhanes$marital2[nhanes$RIDAGEYR < 20] <- "N/A (<20 yrs)"  # nao funciona

# converter em string primeiro
nhanes$marital2 <- as.character(nhanes$marital)
nhanes$marital2[nhanes$RIDAGEYR < 20] <- "N/A (<20 yrs)"  # agora sim

table(nhanes$marital3)
table(nhanes$marital2)
nhanes$marital2 <- as.factor(nhanes$marital2)             # reconverter em factor
table(nhanes$marital2)
class(nhanes$marital2)

# com tidyverse
nhanes <- nhanes |> mutate(marital3 = as.character(marital),
                            marital3 = if_else(RIDAGEYR < 20, "N/A (<20 yrs)", marital3),
                            marital3 = as.factor(marital3))

# a ordem ficou alfabetico...
with(nhanes,
     table(marital3))

# ordem corregido
nhanes$marital2 <- factor(nhanes$marital2, levels = c(levels(nhanes$marital), "N/A (<20 yrs)"))
nhanes <- nhanes |> mutate(marital3 = factor(marital3, 
                                              levels = c(levels(marital), "N/A (<20 yrs)")))

## Recusas ---------------------------------------------------------------------

sum(nhanes$marital2 == "Don't Know")
sum(nhanes$marital2 %in% c("Don't Know", "Refused"))  # %in% e conveniente
sum(is.na(nhanes$marital2))

nhanes$marital2[nhanes$marital2 %in% c("Don't Know", "Refused")] <- NA
sum(nhanes$marital2 %in% c("Don't Know", "Refused"))
sum(is.na(nhanes$marital2))

addmargins(table(nhanes$marital2))   # 9968 + 3 = 9971 onde que foram os msisings?
addmargins(table(nhanes$marital2, useNA="ifany"))         # sempre bom confirmar

# obs: notar a diferenca entre o codigo "N/A (<20 yrs)" e <NA>

## com Tidy --------------------------------------------------------------------

# nhanes$marital3 <- as.character(nhanes$marital)
# nhanes$marital3[nhanes$RIDAGEYR < 20] <- "N/A (<20 yrs)"  # agora sim
# nhanes$marital3 <- as.factor(nhanes$marital3)             # reconverter em factor

nhanes <- nhanes |> mutate(marital3 = if_else(marital3 %in% c("Don't Know", "Refused"),
                                               NA, marital3))

# as categorias 'Refused' e 'Dont' Know' agora estao vazias (mas ainda presentes!)
table(nhanes$marital3)

# mesma coisa que base?
all.equal(nhanes$marital2, nhanes$marital3)

# suprimir as categorias vazias
nhanes <- nhanes |> mutate(marital2 = factor(marital2), 
                            marital3 = factor(marital3))
table(nhanes$marital3)

all.equal(nhanes$marital2, nhanes$marital3)

# tabulacao --------------------------------------------------------------------

library(gtsummary)   # para tbl_*
library(knitr)       # para kable()

# somente imprimir uma tabela - nao ideal para tabelas grandes
pers |> kable()

# resumir a tabela em termos de distribuicao das variaveis categoricas, medias
# dos numericos, etc.
nhanes |> tbl_summary()

# estratificada, e somente certas variaveis
nhanes |> tbl_summary(by = sex,
                       include = c(hhref_edu, hh_inc, race, marital2))

# acrescentar ci or p-value
nhanes |> tbl_summary(include = c(hhref_edu)) |> add_ci()

nhanes |> tbl_summary(by = hh_inc_high, 
                       include = c(race)) |> 
  add_p() |> add_overall()                       # acrescentar uma coluna para o total

# percentagem de coluna
nhanes |> tbl_summary(by = sex,
                       include = c(hhref_edu, hh_inc_high, race, marital3),
                       percent = "column",
                       label = list(hhref_edu = "HH ref person education",
                                    hh_inc_high = "HH ref person income",
                                    marital3 = "Marital Status")  # corregir as etiquetas
                       ) |>
  add_overall() |> 
  add_p()                                         # acrescentar p-value para chi-square

# suprimir os missings
nhanes |> tbl_summary(by = sex,
                       include = c(hhref_edu, hh_inc_high, race, marital3),
                       percent = "column",
                       label = list(hhref_edu = "HH ref person education",
                                    hh_inc_high = "HH ref person income",
                                    marital3 = "Marital Status"),  # corregir as etiquetas
                       missing = "no") |>                         # remover missing
  
  add_overall() |> 
  add_p()                                         # acrescentar p-value para chi-square

# Exercicio: como podemos substituir os niveis 0,1 para hh_inc_high com "high", "low"?
# Exercicio: cruzar income e marital, sao associados?

# tabulacao de regressao -------------------------------------------------------

# primeiro, criar o modelo de regressao
gmod2 <- glm(hh_inc_high ~ race_white * sex + hhref_edu3, data = nhanes, 
             family = binomial(link = "logit"))
summary(gmod2)

# formulate a multivariate regression table
gmod2 |> tbl_regression(label = list(race_white = "White race",
                                      hhref_edu3 = "Higher education"))

# univariate table
nhanes |> tbl_uvregression(method = glm,
                            method.args = list(family = binomial),
                            y = hh_inc_high,
                            include = c(race_white, sex, hhref_edu3),
                            label = list(race_white = "White race",
                                         hhref_edu3 = "Higher education"))

# exponentiate
nhanes |> tbl_uvregression(method = glm,
                            method.args = list(family = binomial),
                            y = hh_inc_high,
                            include = c(race_white, sex, hhref_edu3),
                            label = list(race_white = "White race",
                                         hhref_edu3 = "Higher education"),
                            exponentiate = TRUE)

# graficos ---------------------------------------------------------------------

library(ggplot2)
nhanes |> ggplot(aes(x = hhref_edu, y = hh_inc_num)) +
  geom_boxplot()

# cleanup
nhanes |> ggplot(aes(x = hhref_edu, y = hh_inc_num)) +
  geom_boxplot() +
  ggtitle("HH reference person income by education")  + 
  xlab("Education") + ylab("Income")

# we can stratify in various ways
nhanes |> ggplot(aes(x = hhref_edu, y = hh_inc_num, color = sex)) +
  geom_boxplot() +
  ggtitle("HH reference person income by education")  + 
  xlab("Education") + ylab("Income")

nhanes |> ggplot(aes(x = hhref_edu3, y = hh_inc_num)) +
  geom_boxplot() +
  ggtitle("HH reference person income by education")  + 
  xlab("Education") + ylab("Income") + facet_wrap(~sex)

