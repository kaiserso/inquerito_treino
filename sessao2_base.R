## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##
##       R PROGRAM: sessao2_base.R
##
##          AUTHOR: Peter Young - CDC Mozambique
##           EMAIL: fqm1@cdc.gov
##
##         PROJECT: INS Survey Data Analysis Training
##
##     Description: Teaching code for Session 2, Basic R with Base R
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
##            Date: Mon Jun  9 10:03:01 2025
##
##      CHANGE LOG: Date        Change
##                  ---         ---
##                  Mon Jun  9, 2025    Initial Version
##                  Tue Jun 17, 2025    Refactored out advanced code to shorten
##                  ---         ---
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

# Ajuda --------------------------------------------------------------------

?ls
help("ls")
help("<-")              # para simbolos, melhor ""

# Bibliotecas --------------------------------------------------------------

# install.packages("haven", "epitools", "gmodels") # somente a primeira vez

library(haven)
library(epitools)
help(haven)             # os pacotes tambem tem help

# Exemplo ilustrativo -----------------------------------------------------

# Este exemplo so usa Base R (com excepcao de Haven).

# Ler uma base de dados em formato de Stata, visualizar, manipular, e fazer analise 
# estatistica

nhanes <- read_dta("data/nhanes.dta")

# recodificar os variaveis categoricas como factores
nhanes$hhref_edu <- as_factor(nhanes$hhref_edu)
nhanes$hh_inc <- as_factor(nhanes$hh_inc)

# excluir categorias que nao queremos considerar
nhanes <- subset(nhanes,                                     # a base
                 !hh_inc %in% c("Refused", "Don't Know") &   # as linhas a seleccionar
                   !hhref_edu == "Don't Know",               
                 c(hhref_edu, hh_inc))                       # as colunas a seleccionar

summary(nhanes)

# excluir as categorias vazias (don't know, refused)
nhanes$hhref_edu <- droplevels(nhanes$hhref_edu)
nhanes$hh_inc <- droplevels(nhanes$hh_inc)

summary(nhanes)

# converter as categorias em valores medias (midpoint) de cada categoria para
# ser mais proporcional no boxplot
nhanes$hh_inc_num <- factor(nhanes$hh_inc,
                            labels=c(2500, 5750, 12500, 17500, 22500, 
                                     30000, 40000, 50000, 60000, 70000, 
                                     40000, 10000, 90000, 150000))

# converter o factor em numero correspondente
nhanes$hh_inc_num <- as.numeric(as.character(nhanes$hh_inc_num))

# criar um boxplot estratificado (basico)
with(nhanes,
     boxplot(hh_inc_num ~ hhref_edu, 
        xlab = "Educa??o",     # se aparecer como 'Educa??o', tem de reabrir com codificacao LATIN1
        ylab = 'Renda',
        main = "Educa??o versus renda no agregado familiar (NHANES, 2015-16)",
        col = "bisque"))

# tabela simples
with(nhanes,
     addmargins(table(hh_inc, hhref_edu)))

# a evidencias de uma diferenca em renda por nivel de educacao?
with(nhanes,
     chisq.test(hh_inc, hhref_edu))

# faixinha - remover do ambiente
rm(nhanes)

# Variaveis ---------------------------------------------------------------

## Escalares --------------------------------------------------------------

x <- "hello world"        # um scalar de cadeia de letras (string)
x

y <- 30                   # um scalar de numero (integral)
y*100                     # expressao aritmetica
y > 20                    # expressao logica
y > Inf                   # Nada e > Infinito
y^2                       # y quadrado

# Exercicio: criar um variavel numerico com valor de -100 e dividir por 10
# Obs: so devem comecar o nome dos variaveis com letra, nao com numero ou simbolo
## Vectores -----------------------------------------------------------------

z <- c(1, 10)             # vector de numeros
z

a <- c("hello", "world")  # vector de strings
a

b <- c(1, "world")        # se tentamos misturar...
b

z[1]                      # inspeccao de elementos de um vector
a[2]                  

d <- c(z[1], a[2])        # criacao de um novo vector com elementos de outros

# Chamar funcoes ---------------------------------------------------------------

ls()                        # uso de funcoes predeterminadas
sqrt(4)                     # uma funcao pode ter argumentos, e pode produzir resultados
is.na(100)                  # nao esta em falta
is.na(NA)                   # esta sim em falta
sum(c(is.na(NA), is.na(2))) # quantos elementos estao em falta?

all.equal(b, d)             # uso de funcoes com >1 argumentos
all.equal(ls(),ls())        # funcoes podem ser 'nested' (chamar all.equal com resultado de ls)

# Exercicio: confirmar que dois quadrado e igual a menos dois quadrado com all.equals

# Listas e etiquetas --------------------------------------------------------

e <- list(1, "world")       # as listas sao importantes para alguns padroes no tidyverse
e
e <- list(primeiro = 1, segundo = "world")
e
names(e) <- c("Pri", "Seg") # renomear os elementos
e
e[1]                  # o '[' e para indexar listas e vectores.
e$Seg                 # uma alternativa quando o elemento tem etiqueta e usar '$'
e["Seg"]              # outra alternativa quando tem etiqueta (nome)

# Conjuntos de dados -------------------------------------------------------

## Data frames -------------------------------------------------------------

provs <- c("Maputo Cidade", "Maputo Provincia", "Gaza", "Inhambane")
pops <- c(1130319, 2390673, 1465802, 1564289)
f <- data.frame(provincia = provs, pop = pops)
f
str(f)                # descrever o objecto (same as Environment)
summary(f)            # resumir as variaveis
dim(f)                # dimensoes do objecto

str(e)                # str tambem funciona com outros objectos
dim(e)                # mas dim somente funciona com arrays, matrices, data frames...
length(e)             # length sim funciona
length(f)             # tambem com data frames


## Indexing ----------------------------------------------------------------

f[1,2]                # indexing - o item na primeira linha, segunda coluna
f[,1]                 # a 1a coluna
f[1,]                 # a 1a fila
f[1]                  # a 1a coluna (igual a f[,1])
f[c(1,2),]            # as 1as duas filas
f[,c(1,2)]            # as 1as duas colunas (somente tem 2)
f[c(3,4),2]           # a 3a e 4a iten na segunda coluna

f$provincia           # aceder com nome
f$pop                  
f$pop[2]              # o segundo elemento do vector resultando de seleccionar a 
                      # variavel 'pop'

## Operacoes vectorizadas -------------------------------------------------------

names(f) <- c("a", "b")   # podemos renomear as variaveis
f
names(f) <- c("provincia", "pop") # mas nao queremos!
f$pop / 100000            # podemos fazer operacoes aritmeticas 
f$pop_m <- f$pop / 100000   # podemos actualizar os valores
f

# Exercicio: criar uma nova variavel no f com nome "pop2" que e pop / 1000

## expressoes logicas -----------------------------------------------------

f$pop_m>15
f$provincia[c(FALSE, TRUE, FALSE, TRUE)]
f[f$pop_m>15,]
subset(f, pop_m>15)
subset(f, pop_m>15, provincia)

## Recodificacao ----------------------------------------------------------------

# usar vectorized assignment para recodificacao

f$pop_cat <- cut(f$pop, breaks = c(-1, 1499999, 9999999), 
                 labels = c("<1,5m", ">=1,5m")) # intervals fechado do direito por omissao
f
table(f$pop_cat)

f$provincia <- toupper(f$provincia)    # converter minuscula em maiuscula
f["provincia"]

# Exercicio: converter os nomes das provincias em minuscula

# Carregando dados ------------------------------------------------------------

# Podemos usar haven para ler dados em outros formatos. Um dos mais basicos
# e 'csv'. CSV ? muito flexivel, mas nao tem muita estrutura.

nhanes_raw <- read.csv("data/nhanes.csv")
dim(nhanes_raw)                         # quais sao as 'dimensoes' do nhanes_raw
str(nhanes_raw)                         # descrever o conteudo

# Obs: 'str' e igual ao que aparece no painel de 'Environment'

## Usando factores ----------------------------------------------------------

# criar um factor
nhanes_raw$sex <- factor(nhanes$RIAGENDR, 
                         levels = c(1,2), 
                         labels = c("Masculino", "Feminino"))

# o que e um factor a final?
table(nhanes_raw$sex)
table(as.numeric(nhanes_raw$sex))
typeof(nhanes_raw$sex)                   # afinal, e um numero integral?
class(nhanes_raw$sex)                    # a classe e factor
attributes(nhanes_raw$sex)               # esta classe tem etiquetas

nhanes_raw$ageyrs <- nhanes_raw$RIDAGEYR     # renomear
summary(nhanes_raw$ageyrs)               
nhanes_raw$agecat1 <- cut(nhanes_raw$ageyrs,
                          breaks = c(-1, 14, 24, 44, 64, 80))

with(nhanes_raw,
    table(ageyrs, agecat1))              # verificar a categorizacao esta certa

# Exercicio: como fazer a tabela sem usar 'with'?

table(nhanes_raw$agecat1)              
addmargins(table(nhanes_raw$agecat1))    # e o total e 9971

# pode renomear um factor com factor()
nhanes_raw$agecat1 <- factor(nhanes_raw$agecat1, 
                             labels = c("<15", "15-24", "25-44", "45-64", "65+"))
with(nhanes_raw,
  table(ageyrs, agecat1))        # verificar a categorizacao ainda esta certa

# Exercicio: converter o factor de Stata 'time_us' em R com as_factor. Confirmar
# quantos missings tem. Porque sera que tem tantos missings?  COmo sugere recodificar
# para analise?

## Ficheiros de Stata ----------------------------------------------------

# podemos aproveitar de haven para ler ficheiros em outros formatos
# estatisticos. Eu prefiro Stata, por ser simples mas ter um bom sistema de 
# etiquetas para valores de variaveis categoricas

nhanes <- read_dta("data/nhanes.dta")

str(nhanes[, c("sex", "marital")])  # mas precisa mais um paso para ser util

## Ajudantes de haven --------------------------------------------------------

# as_factor() puxa os atributos do Stata para R
nhanes$sex <- as_factor(nhanes$sex)
nhanes$marital <- as_factor(nhanes$marital)
nhanes$hhref_edu <- as_factor(nhanes$hhref_edu)
nhanes$hh_inc <- as_factor(nhanes$hh_inc)
nhanes$race <- as_factor(nhanes$race)
str(nhanes[, c("sex", "marital")]) 

addmargins(table(nhanes$sex))
addmargins(table(nhanes$marital))   # mas 5719 < 9971! verificar o codebook.

# Pergunta: porque sera que falta estado marital para alguns?  Se for para analisar
# estado marital, como faria a codificacao antes?

summary(subset(nhanes, RIDAGEYR < 20, marital))           

# Acrescentando um nivel de factor ----------------------------------------------

nhanes$marital2 <- nhanes$marital
nhanes$marital2[nhanes$RIDAGEYR < 20] <- "N/A (<20 yrs)"  # nao funciona

# converter em string primeiro
nhanes$marital2 <- as.character(nhanes$marital)
nhanes$marital2[nhanes$RIDAGEYR < 20] <- "N/A (<20 yrs)"  # agora sim

table(nhanes$marital2)
nhanes$marital2 <- as.factor(nhanes$marital2)             # reconverter em factor
table(nhanes$marital2)
class(nhanes$marital2)

# Converter a missing ---------------------------------------------------

sum(nhanes$marital2 == "Don't Know")
sum(nhanes$marital2 %in% c("Don't Know", "Refused"))

nhanes$marital2[nhanes$marital2 %in% c("Don't Know", "Refused")] <- NA
sum(nhanes$marital2 %in% c("Don't Know", "Refused"))

addmargins(table(nhanes$marital2))   # 9968 + 3 = 9971 onde que foram os msisings?
addmargins(table(nhanes$marital2, useNA="ifany"))         # sempre bom confirmar

# Graficos em R Base ---------------------------------------------------------

hist(nhanes$RIDAGEYR)
boxplot(nhanes$hh_inc ~ nhanes$hhref_edu)  # uso de formula ~

# podemos eliminar os missings no income - neste caso, estamos a recodificar como
# missing (mas nao estamos a eliminar os casos da base de dados)
nhanes$hh_inc[nhanes$hh_inc %in% c("Don't Know", "Refused")] <- NA
table(nhanes$hh_inc)

boxplot(nhanes$hh_inc ~ nhanes$hhref_edu)  # missings na educacao

nhanes$hh_inc_num <- as.numeric(nhanes$hh_inc)
table(nhanes$hh_inc_num)
table(nhanes$hh_inc_num, nhanes$hh_inc)

# Manipular os niveis do factor ----------------------------------------------

# podemos 'ponderar' as categorias de income em termos do valor no meio
# do intervalo (uma aproximacao)
nhanes$hh_inc_num <- factor(nhanes$hh_inc,
                             labels=c(2500, 5750, 12500, 17500, 22500, 
                                      30000, 40000, 50000, 60000, 70000, 
                                      40000, 10000, 90000, 150000))
table(nhanes$hh_inc_num)
class(nhanes$hh_inc_num)
nhanes$hh_inc_num <- as.numeric(as.character(nhanes$hh_inc_num))

# visualisar o que fizemos
boxplot(nhanes$hh_inc_num ~ nhanes$hhref_edu)         # missings??

# excluir os missings
nhanes$hhref_edu[nhanes$hhref_edu %in% c("Don't Know")] <- NA

# confirmar a exclusao dos missings
boxplot(nhanes$hh_inc_num ~ nhanes$hhref_edu)         # esta quase
nhanes$hhref_edu <- droplevels(nhanes$hhref_edu)      # remover niveis nao em uso
nhanes$hh_inc <- droplevels(nhanes$hh_inc)        
boxplot(nhanes$hh_inc_num ~ nhanes$hhref_edu)         # agora sim

boxplot(log10(nhanes$hh_inc_num) ~ nhanes$hhref_edu)  # transformacao do desfecho

# Analise categorica -----------------------------------------------

# o uso de 'with' simplificar a digitacao
with(nhanes,
     table(hh_inc, hhref_edu))

with(nhanes,
     chisq.test(hh_inc, hhref_edu))

# estratificacao - primeiro, simplificar

with(nhanes,
     boxplot(hh_inc_num ~ race))

# dicotimizar raca
nhanes$race_white <- ifelse(nhanes$race == "Non-Hispanic White", 1, 0)

with(nhanes,
     boxplot(hh_inc_num ~ race_white))

summary(nhanes$hh_inc_num)

# dicotimizar renda
nhanes$hh_inc_high <- ifelse(nhanes$hh_inc_num > 40000, 1, 0)

# dicotomizar educacao - ifelse, condicional e vectorizado
nhanes$hhref_edu3 <- ifelse(nhanes$hhref_edu %in% c("<9th Grade", "9-11th Gr"), 
                            "<HS", 
                            ">=HS")
nhanes$hhref_edu3 <- factor(nhanes$hhref_edu3)

# sempre verificar
with(nhanes,
     table(hh_inc_num, hh_inc_high, useNA="ifany"))

# Obs: e sempre bom incluir 'useNA="ifany"' para assegurar que os missings foram
# codificados devidamente.

## Analise estratificada ----------------------------------------------------

# uma tabela 2x2x2 (analise estratificada)
with(nhanes,
     table(hh_inc_high, race_white, hhref_edu3))

# facilitar tabulacao similar a SPSS
library(gmodels)
with(nhanes,
     CrossTable(hh_inc_high, race_white, 
                chisq=TRUE))

# Outra opcao, incluir ORs, RRs, etc
library(epitools)
with(nhanes,
     addmargins(table(hh_inc_high, race_white)))

with(nhanes,
     oddsratio(race_white, hh_inc_high))

# outra alternativa
with(nhanes,
     epitab(race_white, hh_inc_high))

# Verificar - conseguimos reproduzir?
3619*1779/(2691*1183)   # sim - 2.02

with(nhanes,
     riskratio(race_white, hh_inc_high))

# Verificar - conseguimos reproduzir?
1779/2962/(2691/6310)   # sim - 1.41

# fazer a mesma coisa com dados agregados
tab <- with(nhanes,
            table(race_white, hh_inc_high))
riskratio(tab)

# Modelos de regressao -------------------------------------------------------

lm(hh_inc_num ~ hhref_edu, data = nhanes)
lmod <- lm(hh_inc_num ~ hhref_edu, data = nhanes) # salvar o resultado

summary(lmod)                                   # qual e o grupo de referencia?
str(nhanes$hhref_edu)                          # o primeiro
levels(nhanes$hhref_edu)

# mudar a categoria de referencia
nhanes$hhref_edu2 <- relevel(nhanes$hhref_edu, ref = ">=Col.")
lmod2 <- lm(hh_inc_num ~ hhref_edu2, 
            data = nhanes)

summary(lmod2)                                  # agora as est. sao negativas
par(mfrow=c(2,2))                     # para ver todos os plots ao mesmo tempo
plot(lmod2)

# Regressao logistica -----------------------------------------------------

# devemos obter ~3,9 OR (veja acima analise categorica)
gmod1 <- glm(hh_inc_high ~ race_white + hhref_edu3, data = nhanes, 
             family = binomial(link = "logit"))
summary(gmod1)
exp(coefficients(gmod1))      # lembrar, coeficientes estao na escala logaritmico
exp(confint(gmod1))           # os CIs tambem

# sera que temos alguma interaccao com sexo? Parece que nao

gmod2 <- glm(hh_inc_high ~ race_white * sex + hhref_edu3, data = nhanes, 
             family = binomial(link = "logit"))
summary(gmod2)
exp(coefficients(gmod2))      # lembrar, coeficientes estao na escala logaritmico

write_dta(nhanes, path = "data/nhanes_recode.dta")
rm(nhanes)

# Fim!
