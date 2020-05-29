# Modelos Econometricos para atividade de Análise da Conjuntura


# Limpar tudo
rm(list=ls())

# Selecionar diretorio
setwd("/Users/nicholaslepetit/Documents/ECONOMIA/02.2019/ANALISE CONJUNTURA/")
dir()

# Lendo banco de dados
milho <- read.csv2("datacsv.csv")
head(milho)

# Removendo a coluna dos anos
milho <- subset(milho, select = -c(Anos))
head(milho)

# Transformando em serie temporal
milho.ts <- ts(milho, start = c(2004,1), end = c(2019,1), frequency = 1)
head(milho.ts)
library(dynlm)


######################################//###########################################
################### Modelo de Koyck - Defasagens infinitas ########################

#milho.inf <- dynlm(Prod ~ L(Preco, 1) + Cambio + Cons + Estoque.Inicial + Exp + Imp + L(Prod, 1), data = milho.ts)
milho.inf <- dynlm(Prod ~ Preco + Cambio + Cons + Estoque.Inicial + Exp + Imp + L(Prod, 1), data = milho.ts)
summary(milho.inf)

library(lmtest)
# Teste Breusch-Pagan para Heterocedasticidade. Uma hipótese de MQO é a homocedasticidade.
bptest(milho.inf)
# Nao ha evidencias estatisticas para rejeitarmos a hipotese H0 da homocedasticidade.
# Portanto nao podemos afirmar que o modelo é heterocedastico.

# Visualizacao grafica autocorrelacao
ehat2 <- resid(milho.inf)
milhoacf <- acf(ehat2,main="Correlograma")

# Testar autocorrelacao com o Teste Breusch-Godfrey
library(lmtest)
bg_1 <- bgtest(milho.inf,order=1, type="Chisq")
bg_1
bg_2 <- bgtest(milho.inf,order=2, type="Chisq")
bg_2
bg_3 <- bgtest(milho.inf,order=3, type="Chisq")
bg_3
bg_4 <- bgtest(milho.inf, order=4, type="Chisq")
bg_4
# Diagnostico: Há evidencias estatisticas para rejeitarmos H0 e aceitarmos a HA de que
# existe autocorrelacao dos erros, a partir da ordem 2, mas beira o alpha na ordem 1.

# Estimar um modelo consistente com heterocedasticidade e autocorrelacao - 
# (Correcao de Newey-West).
library(sandwich)
nw_milho.inf <- coeftest(milho.inf, vcov. = NeweyWest(milho.inf))
nw_milho.inf # Modelo corrigido

########### Estimando efeitos de LP; Defasagem Média e Defasagem Mediana ##############

# Armazenando os coeficientes
alpha <- coef(milho.inf)[[1]]
b0preco <- coef(milho.inf)[["Preco"]]
b0cambio <- coef(milho.inf)[["Cambio"]]
b0cons <- coef(milho.inf)[["Cons"]]
b0estoque <- coef(milho.inf)[["Estoque.Inicial"]]
b0exp <- coef(milho.inf)[["Exp"]]
b0imp <- coef(milho.inf)[["Imp"]]
lambda <- coef(milho.inf)[[8]]

# b0(1/1-lambda)

######## Influencias de variacoes em t-1 das variaveis em destaque no tempo t0

# Influencia de variacao no preco de t-1 atuando no periodo t0
bpreco1 <- b0preco*(lambda^1)
bpreco1
bpreco2 <- b0preco*(lambda^2)
bpreco2
bpreco3 <- b0preco*(lambda^3)
bpreco3
# Uma variacao positiva de R$1 no preco durante o periodo anterior (t-1, um ano atras),
# exerce sob a produção atual (t0) uma influencia na forma de uma reducao de, em média
# -53.620 toneladas produzidas.

# Influencia de variacao no cambio de t-1 atuando no periodo t0
bcambio1 <- b0cambio*(lambda^1)
bcambio1
bcambio2 <- b0cambio*(lambda^2)
bcambio2
bcambio3 <- b0cambio*(lambda^3)
bcambio3
# Uma variacao positiva de R$1 no cambio durante o periodo anterior (t-1, um ano atras),
# exerce sob a produção atual (t0) uma influencia na forma de um aumento de, em média
# 975.543 toneladas produzidas.

# Influencia de variacao no consumo interno de t-1 atuando no periodo t0
bcons1 <- b0cons*(lambda^1)
bcons1
# Uma variacao positiva de 1.000 toneladas no consumo interno durante o periodo anterior
# (t-1, um ano atras), exerce sob a produção atual (t0) uma influencia na forma de aumento
# de, em média, 233 toneladas produzidas.

# Influencia de variacao no estoque inicial de t-1 atuando no periodo t0
bestoque1 <- b0estoque*(lambda^1)
bestoque1
# Uma variacao positiva de 1.000 toneladas nos estoques iniciais durante o periodo anterior
# (t-1, um ano atras), exerce sob a produção atual (t0) uma influencia na forma de reducao
# de, em média, -196 toneladas produzidas.

# Influencia de variacao na exportacao de t-1 atuando no periodo t0
bexp1 <- b0exp*(lambda^1)
bexp1
# Uma variacao positiva de 1.000 toneladas na exportação durante o periodo anterior
# (t-1, um ano atras), exerce sob a produção atual (t0) uma influencia na forma de aumento
# de, em média, 28,6 toneladas produzidas.

# Influencia de variacao na importacao de t-1 atuando no periodo t0
bimp1 <- b0imp*(lambda^1)
bimp1
# Uma variacao positiva de 1.000 toneladas na importacao durante o periodo anterior
# (t-1, um ano atras), exerce sob a produção atual (t0) uma influencia na forma de reducao
# de, em média, 171 toneladas produzidas.

# Efeito no longo prazo do aumento de R$1 no preco no periodo t
LPpreco <- b0preco*(1/(1-lambda))
LPpreco
# Espera-se que, um aumento de R$1 no preco da saca de 60kg de milho, tenha um efeito
# sob a producao no longo-prazo de uma variacao negativa de, em media, -641 mil ton.


# Efeito no longo prazo do aumento de R$1 no cambio no periodo t
LPcambio <- b0cambio*(1/(1-lambda))
LPcambio
# Espera-se que, um aumento de R$1 no cambio R$/USD, tenha um efeito sob a producao
# no longo-prazo na forma de uma variacao positiva de, em media, 11.668.370 ton, ou ainda,
# 11,67 mi de toneladas.

# Efeito no longo prazo do aumento de 1 unidade no consumo no periodo t
LPcons <- b0cons*(1/(1-lambda))
LPcons
# Espera-se que, um aumento de 1.000 toneladas no consumo interno, aumente a producao
# no longo-prazo, em média, 2.790 toneladas.

# Efeito no longo prazo do aumento de R$1 no preco no periodo t
LPestoque <- b0estoque*(1/(1-lambda))
LPestoque
# Uma variacao positiva de 1.000 toneladas no estoque inicial, deve levar no longo-prazo,
# a uma queda de, em média, -2.346 toneladas na producao.

# Efeito no longo prazo do aumento de R$1 no preco no periodo t
LPexp <- b0exp*(1/(1-lambda))
LPexp
# Uma variacao positiva de 1.000 toneladas na exportacao, deve levar no longo-prazo,
# a um aumento de, em média, 342 toneladas na producao.

# Efeito no longo prazo do aumento de R$1 no preco no periodo t
LPimp <- b0imp*(1/(1-lambda))
LPimp
# Uma variacao positiva de 1.000 toneladas na importacao, deve levar no longo-prazo,
# a uma queda de, em média, -2.056 toneladas na producao.



# Defasagem Mediana
DMediana <- -log(2)/log(lambda)
DMediana
# 0,29 anos para que ocorra 50% do efeito de LP. Ou seja, antes de o primeiro tri-
# mestre se encerrar, voce ja tem 50% do efeito de Longo Prazo operando

# 2 produces ao ano enquanto a comercializacao das safras se da de 2 em 2 meses. 1 safra:
# 4 meses, segunda safra + 4 meses.

# Defasagem Média
DMedia <- lambda/(1-lambda)
DMedia

# DMedia é o periodo que se leva para perceber o efeito de longo prazo na variacao
# da producao. Em 1,2 meses jaá se percebe o efeito de LP.








######################################//###########################################
################## Previsoes usando modelos autoregressivos #######################
################################ ///////////////// ################################

# Carregando bancos de dados e transformando em séries temporais

dpreco <- read.csv("AR/dpreco.csv")
dpreco <- ts(dpreco, start=c(2004,1), end = c(2019, 1), frequency = 1)

dcambio <- read.csv("AR/dcambio.csv")
dcambio <- ts(dcambio, start=c(1996,1), end = c(2019, 1), frequency = 1)

d4v <- read.csv2("AR/f4v2.csv")
head(d4v)
d4v.ts <- ts(d4v, start = c(2000, 1), end = c(2019,1))
d4v.ts

dest <- d4v.ts[,"Estoque"]
dimp <- d4v.ts[,"Imp"]
dcons <- d4v.ts[,"Cons"]
dexp <- d4v.ts[,"Exp"]

library(forecast)
# AR Preco
preco.ar <- ar(dpreco,aic=TRUE,method="ols") # AIC = Akaike Information Criterion
preco.ar
fc.preco <- data.frame(forecast(preco.ar, 1))
fc.preco

# AR Cambio
cambio.ar <- ar(dcambio,aic=TRUE,method="ols") # AIC = Akaike Information Criterion
cambio.ar
fc.cambio <- data.frame(forecast(cambio.ar, 1))
fc.cambio

# AR Consumo
cons.ar <- ar(dcons,aic=TRUE,method="ols") # AIC = Akaike Information Criterion
cons.ar
fc.cons <- data.frame(forecast(cons.ar, 1))
fc.cons

# AR Estoque Inicial
est.ar <- ar(dest,aic=TRUE,method="ols") # AIC = Akaike Information Criterion
est.ar
fc.est <- data.frame(forecast(est.ar, 1))
fc.est

# AR Exp
exp.ar <- ar(dexp,aic=TRUE,method="ols") # AIC = Akaike Information Criterion
exp.ar
fc.exp <- data.frame(forecast(exp.ar, 1))
fc.exp

# AR Imp
imp.ar <- ar(dimp,aic=TRUE,method="ols") # AIC = Akaike Information Criterion
imp.ar
fc.imp <- data.frame(forecast(imp.ar, 1))
fc.imp

summary(milho.inf)
# Cenario P = Pessimista
yhatp <- coef(milho.inf)[[1]] + b0preco * 49.39 + b0cambio *  3.89 + b0cons * 63729.29 + b0estoque * 23648.7 + b0exp * 119118.8 + b0imp * 14882.25
yhatp

# Cenario C = Conservador
yhatc <- coef(milho.inf)[[1]] + b0preco * 45.03 + b0cambio *  3.89 + b0cons * 65057.65 + b0estoque * 26255.38 + b0exp * 123525.05 + b0imp * 14472.10
yhatc

# Cenario O = Otimista
yhato <- coef(milho.inf)[[1]] + b0preco * 40.66 + b0cambio *  3.89 + b0cons * 66386 + b0estoque * 28862.05 + b0exp * 127931.3 + b0imp * 14061.94
yhato


# Estimar g.ar2 (mesmo modelo basicamente)
prod <- milho[,"Prod"]
prod.ts <- ts(prod, start = c(2004,1), end = c(2019,1), frequency = 1)

# usando as estimativas para previsao forecast
library(forecast)

# Criando o modelo com ar()
gprod.ar <- ar(prod.ts,aic=TRUE,method="ols") # AIC = Akaike Information Criterion - Uma estatistica
gprod.ar
# Fazer a previsao com forecast
forecast.prod <- data.frame(forecast(gprod.ar, 2))
forecast.prod
# Quanto menor os intervalos entre as estimativas Lo e Hi, mais confiavel e melhor o modelo
# é para que sejam feitas previsões.

plot(forecast(gprod.ar, 2))

