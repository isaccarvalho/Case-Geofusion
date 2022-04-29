#Case Geofusion - Cientista de Dados - Isac Carvalho - 04/2022


#Bibliotecas usadas
install.packages("tidyverse")
install.packages("data.table")
install.packages("readxl")
install.packages("caret")
install.packages("miscTools")
install.packages("caTools")
install.packages("randomForest")
install.packages("h2o")


library(miscTools)
library(caTools)
library(tidyverse)
library(readr)
library(data.table)
library(readxl)
library(caret)
library(randomForest)
library(h2o)



###Carregando base de dados---------------------------------------------------------------------

#Base de Dados Geofusion
Base_Dados_Geo = read.csv("DadosDesafioCientista.csv", header=TRUE, sep=";",encoding = "UTF-8")


###Etapa_Análise_Exploratória------------------------------------------------------------------

tibble(Base_Dados_Geo)
unique(Base_Dados_Geo$cidade)

#Com a função já podemos observar como é a base de dados que vamos trabalhar.
#Temos uma base de dados de 456 linhas e 24 variáveis (456x24).
#Na variável 'cidade' temos duas categorias: "Rio de Janeiro" e "São Paulo".


Base_Dados_Geo_analysis1 = Base_Dados_Geo %>%
  group_by(cidade) %>%
  summarise(qtde_bairros = n()) 

print(Base_Dados_Geo_analysis1)
#Na cidade de Rio de Janeiro temos 160 bairros e na cidade de São Paulo temos 296.

#Podemos verificar em relação a população das 2 cidades
Base_Dados_Geo_analysis2 = Base_Dados_Geo %>%
  group_by(cidade) %>%
  summarise(populacao = sum(populacao))

print(Base_Dados_Geo_analysis2)
#Na cidade de Rio de Janeiro temos uma população de mais de 6,8 milhões de pessoas.
#Na cidade de São Paulo temos uma população de mais de 12 milhões de pessoas.

#Agora vamos verificar em relação a quantidade de domicílios
#Primeiro precisamos do Total de Domicílios
Base_Dados_Geo = Base_Dados_Geo %>%
  mutate(Total_Domicilios = rowSums(Base_Dados_Geo[14:21])) %>%
  select(codigo:popMaisDe60,Total_Domicilios,domiciliosA1:potencial)

tibble(Base_Dados_Geo)

#Agora podemos ver o total de domicílios por cidade.
#Com a coluna 'Total_Domicilios' nossa base de dados passa a ter 456 linhas e 25 colunas (453x25).

Base_Dados_Geo_analysis3 = Base_Dados_Geo %>%
  group_by(cidade) %>%
  summarise(Total_Domicilios = sum(Total_Domicilios))

print(Base_Dados_Geo_analysis3)
#Na cidade de Rio de Janeiro temos mais de 2,2 milhões de domicílios.
#Na cidade de São Paulo temos mais de 4,1 milhões de domicílios.

#Podemos ver também o total da população e de domicílios do público alvo das duas cidades.

#pra ajudar na compreensão vamos ver a média de cada tipo de população e
#domicílio fazer um consolidado em uma única tabela para depois comparararmos as duas cidades.
Base_Dados_Geo_analysis4 = Base_Dados_Geo %>%
  group_by(cidade) %>%
  summarise(Media_populacao25_34 = round(mean(popDe25a34), digits = 0))

Base_Dados_Geo_analysis5 = Base_Dados_Geo %>%
  group_by(cidade) %>%
  summarise(Media_populacao34_49 = round(mean(popDe35a49), digits = 0))

Base_Dados_Geo_analysis6 = Base_Dados_Geo %>%
  group_by(cidade) %>%
  summarise(Media_domiciliosA1 = round(mean(domiciliosA1), digits = 0))

Base_Dados_Geo_analysis7 = Base_Dados_Geo %>%
  group_by(cidade) %>%
  summarise(Media_domiciliosA2 = round(mean(domiciliosA2), digits = 0)) 

Base_Dados_Geo_analysis8 = Base_Dados_Geo %>%
  group_by(cidade) %>%
  summarise(Media_domiciliosB1 = round(mean(domiciliosB1), digits = 0)) 

Base_Dados_Geo_analysis9 = Base_Dados_Geo %>%
  group_by(cidade) %>%
  summarise(Media_domiciliosB2 = round(mean(domiciliosB2), digits = 0))

Base_Dados_Geo_analysis_all = merge(Base_Dados_Geo_analysis4,
                                    Base_Dados_Geo_analysis5,
                                    by= "cidade")

Base_Dados_Geo_analysis_all = merge(Base_Dados_Geo_analysis_all,
                                    Base_Dados_Geo_analysis6,
                                    by= "cidade")

Base_Dados_Geo_analysis_all = merge(Base_Dados_Geo_analysis_all,
                                    Base_Dados_Geo_analysis7,
                                    by= "cidade")

Base_Dados_Geo_analysis_all = merge(Base_Dados_Geo_analysis_all,
                                    Base_Dados_Geo_analysis8,
                                    by= "cidade")

Base_Dados_Geo_analysis_all = merge(Base_Dados_Geo_analysis_all,
                                    Base_Dados_Geo_analysis9,
                                    by= "cidade")

print(Base_Dados_Geo_analysis_all)

#Como primeiras conclusões podemos perceber que dentro do público alvo a cidade de São Paulo apresenta Médias da
#população e de domicílios bem próximas e em alguns casos até superiores em relação a cidade do Rio de Janeiro.
#As médias mostram um ótimo indicador dizendo que investir na cidade de São Paulo tem um grande potencial.


#Agora para continuar analisando nossa base de dados, vamos usar a função 'str' para verificar as classes dos dados.

str(Base_Dados_Geo)

#usando a função str já conseguimos notar que há algumas variáveis possuem classes incorretas.
#No caso vamos corrigir as classes das variáveis 'rendaMedia' e 'potencial'.

Base_Dados_Geo = Base_Dados_Geo %>%
  mutate(rendaMedia = as.numeric(rendaMedia)) %>%
  mutate(potencial = as.factor(potencial))

str(Base_Dados_Geo)
#Na variável 'potencial' com sua classe transformada em fator, mostra que temos 4 levels,
#sendo que deveríamos ter apenas 3 levels (Alto,Baixo e Médio)

levels(Base_Dados_Geo$potencial)
#Realmente temos 4 levels, sendo 3 deles como [Alto,Baixo e Médio] e o outro level como vazio("").
#Vamos usar a função 'summary' pra entender um pouco melhor.

summary(Base_Dados_Geo)

#usando a função summary já conseguimos notar algumas coisas bem importantes.
#Caso 1: na variável 'população' está indicando que tem bairros com população mínima de zero(0).
#Caso 2: nas variáveis 'rendaMedia' e 'faturamento' está indicando que há valores vazios(NA).
#Caso 3: na variável 'potencial' tem uma categoria vazia com 293 linhas.
#vamos verificar os 3 casos:

#Caso_1 - população zero
Base_Dados_Geo_C1 = Base_Dados_Geo %>%
  filter(populacao == 0)

tibble(Base_Dados_Geo_C1)
summary(Base_Dados_Geo_C1)
print(Base_Dados_Geo_C1$nome)

#Podemos perceber que os 3 bairros de SP: "Reserva Da Cantareira", "Eta Guaraú" e "Pico Do Jaraguá", estão com todas variáveis em zero(0),
#por falta de dados nesses 3 bairros, vamos excluí-los da nossa base de dados.

#excluindo bairros com população zero(0)
Base_Dados_Geo = Base_Dados_Geo %>%
  filter(populacao != 0)

tibble(Base_Dados_Geo)
summary(Base_Dados_Geo)
str(Base_Dados_Geo)


#Agora temos uma base de dados de 453 linhas e 25 colunas (453x25) e população mínima em 43.

Base_Dados_Geo_analysis1 = Base_Dados_Geo %>%
  group_by(cidade) %>%
  summarise(qtde_bairros = n()) 

print(Base_Dados_Geo_analysis1)
#E agora na cidade de Rio de Janeiro temos 160 bairros e na cidade de São Paulo temos 293.


#Caso 2 - rendaMedia NA

Base_Dados_Geo_C2_RM = Base_Dados_Geo %>%
  filter(is.na(rendaMedia))

str(Base_Dados_Geo_C2_RM)
unique(Base_Dados_Geo_C2_RM$cidade)
unique(Base_Dados_Geo_C2_RM$nome)


#Como podemos ver temos 6 observações com valores NA em 'rendaMedia'.
#Essas 6 observações são bairros do Rio Janeiro.
#Apesar da ausência desses dados ser apenas em 6 observações, são dados que podemos estimar.
#Pensando em um cenário real, esse problema poderia ocorrer novamente se o mesmo estudo for aplicado para outras cidades.
#criar uma solução para esse caso evitaria a perda de dados e faríamos ganhar tempo a longo prazo.

#Caso 2 - faturamento NA
Base_Dados_Geo_C2_FA = Base_Dados_Geo %>%
  filter(is.na(faturamento))

str(Base_Dados_Geo_C2_FA)
unique(Base_Dados_Geo_C2_FA$cidade)


#Como podemos ver temos 293 observações com valores NA em 'faturamento'.
#Essas 293 observações são bairros de São Paulo.
#Como esperávamos essa falta de dados é o objetivo-problema que foi nos dado para resolver.

#Caso 3 - potencial categoria vazia

Base_Dados_Geo_C3 = Base_Dados_Geo %>%
  filter(potencial == "")

str(Base_Dados_Geo_C3)
unique(Base_Dados_Geo_C3$cidade)

#também temos 293 observações com dados vazios em 'potencial'.
#Essas 293 observações também são bairros de São Paulo.
#Essa falta de dados é o nosso outro objetivo-problema que foi nos dado para resolver.

#Agora que já conhecemos a base de dados que vamos trabalhar, podemos passar para etapa de modelagem.


###Etapa_Modelagem_de_Dados------------------------------------------------------------------

#Primeiro vamos estimar a Renda Média dos bairros que estão faltando na base do Rio de Janeiro.
#Tendo a renda média dos bairros que faltam, vamos poder estimar o Faturamento dos bairros de São Paulo.
#Com os valores de Faturamento estimados, poderemos estimar o Potencial de cada um.

###Renda Média-------------------------------------------------------------------------------
#Para estimar a Renda Média vamos usar o modelo de regressão Random Forest.
#Mas Primeiro precisamos preparar a base de treino e teste.


#cidades do Rio de Janeiro que estão com dados vazios em Renda média e qual potencial de cada uma.
Base_Dados_Geo_analysis12 = Base_Dados_Geo %>%
  filter(is.na(rendaMedia)) %>%
  group_by(codigo,cidade,nome) %>%
  summarise(qtde = n())

print(Base_Dados_Geo_analysis12)

#vamos separar da base os bairros que não temos a Renda Média para estimar depois

Base_Dados_Geo_Rio = Base_Dados_Geo %>%
  filter(estado == "RJ") %>%
  select(codigo:domiciliosE,faturamento,potencial,rendaMedia)

#Base de dados para uso do modelo Renda Média excluindo os bairros sem renda média
Base_Dados_Geo_sem_RendaM = Base_Dados_Geo %>%
  filter(estado == "RJ") %>%
  filter(codigo != "3304557005",
         codigo != "3304557006",
         codigo != "3304557032",
         codigo != "3304557082",
         codigo != "3304557146",
         codigo != "3304557086") %>%
  select(codigo:domiciliosE,faturamento,potencial,rendaMedia)

#Bairros que precisam ter a Renda Media estimada
Base_Dados_Geo_RendaM_prev = anti_join(Base_Dados_Geo_Rio,Base_Dados_Geo_sem_RendaM, "codigo")
Base_Dados_Geo_RendaM_prev = Base_Dados_Geo_RendaM_prev %>% 
  mutate(rendaMedia = 0)


#separando Base treino e Base Teste
set.seed(1)
divisao = sample.split(Base_Dados_Geo_sem_RendaM$rendaMedia, SplitRatio = 0.70)
Base_Dados_Geo_RendaM_treino = subset(Base_Dados_Geo_sem_RendaM, divisao == TRUE)
Base_Dados_Geo_RendaM_teste = subset(Base_Dados_Geo_sem_RendaM, divisao == FALSE)


###criação do modelo regressão Randon Forest Renda Média
regressor = randomForest(x = Base_Dados_Geo_RendaM_treino[5:23], y = Base_Dados_Geo_RendaM_treino$rendaMedia, ntree = 1000)

previsoes_treinamento = predict(regressor, newdata = Base_Dados_Geo_RendaM_treino[-1])

R2_treinamento = rSquared(Base_Dados_Geo_RendaM_treino[['rendaMedia']], resid = Base_Dados_Geo_RendaM_treino[['rendaMedia']] - previsoes_treinamento)

previsoes_teste = predict(regressor, newdata = Base_Dados_Geo_RendaM_teste[-1])
MAE = mean(abs(Base_Dados_Geo_RendaM_teste[['rendaMedia']] - previsoes_teste))
R2_teste = rSquared(Base_Dados_Geo_RendaM_teste[['rendaMedia']], resid = Base_Dados_Geo_RendaM_teste[['rendaMedia']] - previsoes_teste)

#Coeficiente de determinação R²
print(R2_treinamento)
print(R2_teste)
#O modelo de regressão Random Forest funcionou muito bem e trouxe um resultado excelente no teste R².
#Com coeficiente de ~0.8 na base de treino e ~0.94 na base de teste,
#esses indicadores nos mostra que o modelo se ajustou muito bem a base de dados.

#Erro médio absoluto
print(MAE)
print(summary(Base_Dados_Geo_sem_RendaM$rendaMedia))

#O Erro Médio Absoluto(MAE) é de ~551. Isso quer dizer que temos uma margem de erro de 551 para mais ou
#para menos nos valores previstos para Renda Média.
#Em comparação com os valores (mínimo, 1º quartil, mediana, média, 3º quartil e máximo) de Renda Média da
#base de dados original, o Erro Médio Absoluto é relativamente baixo.

#Aplicando o modelo Randomforest para prever a Renda Média dos bairros que faltavam
Base_Dados_Geo_RendaM_prev$rendaMedia = predict(regressor, newdata = Base_Dados_Geo_RendaM_prev[-1])

Base_Dados_Geo_RendaM_prev = Base_Dados_Geo_RendaM_prev %>%
  mutate(rendaMedia = round(rendaMedia, digits = 0))

RendaM_prev_bairros = Base_Dados_Geo_RendaM_prev %>%
  select(codigo,cidade,nome,rendaMedia)

print(RendaM_prev_bairros)

#agora que já temos a Renda Média dos bairros que faltavam, vamos incluí-las em nossa base de dados.

#Unindo base com os bairros que tiveram a Renda Média estimada 
Base_Dados_Geo_Rio = rbind(Base_Dados_Geo_sem_RendaM,Base_Dados_Geo_RendaM_prev)

print(Base_Dados_Geo_Rio)

###Faturamento-------------------------------------------------------------------------------
#Para estimar o Faturamento vamos usar o modelo de regressão redes neurais H2O.
#Mas Primeiro precisamos preparar a base de treino e teste.

#Organizando variáveis
Base_Dados_Geo_Rio = Base_Dados_Geo_Rio %>%
  select(codigo:domiciliosE,rendaMedia,potencial,faturamento)

#Separando base de São Paulo para estimar o faturamento depois
Base_Dados_Geo_fatura = Base_Dados_Geo %>%
  select(codigo:domiciliosE,rendaMedia,potencial,faturamento)

Base_Dados_Geo_fatura_sampa = anti_join(Base_Dados_Geo_fatura,Base_Dados_Geo_Rio, "codigo")
Base_Dados_Geo_fatura_sampa = Base_Dados_Geo_fatura_sampa %>% 
  mutate(faturamento = 0)

#separando Base treino e Base Teste
set.seed(1)
divisao = sample.split(Base_Dados_Geo_Rio$faturamento, SplitRatio = 0.70)
Base_Dados_Geo_Rio_treino = subset(Base_Dados_Geo_Rio, divisao == TRUE)
Base_Dados_Geo_Rio_teste = subset(Base_Dados_Geo_Rio, divisao == FALSE)


###criando modelo de regressão - redes neurais h2o
h2o.init(nthreads = -1)
regressor = h2o.deeplearning(y = 'faturamento', training_frame = as.h2o(Base_Dados_Geo_Rio_treino), activation = 'Rectifier',
                             hidden = c(100, 100), epochs = 1000)

previsoes_treinamento = predict(regressor, newdata = as.h2o(Base_Dados_Geo_Rio_treino[-1]))
previsoes_treinamento = as.vector(previsoes_treinamento)

R2_treinamento = rSquared(Base_Dados_Geo_Rio_treino[['faturamento']], resid = Base_Dados_Geo_Rio_treino[['faturamento']] - previsoes_treinamento)

previsoes_teste = predict(regressor, newdata = as.h2o(Base_Dados_Geo_Rio_teste[-1]))
previsoes_teste = as.vector(previsoes_teste)
MAE = mean(abs(Base_Dados_Geo_Rio_teste[['faturamento']] - previsoes_teste))
R2_teste = rSquared(Base_Dados_Geo_Rio_teste[['faturamento']], resid = Base_Dados_Geo_Rio_teste[['faturamento']] - previsoes_teste)


#Coeficiente de determinação R²
print(R2_treinamento)
print(R2_teste)
#O modelo de regressão h2o.deeplearning trouxe um resultado excelente no teste R².
#Com coeficiente de ~0.997 na base de treino e ~0.994 na base de teste,
#esses indicadores nos mostra que a interpretalidade do modelo se ajustou muito bem a base de dados.

#Erro médio absoluto
print(MAE)
print(summary(Base_Dados_Geo_Rio$faturamento))
#O Erro Médio Absoluto(MAE) é de ~24387. Isso quer dizer que temos uma margem de erro de 24387 para mais ou
#para menos nos valores previstos para Faturamento.
#Em comparação com os valores (mínimo, 1º quartil, mediana, média, 3º quartil e máximo) de Faturamento da
#base de dados original, o Erro Médio Absoluto está bem abaixo.

#Aplicando o modelo deeplearning h2o para prever o faturamento dos bairros de São Paulo
Base_Dados_Geo_fatura_sampa$faturamento = predict(regressor, newdata = as.h2o(Base_Dados_Geo_fatura_sampa[-1]))
Base_Dados_Geo_fatura_sampa$faturamento = as.vector(Base_Dados_Geo_fatura_sampa$faturamento)

Base_Dados_Geo_fatura_sampa = Base_Dados_Geo_fatura_sampa %>%
  mutate(faturamento = round(faturamento, digits = 0)) %>%
  mutate(faturamento = abs(faturamento))

Base_Dados_Geo_sampa = Base_Dados_Geo_fatura_sampa

faturamento_prev_bairros = Base_Dados_Geo_sampa %>%
  select(codigo,cidade,nome,faturamento)

print(faturamento_prev_bairros)

#agora que já temos o Faturamento dos bairros de São Paulo,
#podemos estimar o potencial de cada um.



###potencial-------------------------------------------------------------------------------
#Para estimar o potencial vamos usar o modelo de classificação redes neurais H2O.
#Mas Primeiro precisamos preparar a base de treino e teste.

#Organizando variáveis

#transformando as categorias de potencial em decimal
Base_Dados_Geo_Rio_esc2 = Base_Dados_Geo_Rio %>%
  mutate(potencial_n = as.numeric(potencial))

Base_Dados_Geo_potencial_dec = Base_Dados_Geo_Rio_esc2 %>%
  group_by(potencial,potencial_n) %>%
  summarise(qtde = n())

print(Base_Dados_Geo_potencial_dec)

Base_Dados_Geo_Rio_esc = Base_Dados_Geo_Rio %>%
  mutate(potencial_n = as.numeric(potencial)) %>%
  select(codigo:estado,popDe25a34,popDe35a49,domiciliosA1:domiciliosB2,rendaMedia,faturamento,potencial_n)


# Escalonamento
Base_Dados_Geo_Rio_esc[,5:12] = scale(Base_Dados_Geo_Rio_esc[,5:12])


#Separando base de São Paulo para estimar o Potencial depois
Base_Dados_Geo_poten_sampa = Base_Dados_Geo_sampa %>%
  mutate(potencial_n = 0) %>%
  select(codigo:estado,popDe25a34,popDe35a49,domiciliosA1:domiciliosB2,rendaMedia,faturamento,potencial_n)


#separando Base treino e Base Teste
set.seed(1)
divisao = sample.split(Base_Dados_Geo_Rio_esc$potencial_n, SplitRatio = 0.80)
Base_Dados_Geo_Rio_treino = subset(Base_Dados_Geo_Rio_esc, divisao == TRUE)
Base_Dados_Geo_Rio_teste = subset(Base_Dados_Geo_Rio_esc, divisao == FALSE)

###criando modelo de classificação - redes neurais h2o
h2o.init(nthreads = -1)
classificador = h2o.deeplearning(y = 'potencial_n',
                                 training_frame = as.h2o(Base_Dados_Geo_Rio_treino),
                                 activation = 'Rectifier',
                                 hidden = c(200,200,200,200),
                                 epochs = 4000)

previsoes = h2o.predict(classificador, newdata = as.h2o(Base_Dados_Geo_Rio_teste[-13]))

previsoes = round(previsoes, digits = 0)
previsoes = as.vector(previsoes)
matriz_confusao = table(Base_Dados_Geo_Rio_teste[,13], previsoes)

print(matriz_confusao)

print(confusionMatrix(matriz_confusao))

#O modelo de classificação h2o.deeplearning trouxe um resultado satisfatório.
#Com acurácia de ~96.8%, isso quer dizer que a cada 100 previsões feitas pelo modelo,
#vamos ter ~3 erros. Na matriz de confusão na proporção 80/20 tivemos apenas um erro. 
#Isso nos indicada que a interpretalidade do modelo se ajustou muito bem a base de dados.


#estimando potencial dos bairros de São Paulo
# Escalonamento
Base_Dados_Geo_poten_sampa[,5:12] = scale(Base_Dados_Geo_poten_sampa[,5:12])

previsoes = h2o.predict(classificador, newdata = as.h2o(Base_Dados_Geo_poten_sampa[5:12]))

previsoes = round(previsoes, digits = 0)

Base_Dados_Geo_sampa$potencial = as.vector(previsoes)


#trazendo nome da categoria potencial 
Base_Dados_Geo_potencial_dec = Base_Dados_Geo_potencial_dec %>%
  mutate(potencial_nome = potencial) %>%
  mutate(potencial = potencial_n) %>%
  select(potencial,potencial_nome)

Base_Dados_Geo_sampa = merge(Base_Dados_Geo_sampa,Base_Dados_Geo_potencial_dec, by="potencial")

Base_Dados_Geo_sampa = Base_Dados_Geo_sampa %>%
  select(codigo:potencial_nome) %>%
  mutate(potencial = potencial_nome) %>%
  select(codigo:faturamento,potencial)

print(Base_Dados_Geo_sampa)
summary(Base_Dados_Geo_sampa)


#unindo as base de dados do Rio de Janeiro e São Paulo

#Base_Dados_Geo_Rio
#Base_Dados_Geo_sampa

Base_Dados_Geo_final = rbind(Base_Dados_Geo_Rio,Base_Dados_Geo_sampa)

print(Base_Dados_Geo_final)

#Com a Renda Média, Faturamento e Potencial estimados, a etapa de modelagem está encerrada

#Agora vamos Segmentar os bairros de São Paulo de acordo com a renda e a
#idade, e indicar aqueles com maior aderência ao público alvo.

#podemos obter seguimentação do público alvo de maneira bem simples.
#Primeiro precisamos saber qual a fatia da poupulação de 25 a 50anos
#tem em relação a população total. O mesmo em relação a classe de domicílios. 

#Segmentando população e domicílios com o Público Alvo (PA)
#somando grupos 
Base_Dados_Geo_final = Base_Dados_Geo_final %>%
  mutate(Total_pop_PA = popDe25a34+popDe35a49) %>%
  mutate(Total_dom_PA = domiciliosA1+domiciliosA2+domiciliosB1+domiciliosB2)

#Criando segmento grupos
Base_Dados_Geo_final = Base_Dados_Geo_final %>%
  mutate(segmento_pop = round(Total_pop_PA/populacao, digits = 2)) %>%
  mutate(segmento_dom = round(Total_dom_PA/Total_Domicilios, digits = 2))

#Com essa proporção dos dois grupos tiramos a razão entre os dois para saber
#qual é o bairro possui o indice maior.

#Criando indice de segmentação Público alvo
Base_Dados_Geo_final = Base_Dados_Geo_final %>%
  mutate(indice_publico_alvo = round((segmento_pop+segmento_dom)/2,digits = 2))

#ordenando bairros de São Paulo
Base_Dados_Geo_segpa = Base_Dados_Geo_final %>%
  filter(estado == "SP")

Base_Dados_Geo_maiores = Base_Dados_Geo_segpa %>%
  group_by(codigo,cidade,nome,indice_publico_alvo) %>%
  summarise(qtde=n())

Base_Dados_Geo_maiores = Base_Dados_Geo_maiores[order(Base_Dados_Geo_maiores$indice_publico_alvo, decreasing = TRUE),]

#Pegando os 25 bairros com os maiores indices
Base_Dados_Geo_10maiores = Base_Dados_Geo_maiores[1:10,-5]

#Extraindo_base_dados_final-----------------------------------

write.table(Base_Dados_Geo_final, file= "C:/Users/isacc/OneDrive/Área de Trabalho/Jaike/Meu novo emprego/Case_Geofusion/Base_Dados_Geo_final.csv", sep=';', dec=',', row.names=FALSE)
