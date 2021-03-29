install.packages("dplyr")
library(dplyr)

# Carregando os dados
?read.csv

viagens <- read.csv(
  file = "/2019_Viagem.csv",
  sep = ';',
  dec = ','
)

#apresentando os dados do dataset nas tabelas presentes


head(viagens)

View(viagens)


dim(viagens)
?summary
summary(viagens)
summary(viagens$Valor.passagens)

#verificando os tipos de dados do dataset

glimpse(viagens)

#Transformação dos dados obtidos

#convertendo campo para tipo de data
?as.Date

viagens$data.inicio <- as.Date(viagens$Período...Data.de.início,
                               "%d/%m/%Y")
glimpse(viagens)

#convertendo o campo de data pata mês e ano

viagens$data.inicio.formatada <- format(viagens$data.inicio, "%Y-%m")

viagens$data.inicio.formatada

?format

#Exploração dos dados

#criando histograma de passagens
hist(viagens$Valor.passagens)

#Valores min, max, média.. da coluna valor
summary(viagens$Valor.passagens)

#visualizando os valores em um boxplot
boxplot(viagens$Valor.passagens)

#calculando o desvio padrão
sd(viagens$Valor.passagens)

#verificando valores não preenchidos
?is.na
?colSums

colSums(is.na(viagens))

#verificando a quantidade de ocorrências
str(viagens$Situação)

table(viagens$Situação)

prop.table(table(viagens$Situação))*100

#Visualização dos resultados

# 1. quais orgãos estão gastando mais com passagens aéreas?

#criando um dataframe com os 15 órgãos que gastam mais
p1 <- viagens %>%
  group_by(Nome.do.órgão.superior) %>%
  summarise(n = sum(Valor.passagens)) %>%
  arrange(desc(n)) %>%
  top_n(15)

names(p1) <- c("orgao","valor")

p1

#plotando os dados com o ggplot

library(dplyr)
library(ggplot2)

ggplot(p1,aes(x= reorder(orgao,valor),y= valor))+
  geom_bar(stat ="identity")+
  coord_flip()+
  labs(x="Valor", y="Órgãos")

#criando um dataframe com as cidades que gastam mais
p2 <- viagens %>%
  group_by(Destinos) %>%
  summarise(n = sum(Valor.passagens)) %>%
  arrange(desc(n)) %>%
  top_n(15)

names(p2) <- c("destino","valor")

#plotando os dados com o ggplot

library(dplyr)
library(ggplot2)

ggplot(p2,aes(x= reorder(destino,valor),y= valor))+
  geom_bar(stat ="identity",fill = "0ba791")+
  geom_text(aes(label=valor), vjust=0.3, size=3)+
  coord_flip()+
  labs(x="Valor", y="Destino")

options(scipen=999)

#criando um dataframe com a quantidade de viagem por mês

p3 <- viagens %>%
  group_by(data.inicio.formatada) %>%
  summarise(qtd = n_distinct(Identificador.do.processo.de.viagem)) 

names(p3)

#plotando gráfico

ggplot(p3, aes(x=data.inicio.formatada, y=qtd, group=1))+
  geom_line()+
  geom_point()

  
