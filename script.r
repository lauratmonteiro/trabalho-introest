library("tidyverse")
library("dplyr")
library("ggplot2")

dados <- read_csv(file = "Base4.csv")

# ANÁLISE UNIVARIADA

# Análise da variável satisfação

satisfacao <- table(dados$Satisfacao)
satisfacao

freq_relativa_satisfeitos <- prop.table(satisfacao)
freq_relativa_satisfeitos

# Análise da variável gênero

genero <- table(dados$Genero)
genero

freq_relativa_genero <- prop.table(genero)
freq_relativa_genero

# Análise da variável tipo

tipo <- table(dados$Tipo)
tipo

freq_relativa_tipo <- prop.table(tipo)
freq_relativa_tipo

# Análise da variável WiFi

wifi <- table(dados$WiFi)
wifi

freq_relativa_wifi <- prop.table(wifi)
freq_relativa_wifi

# Análise da variável Comida_Bebida

alimentacao <- table(dados$Comida_Bebida)
alimentacao

freq_relativa_alimentacao <- prop.table(alimentacao)
freq_relativa_alimentacao

# Análise da variável Limpeza

limpeza <- table(dados$Limpeza)
limpeza

freq_relativa_limpeza <- prop.table(limpeza)
freq_relativa_limpeza

# Análise da variável Distância

n_classes <- nclass.Sturges(dados$Distancia)
maior_distancia <- max(dados$Distancia)
menor_distancia <- min(dados$Distancia)

amplitude <- ceiling((maior_distancia - menor_distancia) / n_classes)
amplitude

classe_inf <- min(dados$Distancia)
classe_sup <- classe_inf + (amplitude * n_classes)

intervalos <- seq(classe_inf, classe_sup, by = amplitude)
intervalos

distancia <- table(cut(dados$Distancia, breaks = intervalos, right = FALSE))
distancia # FIXME: TÁ COM ERROOO

freq_relativa_distancia <- prop.table(distancia)
freq_relativa_distancia

median(dados$Distancia)
mean(dados$Distancia)
sd(dados$Distancia)