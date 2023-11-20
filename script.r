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

satisfacao <- as.data.frame(satisfacao)
satisfacao <- rename(satisfacao, "Satisfacao" = "Var1", "Freq" = "Freq")
satisfacao

freq_relativa_satisfeitos <- as.data.frame(freq_relativa_satisfeitos)
freq_relativa_satisfeitos <- rename(freq_relativa_satisfeitos, "Satisfacao" = "Var1", "Freq" = "Freq") # nolint: line_length_linter.
freq_relativa_satisfeitos

satisfacao %>%
  ggplot(aes(x = Satisfacao, y = Freq)) +
  geom_bar(stat = "identity", fill = rgb(0.1, 0.4, 0.5, 0.7), width = 0.5) +
  labs(title = "Satisfação dos clientes", x = "Satisfação", y = "Frequência") +
  theme(plot.title = element_text(hjust = 0.5))

freq_relativa_satisfeitos %>%
  ggplot(aes(x = Satisfacao, y = Freq)) +
  geom_bar(stat = "identity", fill = rgb(0.1, 0.4, 0.5, 0.7), width = 0.5) +
  labs(title = "Satisfação dos clientes", x = "Satisfação", y = "Frequência Relativa") + # nolint: line_length_linter.
  theme(plot.title = element_text(hjust = 0.5))

# Análise da variável gênero

genero <- table(dados$Genero)
genero

freq_relativa_genero <- prop.table(genero)
freq_relativa_genero

genero <- as.data.frame(genero)
genero <- rename(genero, "Genero" = "Var1", "Freq" = "Freq")

freq_relativa_genero <- as.data.frame(freq_relativa_genero)
freq_relativa_genero <- rename(freq_relativa_genero, "Genero" = "Var1", "Freq" = "Freq") # nolint: line_length_linter.

genero %>%
  ggplot(aes(x = Genero, y = Freq)) +
  geom_bar(stat = "identity", fill = rgb(0.1, 0.4, 0.5, 0.7), width = 0.5) +
  labs(title = "Gênero dos clientes", x = "Gênero", y = "Frequência") +
  theme(plot.title = element_text(hjust = 0.5))

freq_relativa_genero %>%
    ggplot(aes(x = Genero, y = Freq)) +
    geom_bar(stat = "identity", fill = rgb(0.1, 0.4, 0.5, 0.7), width = 0.5) +
    labs(title = "Gênero dos clientes", x = "Gênero", y = "Frequência Relativa") + # nolint: line_length_linter.
    theme(plot.title = element_text(hjust = 0.5))

# Análise da variável tipo

tipo <- table(dados$Tipo)
tipo

freq_relativa_tipo <- prop.table(tipo)
freq_relativa_tipo

tipo <- as.data.frame(tipo)
tipo <- rename(tipo, "Tipo" = "Var1", "Freq" = "Freq")

freq_relativa_tipo <- as.data.frame(freq_relativa_tipo)
freq_relativa_tipo <- rename(freq_relativa_tipo, "Tipo" = "Var1", "Freq" = "Freq") # nolint: line_length_linter.

tipo %>%
  ggplot(aes(x = Tipo, y = Freq)) +
  geom_bar(stat = "identity", fill = rgb(0.1, 0.4, 0.5, 0.7), width = 0.5) +
  labs(title = "Tipo de cliente", x = "Tipo", y = "Frequência") +
  theme(plot.title = element_text(hjust = 0.5))

freq_relativa_tipo %>%
    ggplot(aes(x = Tipo, y = Freq)) +
    geom_bar(stat = "identity", fill = rgb(0.1, 0.4, 0.5, 0.7), width = 0.5) +
    labs(title = "Tipo de cliente", x = "Tipo", y = "Frequência Relativa") + # nolint: line_length_linter.
    theme(plot.title = element_text(hjust = 0.5))

# Análise da variável WiFi

wifi <- table(dados$WiFi)
wifi

freq_relativa_wifi <- prop.table(wifi)
freq_relativa_wifi

wifi <- as.data.frame(wifi)
wifi <- rename(wifi, "WiFi" = "Var1", "Freq" = "Freq")

freq_relativa_wifi <- as.data.frame(freq_relativa_wifi)
freq_relativa_wifi <- rename(freq_relativa_wifi, "WiFi" = "Var1", "Freq" = "Freq") # nolint: line_length_linter.

wifi %>%
  ggplot(aes(x = WiFi, y = Freq)) +
  geom_bar(stat = "identity", fill = rgb(0.1, 0.4, 0.5, 0.7), width = 0.5) +
  labs(title = "WiFi", x = "WiFi", y = "Frequência") +
  theme(plot.title = element_text(hjust = 0.5))

freq_relativa_wifi %>%
    ggplot(aes(x = WiFi, y = Freq)) +
    geom_bar(stat = "identity", fill = rgb(0.1, 0.4, 0.5, 0.7), width = 0.5) +
    labs(title = "WiFi", x = "WiFi", y = "Frequência Relativa") + # nolint: line_length_linter.
    theme(plot.title = element_text(hjust = 0.5))

# Análise da variável Comida_Bebida

alimentacao <- table(dados$Comida_Bebida)
alimentacao

freq_relativa_alimentacao <- prop.table(alimentacao)
freq_relativa_alimentacao

alimentacao <- as.data.frame(alimentacao)
alimentacao <- rename(alimentacao, "Comida_Bebida" = "Var1", "Freq" = "Freq")

freq_relativa_alimentacao <- as.data.frame(freq_relativa_alimentacao)
freq_relativa_alimentacao <- rename(freq_relativa_alimentacao, "Comida_Bebida" = "Var1", "Freq" = "Freq") # nolint: line_length_linter.

alimentacao %>%
  ggplot(aes(x = Comida_Bebida, y = Freq)) +
  geom_bar(stat = "identity", fill = rgb(0.1, 0.4, 0.5, 0.7), width = 0.5) +
  labs(title = "Comida e bebida", x = "Comida e bebida", y = "Frequência") +
  theme(plot.title = element_text(hjust = 0.5))

freq_relativa_alimentacao %>%
    ggplot(aes(x = Comida_Bebida, y = Freq)) +
    geom_bar(stat = "identity", fill = rgb(0.1, 0.4, 0.5, 0.7), width = 0.5) +
    labs(title = "Comida e bebida", x = "Comida e bebida", y = "Frequência Relativa") + # nolint: line_length_linter.
    theme(plot.title = element_text(hjust = 0.5))

# Análise da variável Limpeza

limpeza <- table(dados$Limpeza)
limpeza

freq_relativa_limpeza <- prop.table(limpeza)
freq_relativa_limpeza

limpeza <- as.data.frame(limpeza)
limpeza <- rename(limpeza, "Limpeza" = "Var1", "Freq" = "Freq")

freq_relativa_limpeza <- as.data.frame(freq_relativa_limpeza)
freq_relativa_limpeza <- rename(freq_relativa_limpeza, "Limpeza" = "Var1", "Freq" = "Freq") # nolint: line_length_linter.

limpeza %>%
  ggplot(aes(x = Limpeza, y = Freq)) +
  geom_bar(stat = "identity", fill = rgb(0.1, 0.4, 0.5, 0.7), width = 0.5) +
  labs(title = "Limpeza", x = "Limpeza", y = "Frequência") +
  theme(plot.title = element_text(hjust = 0.5))

# Análise da variável Distância

n_classes <- nclass.Sturges(dados$Distancia)
maior_distancia <- max(dados$Distancia)
menor_distancia <- min(dados$Distancia)

amplitude <- ceiling((maior_distancia - menor_distancia) / n_classes)
amplitude


classe_inf <- min(dados$Distancia)
classe_sup <- classe_inf + (amplitude * n_classes)

intervalos <- seq(classe_inf, classe_sup, by = amplitude) # nolint: line_length_linter.
intervalos

distancia <- table(cut(dados$Distancia, breaks = intervalos, right = FALSE))
distancia

freq_relativa_distancia <- prop.table(distancia)
freq_relativa_distancia

distancia <- as.data.frame(distancia)
distancia <- rename(distancia, "Distancia" = "Var1", "Freq" = "Freq")
distancia

freq_relativa_distancia <- as.data.frame(freq_relativa_distancia)
freq_relativa_distancia <- rename(freq_relativa_distancia, "Distancia" = "Var1", "Freq" = "Freq") # nolint: line_length_linter.

median(dados$Distancia)
mean(dados$Distancia)
sd(dados$Distancia)