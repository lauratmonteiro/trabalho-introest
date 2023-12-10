library("tidyverse")
library("dplyr")
library("ggplot2")
library("readr")
library("expss")
library("fdth")
install.packages("moments")
library("moments")

data <- read_csv("Base4.csv")

# ANÁLISE UNIVARIADA

# Variável Satisfação
data$Satisfacao <- factor(data$Satisfacao, levels = c("Neutro ou Insatisfeito", "Satisfeito"))
data %>% select("Satisfacao") %>% fre()

ggplot(data = data, mapping = aes(x = Satisfacao)) + geom_bar() + labs(x = "Satisfação", y = "Frequência") + theme_bw()
ggplot(data = data, mapping = aes(x = Satisfacao, y = after_stat(prop),group = 1)) + geom_bar() +
  scale_y_continuous(labels = scales::percent_format()) + theme_bw() +
  xlab("Satisfação") + ylab("Frequência Relativa (%)")

# Variável Gênero
data$Genero <- factor(data$Genero, levels = c("Masculino", "Feminino"))
data %>% select("Genero") %>% fre()

ggplot(data = data, mapping = aes(x = Genero)) + geom_bar() + labs(x = "Gênero", y = "Frequência") + theme_bw()
ggplot(data = data, mapping = aes(x = Genero, y = after_stat(prop),group = 1)) + geom_bar() +
  scale_y_continuous(labels = scales::percent_format()) + theme_bw() +
  xlab("Gênero") + ylab("Frequência Relativa (%)")

unique(data$Tipo)
data$Tipo <- factor(data$Tipo, levels = c("Cliente Fiel", "Cliente Nao Fiel"))
data %>% select("Tipo") %>% fre()

# Variável WiFi
data$WiFi <- factor(data$WiFi, levels = c(0, 1, 2, 3, 4, 5),
    labels = c("Não se aplica", "Muito Insatisfeito", "Insatisfeito", "Neutro", "Satisfeito", "Muito Satisfeito"))
data %>% select("WiFi") %>% fre()

ggplot(data = data, mapping = aes(x = WiFi)) + geom_bar() + labs(x = "Nível de satisfação com o WiFi", y = "Frequência") + theme_bw()
ggplot(data = data, mapping = aes(x = WiFi, y = after_stat(prop),group = 1)) + geom_bar() +
  scale_y_continuous(labels = scales::percent_format()) + theme_bw() +
  xlab("Nível de satisfação com o WiFi") + ylab("Frequência Relativa (%)")

# Variável Comida_Bebida
data$Comida_Bebida <- factor(data$Comida_Bebida, levels = c(0, 1, 2, 3, 4, 5),
    labels = c("Não se aplica", "Muito Insatisfeito", "Insatisfeito", "Neutro", "Satisfeito", "Muito Satisfeito"))
data %>% select("Comida_Bebida") %>% fre()

ggplot(data = data, mapping = aes(x = Comida_Bebida)) + geom_bar() + labs(x = "Nível de satisfação com a comida e bebida", y = "Frequência") + theme_bw()
ggplot(data = data, mapping = aes(x = Comida_Bebida, y = after_stat(prop),group = 1)) + geom_bar() +
  scale_y_continuous(labels = scales::percent_format()) + theme_bw() +
  xlab("Nível de satisfação com a comida e bebida") + ylab("Frequência Relativa (%)")

# Variável Limpeza
data$Limpeza <- factor(data$Limpeza, levels = c(0, 1, 2, 3, 4, 5),
    labels = c("Não se aplica", "Muito Insatisfeito", "Insatisfeito", "Neutro", "Satisfeito", "Muito Satisfeito"))
data %>% select("Limpeza") %>% fre()

ggplot(data = data, mapping = aes(x = Limpeza)) + geom_bar() + labs(x = "Nível de satisfação com a limpeza", y = "Frequência") + theme_bw()
ggplot(data = data, mapping = aes(x = Limpeza, y = after_stat(prop),group = 1)) + geom_bar() +
  scale_y_continuous(labels = scales::percent_format()) + theme_bw() +
  xlab("Nível de satisfação com a limpeza") + ylab("Frequência Relativa (%)")

# Variável Distância
data %>% select("Distancia") %>% fdt(breaks="Sturges") %>% summary()

data %>% summarise(Media = mean(Distancia),
                    Mediana = median(Distancia),
                    Desvio_Padrao = sd(Distancia))

data %>% summarise(Minimo = min(Distancia),
                    Maximo = max(Distancia),
                    Q_1 = quantile(Distancia,prob=0.25),
                    Q_2 = quantile(Distancia,prob=0.50),
                    Q_3 = quantile(Distancia,prob=0.75))

data %>% summarise(Assimetria = skewness(Distancia),
                    Curtose= kurtosis(Distancia)-3)

breaks = pretty(range(data$Distancia),
                 n = nclass.Sturges(data$Distancia),
                 min.n = 1)

ggplot(data = data, mapping = aes(x = Distancia)) + geom_histogram(breaks=breaks) +
  theme_bw() + xlab("Distância") + ylab("Frequencia")

ggplot(data = data, mapping = aes(x = Distancia, y=" ")) + geom_boxplot(fill="grey") +
  theme_bw() + xlab("Distância") + ylab(" ")

#  ANÁLISE BIVARIADA

# Variável Satisfação e Gênero

freq_by_cases_sg <- data %>% cross_cases(Satisfacao, Genero)
freq_by_cases_sg

prop_by_row_sg <- data %>% cross_rpct(Satisfacao, Genero, total_row_position = "none")
prop_by_row_sg

prop_by_col_sg <- data %>% cross_cpct(Satisfacao, Genero, total_row_position = "none")
prop_by_col_sg

# TODO: Variáveis Satisfação e Tipo

# Variáveis Satisfação e WiFi

freq_by_cases_sw <- data %>% cross_cases(Satisfacao, WiFi)
freq_by_cases_sw
