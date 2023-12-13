library("tidyverse")
library("dplyr")
library("ggplot2")
library("readr")
library("expss")
library("fdth")
library("moments")
library("ggmosaic")

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

# Variável tipo
data$Tipo <- factor(data$Tipo, levels = c("Cliente Fiel", "Cliente Nao Fiel"))
data %>% select("Tipo") %>% fre()

ggplot(data = data, mapping = aes(x = Tipo)) + geom_bar() + labs(x = "Tipo", y = "Frequência") + theme_bw()
ggplot(data = data, mapping = aes(x = Tipo, y = after_stat(prop),group = 1)) + geom_bar() +
  scale_y_continuous(labels = scales::percent_format()) + theme_bw() +
  xlab("Tipo") + ylab("Frequência Relativa (%)")

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

data %>% summarise(media = mean(Distancia),
                    mediana = median(Distancia),
                    desvio_padrao = sd(Distancia))

data %>% summarise(minimo = min(Distancia),
                    maximo = max(Distancia),
                    Q1 = quantile(Distancia,prob=0.25),
                    Q2 = quantile(Distancia,prob=0.50),
                    Q3 = quantile(Distancia,prob=0.75))

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

# Variáveis Satisfação e Gênero

freq_by_cases_sg <- data %>% cross_cases(Satisfacao, Genero)
freq_by_cases_sg

prop_by_row_sg <- data %>% cross_rpct(Satisfacao, Genero, total_row_position = "none")
prop_by_row_sg

prop_by_col_sg <- data %>% cross_cpct(Satisfacao, Genero, total_row_position = "none")
prop_by_col_sg

ggplot(data, mapping = aes(x = Genero, fill=Satisfacao)) + 
  geom_bar(position = "fill") + theme_bw() + scale_y_continuous(labels = scales::percent_format()) + xlab("Gênero") + ylab("Frequência Relativa (%)")+scale_fill_discrete(name = "Satisfação")

ggplot(data) + 
  geom_mosaic(aes(x=product(Genero, Satisfacao),fill=Genero)) + theme_bw() + scale_y_continuous(labels = scales::percent_format()) + xlab("Satisfação") + ylab("Frequência Relativa (%)")+scale_fill_discrete(name = "Gênero")

x2_sg = chisq.test(table(data$Genero, data$Satisfacao),correct = FALSE)$statistic
x2_sg

total <- 2500

c_sg = sqrt(x2_sg/(total-1))
c_sg

v_sg = sqrt(x2_sg/((total*min(2,2)-1)))
v_sg

# Variáveis Satisfação e Tipo

freq_by_cases_st <- data %>% cross_cases(Satisfacao, Tipo)
freq_by_cases_st

prop_by_row_st <- data %>% cross_rpct(Satisfacao, Tipo, total_row_position = "none")
prop_by_row_st

prop_by_col_st <- data %>% cross_cpct(Satisfacao, Tipo, total_row_position = "none")
prop_by_col_st

ggplot(data, mapping = aes(x = Tipo, fill=Satisfacao)) + 
  geom_bar(position = "fill") + theme_bw() + scale_y_continuous(labels = scales::percent_format()) + xlab("Tipo") + ylab("Frequência Relativa (%)")+scale_fill_discrete(name = "Satisfação")

ggplot(data) + 
  geom_mosaic(aes(x=product(Tipo, Satisfacao),fill=Tipo)) + theme_bw() + scale_y_continuous(labels = scales::percent_format()) + xlab("Satisfação") + ylab("Frequência Relativa (%)")+scale_fill_discrete(name = "Tipo")

x2_st = chisq.test(table(data$Tipo, data$Satisfacao),correct = FALSE)$statistic
x2_st

c_st = sqrt(x2_st/(total-1))
c_st

v_st = sqrt(x2_st/((total*min(2,2)-1)))
v_st

# Variáveis Satisfação e WiFi

freq_by_cases_sw <- data %>% cross_cases(Satisfacao, WiFi)
freq_by_cases_sw

prop_by_row_sw <- data %>% cross_rpct(Satisfacao, WiFi, total_row_position = "none")
prop_by_row_sw

prop_by_col_sw <- data %>% cross_cpct(Satisfacao, WiFi, total_row_position = "none")
prop_by_col_sw

ggplot(data, mapping = aes(x = WiFi, fill=Satisfacao)) + 
  geom_bar(position = "fill") + theme_bw() + scale_y_continuous(labels = scales::percent_format()) + xlab("Nível de satisfação com o WiFi") + ylab("Frequência Relativa (%)")+scale_fill_discrete(name = "Satisfação")

ggplot(data) +
  geom_mosaic(aes(x=product(WiFi, Satisfacao),fill=WiFi)) + theme_bw() + scale_y_continuous(labels = scales::percent_format()) + xlab("Satisfação") + ylab("Frequência Relativa (%)")+scale_fill_discrete(name = "Nível de satisfação com o WiFi")

x2_sw = chisq.test(table(data$WiFi, data$Satisfacao),correct = FALSE)$statistic
x2_sw

c_sw = sqrt(x2_sw/(total-1))
c_sw

v_sw = sqrt(x2_sw/((total*min(6,2)-1)))
v_sw

# Variáveis Satisfação e Comida_Bebida

freq_by_cases_sc <- data %>% cross_cases(Satisfacao, Comida_Bebida)
freq_by_cases_sc

prop_by_row_sc <- data %>% cross_rpct(Satisfacao, Comida_Bebida, total_row_position = "none")
prop_by_row_sc

prop_by_col_sc <- data %>% cross_cpct(Satisfacao, Comida_Bebida, total_row_position = "none")
prop_by_col_sc

ggplot(data, mapping = aes(x = Comida_Bebida, fill=Satisfacao)) + 
  geom_bar(position = "fill") + theme_bw() + scale_y_continuous(labels = scales::percent_format()) + xlab("Nível de satisfação com a comida e bebida") + ylab("Frequência Relativa (%)")+scale_fill_discrete(name = "Satisfação")

ggplot(data) +
  geom_mosaic(aes(x=product(Comida_Bebida, Satisfacao),fill=Comida_Bebida)) + theme_bw() + scale_y_continuous(labels = scales::percent_format()) + xlab("Satisfação") + ylab("Frequência Relativa (%)")+scale_fill_discrete(name = "Nível de satisfação com a comida e bebida")

x2_sc = chisq.test(table(data$Comida_Bebida, data$Satisfacao),correct = FALSE)$statistic
x2_sc

c_sc = sqrt(x2_sc/(total-1))
c_sc

v_sc = sqrt(x2_sc/((total*min(6,2)-1)))
v_sc

# Variáveis Satisfação e Limpeza

freq_by_cases_sl <- data %>% cross_cases(Satisfacao, Limpeza)
freq_by_cases_sl

prop_by_row_sl <- data %>% cross_rpct(Satisfacao, Limpeza, total_row_position = "none")
prop_by_row_sl

prop_by_col_sl <- data %>% cross_cpct(Satisfacao, Limpeza, total_row_position = "none")
prop_by_col_sl

ggplot(data, mapping = aes(x = Limpeza, fill=Satisfacao)) + 
  geom_bar(position = "fill") + theme_bw() + scale_y_continuous(labels = scales::percent_format()) + xlab("Nível de satisfação com a limpeza") + ylab("Frequência Relativa (%)")+scale_fill_discrete(name = "Satisfação")
ggplot(data) +
  geom_mosaic(aes(x=product(Limpeza, Satisfacao),fill=Limpeza)) + theme_bw() + scale_y_continuous(labels = scales::percent_format()) + xlab("Satisfação") + ylab("Frequência Relativa (%)")+scale_fill_discrete(name = "Nível de satisfação com a limpeza")

x2_sl = chisq.test(table(data$Limpeza, data$Satisfacao),correct = FALSE)$statistic
x2_sl

c_sl = sqrt(x2_sl/(total-1))
c_sl

v_sl = sqrt(x2_sl/((total*min(6,2)-1)))
v_sl

# Variáveis Satisfação e Distância

data %>% 
  select(Satisfacao, Distancia) %>%
  fdt(breaks="Sturges",by="Satisfacao") %>%
  summary()

ggplot(data, mapping = aes(x = Satisfacao, y = Distancia, fill=Satisfacao)) +
  geom_boxplot() + 
  theme_bw()+
  xlab("Satisfação")+
  ylab("Nível de satisfação por distância")+
  theme(legend.position="none")

res_sd <- data %>% group_by(Satisfacao)%>%
           summarise(n=n(),
                    min = min(Distancia),
                    media = mean(Distancia),
                    Q1 = quantile(Distancia, 0.25),
                    mediana = median(Distancia),
                    Q3 = quantile(Distancia, 0.75),
                    max = max(Distancia),
                    desvio_padrao = sd(Distancia),
                    var = var(Distancia),
                    assimetria = skewness(Distancia),
                    cv = (desvio_padrao/media)*100)
res_sd

var <- var(data$Distancia)
var
res_sd$n
res_sd$var
var_b <- sum(res_sd$n*res_sd$var)/sum(res_sd$n)
var_b

r2 <- (1-var_b/var)*100
r2