library("tidyverse")
library("dplyr")

dados <- read_csv(file = "Base4.csv")

total = nrow(dados)
total

qtd_satisfeitos <- length(which(dados["Satisfacao"] == "Satisfeito"))

qtd_neutros_insatisfeitos <-
  length(which(dados["Satisfacao"] == "Neutro ou Insatisfeito"))

satisfacao <- data.frame(name = c("Satisfeitos", "Neutros ou Insatisfeitos"),
                         value = c(qtd_satisfeitos, qtd_neutros_insatisfeitos))
satisfacao

satisfacao %>%
  ggplot(aes(x = reorder(name, value), y = value)) +
  geom_bar(stat = "identity", width = 0.3, color = "blue", fill = rgb(0.1, 0.4, 0.5, 0.7)) + # nolint: line_length_linter.
  ggtitle("Satisfação dos Clientes") + theme(plot.title = element_text(hjust = 0.5)) + # nolint: line_length_linter.
  xlab("Grau de Satisfação") + ylab("Quantidade")

freq_relativa <- data.frame(name = c("Satisfeitos", "Neutros ou Insatisfeitos"),
                            value = c((qtd_satisfeitos / total), # nolint: line_length_linter.
                                      (qtd_neutros_insatisfeitos / total))) # nolint: line_length_linter.
freq_relativa

freq_relativa %>%
  ggplot(aes(x = reorder(name, value), y = value)) +
  geom_bar(stat = "identity", width = 0.3, color = "blue", fill = rgb(0.1, 0.4, 0.5, 0.7)) + # nolint: line_length_linter.
  ggtitle("Satisfação dos Clientes") + theme(plot.title = element_text(hjust = 0.5)) + # nolint: line_length_linter.
  xlab("Grau de Satisfação") + ylab("Frequência Relativa")