library(tidyverse)

setwd("C:\\Users\\gusta\\OneDrive\\FEARP\\2º semestre\\APS\\prova\\dados")

ptrf <- read.csv("ptrfprova.csv", sep = ";", row.names = 1)

glimpse(ptrf)
summary(ptrf)
head(ptrf)

# objetivo: verificar se maiores gastos impactam o desempenho dos alunos
# (nıvel e variacao)

# histograma para avaliar possivel manipulacao do cut off
ggplot(ptrf) +
  aes(x = matriculas.totais.ens.fund.2005) +
  geom_histogram(bins = 50) +
  labs(
    x = "Matrículas totais",
    y = " ",
    title = "Histograma de matrículas totais (2005)",
    subtitle = "bins = 50"
  ) +
  theme_classic() +
  geom_vline(xintercept = 800, linetype="dashed", 
             color = "red", size=1.5)
