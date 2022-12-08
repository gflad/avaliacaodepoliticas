library(tidyverse)
library(broom)
library(rdd)
library(rddensity)
library(rddtools)

#a

setwd("C:\\Users\\gusta\\OneDrive\\FEARP\\2º semestre\\APS\\prova\\dados")

ptrf <- read.csv("ptrfprova.csv", sep = ";", row.names = 1)

glimpse(ptrf)
summary(ptrf)
head(ptrf)

# objetivo: verificar se maiores gastos impactam o desempenho dos alunos
# (nıvel e variacao)

# passo 2.1 Skrovon e Titiunik

# histograma para avaliar possivel manipulacao do cut off
ggplot(ptrf) +
  aes(x = matriculas.totais.ens.fund.2005) +
  geom_histogram(bins = 30L,
                 fill = "#0C4C8A",
                 col = "grey") +
                 #binwidth = 2) +
  labs(
    x = "Matrículas totais",
    y = " ") +
    #title = "Histograma de matrículas totais (2005)") +
  theme_classic() +
  geom_vline(xintercept = 800, linetype="solid", 
             color = "red", size=1)


# passo 2.2 Skrovon e Titiunik

# grafico 1: matriculas totais x notas matematica
ptrf %>% 
  select(matriculas.totais.ens.fund.2005, mat.4.s.p.bra.2007) %>% 
  mutate(D = as.factor(ifelse(matriculas.totais.ens.fund.2005 >= 800, 1, 0))) %>% 
  ggplot(aes(x = matriculas.totais.ens.fund.2005,
             y = mat.4.s.p.bra.2007,
             color = D)) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_vline(xintercept = 800, size = 1) + 
  labs(x = "Matrículas totais (2005)",
       y = "Nota de matemática (2007)") +
  theme_classic() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

# grafico 2: matriculas totais x notas portugues
ptrf %>% 
  select(matriculas.totais.ens.fund.2005, port.4.s.p.bra.2007) %>% 
  mutate(D = as.factor(ifelse(matriculas.totais.ens.fund.2005 >= 800, 1, 0))) %>% 
  ggplot(aes(x = matriculas.totais.ens.fund.2005,
             y = port.4.s.p.bra.2007,
             color = D)) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_vline(xintercept = 800, size = 1) + 
  labs(x = "Matrículas totais (2005)",
       y = "Nota de português (2007)") +
  theme_classic() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

# grafico 3: matriculas totais x variacao nas notas matematica
ptrf %>% 
  select(matriculas.totais.ens.fund.2005, variacao.4.s.pb.mat) %>% 
  mutate(D = as.factor(ifelse(matriculas.totais.ens.fund.2005 >= 800, 1, 0))) %>% 
  ggplot(aes(x = matriculas.totais.ens.fund.2005,
             y = variacao.4.s.pb.mat,
             color = D)) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_vline(xintercept = 800, size = 1) + 
  labs(x = "Matrículas totais (2005)",
       y = "Variação das notas de matemática") +
  theme_classic() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

# grafico 4: matriculas totais x variacao nas notas portugues
ptrf %>% 
  select(matriculas.totais.ens.fund.2005, variacao.4.s.pb.port) %>% 
  mutate(D = as.factor(ifelse(matriculas.totais.ens.fund.2005 >= 800, 1, 0))) %>% 
  ggplot(aes(x = matriculas.totais.ens.fund.2005,
             y = variacao.4.s.pb.port,
             color = D)) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_vline(xintercept = 800, size = 1) + 
  labs(x = "Matrículas totais (2005)",
       y = "Variação das notas de português") +
  theme_classic() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))


# passo 2.3: density tests, both graphically and with formal statistical tests.

# run McCrary (2008) version of the test
# library(rdd)

# Give it the running variable and the cutpoint
# it will automatically produce a plot and select the number of bins and the bandwidth
# The output will be the p-value for the presence of a discontinuity
DCdensity(ptrf$matriculas.totais.ens.fund.2005, c = 800)

## Run the Cattaneo, Jansson, and Ma (2020) estimator
# library(rddensity)

# Give it the running variable and the cutoff
# It will pick the bandwidth, and has default polynomials, kernel, and bias correction
# It doesn't have bins to pick
denstest <- rddensity(ptrf$matriculas.totais.ens.fund.2005, c = 800)
summary(denstest)

# Now plot the density discontinuity
# It needs the density test object we just made
rdplotdensity(denstest, ptrf$matriculas.totais.ens.fund.2005)

# passo 2.4: Look at the treatment effect on important predetermined covariates 
# and, if available, placebo outcomes.

lm_rdd_nonoutcome = rdd::RDestimate(sal.med.nom.total.seade.2002 ~ matriculas.totais.ens.fund.2005, ptrf,
                                    cutpoint = 800)
summary(lm_rdd_nonoutcome)

# passo 3: After successful falsification, analyze the outcome of interest, 
# both graphically via an RD plot and formally with appropriate 
# estimation and inference methods.

# estimacao linear nao parametrica, recomendado por Skovron e Titiunik
# notas de matematica:
ptrf2 = rdd_data(y = mat.4.s.p.bra.2007,
                 x = matriculas.totais.ens.fund.2005,
                 data = ptrf,
                 cutpoint = 800)

npll_mat = rdd_reg_np(ptrf2)
npll_mat

# notas de portugues:
ptrf2 = rdd_data(y = port.4.s.p.bra.2007,
                 x = matriculas.totais.ens.fund.2005,
                 data = ptrf,
                 cutpoint = 800)

npll_port = rdd_reg_np(ptrf2)
npll_port

# variacao notas de matematica:
ptrf2 = rdd_data(y = variacao.4.s.pb.mat,
                 x = matriculas.totais.ens.fund.2005,
                 data = ptrf,
                 cutpoint = 800)

npll_var_mat = rdd_reg_np(ptrf2)
npll_var_mat

# variacao notas de portugues:
ptrf2 = rdd_data(y = variacao.4.s.pb.port,
                 x = matriculas.totais.ens.fund.2005,
                 data = ptrf,
                 cutpoint = 800)

npll_var_port = rdd_reg_np(ptrf2)
npll_var_port
