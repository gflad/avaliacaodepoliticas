library(readr)
library(tidyverse)

setwd("C:\\Users\\gusta\\OneDrive\\FEARP\\2º semestre\\APS\\prova\\dados")

BF <- read_dta("prova_aps_2022_BF.dta")

########################################################
################## AJUSTANDO OS DADOS ##################
########################################################

# vetor de variaveis de identificacao
identificacao <- c("UPA", "Estrato", "V1008", "V1014")
# vetor de variaveis confounders
covariadas <- c("V2007", "V1022", "V2009", "V2010", "V5002A",
                "V5002A2","VD3004", "VD4001", "VD4002", "VD4017", "VD5005")
# variaveis extras criadas
extras <- c("dom_rec_bf", "total_bf", "bf_pc")

# selecionando colunas
dados <- BF %>% 
  # somente em idade ativa (14 a 65 anos [ou 60?])
  filter(V2009 %in% c(14:65)) %>%
  select(identificacao, covariadas, extras)

# renomeando colunas
dados <- dados %>% 
  rename(urbano = V1022,
         mulher = V2007,
         idade = V2009,
         negro = V2010,
         recebeu_bf = V5002A,
         valor_recebido_bf = V5002A2,
         educacao = VD3004,
         economicamente_ativa = VD4001,
         ocupada = VD4002,
         renda = VD4017,
         renda_pc = VD5005)

# redefinindo valores
dados <- dados %>% 
  mutate(urbano = ifelse(urbano == 1, 1, 0),
         mulher = ifelse(mulher == 2, 1, 0),
         negro = ifelse(negro %in% c(2, 3, 4, 5), 1, 0),
         recebeu_bf = ifelse(recebeu_bf == 1, 1, 0),
         economicamente_ativa = ifelse(economicamente_ativa == 1, 1, 0),
         ocupada = ifelse(ocupada == 1, 1, 0),
         # variavel de renda familiar pc sem o beneficio
         renda_pc_sem_bf = renda_pc - bf_pc)


########################################################
################## ANALISES INICIAIS ###################
########################################################

dados %>% 
  filter(mulher == 1) %>% 
  group_by(dom_rec_bf, recebeu_bf, mulher) %>% 
  summarise(beneficiarios = n(),
            media_economicamente_ativa = mean(economicamente_ativa),
            erro_padrao = sd(economicamente_ativa)/sqrt(beneficiarios))

# p-valor <0.05, entao a diferenca entre os grupos e significativa
t.test(dados$economicamente_ativa ~ dados$dom_rec_bf)

# analisando as diferencas nas covariadas
dados %>% 
  group_by(dom_rec_bf) %>% 
  select(one_of(c("urbano", "negro", "educacao", "mulher"))) %>% 
  summarise_all(funs(mean(., na.rm = T)))
