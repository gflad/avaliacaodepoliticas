require(haven)
require(tidyverse)
library(stargazer)

mogno <- read_dta(file = "C:\\Users\\gusta\\OneDrive\\FEARP\\2º semestre\\APS\\prova\\dados\\banco_mogno_prova.dta")

# analise exploratoria rapida para entender o dataset

glimpse(mogno)
summary(mogno)

# qtde de municipios
length(unique(mogno$municip))

# qtde de tratados e controle
table(mogno$mahog_area)

table(mogno$year)


#### primeiro modelo sem "dividir" o tempo pos tratamento
# criando dummy pre tratamento = 0 e pos tratamento = 1
mogno <- mutate(mogno,
                pos_1999 = ifelse(year < 1999, 0, 1))

# estimacao via mqo
modelo1 <- lm(hom_tx ~ mahog_area + pos_1999 + mahog_area*pos_1999,
              data = mogno)

summary(modelo1)
#stargazer(modelo1,type = "text")


#### segundo modelo dividindo o tempo pos tratamento em 3 periodos
# criando dummies de periodos
mogno <- mutate(mogno,
                anos_1999_2001 = ifelse(year >= 1999 & year <= 2001, 1, 0),
                anos_2002_2008 = ifelse(year >= 2002 & year <= 2008, 1, 0),
                pos_2009 = ifelse(year >= 2009, 1, 0))

# estimacao via mqo
modelo2 <- lm(hom_tx ~ mahog_area + anos_1999_2001 + anos_2002_2008 + pos_2009
              + mahog_area*anos_1999_2001 + mahog_area*anos_2002_2008 +
                mahog_area*pos_2009,
              data = mogno)

summary(modelo2)
#stargazer(modelo2, type = "text")

stargazer(modelo1, modelo2, type = "latex")
