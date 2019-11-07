##Data scince 
## Dia 06/11/19
## Feito por: Laris, Thiago, e Fabiano
## KNN (K-Nearest Neighbour)


# Pacote manipulacao eficiente dados
library(tidyverse)

# Carrega basede dados
riscoCredito <- read.csv(
  file = "https://www.ufrgs.br/wiki-r/images/c/ca/RiscoCredito.csv",
  header = TRUE,
  sep = ";"
) %>% 
  as.tibble() %>% 
  mutate(
    Inadimplente = as.logical(Inadimplente),
    Genero = factor(Genero),
    GrauEscolaridade = as.factor(GrauEscolaridade),
    RendaMedia = as.double(RendaMedia),
    Profissao = factor(Profissao)
  )

# Visualizar dados
View(riscoCredito)
summary(riscoCredito)


# ----------------------------  Criar base de treino e teste ------------------------

# Apura amostra de 70% das linhas para treino
linhasTreino <- sample.int(
  n = nrow(riscoCredito),
  size = nrow(riscoCredito) * 0.7
)

# Extrai base de dados para treino
treinoRiscoCredito <- riscoCredito %>% 
  slice(linhasTreino)

# Extrai base de dados para teste
testeRiscoCredito <- riscoCredito %>% 
  slice(-linhasTreino)


# --------- ------------------------------ Criar modelo    ------------

# Instala pacote com algoritmos machine learning
#install.packages("caret", dependencies = TRUE)

# Carrega pacote com knn
library(caret)

# Cria modelo Knn
modeloKnn <- knn3(
  formula = Inadimplente ~ Genero + GrauEscolaridade + RendaMedia + Profissao,
  data = treinoRiscoCredito,
  k = 5
)


# -------------------- Testar Modelo -----------------

# Executa predicao do modelo
predicao <- predict(
  object = modeloKnn,
  testeRiscoCredito %>% 
    select(-Inadimplente),
  type = "prob"
) %>% 
  as.tibble() 

# Visualiza resultado
View(predicao)


# ---------------------- Resultado Final (Testado com dataset test) -----------------

# Cria tabela para conferir com gabarito
predicao %>% 
  transmute(Predicao = round(x = `TRUE`, digits = 0)) %>% 
  bind_cols(testeRiscoCredito %>% 
              select(Inadimplente)) %>% 
  mutate(Acertou = (Predicao == Inadimplente)) %>% 
  count(Acertou)