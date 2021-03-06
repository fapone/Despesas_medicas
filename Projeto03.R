# Exercicio 03 - Prevendo despesas Hospitalares

# Inicia biblioteca
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("dplyr")
library(ggplot2)
library(ggthemes)
library(dplyr)


# Realiza a importa��o do arquivo CSV, contendo dados hospitalares dos pacientes do plano de saude.
df <- read.csv("C:/RFundamentos/Parte7/despesas.csv")

# Inicia a an�lise explanat�ria dos dados
head(df)
str(df)
summary(df)
any(is.na(df))



ggplot(df, aes(x = gastos)) + 
  geom_histogram(bins = 20, 
                 alpha = 0.5, fill = 'blue') + 
  theme_minimal()



# *** Fim da an�lise explanat�ria


# ** Inicio de Machine Learning *** 

# Treinando modelo de regre��o linear, atraves da fun��o lm
install.packages("caTools")
library(caTools)

# Cria as amostras
set.seed(101)
amostra <- sample.split(df$gastos, SplitRatio = 0.70)

# Cria��o dos dados de treino e dados de teste

# Dados de treino - 70% dos dados
treino = subset(df, amostra == TRUE)
head(treino)
str(treino)

# Dados de teste - 30% dos dados
teste = subset(df, amostra == FALSE)
head(teste)
str(teste)

# Gerando o Modelo (Usando todos os atributos)
model1 <- lm(gastos ~ ., treino)
summary(model1)

# ** Fazendo previsoes no modelo
prev1 <- predict(model1, teste)

# Visualizando os valores previstos e observados
resultados <- cbind(prev1, teste$gastos) 
colnames(resultados) <- c('Previsto','Real')
resultados <- as.data.frame(resultados)
resultados
min(resultados)
max(resultados)

# Calculando o erro médio
# Quão distantes seus valores previstos estão dos valores observados
# MSE
mse <- mean((resultados$Real - resultados$Previsto)^2)
print(mse)

# RMSE
rmse <- mse^0.5
rmse

# Calculando R Squared
SSE = sum((resultados$Previsto - resultados$Real)^2)
SST = sum( (mean(df$gastos) - resultados$Real)^2)

# R-Squared
# Ajuda a avaliar o n�?vel de precisão do nosso modelo. Quanto maior, melhor, sendo 1 o valor ideal.
R2 = 1 - (SSE/SST)
R2



