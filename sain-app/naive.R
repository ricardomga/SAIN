source("preparacao_dados.R")
source("geral.R")
library(rminer)
library(DMwR)
library(e1071)
set.seed(12345)

naive_bayes_desbalanceados <- function(dataset) {
  # primeiros 70% do dataset para treino ultimos 30% para teste
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_treino <- dataset[holdout$tr, ]
  dados_teste <- dataset[holdout$ts, ]
  naive_bayes <- naiveBayes(Y ~ ., data = dados_treino)
  previsoes <- predict(naive_bayes, dados_teste, type = "raw")
  metrics(dados_teste$Y, previsoes, "NAIVE BAYES Desbalanceados")
}

naive_bayes_smote <- function(dataset) {
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_treino <- dataset[holdout$tr, ]
  dados_teste <- dataset[holdout$ts, ]
  smote <- SMOTE(Y ~ ., dados_treino, perc.over = 250, k = 10, perc.under = 150)
  naive_bayes <- naiveBayes(Y ~ ., data = smote)
  previsoes <- predict(naive_bayes, dados_teste, type = "raw")
  metrics(dados_teste$Y, previsoes, "NAIVE BAYES SMOTE")
}

naive_bayes_oversampling <- function(dataset) {
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_teste <- dataset[holdout$ts, ]
  dados_treino <- dataset[holdout$tr, ]
  oversampling <- oversampling(dados_treino, dataset)
  naive_bayes <- naiveBayes(Y ~ ., data = oversampling)
  previsoes <- predict(naive_bayes, dados_teste, type = "raw")
  metrics(dados_teste$Y, previsoes, "NAIVE BAYES Oversampling")
}

naive_bayes_undersampling <- function(dataset) {
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_teste <- dataset[holdout$ts, ]
  dados_treino <- dataset[holdout$tr, ]
  undersampling <- undersampling(dados_treino, dataset)
  naive_bayes <- naiveBayes(Y ~ ., data = undersampling)
  previsoes <- predict(naive_bayes, dados_teste, type = "raw")
  metrics(dados_teste$Y, previsoes, "NAIVE BAYES Undersampling")
}

naive_bayes_rolling_desbalanceados <- function(dataset) {
  reais <- character()
  previsoes <- matrix(ncol = 2)
  iteracoes <- 30
  for (i in 1:iteracoes){
    holdout <- holdout(
      dataset$Y,
      ratio = 200,
      mode = "rolling",
      iter = i,
      window = 3000,
      increment = 60)
    dados_treino <-  dataset[holdout$tr, ]
    dados_teste <- dataset[holdout$ts, ]
    naive_bayes <- naiveBayes(Y ~ ., data = dados_treino)

    if (i == 1)
      previsoes <- predict(naive_bayes, dados_teste, type = "raw")
    else
      previsoes <- rbind(previsoes, predict(naive_bayes, dados_teste, type = "raw"))

    reais <- c(reais, as.character(dados_teste$Y))
  }
  reais <- factor(reais)
  metrics(reais, previsoes, "NAIVE BAYES Rolling Desbalanceados")
}

naive_bayes_rolling_smote <- function(dataset) {
  reais <- character()
  previsoes <- matrix(ncol = 2)
  iteracoes <- 30
  for (i in 1:iteracoes){
    holdout <- holdout(
      dataset$Y,
      ratio = 200,
      mode = "rolling",
      iter = i,
      window = 3000,
      increment = 60)
    dados_treino <-  dataset[holdout$tr, ]
    dados_teste <- dataset[holdout$ts, ]
    s <- SMOTE(Y ~ ., dados_treino, perc.over = 250, k = 10, perc.under = 150)
    naive_bayes <- naiveBayes(Y ~ ., data = s)

    if (i == 1)
      previsoes <- predict(naive_bayes, dados_teste, type = "raw")
    else
      previsoes <- rbind(previsoes, predict(naive_bayes, dados_teste, type = "raw"))

    reais <- c(reais, as.character(dados_teste$Y))
  }
  reais <- factor(reais)
  metrics(reais, previsoes, "NAIVE BAYES Rolling SMOTE")
}

naive_bayes_rolling_oversampling <- function(dataset) {
  reais <- character()
  previsoes <- matrix(ncol = 2)
  iteracoes <- 30
  for (i in 1:iteracoes){
    holdout <- holdout(
      dataset$Y,
      ratio = 200,
      mode = "rolling",
      iter = i,
      window = 3000,
      increment = 60)
    dados_teste <- dataset[holdout$ts, ]
    dados_treino <- dataset[holdout$tr, ]
    oversampling <- oversampling(dados_treino, dataset)
    naive_bayes <- naiveBayes(Y ~ ., data = oversampling)

    if (i == 1)
      previsoes <- predict(naive_bayes, dados_teste, type = "raw")
    else
      previsoes <- rbind(previsoes, predict(naive_bayes, dados_teste, type = "raw"))

    reais <- c(reais, as.character(dados_teste$Y))
  }
  reais <- factor(reais)
  metrics(reais, previsoes, "NAIVE BAYES Rolling Oversampling")
}

naive_bayes_rolling_undersampling <- function(dataset) {
  reais <- character()
  previsoes <- matrix(ncol = 2)
  iteracoes <- 30
  for (i in 1:iteracoes){
    holdout <- holdout(
      dataset$Y,
      ratio = 200,
      mode = "rolling",
      iter = i,
      window = 3000,
      increment = 60)
    dados_teste <- dataset[holdout$ts, ]
    dados_treino <- dataset[holdout$tr, ]
    undersampling <- undersampling(dados_treino, dataset)
    naive_bayes <- naiveBayes(Y ~ ., data = undersampling)

    if (i == 1)
      previsoes <- predict(naive_bayes, dados_teste, type = "raw")
    else
      previsoes <- rbind(previsoes, predict(naive_bayes, dados_teste, type = "raw"))

    reais <- c(reais, as.character(dataset[holdout$ts, ]$Y))
  }
  reais <- factor(reais)
  metrics(reais, previsoes, "NAIVE BAYES Rolling Undersampling")
}

naive_oversampling_produtivo <- function(dataset, tempo, n_clientes) {
  dados_treino <- dataset[1: (tempo + 1), ]
  oversampling <- oversampling(dados_treino, dataset)
  naive_bayes <- naiveBayes(Y ~ ., data = oversampling)
  dados_a_prever <- dataset[(tempo + 1) : (tempo + n_clientes), ]
  dados_a_prever$naive <- predict(naive_bayes, dados_a_prever, type = "raw")
  dados_a_prever$naive <- as.vector(dados_a_prever$naive[, 2])
  dados_a_prever$naive[dados_a_prever$naive < 0.5] <- 0
  dados_a_prever$naive[dados_a_prever$naive >= 0.5] <- 1
  dados_a_prever
}
