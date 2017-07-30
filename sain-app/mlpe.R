source("preparacao_dados.R")
source("geral.R")
library(rminer)
library(DMwR)
library(nnet)
set.seed(12345)

mlpe_desbalanceados <- function(dataset) {
    holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
    dados_treino <- dataset[holdout$tr, ]
    dados_teste <- dataset[holdout$ts, ]
    mlpe <- fit(Y~., data = dados_treino, model = "mlpe", search = "heuristic")
    previsoes <- predict(mlpe, dados_teste, type = "prob")
    metrics(dados_teste$Y, previsoes, "MLPE Desbalanceados")
}

mlpe_smote <- function(dataset) {
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_treino <- dataset[holdout$tr, ]
  dados_teste <- dataset[holdout$ts, ]
  smote <- SMOTE(Y ~ ., dados_treino, perc.over = 250, k = 10, perc.under = 150)
  mlpe <- fit(Y ~ ., data = smote, model = "mlpe", search = "heuristic")
  previsoes <- predict(mlpe, dados_teste, type = "prob")
  metrics(dados_teste$Y, previsoes, "MLPE SMOTE")
}

mlpe_oversampling <- function(dataset) {
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_teste <- dataset[holdout$ts, ]
  dados_treino <- dataset[holdout$tr, ]
  oversampling <- oversampling(dados_treino, dataset)
  mlpe <- fit(Y ~ ., data = oversampling, model = "mlpe", search = "heuristic")
  previsoes <- predict(mlpe, dados_teste, type = "prob")
  metrics(dados_teste$Y, previsoes, "MPLE Oversampling")
}

mlpe_undersampling <- function(dataset) {
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_teste <- dataset[holdout$ts, ]
  dados_treino <- dataset[holdout$tr, ]
  undersampling <- undersampling(dados_treino, dataset)
  mlpe <- fit(Y ~ ., data = undersampling, model = "mlpe", search = "heuristic")
  previsoes <- predict(mlpe, dados_teste, type = "prob")
  metrics(dados_teste$Y, previsoes, "MLPE Undersampling")
}

mlpe_rolling_desbalanceados <- function(dataset) {
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
    mlpe <- fit(Y~., data = dados_treino, model = "mlpe", search = "heuristic")

    if (i == 1)
      previsoes <- predict(mlpe, dados_teste, type = "prob")
    else
      previsoes <- rbind(previsoes, predict(mlpe, dados_teste, type = "prob"))

    reais <- c(reais, as.character(dados_teste$Y))
  }
  reais <- factor(reais)
  # previsoes ou reais estãoa  dar problemas
  metrics(reais, previsoes, "MLPE Rolling Desbalanceados")
}

mlpe_rolling_smote <- function(dataset) {
  reais <- character()
  previsoes <- matrix()
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
    mlpe <- fit(Y ~ ., data = s, model = "mlpe", search = "heuristic")

    if (i == 1)
      previsoes <- predict(mlpe, dados_teste, type = "prob")
    else
      previsoes <- rbind(previsoes, predict(mlpe, dados_teste, type = "prob"))

    reais <- c(reais, as.character(dados_teste$Y))
  }
  reais <- factor(reais)
  metrics(reais, previsoes, "MLPE Rolling SMOTE")
}

mlpe_rolling_oversampling <- function(dataset) {
  reais <- character()
  previsoes <- matrix()
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
    mlpe <- fit(Y~., data = oversampling, model = "mlpe", search = "heuristic")

    if (i == 1)
      previsoes <- predict(mlpe, dados_teste, type = "prob")
    else
      previsoes <- rbind(previsoes, predict(mlpe, dados_teste, type = "prob"))

    reais <- c(reais, as.character(dados_teste$Y))
  }
  reais <- factor(reais)
  metrics(reais, previsoes, "MLPE Rolling Oversampling")
}

mlpe_rolling_undersampling <- function(dataset) {
  reais <- character()
  previsoes <- matrix()
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
    mlpe <- fit(Y~., data = undersampling, model = "mlpe", search = "heuristic")

    if (i == 1)
      previsoes <- predict(mlpe, dados_teste, type = "prob")
    else
      previsoes <- rbind(previsoes, predict(mlpe, dados_teste, type = "prob"))

    reais <- c(reais, as.character(dataset[holdout$ts, ]$Y))
  }
  reais <- factor(reais)
  metrics(reais, previsoes, "MLPE Rolling Undersampling")
}


mlpe_smote_produtivo <- function(dataset, tempo, n_clientes) {
  dados_treino <- dataset[1: (tempo + 1), ]
  smote <- SMOTE(Y ~ ., dados_treino, perc.over = 250, k = 10, perc.under = 150)
  mlpe <- fit(Y ~ ., data = smote, model = "mlpe", search = "heuristic")
  dados_a_prever <- dataset[(tempo + 1) : (tempo + n_clientes), ]
  dados_a_prever$mlpe <- predict(mlpe, dados_a_prever, type = "prob")
  dados_a_prever$mlpe <- as.vector(dados_a_prever$mlpe[, 2])
  dados_a_prever$mlpe[dados_a_prever$mlpe < 0.5] <- 0
  dados_a_prever$mlpe[dados_a_prever$mlpe >= 0.5] <- 1
  dados_a_prever
}
