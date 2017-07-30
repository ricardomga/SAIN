source("preparacao_dados.R")
source("geral.R")
library(rminer)
library(DMwR)
set.seed(12345)

mlp_desbalanceados <- function(dataset) {
    holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
    dados_treino <- dataset[holdout$tr, ]
    dados_teste <- dataset[holdout$ts, ]
    mlp <- fit(Y~., data = dados_treino, model = "mlp", search = "heuristic")
    previsoes <- predict(mlp, dados_teste, type = "prob")
    metrics(dados_teste$Y, previsoes, "MLP Desbalanceados")
}

mlp_smote <- function(dataset) {
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_treino <- dataset[holdout$tr, ]
  dados_teste <- dataset[holdout$ts, ]
  smote <- SMOTE(Y ~ ., dados_treino, perc.over = 250, k = 10, perc.under = 150)
  mlp <- fit(Y ~ ., data = smote, model = "mlp", search = "heuristic")
  previsoes <- predict(mlp, dados_teste, type = "prob")
  metrics(dados_teste$Y, previsoes, "MLP SMOTE")
}

mlp_oversampling <- function(dataset) {
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_teste <- dataset[holdout$ts, ]
  dados_treino <- dataset[holdout$tr, ]
  oversampling <- oversampling(dados_treino, dataset)
  mlp <- fit(Y ~ ., data = oversampling, model = "mlp", search = "heuristic")
  previsoes <- predict(mlp, dados_teste, type = "prob")
  metrics(dados_teste$Y, previsoes, "MLP Oversampling")
}

mlp_undersampling <- function(dataset) {
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_teste <- dataset[holdout$ts, ]
  dados_treino <- dataset[holdout$tr, ]
  undersampling <- undersampling(dados_treino, dataset)
  mlp <- fit(Y ~ ., data = undersampling, model = "mlp", search = "heuristic")
  previsoes <- predict(mlp, dados_teste, type = "prob")
  metrics(dados_teste$Y, previsoes, "MLP Undersampling")
}

mlp_rolling_desbalanceados <- function(dataset) {
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
    mlp <- fit(Y~., data = dados_treino, model = "mlp", search = "heuristic")

    if (i == 1)
      previsoes <- predict(mlp, dados_teste, type = "prob")
    else
      previsoes <- rbind(previsoes, predict(mlp, dados_teste, type = "prob"))

    reais <- c(reais, as.character(dados_teste$Y))
  }
  reais <- factor(reais)
  # previsoes ou reais estãoa  dar problemas
  metrics(reais, previsoes, "MLP Rolling Desbalanceados")
}

mlp_rolling_smote <- function(dataset) {
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
    mlp <- fit(Y ~ ., data = s, model = "mlp", search = "heuristic")

    if (i == 1)
      previsoes <- predict(mlp, dados_teste, type = "prob")
    else
      previsoes <- rbind(previsoes, predict(mlp, dados_teste, type = "prob"))

    reais <- c(reais, as.character(dados_teste$Y))
  }
  reais <- factor(reais)
  metrics(reais, previsoes, "MLP Rolling SMOTE")
}

mlp_rolling_oversampling <- function(dataset) {
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
    oversampling <- oversampling(holdout, dataset)
    mlp <- fit(Y~., data = oversampling, model = "mlp", search = "heuristic")

    if (i == 1)
      previsoes <- predict(mlp, dados_teste, type = "prob")
    else
      previsoes <- rbind(previsoes, predict(mlp, dados_teste, type = "prob"))

    reais <- c(reais, as.character(dados_teste$Y))
  }
  reais <- factor(reais)
  metrics(reais, previsoes, "MLP Rolling Oversampling")
}

mlp_rolling_undersampling <- function(dataset) {
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
    mlp <- fit(Y~., data = undersampling, model = "mlp", search = "heuristic")

    if (i == 1)
      previsoes <- predict(mlp, dados_teste, type = "prob")
    else
      previsoes <- rbind(previsoes, predict(mlp, dados_teste, type = "prob"))

    reais <- c(reais, as.character(dataset[holdout$ts, ]$Y))
  }
  reais <- factor(reais)
  metrics(reais, previsoes, "MLP Rolling Undersampling")
}


mlp_smote_produtivo <- function(dataset, tempo, n_clientes) {
  dados_treino <- dataset[1: (tempo + 1), ]
  smote <- SMOTE(Y ~ ., dados_treino, perc.over = 250, k = 10, perc.under = 150)
  mlp <- fit(Y ~ ., data = smote, model = "mlp", search = "heuristic")
  dados_a_prever <- dataset[(tempo + 1) : (tempo + n_clientes), ]
  dados_a_prever$mlp <- predict(mlp, dados_a_prever, type = "prob")
  dados_a_prever$mlp <- as.vector(dados_a_prever$mlp[, 2])
  dados_a_prever$mlp[dados_a_prever$mlp < 0.5] <- 0
  dados_a_prever$mlp[dados_a_prever$mlp >= 0.5] <- 1
  dados_a_prever
}
