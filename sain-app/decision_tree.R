source("preparacao_dados.R")
source("geral.R")
library(rminer)
library(DMwR)
set.seed(12345)

decision_tree_desbalanceados <- function(dataset) {
  # primeiros 70% do dataset para treino ultimos 30% para teste
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_treino <- dataset[holdout$tr, ]
  dados_teste <- dataset[holdout$ts, ]
  decision_tree <- fit(Y ~ ., dados_treino, model = "dt")
  previsoes <- predict(decision_tree, dados_teste)
  metrics(dados_teste$Y, previsoes, "DECISION TREE Desbalanceados")
}


decision_tree_smote <- function(dataset) {
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_treino <- dataset[holdout$tr, ]
  dados_teste <- dataset[holdout$ts, ]
  smote <- SMOTE(Y ~ ., dados_treino, perc.over = 250, k = 10, perc.under = 150)
  decision_tree <- fit(Y ~ ., smote, model = "dt")
  previsoes <- predict(decision_tree, dados_teste, type = "prob")
  metrics(dados_teste$Y, previsoes, "DECISION TREE SMOTE")
}


decision_tree_oversampling <- function(dataset) {
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_teste <- dataset[holdout$ts, ]
  dados_treino <- dataset[holdout$tr, ]
  oversampling <- oversampling(dados_treino, dataset)
  decision_tree <- fit(Y ~ ., oversampling, model = "dt")
  previsoes <- predict(decision_tree, dados_teste, type = "prob")
  metrics(dados_teste$Y, previsoes, "DECISION TREE Oversampling")
}


decision_tree_undersampling <- function(dataset) {
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_teste <- dataset[holdout$ts, ]
  dados_treino <- dataset[holdout$tr, ]
  undersampling <- undersampling(dados_treino, dataset)
  decision_tree <- fit(Y~ ., undersampling, model = "dt")
  previsoes <- predict(decision_tree, dados_teste, type = "prob")
  metrics(dados_teste$Y, previsoes, "DECISION TREE Undersampling")
}


decision_tree_rolling_desbalanceados <- function(dataset) {
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
    decision_tree <- fit(Y ~ ., dados_treino, model = "dt")

    if (i == 1)
      previsoes <- predict(decision_tree, dados_teste, type = "prob")
    else
      previsoes <- rbind(previsoes, predict(decision_tree, dados_teste, type = "prob"))

    reais <- c(reais, as.character(dados_teste$Y))
    print(i)
  }
  reais <- factor(reais)
  metrics(reais, previsoes, "DECISION TREE Rolling Desbalanceados")
}


decision_tree_rolling_smote <- function(dataset) {
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
    decision_tree <- fit(Y ~ ., s, model = "dt")

    if (i == 1)
      previsoes <- predict(decision_tree, dados_teste, type = "prob")
    else
      previsoes <- rbind(previsoes, predict(decision_tree, dados_teste, type = "prob"))

    reais <- c(reais, as.character(dados_teste$Y))
  }
  reais <- factor(reais)
  metrics(reais, previsoes, "DECISION TREE Rolling SMOTE")
}


decision_tree_rolling_oversampling <- function(dataset) {
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
    decision_tree <- fit(Y ~ ., oversampling, model = "dt")

    if (i == 1)
      previsoes <- predict(decision_tree, dados_teste, type = "prob")
    else
      previsoes <- rbind(previsoes, predict(decision_tree, dados_teste, type = "prob"))

    reais <- c(reais, as.character(dados_teste$Y))
  }
  reais <- factor(reais)
  metrics(reais, previsoes, "DECISION TREE Rolling Oversampling")
}


decision_tree_rolling_undersampling <- function(dataset) {
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
    decision_tree <- fit(Y ~ ., undersampling, model = "dt")

    if (i == 1)
      previsoes <- predict(decision_tree, dados_teste, type = "prob")
    else
      previsoes <- rbind(previsoes, predict(decision_tree, dados_teste, type = "prob"))

    reais <- c(reais, as.character(dataset[holdout$ts, ]$Y))
  }
  reais <- factor(reais)
  metrics(reais, previsoes, "DECISION TREE Rolling Undersampling")
}


decision_tree_undersampling_produtivo <- function(dataset, tempo, n_clientes) {
  dados_treino <- dataset[1: (tempo + 1), ]
  undersampling <- undersampling(dados_treino, dataset)
  decision_tree <- fit(Y~ ., undersampling, model = "dt")
  dados_a_prever <- dataset[(tempo + 1) : (tempo + n_clientes), ]
  dados_a_prever$decision_tree <- predict(decision_tree, dados_a_prever, type = "prob")
  dados_a_prever$decision_tree <- as.vector(dados_a_prever$decision_tree[, 2])
  dados_a_prever$decision_tree[dados_a_prever$decision_tree < 0.5] <- 0
  dados_a_prever$decision_tree[dados_a_prever$decision_tree >= 0.5] <- 1
  dados_a_prever
}

