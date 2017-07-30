source("preparacao_dados.R")
source("geral.R")
library(rminer)
library(DMwR)
library(randomForest)
set.seed(12345)

ksvm_desbalanceados <- function(dataset) {
  # primeiros 70% do dataset para treino ultimos 30% para teste
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_treino <- dataset[holdout$tr, ]
  dados_teste <- dataset[holdout$ts, ]
  ksvm <- fit(Y ~ .,data = dados_treino, model = "ksvm")
  previsoesksvm <- predict(ksvm, dados_teste)
  metrics(dados_teste$Y, previsoesksvm, "KSVM Desbalanceados")
}

ksvm_smote <- function(dataset) {
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_treino <- dataset[holdout$tr, ]
  dados_teste <- dataset[holdout$ts, ]
  smote <- SMOTE(Y ~ ., dados_treino, perc.over = 250, k = 10, perc.under = 150)
  ksvm <- fit(Y ~ ., data = smote, model = "ksvm")
  previsoesksvm <- predict(ksvm, dados_teste)
  metrics(dados_teste$Y, previsoesksvm, "KSVM SMOTE")
}

ksvm_oversampling <- function(dataset) {
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_teste <- dataset[holdout$ts, ]
  dados_treino <- dataset[holdout$tr, ]
  oversampling <- oversampling(dados_treino, dataset)
  ksvm <- fit(Y ~ ., data = oversampling, model = "ksvm")
  previsoesksvm <- predict(ksvm, dados_teste)
  metrics(dados_teste$Y, previsoesksvm, "KSVM Oversampling")
}

ksvm_undersampling <- function(dataset) {
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_teste <- dataset[holdout$ts, ]
  dados_treino <- dataset[holdout$tr, ]
  undersampling <- undersampling(dados_treino, dataset)
  ksvm <- fit(Y ~ ., data = undersampling, model = "ksvm")
  previsoesksvm <- predict(ksvm, dados_teste)
  metrics(dados_teste$Y, previsoesksvm, "KSVM Undersampling")
}

ksvm_rolling_desbalanceados <- function(dataset) {
  reais <- character()
  previsoesksvm <- matrix(ncol = 2)
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
    ksvm <- fit(Y ~ ., data = dados_treino, model = "ksvm")

    if (i == 1)
      previsoesksvm <- predict(ksvm, dados_teste)
    else
      previsoesksvm <- rbind(previsoesksvm, predict(ksvm, dados_teste))

    reais <- c(reais, as.character(dados_teste$Y))
  }
  reais <- factor(reais)
  metrics(reais, previsoesksvm, "KSVM Rolling Desbalanceados")
}

ksvm_rolling_smote <- function(dataset) {
  reais <- character()
  previsoesksvm <- matrix(ncol = 2)
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
    ksvm <- fit(Y ~ ., data = s, model = "ksvm")

    if (i == 1)
      previsoesksvm <- predict(ksvm, dados_teste)
    else
      previsoesksvm <- rbind(previsoesksvm, predict(ksvm, dados_teste))

    reais <- c(reais, as.character(dados_teste$Y))
  }
  reais <- factor(reais)
  metrics(reais, previsoesksvm, "KSVM Rolling SMOTE")
}

ksvm_rolling_oversampling <- function(dataset) {
  reais <- character()
  previsoesksvm <- matrix(ncol = 2)
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
    ksvm <- fit(Y ~ ., data = oversampling, model = "ksvm")

    if (i == 1)
      previsoesksvm <- predict(ksvm, dados_teste)
    else
      previsoesksvm <- rbind(previsoesksvm, predict(ksvm, dados_teste))

    reais <- c(reais, as.character(dados_teste$Y))
  }
  reais <- factor(reais)
  metrics(reais, previsoesksvm, "KSVM Rolling Oversampling")
}

ksvm_rolling_undersampling <- function(dataset) {
  reais <- character()
  previsoesksvm <- matrix(ncol = 2)
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
    ksvm <- fit(Y ~ ., data = undersampling, model = "ksvm")

    if (i == 1)
      previsoesksvm <- predict(ksvm, dados_teste)
    else
      previsoesksvm <- rbind(previsoesksvm, predict(ksvm, dados_teste))

    reais <- c(reais, as.character(dados_teste$Y))
  }
  reais <- factor(reais)
  metrics(reais, previsoesksvm, "KSVM Rolling Undersampling")
}


ksvm_oversampling_produtivo <- function(dataset, tempo, n_clientes) {
  dados_treino <- dataset[1: (tempo + 1), ]
  oversampling <- oversampling(dados_treino, dataset)
  ksvm <- fit(Y ~ ., data = oversampling, model = "ksvm")
  dados_a_prever <- dataset[(tempo + 1) : (tempo + n_clientes), ]
  dados_a_prever$ksvm <- predict(ksvm, dados_a_prever)
  dados_a_prever$ksvm <- as.vector(dados_a_prever$ksvm[, 2])
  dados_a_prever$ksvm[dados_a_prever$ksvm < 0.5] <- 0
  dados_a_prever$ksvm[dados_a_prever$ksvm >= 0.5] <- 1
  dados_a_prever
}
