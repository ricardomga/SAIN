source("preparacao_dados.R")
source("geral.R")
library(rminer)
library(DMwR)
library(randomForest)
set.seed(12345)

random_forest_desbalanceados <- function(dataset) {
  # primeiros 70% do dataset para treino ultimos 30% para teste
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_treino <- dataset[holdout$tr, ]
  dados_teste <- dataset[holdout$ts, ]
  random_forest <- randomForest(Y ~ ., data = dados_treino)
  previsoes <- predict(random_forest, dados_teste, type = "prob")
  metrics(dados_teste$Y, previsoes, "RANDOM FOREST Desbalanceados")
}

random_forest_smote <- function(dataset) {
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_treino <- dataset[holdout$tr, ]
  dados_teste <- dataset[holdout$ts, ]
  smote <- SMOTE(Y ~ ., dados_treino, perc.over = 250, k = 10, perc.under = 150)
  random_forest <- randomForest(Y ~ ., data = smote)
  previsoes <- predict(random_forest, dados_teste, type = "prob")
  metrics(dados_teste$Y, previsoes, "RANDOM FOREST SMOTE")
}

random_forest_oversampling <- function(dataset) {
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_teste <- dataset[holdout$ts, ]
  dados_treino <- dataset[holdout$tr, ]
  oversampling <- oversampling(dados_treino, dataset)
  random_forest <- randomForest(Y ~ ., data = oversampling)
  previsoes <- predict(random_forest, dados_teste, type = "prob")
  metrics(dados_teste$Y, previsoes, "RANDOM FOREST Oversampling")
}

random_forest_undersampling <- function(dataset) {
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_teste <- dataset[holdout$ts, ]
  dados_treino <- dataset[holdout$tr, ]
  undersampling <- undersampling(dados_treino, dataset)
  random_forest <- randomForest(Y ~ ., data = undersampling)
  previsoes <- predict(random_forest, dados_teste, type = "prob")
  metrics(dados_teste$Y, previsoes, "RANDOM FOREST Undersampling")
}

random_forest_rolling_desbalanceados <- function(dataset) {
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
    random_forest <- randomForest(Y ~ ., data = dados_treino)

    if (i == 1)
      previsoes <- predict(random_forest, dados_teste, type = "prob")
    else
      previsoes <- rbind(previsoes, predict(random_forest, dados_teste, type = "prob"))

    reais <- c(reais, as.character(dados_teste$Y))
    }
  reais <- factor(reais)
  metrics(reais, previsoes, "RANDOM FOREST Rolling Desbalanceados")
}

random_forest_rolling_smote <- function(dataset) {
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
    random_forest <- randomForest(Y ~ ., data = s)

    if (i == 1)
      previsoes <- predict(random_forest, dados_teste, type = "prob")
    else
      previsoes <- rbind(previsoes, predict(random_forest, dados_teste, type = "prob"))

    reais <- c(reais, as.character(dados_teste$Y))
  }
  reais <- factor(reais)
  metrics(reais, previsoes, "RANDOM FOREST Rolling SMOTE")
}

random_forest_rolling_oversampling <- function(dataset) {
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
    random_forest <- randomForest(Y ~ ., data = oversampling)

    if (i == 1)
      previsoes <- predict(random_forest, dados_teste, type = "prob")
    else
      previsoes <- rbind(previsoes, predict(random_forest, dados_teste, type = "prob"))

    reais <- c(reais, as.character(dados_teste$Y))
  }
  reais <- factor(reais)
  metrics(reais, previsoes, "RANDOM FOREST Rolling Oversampling")
}

random_forest_rolling_undersampling <- function(dataset) {
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
    random_forest <- randomForest(Y ~ ., data = undersampling)

    if (i == 1)
      previsoes <- predict(random_forest, dados_teste, type = "prob")
    else
      previsoes <- rbind(previsoes, predict(random_forest, dados_teste, type = "prob"))

    reais <- c(reais, as.character(dataset[holdout$ts, ]$Y))
  }
  reais <- factor(reais)
  metrics(reais, previsoes, "RANDOM FOREST Rolling Undersampling")
}



random_forest_undersampling_produtivo <- function(dataset, tempo, n_clientes) {
  dados_treino <- dataset[1: (tempo + 1), ]
  undersampling <- undersampling(dados_treino, dataset)
  random_forest <- randomForest(Y ~ ., data = undersampling)
  dados_a_prever <- dataset[(tempo + 1) : (tempo + n_clientes), ]
  dados_a_prever$random_forest <- predict(random_forest, dados_a_prever, type = "prob")
  dados_a_prever$random_forest <- as.vector(dados_a_prever$random_forest[, 2])
  dados_a_prever$random_forest[dados_a_prever$random_forest < 0.5] <- 0
  dados_a_prever$random_forest[dados_a_prever$random_forest >= 0.5] <- 1
  dados_a_prever
}




