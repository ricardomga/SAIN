source("preparacao_dados.R")
source("geral.R")
library(rminer)
library(DMwR)
set.seed(12345)

glm_desbalanceados <- function(dataset) {
  # primeiros 70% do dataset para treino ultimos 30% para teste
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_treino <- dataset[holdout$tr, ]
  dados_teste <- dataset[holdout$ts, ]
  glm <- glm(Y ~ ., family = binomial(link = "logit"), data = dados_treino)
  previsoes <- predict(glm, dados_teste, type = "response")
  metrics_glm(dados_teste$Y, previsoes, "GLM Desbalanceados")
}

glm_smote <- function(dataset) {
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_treino <- dataset[holdout$tr, ]
  dados_teste <- dataset[holdout$ts, ]
  smote <- SMOTE(Y ~ ., dados_treino, perc.over = 250, k = 10, perc.under = 150)
  glm <- glm(Y ~ ., family = binomial(link = "logit"), data = smote)
  previsoes <- predict.glm(glm, dados_teste, type = "response")
  metrics_glm(dados_teste$Y, previsoes, "GLM SMOTE")
}

glm_oversampling <- function(dataset) {
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_teste <- dataset[holdout$ts, ]
  dados_treino <- dataset[holdout$tr, ]
  oversampling <- oversampling(dados_treino, dataset)
  glm <- glm(Y ~ ., family = binomial(link = "logit"), data = oversampling)
  previsoes <- predict.glm(glm, dados_teste, type = "response")
  metrics_glm(dados_teste$Y, previsoes, "GLM Oversampling")
}

glm_undersampling <- function(dataset) {
  holdout <- holdout(dataset$Y, ratio = 0.7, mode = "order")
  dados_teste <- dataset[holdout$ts, ]
  dados_treino <- dataset[holdout$tr, ]
  undersampling <- undersampling(dados_treino, dataset)
  glm <- glm(Y ~ ., family = binomial(link = "logit"), data = undersampling)
  previsoes <- predict.glm(glm, dados_teste, type = "response")
  metrics_glm(dados_teste$Y, previsoes, "GLM Undersampling")
}

glm_rolling_desbalanceados <- function(dataset) {
  reais <- character()
  previsoes <- numeric()
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
    glm <- glm(Y ~ ., family = binomial(link = "logit"), data = dados_treino)
    previsoes <- c(previsoes, predict.glm(glm, dados_teste, type = "response"))
    reais <- c(reais, as.character(dados_teste$Y))
  }
  reais <- factor(reais)
  metrics_glm(reais, previsoes, "GLM Rolling Desbalanceados")
}

glm_rolling_smote <- function(dataset) {
  reais <- character()
  previsoes <- numeric()
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
    glm <- glm(Y ~ ., family = binomial(link = "logit"), data = s)
    previsoes <- c(previsoes, predict.glm(glm, dados_teste, type = "response"))
    reais <- c(reais, as.character(dados_teste$Y))
  }
  reais <- factor(reais)
  metrics_glm(reais, previsoes, "GLM Rolling SMOTE")
}

glm_rolling_oversampling <- function(dataset) {
  reais <- character()
  previsoes <- numeric()
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
    mrl <- glm(Y ~ ., family = binomial(link = "logit"), data = oversampling)
    previsoes <- c(previsoes, predict.glm(mrl, dados_teste, type = "response"))
    reais <- c(reais, as.character(dados_teste$Y))
  }
  reais <- factor(reais)
  metrics_glm(reais, previsoes, "GLM Rolling Oversampling")
}

glm_rolling_undersampling <- function(dataset) {
  reais <- character()
  previsoes <- numeric()
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
    glm <- glm(Y ~ ., family = binomial(link = "logit"), data = undersampling)
    previsoes <- c(previsoes, predict.glm(glm, dados_teste, type = "response"))
    reais <- c(reais, as.character(dataset[holdout$ts, ]$Y))
  }
  reais <- factor(reais)
  metrics_glm(reais, previsoes, "GLM Rolling Undersampling")
}


glm_smote_produtivo <- function(dataset, tempo, n_clientes) {
  dados_treino <- dataset[1: (tempo + 1), ]
  smote <- SMOTE(Y ~ ., dados_treino, perc.over = 250, k = 10, perc.under = 150)
  glm <- glm(Y ~ ., family = binomial(link = "logit"), data = smote)
  dados_a_prever <- dataset[(tempo + 1) : (tempo + n_clientes), ]
  dados_a_prever$glm <- predict.glm(glm, dados_a_prever, type = "response")
  dados_a_prever$glm[dados_a_prever$glm >= 0.5] <- 1
  dados_a_prever$glm[dados_a_prever$glm < 0.5] <- 0
  dados_a_prever
}










