oversampling <- function(dados_treino, dataset){
    incump <- dados_treino[which(dados_treino$Y == 1), ]
    cumprimentos <- dados_treino[which(dados_treino$Y == 0), ]
    n <- floor(nrow(cumprimentos) / nrow(incump))
    incump_rep <- do.call("rbind", replicate(n, incump, simplify = FALSE))
    rbind(incump_rep, cumprimentos)
}

undersampling <- function(dados_treino, dataset){
  incumprimentos <- dados_treino[which(dados_treino$Y == 1), ]
  n_incump <- length(which(dados_treino$Y == 1))
  amostra_cumprimentos <- dados_treino[sample(which(dados_treino$Y == 0), n_incump), ]
  rbind(amostra_cumprimentos, incumprimentos)
}

metrics_glm <- function(reais, previsoes, nome){
  mgraph(reais, previsoes, "ROC", main = nome, baseline = TRUE, Grid = 10)
  metrics <- mmetric(
    reais,
    previsoes,
    metric = c("CONF", "ACC", "PRECISION", "CE", "TNR", "TPR"))

  metrics$res["AUC"] <- mmetric(
    reais,
    matrix(c(1 - previsoes, previsoes), ncol = 2),
    metric = "AUC")

  print(metrics$conf)
  print(metrics$res)
  metrics$res
}

metrics <- function(reais, previsoes, nome){
  mgraph(reais, previsoes, "ROC", main = nome, baseline = TRUE, Grid = 10)
  metrics <- mmetric(
    reais,
    previsoes,
    metric = c("CONF", "ACC", "PRECISION", "CE", "TNR", "TPR"))

  metrics$res["AUC"] <- mmetric(
    reais,
    previsoes,
    metric = "AUC")

  print(metrics$conf)
  print(metrics$res)
  metrics$res
}