source("funcao_avaliacao.R")

repair <- function(solucao){
  if (min(solucao) < 1)
    solucao[which(solucao < 1)] <- 1

  if (max(solucao) > 5)
    solucao[which(solucao > 5)] <- 5

  solucao
}

repair_restricao <- function(solucao, N){
  if (any(is.nan(solucao)))
    solucao[which(solucao %in% NaN)] <- 0

  if (min(solucao) <0 )
    solucao[which(solucao < 0)] <- 0

  if (max(solucao) > 5)
    solucao[which(solucao > 5)] <- 5

  # n numero de zeros a mais 
  n <- length(which(solucao == 0)) - floor(length(solucao) / 2)

  if (n > 0)
    solucao[which(solucao == 0)[1:n]] <- 1

  if (n < 0)
    solucao[which(solucao != 0)[1:abs(n)]] <- 0

  solucao
}


eval_min_dp <- function(solucao){
  solucao <- round(solucao)
  if (min(solucao) < 1 || max(solucao) > 5) return(Inf)
  -profit(solucao)
}

eval_max_dp <- function(solucao){
  solucao <- round(solucao)
  if (min(solucao) < 1 || max(solucao) > 5) return(-Inf)
  profit(solucao)
}

eval_min_dp_restricao <- function(solucao){
  solucao <- round(solucao)
  if ((min(solucao) < 0 || max(solucao) > 5) || length(which(solucao == 0)) != 5) return(Inf)
  -profit(solucao)
}

eval_max_dp_restricao <- function(solucao){
  solucao <- round(solucao)
  if ((min(solucao) < 0 || max(solucao) > 5) || length(which(solucao == 0)) != 5) return(-Inf)
  profit(solucao)
}

eval_min_repair <- function(solucao){
  solucao <- round(solucao)
  solucao <- repair(solucao)
  -profit(solucao)
}

eval_max_repair <- function(solucao){
  solucao <- round(solucao)
  solucao <- repair(solucao)
  profit(solucao)
}

eval_min_repair_restricao <- function(solucao){
  solucao <- round(solucao)
  solucao <- repair_restricao(solucao)
  -profit(solucao)
}

eval_max_repair_restricao <- function(solucao){
  solucao <- round(solucao)
  solucao <- repair_restricao(solucao)
  profit(solucao)
}