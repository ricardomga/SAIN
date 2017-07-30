source("preparacao_dados.R")
source("geral_otimizacao.R")
library(tabuSearch)

tempo <- 3500

binary_to_products <- function(solucao){
  sol_bin <- split(solucao, ceiling(seq_along(solucao) / 5))
  produtos <- c();

  for (i in 1:length(sol_bin)){
    if (is.na(match(1, sol_bin[[i]])))
      produto <- 0
    else
      produto <- match(1, sol_bin[[i]])

    produtos <- c(produtos, produto)
  }

  produtos
}


solucao_inicial <- function(N){
  sol <- c()
  for (i in 1:N){
    sol <- c(sol, sample(c(rep(0, 4), 1)))
  }

  sol
}

solucao_inicial_restricao <- function(N){
  sol <- c()
  for (i in 1:floor(N / 2)){
    sol <- c(sol, sample(c(sample(c(rep(0, 4), 1)), rep(0, 5))))
  }
  sol
}

eval_tabu_repair <- function(solucao) {
  solucao <- binary_to_products(solucao)
  solucao <- repair(solucao)
  profit(solucao)
}

eval_tabu_restricao_repair <- function(solucao) {
  solucao <- binary_to_products(solucao)
  solucao <- repair_restricao(solucao)
  profit(solucao)
}

tabu_search_repair <- function(N) {
  melhor_lucro <- -Inf
  for (i in 1:10) {
    solucao_inicial <- solucao_inicial(N)
    solucao <- tabuSearch(50, objFunc = eval_tabu_repair, config = solucao_inicial, iters = 2)
    b <- which.max(solucao$eUtilityKeep)
    lucro <- solucao$eUtilityKeep[b]
    cat("Execucao:", i, " Solucao: ", repair(binary_to_products(solucao$configKeep[b, ])), "Lucro: ", lucro, "\n")
    if (lucro > melhor_lucro) {
      melhor_solucao <- solucao
      melhor_lucro <- lucro
    }
  }
  b <- which.max(melhor_solucao$eUtilityKeep)
  solucao <- binary_to_products(melhor_solucao$configKeep[b, ])
  cat(">>> Melhor Solucao: ", solucao, "Lucro:", melhor_lucro, "\n")
  list(solucao = solucao, lucro = melhor_lucro)
}


tabu_search_repair_restricao <- function(N) {
  melhor_lucro <- -Inf
  for (i in 1:10) {
    solucao_inicial <- solucao_inicial_restricao(N)
    solucao <- tabuSearch(50, objFunc = eval_tabu_restricao_repair, config = solucao_inicial, iters = 2)
    b <- which.max(solucao$eUtilityKeep)
    lucro <- solucao$eUtilityKeep[b]
    cat("Execucao:", i, " Solucao: ", repair_restricao(binary_to_products(solucao$configKeep[b, ])), "Lucro: ", lucro, "\n")
    if (lucro > melhor_lucro) {
      melhor_solucao <- solucao
      melhor_lucro <- lucro
    }
  }
  b <- which.max(melhor_solucao$eUtilityKeep)
  solucao <- binary_to_products(melhor_solucao$configKeep[b, ])
  cat(">>> Melhor Solucao: ", solucao, "Lucro:", melhor_lucro, "\n")
  list(solucao = solucao, lucro = melhor_lucro)
}
# 
# tabu_search_repair_restricao(10)
# tabu_search_repair(10)
