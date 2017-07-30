source("preparacao_dados.R")
source("geral_otimizacao.R")
tempo <- 3500

sann_dp <- function(N) {
  melhor_lucro <- -Inf
  for (i in 1:10) {
    solucao_inicial <- sample(1:5, N, replace = TRUE)
    solucao <- optim(
      solucao_inicial,
      fn = eval_min_dp,
      method = "SANN",
      control = list(maxit = 100, temp = 2000))
    lucro <- profit(solucao$par)
    cat("Execucao:", i, " Solucao:", round(solucao$par), " Lucro:", lucro, "\n")

    if (lucro > melhor_lucro) {
      melhor_solucao <- solucao
      melhor_lucro <- lucro
    }
  }
  solucao <- round(melhor_solucao$par)
  cat(">>> Melhor Solucao: ", solucao, "Lucro:", melhor_lucro, "\n")
  list(solucao = solucao, lucro = melhor_lucro)
}

sann_dp_restricao <- function(N) {
  melhor_lucro <- -Inf
  for (i in 1:10) {
    solucao_inicial <- sample(c(sample(1:5, N / 2, replace = TRUE), sample(0, N / 2, replace = TRUE)))
    solucao <- optim(
      solucao_inicial,
      fn = eval_min_dp_restricao,
      method = "SANN",
      control = list(maxit = 100, temp = 2000))
    lucro <- profit(solucao$par)
    cat("Execucao:", i, " Solucao:", round(solucao$par), " Lucro:", lucro, "\n")
    
    if (lucro > melhor_lucro) {
      melhor_solucao <- solucao
      melhor_lucro <- lucro
    }
  }
  solucao <- round(melhor_solucao$par)
  cat(">>> Melhor Solucao: ", solucao, "Lucro:", melhor_lucro, "\n")
  list(solucao = solucao, lucro = melhor_lucro)
}

sann_repair <- function(N) {
  melhor_lucro <- -Inf
  for (i in 1:10) {
    solucao_inicial <- sample(1:5, N, replace = TRUE)
    solucao <- optim(
      solucao_inicial,
      fn = eval_min_repair,
      method = "SANN",
      control = list(maxit = 100, temp = 2000))
    lucro <- profit(repair(solucao$par))
    cat("Execucao:", i, " Solucao:", repair(round(solucao$par)), " Lucro:", lucro, "\n")
    if (lucro > melhor_lucro) {
      melhor_solucao <- solucao
      melhor_lucro <- lucro
    }
  }
  solucao <- repair(round(melhor_solucao$par))
  cat(">>> Melhor Solucao: ", solucao, "Lucro:", melhor_lucro, "\n")
  list(solucao = solucao, lucro = melhor_lucro)
}

sann_repair_restricao <- function(N){
  melhor_lucro <- -Inf
  for (i in 1:10) {
    solucao_inicial <- sample(c(sample(1:5, round(N / 2), replace = TRUE), sample(0, floor(N / 2), replace = TRUE)))
    solucao <- optim(
      solucao_inicial,
      fn = eval_min_repair_restricao,
      method = "SANN",
      control = list(maxit = 100, temp = 2000))
    lucro <- profit(repair_restricao(solucao$par))
    cat("Execucao:", i, " Solucao:", repair_restricao(round(solucao$par)), " Lucro:", lucro, "\n")
    if (lucro > melhor_lucro) {
      melhor_solucao <- solucao
      melhor_lucro <- lucro
    }
  }
  solucao <- repair_restricao(round(melhor_solucao$par))
  cat(">>> Melhor Solucao: ", solucao, "Lucro:", melhor_lucro, "\n")
  list(solucao = solucao, lucro = melhor_lucro)
}

# sann_dp(10)
# sann_dp_restricao(10)
# sann_repair(10)
# sann_repair_restricao(10)