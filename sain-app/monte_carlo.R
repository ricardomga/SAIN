source("preparacao_dados.R")
source("geral_otimizacao.R")
source("blind.R") # fsearch is defined here
source("montecarlo.R") # mcsearch is defined here


montecarlo_dp <- function(N) {
  melhor_lucro <- -Inf
  for (i in 1:10) {
    solucao <- mcsearch(
      N = 1000,
      lower = rep(1, N),
      upper = rep(5, N),
      FUN = eval_max_dp,
      type = "max")
    lucro <- solucao$eval
    cat("Execucao:", i, " Solucao:", round(solucao$sol), " Lucro:", lucro, "\n")
    if (lucro > melhor_lucro) {
      melhor_solucao <- solucao
      melhor_lucro <- lucro
    }
  }
  solucao <- round(melhor_solucao$sol)
  cat(">>> Melhor Solucao: ", solucao, "Lucro:", melhor_lucro, "\n")
  list(solucao = solucao, lucro = melhor_lucro)
}


montecarlo_dp_restricao <- function(N) {
  melhor_lucro <- -Inf
  for (i in 1:10) {
    limite <- sample(c(sample(1:5, round(N / 2), replace = TRUE), sample(0, floor(N / 2), replace = TRUE)))
    solucao <- mcsearch(
      N = 1000,
      limite,
      limite,
      FUN = eval_max_dp_restricao,
      type = "max")
    lucro <- solucao$eval
    cat("Execucao:", i, " Solucao:", round(solucao$sol), " Lucro:", lucro, "\n")
    if (lucro > melhor_lucro) {
      melhor_solucao <- solucao
      melhor_lucro <- lucro
    }
  }
  solucao <- round(melhor_solucao$sol)
  cat(">>> Melhor Solucao: ", solucao, "Lucro:", melhor_lucro, "\n")
  list(solucao = solucao, lucro = melhor_lucro)
}


montecarlo_repair <- function(N) {
  melhor_lucro <- -Inf
  for (i in 1:10) {
    solucao <- mcsearch(
      N = 1000,
      lower = rep(1, N),
      upper = rep(5, N),
      FUN = eval_max_repair,
      type = "max")
    lucro <- solucao$eval
    cat("Execucao:", i, " Solucao:", repair(round(solucao$sol)), " Lucro:", lucro, "\n")
    if (lucro > melhor_lucro) {
      melhor_solucao <- solucao
      melhor_lucro <- lucro
    }
  }
  solucao <- repair(round(melhor_solucao$sol))
  cat(">>> Melhor Solucao: ", solucao, "Lucro:", melhor_lucro, "\n")
  list(solucao = solucao, lucro = melhor_lucro)
}


montecarlo_repair_restricao <- function(N) {
  melhor_lucro <- -Inf
  for (i in 1:10) {
    limite <- sample(c(sample(1:5, round(N / 2), replace = TRUE), sample(0, floor(N / 2), replace = TRUE)))
    solucao <- mcsearch(
      N = 1000,
      limite,
      limite,
      FUN = eval_max_repair_restricao,
      type = "max")
    lucro <- solucao$eval
    cat("Execucao:", i, " Solucao:", repair_restricao(round(solucao$sol)), " Lucro:", lucro, "\n")
    if (lucro > melhor_lucro) {
      melhor_solucao <- solucao
      melhor_lucro <- lucro
    }
  }
  solucao <- repair_restricao(round(melhor_solucao$sol))
  cat(">>> Melhor Solucao: ", solucao, "Lucro:", melhor_lucro, "\n")
  list(solucao = solucao, lucro = melhor_lucro)
}

