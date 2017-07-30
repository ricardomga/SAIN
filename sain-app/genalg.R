source("preparacao_dados.R")
source("geral_otimizacao.R")
library(genalg)

tempo <- 3500

genalg_dp <- function(N) {
  melhor_lucro <- Inf
  for (i in 1:10) {
    solucao_inicial <- matrix(data = sample(1:5, N, replace = TRUE), nrow = 1, byrow = TRUE)
    solucao <- rbga(
      popSize = 100,
      suggestions = solucao_inicial,
      iters = 5,
      evalFunc = eval_min_dp,
      stringMin = c(rep(1, N)),
      stringMax = c(rep(5, N)),
      mutationChance = 0.5,
      elitism = TRUE)
    best <- which.min(solucao$evaluations)
    lucro <- solucao$evaluations[best]
    cat("Execucao:", i, " Solucao:", round(solucao$population[best, ]), " Lucro:", abs(lucro), "\n")
    if (lucro < melhor_lucro) {
      melhor_solucao <- solucao
      melhor_lucro <- lucro
    }
  }
  gen_best <- which.min(melhor_solucao$evaluations)
  solucao <- round(melhor_solucao$population[gen_best, ])
  cat(">>> Melhor Solucao: ", solucao, "Lucro:", abs(melhor_lucro), "\n")
  list(solucao = solucao, lucro = abs(melhor_lucro))
}

genalg_dp_restricao <- function(N) {
  melhor_lucro <- Inf
  for (i in 1:10) {
    s <- sample(c(sample(1:5, round(N / 2), replace = TRUE), sample(0, floor(N / 2), replace = TRUE)))
    solucao_inicial <- matrix(data = s, nrow = 1, byrow = TRUE)
    solucao <- rbga(
      popSize = 100,
      suggestions = solucao_inicial,
      iters = 5,
      evalFunc = eval_min_dp_restricao,
      stringMin = sample(c(sample(1, round(N / 2), replace = TRUE), sample(0, floor(N / 2), replace = TRUE))),
      stringMax = sample(c(rep(5, round(N / 2)), sample(0, floor(N / 2), replace = TRUE))),
      mutationChance = 0.5,
      elitism = TRUE)
    best <- which.min(solucao$evaluations)
    lucro <- solucao$evaluations[best]
    cat("Execucao:", i, " Solucao:", round(solucao$population[best, ]), " Lucro:", abs(lucro), "\n")
    if (lucro < melhor_lucro) {
      melhor_solucao <- solucao
      melhor_lucro <- lucro
    }
  }
  gen_best <- which.min(melhor_solucao$evaluations)
  solucao <- round(melhor_solucao$population[gen_best, ])
  cat(">>> Melhor Solucao: ", solucao, "Lucro:", abs(melhor_lucro), "\n")
  list(solucao = solucao, lucro = abs(melhor_lucro))
}

genalg_repair <- function(N) {
  melhor_lucro <- Inf
  for (i in 1:10) {
    solucao_inicial <- matrix(data = sample(1:5, N, replace = TRUE), nrow = 1, byrow = TRUE)
    solucao <- rbga(
      popSize = 100,
      suggestions = solucao_inicial,
      iters = 5,
      evalFunc = eval_min_repair,
      stringMin = c(rep(1, N)),
      stringMax = c(rep(5, N)),
      mutationChance = 0.5,
      elitism = TRUE)
    best <- which.min(solucao$evaluations)
    lucro <- solucao$evaluations[best]
    cat("Execucao:", i, " Solucao:", repair(round(solucao$population[best, ])), " Lucro:", abs(lucro), "\n")
    if (lucro < melhor_lucro) {
      melhor_solucao <- solucao
      melhor_lucro <- lucro
    }
  }
  gen_best <- which.min(melhor_solucao$evaluations)
  solucao <- repair(round(melhor_solucao$population[gen_best, ]))
  cat(">>> Melhor Solucao: ", solucao, "Lucro:", abs(melhor_lucro), "\n")
  list(solucao = solucao, lucro = abs(melhor_lucro))
}

genalg_repair_restricao <- function(N) {
  melhor_lucro <- Inf
  for (i in 1:10) {
    s <- sample(c(sample(1:5, round(N / 2), replace = TRUE), sample(0, floor(N / 2), replace = TRUE)))
    solucao_inicial <- matrix(data = s, nrow = 1, byrow = TRUE)
    solucao <- rbga(
      popSize = 100,
      suggestions = solucao_inicial,
      iters = 5,
      evalFunc = eval_min_repair_restricao,
      stringMin = sample(c(sample(1, round(N / 2), replace = TRUE), sample(0, floor(N / 2), replace = TRUE))),
      stringMax = sample(c(rep(5, round(N / 2)), sample(0, floor(N / 2), replace = TRUE))),
      mutationChance = 0.5,
      elitism = TRUE)
    best <- which.min(solucao$evaluations)
    lucro <- solucao$evaluations[best]
    cat("Execucao:", i, " Solucao:", repair_restricao(round(solucao$population[best, ])), " Lucro:", abs(lucro), "\n")
    if (lucro < melhor_lucro) {
      melhor_solucao <- solucao
      melhor_lucro <- lucro
    }
  }
  gen_best <- which.min(melhor_solucao$evaluations)
  solucao <- repair_restricao(round(melhor_solucao$population[gen_best, ]))
  cat(">>> Melhor Solucao: ", solucao, "Lucro:", abs(melhor_lucro), "\n")
  list(solucao = solucao, lucro = abs(melhor_lucro))
}

# genalg_dp(10)
# genalg_dp_restricao(10)
# genalg_repair(10)
# genalg_repair_restricao(10)
