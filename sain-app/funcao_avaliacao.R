profit <- function(solucao) {
  solucao <- round(solucao)
  profit <- 0

  for (i in 1:length(solucao)){
    if (solucao[i] == 1)
      profit <- profit + produto_1(i)
    else if (solucao[i] == 2)
      profit <- profit + produto_2(i)
    else if (solucao[i] == 3)
      profit <- profit + produto_3(i)
    else if (solucao[i] == 4)
      profit <- profit + produto_4(i)
    else if (solucao[i] == 5)
      profit <- profit + produto_5(i)
  }

  profit
}

lucro_base <- function(i) {
  cliente <- d[tempo + i, ]
  limit_bal <- cliente$X1_LIMIT_BAL

  if (cliente$Y == 0)
    lucro_base <-  limit_bal * 0.01
  else
    lucro_base <- limit_bal * 0.005

  lucro_base
}

produto_1 <- function(i) {
  cliente <- d[tempo + i, ]

  if (cliente$X2_SEX == 1)
    lucro <- lucro_base(i) * 1.02
  else if (cliente$X2_SEX == 2)
    lucro <- lucro_base(i) * 0.98

  lucro
}

produto_2 <- function(i) {
  cliente <- d[tempo + i, ]

  if (cliente$X2_SEX == 2)
    lucro <- lucro_base(i) * 1.03
  else if (cliente$X2_SEX == 1)
    lucro <- lucro_base(i) * 0.97

  lucro
}

produto_3 <- function(i) {
  cliente <- d[tempo + i, ]

  if (cliente$X5_AGE == "21-30")
    lucro <- lucro_base(i) + 10
  else if (cliente$X5_AGE == "31-40")
    lucro <- lucro_base(i) + 30
  else if (cliente$X5_AGE == "41-75")
    lucro <- lucro_base(i) + 50

  lucro
}

produto_4 <- function(i) {
  cliente <- d[tempo + i, ]

  if (cliente$X4_MARRIAGE == 1)
    lucro <- lucro_base(i) * 1.03
  else
    lucro <- lucro_base(i)

  lucro
}

produto_5 <- function(i) {
  d[tempo + i, ]$X1_LIMIT_BAL * 0.0075
}