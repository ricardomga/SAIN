library(rminer)
library(DMwR)
library(randomForest)
library(genalg)
library(tabuSearch)
source("blind.R") # fsearch is defined here
source("montecarlo.R") # mcsearch is defined here

set.seed(12345)

d=read.csv("card.csv", header=TRUE, sep=";")

names(d) <- c("ID","X1_LIMIT_BAL","X2_SEX","X3_EDUCATION","X4_MARRIAGE","X5_AGE","X6_PAY_Set","X7_PAY_Ago","X8_PAY_Jul","X9_PAY_Jun","X10_PAY_Mai","X11_PAY_Abr","X12_BILL_Set","X13_BILL_Ago","X14_BILL_Jul","X15_BILL_Jun","X16_BILL_Mai","X17_BILL_Abr","X18_PAY_Set","X19_PAY_Ago","X20_PAY_Jul","X21_PAY_Jun","X22_PAY_Mai","X23_PAY_Abr", "Y")

# gender:
d$X2_SEX=factor(d$X2_SEX)
# education: 
d$X3_EDUCATION=factor(d$X3_EDUCATION, levels=c("4","3","2","1"))
# marriage:
d$X4_MARRIAGE=factor(d$X4_MARRIAGE)
#age:
d$X5_AGE=cut(d$X5_AGE,c(21,31,41,75),c("21-30","31-40","41-75"), include.lowest = TRUE)
# y:
d$Y=factor(d$Y)

#d$X6_PAY_Set=factor(d$X6_PAY_Set)
d$X6_PAY_Set=cut(d$X6_PAY_Set,c(-2,0,4,9),c("Normal","Em falta","Muito grave"), include.lowest = TRUE)
#d$X7_PAY_Ago=factor(d$X7_PAY_Ago)
d$X7_PAY_Ago=cut(d$X7_PAY_Ago,c(-2,0,4,9),c("Normal","Em falta","Muito grave"), include.lowest = TRUE)
#d$X8_PAY_Jul=factor(d$X8_PAY_Jul)
d$X8_PAY_Jul=cut(d$X8_PAY_Jul,c(-2,0,4,9),c("Normal","Em falta","Muito grave"), include.lowest = TRUE)
#d$X9_PAY_Jun=factor(d$X9_PAY_Jun)
d$X9_PAY_Jun=cut(d$X9_PAY_Jun,c(-2,0,4,9),c("Normal","Em falta","Muito grave"), include.lowest = TRUE)
#d$X10_PAY_Mai=factor(d$X10_PAY_Mai)
d$X10_PAY_Mai=cut(d$X10_PAY_Mai,c(-2,0,4,9),c("Normal","Em falta","Muito grave"), include.lowest = TRUE)
#d$X11_PAY_Abr=factor(d$X11_PAY_Abr)
d$X11_PAY_Abr=cut(d$X11_PAY_Abr,c(-2,0,4,9),c("Normal","Em falta","Muito grave"), include.lowest = TRUE)
d$ID = NULL
#d$X2_SEX=NULL
#d$X4_MARRIAGE = NULL
#d$X3_EDUCATION = NULL
#d$X5_AGE = NULL

print(summary(d))

#linhasTreino=1:as.integer(nrow(d)*0.7)

hold=holdout(d$Y, ratio=0.7, mode = "order")
dadosTreino=d[hold$tr,]
dadosTeste=d[hold$ts,]

#balancear dados de treino
dadosTreinoBalanceados= as.data.frame(SMOTE(Y ~ ., dadosTreino, perc.over = 250, k=10, perc.under = 150))
print(summary(dadosTreinoBalanceados))

#dados balanceados
#f=fit(Y ~ ., dadosTreinoBalanceados, model = "randomForest", importance = TRUE)
f=randomForest(Y ~ ., data = dadosTreinoBalanceados, importance = TRUE)
print(importance(f))
dadosTeste$Y1=predict(f,dadosTeste, type = "prob")

#ksvm
f2=fit(Y ~., dadosTreinoBalanceados, model = "ksvm")
dadosTeste$Y2=predict(f2, dadosTeste)

#dados nao balanceados
#randomForest
f3=fit(Y~., dadosTreino, model = "randomForest")
dadosTeste$Y3=predict(f3,dadosTeste)

#ksvm
f4=fit(Y~., dadosTreino,model = "ksvm")
dadosTeste$Y4=predict(f4,dadosTeste)


print(mmetric(dadosTeste$Y, dadosTeste$Y1, metric=c("ACC", "AUC")))
print(mmetric(dadosTeste$Y, dadosTeste$Y1, metric = c("CONF", "PRECISION", "CE", "TPR", "TNR")))

print(mmetric(dadosTeste$Y, dadosTeste$Y2, metric=c("ACC", "AUC")))
print(mmetric(dadosTeste$Y, dadosTeste$Y2, metric = c("CONF", "PRECISION", "CE", "TPR", "TNR")))

print(mmetric(dadosTeste$Y, dadosTeste$Y3, metric=c("ACC", "AUC")))
print(mmetric(dadosTeste$Y, dadosTeste$Y3, metric = c("CONF", "PRECISION", "CE", "TPR", "TNR")))

print(mmetric(dadosTeste$Y, dadosTeste$Y4, metric=c("ACC", "AUC")))
print(mmetric(dadosTeste$Y, dadosTeste$Y4, metric = c("CONF", "PRECISION", "CE", "TPR", "TNR")))

mgraph(dadosTeste$Y, dadosTeste$Y1, graph = "ROC", TC = 2, baseline = TRUE, Grid = 10)
mgraph(dadosTeste$Y, dadosTeste$Y2, graph = "ROC", TC = 2, baseline = TRUE, Grid = 10)
mgraph(dadosTeste$Y, dadosTeste$Y3, graph = "ROC", TC = 2, baseline = TRUE, Grid = 10)
mgraph(dadosTeste$Y, dadosTeste$Y4, graph = "ROC", TC = 2, baseline = TRUE, Grid = 10)

####OTIMIZAÇÃO####

#definição da função de avaliação
binaryStrToProducts = function(x){
  
  solBin = split(x, ceiling(seq_along(x)/5))
  
  products = c();
  
  for(i in 1:length(solBin)){
    if(is.na(match(1, solBin[[i]])))
      product = 0
    else
      product = match(1, solBin[[i]])
    
    products = c(products, product)
  }
  products
}


solIni = function(){
  sol = c()
  for(i in 1:10){
    sol = c(sol, sample(c(rep(0,4),1)))
  }
  # paste(sol, collapse = "")
  sol
}

solIniRestr = function(){
  for(i in 1:5){
  sol = c(sol,sample(c(sample(c(rep(0, 4), 1)), rep(0,5))))
}
  sol
}

evalTabu = function(x) {
  x = binaryStrToProducts(x)
  if(any(is.na(x))) return (-Inf)
  if((min(x)<1 || max(x)>5)) return(-Inf)
  profit(x)
}

evalTabuRepair = function(x) {
  x = binaryStrToProducts(x)
  if(min(x)<1){
    g = c()
    g <- which(x < 1)
    x[g] = 1
  }
  
  if(max(x)>5){
    g = c()
    g <- which(x > 5)
    x[g] = 5
  }
  profit(x)
}

evalTabuRestr = function(x) {
  x = binaryStrToProducts(x)
  if(any(is.na(x))) return (-Inf)
  if((min(x)<0 || max(x)>5)) return(-Inf)
  profit(x)
}

evalTabuRestrRepair = function(x) {
  x = binaryStrToProducts(x)
  x = correctVector(x)
  profit(x)
}

evalPopBin = function(x) {
  x = binaryStrToProducts(x)
  if(any(is.na(x))) return (Inf)
  if((min(x)<1 || max(x)>5)) return(Inf)
  -profit(x)
}

eval = function(x) {
  x = round(x)
  if((min(x)<1 || max(x)>5)) return(Inf)
  # cat("Boa solucao: ", x,"\n")
  -profit(x)
}

evalRepair = function(x) {
  x = round(x)
  
  if(min(x)<1){
    g = c()
    g <- which(x < 1)
    x[g] = 1
  }
  
  if(max(x)>5){
    g = c()
    g <- which(x > 5)
    x[g] = 5
  }
  
  -profit(x)
}

evalRestr = function(x) {
  x = round(x)
  if((min(x)<0 || max(x)>5) || length(which(x==0)) != 5) {
    # print("solucao de merda!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    return(Inf)
  }
  # cat("Boa solucao: ", x)
  -profit(x)
}

correctVector = function(x){
  if(any(is.nan(x))){
    g = c()
    g <- which(x %in% NaN)
    x[g] = 0
  }
  
  if(min(x)<0){
    g <- which(x < 0)
    x[g] = 0
  }
  
  if(max(x)>5){
    g <- which(x > 5)
    x[g] = 5
  }
  
  if(length(which(x==0)) > 5){
    g <- which(x == 0)
    n <- length(g)-5
    t <- g[1:n]
    x[g[t]] <- rep(1, n)
  }
  
  if(length(which(x==0)) < 5){
    g <- which(x != 0)
    n <- length(g)-5
    t <- g[1:n]
    x[g[t]] = rep(0, n)
  }
  return(x)
}

evalRestrRepair = function(x) {
  x = round(x)
  x = correctVector(x)
  -profit(x)
}

evalMC = function(x) {
  x = round(x)
  if((min(x)<1 || max(x)>5)) return(-Inf)
  # cat("Boa solucao: ", x,"\n")
  profit(x)
}

evalMCRestr = function(x) {
  x = round(x)
  if((min(x)<0 || max(x)>5) || length(which(x==0)) != 5) {
    # print("solucao de merda!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    return(-Inf)
  }
  # cat("Boa solucao: ", x)
  profit(x)
}


evalMCRepair = function(x) {
  x = round(x)
  if(min(x)<1){
    g = c()
    g <- which(x < 1)
    x[g] = 1
  }
  
  if(max(x)>6){
    g = c()
    g <- which(x > 6)
    x[g] = 5
  }
  profit(x)
}


evalMCRestrRepair = function(x) {
  x = round(x)
  x = correctVector(x)
  profit(x)
}


#definição da função para o lucro 
profit = function(x) {
  x = round(x)
  profit = 0
  
  for(i in 1:length(x)){
      if(x[i] == 1){
      profit = profit + p1(i)
    }else if(x[i] == 2){
      profit = profit + p2(i)
    }else if(x[i] == 3){
      profit = profit + p3(i)
    }else if(x[i] == 4){
      profit = profit + p4(i)
    }else if(x[i] == 5){
      profit = profit + p5(i)
    }else if(x[i] == 0){
      profit = profit
    }
  }
  profit
}

#calculos do lucro base
lucroBase = function(x) {
  x1Max = dadosTeste[x,]$X1_LIMIT_BAL
  if (dadosTeste[x,]$Y == 0) {
    lucroBase1 =  x1Max * 0.01
  }
  else{
    lucroBase1 = x1Max * 0.005
  }
  lucroBase1
}

#calculo do p1
p1 = function(x) {
  if ((dadosTeste[x, ]$X2_SEX) == 1) {
    lucroP1 = lucroBase(x) * 1.02
    return(lucroP1)
  }
  if (dadosTeste[x, ]$X2_SEX == 2) {
    lucroP1 = lucroBase(x) * 0.98
    return(lucroP1)
  }
}

# calculo do p2
p2 = function(x) {
  if (dadosTeste[x, ]$X2_SEX == 2) {
    lucroP2 = lucroBase(x) * 1.03
    return(lucroP2)
  }
  else{
    lucroP2 = lucroBase(x) * 0.97
    return(lucroP2)
  }
}

#lucro P3

p3 = function(x) {
  if (dadosTeste[x, ]$X5_AGE == "21-30") {
    lucroP3 = lucroBase(x) + 10
    return(lucroP3)
  }
  if (dadosTeste[x, ]$X5_AGE == "31-40") {
    lucroP3 = lucroBase(x) + 30
    return(lucroP3)
  }
  if (dadosTeste[x, ]$X5_AGE == "41-75") {
    lucroP3 = lucroBase(x) + 50
    return(lucroP3)
  }
}

#lucro P4
p4 = function(x) {
  if (dadosTeste[x, ]$X4_MARRIAGE == 1) {
    lucroP4 = lucroBase(x) * 1.03
    return(lucroP4)
  }
  else{
    lucroP4 = lucroBase(x)
    return(lucroP4)
  }
}

#lucro P5
p5 = function(x) {
  dadosTeste[x, ]$X1_LIMIT_BAL * 0.0075
}

#sabendo que é uma otimização
best = -Inf # - infinity
best1 = -Inf # - infinity
best2 = Inf
best3 = Inf
best4= -Inf
best5= -Inf
best6 = Inf
best7 = Inf
best8 = -Inf
best9 = -Inf

for(i in 1:10){
  x=sample(1:5, 10, replace = TRUE)

  sa = optim(
    x,
    fn = eval,
    method = "SANN",
    control = list(maxit=100, temp = 2000)
    # lower=rep(1,10),
    # upper = rep(5, 10)
  )
  L = profit(sa$par)
  cat("execution:", i, " solution:", round(sa$par), " profit:", L, "\n")
  if (L > best) {
    BESTSA = sa
    best = L
  }
}

cat("\n", "\n", ">> Solution: ",round(BESTSA$par),"profit:",profit(BESTSA$par),"\n", "\n")

N = 10

for(i in 1:N){
  x=sample(c(sample(1:5, N/2, replace = TRUE), sample(0,N/2, replace = TRUE)))
  # x=sample(0:5,N,replace = TRUE)

  sm = optim(
    x,
    fn = evalRestr,
    method = "SANN",
    control = list(maxit=100, temp = 2000)
    # control = list(trace=1)
  )


  L1 = profit(sm$par)
  cat("execution:",i, " solution:", round(sm$par), " profit:", L1, "\n")
  if (L1 > best1) {
    BESTSA1 = sm
    best1 = L1
  }

}



cat("\n", "\n", ">> Solution: ",round(BESTSA1$par),"profit:",profit(BESTSA1$par),"\n", "\n")

############## Tabu Search (Death Penalty) ################

solucaoInicial = solIni()
optTabu = tabuSearch(50, objFunc = evalTabu, config = solucaoInicial, iters = 2)

b = which.max(optTabu$eUtilityKeep)
cat("\n", "\n", "Best: ", binaryStrToProducts(optTabu$configKeep[b,]), "\nLucro: ", optTabu$eUtilityKeep[b],"\n", "\n")


############## Tabu Search Restrição (Death Penalty) ################

solucaoInicial = solIniRestr()
optTabu = tabuSearch(50, objFunc = evalTabuRestr, config = solucaoInicial, iters = 2)

b = which.max(optTabu$eUtilityKeep)
cat("\n", "\n", "Best: ", binaryStrToProducts(optTabu$configKeep[b,]), "\nLucro: ", optTabu$eUtilityKeep[b],"\n", "\n")



############## Tabu Search (Repair) ################

solucaoInicial = solIni()
optTabu = tabuSearch(50, objFunc = evalTabuRepair, config = solucaoInicial, iters = 2)

b = which.max(optTabu$eUtilityKeep)
cat("\n", "\n", "Best: ", binaryStrToProducts(optTabu$configKeep[b,]), "\nLucro: ", optTabu$eUtilityKeep[b],"\n", "\n")



############## Tabu Search Restrição (Repair) ################

solucaoInicial = solIniRestr()
optTabu = tabuSearch(50, objFunc = evalTabuRestrRepair, config = solucaoInicial, iters = 2)

b = which.max(optTabu$eUtilityKeep)
cat("\n", "\n", "Best: ", binaryStrToProducts(optTabu$configKeep[b,]), "\nLucro: ", optTabu$eUtilityKeep[b],"\n", "\n")





########### Based Genetic Algorithm (death penalty) ###############

for(i in 1:N){
genSample = sample(1:5, 10, replace = TRUE)
genToMatrix = matrix(data = genSample, nrow = 1, byrow = TRUE)
gen = rbga(popSize = 100, suggestions = genToMatrix, iters = 5, evalFunc = eval, stringMin = c(rep(1,10)), stringMax = c(rep(5,10)), mutationChance = 0.5, elitism = TRUE)

genBest=which.min(gen$evaluations)
LPop = gen$evaluations[genBest]

cat("execution:",i, " solution:", round(gen$population[genBest,]), " profit:", abs(LPop), "\n")
if (LPop < best2) {
  BESTSPop = gen
  best2 = LPop
}
}
genBest = which.min(BESTSPop$evaluations)
cat("\n", "\n", "Best solution:",round(BESTSPop$population[genBest,]), "Best profit:", abs(BESTSPop$evaluations[genBest]), "\n", "\n")


########## Based Genetic Algorithm Restrição (death penalty) #############

for(i in 1:N){
  genRestrSample=sample(c(sample(1:5, N/2, replace = TRUE), sample(0,N/2, replace = TRUE)))
  genRestrToMatrix = matrix(data = genRestrSample, nrow = 1, byrow = TRUE)
  minPop = sample(c(sample(1, N/2, replace = TRUE), sample(0,N/2, replace = TRUE)))
  maxPop = sample(c(rep(5, N/2), sample(0,N/2, replace = TRUE)))
  genRestr = rbga(popSize = 200, suggestions = genRestrToMatrix, iters = 15, evalFunc = evalRestr, stringMin = minPop, stringMax = maxPop, mutationChance = 0.5, elitism = TRUE)
  
  genRestrBest=which.min(genRestr$evaluations)
  LPop = genRestr$evaluations[genRestrBest]
  
  cat("execution:",i, " solution:", round(genRestr$population[genRestrBest,]), " profit:", abs(LPop), "\n")
  if (LPop < best3) {
    BESTSPopRestr = genRestr
    best3 = LPop
  }
}
genRestrBest = which.min(BESTSPopRestr$evaluations) 
cat("\n", "\n", "Best solution:",round(BESTSPopRestr$population[genRestrBest,]), "Best profit:", abs(BESTSPopRestr$evaluations[genRestrBest]), "\n", "\n")



################# Based Genetic Algorithym (repair) ##################

for(i in 1:N){
  genSample = sample(1:5, 10, replace = TRUE)
  genToMatrix = matrix(data = genSample, nrow = 1, byrow = TRUE)
  gen = rbga(popSize = 100, suggestions = genToMatrix, iters = 5, evalFunc = evalRepair, stringMin = c(rep(1,10)), stringMax = c(rep(5,10)), mutationChance = 0.5, elitism = TRUE)
  
  genBest=which.min(gen$evaluations)
  LPop = gen$evaluations[genBest]
  
  cat("execution:",i, " solution:", round(gen$population[genBest,]), " profit:", abs(LPop), "\n")
  if (LPop < best6) {
    BESTSPopRepair = gen
    best6 = LPop
  }
}
genBestRepair = which.min(BESTSPopRepair$evaluations)
cat("\n", "\n", "Best solution:",round(BESTSPopRepair$population[genBestRepair,]), "Best profit:", abs(BESTSPopRepair$evaluations[genBestRepair]), "\n", "\n")



########## Based Genetic Algorithm Restrição (repair) #############

for(i in 1:N){
  genRestrSample=sample(c(sample(1:5, N/2, replace = TRUE), sample(0,N/2, replace = TRUE)))
  genRestrToMatrix = matrix(data = genRestrSample, nrow = 1, byrow = TRUE)
  minPop = sample(c(sample(1, N/2, replace = TRUE), sample(0,N/2, replace = TRUE)))
  maxPop = sample(c(rep(5, N/2), sample(0,N/2, replace = TRUE)))
  genRestr = rbga(popSize = 100, suggestions = genRestrToMatrix, iters = 5, evalFunc = evalRestrRepair, stringMin = minPop, stringMax = maxPop, mutationChance = 0.5, elitism = TRUE)
  
  genRestrBest=which.min(genRestr$evaluations)
  LPop = genRestr$evaluations[genRestrBest]
  
  cat("execution:",i, " solution:", correctVector(round(genRestr$population[genRestrBest,])), " profit:", abs(LPop), "\n")
  if (LPop < best7) {
    BESTSPopRestrRepair = genRestr
    best7 = LPop
  }
}
genRestrBestRepair = which.min(BESTSPopRestrRepair$evaluations) 
cat("\n", "\n", "Best solution:",correctVector(round(BESTSPopRestrRepair$population[genRestrBestRepair,])), "Best profit:", abs(BESTSPopRestrRepair$evaluations[genRestrBestRepair]), "\n", "\n")



#################### Monte Carlo (Death Penalty) ###################

for(i in 1:N){
lower=rep(1,10) # lower bounds
upper=rep(5,10) #  upper bounds
MC=mcsearch(N=1000,lower=lower,upper=upper,FUN=evalMC,type="max")

LPop = MC$eval

cat("execution:",i, " solution:", round(MC$sol), " profit:", LPop, "\n")
if (LPop > best4) {
  BESTSMC = MC
  best4 = LPop
}
}
cat("\n", "\n", "Best solution:",round(BESTSMC$sol), "Best profit:", BESTSMC$eval, "\n", "\n")



############# Monte Carlo Restrição (Death Penalty) ####################

for(i in 1:N){
  lowerRestr=sample(c(sample(1:5, N/2, replace = TRUE), sample(0,N/2, replace = TRUE))) # lower bounds
  # upperRestr=sample(c(sample(5:5, N/2, replace = TRUE), sample(0,N/2, replace = TRUE))) #  upper bounds

  MCRes=mcsearch(N=1000,lowerRestr,lowerRestr ,FUN=evalMCRestr, type="max")
  
  LPop = MCRes$eval 
  cat("execution:",i, " solution:", round(MCRes$sol), " profit:", LPop, "\n")
  if (LPop > best5) {
    BESTSMCRe = MCRes
    best5 = LPop
  }
}

# g = sample(size = 10, replace = TRUE, x = c(rep(1:5,5),rep(0,5)))
cat("\n", "\n", "Best solution:",round(BESTSMCRe$sol), "Best profit:", BESTSMCRe$eval, "\n", "\n")





#################### Monte Carlo (Repair) ###################

for(i in 1:N){
  lower=rep(1,10) # lower bounds
  upper=rep(5,10) #  upper bounds
  MCRepair=mcsearch(N=1000,lower=lower,upper=upper,FUN=evalMCRepair,type="max")
  
  LPop = MCRepair$eval
  
  cat("execution:",i, " solution:", round(MCRepair$sol), " profit:", LPop, "\n")
  if (LPop > best8) {
    BESTSMCRepair = MCRepair
    best8 = LPop
  }
}
cat("\n", "\n", "Best solution:",round(BESTSMCRepair$sol), "Best profit:", BESTSMCRepair$eval, "\n", "\n")


############# Monte Carlo Restrição (Repair) ####################

for(i in 1:N){
  lowerRestr=sample(c(sample(1:5, N/2, replace = TRUE), sample(0,N/2, replace = TRUE))) # lower bounds
  # upperRestr=sample(c(sample(5:5, N/2, replace = TRUE), sample(0,N/2, replace = TRUE))) #  upper bounds
  
  MCResRepair=mcsearch(N=1000,lowerRestr,lowerRestr ,FUN=evalMCRestrRepair, type="max")
  
  LPop = MCResRepair$eval 
  cat("execution:",i, " solution:", correctVector(round(MCResRepair$sol)), " profit:", LPop, "\n")
  if (LPop > best9) {
    BESTSMCReRepair = MCResRepair
    best9 = LPop
  }
}

# g = sample(size = 10, replace = TRUE, x = c(rep(1:5,5),rep(0,5)))
cat("\n", "\n", "Best solution:",correctVector(round(BESTSMCReRepair$sol)), "Best profit:", BESTSMCReRepair$eval, "\n", "\n")

