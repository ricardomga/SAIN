library(shiny)
source("preparacao_dados.R")
source("regressao_logistica.R")
source("sann.R")
source("monte_carlo.R")
source("tabu_search.R")
source("genalg.R")
source("random_forest.R")
source("ksvm.R")
source("mlpe.R")
source("mlp.R")
source("decision_tree.R")
source("naive.R")
# read.csv("sain-app/data/card.csv")

shinyServer(function(input, output, session) {
  output$clientes <- renderDataTable({
    tempo <- input$tempo
    clientes <- input$clients
    
    if (input$modelprev == 1){
      # RANDOM FOREST
      colunas <- c("X2_SEX", "X5_AGE", "X4_MARRIAGE", "Y", "random_forest", "produto")
      previsoes <- random_forest_undersampling_produtivo(d, tempo, clientes)
      if (input$modeloptm == 1){
        # optimizacao SANN
        if (input$rest == "com"){
          optimizacao <- sann_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- sann_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      } else if (input$modeloptm == 2){
        # optimizacao MONTECARLO
        if (input$rest == "com"){
          optimizacao <- montecarlo_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- montecarlo_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      } else if (input$modeloptm == 3){
        # optimizacao TABUSEARCH
        if (input$rest == "com"){
          optimizacao <- tabu_search_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- tabu_search_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      } else if (input$modeloptm == 4){
        # optimizacao GENALG
        if (input$rest == "com"){
          optimizacao <- genalg_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- genalg_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      }
    } else if (input$modelprev == 2){
      # KSVM
      colunas <- c("X2_SEX", "X5_AGE", "X4_MARRIAGE", "Y", "ksvm", "produto")
      previsoes <- ksvm_oversampling_produtivo(d, tempo, clientes)
      if (input$modeloptm == 1){
        # optimizacao SANN
        if (input$rest == "com"){
          optimizacao <- sann_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- sann_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      } else if (input$modeloptm == 2){
        # optimizacao MONTECARLO
        if (input$rest == "com"){
          optimizacao <- montecarlo_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- montecarlo_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      } else if (input$modeloptm == 3){
        # optimizacao TABUSEARCH
        if (input$rest == "com"){
          optimizacao <- tabu_search_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- tabu_search_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      } else if (input$modeloptm == 4){
        # optimizacao GENALG
        if (input$rest == "com"){
          optimizacao <- genalg_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- genalg_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      }
    }else if (input$modelprev == 3){
      # MLPE
      colunas <- c("X2_SEX", "X5_AGE", "X4_MARRIAGE", "Y", "mlpe", "produto")
      previsoes <- mlpe_smote_produtivo(d, tempo, clientes)
      if (input$modeloptm == 1){
        # optimizacao SANN
        if (input$rest == "com"){
          optimizacao <- sann_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- sann_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      } else if (input$modeloptm == 2){
        # optimizacao MONTECARLO
        if (input$rest == "com"){
          optimizacao <- montecarlo_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- montecarlo_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      } else if (input$modeloptm == 3){
        # optimizacao TABUSEARCH
        if (input$rest == "com"){
          optimizacao <- tabu_search_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- tabu_search_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      } else if (input$modeloptm == 4){
        # optimizacao GENALG
        if (input$rest == "com"){
          optimizacao <- genalg_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- genalg_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      }
    }else if (input$modelprev == 4){
      # MLP
      colunas <- c("X2_SEX", "X5_AGE", "X4_MARRIAGE", "Y", "mlp", "produto")
      previsoes <- mlp_smote_produtivo(d, tempo, clientes)
      if (input$modeloptm == 1){
        # optimizacao SANN
        if (input$rest == "com"){
          optimizacao <- sann_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- sann_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      } else if (input$modeloptm == 2){
        # optimizacao MONTECARLO
        if (input$rest == "com"){
          optimizacao <- montecarlo_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- montecarlo_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      } else if (input$modeloptm == 3){
        # optimizacao TABUSEARCH
        if (input$rest == "com"){
          optimizacao <- tabu_search_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- tabu_search_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      } else if (input$modeloptm == 4){
        # optimizacao GENALG
        if (input$rest == "com"){
          optimizacao <- genalg_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- genalg_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      }
    } else if(input$modelprev == 5){
      # previsoes GLM
      colunas <- c("X2_SEX", "X5_AGE", "X4_MARRIAGE", "Y", "glm", "produto")
      previsoes <- glm_smote_produtivo(d, tempo, clientes)
      if (input$modeloptm == 1){
        # optimizacao SANN
        if (input$rest == "com"){
          optimizacao <- sann_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- sann_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      } else if (input$modeloptm == 2){
        # optimizacao MONTECARLO
        if (input$rest == "com"){
          optimizacao <- montecarlo_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- montecarlo_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      } else if (input$modeloptm == 3){
        # optimizacao TABUSEARCH
        if (input$rest == "com"){
          optimizacao <- tabu_search_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- tabu_search_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      } else if (input$modeloptm == 4){
        # optimizacao GENALG
        if (input$rest == "com"){
          optimizacao <- genalg_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- genalg_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      }
    } else if (input$modelprev == 6){
      # DECISION TREE
      colunas <- c("X2_SEX", "X5_AGE", "X4_MARRIAGE", "Y", "decision_tree", "produto")
      previsoes <- decision_tree_undersampling_produtivo(d, tempo, clientes)
      if (input$modeloptm == 1){
        # optimizacao SANN
        if (input$rest == "com"){
          optimizacao <- sann_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- sann_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      } else if (input$modeloptm == 2){
        # optimizacao MONTECARLO
        if (input$rest == "com"){
          optimizacao <- montecarlo_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- montecarlo_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      } else if (input$modeloptm == 3){
        # optimizacao TABUSEARCH
        if (input$rest == "com"){
          optimizacao <- tabu_search_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- tabu_search_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      } else if (input$modeloptm == 4){
        # optimizacao GENALG
        if (input$rest == "com"){
          optimizacao <- genalg_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- genalg_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      }
    } else if (input$modelprev == 7){
      # NAIVE
      colunas <- c("X2_SEX", "X5_AGE", "X4_MARRIAGE", "Y", "naive", "produto")
      previsoes <- naive_oversampling_produtivo(d, tempo, clientes)
      if (input$modeloptm == 1){
        # optimizacao SANN
        if (input$rest == "com"){
          optimizacao <- sann_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- sann_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      } else if (input$modeloptm == 2){
        # optimizacao MONTECARLO
        if (input$rest == "com"){
          optimizacao <- montecarlo_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- montecarlo_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      } else if (input$modeloptm == 3){
        # optimizacao TABUSEARCH
        if (input$rest == "com"){
          optimizacao <- tabu_search_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- tabu_search_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      } else if (input$modeloptm == 4){
        # optimizacao GENALG
        if (input$rest == "com"){
          optimizacao <- genalg_repair_restricao(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }else{
          optimizacao <- genalg_repair(clientes)
          previsoes$produto <- optimizacao$solucao
          output$lucro <- renderText({ paste("Lucro: ", optimizacao$lucro, "$" )})
          return(previsoes[, colunas])
        }
      }
    }
  })
})


    
  
