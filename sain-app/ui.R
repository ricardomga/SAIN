library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Previsao e Otimizacao de Produtos Bancarios"),
  sidebarLayout(
    sidebarPanel(
      img(src="man.png", height = 150, width = 150),
      br(),
      selectInput("modelprev",
        "Selecione o modelo de previsao:",
        choices =  c(
          "",
          "Random Forest" = 1,
          "KSVM" = 2,
          "MLPE" = 3,
          "MLP" = 4,
          "Regressao Logistica" = 5,
          "Arvore de Decisao" = 6,
          "Naive Bayes" = 7
        )
      ),
      br(),
      selectInput("modeloptm",
        "Selecione o modelo de optimizacao:",
        choices = c(
          "",
          "SANN" = 1,
          "Montecarlo" = 2,
          "Tabu Search" = 3,
          "Based Gentetic Algorithm" = 4
        )
      ),
      br(),
      radioButtons("rest",
        "Selecione a possibilidade de optimizacao:",
        choices = c(
          "Sem restricoes" = "sem",
          "Com restricoes" = "com"
        )
      ),
      br(),
      numericInput("tempo",
                   "Escolha o 'tempo' atual da analise:",
                   value = 3500, min = 1, max = 5000
      ),
      br(),
      numericInput("clients",
                   "Escolha o numero de clientes:",
                   value = 10, min = 1, max = 5000
      ),
      
      
      
      hr(),
      
      submitButton("Submeter")
      
    ),
    mainPanel(
      h1(textOutput("lucro")),
      h4(dataTableOutput("clientes"))
    )
    
  )
))




