library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Testes de Hipóteses"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons("dados", "Escolher dados",
                   c("Dados 1" = "d1",
                     "Dados 2" = "d2",
                     "Dados 3" = "d3")
      ),
      sliderInput("mu0", "Selecione mu0",
                  min = 0, max = 30, value = 15
      ),
      
      sliderInput("alfa", "Selecione alfa",
                  min = 0.01, max = 0.10, value = 0.05
      ),
      
      radioButtons("tipo", "Tipo do teste",
                   c("Bilateral" = "bi",
                     "Unilateral a Esquerda" = "esq",
                     "Unilateral a Direita" = "dir"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      
      tabsetPanel(type = "tabs",
                  tabPanel("Mapa", "Nada por enquanto!"),
                  tabPanel("Testes de hipóteses", 
                           tableOutput('table'),
                           plotOutput('hist')
                           ),
                  tabPanel("Intervalo de Confiança", "Nada por enquanto!"),
                  tabPanel("Regressão", plotOutput('reg'))
      )
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dados = reactive(input$dados)
  escolha_dados = renderText(dados())
  
  x = reactive({
    if(escolha_dados() == "d1"){
      c(2,5,13, 27, 21, 10, 9, 15)
    }else if(escolha_dados() == "d2"){
      c(15,2,13, 15, 12, 10, 5, 4)
    }else{
      c(1, 2, 13, 27, 7, 16, 5, 20)
    }
  })
  
  
  n = reactive(length(x()))
  xbarra = reactive(mean(x()))
  sig = reactive(sd(x()))
  sig_xbar = reactive(sig()/sqrt(n()))
  
  mu0 = reactive({
    as.integer(input$mu0)
  })
  
  
  alfa = reactive(as.numeric(input$alfa))
  
  tipo = reactive(input$tipo)
  teste = renderText(tipo())
  
  p = reactive({
    if(teste() == "bi"){
      1 - alfa() + alfa()/2
    }else if(teste() == "esq"){
      alfa()
    }else{
      1-alfa()
    }
  })
  
  
  ztab = reactive(
    as.numeric(qnorm(p()))
  )
  
  
  zcalc = reactive(
    as.numeric((xbarra()-mu0())/sig_xbar())
  )
  
  
  output$table <- renderTable(
    if(teste() == "bi" & zcalc() < ztab() & zcalc() > -ztab() |
       teste() == "esq" & zcalc() < ztab() | 
       teste() == "dir" & zcalc() > ztab()
       ){
      data.frame(Resultado = paste0('Aceitamos H0 ao nível de sig. = ', alfa()))
    }else{
      data.frame(Resultado = paste0('Rejeitamos H0 ao nível de sig. = ', alfa()))
    }
  )
  
  
  
  output$hist = renderPlot({
    hist(x(), main='', freq = FALSE)
    abline(v = mu0(), col= 'red')
    abline(v = xbarra(), col= 'blue')
  })
  
  output$reg = renderPlot({
    #gráfico de regressão com dados 'cars' aqui!
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)