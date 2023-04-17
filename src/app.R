library(shiny)
library("readxl")
library(tidyverse)
library(ggplot2)
library(OpenStreetMap)
library(sp)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Testes de Hipóteses"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("variancia", "Digite a Variância", value=15),
      radioButtons("tipo", "Tipo do teste",
                   c("Bilateral" = "bi",
                     "Unilateral a Esquerda" = "esq",
                     "Unilateral a Direita" = "dir")),
      sliderInput("mu0", "Selecione mu0",
                  min = 4, max = 12, value = 10
      ),
      
      sliderInput("alfa", "Selecione alfa",
                  min = 0.01, max = 0.10, value = 0.05
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      
      tabsetPanel(type = "tabs",
                  tabPanel("Mapa",
                           plotOutput("mapa")),
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
  
  
  data = read_excel("~/Dashboard/resource/dados_de_caminhada_corrida.xlsx")
  data_filtrada = data %>%
    separate(Velocidade, into = c("Velocidade"), sep = " ") %>%
    mutate(Hora = format(as.POSIXct(Hora, tz = "UTC"), "%T"), Velocidade = as.numeric(Velocidade)) %>%
    filter(Hora > "18:40:53" & Hora < "18:45:12")
  velocidade = data_filtrada$Velocidade
  
  n = reactive(length(velocidade))
  xbarra = reactive(mean(velocidade))
  sig = reactive(sd(velocidade))
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
    hist(velocidade, main='', freq = FALSE)
    abline(v = mu0(), col= 'red')
    abline(v = xbarra(), col= 'blue')
  })
  
  output$reg = renderPlot({
    data(cars)
    # Calculando a tabela de soma
    x = cars$speed
    y = cars$dist
    n = length(x)
    s_x = sum(x)
    s_y = sum(y)
    s_x2 = sum(x ^ 2)
    s_y2 = sum(y ^ 2)
    s_xy = sum(x * y)

    # Calculando o R e o R2
    num = n * s_xy - s_x * s_y
    den1 = sqrt(n * s_x2 - s_x ^ 2)
    den2 = sqrt(n * s_y2 - s_y ^ 2)
    R = num / (den1 * den2)
    R2 = R^2

    # Calculando a equação da reta
    b = num / (den1 ^ 2)
    a = (s_y - b * s_x) / n
    eq_reta = paste0("y = ", round(a, 2), " + ", round(b, 2), "x")

    # Desenhando o gráfico de dispersão
    plot(x, y, xlab = "Velocidade", ylab = "Distância", main = "Regressão Linear", pch = 16)
    text(7.5, 100, paste0("Equação da reta:", eq_reta))
    text(7.5, 90, paste0("R:", R))
    text(7.5, 80, paste0("R2:", R2))

    # Adicionando a linha de regressão
    abline(a, b, col = "red")
  })
  
  output$mapa = renderPlot({
    dados = read_excel("~/Dashboard/resource/dados_de_caminhada_corrida.xlsx")
    cood = dados %>%
      separate(Coordenadas, into = c("lat", "long"), sep = ",") %>%
      select(long, lat) %>%
      mutate(long = as.numeric(long), lat = as.numeric(lat))

    bb = matrix(c(-34.9525, -34.949, 
                  -8.018, -8.014), 2,2, byrow=T)

    rownames(bb) = c('long', 'lat')
    colnames(bb) = c('min', 'max')

    crs = CRS("+proj=utm +zone=25 +south +datum=WGS84")

    lonr = bb[1,2]; latu = bb[2,2] 
    lonl = bb[1,1]; latd = bb[2,1]

    sa_map = openmap(c(latu+0.001, lonl-0.001), 
                    c(latd-0.001, lonr+0.001),
                    type = "osm", mergeTiles = TRUE, minNumTiles = 9L)

    sa_map2 = openproj(sa_map)


    sa_map2_plt = OpenStreetMap::autoplot.OpenStreetMap(sa_map2) + 
      geom_point(data = cood,
                aes(x = long, y = lat), # slightly shift the points
                colour = "red", size =  2.5) +
      xlab("Longitude") + ylab("Latitude")
    sa_map2_plt
  })
}

# Run the application 
shinyApp(ui = ui, server = server)