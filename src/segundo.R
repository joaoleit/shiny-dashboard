library("readxl")
library(tidyverse)

dados = read_excel("~/Dashboard/resource/dados_de_caminhada_corrida.xlsx")
velocidade = dados %>%
  separate(Velocidade, into = c("Velocidade"), sep = " ") %>%
  mutate(Hora = format(as.POSIXct(dados$Hora, tz = "UTC"), "%T"), Velocidade = as.numeric(Velocidade)) %>%
  filter(Hora > "18:40:53" & Hora < "18:45:12") %>%
  select(Velocidade)
