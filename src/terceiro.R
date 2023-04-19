library("readxl")
library(tidyverse)

dados = read_excel("~/Dashboard/resource/dados_de_caminhada_corrida.xlsx")
data_filtrada = dados %>%
  separate(Velocidade, into = c("Velocidade"), sep = " ") %>%
  mutate(Hora = format(as.POSIXct(dados$Hora, tz = "UTC"), "%T"), Velocidade = as.numeric(Velocidade)) %>%
  filter(Hora > "18:45:18" & Hora < "18:49:23") %>%
  select(Velocidade)
velocidade = data_filtrada$Velocidade

nivel_de_confianca = 0.95 # Valor de sliderInput
alfa = (1 - nivel_de_confianca)

media = mean(velocidade)
desvio_padrao = sd(velocidade)
erro_padrao = desvio_padrao / sqrt(length(dados))

z_alpha = qnorm(1 - (alfa / 2))

limite_inferior = media - (z_alpha * erro_padrao)
limite_superior = media + (z_alpha * erro_padrao)
paste0("[", limite_inferior, ", ", limite_superior, "]")
