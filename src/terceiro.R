library("readxl")
library(tidyverse)

# Filtra os dados recebidos pelo excel
dados = read_excel("~/Dashboard/resource/dados_de_caminhada_corrida.xlsx")
data_filtrada = dados %>%
    separate(Velocidade, into = c("Velocidade"), sep = " ") %>%
    mutate(Hora = format(as.POSIXct(dados$Hora, tz = "UTC"), "%T"), Velocidade = as.numeric(Velocidade)) %>%
    filter(Hora > "18:45:18" & Hora < "18:49:23") %>%
    select(Velocidade)
velocidade = data_filtrada$Velocidade

# Calcula o alfa pelo valor recebido do nível de confiança
nivel_de_confianca = 0.95  # Valor do SliderInput
alfa = (1 - nivel_de_confianca)

media = mean(velocidade)
desvio_padrao = sd(velocidade)
erro_padrao = desvio_padrao / sqrt(length(dados))

# Achando o valor de Z para o valor de 1 - (alfa / 2)
valor_z = qnorm(1 - (alfa / 2))

limite_inferior = media - (valor_z * erro_padrao)
limite_superior = media + (valor_z * erro_padrao)
paste0("[", round(limite_inferior, 4), ", ", round(limite_superior, 4), "]")
