# Carregando a base de dados interna 'cars'
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
equacao_reta = paste0("y = ", round(a, 2), " + ", round(b, 2), "x")

# Desenhando o gráfico de dispersão
plot(x, y, xlab = "Velocidade", ylab = "Distância", main = "Regressão Linear", pch = 16)
text(7.5, 100, paste0("Equação da reta:", equacao_reta))
text(7.5, 90, paste0("R:", R))
text(7.5, 80, paste0("R2:", R2))

# Adicionando a linha de regressão
abline(a, b, col = "red")
