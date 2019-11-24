# Regressão Linear multipla
library(ggplot2)

df <- data.frame(x1 = c(25,31,45,60,65,72,80,84,75,60,50,38),
x2 = c(24,21,24,25,25,26,25,25,24,25,25,23),
x3 = c(91,90,88,87,91,94,87,86,88,91,90,89),
x4 = c(100,95,110,88,94,99,97,96,110,105,100,98),
y = c(240,236,270,274,301,316,300,296,267,276,288,261))

x1 <- df$x1
x2 <- df$x2
x3 <- df$x3
x4 <- df$x4
y <- df$y

# Vamos plotar um grafico de dispersão para analisarmos inicialmente:
plot(df)

# Hipotese: Há uma relação entre y e x1,x2,x3,x4
#           Relação é linear

lm.model <- lm(y ~ x1 + x2 + x3 + x4)

# Investigar atraves de testes descritivos como: teste T (hipotese nula), p-values 
# correlção, anova e variancia do erro quadrado, R quadrado.

summary(lm.model)

# Vemos que x2 e x3 tem significancia, mas não pode-se assumir muito em realção a isto
# o t-valor para x1 está mais longe de 0 e por isso rejeitamos a hipotese nula nele, bem
# como pode ser visto que apenas ele tem p-value baixo o suficiente para podermos inferir
# que x1 possivelmente influencia y.
#
# Porem x4 apresenta valor muito proximo de 0 no seu t-value e um p-value mais alto
# entre todas as variaveis independentes, portanto podemos concluir que x4 pouco

anova(lm.model)

# Utilizando o anova , vemos que o F valor para x4 e x3 é muito proximo de 1, tendo tambem
# p-values altos, portanto podemos inferir que x4 e x3 pouco interferem em y
# No anova podemos ainda perceber que o p-value e o F valor de x1 e x2 são bem satisfatorios
# portanto, vamos analisar uma nova hipotese com x1 e x2 influenciando y

install.packages("corrplot")
library(corrplot)
# primeiro fazemos a matriz de correlação
M <- cor(df)
corrplot(M, method = "circle")

# A corelação nos mostra que exatamente x1 e x2 influenciam mais y que os demais.

# ----------- Nova Hipotese -----------------

d2f <- data.frame(x1 = c(25,31,45,60,65,72,80,84,75,60,50,38),
x2 = c(24,21,24,25,25,26,25,25,24,25,25,23),y = c(240,236,270,274,301,316,300,296,267,276,288,261))

x11 <- d2f$x1
x22 <- d2f$x2
yy <- d2f$y


lm.model1 <- lm(yy ~ x11 + x22)

anova(lm.model1)
summary(lm.model1)

M <- cor(d2f)
corrplot(M, method = "circle")
M

## ---------------- Nova hipotese
d3f <- data.frame(x2 = c(24,21,24,25,25,26,25,25,24,25,25,23),
                  y = c(240,236,270,274,301,316,300,296,267,276,288,261))


x222 <- d3f$x2
yyy <- d3f$y


lm.model2 <- lm(yyy ~ x222)

anova(lm.model2)
summary(lm.model2)

M <- cor(d3f)
corrplot(M, method = "circle")
M

## -------------------- Nova hipotese
d4f <- data.frame(x1 = c(25,31,45,60,65,72,80,84,75,60,50,38),
                  y = c(240,236,270,274,301,316,300,296,267,276,288,261))

x111 <- d2f$x1

yyy <- d2f$y


lm.model3 <- lm(yyy ~ x11)

anova(lm.model3)
summary(lm.model3)
