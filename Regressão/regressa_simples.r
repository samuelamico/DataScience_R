# Questao 11.8
install.packages('ggplot2')
library(ggplot2)

tabela2 <- data.frame(Sound = c(60,63,65,70,70,70,80,90,80,80,85,89,90,90,90,90,94,100, 
100,100), Blood = c(1,0,1,2,5,1,4,6,2,3,5,4,6,8,4,5,7,9,7,6))

# Fazendo a regressao linear
data.lm <- lm(tabela2$Blood ~ tabela2$Sound)

ggplot(data = tabela2, aes(x=Sound,y=Blood)) + geom_point(color = 'blue',size=4) + geom_smooth(method = lm,color = 'red')



# teste estatistico:
summary(data.lm)



###### PLOTANDO SEPARADAMENTE ##########

model1 <- lm(tabela2$Blood ~ tabela2$Sound)
summary(model1)

plot(tabela2$Sound, tabela2$Blood, xlab="Sound", ylab="Blood", main="Regression")
abline(model1, col="blue")

# confidence interval for a point
conf_interval_3 <- predict(model1, newdata=data.frame(Sound=76), interval="confidence",
                           level = 0.95)
conf_interval_3

# confidence interval for Line
summary(tabela2)

#plotando:
newx <- seq(60, 98, by=2)

conf_interval <- predict(model1, newdata=data.frame(Sound=newx), interval="confidence",
                         level = 0.95)
lines(newx, conf_interval[,2], col="green", lty=2)
lines(newx, conf_interval[,3], col="green", lty=2)

# predict values
pred_interval_3 <- predict(model1, newdata=data.frame(Sound=76), interval="prediction",
                           level = 0.95)
pred_interval_3

pred_interval <- predict(model1, newdata=data.frame(Sound=newx), interval="prediction",
                         level = 0.95)
lines(newx, pred_interval[,2], col="orange", lty=2)
lines(newx, pred_interval[,3], col="orange", lty=2)
