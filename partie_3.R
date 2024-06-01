# Projet AC04

library(lmtest)
data(anscombe)
ans <- read.csv("./Jeux_Donnees/anscombe.csv")

plot(anscombe$x1, anscombe$y1)
plot(anscombe$x2, anscombe$y2)
plot(anscombe$x3, anscombe$y3)
plot(anscombe$x4, anscombe$y4)

#Q15

reg1<-lm(anscombe$y1~anscombe$x1)
summary(reg1)
plot(anscombe$x1, anscombe$y1)
abline(reg1)
reg$coefficient # a=3 et b=0.5 (environ) et un R carré de 0,66

reg2<-lm(anscombe$y2~anscombe$x2)
summary(reg2)
plot(anscombe$x2, anscombe$y2)
abline(reg2)
reg2$coefficient # a=3 et b=0.5 (environ) et un R carré de 0,66

reg3<-lm(anscombe$y3~anscombe$x3)
summary(reg3)
plot(anscombe$x3, anscombe$y3)
abline(reg3)
reg3$coefficient  # a=3 et b=0.5 (environ) et un R carré de 0,66

reg4<-lm(anscombe$y4~anscombe$x4)
summary(reg4)
plot(anscombe$x4, anscombe$y4)
abline(reg4)
reg4$coefficient  # a=3 et b=0.5 (environ) et un R carré de 0,66

# Droites de régressions linéaires ont les mêmes équations et les coeffs r² sont quasiment les mêmes au millième près

# Q16
reg1

# Analyse de la normalité
residuals1 <- residuals(reg1)
qqnorm(residuals1)
qqline(residuals1)
# Histogramme des résidus corrigés
hist(residuals1, prob= TRUE, main="Histogramme des résidus", xlab="Résidus")
curve(dnorm(x, mean=mean(residuals1), sd=sd(residuals1)), add=TRUE, col="blue")
shapiro.test(residuals1)    # p-value > 0,05 donc hypothèse de normalité validée


# Analyser l'homoscédasticité
# Résidus standardisés en fonction des valeurs prédites
residuals_standardized1 <- rstandard(reg1)
plot(fitted.values(reg1), residuals_standardized1, main="Homoscédasticité", xlab="Valeurs prédites", ylab="Résidus standardisés")
abline(lm(residuals_standardized1 ~ fitted(reg1)), col="red")
bptest(reg1)
plot(reg1, which = 3)
# On voit que la dispersion est constante, que la ligne de tendance est à 0 donc la variance des residus est constante 
# Hypothèse d'homocédacité vérifiée

# Analyser la linéarité 
plot(reg1, which = 1)
plot(fitted(reg1), residuals1)
resettest(reg1)



reg2

# Analyse de la normalité
residuals2 <- residuals(reg2)
qqnorm(residuals2)
qqline(residuals2)
# Histogramme des résidus corrigés
hist(residuals2, prob= TRUE, main="Histogramme des résidus", xlab="Résidus")
curve(dnorm(x, mean=mean(residuals2), sd=sd(residuals2)), add=TRUE, col="blue")
shapiro.test(residuals2)   # p-value > 0,05 donc hypothèse de normalité validée mais moins évident 

# Analyser l'homoscédasticité
# Résidus standardisés en fonction des valeurs prédites
residuals_standardized2 <- rstandard(reg2)
plot(fitted.values(reg2), residuals_standardized2, main="Homoscédasticité", xlab="Valeurs prédites", ylab="Résidus standardisés")
abline(lm(residuals_standardized2 ~ fitted(reg2)), col="red")
bptest(reg2)
plot(reg2, which = 3)
# On voit que la dispersion est constante, que la ligne de tendance est à 0 donc la variance des residus est contante 
# Hypothèse d'homocédastité non vérifiée

# Analyser la linéarité 
plot(reg2, which = 1)
plot(fitted(reg2), residuals2)
resettest(reg2)


reg3

# Analyse de la normalité
residuals3 <- residuals(reg3)
qqnorm(residuals3)
qqline(residuals3)
# Histogramme des résidus corrigés
hist(residuals3, prob= TRUE, main="Histogramme des résidus", xlab="Résidus")
curve(dnorm(x, mean=mean(residuals3), sd=sd(residuals3)), add=TRUE, col="blue")
shapiro.test(residuals3)  # p-value < 0,05 donc hypothèse de normalité non validée 

# Analyser l'homoscédasticité
# Résidus standardisés en fonction des valeurs prédites
residuals_standardized3 <- rstandard(reg3)
plot(fitted.values(reg3), residuals_standardized3, main="Homoscédasticité", xlab="Valeurs prédites", ylab="Résidus standardisés")
abline(lm(residuals_standardized3 ~ fitted(reg3)), col="red")
bptest(reg3)
plot(reg3, which = 3)
# On voit que la dispersion est constante, que la ligne de tendance est à 0 donc la variance des residus est contante 
# Hypothèse d'homocédacité non vérifiée

# Analyser la linéarité 
plot(reg3, which = 1)
plot(fitted(reg3), residuals3)
resettest(reg3)

reg4

# Analyse de la normalité
residuals4 <- residuals(reg4)
qqnorm(residuals4)
qqline(residuals4)
# Histogramme des résidus corrigés
hist(residuals4, prob= TRUE, main="Histogramme des résidus", xlab="Résidus")
curve(dnorm(x, mean=mean(residuals4), sd=sd(residuals4)), add=TRUE, col="blue")
shapiro.test(residuals4)   # p-value > 0,05 donc hypothèse de normalité validée  

# Analyser l'homoscédasticité
# Résidus standardisés en fonction des valeurs prédites
residuals_standardized4 <- rstandard(reg4)
plot(fitted.values(reg4), sqrt(residuals_standardized4), main="Homoscédasticité", xlab="Valeurs prédites", ylab="Résidus standardisés")
abline(lm(sqrt(residuals_standardized4) ~ fitted(reg4)), col="red")
bptest(reg4)
# On voit que la dispersion est constante, que la ligne de tendance est à 0 donc la variance des residus est contante 
# Hypothèse d'homocédacité non vérifiée

# Analyser la linéarité 
plot(reg4, which = 1)
plot(fitted(reg4), residuals4)
resettest(reg4)


