# car.csv
# on enlève les valeurs qualitatives
# prix-km, prix-portes

# happiness
# 

# bodyfat
# fat avec les 3 autres
library(lmtest)

bodyfat <- read.csv('Jeux_Donnees/bodyfat.csv')
bodyfat$Fat

# ======= fat - triceps =======
plot(bodyfat$Fat, bodyfat$Triceps)
reg<-lm(bodyfat$Triceps~bodyfat$Fat)
summary(reg)
plot(bodyfat$Fat, bodyfat$Triceps)
abline(reg)
reg$coefficient

# Analyse de la normalité
# QQ plot
residuals1 <- residuals(reg)
qqnorm(residuals1)
qqline(residuals1)
# Histogramme des résidus corrigés
hist(residuals1, prob= TRUE, main="Histogramme des résidus", xlab="Résidus")
curve(dnorm(x, mean=mean(residuals1), sd=sd(residuals1)), add=TRUE, col="blue")
shapiro.test(residuals1)

# Analyser l'homoscédasticité
# Résidus standardisés en fonction des valeurs prédites
residuals_standardized1 <- rstandard(reg)
plot(fitted.values(reg), residuals_standardized1, main="Homoscédasticité", xlab="Valeurs prédites", ylab="Résidus standardisés")
abline(lm(residuals_standardized1 ~ fitted(reg)), col="red")
bptest(reg)
plot(reg, which = 3)


# Analyser la linéarité 
plot(reg, which = 1)
plot(fitted(reg), residuals1)
resettest(reg)


# ======= fat - thigh =======
plot(bodyfat$Fat, bodyfat$Thigh)
reg<-lm(bodyfat$Thigh~bodyfat$Fat)
summary(reg)
plot(bodyfat$Fat, bodyfat$Thigh)
abline(reg)
reg$coefficient

# Analyse de la normalité
residuals1 <- residuals(reg)
qqnorm(residuals1)
qqline(residuals1)
# Histogramme des résidus corrigés
hist(residuals1, prob= TRUE, main="Histogramme des résidus", xlab="Résidus")
curve(dnorm(x, mean=mean(residuals1), sd=sd(residuals1)), add=TRUE, col="blue")
shapiro.test(residuals1)    # p-value > 0,05 donc hypothèse de normalité validée

# Analyser l'homoscédasticité
# Résidus standardisés en fonction des valeurs prédites
residuals_standardized1 <- rstandard(reg)
plot(fitted.values(reg), residuals_standardized1, main="Homoscédasticité", xlab="Valeurs prédites", ylab="Résidus standardisés")
abline(lm(residuals_standardized1 ~ fitted(reg)), col="red")
bptest(reg)
plot(reg, which = 3)

# Analyser la linéarité 
plot(reg, which = 1)
plot(fitted(reg), residuals1)
resettest(reg)


# ======= fat - midharm =======
plot(bodyfat$Fat, bodyfat$Midarm)
reg<-lm(bodyfat$Midarm~bodyfat$Fat)
summary(reg)
plot(bodyfat$Fat, bodyfat$Midarm)
abline(reg)
reg$coefficient

# Analyse de la normalité
residuals1 <- residuals(reg)
qqnorm(residuals1)
qqline(residuals1)
# Histogramme des résidus corrigés
hist(residuals1, prob= TRUE, main="Histogramme des résidus", xlab="Résidus")
curve(dnorm(x, mean=mean(residuals1), sd=sd(residuals1)), add=TRUE, col="blue")
shapiro.test(residuals1)    # p-value > 0,05 donc hypothèse de normalité validée

# Analyser l'homoscédasticité
# Résidus standardisés en fonction des valeurs prédites
residuals_standardized1 <- rstandard(reg)
plot(fitted.values(reg), residuals_standardized1, main="Homoscédasticité", xlab="Valeurs prédites", ylab="Résidus standardisés")
abline(lm(residuals_standardized1 ~ fitted(reg)), col="red")
bptest(reg)
plot(reg, which = 3)

# Analyser la linéarité 
plot(reg, which = 1)
plot(fitted(reg), residuals1)
resettest(reg)
