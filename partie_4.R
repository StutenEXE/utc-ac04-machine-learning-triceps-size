# car.csv
# on enlève les valeurs qualitatives
# prix-km, prix-portes

# happiness
# happiness$Happiness.Score, happiness$Economy..GDP.per.Capita.
# happiness$Happiness.Score, happiness$Family
# happiness$Freedom~happiness$Dystopia.Residual (pas ouf mais je le mets la car il est drole je trouve)

# bodyfat
# fat avec les 3 autres
library(lmtest)

bodyfat <- read.csv('Jeux_Donnees/bodyfat.csv')
bodyfat$Fat
# /!\ ajouter des éléments de stats descriptives pour appuyer le choix

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


# ======= fat - thigh ======= -> La linéarité n'est pas aussi bonne que triceps
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


# ======= fat - midharm ======= -> R^2 trop petit & manque de correlation entre les données
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

########################################################
##### ============ CHOIX FAT~TRICEPS  ============ #####
########################################################

## Q17

plot(bodyfat$Fat, bodyfat$Triceps)
reg<-lm(bodyfat$Triceps~bodyfat$Fat)
summary(reg) # R^2 à 0.7111 > 0.6 données corrélées 
# (71% des valeurs de l'un sont expliquées par les valeurs de l'autre), à vérifier
plot(bodyfat$Fat, bodyfat$Triceps)
abline(reg) # Bonne corrélation des variables (autour de la droite de régression)
reg$coefficient

# Analyse de la normalité
# QQ plot
residuals1 <- residuals(reg)
qqnorm(residuals1)
qqline(residuals1) # La plupart des quantiles théoriques en fonction des
# quantiles observés appartiennent à la droite qqline 
# Histogramme des résidus corrigés
hist(residuals1, prob= TRUE, main="Histogramme des résidus", xlab="Résidus")
curve(dnorm(x, mean=mean(residuals1), sd=sd(residuals1)), add=TRUE, col="blue")
# Suit à peu près une loi normale
shapiro.test(residuals1) # p-value = 0.2955 > 0.05 

# Analyser l'homoscédasticité
# Résidus standardisés en fonction des valeurs prédites
residuals_standardized1 <- rstandard(reg)
plot(fitted.values(reg), residuals_standardized1, main="Homoscédasticité", xlab="Valeurs prédites", ylab="Résidus standardisés")
abline(lm(residuals_standardized1 ~ fitted(reg)), col="red")
# On observe pas beaucoup de variation dans la dispertion autour de la ligne de tendance 
# Ligne de tendance à 0 donc la variance des residus est constante 
bptest(reg) # p-value = 0.3987 > 0.05

# Analyser la linéarité
plot(reg, which = 1) # La courbe se rapproche grandement de la ligne de tendance
# plot(fitted(reg), residuals1)
resettest(reg) # p-value = 0.8107 > 0.05

## Q18
b_estim <- function(xy, n) {
  x <- xy[[1]]
  y <- xy[[2]]
  sxY <- mean(x*y)-mean(x)*mean(y)
  s2x <- mean((x - mean(x))**2)
  return(sxY/s2x)
}
a_estim <- function(xy, n) {
  x <- xy[[1]]
  y <- xy[[2]]
  b_est <- b_estim(xy, n)
  return(mean(y) - b_est*mean(x))
}
sigma_estim <- function(xy, n) {
  x <- xy[[1]]
  y <- xy[[2]]
  a_est <- a_estim(xy, n)
  b_est <- b_estim(xy, n)
  return(mean((y - a_est - b_est*x)**2))
}

x <- bodyfat$Fat
y <- bodyfat$Triceps
plot(x, y)
n <- length(x)
xy <- data.frame(x, y)
a_est <- a_estim(xy, n)
b_est <- b_estim(xy, n)
y_ests <- a_est + b_est*x
lines(x, y_ests, col="green")
regression <- lm(y~x)
abline(regression, col="red")
# On retrouve la corrélation trouvée plus tôt

## Q19 
confint(regression, level=0.95) # On prend un intervalle de confiance à 95%

## Q20
R.squared <- summary(regression)$r.squared # R2 > 0.6 = Super 

## Q21
S.2.xy <- (mean(x*y)-mean(x)*mean(y))**2
S.2.x <- mean((x - mean(x))**2)
S.2.y <- mean((y - mean(y))**2)
S.2.xy/(S.2.x * S.2.y)
R.squared
# Les 2 sont égaux
# Notre régression explique la plupart de la variance de la variable dépendante

## Q22
regression$coefficients[2] # b != 0

## Q23
residuals <- residuals(regression)
qqnorm(residuals)
qqline(residuals)
# La plupart des quantiles théoriques en fonction des
# quantiles observés appartiennent à la droite qqline 

## Q24
residuals_standardized <- rstandard(regression)
plot(fitted.values(regression), residuals_standardized, main="Homoscédasticité", xlab="Valeurs prédites", ylab="Résidus standardisés")
abline(lm(residuals_standardized ~ fitted(regression)), col="red")
# On observe pas beaucoup de variation dans la dispertion autour de la ligne de tendance -> Homoscédastique
# Ligne de tendance à 0 donc la variance des residus est constante 

## Q25
x0.1 <- round(runif(1,11,26),1)
x0.2 <- round(runif(1,11,26),1)
# On recupère les intervalles de confiances pour a et b
a.IC <- confint(regression, level=0.95)[1, ]
b.IC <- confint(regression, level=0.95)[2, ]
# On obtient 2 intervalles de confiance pour E(y0.1) et E(y0.2)
esp.yO.1_IC <- a.IC + b.IC*x0.1
esp.y0.2_IC <- a.IC + b.IC*x0.2

# Fonction pour obtenir un intervalle de prédiction de Y0
predict(regression,data.frame(x = c(x0.1, x0.2)),interval="prediction",level = 0.95)