# Auteurs  : Chloé Lezaire & Alexandre Bidaux

# Scripts de la partie 2

## Setup 
a0<-round(runif(1,3,15),2)  
b0<-round(runif(1,1,5),2)  
s0<-round(runif(1,1,3),2)

## Q1a
# n est une valeure aléatoire comprise entre 200 et 1000
# (si on veut des résultats toujours a peu près similaire on peut aussi fixer la variable)
n <- sample(200:1000,1, replace=F) 
x <- runif(n, 0, 5)


## Q1b - On crée Y tel que y suit une formule de regression lineaire (basée sur la loi normale)
generate_y <- function(x, a, b, s) {
  v <- c()
  for(xi in x) {
    v <- c(v, rnorm(1, a + xi*b, sqrt(s)))
  }
  return(v)
}
y <- generate_y(x, a0, b0, s0)

## Q2
# y en fonction de x (plot)
plot(x, y, col = "blue")
# b0*x + a0 (courbe) 
lines(x, a0 + x*b0, add=TRUE, col = "red")

## Q3
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
  # Erreur dans le sujet ? pas de sqrt ici normalement mais sans les résultats ne sont plus cohérents
  #return(sqrt(mean((y - a_est - b_est*x)**2)))
  return(mean((y - a_est - b_est*x)**2))
}

## Q4
# On crée la liste de couples
xy <- list(x,y)
a_est <- a_estim(xy, n)
b_est <- b_estim(xy, n)
sigma_est <- sigma_estim(xy, n)

# DEMO PROF
s_nous <- sigma_estim(xy, n)
s_vous <- sigma_estim(xy, n)**2

## Q5
plot(x, y, col="blue")
lines(x, a0 + x*b0, add=TRUE, col="red")
y_ests <- a_est + b_est*x
lines(x, y_ests, add=TRUE, col="green")

## Q6
# e_ests est le vecteur des résidus
e_ests <- y - y_ests
sum(e_ests)

## Q7
plot(mean(x), mean(y), col="red", pch=4)
lines(x, y_ests, col="black", add=TRUE)

## Q8
donnees <- data.frame(varx = x, vary = y)
reg <- lm(vary~varx, data = donnees)
summary(reg)

## Q9
n = seq(2, 1000, by=1)
a_ests = c()
b_ests = c()
for (ni in n) {
  x2 <- runif(ni, 0, 5)
  y2 <- generate_y(x2, a0, b0, s0)
  x2y2 <- list(x2, y2)
  a_ests = c(a_ests, a_estim(x2y2, ni))
  b_ests = c(b_ests, b_estim(x2y2, ni))
}
plot(n, a_ests, col="black")
lines(n, n*0 + a0, col="red", add=TRUE)
plot(n, b_ests, col="black")
lines(n, n*0 + b0, col="red", add=TRUE)

## Q10
gen_couple_xy <- function(n) {
  x <- runif(n, 0, 5)
  y <- generate_y(x, a0, b0, s0)
  return(list(x,y))
}
n <- 200
# Creation de 150 couples (x, y)
xt_yt <- replicate(150, gen_couple_xy(n))
formulas <- c()
for (t in 1:150) {
  xti <- unlist(xt_yt[1, t])
  yti <- unlist(xt_yt[2, t])
  a_est_t <- a_estim(xt_yt[, t], n)
  # Ici on utilise b_estim mais nous ne savons pas comment l'utiliser autrement
  s2xt <- mean((yti - mean(yti))*(xti - mean(xti)))/b_estim(xt_yt[, t], n)
  #s2xt <-  mean((xti - mean(xti))**2)
  sigma_est_t <- sigma_estim(xt_yt[, t], n)
  # On calcule avec formule donnée et on stocke dans le vecteur formulas 
  res <- (a_est_t - a0)/sqrt((sigma_est_t/n)*(1+((mean(xti)**2)/s2xt)))
  formulas <- c(formulas, res)
}
# Comparaison de la répartition de nos formules avec la fonction de 
# densité de la loi de Student à n-2 degrés de liberté
hist(formulas, prob = TRUE, ylim = c(0, max(dt(x, df=n-2))))
curve(dt(x, df=n-2), col="red", add=TRUE)

## Q11
gen_IC_a <- function(xy, alpha) {
  x <- unlist(xy[1])
  y <- unlist(xy[2])
  n <- length(x)
  a_est <- a_estim(xy, n)
  b_est <- b_estim(xy, n)
  s2x <- mean((y - mean(y))*(x - mean(x)))/b_est
  a_IC <- a_est + c(-1,1) * qt(1 - (alpha/2), df=n-2) * sqrt((sigma_est/n) * (1+((mean(x)**2)/s2x))) 
  return(a_IC)
}
gen_IC_b <- function (xy, alpha) {
  x <- unlist(xy[1])
  y <- unlist(xy[2])
  n <- length(x)
  sigma_est <- sigma_estim(xy, n)
  b_est <- b_estim(xy, n)
  s2x <- mean((y - mean(y))*(x - mean(x)))/b_est
  b_IC <- b_est + c(-1, 1) * qt(1 - (alpha/2), df=n-2) * sqrt(sigma_est/(n*s2x))
  return(b_IC)
}
gen_IC_s <- function(xy, alpha) {
  x <- unlist(xy[1])
  y <- unlist(xy[2])
  n <- length(x)
  chi2_quantiles <- c(qchisq(1 - alpha/2, df = n-2), qchisq(alpha/2, df = n-2))
  sigma_est <- sigma_estim(xy, n)^2
  s_IC <- sqrt((n-2) * sigma_est / chi2_quantiles)
  return(s_IC)
}


## Q12
alpha <- 0.05
n <- 1000
a_IC_100 <- replicate(n, gen_IC_a(gen_couple_xy(100), alpha))
b_IC_100 <- replicate(n, gen_IC_b(gen_couple_xy(100), alpha))
s_IC_100 <- replicate(n, gen_IC_s(gen_couple_xy(100), alpha))

## Q13
source("Utils.R")
plot_ICs(a_IC_100, a0)
plot_ICs(b_IC_100, b0)
plot_ICs(s_IC_100, s0)

