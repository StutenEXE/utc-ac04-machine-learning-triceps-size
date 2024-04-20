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
    v <- c(v, rnorm(1, a + xi*b, s))
  }
  return(v)
}
y <- generate_y(x, a0, b0, s0)

## Q2
plot(x, y, col="blue")
lines(x, a0 + x*b0, add=TRUE, col="red")

## Q3
b_estim <- function(x, y) {
  if (length(x) != length(y)) {
    stop("x et y n'ont pas la même taille")
  }
  n <- length(x)
  sxY <- mean(x*y)-mean(x)*mean(y)
  s2x <- mean((x - mean(x))**2)
  return(sxY/s2x)
}
a_estim <- function(x, y) {
  if (length(x) != length(y)) {
    stop("x et y n'ont pas la même taille")
  }
  b_est <- b_estim(x, y)
  return(mean(y) - b_est*mean(x))
}
sigma_estim <- function(x, y) {
  if (length(x) != length(y)) {
    stop("x et y n'ont pas la même taille")
  }
  a_est <- a_estim(x, y)
  b_est <- b_estim(x, y)
  # Erreur dans le sujet ? pas de sqrt ici normalement mais sans les résultats ne sont plus cohérents
  return(sqrt(mean((y - a_est - b_est*x)**2)))
  #return(mean((y - a_est - b_est*x)**2))
}

## Q4
a_est <- a_estim(x, y)
b_est <- b_estim(x, y)
sigma_est <- sigma_estim(x, y)

## Q5
plot(x, y, col="blue")
lines(x, a0 + x*b0, add=TRUE, col="red")
y_ests <- a_est + b_est*x
lines(x, y_ests, add=TRUE, col="green")

## Q6
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
n = seq(1, 1000, by=1)
a_ests = c()
b_ests = c()
for (ni in n) {
  x2 <- runif(ni, 0, 5)
  y2 <- generate_y(x2, a0, b0, s0)
  a_ests = c(a_ests, a_estim(x2, y2))
  b_ests = c(b_ests, b_estim(x2, y2))
}
plot(n, a_ests, col="black")
lines(n, n*0 + a0, col="red", add=TRUE)
plot(n, b_ests, col="black")
lines(n, n*0 + b0, col="red", add=TRUE)

## Q10
n <- 200
xt <- replicate(150, runif(n, 0, 5))
formulas <- c()
for (t in 1:150) {
  xti <- xt[t,]
  yti <- generate_y(xti, a0, b0, s0)
  a_est_t <- a_estim(xti, yti)
  s2xt <-  mean((xti - mean(xti))**2)
  res <- (a_est_t - a0)/sqrt((var(yti)/n)*(1+((mean(xti)**2)/s2xt)))
  formulas <- c(formulas, res)
}
hist(formulas)
curve(dt(x, df=n-2), col="red", add=TRUE)
      