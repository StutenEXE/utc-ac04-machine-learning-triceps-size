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
y <- c()
for(xi in x) {
  #y <- c(y, reglineaire(xi, a0, b0, s0))
  y <- c(y, rnorm(1, a0 + xi*b0, s0))
}

## Q2
plot(x, y, col = "blue")
lines(x, a0 + x*b0, add=TRUE, col = "red")

## Q3
b_estim <- function() {
  
}
a_estim <- function() {
  
}
sigma_estim <- function() {
  
}




