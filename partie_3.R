# Q14
anscombe <- read.csv('Jeux_Donnees/anscombe.csv')

plot(anscombe$X1, anscombe$Y1)
plot(anscombe$X2, anscombe$Y2)
plot(anscombe$X3, anscombe$Y3)
plot(anscombe$X4, anscombe$Y4)

#Q15

reg<-lm(anscombe$Y1~anscombe$X1)
summary(reg)
plot(anscombe$X1, anscombe$Y1)
abline(reg)
reg$coefficient # a=3 et b=0.5 (environ)

reg2<-lm(anscombe$Y2~anscombe$X2)
summary(reg2)
plot(anscombe$X2, anscombe$Y2)
abline(reg2)
reg2$coefficient

reg3<-lm(anscombe$Y3~anscombe$X3)
summary(reg3)
plot(anscombe$X3, anscombe$Y3)
abline(reg3)
reg3$coefficient

reg4<-lm(anscombe$Y4~anscombe$X4)
summary(reg4)
plot(anscombe$X4, anscombe$Y4)
abline(reg4)
reg4$coefficient

# Droites de régressions linéaires ont les mêmes équations et les coeffs r² sont quasiment les mêmes au millième près

# Q16
rstandard(reg)
fitted.values(reg)
plot(fitted.values(reg), rstandard(reg))
# Nous n'observons pas de variation dans la dispersion. -> homoscédasticité validée

qqnorm(rstandard(reg))
qqline(rstandard(reg))
hist(rstandard(reg), breaks=5)
lines(density(rstandard(reg)))

qqnorm(rstandard(reg2))
fitted.values(reg2)
plot(anscombe[['X2']], rstandard(reg2))
# pareil mais avec les fonctions demandées dans l'énoncée :
plot(fitted.values(reg2), rstandard(reg2))
# Nous observons une variation dans la dispersion en fonction de x -> homoscédasticité invalide

qqnorm(rstandard(reg3))
plot(anscombe[['X3']], rstandard(reg3))
# pareil mais avec les fonctions demandées dans l'énoncée :
plot(fitted.values(reg3), rstandard(reg3))
# Nous observons une variation dans la dispersion en fonction de x -> homoscédasticité invalide

qqnorm(rstandard(reg4))
plot(anscombe[['X4']], rstandard(reg4))
# pareil mais avec les fonctions demandées dans l'énoncée :
plot(fitted.values(reg4), rstandard(reg4))
# Nous n'observons pas de variation dans la dispersion. -> homoscédasticité validée




# tests Julie - brouillon :
test <- lm(anscombe$Y1~anscombe$X1)
test
test1 <- lm(Y1~1+X1, data=anscombe)
test1
hatvalues(test1)
plot(anscombe[['X1']], hatvalues(test1))


qqnorm(rstandard(reg))
qqline(rstandard(reg))

plot(anscombe[['X1']], rstandard(reg))
