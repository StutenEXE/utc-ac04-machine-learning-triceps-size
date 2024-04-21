# Q14
anscombe <- read.csv('Jeux_Donnees/anscombe2.csv')

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
