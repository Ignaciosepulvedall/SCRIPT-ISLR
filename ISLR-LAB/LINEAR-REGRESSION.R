#Apuntes de ISLR
library (MASS)
library (ISLR)
library(car)
#3.6) Lab: Linear Regression
fix(Boston)
lm.fit =lm(medv???lstat ,data=Boston )
coef(lm.fit)#coeficientes
summary(lm.fit)
confint (lm.fit)#intervalos de confianza
predict (lm.fit ,data.frame(lstat=c(5 ,10 ,15) )
         ,interval ="confidence")#Prediccion a valores dados
lm.fit =lm(medv???.,data=Boston )
summary (lm.fit)
vif(lm.fit)
#Si quiero eliminar una
lm.fit =lm(medv???.-age,data=Boston )
summary(lm.fit)
#Non linear regression
lm.fit2=lm(medv???lstat +I(lstat ^2),data=Boston)
summary (lm.fit2)
#Qualitative predictors
fix( Carseats )
lm.fit =lm(Sales???.+ Income :Advertising +Price :Age ,data=Carseats )
summary (lm.fit)
