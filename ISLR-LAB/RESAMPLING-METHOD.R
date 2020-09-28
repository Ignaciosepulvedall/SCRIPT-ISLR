## ISLR
##Cross-Validation and the Bootstrap

##Validation Set Approach
library(ISLR)
library(boot)#for cv
set.seed(6)
train=sample(392,196)#muestra aleatoria de indeex 1:392
lm.fit =lm(mpg???horsepower,data=Auto,subset =train)
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg???poly(horsepower,2),data=Auto,subset =train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg???poly(horsepower,3),data=Auto,subset =train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
#Resultado es consistente con lo mencionado a traves
#del capitulo son que cuadratic tenia el menor error
##LEAVE-ONE-CROSS-VALIDATION
glm.fit=glm(mpg???horsepower ,data=Auto)
cv.error=cv.glm(Auto,glm.fit)
#delta sera los cross-validation result
cv.error$delta
#Para polinomios de distinto orden
cv.error=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[2]
}
cv.error
## K-FOLD CROSS-VALIDATION 
#El delta dos es un biased correction
set.seed(17)
cv.error.10=rep(0,10)
for(i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]}
cv.error.10
##Portafolio->Evaluando el estadistico de interes
#Creando la funcion de interes
alpha.fn=function(data,index){
X=data$X[index]
Y=data$Y[index]
return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))}
#Evaluando
alpha.fn(Portfolio,1:100)
#Only one boostrap
set.seed(2)
alpha.fn(Portfolio,sample(100,100,replace =T))
#Necesitaria hacerlo muchas veces, guardarlo y sacar la SE
boot(Portfolio,alpha.fn,R=1000)
#Estimating the Accuracy of a Linear Regression Model
boot.fn=function(data,index)
  return(coef(lm(mpg~horsepower,data=data, subset=index)))
boot.fn(Auto,1:392)
set.seed (1)
boot.fn(Auto ,sample (392 ,392 , replace =T))
# Computamos el bootstrap
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower,data=Auto))$coef
