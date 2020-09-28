##ISLR 
## LINEAR MODEL SELECTION AND REGULARIZATION
## BEST SUBSET SELECTION
library(ISLR)
library(leaps)
library(glmnet)
library(pls)
fix(Hitters)
Hitters=na.omit(Hitters)
regfit.full=regsubsets(Salary~.,Hitters)#Escojera el mejor subset en terminos de su RSS
summary(regfit.full)
#Modelo viene restringido hasta 8 predictores but,
regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=18)
reg.summary=summary(regfit.full)
#Plots
par(mfrow =c(2,2))
plot(reg.summary$rss ,xlab=" Number of Variables ",ylab=" RSS",
       type="l")
plot(reg.summary$adjr2 ,xlab =" Number of Variables ",
       ylab=" Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11],col ="red",cex =2,pch =20)
## Corroborando con Cp,R2 adjust, AIC and BIC
plot(reg.summary$cp ,xlab =" Number of Variables ",ylab="Cp",
       type='l')
which.min (reg.summary$cp)
points(10,reg.summary$cp[10],col ="red",cex =2, pch =20)
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab=" Number of Variables ",ylab=" BIC",type = "l")
points(6,reg.summary$bic[6],col =" red",cex =2, pch =20)
# Forma shorty de hacerla
plot(regfit.full,scale ="r2")
plot(regfit.full,scale ="adjr2")
plot(regfit.full,scale ="Cp")
plot(regfit.full,scale ="bic")
#cuadrados nos indica predictores, si quiero los coef
coef(regfit.full,4)
## FORWARD AND BACKWARD STEPWISE SELECTION
#misma sintaxis que subset, solo que ahora selecciono el metodo
regfit.fwd=regsubsets(Salary???.,data=Hitters,nvmax =19,method ="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary???.,data=Hitters,nvmax =19,method ="backward")
summary(regfit.bwd)
## ESCOJIENDO EL MODELO CON VS AND CV
set.seed(1)
train=sample(c(TRUE ,FALSE),nrow(Hitters),rep=TRUE)
test =(!train)
regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)
test.mat=model.matrix(Salary???.,data=Hitters[test,])
val.errors=rep(NA ,19)
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat [,names(coefi)]%*% coefi
  val.errors[i]= mean((Hitters$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors)#El menor
coef(regfit.best,7)#sus coeficientes
# Es tortuoso dado que no podemos aplicar predict but,
predict.regsubsets=function(object,newdata,id,...){
form=as.formula(object$call[[2]])
mat=model.matrix(form,newdata )
coefi=coef(object,id=id)
xvars=names(coefi)
mat[,xvars]%*%coefi}
regfit.best=regsubsets(Salary???.,data=Hitters,nvmax=19)
coef(regfit.best,7)#Diferencias entre full data y trainig sets
# Ahora intentamos escojeer el mejor entre diferentes tamaños
k=10
set.seed (1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19,dimnames=list(NULL,paste(1:19)))
for(j in 1:k){
  best.fit=regsubsets(Salary???.,data=Hitters[folds !=j,],nvmax =19)
  for(i in 1:19){
    pred=predict(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)}}
#Tomando promedio por la predictors
mean.cv.errors=apply(cv.errors,2,mean)
par(mfrow =c(1,1))
plot(mean.cv.errors,type='b')#se cacha que el 10
#Mis coeficientes y predictores
reg.best=regsubsets(Salary???.,data=Hitters,nvmax =19)
coef(reg.best,10)
##LASSO AND RIDGE REGRESSION
x=model.matrix(Salary???.,Hitters)[,-1]
y=Hitters$Salary
#Useful for packages
##RIDGE REGRESSION
#alpha=0 for ridge & 1 for lasso
grid=10**seq(10,-2,length =100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)#estandarizada por defecto
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
ridge.mod$lambda[80]
coef(ridge.mod)[,80]
sqrt(sum(coef(ridge.mod)[-1,80]^2))
#Mientras menor lambda menor la penalizacion
predict(ridge.mod,s=705,type ="coefficients")[1:20,]#si quiero cambiar lambda
#Ahora para estimar el test error
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train )
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4, newx=x[test,])#Mientras mas grande el s mayor test error
mean((ridge.pred-y.test)^2)
#En vez de escojer arbitrariamente lambda usamos CV
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
## LASSO
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
#CV
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha =1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha =1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam )[1:20,]
lasso.coef
## PCR and PLS REGRESION.
##PRINCIPAL COMPONENTS REGRESSION
set.seed(2)
pcr.fit=pcr(Salary???., data=Hitters ,scale=TRUE ,
              validation ="CV")#scale estandariza
summary(pcr.fit)
validationplot(pcr.fit ,val.type="MSEP")
#test performance
set.seed(1)
pcr.fit=pcr(Salary???.,data=Hitters,subset =train ,scale=TRUE,validation ="CV")
validationplot(pcr.fit ,val.type="MSEP")
#en el mio se ve como en el 5
pcr.pred=predict(pcr.fit,x[test,],ncomp=5)
mean((pcr.pred-y.test)^2)
pcr.fit=pcr(y???x,scale =TRUE,ncomp =7)
summary(pcr.fit)
##PLS
set.seed(1)
pls.fit=plsr(Salary???.,data=Hitters,subset=train,scale=TRUE,validation ="CV")
summary(pls.fit)
#the test set
pls.pred=predict(pls.fit,x[test,],ncomp=3)
mean((pls.pred-y.test)^2)
pls.fit=plsr(Salary???.,data=Hitters,scale=TRUE,ncomp =2)
summary(pls.fit)# PCR busca maximizar el monto de la varianza explicada