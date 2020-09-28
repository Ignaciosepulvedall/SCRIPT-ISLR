#SUPPORT VECTOR MACHINES
##Svc
set.seed(1)
x=matrix(rnorm(20*2),ncol=2)
y=c(rep(-1,10),rep(1 ,10) )
x[y==1,]=x[y==1,] + 1
plot(x,col=(3-y))
dat=data.frame(x=x,y=as.factor(y))
library(e1071)
svmfit=svm(y???.,data=dat,kernel="linear",cost=10,scale =FALSE)
plot(svmfit,dat,col=c('magenta','darkred'))
svmfit$index
summary(svmfit)
#que pasa si achicamos el costo
svmfit=svm(y???.,data=dat,kernel ="linear",cost =0.1,
              scale=FALSE )
plot(svmfit,dat)
svmfit$index 
#mas support vector porque amrgen es mas grande
#si quiero comparar a traves de un rango de parametros
set.seed(1)
tune.out=tune(svm ,y???.,data=dat ,kernel="linear",
                ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)
#predcit sobre new test values
xtest=matrix(rnorm(20*2),ncol =2)
ytest=sample(c(-1,1),20,rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,]+1
testdat=data.frame(x=xtest,y=as.factor(ytest))
ypred=predict(bestmod,testdat)
table(predict=ypred,truth=testdat$y)
#y si cambio el costo
svmfit=svm(y???.,data=dat,kernel="linear",cost=.01,
              scale=FALSE)
ypred=predict(svmfit,testdat)
table(predict=ypred,truth=testdat$y)
#consideremos una situacion donde es linealmente separable
x[y==1,]=x[y==1,]+0.5
plot(x,col=(y+5)/2,pch =19)
dat=data.frame(x=x,y=as.factor (y))
svmfit =svm(y???.,data=dat,kernel ="linear",cost =1e5)
summary(svmfit)
plot(svmfit,dat)
svmfit=svm(y???.,data=dat,kernel="linear",cost=1)
summary(svmfit)
plot(svmfit,dat)
#SUPPORT VECTOR MACHINE
#For no linear kernel fit 
#generate a non-linear data
set.seed(1)
x=matrix(rnorm(200*2),ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))
plot(x,col=y)
#fit
train=sample(200 ,100)
svmfit =svm(y???.,data=dat[train,],kernel="radial",gamma =1,cost =1)
plot(svmfit,dat[train,])
summary(svmfit)
#CV for gamma
set.seed(1)
tune.out=tune(svm,y???.,data=dat[train,],kernel ="radial",
                ranges=list(cost=c(0.1 ,1 ,10 ,100,1000),
                             gamma=c(0.5,1,2,3,4)))
summary(tune.out)
table(true=dat[-train ,"y"],pred=predict(tune.out$best.model,newdata=dat[-train,]))
#ROC CURVES
library(ROCR)
rocplot=function(pred,truth,...){
    predob=prediction(pred,truth)
    perf=performance(predob,"tpr","fpr")
    plot(perf,...)}
svmfit_opt=svm(y???.,data=dat[train,],kernel="radial",gamma=2,cost=1,decision.values=T)
fitted=attributes(predict(svmfit_opt,dat[train,],decision.values=TRUE))$decision.values
par(mfrow =c(1,2))
rocplot(fitted,dat[train,"y"],main="Training Data")
#increasing gamma nos muestras mejoras
svmfit_flex=svm(y???.,data=dat[train,],kernel="radial",gamma=50,cost=1,decision.values =T)
fitted=attributes(predict(svmfit_flex,dat[train,],decision.values=T))$decision.values
rocplot(fitted,dat[train,"y"],add=T,col="red")
#In the test data
fitted=attributes(predict(svmfit_opt,dat[-train,],decision.values=T))$decision.values
rocplot(fitted ,dat[-train,"y"],main="Test Data")
fitted=attributes(predict(svmfit_flex,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],add=T,col="red")
#notamos que gamma dos lo hace mejor
#SVM WITH TWO O MORE CLASSES
set.seed(1)
x=rbind(x,matrix(rnorm(50*2),ncol=2))
y=c(y,rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat=data.frame(x=x,y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))
svmfit=svm(y???.,data=dat,kernel ="radial",cost =10,gamma =1)
plot(svmfit,dat)
#Application to Gene
library(ISLR)
names(Khan)
dat=data.frame(x=Khan$xtrain,y=as.factor(Khan$ytrain))
out=svm(y???.,data=dat,kernel="linear",cost=10)
summary(out)
table(out$fitted,dat$y)#VALE CALLAMPA OVERFITTING SOBRE EL TRAIN DATA Y LARGE SAMPLE
#PERFORMANCE EN TEST
dat_te=data.frame(x=Khan$xtest,y=as.factor(Khan$ytest))
pred_te=predict(out,newdata=dat_te)
table(pred_te,dat_te$y)
