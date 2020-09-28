##ISLR
##MOVING BEYOND LINEARITY
library(ISLR)
library(splines)
attach(Wage)
fix(Wage)
##Polynomial regression and step functions
fit=lm(wage???poly(age,4),data=Wage)#EACH COLUMN IS LINEAR COMBINATION DE LOS OTRO GRADOS
coef(summary(fit))
fit2=lm(wage???poly(age,4,raw=T),data=Wage)
coef(summary(fit2))
agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(age,wage,xlim=agelims,cex =.8, col =" darkgrey ")
title("Degree -4 Polynomial",outer =T)
lines(age.grid,preds$fit,lwd =2,col=" blue")
matlines(age.grid,se.bands,lwd=1,col="orange",lty=3)
preds2=predict(fit2,newdata=list(age=age.grid),se=TRUE)
max(abs(preds$fit-preds2$fit))
#Son identicos sus fitted values
#Ahora analizamos ANOVA para ver cual model se ajusta mejor etadisticamente
fit.1=lm(wage???age,data=Wage)
fit.2=lm(wage???poly(age,2),data=Wage)
fit.3=lm(wage???poly(age,3),data=Wage)
fit.4=lm(wage???poly(age,4),data=Wage)
fit.5=lm(wage???poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)
coef(summary(fit.5))
#anova parece no trabajar tan bien  con elementos ortogonales
fit.1= lm(wage???education+age,data=Wage)
fit.2= lm(wage???education+poly(age,2),data=Wage)
fit.3= lm(wage???education+poly(age,3),data=Wage)
anova(fit.1,fit.2,fit.3)
#Ahora estimaremos las probabilidad de>250
fit=glm(I(wage>250)???poly(age,4),data=Wage,family=binomial)
preds=predict(fit,newdata=list(age=age.grid),se=T)
pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
se.bands=exp(se.bands.logit)/(1+exp(se.bands.logit))
#o mas facil
preds=predict(fit,newdata=list(age=age.grid),type="response",se=T)
#ploteo
plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,.2))
points(jitter(age),I((wage>250)/5),cex =.5,pch ="|",col="darkgrey ")
lines(age.grid,pfit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col=" blue",lty =3)
#Para hacer step functions 
table(cut(age,4))#cortes
fit=lm(wage???cut(age,4),data=Wage)
coef(summary(fit))
## SPLINES
fit=lm(wage???bs(age,knots=c(25,40,60)),data=Wage)
pred=predict(fit,newdata =list(age =age.grid),se=T)
plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd =2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")
fit2=lm(wage???ns(age,df=4),data=Wage)
pred2=predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid,pred2$fit,col="red",lwd=2)
lines(fit2 ,col =" blue",lwd =2)
legend("topright",legend =c("16 DF " ,"6.8 DF"),
          col=c("red "," blue "),lty =1, lwd =2, cex =.8)
#LOCAL regression
plot(age ,wage ,xlim=agelims ,cex =5, col ="darkgrey")
title (" Local Regression ")
fit=loess(wage???age ,span =.2, data=Wage)
fit2=loess(wage???age ,span =.5, data=Wage)
lines(age.grid,predict (fit ,data.frame(age=age.grid)),
        col ="red",lwd =2)
lines(age.grid,predict (fit2 ,data.frame(age=age.grid)),
        col="blue",lwd=2)
legend("topright",legend =c("Span =0.2"," Span =0.5"),#span dice de la cantidad de neighborshood que ocupamos
          col=c("red","blue"),lty =1, lwd =2, cex =.8)
#GAMs
library(gam)
gam1=lm(wage???ns(year ,4)+ns(age ,5) +education ,data=Wage)
gam.m3=gam(wage???s(year,4)+s(age,5)+education,data=Wage)
par(mfrow =c(1,3))
plot(gam.m3,se=TRUE ,col ="blue ")
plot(gam1 , se=TRUE , col ="red ")
#We look a linear relationship between with year
#Hacemos ANOVA pa cachar cual es mejor sin year, linear year, spiles year.
gam.m1=gam(wage???s(age ,5) +education ,data=Wage)
gam.m2=gam(wage???year+s(age ,5)+education ,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")
summary(gam.m3)
#HAcemos la prediction
preds=predict(gam.m2,newdata =Wage)
# SI queremos local regression
gam_lo=gam(wage???s(year ,df=4)+lo(age ,span =0.7)+education,data=Wage)
plot(gam_lo,se=TRUE,col="green")
# un modelo iterado
gam_lo_i=gam (wage???lo(year ,age ,span =0.5) +education,data=Wage)
library (akima)
plot(gam_lo_i)
#logistic gam
gam_lr=gam(I(wage >250)???year+s(age ,df =5)+education ,
             family =binomial ,data=Wage)
par(mfrow =c(1,3))
plot(gam_lr,se=T,col =" green ")
#AJustando
gam_lr_s=gam(I(wage >250)???year+s(age ,df=5)+education ,family =
                binomial ,data=Wage ,subset =( education !="1. < HS Grad"))
plot(gam_lr_s,se=T,col =" green ")
