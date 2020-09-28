#ISLR-LOGISTIC REGRESSION, LDA, QDA AND KNN
library(ISLR)
library(MASS)
library(class)
summary(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)
## LOGISTIC REGRESION
glm.fits=glm(Direction???Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data=Smarket ,family =binomial )
summary (glm.fits)
glm.probs<-predict(glm.fits,type ="response") #type es el que hace PROB(Y=1|X)
glm.probs[1:10]#Probabilades del grupo de entranamiento
#Para fijarme en cual es 1 
contrasts(Direction)
#CREANDO LA CLASE up & down
glm.pred=rep("Down",1250)
glm.pred[glm.probs >.5]="Up"
#Haciendo una confusion matrix
table(glm.pred,Direction)
#Que tan correcta es nuestra prediccion?
mean(glm.pred==Direction)
#traning error
1-mean(glm.pred==Direction)
#Dado que este error es altamente sub estimado
#mejor seria separar una training data.
train<-(Year<2005)
Smarket.2005<-Smarket[!train,]
Direction.2005=Direction[!train]
#Entrenando los dato y train es un subset booleano
glm.fits=glm(Direction???Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type='response')
#Probando como nos fue
glm.pred=rep("Down",250)
glm.pred[glm.probs >.5]="Up"
table(glm.pred,Direction.2005)
#A cuanto le achunte y test error
mean(glm.pred!=Direction.2005)
# Dado que los p-values son como el ollo,y dado que
# producen aumento de varianza pero sin beneficios en sesgo
glm.fits=glm(Direction???Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type='response')
glm.pred=rep("Down",250)
glm.pred[glm.probs >.5]="Up"
table(glm.pred,Direction.2005)
#Calculamos el nuevo 
mean(glm.pred!=Direction.2005)
#Notar que le achunto mas cuando mercado go up,
#una estrategia seria solo a postar una subida si
#anterior subio.
#Si quiero un predicion sobre datos especificos
predict(glm.fits,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type='response')
## LINEAR DISCRIMINANT ANALYSIS
lda.fit<-lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit # Prior prob nos muestra al peso de cada una en training set
# and group mean es el estimador de la media
#The coefficients nos diran la LDA decision rule
lda.pre=predict(lda.fit,Smarket.2005)
lda.class=lda.pre$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)
#A mano el lda.class
sum(lda.pre$posterior[,1]>=.5)
sum(lda.pre$posterior[,1]<.5)
# the posteriori probability output correspond
# prob go down
lda.pre$posterior[1:20,1]
lda.class[1:20]
# Si es que queremos usa otro limite de prob. posterior 
# Si proba posteriori osea que decrece es 0.9
sum(lda.pre$posterior[,1]>=.9)
## Quadratic Discriminant Analysis
qda.fit<-qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
#Cuanto me equivoco?
1-mean(qda.class==Direction.2005)
## K-NEAREST NEIGHBORS
train.x=cbind(Lag1,Lag2)[train,]
test.x=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]
# Sea seed bc if we have a tie,R chose randomly
knn_function<-function(x){
a=x
for(i in 1:length(a)){
set.seed(1)
knn.pred=knn(train.x,test.x,train.Direction,k=a[i])
table(knn.pred,Direction.2005)
#Cachando que waa
print(mean(knn.pred==Direction.2005))}}
knn_function(1:23)
## An application to Caravan Insurance Data
attach(Caravan)
fix(Caravan)
summary(Purchase) # only 6% of people purchased caravan insurance.
# Problema de escala en la variables, diferencia entre escalas de variable
# Salario y edad son muy grandes por example.
# STANDARIZED
standardized.X=scale(Caravan [,-86]) #N(0,1) y 86 es purchase
#Ajustamos training y test data
test=1:1000
train.x=standardized.X[-test,]
test.x=standardized.X[test,]
train.y=Purchase[-test]
test.y=Purchase[test]
knn_function<-function(x){
  a=x
  for(i in 1:length(a)){
    set.seed(1)
    knn.pred=knn(train.x,test.x,train.y,k=a[i])
    print(mean(test.y!=knn.pred))
    print(mean(test.y!='No'))
    print(table(knn.pred,test.y))}}
knn_function(1:5)
# Comparando con logistic reg
glm.fits=glm(Purchase~.,data=Caravan ,family =binomial,subset=-test)
glm.probs=predict(glm.fits,Caravan[test,],type='response')
glm.pred=rep('No',1000)
glm.pred[glm.probs>.12]='Yes'
table(glm.pred,test.y)
mean(glm.pred==test.y)
