library(ISLR)
library(MASS)
require(tree)
attach(Carseats)
High=ifelse(Sales<=8,'No','Yes')
High=as.factor(High)
Carseats=data.frame(Carseats,High)
tree_carseats=tree(High???.-Sales,data=Carseats)
summary(tree_carseats)
plot(tree_carseats)
text(tree_carseats,pretty=0)
#Estimates de test error
set.seed(2)
train=sample(1:nrow(Carseats),200)
Carseats_test=Carseats[-train,]
High_test=High[-train]
tree_carseats=tree(High???.-Sales ,Carseats ,subset =train )
tree_pred=predict(tree.carseats ,Carseats.test ,type ="class")
table(tree_pred,High.test)
set.seed(3)
cv_carseats=cv.tree(tree_carseats ,FUN=prune.misclass )
names(cv_carseats)
cv_carseats
#The lowest Cv errors rate is with 74 and 21 nodes
par(mfrow=c(1,2))
plot(cv_carseats$size,cv_carseats$dev,type="b")
plot(cv_carseats$k ,cv_carseats$dev ,type="b")
#now prunet the tree
prune_carseats =prune.misclass(tree_carseats,best=21)
plot(prune_carseats)
text(prune_carseats,pretty=0)
#predict
tree_pred=predict(prune_carseats,Carseats_test,type="class")
table(tree_pred ,High_test)
#Fitting Regression trees
set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)
tree_boston=tree(medv???.,Boston ,subset=train)
summary(tree_boston)
plot(tree_boston)
text(tree_boston,pretty =0)
#Use cv for pruning
cv_boston =cv.tree(tree_boston)
#USE 7
prune_boston =prune_tree(tree_boston,best=7)
plot(prune_boston)
text(prune_boston,pretty =0)
plot(cv_boston$size,cv_boston$dev,type='b')
yhat=predict(tree_boston,newdata=Boston[-train ,])
boston_test=Boston[-train,"medv"]
plot(yhat,boston_test)
abline(0,1)
mean((yhat-boston_test)^2)
#BAGGING AND RANDOM FOREST
#baggin==random_forest si m=p
library(randomForest)
set.seed(3)
bag_boston=randomForest(medv???.,data=Boston ,subset=train,
                           mtry=13,importance=TRUE)
bag_boston
#bagged regression tree for test
yhat_bag=predict(bag_boston,newdata=Boston[-train,])
plot(yhat_bag,boston_test)
abline(0,1)
mean((yhat_bag-boston_test)^2)
#Changing the ntree argument:
bag_boston=randomForest(medv???.,data=Boston ,subset =train ,
                           mtry=13, ntree =25)
yhat_bag = predict(bag_boston,newdata=Boston[-train,])
mean((yhat_bag-boston_test)^2)
#RANDOM forest
set.seed(1)
rf_boston=randomForest(medv???.,data=Boston,subset=train ,
                          mtry=6,importance =TRUE)
yhat_rf=predict(rf_boston,newdata=Boston[-train,])
mean((yhat_rf-boston_test)^2)
#importance por aech 
importance(rf_boston)
#el plot
varImpPlot(rf_boston )
#Boosting
library(gbm)
set.seed(1)
boost_boston=gbm(medv???.,data=Boston[train,],distribution="gaussian",n.trees =5000 ,interaction.depth =4)
#interation.depth: limite permitido para cada node
summary(boost_boston)
par(mfrow=c(1,2))
plot(boost_boston,i="rm")
plot(boost_boston,i="lstat")
#test error
yhat_boost=predict(boost_boston,newdata=Boston[-train ,],
                    n.trees =5000)
mean((yhat_boost-boston_test)^2)
#Puedo cambiar el shrinkage
boost_boston=gbm(medv???.,data=Boston[train,],distribution="gaussian",n.trees =5000, 
                 interaction.depth=4,shrinkage=0.2,verbose =F)
yhat_boost=predict(boost_boston ,newdata =Boston[-train ,],
                      n.trees =5000)
mean((yhat_boost-boston_test)^2)