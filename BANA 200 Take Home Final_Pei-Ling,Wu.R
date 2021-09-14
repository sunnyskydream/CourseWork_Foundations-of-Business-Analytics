#load data
setwd("C:/Ashley/UCI MSBA/course/summer 2021/BANA 200 FNDTNS BUS ANALYTCS/BANA 200 Take Home Final")
starbucks <- read.table("Starbucks HW2 Data.txt", header = T, sep="\t")
attach(starbucks)
options(scipen=999)

#Q1 a split training and test
K <- 5000
N <- nrow(starbucks)

train <- as.data.frame(starbucks[1:K,])
test <- as.data.frame(starbucks[(K+1):N,])
test.Y <- test$recommend
test.X <- test[,1:22]

#Q1 b training model
train.reg <- lm(recommend ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21+X22, data = train)
summary(train.reg)          
sort(round(coef(summary(train.reg))[(2:23),4],4))

#Q1 c model on test samples
test.preds <- as.vector(predict(object = train.reg, newdata = test.X))
test.preds

library(miscTools)
test.r2 <- rSquared(y = test.Y, resid = (test.Y - test.preds))
test.r2

train.r2 <- rSquared(y=train$recommend, resid = resid(train.reg))
train.r2

round(train.r2,4)
round(test.r2,4)

round(train.r2,4) - round(test.r2,4)
(round(train.r2,4)-round(test.r2,4))/round(train.r2,4)

#Q2 a forward selection
starbucks2 <- cbind(train[24], train[,1:22])

null.model <- lm(recommend ~ 1, data = starbucks2)
summary(null.model)

full.model <- lm(recommend ~ ., data = starbucks2)
summary(full.model) 

forward.results <- step(object=null.model, direction="forward", scope=formula(full.model))
summary(forward.results)

forward.vars.used <- as.data.frame(sort(names(coef(forward.results)[-1])))
colnames(forward.vars.used) <- "Forward Selection"
forward.vars.used <- cbind.data.frame(forward.vars.used, "Forward")
forward.vars.used

full.vars.used <- as.data.frame(sort(names(coef(full.model)[-1])))
colnames(full.vars.used) <- "Full model"
full.vars.used <- cbind.data.frame(full.vars.used, "Full")
full.vars.used

vars.selected <- merge(x=forward.vars.used, y=full.vars.used, by.x="Forward Selection", by.y="Full model", all=TRUE)
vars.selected 

#Q3 a
X <- as.matrix(starbucks[,1:22])

#Q3 b optimal number of clusters 
library(cluster)
library(NbClust)
library(factoextra)
library(ggplot2)
library(broom)

fviz_nbclust(x=X, FUNcluster = kmeans, nstart=100, method="wss", k.max = 10) + 
  labs(title="Optimal Number of Clusters: Elbow Plot") + 
  coord_cartesian(ylim=c(0,12000)) + geom_line(size=2)

nb <- NbClust(X, distance="euclidean", min.nc=2, max.nc=10, method="kmeans")
fviz_nbclust(nb)

#Q3 c k-means cluster analysis
cluster.results = kmeans(x = X, centers = 2, iter.max=1000, nstart=100)


cluster.numbers = cluster.results$cluster
cluster.numbers

segment_sizes = table(cluster.numbers)
segment_sizes

#Q3 d
(round(cluster.results$centers[,1:5],2))

#Q3 e
starbucks3 <- cbind(starbucks,cluster.numbers)
most_satisfied <- subset(starbucks3,cluster.numbers == 1)
all_other <- subset(starbucks3,cluster.numbers == 2)

most_satisfied.reg <- lm(recommend ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21+X22, data = most_satisfied)
all_other.reg <- lm(recommend ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21+X22, data = all_other)
summary(most_satisfied.reg)
summary(all_other.reg)

round(mean(fitted.values(most_satisfied.reg)),2)
round(mean(fitted.values(all_other.reg)),2)

#Q4
all_other_adjusted <-all_other

z <- c(1,2,7,8,10)

for (j in z){
  for (i in 1:nrow(all_other_adjusted)){
    if (all_other_adjusted[,j][i] ==5){
      all_other_adjusted[,j][i] <- all_other_adjusted[,j][i] 
    }
    else{
      all_other_adjusted[,j][i] <- all_other_adjusted[,j][i]+1
    }
  }
}

cbind(all_other_adjusted[1:25,c(1,2,7,8,10)], all_other[1:25,c(1,2,7,8,10)])

adjusted.Y <- all_other_adjusted$recommend
adjusted.X <- all_other_adjusted[,1:22]

all_other_adjusted.pred <- as.vector(predict(object=all_other.reg, newdata=adjusted.X))
all_other_adjusted.pred

round(mean(all_other_adjusted.pred),2)
