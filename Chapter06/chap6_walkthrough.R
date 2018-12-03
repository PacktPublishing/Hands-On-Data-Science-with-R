## Linear Regression with R
# install car package
if(!require(car)){install.packages('car')}
# load car and peek UN dataset
library(car)
dim(UN)
head(UN)
# filtering NAs
dt <- (UN[!is.na(UN[,4]) 
          & !is.na(UN[,7]), c(7,4)])
dim(dt)
# plot relations
plot(y = dt[,1],x = dt[,2],
     ylab = 'infant deaths/1,000 live birts',
     xlab = 'Per capita GDP (U.S. dollars)')
# examing a transformation
plot(y = dt[,1], x = log(dt[,2]), 
     ylab = 'infant deaths/1,000 live birts',
     xlab = 'Per capita GDP (growing rate)')
# aplying the log 4 times in a row
plot(y = dt[,1], x = log(log(log(log(dt[,2])))), 
     ylab = 'infant deaths/1,000 live birts',
     xlab = 'hardly meaningful variable')
# creating transformed variable
dt$log_gdp <- log(dt$ppgdp)
# partioning data
set.seed(5)
n <- sample(dim(dt)[1], size = 40)
dt_est <- dt[-n,]
dt_tst <- dt[n,]
# running the regression
reg <- lm(infantMortality ~ log_gdp, data = dt_est)
# detailed summary
summary(reg)
# calculating MSE
mean(residuals(reg)^2)
mean(reg$residuals^2)
# out of sample MSE
out <- predict(reg, newdata = dt_tst)
mean((dt_tst[,1] - out)^2)
# another way to do it
# set.seed(5)
# n <- sample(dim(dt)[1], size = 40)
reg <- lm(infantMortality ~ log(ppgdp), data = dt[-n,])
out <- predict(reg, newdata = dt[n,])

## Decision trees
# Chile data set - uncomment to install car
# install.packages('car')
library(car)
?Chile
head(Chile)
# sampling data
dt_Chile <- Chile[complete.cases(Chile),]
set.seed(50)
i_out <- sample(dim(dt_Chile)[1], 
                size = round(dim(dt_Chile)[1]*.3))
# spliting out data
val <- i_out[ 1 : (length(i_out) %/% 2)]
test <- i_out[ (length(i_out) %/% 2 + 1) : length(i_out)]
# growing a decision tree with tree
set.seed(10)
if(!require(tree)){install.packages('tree')}
library(tree)
tree_tree <- tree(vote ~ . ,
                  data = dt_Chile[-i_out,],
                  method = 'class',
                  mindev = 0)
# visualization
plot(tree_tree)
text(tree_tree, cex = .7)
# errors and quick summary
summary(tree_tree)
# decision tree with rpart
if(!require(rpart)){install.packages('rpart')}
library(rpart)
tree_rpart <- rpart(vote ~ ., 
                    data = dt_Chile[-i_out,],
                    method = 'class',
                    control=rpart.control(cp = 0))
# misclassification error rate
mean(residuals(tree_rpart))
# Visualization
if(!require(rattle)){install.packages('rattle')}
if(!require(RColorBrewer)){install.packages('RColorBrewer')}
library(rattle)
fancyRpartPlot(tree_rpart, sub = '')
# or plot(tree_rpart) ; text(rpart, cex = .7)
# predict and test hit rate with tree package
predict(tree_tree, type = 'class', 
        newdata = dt_Chile[test,])
mean(predict(tree_tree, type = 'class', 
             newdata = dt_Chile[test,]) == dt_Chile[test, 'vote'])

predict(tree_rpart, type = 'class', 
        newdata = dt_Chile[test,])
mean(predict(tree_rpart, type = 'class', 
             newdata = dt_Chile[test,]) == dt_Chile[test, 'vote'])
# pruning tree
p_tree <- prune.misclass(tree_tree, 
                         best = 5,
                         newdata = dt_Chile[val,])

mean(predict(p_tree, type = 'class', 
             newdata = dt_Chile[test,]) == dt_Chile[test, 'vote'])

plot(p_tree);text(p_tree, cex = .7)
# pruning rpart
p_rpart <- prune(tree_rpart, cp = .01)

mean(predict(p_rpart, type = 'class', 
             newdata = dt_Chile[test,]) == dt_Chile[test, 'vote'])
# plot
# library(rattle)
fancyRpartPlot(p_rpart, sub = '')
# another way to do it
tree_tree2 <- tree(vote ~ . ,
                   data = dt_Chile[-i_out,],
                   method = 'class',
                   mindev = .01)
mean(predict(tree_tree2, type = 'class', 
             newdata = dt_Chile[test,]) == dt_Chile[test, 'vote'])
tree_rpart2 <- rpart(vote ~ ., 
                     data = dt_Chile[-i_out,], 
                     method = 'class',
                     control=rpart.control(cp = .01))
mean(predict(tree_rpart2, type = 'class', 
             newdata = dt_Chile[test,]) == dt_Chile[test, 'vote'])
# much easir to visualize
# plot(tree_tree2)
# text(tree_tree2, cex = .7)
 
## Random Forest
# installing randomForest
if(!requireNamespace('randomForest', quietly = T)){
  install.packages('randomForest')}
# load the package and set a seed
library(randomForest)
set.seed(1999)
# fitting a random forest
time0_rf <- Sys.time()

rf_model <- randomForest(vote ~ . , 
                         data = dt_Chile[-i_out ,], 
                         ntree = 450, 
                         na.action = na.exclude)

time1_rf <- Sys.time()

mean(predict(rf_model, type = 'class', 
             newdata = dt_Chile[test,]) == dt_Chile[test, 'vote'])
# [1] 0.6547945
time1_rf - time0_rf
# print(rf_model)
# Time difference of 1.061416 secs // YOU MIGHT NOT GET THE SAME
# installing and loading ipred
if(!require(ipred)){install.packages('ipred')}
library(ipred)
# bagging with ipred
time0_bag <- Sys.time()

bag_model <- bagging(vote ~ ., data = dt_Chile[-i_out ,])

time1_bag <- Sys.time()
# checking test sample hit rate
mean(predict(bag_model, type = 'class', 
             newdata = dt_Chile[test,]) == dt_Chile[test, 'vote'])
# [1] 0.6356164
time1_bag - time0_bag
# Time difference of 0.350996 secs
# install and laod gbm
if(!require(gbm)){install.packages('gbm')}
library(gbm)
# fit a generalized boosted model
time0_gbm <- Sys.time()

gbm_model <- gbm(formula = formula(dt_Chile[,c(8, 1:7)]), 
                 data = dt_Chile[-i_out ,], 
                 distribution = 'multinomial',
                 n.trees = 450)

time1_gbm <- Sys.time()
# calculating the hit rate
pred_gbm <- predict(gbm_model, 
                    type = 'response', 
                    n.trees = 450, 
                    newdata = dt_Chile[test,])
# coercing into a data frame
pred_gbm <- as.data.frame(pred_gbm[,,1])
# rowSums(pred_gbm) should give 1 every time
pred_gbm <- names(pred_gbm)[max.col(pred_gbm, 'first')]
# measuring hit rate and running time
mean(pred_gbm == dt_Chile[test, 'vote'])
# [1] 0.6630137
time1_gbm - time0_gbm
# Time difference of 0.9355559 secs

## Support Vector Machines (SVM)
# check-install
if(!require('lubridate')){install.packages('lubridate')}
if(!require('caret')){install.packages('caret')}
# loading lubridate and setting a SNG (seed number generator)
library(caret)
set.seed(2018)
# creating a tune grid
tune_grid <- expand.grid(sigma = c(.025,.0025,.001), C = c(1,2))
# training a SVM model
time0_svm <- Sys.time()

svm_caret <- caret::train(vote ~ . , 
                          data = dt_Chile[-i_out,], 
                          tuneGrid = tune_grid,
                          method = 'svmRadial')

time1_svm <- Sys.time()
# checking accuracy
mean(predict(svm_caret, newdata = dt_Chile[-i_out,]) == dt_Chile[-i_out, 'vote'])
# [1] 0.6721504
mean(predict(svm_caret, newdata = dt_Chile[test,]) == dt_Chile[test, 'vote'])
# [1] 0.6931507
time1_svm - time0_svm
# Time difference of 1.108781 mins

## Clustering
# calculating distance
dist(dt_Chile[1:5, c(2,4,6,7)])
# storing clust
h_clust <- hclust(dist(dt_Chile[1:10, c(2,4,6,7)]))
# Plotting results
plot(h_clust)
# Better plot
plot(h_clust, hang = .2)
abline(h = 5000, col = 'red', lwd = 2)
# transforming data
dt_z <- apply(dt_Chile[,c(2,4,6,7)], MARGIN = 2, 
              FUN = function(x){ xbar <- mean(x); s <- sd(x); return((x-xbar)/s)}
              )
# viz - orignal x standardized data
h_clust_z <- hclust(dist(dt_z[1:10,]))

par(mfrow = c(1,2))

plot(h_clust, hang = .3, 
     labels = dt_Chile[1:10, 'vote'], 
     main = 'Cluster\n(Original data)')

plot(h_clust_z, hang = .3,
     labels = dt_Chile[1:10, 'vote'], 
     main = 'Cluster\n(Standardized data)')
# cluster the whole data and cut the tree
h_clust_z2 <- hclust(dist(dt_z), method = 'ward.D')
cutree(h_clust_z, k = 4)
# simple check
table(cutree(h_clust_z2, k = 4)[-i_out],
      dt_Chile[-i_out, 'vote'])
# simple-heuristic, hit rate
mean(
  sapply(factor(cutree(h_clust_z2, k = 6))[test], 
         FUN = function(x){
           if(x == 1 | x == 3 | x == 4) return('Y')
           else return('N')}         ) 
  == dt_Chile[test, 'vote'])
# [1] 0.5232877
# trying the feature
dt_Chile$hc_cluster <- factor(cutree(h_clust_z2, k = 4))

#library(randomForest)
set.seed(2010)

rf2 <- randomForest(vote ~ . , dt_Chile[-i_out,])
mean(predict(rf2, type = 'class', newdata = dt_Chile[test,]) == dt_Chile[test,'vote'])
# [1] 0.6520548
## K Means Clustering
# install.packages('factoextra')
library(factoextra)
fviz_nbclust(dt_z, kmeans, method = 'wss')
# k-means clustering
set.seed(10)
k_clust <- kmeans(dt_z, 3)
# cluster caracterization
aggregate(dt_Chile[, c(2,4,6,7)], 
          by = list(clusters = fitted(k_clust, method = 'classes')), 
          mean)
# creating mode function
find_mode <- function(vals) {
  if(max(table(vals)) == min(table(vals)))
    'amodal'
  else
    names(table(vals))[table(vals)==max(table(vals))]
}
# meam or mode function
mean_or_mode <- function(x){
  if(is.numeric(x)) return(mean(x))
  else return(find_mode(x))
}
# adressing a customized summary
aggregate(dt_Chile, 
          by = list(clusters = k_clust$cluster), 
          mean_or_mode) 
# testing k-means cluster features
dt_Chile$k_cluster <- fitted(k_clust, method = 'classes')

set.seed(2010)

rf3 <- randomForest(vote ~ . , dt_Chile[-i_out,-9])
mean(predict(rf3, type = 'class', newdata = dt_Chile[test,]) == dt_Chile[test,'vote'])
# fuzzy k-means clustring
if(!require('fclust')){install.packages('fclust')}
# clustering
library(fclust)
set.seed(10)
f_clust <- FKM.ent(dt_z, k = 4, conv = 1e-16)
# testing fuzzy k-means
dt_Chile$f_cluster <- factor(f_clust$clus[,1])
set.seed(2010)
#library(randomForest)
rf4 <- randomForest(vote ~ . , dt_Chile[-i_out,c(-9,-10)])
mean(predict(rf4, type = 'class', newdata = dt_Chile[test,]) == dt_Chile[test,'vote'])
# Neural Networks
if(!require('h2o')){install.packages('h2o')}
# loading initializing h2o
library(h20)
h2o.init(nthreads=-1, max_mem_size='2G')
# training nn
time0_nn <- Sys.time()

nn <- h2o.deeplearning(x = 1:7,
                       y = 8,
                       training_frame = as.h2o(dt_Chile[-i_out,1:8]),
                       validation_frame = as.h2o(dt_Chile[val,1:8]),
                       hidden = c(6,6),
                       standardize = T,
                       activation = 'Tanh',
                       l2 = .0025, epochs = 50,
                       reproducible = T,
                       seed = 10)

time1_nn <- Sys.time()
# visualizing the training
plot(nn)
# evaluating resutls
nn_pred <- h2o.predict(nn, newdata = as.h2o(dt_Chile[test,1:8]))
nn_pred <- as.matrix(nn_pred)
mean(nn_pred[,1] == dt_Chile[test,'vote'])
# [1] 0.6849315
time1_nn - time0_nn
# Time difference of 2.173553 secs
# Quiz answers
set.seed(2000)
round(runif(3,1,4))

