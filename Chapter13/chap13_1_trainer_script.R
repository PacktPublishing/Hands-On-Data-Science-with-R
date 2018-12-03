library(gbm)
set.seed(13)
model <- gbm(formula = Class ~ ., 
             data = dataset, 
             distribution = 'bernoulli', 
             n.trees = 3*10^4)