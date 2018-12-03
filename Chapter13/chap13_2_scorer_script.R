library(gbm)
probabilities <- predict.gbm(model, 
                             newdata = dataset, 
                             n.trees = 3*10^4, 
                             type = 'response')
class <- as.factor(as.numeric(probabilities >= 0.5))
scores <- data.frame(Predicted = class, Probabilities = probabilities)