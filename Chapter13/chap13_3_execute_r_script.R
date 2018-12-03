test_set <- maml.mapInputPort(1)

hit_rate <- mean(test_set$Class == test_set$Predicted)
hit_rate <- data.frame(hit_rate)

maml.mapOutputPort("hit_rate");