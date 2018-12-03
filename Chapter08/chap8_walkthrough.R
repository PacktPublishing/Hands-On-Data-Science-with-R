# Activation Functions
plot(seq(-3,3,.1),tanh(seq(-3,3,.1)), type = 'l', col = '#e66101', 
     xlab =  'Input (S)', ylab = 'Output (F)', lwd = 2)
lines(seq(-3,3,.1),
      sapply(seq(-3,3,.1), function(x) max(x,0)), 
      col = '#b2abd2',
      lwd = 3)
lines(seq(-3,3,.1),
      sapply(seq(-3,3,.1), function(x) (1/(1+exp(-x)))), 
      col = '#5e3c99',
      lwd = 3)
legend(1 , -.5, 
       legend = c('Tanh','ReLU','Sigmoid'), 
       col = c('#e66101','#b2abd2','#5e3c99'), 
       lty = 1, lwd = 2)
# check the routines to get keras ready
# in another file
# Downloading data
tmp <- tempfile(fileext = '.xls')
url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/00350/default%20of%20credit%20card%20clients.xls'
download.file(url, destfile = tmp, method = 'curl')
# check&install readxl
if(!require(readxl)){ install.packages('readxl')}
# read the file
library(readxl)
dt <- read_xls(tmp, skip = 1)
# unlink(tmp)
# peek-inside
summary(dt)
head(dt)
# design the max-min function
max_min <- function(x){
  return((x - min(x))/(max(x)-min(x)))
}
# make sure your environment is ready
# library(keras)
# transform data
dt[,c(2,6:24)] <- apply(dt[,c(2,6:24)], 2, max_min) # non-categorical: max-min
dt[,c(3:5,25)] <- apply(dt[,c(3:5,25)], 2, to_categorical) # categorical: one-hot encode
# transform 
dt <- as.matrix(dt)
# check failures
apply(dt, 2, sum) != 0
# Remove ID and non existing categories
dt <- dt[,-1]
dt <- dt[,apply(dt, 2, sum) != 0]
# sample training and validation
set.seed(50)
n <- sample(x = 30000, size = 5000)
# split training data - input (dt) / target
train_dt <- dt[-n,1:33]
train_target <- dt[-n,34:35]
# split validation data - input (dt) / target
val_dt <- dt[n,1:33]
val_target <- dt[n,34:35]
# remove the original data to clear some room in the memory
rm(dt)
# design ANN model
heracles_1 <- keras_model_sequential() %>% 
  layer_dense(units = 25, activation = 'relu', input_shape = 33) %>%
  layer_dense(units = 15, activation = 'relu') %>%
  layer_dense(units = 6, activation = 'relu') %>% 
  layer_dense(units = 2, activation = 'softmax')
# check the architeture
summary(heracles_1)
# pick the optmizer (training strategy)
heracles_1 %>% compile(
  optimizer = optimizer_adam(lr = .001),
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
) 
# traing (fit) the model
heracles_1 %>% fit(x = train_dt,y = train_target, 
                   epochs = 10, batch_size = 150, 
                   validation_data = list(val_dt, val_target)) 
# evaluate (training data)
heracles_1 %>% evaluate(train_dt, train_target)
# 25000/25000 ...
# $loss
# [1] 0.4409437
# 
# $acc
# [1] 0.817
# evaluate (validation data)
heracles_1 %>% evaluate(val_dt, val_target)
# 5000/5000 ...
# $loss
# [1] 0.4362364
#
# $acc
# [1] 0.8224
# benchmarking
mean(train_target[,1])
mean(val_target[,1])
# save the trained model
save_model_hdf5(heracles_1, filepath = 'heracles_1.hdf5',
                overwrite = T, include_optimizer = T)
# if you remove it
rm(heracles_1)
# you can easily load again
heracles_1 <- load_model_hdf5(filepath = 'heracles_1.hdf5')
# alternative architeture (leaky ReLU and dropout)
heracles_2 <- keras_model_sequential() %>% 
  layer_dense(units = 25, input_shape = 33) %>%
  layer_activation_leaky_relu(alpha = .3) %>% 
  layer_dropout(rate = .2) %>% 
  layer_dense(units = 15) %>%
  layer_activation_leaky_relu(alpha = .3) %>% 
  layer_dropout(rate = .1) %>% 
  layer_dense(units = 6) %>% 
  layer_activation_leaky_relu(alpha = .3) %>% 
  layer_dropout(rate = .05) %>% 
  layer_dense(units = 2, activation = 'softmax')
# quiz answers
set.seed(8)
round(runif(3,1,4))