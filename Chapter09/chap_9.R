# Install ldhmm package
if(!require('ldhmm')){install.packages('ldhmm')}
# attach the package
library(ldhmm)
# load data
dt <- ldhmm.ts_log_rtn(symbol = 'spx',
                       on = 'days')
# simple plot 1
plot(x = dt$d, y= dt$p, type = 'l', 
     ylab = 'SPX closing prices',
     xlab = 'time')
# elaborated plot 1
library(ggplot2)
library(ggthemes)
ggplot(data = data.frame(x = dt$d, y = dt$p)) +
  geom_line(aes(x = x, y = y), size = 1.5) +
  ylab('SPX closing prices') +
  xlab('time') +
  theme_pander(base_size = 16)
# simple plot 2
plot(x = dt$d, y= dt$x, type = 'l', 
     ylab = 'SPX log return',
     xlab = 'time')
# elaborated plot 2
ggplot(data = data.frame(x = dt$d, y = dt$x)) +
  geom_line(aes(x = x, y = y), size = 1.1) +
  ylab('SPX log return') +
  xlab('time') +
  theme_pander(base_size = 16)
# define states
states <- 2
# set 9 as the seed and use k-menas clustering
set.seed(9)
k_2 <- kmeans(dt$x, 2)
# plot the result using ggplot2
ggplot(data = data.frame(x = dt$d, y = dt$x, k = factor(k_2$cluster))) +
  geom_point(aes(x = x, y = y, color = k), alpha = .4)
# store the mean for each cluster
mean_1 <- mean(dt$x[k_2$cluster == 1])
mean_1
mean_2 <- mean(dt$x[k_2$cluster == 2])
mean_2
# store the standard deviation for each cluster
sd_1 <- sd(dt$x[k_2$cluster == 1])
sd_1
sd_2 <- sd(dt$x[k_2$cluster == 2])
sd_2
# set the initial parameters for the mixing distributions
param <- matrix(c(
  mean_1, sd_1, 1.3,
  mean_2, sd_2, 1.3),
  states, 3, byrow = T)
# initiate
t <- ldhmm.gamma_init(states)
# parametrize the HMM
hmm <- ldhmm(states, param, t, stationary = T)
# optmize the HMM according to data
hmm_mle <- ldhmm.mle(hmm, dt$x, decode = T, print.level = states)
# plot the results
ldhmm.oxford_man_plot_obs(hmm_mle)
# parameters for the mixing distribution
hmm_mle@param
# transitional matrix
hmm_mle@gamma
# vector of initial probabilities
hmm_mle@delta
# empiracal stats for the two states
hmm_mle@states.local.stats
# theoretical stats for the two states
ldhmm.ld_stats(hmm_mle)