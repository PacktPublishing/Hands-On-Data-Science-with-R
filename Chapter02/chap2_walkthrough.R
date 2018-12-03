## Generating samples
set.seed(10)
small_sample <- rnorm(n = 10, mean = 10, sd = 5)
big_sample <- rnorm(n = 10^5, mean = 10, sd = 5)

## Mean, Median and Mode
mean(small_sample, na.rm = T)
mean(big_sample, na.rm = T)

median(small_sample, na.rm =T)
median(big_sample, na.rm = T)

find_mode <- function(vals) {
  if(max(table(vals)) == min(table(vals)))
    'amodal'
  else
    names(table(vals))[table(vals)==max(table(vals))]
}

find_mode(big_sample)
find_mode(round(big_sample))


## Summaries
if(!require(psych)){ install.packages('psych')}
psych::harmonic.mean(big_sample)
psych::geometric.mean(big_sample)

sd(big_sample)
var(big_sample)

pkgs <- c('psych','Hmisc','pastecs')
pkgs <- pkgs[!(pkgs %in% installed.packages())]
if(length(pkgs) != 0) {install.packages(pkgs)}
rm(pkgs)

psych::describe(big_sample)
Hmisc::describe(big_sample)
pastecs::stat.desc(big_sample)

## Normal and t distributions
x <- seq(-4,4,.1)
par(lwd = 2)
plot(x, dnorm(x), type = 'l', ylab = 'density',
     main = 'prob. density distributions')
lines(x, dt(x, 5), col = '#e66101', lty = 2)
lines(x, dt(x, 10), col = '#5e3c99', lty = 3)
legend('topright', 
       legend = c('normal','t-student (df = 5)', 't-student (df = 10)'), 
       col = c('#000000','#e66101','#5e3c99'), lty = 1:3)


# t-tests
t.test(big_sample, mu = 10, alternative = 'two.sided')

t.test(small_sample, mu = 10, alternative = 'two.sided')

t.test(x = small_sample, y = big_sample, var.equal = T)

# z-test
z.test <- function(sample, mu, sigma, alternative = 'two.sided'){
  n <- length(sample)
  xbar <- mean(sample, na.rm =T)
  z <- (xbar - mu)/(sigma/sqrt(n))
  
  if(alternative == 'two.sided'){
    p.value <- 2*pnorm(-abs(z))
  }
  else if(alternative == 'greater'){
    p.value <- pnorm(z, lower.tail = F)
  }
  else if(alternative == 'less'){
    p.value <- pnorm(z)
  }
  else{stop('alternative is missepecified, accepted values are 
            \'two.sided\',\'greater\' or \'less\'\n')}
  
  cat('z = ', z, ' obs. = ', n, ' p-value = ', p.value, '\n')
  cat('mean of x\n', xbar, '\n')
}

# or source(url('https://bit.do/z_test')) to read it remotely
# Results are pretty similar for big samples
t.test(big_sample, mu = 10)
z.test(big_sample, mu = 10, sigma = 5)
# Results are not so similar for smaller samples
t.test(small_sample, mu = 10)
z.test(small_sample, mu = 10, sigma = 5)
# If we trusted a sd estimator as sigma we would be dammmed
# z.test(small_sample, mu = 10 , sigma = sd(small_sample))
# z =  -2.216899  obs. =  10  p-value =  0.02663 
# alternative hypothesis: true mean is  not equal  10 
# 95  percent confidence interval: 
#   5.377763   9.715668 
# mean of x
# 7.546716 


# A/B testing
control <- c(130, 9870)
variant <- c(170, 9830)
tab <- rbind(control, variant)

fisher.test(tab, alternative = 'less')
power.prop.test(p1 = 130/(130+9870), p2 = 170/(170+9830), 
                power = 0.8, sig.level = 0.012,
                alternative = 'one')