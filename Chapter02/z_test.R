z.test <- function(sample, mu, sigma, conf.lvl = .95,
                   alternative = 'two.sided'){
  n <- length(sample)
  xbar <- mean(sample, na.rm =T)
  z <- (xbar - mu)/(sigma/sqrt(n))
  if(alternative == 'two.sided'){
    p.value <- 2*pnorm(-abs(z))
    alt <- 'not equal '
    err <- -qnorm((1-conf.lvl)/2)*sigma/sqrt(n)
    a <- xbar - err
    b <- xbar + err
  }
  else if(alternative == 'greater'){
    p.value <- pnorm(z, lower.tail = F)
    alt <- 'greater than '
    err <- qnorm(conf.lvl)*sigma/sqrt(n)
    a <- xbar - err
    b <- 'Inf'
  }
  else if(alternative == 'less'){
    p.value <- pnorm(z)
    alt <- 'less than '
    err <- qnorm(conf.lvl)*sigma/sqrt(n)
    a <- '-Inf'
    b <- xbar + err
  }
  else{stop('alternative is missepecified. Accepted values are',
            ' \'two.sided\',\'greater\' or \'less\'\n')}
  
  cat('z = ', z, ' obs. = ', n, ' p-value = ', p.value, '\n')
  cat('alternative hypothesis: true mean is ', alt , mu, '\n')
  cat(conf.lvl*100, ' percent confidence interval: \n')
  cat(a,' ',b, '\n')
  cat('mean of x\n', xbar, '\n')
}