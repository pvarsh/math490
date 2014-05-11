### Packages


### Generating meaningful predictors

N = 100 # number of observations
p.signal = 3 # number of signal predictors
p.noise = 40 # number of noise predictors
p = p.signal + p.noise

df = data.frame(matrix(nrow = N, ncol = p + 1)) # initialize the data frame to dimensions p + 1, N

set.seed(4321) # setting the seed for reproducibility
for (i in seq(p.signal)){
  df[[i + 1]] = rnorm(N, 0, 1)
}

#beta = runif(n = p.signal, min = 1, max = 3)
beta = c(1,2,3)
beta.matrix = t(replicate(N, beta))
beta.matrix
y = beta.matrix * as.matrix(df[2:(1+p.signal)]) + rnorm(N, 0, .3)
head(y)


### Generating noise predictors


### Fitting