### Packages


### Generating meaningful predictors

N = 1000 # number of observations
p.signal = 3 # number of signal predictors
p.noise = 40 # number of noise predictors
p = p.signal + p.noise

df = data.frame(matrix(nrow = N, ncol = p + 1)) # initialize the data frame to dimensions p + 1, N

#set.seed(4321) # setting the seed for reproducibility
for (i in seq(p.signal)){
  df[[i + 1]] = rnorm(N, 0, 1)
}

colnames(df)[1] = "Y" # response variable will be in the first column of df

# generating coefficients
beta = c(10,-4,3) # beta should be the same length as p.signal
beta.matrix = t(replicate(N, beta))

# with error terms
logit.Y = rowSums(beta.matrix * as.matrix(df[2:(1+p.signal)]) + rnorm(N, 0, 1))
# without error terms
#logit.Y = rowSums(beta.matrix * as.matrix(df[2:(1+p.signal)]))

Y = as.numeric(logit.Y > .5)
df[[1]] = Y

cor(df$Y, df$X4)
par(mfrow = c(3,1))
for (i in 2:4){
  plot(df[[i]], df[[1]])  
}

mod1 = glm(Y ~ X2 + X3 + X4, family = binomial(link = "logit"), data = df)
summary(mod1)
### Generating noise predictors


### Fitting