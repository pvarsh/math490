### Packages
#install.packages("glmnet", repos = "http://cran.us.r-project.org")
library(glmnet)

### Generating meaningful predictors

N = 100 # number of observations
p.signal = 3 # number of signal predictors
p.noise = 10 # number of noise predictors
p = p.signal + p.noise

df = data.frame(matrix(nrow = N, ncol = p + 2)) # initialize the data frame to dimensions p + 1, N

set.seed(4321) # setting the seed for reproducibility
for (i in seq(p.signal)){
  df[[i + 1]] = rnorm(N, 0, 1)
}

### Generating noise predictors

for (i in seq(p.signal+2, p.signal + p.noise + 2)){
  df[[i]] = rnorm(N, 0, 1)
}

head(df)


colnames(df)[1] = "Y" # response variable will be in the first column of df

### Generating coefficients
beta = c(1,-1,0.5) # beta should be the same length as p.signal
beta.matrix = t(replicate(N, beta))

### Building the dependent variable
# with error terms
logit.Y = rowSums(beta.matrix * as.matrix(df[2:(1+p.signal)]) + rnorm(N, 0, 1))
# without error terms. Note: will behave weirdly
#logit.Y = rowSums(beta.matrix * as.matrix(df[2:(1+p.signal)]))

Y = as.numeric(logit.Y > 0)
df[[1]] = Y

### Visualizing the relationship between predictors and logit(Y)

par(mfrow = c(3,1))
for (i in 2:4){
  plot(df[[i]], logit.Y, xlab = paste("X", as.character(i), sep = "")) 
}

### Visualizing the relationship between predictors and Y
par(mfrow = c(3,1))
for (i in 2:4){
  plot(df[[i]], df$Y, xlab = paste("X", as.character(i), sep = ""), ylab = "Y")  
}

### Fitting a logistic model without lasso
mod1 = glm(Y ~ X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12, family = binomial(link = "logit"), data = df)
summary(mod1)


### Fitting lasso

# Transforming data to matrix and vector form
# according to the vignette http://www.stanford.edu/~hastie/glmnet/glmnet_alpha.html
# glmnet wants the predictors to be in a matrix and the outcomes as a vector
x = as.matrix(df[2:(p + 2)]) # predictors
y = as.numeric(df[[1]])

# fitting lasso regression
mod.lasso = glmnet(x, y, family = "binomial")
par(mfrow = c(1,1)) # one row one column plot layout
plot(mod.lasso) # plots graph of coefficients as a function of L1 norm
coef(mod.lasso, 0.09)
cv.mod = cv.glmnet(x, y, family = "binomial")
plot(cv.mod)
#cv.mod$lambda.1se
coef(cv.mod, s = "lambda.1se")
coef(cv.mod, s = "lambda.min")
