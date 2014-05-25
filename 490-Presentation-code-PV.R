############# LASSO PRESENTATION EXAMPLES
############# MATH 490
############# Mirai Furukawa, Scott Thomas, Peter Varshavsky
############# Code by Peter Varshavsky
############# Using examples from the glmnet vignette

#############
# For more information on glmnet package, visit the vignette at http://www.stanford.edu/~hastie/glmnet/glmnet_alpha.html
# Examples in the presentation are borrowed from:
#     An Introduction to Statistical Learning by Gareth James, Daniela Witten, Trevor Hastie, Robert Tibshirani
#     Machine Learning class by Professor Andrew Ng available for free at https://www.coursera.org/course/ml
#
# In this demonstration we generate
#     p = p.signal + p.noise independent N(0, 1) variables
#     y is then generated as a logit of a linear combination of signal variables
#       with 0.5 cutoff. That is if Y = 1 if logit(Y) >= 0.5 and Y = 0 otherwise
#     
# noise variables are present to show how lasso performs variable selection
# because the variables are generated randomly, some noise variables may
# actually exhibit high correlation with Y. In that case they may be kept by the
# lasso package, even though we know that the relationship is completely spurious
# since, unlike in real world, in simulations we know the true model
# 
# Although variables are pseudo random, the program generates the same values every time it runs
# To try the program with other random values, change the seed, or just comment out the seed line
#
#
#############

### Loading Packages
#install.packages("glmnet", repos = "http://cran.us.r-project.org")
library(glmnet)


### Generating meaningful predictors

N = 100 # number of observations
p.signal = 2 # number of signal predictors. If you change this parameter, you need to change the length of the beta vector in the GENERATING COEFFICIENTS section
p.noise = 18 # number of noise predictors
p = p.signal + p.noise

df = data.frame(matrix(nrow = N, ncol = p + 2)) # initialize the data frame to dimensions p + 1, N

set.seed(4321) # setting the seed for reproducibility
for (i in seq(p.signal)){ #this loop generates the variables that will be used to model Y
  df[[i + 1]] = rnorm(N, 0, 1)
}
head(df)

### Generating noise predictors
for (i in seq(p.signal+2, p.signal + p.noise + 2)){ # this loop generates variables that will not be used in modeling Y (noise variables)
  df[[i]] = rnorm(N, 0, 1)
}

colnames(df)[1] = "Y" # response variable will be in the first column of df

### Generating coefficients
beta = c(2,-1.5) # beta should be the same length as p.signal
beta.matrix = t(replicate(N, beta))

### Building the dependent variable
# with error terms
logit.Y = rowSums(beta.matrix * as.matrix(df[2:(1+p.signal)]) + rnorm(N, 0, 1))
Y = as.numeric(logit.Y > 0)
df[[1]] = Y

### Visualizing the relationship between predictors and logit(Y)
head(df)
par(mfrow = c(2,1))
for (i in 2:3){
  plot(df[[i]], logit.Y, xlab = paste("X", as.character(i), sep = "")) 
}

### Visualizing the relationship between predictors and Y
par(mfrow = c(2,1))
for (i in 2:3){
  plot(df[[i]], df$Y, xlab = paste("X", as.character(i), sep = ""), ylab = "Y")  
}

### Fitting a logistic model without lasso
mod1 = glm(Y ~ X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14, family = binomial(link = "logit"), data = df)
summary(mod1)


### Fitting lasso

# Transforming data to matrix and vector form
# according to the vignette http://www.stanford.edu/~hastie/glmnet/glmnet_alpha.html
# glmnet wants the predictors to be in a matrix and the outcomes as a vector
x = as.matrix(df[2:(p + 2)]) # predictors
y = as.numeric(df[[1]])

# fitting lasso regression
mod.lasso = glmnet(x, y, family = "binomial", alpha = 1) # parameter alpha specifies the type of regularization, alpha = 0 for ridge, alpha = 1 for lasso
par(mfrow = c(1,1)) # one row one column plot layout
plot(mod.lasso) # plots graph of coefficients as a function of L1 norm
coef(mod.lasso, 0.09)
cv.mod = cv.glmnet(x, y, family = "binomial")
plot(cv.mod)
cv.mod$lambda.1se
coef(cv.mod, s = "lambda.1se")
coef(cv.mod, s = "lambda.min")
cor(logit.Y, x)
