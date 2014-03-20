logit = function(x) {
  return(1/(1 + exp(0.5 + 1.1 * x)))
}

n = 1000
y = rbinom(200, 1, .55)
x = sample(c(0,2,4,5), size = n, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1))

y = rbinom(n, 1, logit(0.03 + 0.1*x))

df = data.frame(x, y)
sdf = as.data.frame(matrix(table(df)))
glm(df)

