logit = function(x) {
  return (1/ (1 + exp(-x)))
}

n = 1000
x = sample(c(0,2,4,5),
           size = n,
           replace = TRUE,
           prob = c(0.4, 0.3, 0.2, 0.1))

y = rbinom(n, 1, logit(-3 + 0.3*x) )

df = data.frame(x,y)
sdf = as.data.frame.matrix(table(df))
sdf$snore = c(0,2,4,5)


colnames(sdf) = c("no", "yes", "snore")
sdf = sdf[c("yes", "no", "snore")]
sdf
snoringGLM1 = glm(cbind(yes, no)~snore,
                  family = binomial(link = "identity"),
                  data = sdf )

summary(snoringGLM1)

#glm(cbind(sdf[[0]],sdf[[1]])~sdf$snore, family=binomial(link = 'identity'))

#sdf$yes /(sdf$yes + sdf$no)
