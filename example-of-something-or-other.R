N = 100

X1 = rnorm(N,0,1)
X2 = rnorm(N,0,1)
X3 = rnorm(N,0,1)
beta1 = 2
beta2 = -3
beta3 = 2
Y = beta1*X1 + beta2*X2 + beta3*X3 + rnorm(N,0,1)
Y.bin = as.numeric(Y > 0)
par(mfrow=c(3,1))
plot(X1, Y)
plot(X2, Y)
plot(X3, Y)

plot(X1, Y.bin)
plot(X2, Y.bin)
plot(X3, Y.bin)

linmod = lm(Y~ X1 + X2 + X3)
summary(linmod)
linmod.bin = lm(Y.bin ~ X1 + X2 + X3)
summary(linmod.bin)