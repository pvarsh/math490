#functions
logit.inv = function(intercept, xcoef, xval){
  numerator = exp(intercept + sum(xcoef * xval))
  return(numerator / (1+numerator))
}

### PROBLEM 4.1

# data
LI = c(8, 10, 12, 14, 16,
       18, 20, 22, 24, 26,
       28, 32, 34, 38)
ncases = c(2, 2, 3, 3, 3,
           1, 3, 2, 1, 1,
           1, 1, 1, 3)
nremissions = c(0, 0, 0, 0, 0,
                1, 2, 1, 0, 1,
                1, 0, 1, 2)

# ungrouping the data
ungr = data.frame()
for (i in seq(1, length(LI))){
  k = 0
  for (j in seq(1, ncases[i])){
    if (k < nremissions[i]){
      ungr = rbind(ungr, c(LI[i], 1))
      print("added 1")
    }
    else {
      ungr = rbind(ungr, c(LI[i], 0))
      print("added 0")
    }
    k = k+1
  }
}
names(ungr) = c("LI", "rem")

#fitting the model
mod4.1 = glm(rem~LI, data = ungr, family = binomial())
sum4.1 = summary(mod4.1) # agrees with the output in textbook
intercept = sum4.1$coef[1, 1]
LI.coef = sum4.1$coef[2, 1]
x1 = 8
exp(intercept + LI.coef*x1)/(1 + exp(intercept + LI.coef*x1))
x2 = 26
exp(intercept + LI.coef*x2)/(1 + exp(intercept + LI.coef*x2))


### PROBLEM 4.2
alpha = 0.05
#b. Construct the confidence interval
exp(c(LI.coef - qnorm(1-alpha/2)*sum4.1$coef[2,2], LI.coef + qnorm(1-alpha/2)*sum4.1$coef[2,2]))

### PROBLEM 4.4
#b
intercept = -3.866
xcoef = 0.397
logit.inv(intercept, xcoef, 0)
logit.inv(intercept, xcoef, 5)


#c
odds0 = 0.0205 / (1-0.0205)
odds5 = 0.1322 / (1-0.1322)
odds5/odds0

### PROBLEM 4.11
2.02/1.71
.72/.41
