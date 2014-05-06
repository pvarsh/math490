################################################################################
### Homework Chapter 4-1
### Mirai Furukawa, Scott Thomas, Peter Varshavsky
### 2014/04/22
### Questions: 4.1,4.2,4.4,4.11,4.14,4.16,4.17,4.24

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
alpha = 0.01
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

### PROBLEM 4.14

interc = -1.0736
azt = -0.7195
race = 0.0555

azt.SE = 0.2790
alpha = 0.05
z = qnorm(1-alpha/2)
azt.CI = c(azt - azt.SE*z, azt + azt.SE*z)
odds.ratio.CI = exp(azt.CI)



### Question 4.16
install.packages("reshape2")
library(reshape2)
library(icda)
data(MBdrink)

### refering AZT_use_AIDS.R.
# using melt and cast to create data in the format we want
#az=melt(AZT)
#azwide=dcast(az, ... ~Symptoms )

MB = melt(MBdrink)
MB = dcast(MB, ... ~Drink)

MB.fit=glm(cbind(Often,Rarely) ~ EI + SN + TF + JP, family=binomial, data=MB)
MB.fit

# fit with interactions if for Problem 5.4
MB.fit.interact = glm(cbind(Often,Rarely) ~ EI + SN + TF + JP + EI*SN + EI*TF + EI*JP + SN*TF + SN*JP + TF*JP,
                      family=binomial,
                      data=MB)
MB.fit.interact

exp(-1.8559)/(1 + exp(-1.8559))

### Question 4.17
MB.fit2=glm(cbind(Often,Rarely) ~ EI + TF, family=binomial, data=MB)
MB.fit2

alpha = 0.05
z = qnorm(1-alpha/2)
EI = 0.5805
EI.SE = 0.2160
EI.CI = EI + EI.SE*c(-z, z)
EI.OR.CI = exp(EI.CI) #E/I Odds Ratio Confidence Interval
print(EI.OR.CI)

### Question 4.24

library(icda)
data(throat)

summary(throat) #T and Y are numeric
throat = transform(throat, T = as.factor(T), Y= as.factor(Y))
summary(throat) #Now, T and Y are categorical

#Fitting the main effects model
throat.mod = glm(Y ~ D + T, data=throat, family=binomial)
g=summary(throat.mod)

#Fitting the model with interaction
throat.modi = glm(Y ~ D * T, data=throat, family=binomial)
gi=summary(throat.modi)

predict(throat.modi,type="response")
