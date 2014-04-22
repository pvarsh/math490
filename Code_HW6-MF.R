################################################################################
### Homework Chapter 4-1
### Mirai Furukawa, Scott Thomas, Peter Varshavsky
### 2014/04/22
### Questions: 4.1,4.2,4.4,4.11,4.14,4.16,4.17,4.24



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
