################################################################################
### Homework Chapter 4-1
### Mirai Furukawa, Scott Thomas, Peter Varshavsky
### 2014/04/22
### Questions: 4.1,4.2,4.4,4.11,4.14,4.16,4.17,4.24



### Question 4.16

library(icda)
data(MBdrink)




### Question 4.24

library(icda)
data(throat)

summary(throat) #T and Y are numeric
throat = transform(throat, T= as.factor(T),Y= as.factor(Y))
summary(throat) #Now, T and Y are categorical

#Fitting the main effects model
throat.mod = glm(Y ~ D + T, data=throat, family=binomial)
g=summary(throat.mod)

#Fitting the model with interaction
throat.modi = glm(Y ~ D * T, data=throat, family=binomial)
gi=summary(throat.modi)

predict(throat.modi,type="response")
