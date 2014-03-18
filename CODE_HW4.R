################################################################################
### Homework Chapter 3-1
### Mirai Furukawa, Scott Thomas, Peter Varshavsky
### 2014/03/20
### Questions: 3.3,3.5,3.8,3.9



### Question 3.3

#a. State the prediction equation, and interpret the intercept and slope.

########################################
# Mirai Begin                          #   
########################################

cat("The prediction equation is Y = 0.00109x+ 0.00255")

#b. Use the model fit to estimate the (i) probabilities of malformation for 
#alcohol levels 0 and 7.0, (ii) relative risk comparing those levels.

cat("probability of malformation for alcohol level of 0 is \nY = 0.00109(0) + 0.00255 = 0.00255")
cat("Y = 0.00109(0) + 0.00255 = 0.00255")
cat("probability of malformation for alcohol level of 7 is")
cat("Y = 0.00109(7) + 0.00255 = ", 0.00109*7 + 0.00255)

cat("Relative risk of x = 0 to x = 7 is",0.00109*7 + 0.00255,"/ 0.00255 = ",(0.00109*7 + 0.00255)/0.0255)

########################################
# Mirai End                            #    
########################################


### Question 3.5

########################################
# Mirai Begin                          #   
########################################


### Entering data
snore=c(0,2,4,5)
snorei=c(0,2,4,6)
snoreii=c(0,1,2,3)
snoreiii=c(1,2,3,4)
yes=c(24,35,21,30)
no=c(1355,603,192,224)
prop.yes=yes/(yes+no)

snoring2=data.frame(snore, yes, no)
snoring2

snoringi=data.frame(snorei, yes, no)
snoringii=data.frame(snoreii, yes, no)
snoringiii=data.frame(snoreiii, yes, no)
### Fitting the linear model
snoringi.ident=glm(cbind(yes, no)~snorei, family=binomial(link="identity"), data=snoringi )
snoringii.ident=glm(cbind(yes, no)~snoreii, family=binomial(link="identity"), data=snoringii )
snoringiii.ident=glm(cbind(yes, no)~snoreiii, family=binomial(link="identity"), data=snoringiii )
#summary(snoringi.ident)

cat("Fit of linear probablity is")
cat("(i)   Y =",summary(snoringi.ident)$coefficients[1],"(snore) +",summary(snoringi.ident)$coefficients[2])
cat("(ii)  Y =",summary(snoringii.ident)$coefficients[1],"(snore) +",summary(snoringii.ident)$coefficients[2])
cat("(iii) Y =",summary(snoringiii.ident)$coefficients[1],"(snore) +",summary(snoringiii.ident)$coefficients[2])


#fit the liner, logistic and probit models
snoring.ident=glm(cbind(yes, no)~snore, family=binomial(link="identity"), data=snoring2 )
summary(snoring.ident)

snoring.logit=glm(cbind(yes, no)~snore, family=binomial(), data=snoring2 )
summary(snoring.logit)

snoring.probit=glm(cbind(yes, no)~snore, family=binomial(link="probit"), data=snoring2 )
summary(snoring.probit)


##########################################################
#predicted values of the model
p.identi=predict(snoringi.ident, type="response") 
p.identii=predict(snoringii.ident, type="response") 
p.identiii=predict(snoringiii.ident, type="response") 

p.identi
p.identii
p.identiii


########################################
# Mirai End                            #    
########################################

### Question 3.8
#a. Report the fit for the probit model, with weight predictor.

########################################
# Mirai Begin                          #   
########################################
## Question 3.7
crab = horseshoecrabs
Y = 1*(crab$Satellites>0)
table.crab=data.frame(crab$Weight, Y)
table.crab=round(table.crab,2)
names(table.crab) <- c("Weight", "Y")

mod.ident1=lm(Y~Weight, data = table.crab)
g=summary(mod.ident1)

## Quesion 3.8
crab.probit=glm(Y~Weight, family=binomial(link="probit"), data=table.crab )
summary(crab.probit)





########################################
# Mirai End                            #    
########################################
#b. Find ^?? at the highest observed weight, 5.20 kg.
#c. Describe the weight effect by finding the difference between the ^?? values
#   at the upper and lower quartiles of weight, 2.85 and 2.00 kg.
#d. Interpret the parameter estimates using characteristics of the normal cdf
#   that describes the response curve.



### Question 3.9