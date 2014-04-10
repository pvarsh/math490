################################################################################
### Homework Chapter 3-2
### Mirai Furukawa, Scott Thomas, Peter Varshavsky
### 2014/04/10
### Questions: 3.13,3.14,3.18,3.20

### Question 3.13

#Read in data

library(icda)
data(horseshoecrabs)
crab=horseshoecrabs

# Poisson regression Log link (a)
mod.fit<-glm(Satellites~Weight, data=crab, family=poisson)
g=summary(mod.fit)

cat("The prediction equation is log(E[Y]) =", g$coef[1,1], "+", g$coef[2,1],"x")

#prediction (b)
predict(mod.fit,  data.frame(Weight=2.44))
predict(mod.fit,  data.frame(Weight=2.44), type="response")

#CI for beta (c)
alpha = 0.05
CI = g$coef[2,1] + (qnorm(1-alpha/2) * g$coef[2,2] * c(-1,1)) # this is a two-sided confidence interval, so we need to have 1-alpha/2, where alpha = 0.5
cat("95% confidence interval for beta is (",CI[1],", ",CI[2],").", sep = "")
cat("95% confidence interval for multiplicative effect on mean is (",exp(CI[1]),", ",exp(CI[2]),").", sep = "")

#Wald test (d)
z = g$coef[2,1] / g$coef[2,2]
p_val = 2 * (1-pnorm(z))
p_val = g$coef[2,4]
cat("p-value of the test is ",g$coef[2,4])

#Log likelyhood ratio test (e) 
LLstat=(g$null.deviance-g$deviance)
p_val=1-pchisq(LLstat, 1)
p_val

### Question 3.14

# Negative binomial

library(MASS)

mod.fit.nb=glm.nb(formula = Satellites ~ Weight, data = crab, link = log)
predict(mod.fit.nb,  data.frame(Weight=2.44),type="response")

g=summary(mod.fit.nb)

# prediction equation and dispersion parameter (a)
cat("The prediction equation is log(E[Y]) =", g$coef[1,1], "+", g$coef[2,1],"x")
cat("The estimate of the dispersion parameter is",(1/g$theta))
CI = g$theta + (qnorm(1-0.025) * g$SE.theta * c(-1,1))
CI = c(1/CI[2],1/CI[1])
cat("95% confidence interval for dispersion parameter, D, is (",CI[1],", ",CI[2],").", sep = "")

# Confidence interval for beta (b)
CI = g$coef[2,1] + (qnorm(1-0.025) * g$coef[2,2] * c(-1,1))
cat("95% confidence interval for beta is (",CI[1],", ",CI[2],").", sep = "")

### Question 3.18

# Creating a data
team=c('Aston','Bradford','Leeds','Bournemouth','West','Hudderfield','Middelsbro','Birmingham','Ipswich','Leicester','Blackburn','Crystal','Shrewbury','Swindon','Sheffield','Stoke','Barnsley','Millwall','Hull','Manchester','Plymouth','Reading','Oldham')
attendance=c(404,286,443,169,222,150,321,189,258,223,211,215,108,210,224,211,168,185,158,429,226,150,148)
arrests=c(308,197,184,149,132,126,110,101,99,81,79,78,68,67,60,57,55,44,38,35,29,20,19)
soccer=data.frame(team, attendance, arrests)

# Fitting the model (b)
soccer.loglin=glm(arrests~offset(log(attendance)),family=poisson, data=soccer)
g=summary(soccer.loglin)

# Plotting and overlaying prediction equations (c)
plot(soccer$attendance, soccer$arrests)
curve(expr = exp(coef(soccer.loglin)[1])*x, col = "darkorange1", add = TRUE, lty = 1, lwd=2)


g$deviance.resid

h=lm.influence(model=soccer.loglin)$h
soccer.std.res = g$deviance.resid / sqrt(predict(soccer.loglin,  data.frame(attendance),type="response") * (1-h))
soccer.std.res

# refer snoring inference_R

# Negative binomial (d)
soccer.nb=glm.nb(formula = arrests~offset(log(attendance)), data=soccer, link=log)
g=summary(soccer.nb)
cat("The estimate of the dispersion parameter is",(1/g$theta))
CI = g$theta + (qnorm(1-0.025) * g$SE.theta * c(-1,1))
CI = c(1/CI[2],1/CI[1])
cat("95% confidence interval for dispersion parameter, D, is (",CI[1],", ",CI[2],").", sep = "")







# # Problem 3.20
# age = c("35-44", "45-54", "55-64", "65-74", "75-84")
# years.nsm = c(18793, 10673, 5710, 2585, 1462)
# years.sm = c(52407, 43248, 28612, 12663, 5317)
# deaths.nsm = c(2, 12, 28, 28, 31)
# deaths.sm = c(32, 104, 206, 186, 102)
# smoke = data.frame(age, years.nsm, years.sm, deaths.nsm, deaths.sm)
# 
# smoke$rate.nsm = smoke$deaths.nsm / years.nsm * 1000
# smoke$rate.sm = smoke$deaths.sm / years.sm * 1000
# smoke$rate.ratio = smoke$rate.sm / smoke$rate.nsm
# smoke
# 
# ### the following code is likely not correct (Peter)
# smoke1 = data.frame(age,
#                     years = c(smoke$years.nsm, smoke$years.sm),
#                     deaths = c(deaths.nsm, deaths.sm),
#                     smoker = c(rep(0, length(years.nsm)), rep(1, length(years.sm))))
# 
# smoke1$rate = smoke1$deaths * 1000 / smoke1$years
# smoke1.pois = glm(rate ~ age + smoker, family = poisson(link = log), data = smoke1)
# smoke.pois = glm(rate)
# ### the above code is likely not correct (Peter)
# 
# 
# 
# 
# age = c("35-44","35-44", "45-54","45-54", "55-64","55-64", "65-74","65-74", "75-84","75-84")
# smoke = c("YES","No","YES","No","YES","No","YES","No","YES","No")
# person = c(52407,18793,43428,10673,28612,5710,12663,2585,5317,1462)
# death = c(32,2,104,12,206,28,186,28,102,31)
# table3.9 = data.frame(age, smoke, person, death)
# 
# table3.9.pos = glm(death ~ age + smoke, data=table3.9, family=poisson)
# summary(table3.9.pos)
# 
# # in class
# table3.9 = data.frame(age, smoke, person, death)
# 
# table3.9.pos = glm(death ~ age*smoke,offest=log(person/1000), data=table3.9, family=poisson)
# summary(table3.9.pos)
# 
