################################################################################
### Homework Chapter 3-2
### Mirai Furukawa, Scott Thomas, Peter Varshavsky
### 2014/03/20
### Questions: 3.13,3.14,3.18,3.20

### Question 3.13

#Read in data

library(icda)
data(horseshoecrabs)
crab=horseshoecrabs

# Poisson regression Log link (a)
mod.fit<-glm(Satellites~Weight, data=crab, family=poisson)
g=summary(mod.fit)

cat("The prediction equation is log(Y) =", g$coef[1,1], "+", g$coef[2,1],"x")

#prediction (b)
predict(mod.fit,  data.frame(Weight=2.44), type="response")

#CI for beta (c)
CI = g$coef[2,1] + (qnorm(1-0.025) * g$coef[2,2] * c(-1,1)) # this is a two-sided confidence interval, so we need to have 1-alpha/2, where alpha = 0.5
cat("95% confidence interval for beta is (",CI[1],", ",CI[2],").", sep = "")
cat("95% confidence interval for multiplicative effect on mean is (",exp(CI[1]),", ",exp(CI[2]),").", sep = "")

#Wald test (d)
# Isn'd wald test 
z = g$coef[2,1] / g$coef[2,2]
p_val = 2 * (1-pnorm(z))
p_val = g$coef[2,4]
cat("p-value of the test is ",g$coef[2,4])

#Log likely food ratio test (e) 
# likely food? do you use speech recognition to code ;)
LLstat=(g$null.deviance-g$deviance)
p_val=1-pchisq(LLstat, 1)


### Question 3.14

# Negative binomial

library(MASS)

mod.fit.nb=glm.nb(formula = Satellites ~ Weight, data = crab, link = log)

summary(mod.fit.nb)


# how to creat a plot, lowess curve is a smoothing of the data 

# plot(table.crab[,1], table.crab[,4], type="n", xlab="Width", ylab="Mean Number of satellites")
# curve(expr = coef(mod.ident)[1]+coef(mod.ident)[2]*x, col = "dodgerblue1", add = TRUE, lty = 1,  lwd=2)
# curve(predict(mod.fit,  data.frame(width=x),type="response"), lty=1, col = "magenta", add = TRUE,  lwd=2)
# curve(predict(mod.fit.nb,  data.frame(width=x),type="response"), lty=2, col = "darkorchid3", add = TRUE,  lwd=2)
# lines(lowess(crab$width, crab$satellite, f=2/3, iter=0), lty=4,col="darkgrey", lwd=2)
# points(table.crab[,1], table.crab[,4], pch=1)
# legend(22, 5,  c( "linear","poisson","negative binom","smoothing", "group means"), lty=c(1,1,2,4,-1), pch=c(-1, -1, -1,-1, 1), col=c("dodgerblue1", "magenta", "darkorchid3", "darkgrey", "black")) 
# 


### Question 3.18

team=c('Aston','Bradford','Leeds','Bournemouth','West','Hudderfield','Middelsbro','Birmingham','Ipswich','Leicester','Blackburn','Crystal','Shrewbury','Swindon','Sheffield','Stoke','Barnsley','Millwall','Hull','Manchester','Plymouth','Reading','Oldham')
attendance=c(404,286,443,169,222,150,321,189,258,223,211,215,108,210,224,211,168,185,158,429,226,150,148)
arrests=c(308,197,184,149,132,126,110,101,99,81,79,78,68,67,60,57,55,44,38,35,29,20,19)
prop.yes=yes/(yes+no)

soccer=data.frame(team, attendance, arrests)
soccer.model = glm(arrests~offset(log(attendance)), data = soccer, family = poisson)
summary(soccer.model)

plot(x = soccer$attendance, y = soccer$arrests)
curve(expr = exp(coef(soccer.model)[1])*x, col = "darkorange1", add = TRUE, lty = 1, lwd=2)
soccer.lm = lm(arrests~attendance, data = soccer) # comparing with regular regression
abline(soccer.lm, lty = 2) # comparing with regular regression

soccer.res = residuals(soccer.model)
names(soccer.res) = soccer$team
soccer.res
cat("\nTeams with large residuals:\n")
print(soccer.res[abs(soccer.res) > 8])

# Problem 3.20
age = c("35-44", "45-54", "55-64", "65-74", "75-84")
years.nsm = c(18793, 10673, 5710, 2585, 1462)
years.sm = c(52407, 43248, 28612, 12663, 5317)
deaths.nsm = c(2, 12, 28, 28, 31)
deaths.sm = c(32, 104, 206, 186, 102)
smoke = data.frame(age, years.nsm, years.sm, deaths.nsm, deaths.sm)

smoke$rate.nsm = smoke$deaths.nsm / years.nsm * 1000
smoke$rate.sm = smoke$deaths.sm / years.sm * 1000
smoke$rate.ratio = smoke$rate.sm / smoke$rate.nsm
smoke

### the following code is likely not correct (Peter)
smoke1 = data.frame(age,
                    years = c(smoke$years.nsm, smoke$years.sm),
                    deaths = c(deaths.nsm, deaths.sm),
                    smoker = c(rep(0, length(years.nsm)), rep(1, length(years.sm))))

smoke1$rate = smoke1$deaths * 1000 / smoke1$years
smoke1.pois = glm(rate ~ age + smoker, family = poisson(link = log), data = smoke1)
smoke.pois = glm(rate)
### the above code is likely not correct (Peter)
#glm(death ~ age + smoke, offset(log(pers/1000))), family = poisson, data = ... )
