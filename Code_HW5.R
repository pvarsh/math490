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

cat("The prediction equation is Y = ",g$coef[1,1],"",g$coef[2,1],"x")

#prediction (b)
predict(mod.fit,  data.frame(Weight=2.44),type="response")

#CI for beta (c)
CI = g$coef[2,1] + (qnorm(1-0.05) * g$coef[2,2] * c(-1,1))
cat("95% confidence interval for beta is (",CI[1],",",CI[2],").")
cat("95% confidence interval for multiplicative effect on mean is (",exp(CI[1]),",",exp(CI[2]),").")

#Wald test (d)
z = g$coef[2,1] / g$coef[2,2]
p_val=2*(1-pnorm(z))
p_val=g$coef[2,4]
cat("p-value of the test is ",g$coef[2,4])

#Log likely food ratio test (e)
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
