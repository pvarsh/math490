### Homework 4 Chapter 3
### Mirai Furukawa, Scott Thomas, Peter Varshavsky
### 2014/03/20
### Questions: 3.3, 3.5, 3.8, 3.9

### Exericse 3.3
### No code needed

### Exercise 3.5
### Code for this exercise is taken from professor Piryatinska's snoring_inference.R

cat("\nEXERCISE 3.5\n")
#imput snoring data for glm 
snoreScores1 = c(0,2,4,6)
snoreScores2 = c(0,1,2,3)
snoreScores3 = c(1,2,3,4)
yes = c(24,35,21,30)
no = c(1355,603,192,224)
#prop.yes = yes/(yes+no)

snoreDF1 = data.frame(snore = snoreScores1, yes, no)
snoreDF2 = data.frame(snore = snoreScores2, yes, no)
snoreDF3 = data.frame(snore = snoreScores3, yes, no)

# Fit the linear, logistic and probit models
# snoreScores1
snoringGLM1 = glm(cbind(yes, no)~snore,
                 family = binomial(link = "identity"),
                 data = snoreDF1 )
g1 = summary(snoringGLM1)
g1$coefficients

# snoreScores2
snoringGLM2 = glm(cbind(yes, no)~snore,
                 family = binomial(link="identity"),
                 data = snoreDF2 )
g2 = summary(snoringGLM2)
g2$coefficients

# snoreScores3
snoringGLM3 = glm(cbind(yes, no)~snore,
                 family = binomial(link="identity"),
                 data = snoreDF3 )
g3 = summary(snoringGLM3)
g3$coefficients


### Exercise 3.8
### Code for this exercise is taken from Alexandra's snoring_inference.R

cat("\nEXERCISE 3.8\n")

# loading data
require(icda)
data(horseshoecrabs)
crabs = horseshoecrabs

# new variable binSat (Binary Satellite):
# 0 for 0 satellites, 1 for > 0 satellites
crabs$binSat = as.numeric(crabs$Satellites > 0)

# fit GLM model with probit link and binomial family
crabsGlm = glm(binSat~Weight,
               family = binomial(link = "probit"),
               data = crabs)

# plot summary of GLM
# a. Report the fit for the probit model, with weight predictor.
summary(crabsGlm)

# b. Find π^ at the highest observed weight, 5.20 kg.
x = 5.20
RHS = crabsGlm$coeff[1] + crabsGlm$coeff[2] * x
cat("\nprobit[pi] =", crabsGlm$coeff[1], "+", crabsGlm$coeff[2], "* x")
cat("\nFor x =", x)
cat("\nprobit[pi] =", RHS)
cat("\npi(x) =", pnorm(RHS))


### plotting probit for this problem
xrange = seq(from = 0, to = 7, by = 0.2)
y = pnorm(crabsGlm$coeff[1] + crabsGlm$coeff[2] * xrange)
plot(xrange, y, type = "l")
abline(v = c(1.2, 5.2), col = "orange1")
xmid = -crabsGlm$coeff[1] / crabsGlm$coeff[2]
abline(v = xmid, col = "cyan")

# c. Describe the weight effect by finding the difference between the π^ values
# at the upper and lower quartiles of weight, 2.85 and 2.00 kg.

quartiles = quantile(crabs$Weight)[c(2, 4)]
quartiles
pnorm(crabsGlm$coeff[1] + crabsGlm$coeff[2] * quartiles)

### Exericse 3.9
### No code needed