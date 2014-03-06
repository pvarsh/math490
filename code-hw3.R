### Homework Chapter 2-2
### Mirai Furukawa, Scott Thomas, Peter Varshavsky
### 2014/03/06
### Questions: 2.18, 2.21, 2.22, 2.30, 2.33, 2.38, 3.2


### Question 2.18

###########################################################################################
# Code: Peter Varshavsky
###########################################################################################
# a. Show how to obtain the estimated expected cell countof 35.8 for the first cell

# create matrix of expected counts
p2.18exp = matrix(c(35.8, 166.1, 88.1,
                    79.7, 370.0, 196.4,
                    52.5, 244.0, 129.5), byrow = T, nrow = 3)
p2.18obs = matrix(c(21, 159, 110,
                    53, 372, 221,
                    94, 249, 83), byrow = T, nrow = 3)
# divide each row by its sum
p2.18expR = p2.18exp / apply(p2.18exp, 1, sum) # expected rows 
p2.18expC = t(t(p2.18exp) / apply(p2.18exp, 2, sum)) # expected columns
p2.18obsCalculated = p2.18expR * p2.18expC * sum(p2.18obs) # calculated expected values
cat("Columns divided by column sums:")
print(p2.18expC)
cat("Rows divided by row sums:")
print(p2.18expR)
cat("Under the null hypthesis Income and Happiness are independent multinomial variables with the following marginals:",
    "\n\tIncome:    ", p2.18expR[1,],
    "\n\tHappiness: ", p2.18expC[,1], sep = " ")
cat("Multiplying the marginals and then multiplying the resulting matrix by total sample size", sum(p2.18obs), "we get the matrix of expected cell values")
print(p2.18obsCalculated)

# b. For testing independence, X^2 = 73.4. Report the degrees of freedom and the P-value and interpret.
cat("# b. For testing independence, X^2 = 73.4. Report the degrees of freedom and the P-value and interpret.
")
cat("For a 3x3 table there are 2*2 = 4 degrees of freedom.")
cat("P-value:", 1-pchisq(73.4, df = 2), "\nWe can reject the null hypothesis of independence")

# c. Interpret the standardized residuals in the corner cells having counts 21 and 83.
cat("# c. Interpret the standardized residuals in the corner cells having counts 21 and 83.
")
cat("Residuals are negative and greater than 2 in absolute value, which means there is likely
    strong evidence against independence. The negative sign means that these cells are
    underrepresented in the sample, that is the evidence suggests that there are fewer
    unhappy rich or happy poor than the hypothesis of independence would imply.")
# d. Interpret the standardized residuals in the corner cells having counts 110 and 94.
cat("Interpret the standardized residuals in the corner cells having counts 110 and 94.")
cat("Residuals are positive and of high absolute value, again giving strong evidence against independence.
    The positive values suggest that these categories are overrepresented in the sample, or
    that there are more happy rich and unhappy poor than would be observed under independence.")

###########################################################################################
# End Code: Peter Varshavsky
###########################################################################################


####################################################################################
### Question 2.21

#Original Table
p2.21 = matrix(c(60,81,75,75,87,86),
               nrow=2,
               byrow=TRUE, 
               dimnames = list(Gender=c("Men","Women"),Reason=c("A","B","C")))
#Table containng only column B and C

p2.21bc = matrix(c(81,75,87,86),
               nrow=2,
               byrow=TRUE, 
               dimnames = list(Gender=c("Men","Women"),Reason=c("B","C")))
#Table containing all column but B and C combined

p2.21abc = matrix(c(60, 81+75,
                    75, 87+86),
               nrow=2,
               byrow=TRUE, 
               dimnames = list(Gender=c("Men","Women"),Reason=c("A","B+C")))
p2.21
p2.21bc
p2.21abc

chisq.test(p2.21)
chisq.test(p2.21bc)
chisq.test(p2.21abc)

G2.test(p2.21)
G2.test(p2.21bc)
G2.test(p2.21abc)

####### Peter's code
standResid(p2.21abc)
####### End Peter's code

####################################################################################
### Question 2.22

p2.22 = matrix(c(105,8,12,2,18,19,47,52,0,13),
               nrow=5,
               byrow=TRUE, 
               dimnames = list(Diagnosis=c("Scizophrenia","Affective disorder","Neurosis","Personality disorder","Special symtomps"),
                               Drugs=c("Drugs","No Drugs")))

procfreq(p2.22)$chi.square
procfreq(p2.22)$likelihood.ratio.stat
procfreq(p2.22)$adjusted.residuals

p2.22i = matrix(c(105,8,12,2),
               nrow=2,
               byrow=TRUE, 
               dimnames = list(Diagnosis=c("Scizophrenia","Affective disorder"),
                               Drugs=c("Drugs","No Drugs")))

p2.22ii = matrix(c(18,19,47,52),
               nrow=2,
               byrow=TRUE, 
               dimnames = list(Diagnosis=c("Neurosis","Personality disorder"),
                               Drugs=c("Drugs","No Drugs")))

p2.22iii = matrix(c(105+12,8+2,18+47,19+52,0,13),
               nrow=3,
               byrow=TRUE, 
               dimnames = list(Diagnosis=c("Scizophrenia + Affective disorder","Neurosis + Personality disorder","Special symtomps"),
                               Drugs=c("Drugs","No Drugs")))

##############
# Problem 2.22
# Code by Peter
##############

p2.22 = matrix(c(105, 8,
                 12, 2,
                 18, 19,
                 47, 52,
                 0, 13), byrow = T, nrow = 5,
               dimnames = list("diagnosis" = c("schizophrenia",
                                               "affective disorder",
                                               "neurosis",
                                               "personality disorder",
                                               "special symptoms"),
                               "treatment" = c("drugs",
                                               "no drugs")))

# a. Conduct a test of independence and interpret the P-value
print(p2.22)
chisq.test(p2.22)

# b. Obtain standardized residuals and interpret

# changing the count in the 0-cell to 0.5
# p2.22a = p2.22
# p2.22a[5,1] = 0.5
# print(p2.22a)
# chisq.test(p2.22a)
# G2.test(p2.22a)
# standResid(p2.22a)

# c. Partition chi-squared into three components to describe differences and similarities among the diagnoses by comparing

# c.i. the first two rows
standResid(p2.22[1:2, ])
chisq.test(p2.22[1:2, ])

# c.ii. the third and fourth rows
standResid(p2.22[3:4, ])
chisq.test(p2.22[3:4, ])

# c.iii. the last row to the first and second rows combined, and the third and fourth rows combined 
p2.22iii.a = colSums(p2.22[c(1,2), ])
p2.22iii.a = rbind(p2.22iii.a, p2.22[5,])
p2.22iii.a
standResid(p2.22iii.a)
chisq.test(p2.22iii.a)

p2.22iii.b = colSums(p2.22[c(3,4), ])
p2.22iii.b = rbind(p2.22iii.b, p2.22[5,])
p2.22iii.b
standResid(p2.22iii.b)
chisq.test(p2.22iii.b)

# changing the zero count in special symptoms to 0.5
# p2.22iii.b[2,1] = 0.5
# p2.22iii.b
# chisq.test(p2.22iii.b)

##############
# End of Code by Peter
##############




####################################################################################
### Question 2.30
# Table 2.17 contains results of a study comparing
# radiation therapy with surgery in treating cancer of the larynx.
# Use Fisher's exact test to test H_0: theta = 1 against H_a: theta > 1.
# Interpret results

p2.30 = matrix(c(21, 2,
                 15, 3),
                nrow=2,
                byrow=TRUE, 
                dimnames = list(TreatmentType=c("Surgery","Radiation therapy"),
                                "Cancer Controlled" = c("Yes","No"))) #### PV: I renamed the variable name CaseControl


fisher.test(p2.30,alternative="greater")
fisherExact(p2.30) #using Peter's function. Answer agrees with fisher.test()


####################################################################################
### Question 2.33


##### Begin Peter code
##### using 3-dimensional array following Alexandra's code
dp = c(19, 132, 11, 63, 0, 9, 6, 103)
dp = array(dp, dim = c(2,2,2))
dimnames(dp) = list(DeathPen = c("yes", "no"),
                     Defendant = c("white", "black"),
                     Victim = c("white", "black"))
dp_flat = ftable(dp, row.vars = c("Victim", "Defendant"), col.vars = "DeathPen")
##### End Peter code

#Partial table victim = white
p2.33w = matrix(c(19,132,11,52),
               nrow=2,
               byrow=TRUE, 
               dimnames = list(DefendantsRace=c("White","Black"),
                               Penalty=c("Yes","No")))
#partial table victim = black
p2.33b = matrix(c(6,97,0,9),
                nrow=2,
                byrow=TRUE, 
                dimnames = list(DefendantsRace=c("White","Black"),
                                Penalty=c("Yes","No")))

odds.ratio(p2.33w)
odds.ratio(p2.33b+0.5)

#Marginal Table
p2.33m = matrix(c(25,229,11,61),
                nrow=2,
                byrow=TRUE, 
                dimnames = list(DefendantsRace=c("White","Black"),
                                Penalty=c("Yes","No")))
odds.ratio(p2.33m)

####################################################################################
### Question 2.38

####################################################################################
### Question 3.2

### Functions

G2.test=function(x)
{
  total=sum(x)
  rowsum=apply(x,1,sum)
  colsum=apply(x,2,sum)
  expected=(matrix(rowsum) %*% t(matrix(colsum))) / total
  tmp=x*log(x/expected) 
  tmp[x==0]=0
  G2=2*sum(tmp)
  df=prod(dim(x)-1)
  attr(G2,"P-value")=1-pchisq(G2,df)
  return(G2)
}

procfreq=function(x, digits=4) # create a fuction similar to the proc freq is SAS
{
  total=sum(x)
  rowsum=apply(x,1,sum)
  colsum=apply(x,2,sum)
  prop=x/total
  rowprop=sweep(x,1,rowsum,"/")
  colprop=sweep(x,2,colsum,"/")
  expected=(matrix(rowsum) %*% t(matrix(colsum))) / total
  dimnames(expected)=dimnames(x)
  resid=(x-expected)/sqrt(expected)
  adj.resid=resid /sqrt((1-matrix(rowsum)/total) %*% t(1-matrix(colsum)/total))
  df=prod(dim(x)-1)
  X2=sum(resid^2)
  attr(X2,"P-value")=1-pchisq(X2,df)
  ## Must be careful about zero freqencies. Want 0*log(0) = 0.
  tmp=x*log(x/expected) 
  tmp[x==0]=0
  G2=2*sum(tmp)
  attr(G2,"P-value")=1-pchisq(G2,df)
  list(sample.size=total, row.totals=rowsum,
       col.totals=colsum,
       overall.proportions=prop,
       row.proportions=rowprop,
       col.proportions=colprop,  
       expected.freqs=expected,
       residuals=resid,
       adjusted.residuals=adj.resid,
       chi.square=X2,
       likelihood.ratio.stat=G2,
       df=df)}

odds.ratio = function(mat, conf.level = 0.95, noPrint = TRUE){
  #  matrix mat must contain counts of out comes:
  #     row1: treatment
  #     row2: control
  #     col1: success
  #     col2: failure
  #     0 counts will result in error
  
  oddsTreatment = mat[1,1] / mat[1,2]
  oddsControl = mat[2,1] / mat[2,2]
  oddsRatio = oddsTreatment / oddsControl
  logOddsRatio = log(oddsRatio)
  
  SE = sqrt((1/mat[1,1] + 1/mat[1,2] + 1/mat[2,1] + 1/mat[2,2]))
  
  quantile = 1 - (1 - conf.level)/2
  
  z = c(-qnorm(quantile, 0, 1), qnorm(quantile, 0, 1)) # two-tailed interval containing conf.level of standard normal distribution
  
  confInt = exp(logOddsRatio + SE * z) # addition, multiplication and exp() are vectorized in R
  
  out = list(oddsRatio = oddsRatio, confInt = confInt) # I'm using a list to return two different data types (numeric and numeric vector)
  
  if (!noPrint){
    print(out) # print can be suppressed by noPrint parameter 
  }
  return(out) 
}

standResid = function(mat){
  # author: Peter
  # parameter:
  #   mat: contingency table of observations
  # returns:
  #   matrix of standardized residuals
  # calls:
  #   marginals()
  
  margMat = marginals(mat)
  pMat = mat / sum(mat)
  p.i.plus = rowSums(mat)/margMat$n
  p.plus.j = colSums(mat)/margMat$n
  print("testing")
  
  resid = mat - margMat$exp * margMat$n
  SE = sqrt(margMat$exp * margMat$n * ((1 - p.i.plus) %*% t(1 - p.plus.j)))
  
  return(resid/SE)
}

marginals = function(mat){
  # author: Peter
  # returns a list with row marginal, column marginal, expected values of joint under independence
  colMarg = colSums(mat)/sum(mat)
  rowMarg = rowSums(mat)/sum(mat)
  expMat = rowMarg %*% t(colMarg)
  
  return(list(original = mat, row = rowMarg, col = colMarg, exp = expMat, n = sum(mat)))
  
}
fisherExact = function(mat){
  # author: Peter
  # One-sided (greater) Fisher's exact test for 2x2 contingency tables
  
  #cat("\nmat: ")
  #print(mat)
  n = sum(mat)
  #cat("\nn:",n)
  observed = mat[1][1]
  rs = rowSums(mat)
  #cat("\nrs:", rs)
  cs = colSums(mat)
  p = 0
  #cat("\nStarting for loop\n")
  for (i in mat[1][1]:rs[1]){
    #print(i)
    p = p + choose(rs[1], i) * choose(rs[2], cs[1] - i) / choose(n, cs[1])
  }
  
  #return(choose(rowSums(mat)[1], mat[1,1]) * choose(rowSums(mat)[2], colSums(mat)[1] - mat[1][1]) / choose(sum(mat), colSums(mat)[1]))
  #cat("\nP-value =",p, "\n")
  names(p) = "P-Value"
  return(p)
}
