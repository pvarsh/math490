### Homework 3 Chapter 2-2
### Mirai Furukawa, Scott Thomas, Peter Varshavsky
### 2014/03/20
### Questions: 3.3, 3.5, 3.8, 3.9

#############################################################
#### Functions from previous homeworks

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
### End: functions from previous homeworks
#############################################################

#############################################################
#### New functions


### End: new functions
#############################################################


### Exercise 3.5
### Code for this exercise is taken from Alexandra's snoring_inference.R

cat("\nEXERCISE 3.5\n")
#imput snoring data for glm 
snoreScores1 = c(0,2,4,6)
snoreScores2 = c(0,1,2,3)
snoreScores3 = c(0,100,200,300)
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
