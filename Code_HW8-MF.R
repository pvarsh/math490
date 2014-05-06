################################################################################
### Homework Chapter 5
### Mirai Furukawa, Scott Thomas, Peter Varshavsky
### 2014/04/22
### Questions: 5.4,5.15,5.19,5.22,5.30

### Question 5.4
# a)
pchisq(11.1419, 11, lower.tail=FALSE)
# b)
pchisq(7.4091, 6, lower.tail=FALSE)


### Question 5.15

age=c("< 13","< 13","14-18","14-18","19 <","19 <")
gender=c("Male","Female","Male","Female","Male","Female")
missing=c(33,38,63,108,157,159)
total=c(3271,2486,7256,8877,5065,3520)
found=total-missing

q15=data.frame(age, gender, missing,found,total)
q15

### Model FIt
q15.fit1=glm(cbind(missing,found)~age+gender, family=binomial, data=q15)
summary(q15.fit1)

##########Chisq and G^2 tests##############################################
pchisq(chisqstat(q15.fit1), df.residual(q15.fit1), lower.tail=FALSE)
pchisq(deviance(q15.fit1), df.residual(q15.fit1), lower.tail=FALSE)



### Question 5.19

# b)
pchisq(21.7, 6, lower.tail=FALSE)


### Question 5.22

# b)
y1=c(0,0,0,0,1,1,1,1)
x1=c(10,20,30,40,60,70,80,90)
q22=data.frame(y1,x1)

q22.fit1=glm(y~x, family=binomial, data=q22)
summary(q22.fit1)

# c)
y2=c(0,0,0,0,0,1,1,1,1,1)
x2=c(10,20,30,40,50,50,60,70,80,90)
q22=data.frame(y2,x2)

q22.fit2=glm(y2~x2, family=binomial, data=q22)
summary(q22.fit2)

# d)
y3=c(0,0,0,0,1,0,1,1,1,1)
x3=c(10,20,30,40,49.9,50.1,60,70,80,90)
q22=data.frame(y3,x3)

q22.fit2=glm(y3~x3, family=binomial, data=q22)
summary(q22.fit2)
