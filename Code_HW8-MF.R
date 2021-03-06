################################################################################
### Homework Chapter 5
### Mirai Furukawa, Scott Thomas, Peter Varshavsky
### 2014/04/22
### Questions: 5.4,5.15,5.19,5.22,5.30

### Question 5.4

# Note: model fits that yield the values used here can be found in Code_HW6-MF.R in problem 4.16.
# a)
pchisq(11.1419, 11, lower.tail=FALSE)
# b)
pchisq(7.4091, 6, lower.tail=FALSE)


### Question 5.15


age=c("< 14","< 14","14-18","14-18","> 18","> 18")
gender=c("Male","Female","Male","Female","Male","Female")
missing=c(33,38,63,108,157,159)
total=c(3271,2486,7256,8877,5065,3520)
found=total-missing

q15 = data.frame(age, gender, missing, found, total)
q15

### Combining the first two age groups (PV)
q15.age = data.frame()
q15.age = rbind(q15.age, q15[1,]+q15[3,])
q15.age[1,1] = "<19"
q15.age[1,2] = "Male"
q15.age = rbind(q15.age, q15[2,]+q15[4,])
q15.age[2,1] = "<19"
q15.age[2,2] = "Female"
q15.age = rbind(q15.age, q15[c(5,6),])

### Model fit
q15.fit1 = glm(cbind(missing,found)~age+gender, family=binomial, data=q15)
summary(q15.fit1)

### Model fit with combined age groups (PV)
q15.fit2 = glm(cbind(missing, found) ~ age + gender, family = binomial, data = q15.age)
summary(q15.fit2)

### Model fit with interaction (PV)
q15.fit3 = glm(cbind(missing, found) ~ age + gender + age*gender, family = binomial, data = q15)
summary(q15.fit3)

##########Chisq and G^2 tests##############################################
pchisq(chisqstat(q15.fit1), df.residual(q15.fit1), lower.tail=FALSE)
pchisq(deviance(q15.fit1), df.residual(q15.fit1), lower.tail=FALSE)

predict(q15.fit1)

### Question 5.19



# b)
pchisq(21.7, 6, lower.tail=FALSE)

# e) (PV)
(512 + 89) / (313 + 19)
(1198+557) / (1493 + 1278)

### Question 5.22

# b)
y1=c(0,0,0,0,1,1,1,1)
x1=c(10,20,30,40,60,70,80,90)
q22=data.frame(y1,x1)

q22.fit1=glm(y1~x1, family=binomial, data=q22)
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
