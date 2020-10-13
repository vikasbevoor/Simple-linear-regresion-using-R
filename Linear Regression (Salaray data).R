Sal <- read.csv("Sal.csv",header=T)
View(Sal)


# install.packages("lattice")
library("lattice")

# Exploratory data analysis
summary(Sal)
attach(Sal)


# Graphical exploration
boxplot(YearsExperience,col="dodgerblue4")
boxplot(Salary,col="red")

hist(YearsExperience)
hist(Salary)

qqnorm(YearsExperience)
qqline(YearsExperience)

qqnorm(Salary)
qqline(Salary)

#Scatter plot
plot(YearsExperience,Salary, main="Scatter Plot", col="Dodgerblue4", 
     col.main="Dodgerblue4", col.lab="green", xlab="Years Experience", 
     ylab="Salary", pch=20)  # plot(x,y)

## alternate simple command
#scatter plot
plot(YearsExperience,Salary)

cor(YearsExperience,Salary)

reg <- lm(Salary~YearsExperience, data= Sal) # Y ~ X
summary(reg)
confint(reg,level=0.95)
pred <- predict(reg,interval="predict")
pred <- as.data.frame(pred)
View(pred)
cor(pred$fit,Salary)

# transform the variables to check whether the predicted values are better
reg_sqrt <- lm(Salary~sqrt(YearsExperience), data=Sal)
summary(reg_sqrt)
confint(reg_sqrt,level=0.95)
pred1 <- predict(reg_sqrt,interval="predict")
View(pred1)
pred1 <- as.data.frame(pred1)
cor(pred1$fit, Salary)


reg_log<-lm(Salary~log(YearsExperience), data=Sal)
summary(reg_log)
confint(reg_log,level=0.95)
predict(reg_log,interval="predict")
pred2 <- predict(reg_log,interval="predict")
pred2 <- as.data.frame(pred2)
View(pred2)
cor(pred2$fit, Salary) 


reg1<-lm(log(Salary)~YearsExperience + I(YearsExperience*YearsExperience), data=Sal)
summary(reg1)
confint(reg1,level=0.95)
predict(reg1,interval="predict")
pred<-predict(reg1,interval="predict")
pred<-as.data.frame(pred)
View(pred)
exp(pred$fit)
cor(exp(pred$fit),Salary)


reg_sqrt1<-lm(sqrt(Salary)~YearsExperience, data=Sal)
summary(reg_sqrt1)
confint(reg_sqrt1,level=0.95)
pred <- predict(reg_sqrt1,interval="predict")
pred<-as.data.frame(pred)
View(pred)
cor(pred$fit,Salary)

reg_log1<-lm(log(Salary)~YearsExperience, data=Sal)
summary(reg_log1)
confint(reg_log1,level=0.95)
pred<-predict(reg_log1,interval="predict")
pred<-as.data.frame(pred)
View(pred)
cor(pred$fit,Salary)
