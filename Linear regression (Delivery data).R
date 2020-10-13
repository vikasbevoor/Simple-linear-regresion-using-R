del <- read.csv("del.csv",header=T)
View(del)


# install.packages("lattice")
library("lattice")


# Exploratory data analysis
summary(del)
attach(del)


# Graphical exploration
boxplot(Delivery.Time,col="dodgerblue4")
boxplot(Sorting.Time,col="red")
boxplot(Delivery.Time, Sorting.Time,col= c("blue", "red"))
?boxplot

hist(Delivery.Time)
hist(Sorting.Time)

qqnorm(Delivery.Time)
qqline(Delivery.Time)

qqnorm(Sorting.Time)
qqline(Sorting.Time)


#Scatter plot
plot(Sorting.Time,Delivery.Time, main="Scatter Plot", col="Dodgerblue4", 
     col.main="Dodgerblue4", col.lab="green", xlab="Sorting.Time", 
     ylab="Delivery.Time", pch=20)  

## alternate simple command
plot(Sorting.Time, Delivery.Time)

cor(Sorting.Time, Delivery.Time)

reg <- lm(Delivery.Time~ Sorting.Time, data= del) 
summary(reg)
confint(reg,level=0.95)
pred <- predict(reg,interval="predict")
pred <- as.data.frame(pred)
View(pred)
cor(pred$fit,Delivery.Time)

# transform the variables to check whether the predicted values are better
reg_sqrt <- lm(Delivery.Time~sqrt(Sorting.Time), data=del)
summary(reg_sqrt)
confint(reg_sqrt,level=0.95)
predict(reg_sqrt,interval="predict")
pred1 <- predict(reg_sqrt,interval="predict")
View(pred1)
pred1 <- as.data.frame(pred1)
cor(pred1$fit, Delivery.Time)


reg_log<-lm(Delivery.Time~log(Sorting.Time), data=del)
summary(reg_log)
confint(reg_log,level=0.95)
predict(reg_log,interval="predict")
pred2 <- predict(reg_log,interval="predict")
pred2 <- as.data.frame(pred2)
cor(pred2$fit, Delivery.Time) 


reg1<-lm(log(Delivery.Time)~ Sorting.Time+ I(Sorting.Time*Sorting.Time), data=del)
summary(reg1)
confint(reg1,level=0.95)
predict(reg1,interval="predict")
pred<-predict(reg1,interval="predict")
pred<-as.data.frame(pred)
View(pred)
exp(pred$fit)
cor(exp(pred$fit),Delivery.Time)


reg_sqrt1<-lm(sqrt(Delivery.Time)~Sorting.Time, data=del)
summary(reg_sqrt1)
confint(reg_sqrt1,level=0.95)
pred <- predict(reg_sqrt1,interval="predict")
pred<-as.data.frame(pred)
View(pred)
cor(pred$fit,Delivery.Time)

reg_log1<-lm(log(Delivery.Time)~Sorting.Time, data=del)
summary(reg_log1)
confint(reg_log1,level=0.95)
pred <- predict(reg_log1,interval="predict")
pred<-as.data.frame(pred)
View(pred)
cor(pred$fit,Delivery.Time)
