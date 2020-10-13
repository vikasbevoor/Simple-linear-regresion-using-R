emp <- read.csv("emp.csv",header=T)
View(emp)


# install.packages("lattice")
library("lattice")

# Exploratory data analysis
summary(emp)
attach(emp)

# Graphical exploration
boxplot(Salary_hike,col="dodgerblue4")
boxplot(Churn_out_rate,col="red")

hist(Salary_hike)
hist(Churn_out_rate)

qqnorm(Salary_hike)
qqline(Salary_hike)

qqnorm(Churn_out_rate)
qqline(Churn_out_rate)

#Scatter plot
plot(Salary_hike, Churn_out_rate, main="Scatter Plot", col="Dodgerblue4", 
     col.main="Dodgerblue4", col.lab="green", xlab="Salary_hike", 
     ylab="Churn out rate", pch=20)  # plot(x,y)

## alternate simple command
#scatter plot
plot(Salary_hike, Churn_out_rate)

cor(Salary_hike, Churn_out_rate)

reg <- lm(Churn_out_rate ~Salary_hike, data= emp) # Y ~ X
summary(reg)
confint(reg,level=0.95)
pred <- predict(reg,interval="predict")
pred <- as.data.frame(pred)
View(pred)
cor(pred$fit,Churn_out_rate)

# transform the variables to check whether the predicted values are better
reg_sqrt <- lm(Churn_out_rate~sqrt(Salary_hike), data=emp)
summary(reg_sqrt)
confint(reg_sqrt,level=0.95)
pred1 <- predict(reg_sqrt,interval="predict")
View(pred1)
pred1 <- as.data.frame(pred1)
cor(pred1$fit, Churn_out_rate)


reg_log<-lm(Churn_out_rate~log(Salary_hike), data=emp)
summary(reg_log)
confint(reg_log,level=0.95)
predict(reg_log,interval="predict")
pred2 <- predict(reg_log,interval="predict")
pred2 <- as.data.frame(pred2)
View(pred2)
cor(pred2$fit, Churn_out_rate) 


reg1<-lm(log(Churn_out_rate)~Salary_hike + I(Salary_hike*Salary_hike), data=emp)
summary(reg1)
confint(reg1,level=0.95)
predict(reg1,interval="predict")
pred<-predict(reg1,interval="predict")
pred<-as.data.frame(pred)
View(pred)
exp(pred$fit)
cor(exp(pred$fit),Churn_out_rate)


reg_sqrt1<-lm(sqrt(Churn_out_rate)~Salary_hike, data=emp)
summary(reg_sqrt1)
confint(reg_sqrt1,level=0.95)
pred <- predict(reg_sqrt1,interval="predict")
pred<-as.data.frame(pred)
View(pred)
cor(pred$fit,Churn_out_rate)

reg_log1<-lm(log(Churn_out_rate)~Salary_hike, data=emp)
summary(reg_log1)
confint(reg_log1,level=0.95)
pred <- predict(reg_log1,interval="predict")
pred<-as.data.frame(pred)
View(pred)
cor(pred$fit,Churn_out_rate)
