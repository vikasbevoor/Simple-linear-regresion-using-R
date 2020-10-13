cal <- read.csv("cal.csv",header=T)
View(cal)


# install.packages("lattice")
library("lattice")
?lattice

# Exploratory data analysis
summary(cal)
attach(cal)
colnames(cal) <- c("Weight_gained", "Calories_consumed")
View(cal)

# Graphical exploration
boxplot(Weight_gained,col="dodgerblue4")
boxplot(Calories_consumed,col="red")

hist(Weight_gained)
hist(Calories_consumed)

qqnorm(Weight_gained)
qqline(Weight_gained)

qqnorm(Calories_consumed)
qqline(Calories_consumed)

#Scatter plot
plot(Calories_consumed,Weight_gained, main="Scatter Plot", col="Dodgerblue4", 
     col.main="Dodgerblue4", col.lab="green", xlab="Calories_Consumed", 
     ylab="Weight_gained",pch=20)  # plot(x,y)

cor(Weight_gained,Calories_consumed)

reg <- lm(Weight_gained ~ Calories_consumed, data=cal) # Y ~ X
summary(reg)
confint(reg,level=0.95)
pred <- predict(reg,interval="predict")
pred <- as.data.frame(pred)
View(pred)

cor(pred$fit,Weight_gained )

# transform the variables to check whether the predicted values are better
reg_sqrt <- lm(Weight_gained ~ sqrt(Calories_consumed), data=cal)
summary(reg_sqrt)
confint(reg_sqrt,level=0.95)
predict(reg_sqrt,interval="predict")
pred1 <- predict(reg_sqrt,interval="predict")
View(pred1)
pred1 <- as.data.frame(pred1)
cor(pred1$fit, Weight_gained)

reg_log<-lm(Weight_gained ~ log(Calories_consumed), data=cal)
summary(reg_log)
confint(reg_log,level=0.95)
predict(reg_log,interval="predict")
pred2 <- predict(reg_log,interval="predict")
pred2 <- as.data.frame(pred2)
cor(pred2$fit,Weight_gained ) 


reg1<-lm(log(Weight_gained)~ Calories_consumed + I(Calories_consumed*Calories_consumed), data=cal)
summary(reg1)
confint(reg1,level=0.95)
pred<-predict(reg1,interval="predict")
pred<-as.data.frame(pred)
View(pred)
exp(pred$fit)
cor(exp(pred$fit),Weight_gained)

reg_sqrt1<-lm(sqrt(Weight_gained)~Calories_consumed, data=cal)
summary(reg_sqrt1)
confint(reg_sqrt1,level=0.95)
predict(reg_sqrt1,interval="predict")
predsq<-predict(reg_sqrt1,interval="predict")
predsq<-as.data.frame(predsq)
View(predsq)
cor(predsq$fit,Weight_gained)