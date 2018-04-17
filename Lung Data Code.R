#Problem 1
library(ISwR)
data(lung)
attach(lung)
lung
mean(volume[method == "A"])
mean(volume[method == "B"])
mean(volume[method == "C"])
mean(volume[subject == 1])
mean(volume[subject == 2])
mean(volume[subject == 3])
mean(volume[subject == 4])
mean(volume[subject == 5])
mean(volume[subject == 6])
boxplot(volume~as.factor(method), col=c("red","green","blue"), xlab="Method", ylab="Volume", main="Volume by Method")
win.graph()
boxplot(volume~subject, col=c(2:7), xlab="Subject", ylab="Volume", main="Volume by Subject")
summary(aov(volume ~ as.factor(method) + subject))
TukeyHSD(aov(volume ~ as.factor(method) + subject))
plot(resid(aov(volume ~ as.factor(method) + subject)))
hist(resid(lm(volume ~ as.factor(method) + subject)))
shapiro.test(resid(lm(volume ~ as.factor(method) + subject)))

#Problem 2
retail.gdp.data <- read.csv("C:/Users/jsunez/Desktop/DSS 665/Week 4/HW Week 4/Retail_GDP_Data.csv", header = T)
attach(retail.gdp.data)
retail.gdp.data
q1.rc <- c(rep(0, length(Quarter)))
for(i  in 1:length(Quarter)){
  if(Quarter[i] == "Q1"){
    q1.rc[i] = 1
  }
}
q1.rc
q2.rc <- c(rep(0, length(Quarter)))
for(i  in 1:length(Quarter)){
  if(Quarter[i] == "Q2"){
    q2.rc[i] = 1
  }
}
q2.rc
q3.rc <- c(rep(0, length(Quarter)))
for(i  in 1:length(Quarter)){
  if(Quarter[i] == "Q3"){
    q3.rc[i] = 1
  }
}
q3.rc
retail.gnp.recoded <- cbind(Retail, GNP, q1.rc, q2.rc, q3.rc)
retail.gnp.recoded
summary(lm(Retail ~ GNP + q1.rc + q2.rc + q3.rc))
summary(lm(Retail ~ GNP + q1.rc + q3.rc))
resid.retail.gnp <- resid(lm(Retail ~ GNP + q1.rc + q3.rc))
shapiro.test(resid.retail.gnp)
hist(resid.retail.gnp)
plot(resid.retail.gnp)