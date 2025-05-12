library(readxl)
library(ggplot2)
bbc<- read_excel("C:/Users/dt/Downloads/bbc (2).xlsx")
testing1 <- read_excel("C:/Users/dt/Downloads/testing (1).xlsx")
summary(bbc)
str(bbc)
View(bbc)
bbc<-as.data.frame(bbc)
head(bbc)

testing1<-as.data.frame(testing1)
m1<-glm(Choice~., data=bbc, family=binomial)
summary(m1)


testing1$predictions<-predict(m1, newdata=testing1, type="response")
head(testing1)


testing1<-testing1[order(testing1$predictions, decreasing=TRUE),]
head(testing1)


testing1$Decile<-rep(1:10, each=230)
head(testing1)

deciletable<-aggregate(testing1$Choice ~ testing1$Decile, data=testing1, sum)
deciletable

names(deciletable)<-c("Decile","Buyers")
deciletable$CumulativeBuyers<-cumsum(deciletable$Buyers)
table(testing1$Choice)
n<-table(testing1$Choice)[2]

deciletable$PercentCaptured<-deciletable$CumulativeBuyers/n
deciletable


plot(PercentCaptured ~ Decile,data=deciletable, type="l", xlab="Deciles", ylab="Percentage of Customers Captured", main="Cumulative Gains Chart for logit model")

##################################################################################################
m2<-lm(Choice~., data=bbc)
summary(m2)
testing1$predictionsRegress<-predict(m2, newdata=testing1, type="response")
head(testing1)
testing1<-testing1[order(testing1$predictionsRegress, decreasing=TRUE),]
head(testing1)


testing1$Decile<-rep(1:10, each=230)
head(testing1)

a<-aggregate(testing1$Choice ~ testing1$Decile, data=testing1, sum)
a

names(a)<-c("Decile","Buyers")
a$CumulateBuyers<-cumsum(a$Buyers)
table(testing1$Choice)
n<-table(testing1$Choice)[2]

a$PercentCaptured2<-a$CumulateBuyers/n
a

ggplot(a, aes(x = Decile, y = PercentCaptured2)) +
  geom_line(color = "green", size = 1) +
  labs(title = "Market Capture by Regression Model",
       x = "Deciles",
       y = "Fraction captured") +
  theme_minimal()



######################################################################33

testing1<-testing1[order(testing1$Amount_purchased, decreasing=TRUE),]
head(testing1)


testing1$Decile<-rep(1:10, each=230)
head(testing1)

d<-aggregate(testing1$Choice ~ testing1$Decile, data=testing1, sum)
d
names(d)<-c("Decile","Buyers")
d$CumuBuyers<-cumsum(d$Buyers)
table(testing1$Choice)
n<-table(testing1$Choice)[2]

d$PercentCaptured3<-d$CumuBuyers/n
d


ggplot(d, aes(x = Decile, y = PercentCaptured3)) +
  geom_line(color = "purple", size = 1) +
  labs(title = "Market Capture by Amount",
       x = "Deciles",
       y = "Fraction captured") +
  theme_minimal()
#############################################################################################
#using logit model
deciletable$Mailings<-(deciletable$Decile/10)*50000
deciletable
margin<-10.2
costperaddressee<-0.65
marketsize<-50000*(204/2300)
deciletable$Profits<-margin*marketsize*deciletable$PercentCaptured - costperaddressee*deciletable$Mailings
deciletable
ggplot(deciletable, aes(x = Decile, y = Profits)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Profit Chart",
       x = "Deciles",
       y = "Profit") +
  theme_minimal()
##############################################################################

#ans1-)I think the best model for this will be the logit model because
#although the logit and regression model predicts a steep curve, which 
#means more amount capture in the upper decile, which is favorable because
#the company can target only those deciles to reduce cost and capture the 
#most market, the theory for regression model states that the dependent 
#variable be continuous since choice is not continuous, it does not make 
#sense. Also, some of the prediction that come from using the regression 
#model are above 100% which is not possible which shows that the logic behind
#using regression for this model is flawed If they market by amount, the upper
#decile will capture less amount thus, they will lose a potential customer or 
#their cost will increase


#ans2-) Top 4 deciles which is 23000


#ans3-)Do not only mail to the top decile because, through time the pool
#of active customers will decrease. Apply a dynamic model to inactive
#customer and make campaign that will target the inactive customers which
#will eventually increase the pool of customer which will increase the profit.

#Comparison of Market Capture Strategies:
comparison_amount_data <- merge(d, a, by = "Decile", suffixes = c("_Amount", "_Regression"))

ggplot(comparison_amount_data, aes(x = Decile)) +
  geom_line(aes(y = PercentCaptured3), color = "purple", size = 1, linetype = "solid", key_glyph = "path") +
  geom_line(aes(y = PercentCaptured2), color = "green", size = 1, linetype = "dashed", key_glyph = "path") +
  labs(title = "Comparison of Market Capture Strategies",
       x = "Deciles",
       y = "Fraction captured",
       color = "Strategy") +
  scale_color_manual(values = c("purple", "green")) +
  theme_minimal()

#Comparison of Models in the Top Deciles:
comparison_data <- merge(a, deciletable, by = "Decile", suffixes = c("_Regression", "_Logit"))

ggplot(comparison_data, aes(x = Decile)) +
  geom_line(aes(y = PercentCaptured2), color = "green", size = 1, linetype = "solid", key_glyph = "path") +
  geom_line(aes(y = PercentCaptured), color = "blue", size = 1, linetype = "dashed", key_glyph = "path") +
  labs(title = "Comparison of Market Capture in Top Deciles",
       x = "Deciles",
       y = "Fraction captured",
       color = "Model") +
  scale_color_manual(values = c("green", "blue")) +
  theme_minimal()

