#Credit Rating
setwd("C:/Users/Jeremy/Desktop/School Code/Stat Data Mining With R")
library(readxl)

credit <- read_excel("CreditRating.xlsx", sheet="Data")

#clean data

#Checking for na's
summary(is.na(credit))

#checking to see if any are not complete cases
which(! complete.cases(credit)) 

summary(credit)

#Can get rid of ID variable
credit$ID <- NULL

#Make gender variable a factor
credit$Gender <- factor(credit$Gender)

#Make female 0, male 1
credit$Gender <- relevel(credit$Gender, "Female")

#Student is a character, will change to factor
credit$Student <- factor(credit$Student)
#make no 0, yes 1
credit$Student <- relevel(credit$Student, "No")

#make married variable a factor
credit$Married <- factor(credit$Married)
#make no 0, yes 1
credit$Married <- relevel(credit$Married, "No")

#make ethnicity variable a factor
credit$Ethnicity <- factor(credit$Ethnicity)
#make caucasian 0
credit$Ethnicity <- relevel(credit$Ethnicity, "Caucasian")

summary(credit)

#' Data Visualization
#' Distribution of Credit Score variable
hist(log(credit$Score), prob=T)
den <- density(credit$Score)                    
lines(den, col="red")

library(corrplot)
m <- cor(cbind(credit[, 1:6], credit[, 11]))
corrplot(m, method="circle")

library(PerformanceAnalytics)
chart.Correlation(m, histogram = TRUE, method = "pearson")

#Checking interaction for gender and married based on credit score
interaction.plot(x.factor = credit$Married, #x-axis variable
                 trace.factor = credit$Gender, #variable for lines
                 response = credit$Score, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Score",
                 xlab = "Married",
                 col = c("pink", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Gender")
#There is interaction for gender and married variable


#Will include all variables and test VIF scores for collinearity
m1 <- lm(Score ~ Income + Limit + Cards + Balance + Age + Student + Married + Gender + Ethnicity + Education, data=credit)

#install.packages("rms")
library(rms)  
vif(m1)  
#Limit has the highest VIF score(highly correlated with limit and cards). I will remove that to deal with the collinearity issue. If a job needed me to provide 
#insight on that variable I would remove balance instead.

m2 <- lm(Score ~ Income + Limit + Cards + Age + Student + Ethnicity + Married + Gender, data=credit) 


library(stargazer)
stargazer(m1, m2, type="text", single.row=TRUE)

#install.packages("rms")
library(rms)
vif(m2)
#No collinearity in new model. 


#Will check that residualas are linear now
plot(m2)
#Looking at the residuals vs fitted plot, the red trend line is relatively flat and close to zero, we can assume that the residuals are linear

#will now look to see if the reisudals follow a normal distribution.
hist(m2$res, col="steelblue")
#Looking at the histogram we see that the resiudals are normally distributed

#Will now look for homoscedasticity (residuals have equal/constant variance)
#Will use breusch-pagan test 
library(lmtest)
bptest(m2) 
#homoscedasticity passed the test and we can assume there is no heteroskedasticity (good thing)


#Will now look for independence, that residuals are uncorrelated
dwtest(m2)
#DW close to 2, so there is no autocorrelation, there is Independence


#Residuals are linear (Linearity)
#Residuals follow a normal distribution (Multivariate normality)
#Residuals have equal (constant) variance (Homoscedasticity)
#Residuals are uncorrelated (Independence)
#Little correlation between predictor variables (Little to No multicollinearity)
#Model passes all 5 assumptions, so we can draw interpretations from the model

library(stargazer)
stargazer(m1, m2, type="text", single.row=TRUE)

#Can have beta interpreations on Limit, Cards, Married and Student variables. 
#The other variables have too high of a standard error to say they have a significant effect on credit score.
#For every $1 increase in a person's credit limit, a person's credit rating increases by .066 (+/- 0.0004).
#For every extra crdit card a person has(1 unit increase in credit cards), a person's credit rating increases by 4.905 (+/- 0.374)
#Students have a 2.943 (+/- 1.718) higher credit score than none students
#Married poeple have a 2.219 (+/- 1.064) higher credit score than none married people. 







