############          TREND       ###########

library(astsa)
library(mgcv)
library(simts)
library(hts)
library(tseries)
library(forecast)

library(readxl)
data <- read_excel("Raw Data.xlsx", sheet = "Mehsana")
View(data)

############   Linear model     #################

lm1 <- lm(data$Area ~ data$Year)
lm1

summary(lm1)

# Residuals, Fitted, ...
#create scatterplot of raw data
model<-plot(data$Year, data$Area, col='red', 
            main='Summary of Regression Model', 
            xlab='Year', ylab='Production')

#add fitted regression line
lines(data$Year, predict(lm1), col = "blue")
legend("bottomright", c("Predicted", "time series data"), 
       col = c("blue4", "red"), bty = "n", lwd = 1)

#Residual Analysis
#Runs test (H0:residuals are randomly distibuted
library(randtests)
x<-factor(sign(residuals(lm1)))
x
runs.test(residuals(lm1))

lm1pred=predict(lm1)
lm1pred
res1=lm1pred-data$Area
res1

#Test for normality
#Shapiro-Wilks test (H0:residuals are normally distributed)
shapiro.test(residuals(lm1))

#Measures of Goodness of Fit

#### MAPE
MLmetrics::MAPE(predict(lm1),data$Area)
#### RMSE
MLmetrics::RMSE(predict(lm1),data$Area)

#### R square
MLmetrics::R2_Score(predict(lm1),data$Area)

AIC(lm1)
BIC(lm1)

###############Quadratic models

#create a new variable for hours2

################  fit quadratic regression model
x2=(data$Year)^2
x2

qM <- lm(data$Area~ data$Year + x2)
check(qM)
qM

#view model summary
summary(qM)

plot(data$Year,data$Area)
lines(data$Year, predict(qM), col = "red")
legend("bottomright", c("Time series", "Predicted"), 
       col = c("blue4", "red"), bty = "n", lwd = 1)

predict(qM)

#Residual Analysis
#Runs test (H0:residuals are randomly distibuted)
x5<-factor(sign(residuals(qM)))
x5

runs.test(residuals(qM))


#Test for normality
#Shapiro-Wilks test (H0:residuals are normally distributed)
shapiro.test(residuals(qM))


#Measures of Goodness of Fit

####### R2_score
MLmetrics::R2_Score(predict(qM),data$Area)

########## Mape 
MLmetrics::MAPE(predict(qM),data$Area)

#RMSE
MLmetrics::RMSE(predict(qM),data$Area)

AIC(qM)
BIC(qM)



###################################################
data$Year
data$Area

################  Gam

library(gam)

fit1 = gam(data$Area ~ s(data$Year))
check(fit1, simple = TRUE)
summary(fit1)

coef(summary.lm(fit1))

fit1$coefficients

library(plotrix)
std.error(fit1$coefficients)
fit1
plot(data$Year,data$Area)

library(rsq)
rsq(fit1)
lines(data$Year, predict(fit1), col = "red")
legend("bottomright", c("Predicted"), 
       col = c("red"), bty = "n", lwd = 1)

#Residual Analysis
#Runs test (H0:residuals are randomly distibuted)
x6<-factor(sign(residuals(fit1)))
x6

runs.test(residuals(fit1))

#Test for normality
#Shapiro-Wilks test (H0:residuals are normally distributed)
shapiro.test(residuals(fit1))


#Measures of Goodness of Fit
#MAPE
MLmetrics::MAPE(predict(fit1),data$Area)

#RMSE
MLmetrics::RMSE(predict(fit1),data$Area)

#R square
MLmetrics::R2_Score(predict(fit1),data$Area)

extractAIC(fit1) 

AIC(fit1)
BIC(fit1)

predict(fit1)
library("writexl")
M=predict(fit1)
v=data.frame(M)
write_xlsx(v,"E:\\8. all pdf files\\research\\Data Analysis\\Mehsana_Area.xlsx")


##############################


############ Exponential

library(SciViews)
exp1 <- lm(log(data$Area)~ data$Year)
coef(exp1)


# (Intercept)     data$Year 


# put the r value in below code

exp(-331.0017218)

expo<-minpack.lm::nlsLM(data$Area ~ a*exp(r*data$Year), 
                        data = data, 
                        start = list(a=1.769207e-144,r=0.1652996), 
                        control = list(maxiter = 500))
expo
summary(expo)

plot(data$Year,data$Area,col="blue")
lines(data$Year, predict(expo), col = "red")
legend("bottomright", c("Time series", "Predicted"), 
       col = c("blue4", "red"), bty = "n", lwd = 1)

a = 1.616e-01/2.710e-02
a
#Residual Analysis
#Runs test (H0:residuals are randomly distibuted)
x8<-factor(sign((residuals(expo))))
x8
runs.test(residuals(expo))

#Test for normality
#Shapiro-Wilks test (H0:residuals are normally distributed)
shapiro.test(residuals(expo))


#Measures of Goodness of Fit
#############     MAPE
MLmetrics::MAPE(predict(expo),data$Area)

##########   R2_Score 
MLmetrics::R2_Score(predict(expo),data$Area)

###########     RMSE
MLmetrics::RMSE(predict(expo),data$Area)



AIC(expo)
BIC(expo)


#####################################

########     Monomolecular growth model
nls.monomolecular <- minpack.lm::nlsLM(data$Area ~alpha*
                                         (1-beta*exp(-k*data$Year)), 
                                       data = data, 
                                       start = list(alpha = 1.808506e+06, 
                                                    beta = 1.150241e+01, 
                                                    k = 1.219733e-03), 
                                       control = list(maxiter = 500))
coef(nls.monomolecular)
summary(nls.monomolecular)

#data
plot(data$Year,data$Area)
lines(data$Year, predict(nls.monomolecular), col = "red")
legend("bottomright", c("Time series", "Predicted"), 
       col = c("blue4", "red"), bty = "n", lwd = 1)
#Residual Analysis
#Runs test (H0:residuals are randomly distibuted)
x4<-factor(sign(residuals(nls.monomolecular)))
x4
runs.test(residuals(nls.monomolecular))

#Test for normality
#Shapiro-Wilks test (H0:residuals are normally distributed)
shapiro.test(residuals(nls.monomolecular))


#Measures of Goodness of Fit

###### MAPE
MLmetrics::MAPE(predict(nls.monomolecular),data$Area)

###### RMSE
MLmetrics::RMSE(predict(nls.monomolecular),data$Area)

########## r2_score
MLmetrics::R2_Score(predict(nls.monomolecular),data$Area)

AIC(nls.monomolecular)
BIC(nls.monomolecular)
predict(nls.monomolecular)


##############STRUCTURAL CHANGE

library(strucchange)

class(data)
data1<-as.ts(data$Area)
data1

test2 <- Fstats(data1~1) #Gets a sequence of fstatistics for all possible 

# break points within the middle 70% of myts1
plot(test2)
sctest(test2)
plot(data1)
lines(breakpoints(test2))
USN.fs <- test2$Fstats #These are the fstats

bd.USN <- breakdates(breakpoints(test2)) #Obtains the implied break data (2018.35, 
bd.USN

breakpoints(test2)
mean(data1[1:breakdates(breakpoints(test2))-1])

###mean area after to break
mean(data1[(breakdates(breakpoints(test2))):21])

#length(USN_Data1)
t.test(data1[1:11],data1[12:21])
#t = -3.9287, df = 14.584, p-value = 0.001408
#from the result we can say that it is significant in both 5% and 1%

