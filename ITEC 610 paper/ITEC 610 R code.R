#Prince Enyiorji (AU ID:5527508)
#Wasif Zawad Talukder (AU ID: #)
#ITEC 610: Managerial Statistics
#Final Project
#Install and load packages
install.packages(c("gridExtra","summarytools","stargazer","ggplot2","pwr"))
library(gridExtra)
library(summarytools)
library(stargazer)
library(ggplot2)
library(car)
library(pwr)

#Import cleaned data from excel: Check for missing values and checked for outliers using IQR and replaced with column vector mean value
StartupData <- read.csv(file.choose())

#Confirm name of headers
names(StartupData)

#Recheck for missing values
MVs <- sum(is.na(StartupData))

#Quantitative Variables Analysis
descr(StartupData$loan_investment_amount,na.rm=F)
descr(StartupData$revenue,na.rm=F)
descr(StartupData$full_time_employees,na.rm=F)

#Categorical Variables Analysis
freq(StartupData$LMI_type,report.nas = F)
freq(StartupData$program_type,report.nas = F)

#Recode for program type 
unique(StartupData$program_type)
Program_type <- as.factor(StartupData$program_type)
contrasts(Program_type) <- contr.treatment(5, base = 5) #Source: R Dummy Coding
StartupData$program_type
CAPR <- c(1,0,0,0,0)
CSR <- c(0,1,0,0,0)
LGR <- c(0,0,1,0,0)
LPR <- c(0,0,0,1,0)
contrasts(Program_type) <- cbind(CAPR,CSR,LGR,LPR)

#Recode for LMI type... 
LMI <- ifelse(StartupData$LMI_type == "LMI",1,0)
Non_LMI <- ifelse(StartupData$LMI_type == "Non-LMI",1,0)

#Run regression model
MLS <- lm(loan_investment_amount ~ revenue + full_time_employees + program_type + LMI, data = StartupData)
summary(MLS) #Regression Model is statistically significant P-value<0.01

stargazer(MLS,type = "text")

#Effect size = (rsqd/(1-rsqd))
f <- sqrt(0.222/(1-0.222))

#Power
pwr.f2.test(u=6,v=21962,f2=f,sig.level = 0.01,power = NULL)

#Test for Multicollinearity
vif(MLS) #All independent variables in the model are moderately correlated 

#Multiple Regression Diagnosis
par(mfrow = c(2,2))
plot(MLS)

#Multiple Regression Plot
RevPlot <- ggplot(data = StartupData, aes(x = revenue, 
                                          y = loan_investment_amount, col=Program_type)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "MLR:Revenue and Loan Investment")


FTEPlot <- ggplot(data = StartupData, aes(x = full_time_employees, 
                                          y = loan_investment_amount, col=LMI_type)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "MLR:Full time Employess and Loan Investment")

#Merge Multiple Regression Plots
#Source: Stack overflow
grid.arrange(RevPlot,FTEPlot, nrow = 1, ncol = 2)

#Scatter Plots
ggplot(data = StartupData, aes(x = revenue, 
                               y = loan_investment_amount, 
                               col=Program_type)) +
  geom_point()

ggplot(data = StartupData, aes(x = full_time_employees, 
                               y = loan_investment_amount, col=LMI_type)) +
  geom_point()

