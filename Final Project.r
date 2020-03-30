# FINAL PROJECT

library(rcompanion)
library(ggplot2)
library(dplyr)

data <- read.csv("FullDataSetTransposed.csv")

data2 <- data.frame(data)


#DATA WRANGLING
## IS DATA NORMAL? IF NOT, TRANSFORM AS NEEDED. ALL DATA NEEDED TRANSFORMATION

data2$ACTIVE.FULL.TIME <- transformTukey(data$ACTIVE.FULL.TIME, plotit = FALSE)
data2$ACTIVE.PART.TIME <- transformTukey(data$ACTIVE.PART.TIME, plotit = FALSE)
data2$ACTIVE.3.4.TIME <- transformTukey(data$ACTIVE.3.4.TIME, plotit = FALSE)
data2$ACTIVE.Temp.Seasonal <- transformTukey(data$ACTIVE.Temp.Seasonal, plotit = FALSE)
data2$Sales <- transformTukey(data$Sales, plotit = FALSE)
data2$Accounting <- transformTukey(data$Accounting, plotit = FALSE)
data2$Payroll <- transformTukey(data$Payroll, plotit = FALSE)
data2$HR <- transformTukey(data$HR, plotit = FALSE)
data2$Benefits <- transformTukey(data$Benefits, plotit = FALSE)
data2$Gross.Profit <- transformTukey(data$Gross.Profit, plotit = FALSE)

## TESTING TRANSFORMED DATA FOR NORMALCY

### ACTIVE WORKSITE EMPLOYEES
plotNormalHistogram(data2$ACTIVE.FULL.TIME)
#normal - need to add all active WSE employee columns together
plotNormalHistogram(data2$ACTIVE.PART.TIME)
#not normal but as close as it will get - need to add all active WSE employee columns together
plotNormalHistogram(data2$ACTIVE.3.4.TIME)
#not normal - need to add all active WSE employee columns together
plotNormalHistogram(data2$ACTIVE.Temp.Seasonal)
#not normal but as close as it will get - need to add all active WSE employee columns together

### ACTIVE INTERNAL EMPLOYEES BY DEPARTMENT
plotNormalHistogram(data2$Sales)
#not normal - however, these are not being anyalyzed independently, but rather will be denominators independently for the analysis
plotNormalHistogram(data2$Accounting)
#not normal - however, these are not being anyalyzed independently, but rather will be denominators independently for the analysis
plotNormalHistogram(data2$Payroll)
#not normal - however, these are not being anyalyzed independently, but rather will be denominators independently for the analysis
plotNormalHistogram(data2$HR)
#not normal - however, these are not being anyalyzed independently, but rather will be denominators independently for the analysis
plotNormalHistogram(data2$Benefits)
#not normal - however, these are not being anyalyzed independently, but rather will be denominators independently for the analysis

### GROSS PROFIT
plotNormalHistogram(data2$Gross.Profit)
#normal

## CREATE NEW COLUMN TO ADD ALL ACTIVE WORKSITE EMPLOYEES TOGETHER INTO ONE
data2$ActiveWSEs <- data2$ACTIVE.FULL.TIME + data2$ACTIVE.PART.TIME + data2$ACTIVE.3.4.TIME + data2$ACTIVE.Temp.Seasonal


plotNormalHistogram(data2$ActiveWSEs)
#normal


# DATA ANALYSIS: What is the annual gross profit by active worksite employee (employees the company processes payroll for 
# outside of the internal team) and how does that compare to industry benchmarks?


###add new columnS to include all active WSE's and totals for Annual GP and average Active WSE's
data$ActiveWSEs <- data$ACTIVE.FULL.TIME + data$ACTIVE.PART.TIME + data$ACTIVE.3.4.TIME + data$ACTIVE.Temp.Seasonal
data$AnnualGPAll <- sum(data$Gross.Profit)
data$AvgActiveWSEs <- mean(data$ActiveWSEs)

GPbyWSE <- data$AnnualGPAll/data$AvgActiveWSEs
GPbyWSE
BMGPbyWSE <- 1595
gapGPbyWSE <- (BMGPbyWSE - GPbyWSE)
gapGPbyWSE

#Predictions: Predict when the company will reach either revenue goals or worksite employee goals based on prior year growth trends.

## Plot with best fit line

d <- ggplot(data, aes(x = ActiveWSEs, y = Gross.Profit))
d + geom_point() + geom_smooth(method=lm, se=FALSE)

###Positive correlation

## Linear regression for finding the line formula for predicting Annual GP by entering number of WSE's

lin_reg <- lm(Gross.Profit ~ ActiveWSEs, data)
print(lin_reg)

summary(lin_reg)


### p-value is very significant at p < .0001

## y = mx + b Predicting Values

x= 4542 ### average number of active WSE's to predict gross profit
y = 76.04 * x +38470.78
y
y*12 #Annual gross profit at x WSE's



## Linear regression for finding the line formula for predicting number of WSE's needed to produce x amount of GP

lin_reg <- lm(ActiveWSEs ~ Gross.Profit, data)
print(lin_reg)

summary(lin_reg)

### p-value is very significant at p < .0001

### y = mx + b Predicting Values

x= 5000000 ### gross profit target to predict number of WSE's to get there
y = .0008538 * x + 272.6
y




