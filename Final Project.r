# FINAL PROJECT

library(rcompanion)
library(ggplot2)

data <- read.csv("FullDataSetTransposed.csv")


#DATA WRANGLING
## IS DATA NORMAL? IF NOT, TRANSFORM AS NEEDED. ALL DATA NEEDED TRANSFORMATION

data$ACTIVE.FULL.TIME <- transformTukey(data$ACTIVE.FULL.TIME, plotit = FALSE)
data$ACTIVE.PART.TIME <- transformTukey(data$ACTIVE.PART.TIME, plotit = FALSE)
data$ACTIVE.3.4.TIME <- transformTukey(data$ACTIVE.3.4.TIME, plotit = FALSE)
data$ACTIVE.Temp.Seasonal <- transformTukey(data$ACTIVE.Temp.Seasonal, plotit = FALSE)
data$Sales <- transformTukey(data$Sales, plotit = FALSE)
data$Accounting <- transformTukey(data$Accounting, plotit = FALSE)
data$Payroll <- transformTukey(data$Payroll, plotit = FALSE)
data$HR <- transformTukey(data$HR, plotit = FALSE)
data$Benefits <- transformTukey(data$Benefits, plotit = FALSE)
data$Gross.Profit <- transformTukey(data$Gross.Profit, plotit = FALSE)

## TESTING TRANSFORMED DATA FOR NORMALCY

### ACTIVE WORKSITE EMPLOYEES
plotNormalHistogram(data$ACTIVE.FULL.TIME)
#normal - need to add all active WSE employee columns together
plotNormalHistogram(data$ACTIVE.PART.TIME)
#not normal but as close as it will get - need to add all active WSE employee columns together
plotNormalHistogram(data$ACTIVE.3.4.TIME)
#not normal - need to add all active WSE employee columns together
plotNormalHistogram(data$ACTIVE.Temp.Seasonal)
#not normal but as close as it will get - need to add all active WSE employee columns together

### ACTIVE INTERNAL EMPLOYEES BY DEPARTMENT
plotNormalHistogram(data$Sales)
#not normal - however, these are not being anyalyzed independently, but rather will be denominators independently for the analysis
plotNormalHistogram(data$Accounting)
#not normal - however, these are not being anyalyzed independently, but rather will be denominators independently for the analysis
plotNormalHistogram(data$Payroll)
#not normal - however, these are not being anyalyzed independently, but rather will be denominators independently for the analysis
plotNormalHistogram(data$HR)
#not normal - however, these are not being anyalyzed independently, but rather will be denominators independently for the analysis
plotNormalHistogram(data$Benefits)
#not normal - however, these are not being anyalyzed independently, but rather will be denominators independently for the analysis

### GROSS PROFIT
plotNormalHistogram(data$Gross.Profit)
#normal

## CREATE NEW COLUMN TO ADD ALL ACTIVE WORKSITE EMPLOYEES TOGETHER INTO ONE
data$ActiveWSEs <- data$ACTIVE.FULL.TIME + data$ACTIVE.PART.TIME + data$ACTIVE.3.4.TIME + data$ACTIVE.Temp.Seasonal

plotNormalHistogram(data$ActiveWSEs)
#normal


# DATA ANALYSIS: What is the annual gross profit by active worksite employee (employees the company processes payroll for 
# outside of the internal team) and how does that compare to industry benchmarks?

lin_reg <- lm(Gross.Profit ~ ActiveWSEs, data)
print(lin_reg)

summary(lin_reg)

### p-value is significant at p < .05

### y = mx + b

x= 1000000.00
y = .000000000000000008297 * x -.00000001344
y

## Plot with best fit line

d <- ggplot(data, aes(x = ActiveWSEs, y = Gross.Profit))
d + geom_point() + geom_smooth(method=lm, se=FALSE)


