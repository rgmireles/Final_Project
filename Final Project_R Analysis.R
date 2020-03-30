### FINAL PROJECT
### WorkSite Employee per Internal Employee Ratio (including Sales)

## IMPORT LIBRARIES

library(rcompanion)
library(ggplot2)
library(dplyr)

## IMPORT DATA

DataSet <- read_xlsx("/Users/rubengarcia/Desktop/DataSet.xlsm")

## NORMALITY TEST

plotNormalHistogram(DataSet$WS_INT_Ratio_S)

## Variables Correlation Chart: Gross Profit - Total Internal Employees

d <- ggplot(DataSet, aes(x = Gross_Profit, y = TTL_INT_S))
d + geom_point() + geom_smooth(method=lm, se=FALSE)

## Pearson's Correlation Test

cor.test(DataSet$Gross_Profit, DataSet$TTL_INT_S, method="pearson", use = "complete.obs")

## Variables Correlation Chart: Total External Employees - Total Internal Employees

d <- ggplot(DataSet, aes(x = TTL_EXT, y = TTL_INT))
d + geom_point() + geom_smooth(method=lm, se=FALSE)

d <- ggplot(DataSet, aes(x = Gross_Profit, y = WS_INT_Ratio_S))
d + geom_point() + geom_smooth(method=lm, se=FALSE)


## Descriptive Statistics - Vector TTL_EXT

TTLEXT <- DataSet$WS_INT_Ratio_S
mean(TTLEXT, trim = 0.05, na.rm = FALSE)
max(TTLEXT)
min(TTLEXT)
var(TTLEXT)
sd(TTLEXT)

## Box Plot Profit_EXT_EE

d <- ggplot(DataSet, aes(x = "", y = WS_INT_Ratio_S))
d + geom_boxplot() + xlab("")

## Normal Probability Plots (Normal data)

ggplot(DataSet, aes(sample = WS_INT_Ratio_S)) + geom_qq()

## Scatter Plots

d <- ggplot(DataSet, aes(x = Gross_Profit, y = WS_INT_Ratio_S))
d + geom_point() + geom_smooth(method=lm)

## Linear Regression

lin_reg <- lm(WS_INT_Ratio_S ~ Gross_Profit, data = DataSet)
print(lin_reg)

summary(lin_reg)

## Best fit line - linear Regression

d <- ggplot(DataSet, aes(x = Gross_Profit, y = WS_INT_Ratio_S))
d + geom_point() + geom_smooth(method=lm, se=FALSE)

summary(lin_reg)

## Statistical Inference
## One sample t-test
##  Ho: m = 110    H1: m # 110

library("DAAG")

t_obj <- t.test(DataSet$WS_INT_Ratio_S, mu = 110)
print(t_obj)

t_obj2 <- t.test(DataSet$WS_INT_Ratio, mu = 129)
print(t_obj2)
