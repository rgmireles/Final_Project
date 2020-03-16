# Normality Testing

plotNormalHistogram(FullDataSetTransposed$Gross_Profit)

plotNormalHistogram(FullDataSetTransposed$TTL_EXT)

plotNormalHistogram(FullDataSetTransposed$TTL_INT)


# Variables Correlation Chart: Gross Profit - Total External Employees

d <- ggplot(FullDataSetTransposed, aes(x = Gross_Profit, y = TTL_EXT))
d + geom_point() + geom_smooth(method=lm, se=FALSE)

## Pearson's Correlation Test

cor.test(FullDataSetTransposed$Gross_Profit, FullDataSetTransposed$TTL_EXT, method="pearson", use = "complete.obs")


# Variables Correlation Chart: Total External Employees - Total Internal Employees

d <- ggplot(FullDataSetTransposed, aes(x = TTL_EXT, y = TTL_INT))
d + geom_point() + geom_smooth(method=lm, se=FALSE)

## Pearson's Correlation Test

cor.test(FullDataSetTransposed$TTL_EXT, FullDataSetTransposed$TTL_INT, method="pearson", use = "complete.obs")

# Descriptive Statistics - Vector TTL_EXT

TTLEXT <- FullDataSetTransposed$TTL_EXT
mean(TTLEXT, trim = 0.05, na.rm = FALSE)
max(TTLEXT)
min(TTLEXT)
var(TTLEXT)
sd(TTLEXT)

# Descriptive Statistics - Vector Profit_EXT_EE

Pro_ExEE <- FullDataSetTransposed$Profit_EXT_EE
mean(Pro_ExEE, trim = 0.05, na.rm = FALSE)
max(Pro_ExEE)
min(Pro_ExEE)
var(Pro_ExEE)
sd(Pro_ExEE)


# Box Plot Profit_EXT_EE

d <- ggplot(FullDataSetTransposed, aes(x = "", y = Profit_EXT_EE))
d + geom_boxplot() + xlab("")

# Normal Probability Plots (Normal data)

ggplot(FullDataSetTransposed, aes(sample = Profit_EXT_EE)) + geom_qq()


# Scatter Plots

d <- ggplot(FullDataSetTransposed, aes(x = Gross_Profit, y = TTL_EXT))
d + geom_point() + geom_smooth(method=lm)


# Linear Regression

lin_reg <- lm(TTL_EXT ~ Gross_Profit, data = FullDataSetTransposed)
print(lin_reg)

summary(lin_reg)

# Best fit line - linear Regression

d <- ggplot(FullDataSetTransposed, aes(x = Gross_Profit, y = TTL_EXT))
d + geom_point() + geom_smooth(method=lm, se=FALSE)

summary(lin_reg)




