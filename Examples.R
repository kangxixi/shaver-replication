# Rstudio:
# - Autocompletion: tab
# - Help: F1
example(lm, ask=F)

# Defining & plotting vectors
x <- c(0.9, 2.8,2.9,3.1,3.7,4.1,4.2,4.9,5.1)
hist(x)
length(x)
y <- x + rnorm(length(x), sd=1)
plot(x,y)

# Basic arithmetics
x + y
x + 10 # Note: 'recycling'
x * y # Element-wise!
x %*% y # Vector / matrix multiplication (in casu, inner product)
sum(x * y)
x > y
mean(x)
var(x) # Sample variance
sqrt(var(x)) # Standard-deviation
summary(x)
var(x)
cor(x,y)
cov(x,y)
lm(y~x)
cov(x,y) / var(x)

# Data frame
df <- data.frame(x,y)
str(df)
summary(df)
summary(lm(y~x, df))
length(df)
nrow(df)
ncol(df)
names(df)
colnames(df)
rownames(df)
as.matrix(df)
t(as.matrix(df)) %*% x

# Selection
x[2]
df[2,'x']
df[2,1]
df[,'x']
df$x
df$x[2]

x > 3
x == 4.1
x != 4.1
!(x == 4.1)
(x >= 3 & x <= 5)

x[x>3]
x[x >= 3 & x <= 5]
x[y>3]
df2 <- df[df$y > 3,]
df2

# Changing data
df2$y[df2$x > 4.5] <- 8
df2
df2 <- rbind(df2, c(5.5,9))
df2

model <- lm(y~x, df)
summary(model)
model2 <- lm(y~x, df2)  # Can easily use multiple data sets in same session!
summary(model2)

# Looking at some actual data
dfCompu <- read.csv('compustat.csv')
str(dfCompu)
summary(dfCompu)
dfCompu$roa <- with(dfCompu, oiadp / at)
summary(dfCompu$roa)
with(dfCompu, plot(log(at), roa))

winsor <- function(y, quant=c(0.01, 0.99)){
  bound <- quantile(y, probs=quant, na.rm=T)
  y <- ifelse(y > bound[2], bound[2], y)
  y <- ifelse(y < bound[1], bound[1], y)
  y
}

with(dfCompu, plot(log(at), winsor(roa)))
with(dfCompu[dfCompu$at > 100,], plot(log(at), winsor(roa)))

dfCompu2 <- with(dfCompu, dfCompu[at > 100 & !is.na(at) & !is.na(oiadp),])
summary(dfCompu2)  # Also see Envirnoment panel in top-right

model1 <- lm(roa ~ at, dfCompu2)
summary(model1)
plot(model1)

model2 <- lm(roa ~ log(at), dfCompu2)
summary(model2)
plot(model2)

model3 <- lm(winsor(roa) ~ log(at), dfCompu2)
summary(model3)
plot(model3)

model4 <- lm(winsor(roa) ~ log(at) + I(log(at)^2) + I(log(at)^3), dfCompu2) # Note the I()
summary(model4) # Note: all powers highly significant
plot(model4)

# Let's add some FE and clustered standard errors
model5 <- lm(winsor(roa) ~ log(at) + I(log(at)^2) + I(log(at)^3) + factor(sic), dfCompu2) # SLOW!
summary(model5) # Note: all powers highly significant

install.package('lfe')
library(lfe) # Much faster & can handle clustered standard errors

model5b <- felm(winsor(roa) ~ log(at) + I(log(at)^2) + I(log(at)^3) | factor(sic) | 0 | sic, dfCompu2)
summary(model5b)
plot(model5b) # Doesn't work :(
qqnorm(resid(model5b))
plot(log(dfCompu2$at), resid(model5b)) # Note: clear need for heteroscedasticity robust s.e.

model6 <- felm(winsor(roa) ~ log(at) + I(log(at)^2) + I(log(at)^3) |            # Regression
                 factor(gvkey)  + factor(sic) : factor(fyear) |  # Fixed effects
                 0 |                                                            # Instrumental variables
                 sic + fyear,                                                           # Clustering for se
               dfCompu2)
summary(model6)

library(texreg)
texreg(list(model1, model2, model3, model4, model5b, model6))
htmlreg(list(model1, model2, model3, model4, model5b, model6), file='example.html')

library(ggplot2)
ggplot(dfCompu2, aes(y=winsor(roa), x=log(at))) + geom_point(aes(color=factor(fyear))) + geom_smooth()
ggsave('example.pdf')

dfYear <- aggregate(cbind(oiadp, at) ~ fyear, dfCompu, sum)
dfYear$roa <- with(dfYear, oiadp/at)
dfYear
