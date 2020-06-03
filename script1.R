library(tidyverse)
library(lfe)

n <- 100
Tt <- 5
gamma <- rep(10, 5)

set.seed(98450437)
u1 <- runif(n)
u2 <- runif(n * Tt)
u3 <- runif(n)
u4 <- runif(n * Tt)
u5 <- runif(n * Tt)

df <- expand_grid(i = 1:n, t = 1:Tt) %>%
  mutate(
    x1 = gamma[1] * u1[i] + gamma[2] * u2,
    x2 = gamma[3] * u3[i] + gamma[4] * u4,
    x3 = gamma[3] * u3[i] + gamma[5] * u5,
    e = rnorm(n * Tt, mean = 0, sd = 1),
    y = 100 + 500 * x1 + 5 * x1 * x3 + 1000 * e)

pairs(df %>% select(x1:y))
model1 <- lm(y ~ x1 * x2 + factor(i) - 1, df)
summary(model1)
plot(model1)

model2 <- felm(y ~ x1 * x2 | i, df)
summary(model2)

model3 <- felm(y ~ x1 * x2 | i + t | 0 | i, df)
summary(model3)

df <- df %>% mutate(r = as.vector(resid(model2)))
ggplot(df, aes(x = x1, y = r, color = t)) +
  geom_point()
