library(tidyverse)
library(lfe)

N <- 1000
n <- 100
Tt <- 5
g1 <- g2 <- c(1, 10, 100)

set.seed(98450437)

sim.shaver <- function(gamma, n = 100, Tt = 5) {
  u1 <- runif(n)
  u2 <- runif(n * Tt)
  u3 <- runif(n)
  u4 <- runif(n * Tt)
  u5 <- runif(n * Tt)
  
  expand_grid(i = 1:n, t = 1:Tt) %>%
    mutate(
      x1 = gamma[1] * u1[i] + gamma[2] * u2,
      x2 = gamma[3] * u3[i] + gamma[4] * u4,
      x3 = gamma[3] * u3[i] + gamma[5] * u5,
      e = rnorm(n * Tt, mean = 0, sd = 1),
      y = 100 + 500 * x1 + 5 * x1 * x3 + 1000 * e)
}

#### Single regression ####

df <- sim.shaver(rep(10,5))

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

#### Many regressions ####

dfData <- expand_grid(g1 = g1, g2 = g2, iter = 1:N) %>%
  mutate(
    df = map2(g1, g2, ~ sim.shaver(c(.x, .y, .x, .y, .y))),
    model = map(df, ~ felm(y ~ x1 * x2 | i, .x)),
    beta2 = map_dbl(model, ~ coef(.x)['x2']),
    beta3 = map_dbl(model, ~ coef(.x)['x1:x2']))
dfData2 <- dfData %>% select(-c(df, model))
dfData2 %>% group_by(g1, g2) %>% summarize(mean(beta3))

dfData2 %>% ggplot(aes(x = beta3)) + geom_histogram() +
  facet_grid(g1 ~ g2, scales = "free")
ggsave("output/miles-chart.pdf")
