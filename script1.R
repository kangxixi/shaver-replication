library(tidyverse)

n <- 100
Tt <- 5
gamma <- rep(10, 5)

set.seed(984590437)
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

