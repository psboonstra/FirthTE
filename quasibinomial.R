library(tidyverse)
library(logistf)

n_pop <- 5e6
n_samp <- 50

pop_dat <- 
  tibble(
    x1 = rbinom(n_pop, 1, 0.005), 
    x2 = rbinom(n_pop, 1, 0.1),
    x3 = rbinom(n_pop, 1, 0.2)) %>%
  mutate(
    y = rbinom(n_pop, 1, plogis(-3 + log(3) * x1 - log(2.5) * x2))
  )

# True treatment effect:
round(mean(pop_dat %>% filter(x1 == 1) %>% pull(y)) -
        mean(pop_dat %>% filter(x1 == 0) %>% pull(y)), 4)

sample_dat <- 
  pop_dat %>%
  slice_sample(n = n_samp, replace = FALSE) %>%
  mutate(weight = 1, orig = 1)
new_dat1 <- sample_dat %>% mutate(x1 = 1)
new_dat0 <- sample_dat %>% mutate(x1 = 0)


library(marginaleffects)

dat <- bind_rows(sample_dat, 
                 sample_dat %>% mutate(y = 0, weight = mod0F$hat.diag / 2, orig = 0), 
                 sample_dat %>% mutate(y = 1, weight = mod0F$hat.diag / 2, orig = 0))
new_dat1 <- sample_dat %>% mutate(x1 = 1)
new_dat0 <- sample_dat %>% mutate(x1 = 0)

mod0FL <- glm(y ~ x1 + orig, data = dat,
              weights = weight,
              family = "binomial")

mod0FLb <- glm(y ~ x1 + orig, data = dat,
              weights = weight,
              family = "quasibinomial")

mean(predict(mod0FL, newdata = new_dat1, type = "resp") - predict(mod0FL, newdata = new_dat0, type = "resp"))
mean(predict(mod0FLb, newdata = new_dat1, type = "resp") - predict(mod0FLb, newdata = new_dat0, type = "resp"))

avg_comparisons(mod0FL, variables = "x1", newdata = sample_dat)
avg_comparisons(mod0FLb, variables = "x1", newdata = sample_dat)
