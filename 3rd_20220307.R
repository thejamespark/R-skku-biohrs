library(magrittr)
getwd()
a <- read.csv("example_g1e.csv")
head(a)

a[, "EXMD_BZ_YYYY"]
a %>% .[, "EXMD_BZ_YYYY"]

a %>% subset(EXMD_BZ_YYYY == 2009)

a %>% .$EXMD_BZ_YYYY

head(subset(a, EXMD_BZ_YYYY == 2009))
a %>% subset(EXMD_BZ_YYYY == 2009) %>% head

b <- subset(a, EXMD_BZ_YYYY %in% 2009:2012)
aggregate(. ~ Q_PHX_DX_HTN + Q_PHX_DX_DM, data = b,
          FUN = function(x){c(mean = mean(x), sd = sd(x))})

a %>% 
  subset(EXMD_BZ_YYYY %in% 2009:2012) %>% 
  aggregate(. ~ Q_PHX_DX_HTN + Q_PHX_DX_DM, data = ., 
            FUN = function(x){c(mean = mean(x), sd = sd(x))})

library(dplyr)
a <- read.csv("https://raw.githubusercontent.com/jinseob2kim/R-skku-biohrs/main/data/smc_example1.csv")
head(a)

a %>%
  filter(Sex == "M") %>%
  select(Sex:HTN) %>% 
  arrange(Age)
  
head(a)
a %>% 
  filter(Age >= 50) %>% 
  select(-STRESS_EXIST) %>% 
  group_by(Sex, Smoking) %>% 
  summarise_all(list(mean = mean, sd = sd))

a %>% 
  subset(Age >= 50) %>% 
  select(-STRESS_EXIST) %>% 
  aggregate(. ~Sex + Smoking, data = .,
            FUN = function(x){c(mean = mean(x), sd = sd(x))})
                