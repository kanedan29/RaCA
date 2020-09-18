options(java.parameters = "-Xmx10g")
library(tidyverse)
library(bartMachine)
library(caret)
library(pls)

load("data/rdata/d_20190107.RData")

bart.vars <- c("NM_365","NM_385","NM_450","NM_500","NM_530",
               "NM_587","NM_632","NM_850","NM_880","NM_940", "NM_1400")

d.save <- d

d <- d.save %>%
  filter(c_tot_ncs < 5)


set.seed(4917)
split <- createDataPartition(d$c_tot_ncs, times = 1, p = 0.8, list = F)



red.bart <- d[split,] %>%
  dplyr::select(bart.vars) %>%
  bartMachine(X = ., y = d[split,"c_tot_ncs"], serialize = T)


gc()
XLConnect::xlcFreeMemory()

test <- d[-split,] %>%
  mutate(pred = predict(red.bart, new_data = d[-split,] %>%
                          dplyr::select(bart.vars)),
         resid = pred - c_tot_ncs)

test2 <- test

mean(abs(test2$resid))

ggplot(test2, aes(x = c_tot_ncs, y = pred, color = efferv_1nhcl))+
  geom_jitter() +
  geom_abline(slope = 1) + 
  xlim(0,10) + 
  ylim(0,10)

test2 %>%
  filter(lay_depth_to_top < 30, 
         ) %>%
ggplot(., aes(x = sand, y = c_tot_ncs))+
  geom_jitter() 
  facet_wrap("model_desg1", nrow = 5, ncol =5)

