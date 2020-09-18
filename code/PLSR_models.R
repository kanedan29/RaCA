options(java.parameters = "-Xmx10g")
library(tidyverse)
library(bartMachine)
library(caret)
library(pls)

d <- data.table::fread("data/raca_scans_merged.csv")
d <- d %>%
  rename_at(.vars = vars(3:2153), .funs = function(x) paste("NM", x, sep = "_")) %>%
  select(-scan_id) %>% 
  mutate(smp_id = as.character(smp_id))

d.carbon <- data.table::fread("data/RaCA_Waves.csv") %>%
  select(-starts_with("X")) %>%
  mutate(sample = as.character(sample)) %>%
  right_join(d, by = c("sample" = "smp_id")) %>%
  filter(!is.na(Carbon))

## Reduced BART

bart.vars <- c("NM_365","NM_385","NM_450","NM_500","NM_530",
               "NM_587","NM_632","NM_850","NM_880","NM_940")

set.seed(4917)
split <- createDataPartition(d.carbon$Carbon, times = 1, p = 0.8, list = F)

red.bart <- d.carbon[split,] %>%
  dplyr::select(bart.vars) %>%
  bartMachine(X = ., y = d.carbon[split,"Carbon"], serialize = T)


gc()
XLConnect::xlcFreeMemory()

test <- d.carbon[-split,] %>%
  mutate(pred = predict(red.bart, new_data = d.carbon[-split,] %>%
                          dplyr::select(bart.vars)),
         resid = pred - Carbon)



test2 <- test %>%
  filter(Carbon < 10)

mean(abs(test2$resid))

ggplot(test2, aes(x = Carbon, y = pred))+
  geom_jitter() +
  geom_abline(slope = 1) + 
  xlim(0,10) + 
  ylim(0,10)


# PLSR models

temp <- as.matrix(d.carbon[11:2161])

d <- data.frame(d.carbon[1:10],  VNIR = rep(0, nrow(d.carbon)))
d$VNIR <- temp

m.PLSR.LU.C <- d %>%
  filter(LU == "C") %>%
  plsr(Carbon ~ VNIR, data =.,validation = "CV")

selectNcomp(m.PLSR.LU.C)

cv = RMSEP(m.PLSR.LU.C)
best.dims = which.min(cv$val[estimate = "adjCV", , ]) - 1


plot(m.PLSR.LU.C, plottype = "coefficients", ncomp = 27)
plot(m.PLSR.LU.C)


coefficients = coef(m.PLSR.LU.C)
sum.coef = sum(sapply(coefficients, abs))
coefficients = coefficients * 100 / sum.coef
coefficients = sort(coefficients[, 1 , 1])
barplot(tail(coefficients, 40))
coefficients.abs = abs(coefficients)

## For samples with <10 % C

d %>%
  filter(LU == "C",
         Carbon < 10) -> test


plsr(Carbon ~ VNIR, data =test,validation = "CV") -> m.PLSR.LU.C.2

selectNcomp(m.PLSR.LU.C.2)

plot(m.PLSR.LU.C.2, plottype = "coefficients", ncomp = 15)
plot(m.PLSR.LU.C)




coefficients = coef(m.PLSR.LU.C.2)
sum.coef = sum(sapply(coefficients, abs))
coefficients = coefficients * 100 / sum.coef
coefficients = sort(coefficients[, 1 , 1])
barplot(tail(coefficients, 40))
coefficients.abs = abs(coefficients)
View(coefficients.abs)




  
  


