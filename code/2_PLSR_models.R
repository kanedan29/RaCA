options(java.parameters = "-Xmx10g")
library(tidyverse)
library(bartMachine)
library(caret)
library(pls)

load("data/rdata/d_20190107.RData")


# PLSR models

temp <- as.matrix(d[136:2286])

d2 <- data.frame(d[1:135],  VNIR = rep(0, nrow(d)))
d2$VNIR <- temp


# global model
m.PLSR <- d2 %>%
  filter(M %in% c("O","A","B"),
         c_tot_ncs < 10) %>%
  plsr(c_tot_ncs ~ VNIR, data =.,validation = "CV")

selectNcomp(m.PLSR, plot = TRUE, ncomp = 65, ylim = c(0,2.25), xlim = c(0,75))

cv <- RMSEP(m.PLSR)
best.dims <- which.min(cv$val[estimate = "adjCV", , ]) - 1


plot(m.PLSR, plottype = "coefficients", ncomp = 10)


summary(m.PLSR)
coefficients = coef(m.PLSR)
sum.coef = sum(sapply(coefficients, abs))
coefficients = coefficients * 100 / sum.coef
coefficients = sort(coefficients[, 1 , 1])
barplot(tail(coefficients, 40))
coefficients.abs = abs(coefficients)

## For samples with <10 % C

m.PLSR.sub10c <- d2 %>%
  filter(c_tot_ncs < 10) %>%
  plsr(c_tot_ncs ~ VNIR, data =.,validation = "CV")


plot(m.PLSR.sub10c, plottype = "coefficients", ncomp = 10)


