source("union_data.R")
source("helper_function.R")

#check Verteilung:
#
library(car)
require(MASS)
# This is so that distributions that must be non-zero can make sense of my
# data
mod_tab <- selct_art(arten = arten, variablen = variablen, sel_id = T)

qqp(mod_tab$alpha1, "norm")

poisson <- fitdistr(mod_tab$alpha1, "Poisson")
qqp(mod_tab$alpha1, "pois", poisson$estimate)

gamma <- fitdistr(mod_tab$shanno, "gamma")
qqp(mod_tab$shanno, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

qqp(mod_tab$shanno, "lnorm")

#Get Variogramm with best AIC modell
Vario2E <- Variogram(M2, form = ~ Dauer + Jahr,
                     data = mod_tab,
                     robust = TRUE, maxDist = 2000,
                     resType = "normalized")
plot(Vario2E, smooth = FALSE)



#Same for GAMM:

library(mgcv)
require(lme4)
library(tidyverse)
library(mgcViz)

#Define the fixed part of the model
fm <- gam(alpha1 ~ Dauer + ni +
            s(Ort, k = 9,  bs="re"), data = mod_tab)

f1 <- formula(alpha1 ~ Dauer + 
                s(Jahr, by = Ort, k = 9,  bs="cs")) #
f2 <- formula(alpha1 ~ Dauer + 
                s(Jahr, by = Ort, k = 9))

erg_gam <- get_gamm_mod(formula_gamm = f1, formula_Haw = f2)

erg_gam$gam[[1]]

#Make Crossvalidation
data1 <- cross_val(k = 10, model = erg_list[[1]], antw_var = mod_tab$shanno,
                   daten = mod_tab, fun = "M1",
                   correlation = NULL, weights = NULL, method = NULL, gls1 = T)

data2 <- cross_val(k = 10, model = erg_list[[2]], antw_var = mod_tab$shanno,
                   daten = mod_tab, fun = "M2",
                   correlation = corCompSymm(form = ~ Jahr), weights = NULL, method = NULL, gls1 = T)

data3 <- cross_val(k = 10, model = erg_list[[3]], antw_var = mod_tab$shanno,
                   daten = mod_tab, fun = "M3",
                   correlation = corAR1(form = ~ Jahr| id), weights = NULL, method = NULL, gls1 = T)

dataB1 <- cross_val(k = 10, model = f1, antw_var = mod_tab$shanno,
                    daten = mod_tab, fun = "B1",
                    correlation = NULL, weights = NULL, method = NULL, gls1 = F)

dataB2 <- cross_val(k = 10, model = f1, antw_var = mod_tab$shanno,
                    daten = mod_tab, fun = "B2",
                    correlation = NULL, weights = list(Jahr=~1), method = NULL, gls1 = F)

dataB3 <- cross_val(k = 10, model = f1, antw_var = mod_tab$shanno,
                    daten = mod_tab, fun = "B3",
                    correlation = corAR1(form =  ~ Jahr | Ort), weights = varIdent(form = ~ 1 | Ort),
                    method = NULL, gls1 = F)

dataH1 <- cross_val(k = 10, model = f2, antw_var = mod_tab$shanno,
                    daten = mod_tab, fun = "H1" ,
                    correlation =  corAR1(form = ~ Jahr | Ort), weights =  varIdent(form =~1 | Ort),
                    method = "REML", gls1 = F)

dataH2 <- cross_val(k = 10, model = f2, antw_var = mod_tab$shanno,
                    daten = mod_tab, fun = "H2",
                    correlation =  corLin(form =~ Jahr | Ort, nugget = TRUE), weights =  varIdent(form =~1 | Ort),
                    method = "REML", gls1 = F)

dataH3 <- cross_val(k = 10, model = f2, antw_var = mod_tab$shanno,
                    daten = mod_tab, fun = "H3",
                    correlation =  corGaus(form = ~ Jahr | Ort, 
                                           nugget = TRUE), weights =  varIdent(form =~1 | Ort),
                    method = "REML", gls1 = F)

dataH4 <- cross_val(k = 10, model = f2, antw_var = mod_tab$shanno,
                    daten = mod_tab, fun = "H4",
                    correlation =   corExp(form =~Jahr | Ort,
                                           nugget = TRUE), weights =  varIdent(form =~1 | Ort),
                    method = "REML", gls1 = F)

full_list <- rbind(dataH4, dataH3, dataB1, dataB2, data1, data2, data3)
full_list <- full_list %>% 
  mutate(RMSE = as.numeric(RMSE), 
         R2 = as.numeric(R2),
         ll = as.numeric(ll))
ggplot(full_list, aes(id, RMSE)) +
  geom_boxplot()
ggplot(full_list, aes(id, ll)) +
  geom_boxplot()
ggplot(full_list, aes(id, R2)) +
  geom_boxplot()
dE1 <- resid(HawD$gam)
F1 <- fitted(HawD$gam)
plot(F1, E1, xlab = "Fitted values", ylab = "Residuals")
AIC(HawB$lme, HawC$lme, HawD$lme)


