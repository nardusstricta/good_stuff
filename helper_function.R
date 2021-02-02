#get Mod Table:
nitr <- arten %>% 
dplyr::select(-treat_id) %>% 
  dplyr::filter(vegan::diversity(.) != 0) %>%
  dplyr::select(nutrient)

selct_art <- function(arten = arten, variablen = variablen, sel_id = T){
  #Auswählen der Wertvollen arten:
  
  treat_art1 <- arten %>% 
    dplyr::select(-treat_id, -nutrient) #remove treatid
  
  #get id from valid spec.
      wert_id <- read.csv("wert_id.csv") %>% 
        mutate(Artname = gsub(" ", ".", Artname)) %>% 
        dplyr::filter(Artname %in% colnames(treat_art1)) %>% 
        dplyr::filter(wert_id != 1)


    
  
  #Auswählen von den Wertvollen bzw. den Störzeigern:
  treat_art <- treat_art1 %>% 
    dplyr::select(wert_id$Artname) %>% 
    dplyr::filter(vegan::diversity(.) != 0)

  #Shannon Index errechen:
  shannon_index <- treat_art %>% 
    vegan::diversity()
  
  #Beta diversität berechnen:
  beta_div <- vegan::vegdist(log(treat_art + 1), method="bray")
  mat = as.matrix(beta_div)
  beta_div1 <- c(NA, mat[row(mat) == col(mat) + 1])
  
  #Alpha diversität berechenen:
  alpha2 <- vegan::specnumber(treat_art)
  
  #Beta diversität für die Beweidung:
  env_dist <- variablen %>% 
    dplyr::filter(vegan::diversity(treat_art1) != 0) %>% 
    dplyr::select(Kälber, Mutterkuh, Ziege, Zicklein, 
           Jungstute_hengst, Stuten, Esel, Hund, Schaf, Hengst)
  
  mat1 = as.matrix(dist(env_dist))
  env_dist1 <- c(NA, mat[row(mat1) == col(mat1) + 1])
  
  #Diversitäts Maße und Variablen zur Modelltabelle hinzufügen:
  mod_tab <- variablen %>% 
    dplyr::filter(vegan::diversity(treat_art1) != 0) %>% 
    dplyr::mutate(shanno = shannon_index,
           alpha1 = alpha2,
           beta1 = beta_div1) %>% 
    dplyr::mutate(beta1 = ifelse(Jahr == 2010, NA, beta1)) %>% 
    dplyr::mutate(env_dist = ifelse(Jahr == 2010, NA, env_dist1))
  return(mod_tab)

}

get_gls_mod <- function(formula_gls){
  #Model Using Generalized Least Squares
  M0 <- gls(formula_gls,
            na.action = na.omit, data = mod_tab)
  
  M1 <- gls(formula_gls,
            na.action = na.omit, data = mod_tab,
            correlation = corCompSymm(form = ~ Jahr))
  
  M2 <- gls(formula_gls,
            data = mod_tab,
            correlation = corAR1(form = ~ Jahr| Ort))
  return(list(M0, M1, M2))
}

get_gamm_mod <- function(formula_gamm = f1, formula_Haw){
  BM0 <- try(gamm(formula_gamm, data = mod_tab), silent=TRUE)
  
  BM1 <-  try(gamm(formula_gamm, data = mod_tab, weights = varIdent(form = ~1 | Ort), control = list(opt = "optim")), silent=TRUE)
  
  BM2 <- try(gamm(formula_gamm,
              correlation = corAR1(form =  ~ Jahr | Ort),
              weights = varIdent(form = ~ 1 | Ort), data = mod_tab), silent=TRUE)
  #Fit the gamms
  HawA <-  try(gamm(formula_Haw, method = "REML", correlation =
                      corAR1(form = ~ Jahr | Ort),
                    weights = varIdent(form =~1 | Ort), 
                    data = mod_tab, control = list(opt = "optim")), silent=TRUE)
  
  
  HawB <- try(gamm(formula_Haw, method = "REML", correlation =
                 corLin(form =~ Jahr | Ort, nugget = TRUE),
               weights = varIdent(form =~1| Ort), data = mod_tab, control = list(opt = "optim")), silent=TRUE)
  
  HawC <- try(gamm(formula_Haw, method = "REML", correlation =
                 corGaus(form = ~ Jahr | Ort, nugget = TRUE),
               weights = varIdent(form =~ 1| Ort), data = mod_tab, control = list(opt = "optim") ), silent=TRUE)
  
  HawD <- try(gamm(formula_Haw, method = "REML", correlation =
                 corExp(form =~Jahr | Ort, nugget = TRUE),
               weights = varIdent(form = ~ 1 | Ort), data = mod_tab, control = list(opt = "optim")), silent=TRUE)
  return(list(gam = list(BM0, BM1, BM2), Ha = list(HawA, HawB, HawC, HawD)))

}


ell <- function(test, predsCV)  sum(dnorm(test, log=T), na.rm=T)
#Cross_validation
cross_val <- function(k, model1, antw_var, daten, fun,
                      correlation = NULL, weights = NULL, method = NULL, gls1 = T){
  ind <- sample(10, length(antw_var), replace = T)
  crossmat <- as.data.frame(matrix(NA, k, 4))
  colnames(crossmat) <- c("RMSE", "R2", "ll", "id")
  if(gls1==T){
    for(i in 1:k){
      fm_cv <- gls(model1, data = daten[!(ind==i),], correlation = correlation)  
      preds <- predict(fm_cv, newdata = daten[ind==i,], type="response")
      rmse <- sqrt(mean((antw_var[ind==i] - preds)^2))
      r2 <- cor(preds, antw_var[ind==i])^2
      ll <- ell(antw_var[ind == i], preds)
      crossmat[i, ] <- c(rmse, r2, ll, fun)
    }
    }else{
      for(i in 1:k){
        fm_cv <- gamm(model1,  correlation = correlation,
                      weights = weights, method = method,
                      data = daten[!(ind==i),], control = list(opt = "optim")) 
        preds <- predict(fm_cv$gam, newdata = daten[ind==i,], type="response")
        rmse <- sqrt(mean((antw_var[ind==i] - preds)^2))
        r2 <- cor(preds, antw_var[ind==i])^2
        ll <- ell(antw_var[ind == i], preds)
        crossmat[i, ] <- c(rmse, r2, ll, fun)
  }
    }
    return(crossmat)
}

Score <- function(x, retrans = F, daten = NA) {
  z <- c(1:length(x))
  for (i in 1:length(x)){
    if (retrans ==T) {
      z[i] <- x[i] * sd(daten) + mean(daten)
    }else{
      z[i] <- (x[i] - mean(x))/sd(x)
    }
  }
  return(z) 
}
