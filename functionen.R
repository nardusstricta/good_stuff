#Funktionen--------

#Loglike

ell <- function(test, predsCV)  sum(dbinom(test, size=1, prob=predsCV, log=T), na.rm=T)
#Cross_validation
cross_val <- function(k, model, antw_var, daten, fun=2){
  zero <- which(antw_var==0)
  ones <- which(antw_var==1)
  ind <- 1
  ind[zero] <- sample(10, length(zero), replace = T)
  ind[ones] <- sample(10, length(ones), replace = T)
  crossmat <- as.data.frame(matrix(NA, k, 3))
  colnames(crossmat) <- c("RMSE", "R2", "ll")
  for(i in 1:k){
    if(fun==4){
      fm_forest <- randomForest(formula(model)[1:3], data = daten[!(ind==i),], type="prob")
      preds <- predict(fm_forest, data = daten[ind==i,], type="prob")[2]
      rmse <- sqrt(mean((art[ind==i] - preds)^2))
      r2 <- NA
      ll <- ell(art[ind == i], predict(lasso, newx=daten[ind==i,]))
    }else{
      if(fun==1){
      lasso.cv <- cv.glmnet(x=daten[!(ind==i),], y=art[!(ind==i)], alpha=1, family=c("binomial"), standardize=FALSE)
      lasso <- glmnet(x= daten[!(ind==i),], y=art[!(ind==i)], lambda=lasso.cv$lambda.min, alpha=1, family=c("binomial"), standardize=FALSE)
      rmse <- sqrt(mean((art[ind==i]- predict(lasso, newx=daten[ind==i,], type="response"))^2))
      r2 <- cor(predict(lasso, newx=daten[ind==i,]), art[ind==i])^2
      ll <- ell(art[ind == i], predict(lasso, newx=daten[ind==i,]))
    }else{
    if(fun==2){
    fm_cv <- glm(formula(model), data = daten[!(ind==i),], family = binomial)
    }
    if(fun==3){
    fm_cv <- gam(formula(model), data = daten[!(ind==i),], family = binomial)  
    }
    preds <- predict(fm_cv, newdata = daten[ind==i,], type="response")
    rmse <- sqrt(mean((art[ind==i] - preds)^2))
    r2 <- cor(preds, art[ind==i])^2
    ll <- ell(art[ind == i], preds)
    }
  }
    crossmat[i, ] <- c(rmse, r2, ll)
  }
  #crossmat$namen <- rep(modellname, nrow(crossmat))
  return(crossmat)
}


#Subsampling
subsam <- function(reps, trainProp, model, daten){
  fold <- numeric(reps)
  for (k in 1:reps){
    trainSize <- floor(trainProp * nrow(daten))
    CVindex <- sample(nrow(daten)) # shuffle row numbers
    fmcv <- glm(formula(model), data=daten[CVindex[1:trainSize],], family=binomial)
    predscv <- predict(fmcv, newdata=daten[CVindex[(trainSize+1):nrow(daten)],])
    fold[k] <- sqrt(mean((art[CVindex[(trainSize+1):nrow(daten)]] - predscv)^2, na.rm=T))
    rm(fmcv, predscv)
  }
  return(fold)
}

#bootstaping
#
formula(fmst)
bot_mat <- bootst(3, fmst, predi_trans)
bootst <- function(steps, model, daten){
  bot_mat <- matrix(NA, steps, length(coef(model)))
  for(i in 1:steps){
    indi <- sample(1: nrow(daten), replace = T)
    bfm <- glm(formula(model), data = daten[indi,], family=binomial) 
    bot_mat[i,] <- coef(bfm)
  }
  return(bot_mat)
  }


#---------scaling funktion------------
zScore <- function(x, retrans = F, daten = NA) {
  z <- c(1:length(x))
  for (i in 1:length(x)){
    if (retrans ==T) {
      z[i] <- x[i]* sd(daten) + mean(daten)
    }else{
      z[i] <- (x[i] - mean(x))/sd(x)
    }
  }
  return(z) 
}

