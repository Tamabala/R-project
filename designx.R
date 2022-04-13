library(R.matlab)
library(Matrix)
library(reshape2)
library(glmnet)

for (j in 1){
  intpath <- r'(D:\MATLAB\a_data\octa_glm\data\)'
  outpath <- r'(D:\MATLAB\a_data\octa_glm\csvdata\)'
  
  idlist <- dir(intpath, pattern = 'eeg*')
  idlist <- gsub('eeg_','',idlist)
  idlist <- gsub('.mat','',idlist)
 
  subj <- idlist[j]
  dat <- readMat(paste(intpath,'eeg_',subj,'.mat',sep = ""))
  eeg <- dat[[1]]
  dat <- readMat(paste(intpath,'stim','.mat',sep = ""))
  stm <- dat[[1]]
  dat <- readMat(paste(intpath,'evt_',subj,'.mat',sep = ""))
  evt <- dat[[1]]
  
  srate <- 500
  ntrl <- nrow(stm)
  nvar <- ncol(stm)
  npnt <- ncol(eeg)
  timelimits <- c(-.2,.8)
  reallims <- c(0,0)
  reallims[1] <- round(timelimits[1]*srate)
  reallims[2] <- round(timelimits[2]*srate-1)
  windowlength <- diff(reallims)+1
  windowtimes <- seq(reallims[1],reallims[2],length.out = windowlength)/srate
  
  eventvec <- matrix(0,nvar,npnt)
  
  for (e in 1:ntrl){
    eventvec[,round(evt[1])] = stm[e,]
  }
  
  n_entries <- rep(ntrl,nvar)
  n_cols <- sum(n_entries) * windowlength
  
  indcol_all <- matrix(NaN,1,n_cols)
  indrow_all <- matrix(NaN,1,n_cols)
  val_all <- matrix(NaN,1,n_cols)
  
  shiftvec <- (1:windowlength) + timelimits[1]*srate -1
  
  ixlist <- cumsum(n_entries)*windowlength
  
  
  for (i in 1:nvar){
    if (i == 1){
      startix <- 1
    }else{
      startix <- ixlist[i-1]+1
    }
    endix <- ixlist[i]
    
    tmp <- rep(1:windowlength,each = n_entries[1])
    indcol_all[startix:endix] <- tmp
    if (i > 1){
      indcol_all[startix:endix]  <- indcol_all[startix:endix]  + indcol_all[startix-1]
    }
    
    tmp <- rep(evt,windowlength)
    tmp <- matrix(tmp,windowlength,ntrl,byrow = 1)
    tmp <- t((tmp+shiftvec))
    tmp <- melt(tmp)[[3]]
    
    indrow_all[startix:endix] <- tmp
    
    tmp <- rep(stm[,i],each = windowlength)
    tmp <- matrix(tmp,ntrl,windowlength,byrow = 1)
    tmp <- melt(tmp)[[3]]
    val_all[startix:endix] <- tmp
  }
  Xdc <- sparseMatrix(i = indrow_all[1,],j = indcol_all[1,], x = val_all[1,],dims = c(npnt,windowlength*nvar))
  
  peakRows <- rowSums(abs(Xdc)) != 0
  X <- Xdc[peakRows,]
  data <- eeg[,peakRows]
  
  channel <- 1:nrow(data)
  beta <- matrix(NaN,ncol(X)+1,nrow(data))
  for (i in channel){
    idata = scale(data[i,],center = T, scale = T);
    cvfit <- cv.glmnet(X,idata,alpha = 0,family="gaussian",standardize = T)
    opt_coef <- coef(cvfit,s = "lambda.min")
    beta[,i] <- as.matrix(opt_coef)
  }
  obeta <- beta[2:(ncol(X)+1),]
  
  write.csv(obeta,file = paste(outpath,subj,'.csv',sep = ""),row.names = F)
  
  # rm(list=ls())
  # gc()
}
  
# rstudioapi::restartSession()
# plotchan <- 15
# Verp <-  matrix(obeta[,plotchan],nvar,windowlength,byrow = 1)

# a <- erp[26,]
# plot(a)
best_lambda <- cvfit$lambda.min
best_fit <- cvfit$glmnet.fit
head(best_fit)

model <- glmnet(X, idata, alpha = 0)
check <- as.matrix(coef(best_ridge, s = best_lambda))

y_pred = predict(model, s=best_lambda, newx = X)




a = as.matrix(check)

b = as.matrix(coef(best_fit, s = best_lambda))

c <- as.matrix(coef(best_fit, s = best_fit$lambda[60]))

