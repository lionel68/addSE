add_se.glmmTMB <- function(model,name_f,name_x="Intercept",type="response"){
  linkinv <- model$modelInfo$family$linkinv
  tt <- type
  #grab the standard error of the coefficients
  se_vec <- sqrt(diag(vcov(model)$cond))
  names(se_vec) <- names(fixef(model)$cond)
  #keep the baseline name for later use
  base_name <- paste0(name_f,levels(model$frame[,name_f])[1])
  if(name_x=="Intercept"){
    #the standard error of the intercept
    se_x <- se_vec[1]
    #get the level-specific standard errors
    se_f <- se_vec[grep(name_f,names(se_vec))]
    se_f <- se_f[grep("^((?!:).)*$",names(se_f),perl=TRUE)]
    #get the covariance between the intercept and the level-specific parameters
    vcov_f <- vcov(model)$cond[grep(name_f,rownames(vcov(model)$cond)),grep(name_x,colnames(vcov(model)$cond))]
    vcov_f <- vcov_f[grep("^((?!:).)*$",names(vcov_f),perl=TRUE)]
    #the estimated average value at each level
    coef_f <- c(fixef(model)$cond[1], fixef(model)$cond[1]+fixef(model)$cond[names(vcov_f)])
  }
  else{
    #similar code for the case of another variable than the intercept
    se_x <- se_vec[name_x]
    se_f <- se_vec[grep(name_f,names(se_vec))]
    se_f <- se_f[grep(":",names(se_f))]
    vcov_f <- vcov(model)$cond[grep(name_f,rownames(vcov(model)$cond)),grep(name_x,colnames(vcov(model)$cond))][,1]
    vcov_f <- vcov_f[grep(":",names(vcov_f))]
    coef_f <- c(fixef(model)$cond[name_x], fixef(model)$cond[name_x]+fixef(model)$cond[names(vcov_f)])
  }
  out <- add_se_xxx(coef_f, se_x, se_f, vcov_f, linkinv, base_name,type = tt)
  return(out)
}
