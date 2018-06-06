add_se.merModLmerTest <- function(model,name_f,name_x="Intercept",type="response"){
  linkinv <- identity
  tt <- type
  #grab the standard error of the coefficients
  se_vec <- sqrt(diag(as.matrix(vcov(model))))
  names(se_vec) <- names(fixef(model))
  #keep the baseline name for later use
  lvls <- paste0(name_f,levels(model@frame[,name_f]))
  if(name_x=="Intercept"){
    #the standard error of the intercept
    se_x <- se_vec[1]
    #get the level-specific standard errors
    se_f <- se_vec[grep(name_f,names(se_vec))]
    se_f <- se_f[grep("^((?!:).)*$",names(se_f),perl=TRUE)]
    #get the covariance between the intercept and the level-specific parameters
    vcov_f <- vcov(model)[grep(name_f,rownames(vcov(model))),grep(name_x,colnames(vcov(model)))]
    vcov_f <- vcov_f[grep("^((?!:).)*$",names(vcov_f),perl=TRUE)]
    #the estimated average value at each level
    coef_f <- c(fixef(model)[1], fixef(model)[1]+fixef(model)[names(vcov_f)])
    row_names <- paste("Intercept",lvls,sep="_")
  }
  else{
    #the SE names of interest
    name_coef <- grep(paste0(name_x,"|",name_f,"\\w+:",name_f,"\\w+|name_x"),names(se_vec),perl=TRUE,value=TRUE)
    #grab the relevant standard errors
    se_f <- se_vec[name_coef]
    #remove the SE from the main slope
    se_x <- se_f[1]
    se_f <- se_f[-1]
    #grab the covariance terms
    vcov_f <- vcov(model)[name_coef[1],name_coef[-1]]
    #the slopes at each level
    coef_f <- c(fixef(model)[name_x], fixef(model)[name_x]+fixef(model)[names(vcov_f)])
    row_names <- paste(name_x,lvls,sep=":")
  }
  out <- add_se_xxx(coef_f, se_x, se_f, vcov_f, linkinv, row_names, type = tt)
  return(out)
}
