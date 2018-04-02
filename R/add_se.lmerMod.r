add_se.lmerMod <- function(model,name_f,name_x="Intercept",type="response"){
  linkinv <- identity
  tt <- type
  #grab the standard error of the coefficients
  se_vec <- sqrt(diag(as.matrix(vcov(model))))
  names(se_vec) <- names(fixef(model))
  #keep the baseline name for later use
  base_name <- paste0(name_f,levels(model@frame[,name_f])[1])
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
  }
  else if(is.factor(model@frame[,name_x])){
    #grab the standard error of the first factor
    se_x <- se_vec[grep(paste0("^",name_x,"[^:]*$"),names(se_vec),perl=TRUE)]
    #get the standard error for the interaction terms
    se_f <- se_vec[grep(paste0("^ (",name_x,"\\w+ :",name_f, "\\w+) | (",name_f,"\\w+:",name_x, "\\w+) $")
                        ,names(se_vec),perl=TRUE)]
    #grab the covariance terms
    lvl_x <- grep(paste0("^",name_x,"[^:]*$"),names(se_vec),perl=TRUE,value = TRUE)
    lvl_f <- grep(paste0("^",name_x,"\\w+:",name_f,"\\w+$"),names(se_vec),perl=TRUE,value = TRUE)
    vcov_f <- NULL #the container for the covariance values
    for(ff in lvl_f){ #loop through the levels of f
      x <- grep(strsplit(ff,":")[[1]][1],lvl_x,value=TRUE) #the row index
      vcov_f <- c(vcov_f,vcov(model)[x,ff]) #only keep relevant interactions
      names(vcov_f)[length(vcov_f)] <- ff #adding names just for safety checks
    }
    coef_f <- c(fixef(model)[grep(paste0("^",name_x,"[^:]*$"),names(se_vec),perl=TRUE)],
                fixef(model)[grep(paste0("^",name_x,"[^:]*$"),names(se_vec),perl=TRUE)] +
                  fixef(model)[grep(paste0("^",name_x,"\\w+:",name_f,"\\w+$"),names(se_vec),perl=TRUE)])
  }
  else{
    #similar code for the case of another variable than the intercept
    se_x <- se_vec[name_x]
    se_f <- se_vec[grep(name_f,names(se_vec))]
    se_f <- se_f[grep(":",names(se_f))]
    vcov_f <- vcov(model)[grep(name_f,rownames(vcov(model))),grep(name_x,colnames(vcov(model)))][,1]
    vcov_f <- vcov_f[grep(":",names(vcov_f))]
    coef_f <- c(fixef(model)[name_x], fixef(model)[name_x]+fixef(model)[names(vcov_f)])
  }
  out <- add_se_xxx(coef_f, se_x, se_f, vcov_f, linkinv, base_name, type = tt)
  return(out)
}
