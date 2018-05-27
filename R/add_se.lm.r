add_se.lm <- function(model,name_f,name_x="Intercept",type="response"){
  linkinv <- identity
  tt <- type
  #grab the standard error of the coefficients
  se_vec <- summary(model)$coefficients[,2]
  #keep the baseline name for later use

  #row_names <- paste0(name_f,levels(model$model[,name_f]))

  if(name_x=="Intercept"){
    #the stabdard error of the intercept
    se_x <- se_vec[1]
    #get the level-specific standard errors
    se_f <- se_vec[grep(name_f,names(se_vec))]
    se_f <- se_f[grep("^((?!:).)*$",names(se_f),perl=TRUE)]
    #get the covariance between the intercept and the level-specific parameters
    vcov_f <- vcov(model)[grep(name_f,rownames(vcov(model))),grep(name_x,colnames(vcov(model)))]
    vcov_f <- vcov_f[grep("^((?!:).)*$",names(vcov_f),perl=TRUE)]
    #the estimated average value at each level
    coef_f <- coef(model)[1]+coef(model)[names(vcov_f)]
  }
  else if(is.factor(model$model[,name_x])){
    #grab the standard error of the first factor
    se_x <- se_vec[grep(paste0("^",name_x,"[^:]*$"),names(se_vec),perl=TRUE)]
    #get the standard error for the interaction terms
    se_f <- se_vec[grep(paste0("^",name_x,"\\w+:",name_f,"\\w+$"),names(se_vec),perl=TRUE)]
    #grab the covariance terms
    lvl_x <- grep(paste0("^",name_x,"[^:]*$"),names(se_vec),perl=TRUE,value = TRUE)
    lvl_f <- grep(paste0("^",name_x,"\\w+:",name_f,"\\w+$"),names(se_vec),perl=TRUE,value = TRUE)
    vcov_f <- NULL #the container for the covariance values
    for(ff in lvl_f){ #loop through the levels of f
      x <- grep(strsplit(ff,":")[[1]][1],lvl_x,value=TRUE) #the row index
      vcov_f <- c(vcov_f,vcov(model)[x,ff]) #only keep relevant interactions
      names(vcov_f)[length(vcov_f)] <- ff #adding names just for safety checks
    }
    coef_f <- coef(model)[grep(paste0("^",name_x,"[^:]*$"),names(se_vec),perl=TRUE)] +
      coef(model)[grep(paste0("^",name_x,"\\w+:",name_f,"\\w+$"),names(se_vec),perl=TRUE)]
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
    coef_f <- c(coef(model)[name_coef[1]],coef(model)[name_coef[1]] + coef(model)[name_coef[-1]])

  }
  out <- add_se_xxx(coef_f, se_x, se_f, vcov_f, linkinv, type=tt)


  return(out)
}
