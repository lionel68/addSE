#function parameter
#@model: a lm fitted model
#@name_f: a character string with the name of the categorical (factor) variable
#@name_x: a character string with the name fo the interacting variable, by default the intercept
add_se.lm <- function(model,name_f,name_x="Intercept",type="response"){
  linkinv <- identity
  tt <- type
  #grab the standard error of the coefficients
  se_vec <- summary(model)$coefficients[,2]
  #keep the baseline name for later use
  base_name <- paste0(name_f,levels(model$model[,name_f])[1])
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
    coef_f <- c(coef(model)[1], coef(model)[1]+coef(model)[names(vcov_f)])
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
  out <- add_se_xxx(coef_f, se_x, se_f, vcov_f, linkinv, base_name,type=tt)

  return(out)
}
