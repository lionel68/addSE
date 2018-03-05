#function parameter
#@model: a lm fitted model
#@name_f: a character string with the name of the categorical (factor) variable
#@name_x: a character string with the name fo the interacting variable, by default the intercept
add_se.lm <- function(model,name_f,name_x="Intercept"){
  linkinv <- identity
  #grab the standard error of the coefficients
  se_vec <- summary(model)$coefficients[,2]
  if(name_x=="Intercept"){
    #the stabdard error of the intercept
    se_x <- se_vec[1]
    #get the level-specific standard errors
    se_f <- se_vec[grep(name_f,names(se_vec))]
    se_f <- se_f[-grep(":",names(se_f))]
    #get the covariance between the intercept and the level-specific parameters
    vcov_f <- vcov(model)[grep(name_f,rownames(vcov(model))),grep(name_x,colnames(vcov(model)))]
    vcov_f <- vcov_f[-grep(":",names(vcov_f))]
    #the estimated average value at each level
    coef_f <- coef(model)[1]+coef(model)[names(vcov_f)]
  }
  else{
    #similar code for the case of another variable than the intercept
    se_x <- se_vec[name_x]
    se_f <- se_vec[grep(name_f,names(se_vec))]
    se_f <- se_f[grep(":",names(se_f))]
    vcov_f <- vcov(model)[grep(name_f,rownames(vcov(model))),grep(name_x,colnames(vcov(model)))][,1]
    vcov_f <- vcov_f[grep(":",names(vcov_f))]
    coef_f <- coef(model)[name_x]+coef(model)[names(vcov_f)]
  }
  out <- add_se_xxx(coef_f, se_x, se_f, vcov_f, linkinv)
  return(out)
}