#hidden function
add_se_xxx <- function(coef_f, se_x, se_f, vcov_f, linkinv, base_name,type){
  #compute the summed SE
  se_f <- c(se_x,sqrt(se_x**2+se_f**2+2*vcov_f))
  #create the output dataframe
  if(type == "link"){
    out <- data.frame(Coef = coef_f, SE = se_f, LCI = coef_f - 1.96 * se_f, UCI = coef_f + 1.96 * se_f)
  }
  else if(type == "response"){
    out <- data.frame(Coef = linkinv(coef_f), LCI = linkinv(coef_f - 1.96 * se_f), UCI = linkinv(coef_f + 1.96 * se_f))
  }
  rownames(out)[1:length(se_x)] <- paste(rownames(out)[1:length(se_x)],base_name,sep=":")
  return(out)
}
