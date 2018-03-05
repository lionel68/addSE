#hidden function
add_se_xxx <- function(coef_f, se_x, se_f, vcov_f, linkinv, base_name){
  #compute the summed SE
  se_f <- c(se_x,sqrt(se_x**2+se_f**2+2*vcov_f))
  #create the output dataframe
  out <- data.frame(Coef_link = coef_f, SE = se_f, Coef_resp = linkinv(coef_f), LCI = linkinv(coef_f - 1.96 * se_f), UCI = linkinv(coef_f + 1.96 * se_f))
  rownames(out)[1] <- paste(rownames(out)[1],base_name,sep=":")
  return(out)
}
