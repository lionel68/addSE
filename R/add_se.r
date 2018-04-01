#' Adding standard errors in regression
#'
#' \code{add_se} compute the coefficients, standard errors and confidence intervals for different levels of factor variables used in linear models.
#'
#' Add_se currently support models fitted via: \code{\link[stats]{lm}}, \code{\link[stats]{glm}}, \code{\link[MASS]{glm.nb}}, \code{\link[nlme]{lme}}, \code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}}, \code{\link[lmerTest]{lmer}} (lmer from lmerTest), \code{\link[glmmTMB]{glmmTMB}}
#'
#' Name_x must be the name of continuous variable interacting with the factor, factor - factor interaction are not supported.
#'
#' Default treatment contrasts are expected in the model, see \code{\link[stats]{contrasts}}, other type of contrasts might work but will lead to non-sensical results.
#'
#' @param model A fitted model object, for the model type currently supported see Details
#' @param name_f A character, the name of the factor variable
#' @param name_x A character, the name of a continuous variable interacting with name_f, default is Intercept
#' @param type whether the coefficient and the confidence interval should be reported on the link (type="link") or response scale (type="response", default), note that SE are only reported on the link scale.
#'
#' @return A data frame where the rows are the different level of the factor, and the columns contain the following values:
#' @return Coef : fitted coefficient value on the link or on the response scale
#' @return SE : standard error of the fitted coefficient on the link scale (only when type="link")
#' @return LCI, UCI : lower and upper bound of the 95\% confidence interval of the fitted coefficient on the appropriate scale (link or response)
#'
#' @examples data(iris)
#' @examples m <- lm(Sepal.Length ~ Sepal.Width * Species, data = iris)
#' @examples #get the regression lines with SE and confidence intervals for
#' @examples #the different iris species
#' @examples add_se(m, name_f = "Species", name_x = "Sepal.Width")
#'
#' @export

add_se <- function(model, name_f, name_x="Intercept", type="response"){
  if(class(name_f) != "character" | class(name_x) != "character"){
    print("Uncorrect arguments passed to function!")
  }
  else{
    UseMethod("add_se")
  }
}
