#' Compute standard error for group-level effects in linear models
#'
#' \code{add_se} compute the coefficients, standard errors and confidence intervals for different levels of factor variables used in linear models.
#'
#' Add_se currently support models fitted via: \code{\link[stats]{lm}}, \code{\link[stats]{lm}}, \code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}}, \code{\link[glmmTMB]{glmmTMB}}
#'
#' Note that name_x must be the name of continuous variable interacting with the factor, factor - factor interaction are not supported.
#'
#' @param model A fitted model object
#' @param name_f A character, the name of the factor variables
#' @param name_x A character, the name of a continuous variable interacting with name_f, default is Intercept
#'
#' @return A data frame where the rows are the different level of the factor, and the columns contain the following values:
#' @return Coef_link : fitted coefficient value on the link scale
#' @return SE : standard error of the fitted coefficient on the link scale
#' @return Coef_resp : fitted coefficient on the response scale (applying the inverse of the link function)
#' @return LCI, UCI : lower and upper bound of the 95\% confidence interval of the fitted coefficient on the response scale
#'
#' @examples data(iris)
#' @examples m <- lm(Sepal.Length ~ Sepal.Width * Species, data = iris)
#' @examples #get the regression lines with SE and confidence intervals for
#' @examples #the different iris species
#' @examples add_se(m, name_f = "Species", name_x = "Sepal.Width")
#'
#' @export

add_se <- function(model,name_f,name_x="Intercept"){
  if(class(name_f) != "character" | class(name_x) != "character"){
    print("Uncorrect arguments passed to function!")
  }
  else{
    UseMethod("add_se")
  }
}
