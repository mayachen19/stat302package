#' Linear Regression Function
#'
#' This function uses linear regression to return the beta coefficient and
#'   estimates, standard error, t value, and p value.
#'
#' @param data input data frame to be fitted for a model by \code{my_lm}.
#' @param formula input formula object to be used for regression model.
#'
#' @keywords Prediction, Inference
#'
#' @return data frame of beta coefficient estimates, standard error, t value,
#'   and p-value.
#'
#' @examples
#' my_lm(my_penguins, bill_length_mm ~ bill_depth_mm + flipper_length_mm)
#' my_lm(my_penguins, flipper_length_mm ~ bill_length_mm + bill_depth_mm)
#'
#' @export

#Function: linear regression function
#Input: dataframe, formula object
#Output: Summarized data frame

#Define function my_lm
my_lm <- function(data, formula) {
  #creates model frame of input data and formula
  model_frame <- model.frame(formula, data)
  #extracts x values into model matrix
  x <- model.matrix(formula, data)
  #extracts y values using model.response
  y <- model.response(model_frame)
  #calculates betas
  beta <- solve(t(x) %*% x) %*% t(x) %*% y
  #calculates degrees of freedom
  degrees <- length(y) - ncol(x)
  #calculates sigma squared
  sigma_squared <- sum(((y - (x %*% beta)) ^ 2) / degrees)
  #calculates standard error
  se <- diag(sqrt((sigma_squared) * solve(t(x) %*% x)))
  #calculates t value
  t_value <- (beta) / se
  #calculates two sided t test p value
  p_value <- 2 * pt(abs(t_value), degrees, lower.tail = FALSE)
  #column binds beta, se , t value, and p value into an output matrix
  output_matrix <- cbind(beta, se, t_value, p_value)
  #names columns of output matrix
  colnames(output_matrix) <- c("Estimate", "Std. error", "t-value", "Pr(>|t|)")
  #returns output matrix as data frame
  return(as.data.frame(output_matrix))
}

