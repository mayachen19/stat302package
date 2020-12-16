#' T-Test function
#'
#' This function calculates t test statistic, degrees of freedom, and
#'   p value given vector of numerics and character string of test condition.
#'
#' @param x Vector of numerics given to \code{my_t_test}.
#' @param alternative Character string of test condition i.e. "two sided,"
#'   "less," or "greater."
#' @param mu numeric input of mu value for t test.
#'
#' @keywords inference
#'
#' @return List of test statistic, df, input character string, and p value.
#'
#' @examples
#' sample_data <- rnorm(1000, 12, 5)
#' my_t_test(sample_data, "two.sided", 10)
#' my_t_test(sample_data, "less", 10)
#'
#' @export

#Function: T-test function
#Input: vector of numerical objects, character string, mu value
#Output: List of test statistic, df, input character string, p value

#defining my_t.test function
my_t_test <- function(x, alternative, mu) {
  #sets initial character string condition
  if (alternative == "two.sided") {
    #calculates t statistic
    test_stat <- (mean(x) - mu) / (sd(x) / sqrt(length(x)))
    #calculates degrees of freedom
    df <- length(x) - 1
    #calculates p value
    p_value <- 2 * pt(abs(test_stat), df, lower.tail = FALSE)
    #returns list of t statistic, p value, df and input string
    return(list(
      "test_stat" = test_stat,
      "df" = df,
      "alternative" = alternative,
      "p_val" = p_value
    ))
  }
  #sets alternative character string condition
  else if (alternative == "less") {
    #calculates t statistic
    test_stat <- (mean(x) - mu) / (sd(x) / sqrt(length(x)))
    #calculates degrees of freedom
    df <- length(x) - 1
    #calculates p value
    p_value <- pt(test_stat, df)
    #returns list of t statistic, p value, df and input string
    return(list(
      "test_stat" = test_stat,
      "df" = df,
      "alternative" = alternative,
      "p_val" = p_value
    ))
  }
  #sets final alternative character string condition
  else if (alternative == "greater") {
    #calculates t statistic
    test_stat <- (mean(x) - mu) / (sd(x) / sqrt(length(x)))
    #calculates degrees of freedom
    df <- length(x) - 1
    #calculates p value
    p_value <- pt(test_stat, df, lower.tail = FALSE)
    #returns list of t statistic, df, input string, and p value
    return(list(
      "test_stat" = test_stat,
      "df" = df,
      "alternative" = alternative,
      "p_val" = p_value
    ))
    #informative stop message
  } else {
    stop("Your alternative parameter is not a character string of one of
    the following: less, greater, or two.sided")
  }
}
