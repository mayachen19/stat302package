#' Random Forest algorithm
#'
#' This function returns the average mean squared error for k folds of
#'   crossvalidation for the randomforest algorithm.
#'
#' @param k The number of folds for k fold cross validation for \code{my_rf_cv}.
#'
#' @keywords Prediction
#'
#' @return Numeric of average mean squared error from \code{my_rf_cv}
#'   predictions.
#'
#' @examples
#' my_rf_cv(2)
#' my_rf_cv(10)
#'
#' @export

##FUNCTION: my_rf_cv - random forest algorithim for palmerspenguins data
##INPUT: k, number of folds for cross validation
##OUTPUT: average CV MSE across all folds

#defines my_rf_cv
my_rf_cv <- function(k) {
  #gets rid of nas with penguins data set
  penguins_df <- na.omit(my_penguins)
  #generates fold number
  fold <- sample(rep(1:k, length = nrow(penguins_df)))
  #adds fold and selects columns for penguins_df
  penguins_df <- penguins_df %>%
    dplyr::mutate(fold = fold) %>%
    dplyr::select(body_mass_g, bill_length_mm,
                  bill_depth_mm, flipper_length_mm, fold)
  #initalizes mse vector
  my_mse <- vector()
  #for loop for 1 through k
  for (i in 1:k) {
    #assigns training data
    training_df <- penguins_df %>%
      dplyr::filter(fold != i) %>%
      dplyr::select(-fold)
    #assigns testing data
    testing_df <- penguins_df %>%
      dplyr::filter(fold == i) %>%
      dplyr::select(-fold)
    #sets true values for ith fold body mass
    true_pred_values <- penguins_df %>%
      dplyr::filter(fold == i) %>%
      dplyr::pull(body_mass_g)
    #assigns formula object for randomForest
    my_formula <- formula(body_mass_g ~ bill_length_mm + bill_depth_mm +
                            flipper_length_mm)
    #creates randomForest prediction algorithm
    model <- randomForest::randomForest(my_formula,
                                        data = training_df, ntree = 100)
    #stores predictions of trained model
    preds <- predict(model, testing_df[, -1])
    #stores mse value for that fold in ith index of mse vector
    my_mse[i] <- mean((preds - true_pred_values) ^ 2)
  }
  #takes average of mse for each fold
  mse_avg <- mean(my_mse)
  #returns average MSE
  return(mse_avg)
}
