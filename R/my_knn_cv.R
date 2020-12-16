#' KNN K-fold CrossValidation Algorithm
#'
#' This function returns knn average cv error and predictions.
#'
#' @param train Training data set to be given to \code{my_knn_cv}.
#' @param cl Class of object to be predicted by \code{my_knn_cv}.
#' @param k_nn Number of k nearest numbers to be used by \code{knn}.
#' @param k_cv Number of folds for k fold cross validation.
#'
#' @keywords Prediction
#'
#' @return List of predicted class vector \code{cl} and average cv error.
#'
#' @examples
#' my_df <- my_penguins
#' my_df <- my_df %>%
#'  dplyr::select(c(species, bill_length_mm, bill_depth_mm,
#'           flipper_length_mm, body_mass_g))
#' my_df$species <- as.character(my_df$species)
#' my_df <- na.omit(my_df)
#' my_df_class <- my_df %>% dplyr::pull(species)
#' my_knn_cv(train = my_df, cl = "species", k_nn = 1, k_cv = 5)
#' my_knn_cv(train = my_df, cl = "species", k_nn = 5, k_cv = 5)
#'
#' @export

##FUNCTION: my_knn_cv - returns list of knn average cv error and predictions
#INPUT: input data frame, class of predictions, # of n. neighbors and folds
#OUTPUT: list of average cv error and full data set predictions

#setting up my_knn_cv function
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  #sets up fold to split by
  fold <- sample(rep(1:k_cv, length = nrow(train)))
  #mutates input df to include new fold variable
  train <- train %>% dplyr::mutate(fold = fold)
  #empty vector for cv_error rate
  cv_error_folds <- vector()
  #for loop for 1 to k_cv
  for (i in 1:k_cv) {
    #training df for knn fuction
    data_train <- train %>%
      dplyr::filter(fold != i) %>%
      dplyr::select(-cl, -fold)
    #testing df for knn function
    data_test <- train %>%
      dplyr::filter(fold == i) %>%
      dplyr::select(-cl, -fold)
    #training class vector for knn function
    cl_train <- train %>%
      dplyr::filter(fold != i) %>%
      dplyr::pull(cl)
    #testing class vector for knn function
    cl_test <- train %>%
      dplyr::filter(fold == i) %>%
      dplyr::pull(cl)
    #stores class predictions for each fold
    class_folds <- class::knn(train = data_train,
                              test = data_test,
                              cl = cl_train,
                              k = k_nn)
    #compares class preds and testing class vector to find cv error rate
    cv_error_folds[i] <- 1 - (sum(class_folds == cl_test) / length(class_folds))
  }
  #assigns class vector from full df
  total_cl <- train %>% dplyr::pull(cl)
  #training df from full df
  total_train <- train %>% dplyr::select(-cl)
  #output class predictions for complete df
  class <- class::knn(train = total_train, test = total_train,
                      cl = total_cl, k = k_nn)
  #averages cv error
  cv_error <- mean(cv_error_folds)
  #list of avg cv_error and knn preds from total df
  output_list <- list(cv_error = cv_error,
                      class = class)
  #returns output list
  return(output_list)
}
