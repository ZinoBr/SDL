create_k_folds_vector <- function(x, k = 10){

  folds <- sample(rep(1:k, length <- length(x) / k))

  if (length(folds) == length(x) ) { return(folds) } else {

          diff = length(x) - length(folds)

          folds = c(folds, sample(1:k, diff, replace = F) )

          return(folds)
        }
    }
