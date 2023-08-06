#' @title Missing Value Imputation
#'
#' @param data input data
#' @param method method of handling missing values: mean, median, mode, locf,knn.
#' @param k argument used in knn method. k is value of number of neighbors will be checked. Default is Null
#'
#' @export
#'
#' @examples impute_missing(data(mtcars), method='mean')
impute_missing <- function(data, method = "mean", k = NULL) {
  imputed_data <- data
  if (is.null(k)) {
    for (col in names(imputed_data)) {
      if (method == "mean") {
        imputed_data[[col]] <- ifelse(is.na(imputed_data[[col]]), mean(imputed_data[[col]], na.rm = TRUE), imputed_data[[col]])
      } else if (method == "median") {
        imputed_data[[col]] <- ifelse(is.na(imputed_data[[col]]), median(imputed_data[[col]], na.rm = TRUE), imputed_data[[col]])
      } else if (method == "mode") {
        imputed_data[[col]] <- ifelse(is.na(imputed_data[[col]]), impute_mode(imputed_data[[col]]), imputed_data[[col]])
      } else if (method == "locf") {
        imputed_data[[col]] <- impute_locf(imputed_data[[col]])
      }
    }
  } else if (method == "knn" && !is.null(k)) {
    imputed_data <- impute_knn(imputed_data, k)
  } else {
    stop("Invalid imputation method. Supported methods are: mean, median, mode, locf, knn, regression, multiple.")
  }

  return(as.data.frame(imputed_data))
}


impute_locf <- function(x) {
  if(!require(zoo)){
    install.packages('zoo')
    library(zoo)
  }
  imputed_values <- na.locf(x, na.rm = FALSE)
  return(imputed_values)
}

impute_mode <- function(x) {
  tbl <- table(x)
  modes <- tbl[tbl == max(tbl)]
  return(as.numeric(names(modes)))
}

impute_knn <- function(data, k) {
  if (!require(caret)) {
    install.packages('caret')
    library(caret)
  }
  if (!require(RANN)) {
    install.packages('RANN')
    library(RANN)
  }
  imputed_values <- preProcess(data, method = 'knnImpute', k = k)
  imputed_data <- predict(imputed_values, data)
  
  procNames <- data.frame(col = names(imputed_values$mean), mean = imputed_values$mean, sd = imputed_values$std)
  for (i in procNames$col) {
    imputed_data[i] <- imputed_data[i] * imputed_values$std[i] + imputed_values$mean[i]
  }
  
  return(imputed_data)
}


