#' @title Missing Value Imputation
#'
#' @param data
#' @param method
#' @param k
#'
#' @return
#' @export
#'
#' @examples
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

  return(imputed_data)
}


# Custom LOCF (Last Observation Carried Forward) imputation
impute_locf <- function(x) {
  require(zoo)
  imputed_values <- na.locf(x, na.rm = FALSE)
  return(imputed_values)
}

# Custom mode function
impute_mode <- function(x) {
  tbl <- table(x)
  modes <- tbl[tbl == max(tbl)]
  return(as.numeric(names(modes)))
}

# Custom kNN imputation
impute_knn <- function(data, k) {
  require(impute)
  imputed_values <- impute.knn(as.matrix(data), k = k)$data
  return(imputed_values)
}

