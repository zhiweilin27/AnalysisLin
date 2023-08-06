#' @title Descriptive Statistics
#' @description desc_stat() function calculates various key descriptive statistics for each
#' variables in the provided data set. The function computes the count, number of unique values,
#' duplicate count, number of missing values, null rate, data type, minimum value, 25th percentile,
#' mean, median, 75th percentile, maximum value, standard deviation, kurtosis, skewness, and jarque_pvalue for each variable.
#' @param data input dataset
#' @param count An logical argument(default TRUE) that determines if count is included in the output
#' @param unique An logical argument(default TRUE) that determines if unique is included in the output
#' @param duplicate An logical argument(default TRUE) that determines if duplicate is included in the output
#' @param null An logical argument(default TRUE) that determines if null is included in the output
#' @param null_rate An logical argument(default TRUE) that determines if null_rate is included in the output
#' @param type An logical argument(default TRUE) that determines if type is included in the output
#' @param min An logical argument(default TRUE) that determines if min is included in the output
#' @param p25 An logical argument(default TRUE) that determines if p25 is included in the output
#' @param mean An logical argument(default TRUE) that determines if mean is included in the output
#' @param median An logical argument(default TRUE) that determines if median is included in the output
#' @param p75 An logical argument(default TRUE) that determines if p75 is included in the output
#' @param max An logical argument(default TRUE) that determines if max is included in the output
#' @param sd An logical argument(default TRUE) that determines if sd is included in the output
#' @param kurtosis An logical argument(default TRUE) that determines if kurtosis is included in the output
#' @param skewness An logical argument(default TRUE) that determines if skewness is included in the output
#' @param jarque_test An logical argument(default TRUE) that determines if jarque_pvalue is included in the output

#' @return A data frame which summarizes the characteristics of a data set
#' @export
#'
#' @examples desc_stat(data(mtcars))
desc_stat <- function(data, count = TRUE, unique = TRUE, duplicate = TRUE, null = TRUE,
                      null_rate = TRUE, type = TRUE, min = TRUE, p25 = TRUE, mean = TRUE,
                      median = TRUE, p75 = TRUE, max = TRUE, sd = TRUE, kurtosis = TRUE,
                      skewness = TRUE, jarque_test = TRUE) {
  
  if (length(data) == 0) {
    stop("Input data is empty.")
  }
  
  desc <- data.frame(matrix(NA,nrow=ncol(data)))
  rownames(desc) <- names(data)
  if (count) {
    desc$count <- sapply(data, function(x) sum(!is.na(x)))
  }
  if (unique) {
    desc$unique <- sapply(data, function(x) length(unique(x)))
  }
  if (duplicate) {
    desc$duplicate <- sum(duplicated(data))
  }
  if (null) {
    desc$null <- sapply(data, function(x) sum(is.na(x)))
  }
  if (null_rate) {
    desc$null_rate <- sapply(data, function(x) sum(is.na(x)) / length(x))
  }
  if (type) {
    desc$type <- sapply(data, function(x) class(x))
  }
  if (min) {
    desc$min <- sapply(data, function(x) ifelse(is.numeric(x), min(x,na.rm=T), NA))
  }
  if (p25) {
    desc$p25 <- sapply(data, function(x) ifelse(is.numeric(x), quantile(x, 0.25,na.rm=T), NA))
  }
  if (mean) {
    desc$mean <- sapply(data, function(x) ifelse(is.numeric(x), mean(x,na.rm=T), NA))
  }
  if (median) {
    desc$median <- sapply(data, function(x) ifelse(is.numeric(x), median(x,na.rm=T), NA))
  }
  if (p75) {
    desc$p75 <- sapply(data, function(x) ifelse(is.numeric(x), quantile(x, 0.75,na.rm=T), NA))
  }
  if (max) {
    desc$max <- sapply(data, function(x) ifelse(is.numeric(x), max(x,na.rm=T), NA))
  }
  if (sd) {
    desc$sd <- sapply(data, function(x) ifelse(is.numeric(x), sd(x), NA))
  }
  if (kurtosis) {
    desc$kurtosis <- sapply(data, function(x) ifelse(is.numeric(x), kurtosis(x), NA))
  }
  if (skewness) {
    desc$skewness <- sapply(data, function(x) ifelse(is.numeric(x), skewness(x), NA))
  }
  if (jarque_test) {
    is_numeric <- sapply(data, is.numeric)
    desc$jarque_pvalue <- ifelse(is_numeric, sapply(data[is_numeric], function(x) jarque_test(x)), NA)
  }
  desc <- desc[,-1]
  cat("Descriptive Statistics Results:\n")
  cat("=================================\n")
  return(desc)
}



skewness <- function(x) {
  n <- length(x)
  mean_val <- mean(x,na.rm=T)
  std_dev <- sqrt(sum((x - mean_val)^2) / (n))
  Z <- (x-mean_val)/std_dev
  
  skewness <- sum(Z^3)/n
  return(skewness)
}



kurtosis <- function(x) {
  n <- length(x)
  mean_val <- mean(x,na.rm=T)
  std_dev <- sqrt(sum((x - mean_val)^2) / (n))
  Z <- (x-mean_val)/std_dev
  kurtosis <- sum(Z^4)/n
  return(kurtosis)
}

jarque_test <- function(x) {
  n <- length(x)
  skew <- skewness(x)
  kurt <- kurtosis(x)
  
  jarque_pvalue <- 1 - pchisq(n/6 * (skew^2 + (kurt - 3)^2 / 4), df = 2)
  return(jarque_pvalue)
}


