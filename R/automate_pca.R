#' @title Principle Component Analysis
#'
#' @param data input dataset
#' @param scale a logical argument(default TRUE) that determines if appling standardized scaling to the dataset
#' @param variance_threshold an argument(default is 0.9) for set a variance_threshold
#' @param scree_plot a logical argument(default TRUE) that determines if scree plot is generated
#'
#' @return rotation and score of PCA and scree plot(optional)
#' @export
#'
#' @examples automate_pca(data(mtcars))
automate_pca <- function(data, scale = TRUE, variance_threshold = 0.9, scree_plot = TRUE) {
  pca <- prcomp(data, scale = scale)
  pca_results <- list()
  pca_results$rotation <- pca$rotation
  pca_results$scores <- pca$x
  cumulative_variance <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
  num_components <- min(which(cumulative_variance >= variance_threshold))
  pca_results$rotation <- pca_results$rotation[, 1:num_components]
  pca_results$scores <- pca_results$scores[, 1:num_components]
  
  if (scree_plot) {
    plot(1:length(pca$sdev), cumulative_variance, type = "b",
         xlab = "Principal Component", ylab = "Proportion of Variance Explained",
         main = "Scree Plot")
    abline(h = variance_threshold, col = "red")
    text(1:length(pca$sdev), cumulative_variance,
         labels = paste0(round(cumulative_variance * 100, 2), "%"), pos = 4)
    legend("bottomright", legend = c("Variance Threshold", "Cumulative Variance Explained"),
           col = c("red", "black"), lty = 1, cex = 0.8)
  }
  cat(paste(
    "PCA Analysis Results:",
    "\n==========================",
    "\nTotal number of variables:", ncol(data),
    "\nNumber of principal components retained:", num_components,
    "\nTotal cumulative variance explained:", round(cumulative_variance[num_components] * 100, 2), "%",
    "\n",
    "\n"
  ))
  result <- list(rotation_matrix = pca_results$rotation, scores_matrix= pca_results$scores)
  return(result)
}










