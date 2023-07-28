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
automate_pca<- function(data, scale=TRUE, variance_threshold = 0.9, scree_plot = TRUE) {
  # Perform PCA
  pca <- prcomp(data, scale = scale)

  # Extract the results
  pca_results <- list()
  pca_results$rotation <- pca$rotation
  pca_results$scores <- pca$x

  # Calculate cumulative variance
  cumulative_variance <- cumsum(pca$sdev^2 / sum(pca$sdev^2))


  # Find the index where the cumulative variance crosses the threshold
  num_components <- min(which(cumulative_variance >= variance_threshold))

  # Extract only the relevant components
  pca_results$rotation <- pca_results$rotation[, 1:num_components]
  pca_results$scores <- pca_results$scores[, 1:num_components]
  if(scree_plot){
    # Create the scree plot
    plot(1:length(pca$sdev),cumulative_variance, type = "b",
         xlab = "Principal Component", ylab = "Proportion of Variance Explained",
         main = "Scree Plot")

    # Add a horizontal line at the variance threshold
    abline(h = variance_threshold, col = "red")

    # Add text indicating the proportion of variance explained at each point
    text(1:length(pca$sdev), cumulative_variance,
         labels = paste0(round(cumulative_variance*100, 2),"%"),pos=4)
  }
  return(pca_results)
}


automate_pca(mtcars[,2:5])

