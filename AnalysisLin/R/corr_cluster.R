#' @title Correlation Cluster
#' @description corr_cluster performs hierarchical clustering based on the corrleation distance, and generates a dendrogram.
#' @param data input dataset
#' @param label label or name for the dataset, optional.
#' @param type pearson or spearman correlation, default is pearson
#' @param method the agglomeration method to be used. This should be (an unambiguous abbreviation of) one of "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC). Dafault is complete
#'
#' @return A plot of dendrogram
#' @export
#'
#' @examples corr_cluster(data(mtcars),type='pearson',method='complete')
corr_cluster<- function(data, label = NULL,type='pearson',method = 'complete') {
  if (!require(Hmisc)) {
    install.packages('Hmisc')
    library(Hmisc)
  }
  corr <- rcorr(as.matrix(data),type = type)$r
  dist_matrix <- as.dist(1 - abs(corr))
  hc <- hclust(dist_matrix, method = method)

  plot(hc, labels = colnames(data), hang = -1, main = paste("Feature Distance in", label, "dataset"))
}

