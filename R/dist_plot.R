#' @title Numerical Variables Distribution
#'
#' @param data input data
#' @param hist a logical argument(default TRUE) that determines if histogram is generated
#' @param prob a logical argument(default FALSE) that determines it is a probability histogram or relative frequency histogram.
#' @param dens a logical argument(default FALSE) that determine if density line is generated
#' @return a list of plots
#' @export
#'
#' @examples
numeric_plot <- function(data,hist=TRUE,prob=FALSE,dens=FALSE) {
  numerical <- names(Filter(is.numeric, data))
  if (length(numerical) == 0) stop("There is no numerical variable in the dataset")
  plots <- list() # Initialize a list to store the plots

  for (i in 1:length(numerical)) {
    # Create the histogram plot
    if (hist){
      hist(data[[numerical[i]]], prob=prob, main = paste(numerical[i], "Distribution"), xlab = NULL, ylab = "Frequency")
    }
    # Add the density plot
    if(dens){
      lines(density(data[[numerical[i]]]), col = "red", lwd = 2)
    }
    # Store the plot in the list
    plots[[i]] <- recordPlot()
  }

  return(plots)
}

#' @title Categorical Variables Plots
#'
#' @param data input data
#' @param pie a logical argument(default TRUE) that determines if pie plot is generated
#' @param pie_legend a logical argument(default TRUE) that determines if a legend of pie is generated
#' @param pie_legend_size an argument(default is 0.5) that determines size of the legend
#' @param pie_legend_position an argument(dafult is bottom) that determines the position of the legend
#' @param bar a logical argument(default TRUE) that determines if bar plot is generated
#' @param bar_legend a logical argument(default TRUE) that determines if a legend of bar is generated
#' @param bar_legend_size an argument(default is 0.5) that determines size of the legend
#' @param bar_legend_position an argument(dafult is bottom) that determines the position of the legend
#' @param n_col an argument that determine how many plot being put on the same rows
#' @return a list of pie charts
#' @export
#'
#' @examples
categoric_plot <- function(data, pie=TRUE, pie_legend = TRUE, pie_legend_size = 0.5,pie_legend_position='bottom',pie_inset = c(0, -0.15),
                           bar=TRUE, bar_legend=TRUE, bar_legend_size=0.5,bar_legend_positon = 'bottom',bar_inset=c(0, -0.4),
                           n_col=1) {
  categorical <- names(Filter(function(x) is.factor(x) || is.character(x), data))
  if (length(categorical) == 0) stop("There is no categorical variable in the dataset")

  for (i in 1:length(categorical)) {
    table_data <- table(data[[categorical[i]]]) # Calculate the frequencies

    # Set up the multi-panel layout
    par(mfrow = c(1, n_col))

    # Create the pie chart
    if (pie){
      pie_data <- table_data / sum(table_data)
      colors <- rainbow(length(table_data))
      pie(pie_data, main = paste(categorical[i], "Pie"), col = colors)

      # Add percentage labels to the pie chart
      labels <- paste0(names(table_data), " (", round(100 * pie_data, 1), "%)")
      if (pie_legend) {
        legend(pie_legend_position, legend = labels, cex = pie_legend_size, horiz=T, fill = colors, xpd = TRUE, inset = pie_inset)
      }
    }
    if (bar){
      bar_colors <- rainbow(length(table_data))
      barplot(table_data, main = paste(categorical[i], "Bar Plot"), xlab = "Frequency", ylab = categorical[i], horiz = TRUE, col = bar_colors)

      # Add legend to the bar plot
      if (bar_legend) {
        legend(bar_legend_positon, legend = names(table_data), horiz=T,fill = bar_colors, xpd = TRUE, inset = bar_inset)
      }
    }
  }
}







