## ----setup--------------------------------------------------------------------
library(AnalysisLin)

## ----echo=FALSE---------------------------------------------------------------
library(knitr)

## -----------------------------------------------------------------------------
# Create a data frame
df <- data.frame(
  Descriptive_Statistics = c("desc_stat()",""),
  Data_Visualization = c("numeric_plot()", "categoric_plot()"),
  Correlation_Analysis = c("corr_matrix()", "corr_cluster()"),
  Feature_Engineering = c("missing_impute()", "automate_pca()")
)

# Print the table
kable(df)

