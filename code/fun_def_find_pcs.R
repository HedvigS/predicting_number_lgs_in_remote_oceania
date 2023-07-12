
options("print.matrix" = FALSE)

find_pcs <- function(data){

  data_pca <- prcomp(data, scale. = TRUE)
  
  data_pca_df <-   data_pca$x  %>%  
    as.matrix() %>% 
    as.data.frame() %>% 
    rownames_to_column("ID")
  
  #testing to evaluate the optimal number of components
  ev <- eigen(cor(data)) # get eigenEstimates
  ap <- nFactors::parallel(
    subject=nrow(data),
    var=ncol(data),
    rep=100,
    cent=0.05)
  nS <- nFactors::nScree(x=ev$values, aparallel=ap$eigen$qevpea)
  
  optimal_components <- nS$Components$nparallel
  
  plotnScree(nS)
  
  prop_explained <- sum(summary(data_pca)$importance[2, 1:optimal_components])
  
  nScree_summary_string <- paste(
    "The optimal number of components is", optimal_components, "and they explain", prop_explained * 100, "% of the variation.\n"
  )
  
  cat(nScree_summary_string)
  
  x <- list(data_pca_df= data_pca_df, 
            optimal_components = optimal_components, 
            nScree_summary_string = nScree_summary_string)
  x
}