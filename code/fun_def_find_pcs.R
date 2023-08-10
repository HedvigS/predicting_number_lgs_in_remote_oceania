options("print.matrix" = FALSE)

find_pcs <- function(data){

  data_pca <- prcomp(data, scale. = TRUE)
  
  data_pca_df <-   data_pca$x  %>%  
    as.matrix() %>% 
    as.data.frame() %>% 
    rownames_to_column("ID")

PCA_summary_importance <- t(summary(data_pca)$importance) %>%
    as.data.frame() %>%
    rownames_to_column("PC") %>%
    dplyr::select(`Proportion of Variance`, everything())
  
  #Tidying the component contributions into a long format.
tidied_pca <- data_pca$rotation %>%
    as.data.frame() %>% 
    rownames_to_column("Parameter_ID") %>%
    reshape2::melt(id.vars = "Parameter_ID") %>% 
    dplyr::rename(PC = variable, Contribution = value) %>%
    left_join(PCA_summary_importance,  by = "PC")

  #testing to evaluate the optimal number of components
  ev <- eigen(cor(data)) # get eigenEstimates
  ap <- nFactors::parallel(
    subject=nrow(data),
    var=ncol(data),
    rep=100,
    cent=0.05)
  nS <- nFactors::nScree(x=ev$values, aparallel=ap$eigen$qevpea)
  
  optimal_components <- nS$Components$nparallel
  
#  plotnScree(nS)
  
  prop_explained <- sum(summary(data_pca)$importance[2, 1:optimal_components])
  
  nScree_summary_string <- paste(
    "The optimal number of components is", optimal_components, "and they explain", prop_explained * 100, "% of the variation.\n"
  )
  
  cat(nScree_summary_string)
  
  maxcol <- optimal_components+1
  
  x <- list(data_pca_df= data_pca_df, 
            optimal_components = optimal_components, 
            nScree_summary_string = nScree_summary_string, maxcol= maxcol,
            PCA_summary_importance = PCA_summary_importance,
            tidied_pca = tidied_pca)
  x
}




flip_PCA_if_need_be <- function(df, col){
  #df = combined
  #  col <- "Carrying_capactiy_PC2"
  cor_test_PCA_direction <- cor.test(df$NPP_terra_mean, df[,{{col}}], method = "pearson")$estimate
  
  if(cor_test_PCA_direction < -0.2){
    cat("Flipping component direction on", col, ".\n")
    df[,{{col}}] <- 2 - df[,{{col}}]
  }
}