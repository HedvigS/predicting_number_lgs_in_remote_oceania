SPLOM_hedders <- function(df){
  df %>% 
  pairs.panels(method = "pearson", # correlation method
                                hist.col = "#a3afd1",# "#a9d1a3","",""),
                                density = TRUE,  # show density plots
                                ellipses = F, # show correlation ellipses
                                cex.labels= 0.7,
                                #           smoother= T,
                                cor=T,
                                lm=T,
                                ci = T, 
                                cex.cor = 1,stars = T)
}