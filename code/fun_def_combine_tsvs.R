
combine_tsvs <- function(fns = NULL){
fns %>% 
  map_df(
    function(x) data.table::fread(x ,
                                  encoding = 'UTF-8', header = TRUE, 
                                  fill = TRUE, blank.lines.skip = TRUE,
                                  sep = "\t", na.strings = "",
    )   %>% 
      mutate(filename = x
             
      ) 
  ) 

}