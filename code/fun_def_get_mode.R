
get_mode <- function(v, na.rm = T) {
  if(na.rm == T){
    v <- v[which(!is.na(v))] }
  
  uniqv <- unique(v)
  table <- tabulate(match(v, uniqv))
  names(table) <- uniqv
  
  tbl <- which(table == max(table))
  if(length(tbl) > 1){
    as.numeric(names(sample(which(table == max(table)), 1)))
  }else{
    as.numeric(names(tbl))
  }
  
}