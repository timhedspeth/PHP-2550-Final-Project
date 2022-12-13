
## This is a function that will compress the levels of factor variables to
## be the top 7 levels so that we ccan have more parcimonius levels in our 
## analysis 

compress_levels_factors <- function(df, var){
  #' This function will compress your variable to be the num levels you want and 
  #' classify the other variables to be "other" 
  #' 
  #'@param df, data frame with the variable of interest  
  #'@param var, the variable you want to compress  
  #'@param num, the number of levels you want your 
  #'@return the data frame with the compressed levels for your factor 
  
  
  top_counts <- df %>% dplyr::filter(!is.na(!!sym(var))) %>% 
    dplyr::group_by(!!sym(var)) %>% 
    dplyr::summarize(counts = n()) %>% 
    dplyr::ungroup() %>% 
    slice_max(counts, n = 7)
  levels <- as.data.frame(top_counts[,1])
  levels1 <- as.vector(c(levels[1,1], levels[2,1], levels[3,1], levels[4,1], 
                         levels[5,1], levels[6,1], levels[7,1]))
  x <- sum(df[[var]] %in% levels1)  
  df[[var]] <- case_when(df[[var]] %in% levels1 ~ df[[var]], 
                         is.na(df[[var]]) ~ df[[var]],
                         !(df[[var]] %in% levels1) ~ "Other")
  if(min(top_counts[,2]) < 10){
    return("One of the top levels has less than 10 observations, you should check if you want to conslidate this data")
  } else{
    return(df[[var]])
  }
  
}
