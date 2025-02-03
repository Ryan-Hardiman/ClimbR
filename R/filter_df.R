
#' Filter a tibble by a parameter list
#'
#' @param df a dataframe / tibble to filter
#' @param ... A named list, where names are columns of the df and parameter vectors of filter variables. 
#'
#' @return a filtered tibble / dataframe 
#' @export
#'
#' @examples filter_df(mtcars, cyl = 6, gear =c(4,5))
filter_df <- function(df,...){
  vars <- rlang::list2(...)
  
  purrr::reduce2(names(vars),vars, 
                 \(acc,nxt_name,nxt_var) 
                 acc|>dplyr::filter(!!rlang::sym(nxt_name) %in% nxt_var),
                 .init = df)
  
}



