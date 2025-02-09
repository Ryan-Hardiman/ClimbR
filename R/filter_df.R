#' Filter a Tibble or Data Frame by Multiple Parameters
#'
#' This function filters a tibble (or data frame) based on multiple criteria, where each 
#' criterion is a column name mapped to a vector of allowed values. The function retains 
#' rows where the column values match any of the provided values or are `NA` (missing).
#'
#' @param df A tibble or data frame to be filtered.
#' @param ... Named arguments where names correspond to column names in `df`, and values 
#'   are vectors specifying the allowed values for filtering.
#'
#' @return A filtered tibble or data frame.
#' 
#' @details This function iteratively applies filtering using `purrr::reduce2()`, ensuring that 
#'   multiple filtering conditions are applied sequentially. The function also retains `NA` values 
#'   to avoid unintentionally excluding missing data.
#'
#' @export
#'
#' @examples 
#' # Example with mtcars dataset
#' filter_df(mtcars, cyl = 6, gear = c(4, 5))
#' 
#' # Example with a tibble
#' library(tibble)
#' df <- tibble(name = c("Alice", "Bob", "Charlie"), age = c(25, 30, 35), city = c("NY", "LA", "SF"))
#' filter_df(df, age = c(25, 30))
filter_df <- function(df, ...) {
  vars <- rlang::list2(...)
  
  purrr::reduce2(
    names(vars), vars, 
    \(acc, nxt_name, nxt_var) acc |> dplyr::filter(!!rlang::sym(nxt_name) %in% c(nxt_var, NA)),
    .init = df
  )
}

