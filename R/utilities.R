#' Function to add a new column indicating group ID based on sequential data
#'
#' @param vec the vector
#'
#' @return a vector where the ID depends on whether the value of x is
#'  sequential. For example, indicate whether a series of years is sequential,
#'  or if there are say three groups of sequential years.
#'  Break up the data by groups with consecutive values
consecutive_group <- function(vec) {
  d_unique_groups <- split(x = vec, f = cumsum(c(1, diff(vec) != 1)))
  for(g in seq_along(d_unique_groups)){
    d_unique_groups[[g]] <- rep(g, times = length(d_unique_groups[[g]]))
  }
  as.vector(unlist(d_unique_groups))
}
