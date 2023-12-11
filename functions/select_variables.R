#' Select Variables
#'
#' Selects variables for cohorts  
#' @param dataset complete dataset 
#' @export 
select_variables <- function(dataset) {
  variables.to.include <- ("pt_Gender")
  prepared.dataset <- dataset[, variables.to.include]
  return (returned.dataset)
}
