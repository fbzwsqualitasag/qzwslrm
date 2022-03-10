
## -- Path To Example Files ----------------------------------------------------

#' Return Path to Example Data
#'
#' @description
#' Return path to example solani result file containing EBV from either whole
#' or partial data.
#'
#' @param ps_data_type type of data set can be either 'partial' or 'whole'
#'
#' @return path to example data
#' @export qzwslrm_example_solani
#'
#' @examples
#' ed_path <- qzwslrm_example_solani('partial')
qzwslrm_example_solani <- function(ps_data_type){
  s_example_data_result <- NULL
  if (ps_data_type == 'partial'){
    s_example_data_result <- system.file("extdata", "mix99", "partial_data", "Solani", package = "qzwslrm")
  } else if (ps_data_type == 'whole'){
    s_example_data_result <- system.file("extdata", "mix99", "whole_data", "Solani", package = "qzwslrm")
  } else {
    stop(" *** ERROR: Unknown type of data: ", ps_data_type, " - either use [partial/whole]")
  }
  return(s_example_data_result)
}
