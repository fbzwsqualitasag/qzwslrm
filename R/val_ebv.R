

## -- EBV Validation -----------------------------------------------------------


#' Validation of Estimated Breeding Values (EBV) Using LR-Method
#'
#' @description
#' Validation statistics for EBV based on bias, correlation and regression
#' coefficients between EBV results based on partial and whole data sets are
#' computed.
#'
#' @param pvec_ebv_partial vector of EBV from partial data
#' @param pvec_ebv_whole vector of EBV from whole data
#'
#' @return list of results containing bias, correlation and regression coefficients
#' @export val_ebv_lrm
#'
#' @examples
#'
val_ebv_lrm <- function(pvec_ebv_partial, pvec_ebv_whole){
  # check length
  if (length(pvec_ebv_partial) != length(pvec_ebv_whole))
    stop(" *** ERROR: EBV vectors [partial/whole] do not have the same length")
  # bias of ebv
  n_bias_ebv <- mean(pvec_ebv_partial) - mean(pvec_ebv_whole)
  # regression of whole on partial
  n_reg_wop <- cov(pvec_ebv_partial, pvec_ebv_whole) / var(pvec_ebv_partial)
  # correlation between whole and partial
  n_cor_wp <- cor(pvec_ebv_partial, pvec_ebv_whole)
  # regression of partial on whole
  n_reg_pow <- cov(pvec_ebv_partial, pvec_ebv_whole) / var(pvec_ebv_whole)
  # return list of results
  return(list(bias    = n_bias_ebv,
              reg_wop = n_reg_wop,
              cor_wp  = n_cor_wp,
              reg_pow = n_reg_pow))
}
