

## -- Summary Function ---------------------------------------------------------


#' Summary of EBV Validation Results
#'
#' @description
#' The list with the validation results is printed in a summary format.
#'
#' @param object validation result
#' @param pn_digits number of digits to be used for rounding the results
#'
#' @export summary_lrm
#'
#' @examples
#' tbl_ebv_whole <- readr_ebv(ps_path = qzwslrm_example_solani("whole"), ps_format = "table",
#'                            pn_ebv_col_idx = 4)
#' tbl_ebv_partial <- readr_ebv(ps_path = qzwslrm_example_solani("partial"), ps_format = "table",
#'                              pn_ebv_col_idx = 4)
#' summary_lrm(l_val_result <- val_ebv_lrm(pvec_ebv_partial = tbl_ebv_partial$ebv,
#'                                         pvec_ebv_whole = tbl_ebv_whole$ebv))
summary_lrm <- function(object, pn_digits = max(3L, getOption("digits") - 3L)){
  cat("\nBias between partial and whole: \t", round(object$bias, digits = pn_digits), "\n", sep = "")
  cat("Regression whole on partial: \t\t", round(object$reg_wop, digits = pn_digits), "\n", sep = "")
  cat("Correlation whole and partial: \t\t", round(object$cor_wp, digits = pn_digits), "\n", sep = "")
  cat("Regression partial on whole: \t\t", round(object$reg_pow, digits = pn_digits), "\n", sep = "")

  return(invisible(NULL))
}

## -- Result Tibble -----------------------------------------------------------

#' Validation Results as Tibble
#'
#' @description
#' The EBV validation results are converted into a tibble
#'
#' @param object validation result
#' @param pn_digits number of digits to be used for rounding the results
#'
#' @return tibble with validation results
#' @export tibble_lrm
#'
#' @examples
#' tbl_ebv_whole <- readr_ebv(ps_path = qzwslrm_example_solani("whole"), ps_format = "table",
#'                            pn_ebv_col_idx = 4)
#' tbl_ebv_partial <- readr_ebv(ps_path = qzwslrm_example_solani("partial"), ps_format = "table",
#'                              pn_ebv_col_idx = 4)
#' tibble_lrm(l_val_result <- val_ebv_lrm(pvec_ebv_partial = tbl_ebv_partial$ebv,
#'                                         pvec_ebv_whole = tbl_ebv_whole$ebv))
tibble_lrm <- function(object, pn_digits = max(3L, getOption("digits") - 3L)){
  tbl_lrm_result <- tibble::tibble(`Validation Statistic` = c("Bias between partial and whole",
                                                              "Regression whole on partial",
                                                              "Correlation whole and partial",
                                                              "Regression partial on whole"),
                                   `Value` = round(c(object$bias,
                                                     object$reg_wop,
                                                     object$cor_wp,
                                                     object$reg_pow), digits = pn_digits))
  return(tbl_lrm_result)

}

