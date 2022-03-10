

## -- EBV Scatter Plot ---------------------------------------------------------

#' Scatter Plot Based on Validation Input
#'
#' @description
#' The EBV for which validation is done is shown in a scatterplot
#'
#' @param ptbl_ebv_whole ebv based on whole data
#' @param ptbl_ebv_partial ebv based on partial data
#'
#' @return ggplot2 objection conaining scatterplot
#' @export scatterplot_lrm
#'
#' @examples
#' tbl_ebv_whole <- readr_ebv(ps_path = qzwslrm_example_solani("whole"), ps_format = "table",
#'                           pn_ebv_col_idx = 4)
#' tbl_ebv_partial <- readr_ebv(ps_path = qzwslrm_example_solani("partial"), ps_format = "table",
#'                             pn_ebv_col_idx = 4)
#' p <- scatterplot_lrm(tbl_ebv_whole, tbl_ebv_partial)
scatterplot_lrm <- function(ptbl_ebv_whole, ptbl_ebv_partial){
  tbl_plot_input <- dplyr::inner_join(ptbl_ebv_whole, ptbl_ebv_partial,
                                      by = c("animal" = "animal"),
                                      suffix = c(".whole", ".partial"))
  p_result <- ggplot2::ggplot(tbl_plot_input, ggplot2::aes(x = ebv.partial, y = ebv.whole)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm") +
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "red")
  # return
  return(p_result)
}
