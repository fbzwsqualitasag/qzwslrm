

## -- Readr Based Input Function -----------------------------------------------

#' Read Solani File Using readr
#'
#' @description
#' The file Solani contains predicted breeding values computed by
#' MiX99. This result file is read using `readr::read_table()`
#'
#' @param ps_path path to input file
#' @param pvec_col_names vector of column names
#' @param pl_col_types col type specifications using `readr::cols()`
#'
#' @return tibble with predicted breeding values
#' @export readr_solani
#'
#' @examples
#' s_ebv_path_partial <- system.file("extdata", "mix99", "partial_data", "Solani", package = "qzwslrm")
#' tbl_solani_partial <- readr_solani(ps_path = s_ebv_path_partial)
readr_solani <- function(ps_path,
                         pvec_col_names = c("animal", "nrprg", "trait", "ebv"),
                         pl_col_types   = readr::cols(animal = readr::col_integer(),
                                                     nprg = readr::col_integer(),
                                                     trait = readr::col_integer(),
                                                     ebv = readr::col_double())){
  # check existence of ps_path
  if (!file.exists(ps_path))
    stop(" *** ERROR: CANNOT FIND input file: ", ps_path)
  # read the input file from ps_path
  tbl_solani <- readr::read_table(file           = ps_path,
                                  col_names      = pvec_col_names,
                                  col_types      = pl_col_types,
                                  show_col_types = FALSE,
                                  progress       = FALSE)
  return(tbl_solani)
}

## -- Data.Table Based Input Function ------------------------------------------

#' Read Solani File using data.table
#'
#' @description
#' The solani result file from MiX99 is read using `data.table::fread()`
#'
#' @param ps_path path to input file
#'
#' @return DataTable with content of solani file
#' @export fread_solani
#'
#' @examples
#' s_ebv_path_partial <- system.file("extdata", "mix99", "partial_data", "Solani", package = "qzwslrm")
#' dt_solani <- fread_solani(ps_path = s_ebv_path_partial)
fread_solani <- function(ps_path){
  # check existence of ps_path
  if (!file.exists(ps_path))
    stop(" *** ERROR: CANNOT FIND input file: ", ps_path)
  # read solani file
  dt_solani <- data.table::fread(file = s_solani_path_whole, header = FALSE)
  # return
  return(dt_solani)
}

## -- R-Base Input Function ---------------------------------------------------

#' Read Solani File using R-Base read.table
#'
#' @description
#' The function `read.table()` from R-base is used to read the solani file.
#'
#' @param ps_path path to input file
#'
#' @return dataframe with content of solani file
#' @export read.solani
#'
#' @examples
#' s_ebv_path_partial <- system.file("extdata", "mix99", "partial_data", "Solani", package = "qzwslrm")
#' df_solani <- read.solani(ps_path = s_ebv_path_partial)
read.solani <- function(ps_path){
  # check existence of ps_path
  if (!file.exists(ps_path))
    stop(" *** ERROR: CANNOT FIND input file: ", ps_path)
  # reading solani into dataframe
  df_solani <- read.table(file = ps_path, header = FALSE, sep = "")
  # return
  return(df_solani)
}
