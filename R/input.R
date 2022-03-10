

## -- Readr Based Input Function -----------------------------------------------

#' Read Estimated Breeding Values (EBV) from Input File
#'
#' @description
#' Given a result file containing EBV and possibly other information,
#' the column with AnimalID and with EBV are extracted and returned as
#' a two column result tibble. The result tibble contains the AnimalID
#' in the first column and the EBV in the second column.
#'
#' @details
#' The EBV input file can be in different formats. Possible formats that
#' are considered can be "csv", "csv2", "delim", "table". Where each of
#' these format specifiers point to the respective function from the `readr`
#' package that is used for importing.
#'
#' @param ps_path path to the EBV input file
#' @param ps_format format of the EBV input file, can be either of "csv", "csv2", "delim" or "table"
#' @param ps_delim_char column delimiting character for format "delim"
#' @param ps_animal_col_name column name for animal ID column
#' @param pn_animal_col_idx column index for animal ID column
#' @param ps_ebv_col_name column name for ebv column
#' @param pn_ebv_col_idx column index for ebv column
#'
#' @return ebv result tibble
#' @export readr_ebv
#'
#' @examples
#' s_ebv_path_partial <- qzwslrm_example_solani("partial")
#' tbl_ebv <- readr_ebv(ps_path = s_ebv_path_partial, ps_format = "table")
readr_ebv <- function(ps_path,
                      ps_format,
                      ps_delim_char      = " ",
                      ps_animal_col_name = NULL,
                      pn_animal_col_idx  = 1,
                      ps_ebv_col_name    = NULL,
                      pn_ebv_col_idx     = NULL){
  # check existence of ps_path
  if (!file.exists(ps_path))
    stop(" *** ERROR: CANNOT FIND input file: ", ps_path)
  # check format
  vec_formats <- c("csv", "csv2", "delim", "table")
  if (!is.element(ps_format, vec_formats))
    stop(" *** ERROR: Non-Recognized format: ", ps_format)

  # decide whether file has column headers
  pb_has_col_header <- !is.null(ps_animal_col_name) || !is.null(ps_ebv_col_name)
  tbl_ebv_result <- NULL
  # read file depending on parameters specified
  if (ps_format == "csv"){
    tbl_ebv_result <- readr::read_csv(file = ps_path, col_names = pb_has_col_header)
  } else if (ps_format == "csv2"){
    tbl_ebv_result <- readr::read_csv2(file = ps_path, col_names = pb_has_col_header)
  } else if (ps_format == "delim"){
    tbl_ebv_result <- readr::read_delim(file = ps_path, delim = ps_delim_char, col_names = pb_has_col_header)
  } else if (ps_format == "table") {
    tbl_ebv_result <- readr::read_table(file = ps_path, col_names = pb_has_col_header)
  } else {
    stop(" *** ERROR: Non-recognized format during reading input: ", ps_format)
  }
  # column names
  vec_col_names_result <- colnames(tbl_ebv_result)
  # select columns with animal IDs and with EBVs
  if (!is.null(ps_animal_col_name)){
    n_animal_col_idx <- which(ps_animal_col_name == vec_col_names_result)
  } else {
    n_animal_col_idx <- pn_animal_col_idx
  }
  # select column idx for ebv
  if (is.null(ps_ebv_col_name)){
    if (is.null(pn_ebv_col_idx)){
      n_ebv_col_idx <- ncol(tbl_ebv_result)
    } else {
      n_ebv_col_idx <- pn_ebv_col_idx
    }
  } else {
    n_ebv_col_idx <- which(ps_ebv_col_name == vec_col_names_result)
  }
  # select only columns for animal ID and ebv and return
  return(tbl_ebv_result[,c(n_animal_col_idx, n_ebv_col_idx)])

}

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
#' s_ebv_path_partial <- qzwslrm_example_solani("partial")
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
#' s_ebv_path_partial <- qzwslrm_example_solani("partial")
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
#' s_ebv_path_partial <- qzwslrm_example_solani("partial")
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
