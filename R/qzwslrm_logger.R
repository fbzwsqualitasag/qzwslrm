### #
### #
### #
### #   Purpose:   Functions Related to Logging
### #   started:   2019-10-03 (pvr)
### #
### # ############################################## ###

#' @title Create log4r Logger for package
#'
#' @param ps_logfile name of the logfile
#' @param ps_level logger level
#'
#' @return qzwslrm_logger
#' @export get_qzwslrm_logger
#'
#' @examples
#' \dontrun{
#' qzwslrm_logger <- get_qzwslrm_logger()
#' }
get_qzwslrm_logger <- function(ps_logfile = 'qzwslrm.log', ps_level = 'FATAL'){
  qzwslrm_logger <- log4r::create.logger(logfile = ps_logfile, level = ps_level)
  return(qzwslrm_logger)
}


#' @title Wrapper for log4r info
#'
#' @param plogger log4r logger object
#' @param ps_caller function from which we are called
#' @param ps_msg logging message
#'
#' @export qzwslrm_log_info
#'
#' @examples
#' \dontrun{
#' qzwslrm_logger <- get_qzwslrm_logger()
#' qzwslrm_log_level(qzwslrm_logger, 'INFO')
#' qzwslrm_log_info(qzwslrm_logger, 'Examples', 'test message')
#' }
qzwslrm_log_info <- function(plogger, ps_caller, ps_msg){
  s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
  log4r::info(logger = plogger, message = s_msg)
}


#' @title Wrapper for log4r debug
#'
#' @param plogger log4r logger object
#' @param ps_caller function from which we are called
#' @param ps_msg logging message
#'
#' @export qzwslrm_log_debug
#'
#' @examples
#' \dontrun{
#' qzwslrm_logger <- get_qzwslrm_logger()
#' qzwslrm_log_level(qzwslrm_logger, 'DEBUG')
#' qzwslrm_log_debug(qzwslrm_logger, 'Examples', 'test message')
#' }
qzwslrm_log_debug <- function(plogger, ps_caller, ps_msg){
  s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
  log4r::debug(logger = plogger, message = s_msg)
}


#' @title Wrapper for log4r warn
#'
#' @param plogger log4r logger object
#' @param ps_caller function from which we are called
#' @param ps_msg logging message
#'
#' @export qzwslrm_log_warn
#'
#' @examples
#' \dontrun{
#' qzwslrm_logger <- get_qzwslrm_logger()
#' qzwslrm_log_level(qzwslrm_logger, 'WARN')
#' qzwslrm_log_warn(qzwslrm_logger, 'Examples', 'test message')
#' }
qzwslrm_log_warn <- function(plogger, ps_caller, ps_msg){
  s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
  log4r::warn(logger = plogger, message = s_msg)
}


#' @title Wrapper for log4r error
#'
#' @param plogger log4r logger object
#' @param ps_caller function from which we are called
#' @param ps_msg logging message
#'
#' @export qzwslrm_log_error
#'
#' @examples
#' \dontrun{
#' qzwslrm_logger <- get_qzwslrm_logger()
#' qzwslrm_log_level(qzwslrm_logger, 'ERROR')
#' qzwslrm_log_error(qzwslrm_logger, 'Examples', 'test message')
#' }
qzwslrm_log_error <- function(plogger, ps_caller, ps_msg){
  s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
  log4r::error(logger = plogger, message = s_msg)
}


#' @title Wrapper for log4r fatal
#'
#' @param plogger log4r logger object
#' @param ps_caller function from which we are called
#' @param ps_msg logging message
#'
#' @export qzwslrm_log_fatal
#'
#' @examples
#' \dontrun{
#' qzwslrm_logger <- get_qzwslrm_logger()
#' qzwslrm_log_level(qzwslrm_logger, 'FATAL')
#' qzwslrm_log_fatal(qzwslrm_logger, 'Examples', 'test message')
#' }
qzwslrm_log_fatal <- function(plogger, ps_caller, ps_msg){
  s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
  log4r::fatal(logger = plogger, message = s_msg)
}


#' @title Wrapper to set the level of a logger
#'
#' @param plogger log4r logger object
#' @param ps_level new level of plogger
#'
#' @export qzwslrm_log_level
#'
#' @examples
#' \dontrun{
#' qzwslrm_logger <- get_qzwslrm_logger()
#' qzwslrm_log_level(qzwslrm_logger, 'INFO')
#' }
qzwslrm_log_level <- function(plogger, ps_level = c('DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL')){
  if (!missing(ps_level) & length(ps_level) > 1) stop(" *** ERROR in level(): only one 'level' allowed.")
  ps_level <- match.arg(ps_level)
  log4r::level(plogger) <- ps_level
}
