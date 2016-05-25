
#' Print In PBDR context
#'
#' Wrapper around \code{comm.cat}
#' @param ... to be forwarded to cat
#' @param all.rank Should all the rank partake in the printing
#' @export
jlp.cat <- function(...,
                    all.rank = TRUE,
                    barrier = FALSE)
{
    comm.cat(quiet = TRUE, Sys.info()["nodename"], "PROC:", comm.rank(),  "|", ..., all.rank = all.rank, barrier = barrier)
}

JPLogger <- setRefClass("JPLogger", contains = "JLogger")

#' @export
setMethod("jlog.trace", "JPLogger", function(jlogger, ..., cat.fun = jlp.cat) callNextMethod(jlogger, cat.fun = cat.fun, ...))

#' @export
setMethod("jlog.debug", "JPLogger", function(jlogger, ..., cat.fun = jlp.cat) callNextMethod(jlogger, cat.fun = cat.fun, ...))

#' @export
setMethod("jlog.info", "JPLogger", function(jlogger, ..., cat.fun = jlp.cat) callNextMethod(jlogger, cat.fun = cat.fun, ...))

#' @export
setMethod("jlog.warn", "JPLogger", function(jlogger, ..., cat.fun = jlp.cat) callNextMethod(jlogger, cat.fun = cat.fun, ...))

#' @export
setMethod("jlog.error", "JPLogger", function(jlogger, ..., cat.fun = jlp.cat) callNextMethod(jlogger, cat.fun = cat.fun, ...))

#' @export
setMethod("jlog.fatal", "JPLogger", function(jlogger, ..., cat.fun = jlp.cat) callNextMethod(jlogger, cat.fun = cat.fun, ...))

JPLOGGER.ENV <- new.env()
#' @name JPLogger
#' @title Parallel JLoggers
#' @details
#' JPLogger are a version of JLogger that can be used with pbdR. It uses \code{comm.cat} internally, so all parameters are forwarded to it. \code{rank.print} can be set as well as other options. \cr
#' JPLogger will create a file for each process. Remember to set a different one for each process unless you want them to appear on only one file.
#' @export
JPLoggerFactory <- function(name, ..., reset = FALSE)
{
    if(!name %in% ls(JPLOGGER.ENV) || reset) assign(name, JPLogger(name, ...), JPLOGGER.ENV)
    get(name, JPLOGGER.ENV)
}
