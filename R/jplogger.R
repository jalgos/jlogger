
#' @title Print In PBDR context
#' @name jlp.print
NULL

#' @describeIn jlp.print Wrapper around \code{comm.cat}
#' @param ... to be forwarded to \code{comm.cat} or \code{comm.print}
#' @param all.rank Should all the rank partake in the printing
#' @export
jlp.cat <- function(...,
                    rank.print = integer(),
                    all.rank = length(rank.print) == 0L,
                    barrier = FALSE)
{
    pbdMPI::comm.cat(quiet = TRUE, Sys.info()["nodename"], "PROC:", pbdMPI::comm.rank(),  "|", ..., all.rank = all.rank, barrier = barrier, rank.print = rank.print)
}

#' @describeIn jlp.print Wrapper around \code{comm.print}
#' @export
jlp.print <- function(...,
                      rank.print = integer(),
                      all.rank = length(rank.print) == 0L,
                      barrier = FALSE)
{
    pbdMPI::comm.print(..., all.rank = all.rank, barrier = barrier, rank.print = rank.print)
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

#' @export
setMethod("jlprint.trace", "JPLogger", function(jlogger, ..., print.fun = jlp.print, cat.fun = jlp.cat) callNextMethod(jlogger, cat.fun = cat.fun, print.fun = jlp.print, ...))

#' @export
setMethod("jlprint.debug", "JPLogger", function(jlogger, ..., print.fun = jlp.print, cat.fun = jlp.cat) callNextMethod(jlogger, cat.fun = cat.fun, print.fun = jlp.print, ...))

#' @export
setMethod("jlprint.info", "JPLogger", function(jlogger, ..., print.fun = jlp.print, cat.fun = jlp.cat) callNextMethod(jlogger, cat.fun = cat.fun, print.fun = jlp.print, ...))

#' @export
setMethod("jlprint.warn", "JPLogger", function(jlogger, ..., print.fun = jlp.print, cat.fun = jlp.cat) callNextMethod(jlogger, cat.fun = cat.fun, print.fun = jlp.print, ...))

#' @export
setMethod("jlprint.error", "JPLogger", function(jlogger, ..., print.fun = jlp.print, cat.fun = jlp.cat) callNextMethod(jlogger, cat.fun = cat.fun, print.fun = jlp.print, ...))

#' @export
setMethod("jlprint.fatal", "JPLogger", function(jlogger, ..., print.fun = jlp.print, cat.fun = jlp.cat) callNextMethod(jlogger, cat.fun = cat.fun, print.fun = jlp.print, ...))


#' @name JPLogger
#' @title Parallel JLoggers
#' @details
#' JPLogger are a version of JLogger that can be used with pbdR. It uses \code{comm.cat} internally, so all parameters are forwarded to it. \code{rank.print} can be set as well as other options. \cr
#' JPLogger will create a file for each process. Remember to set a different one for each process unless you want them to appear on only one file.
#' @export
JPLoggerFactory <- function(name, ..., reset = FALSE)
{
    if(!name %in% ls(JLOGGER.ENV) || reset) assign(name, JPLogger(name, ...), JLOGGER.ENV)
    jlog <- get(name, JLOGGER.ENV)
    if(!is(jlog, "JPLogger")) assign(name, JPLogger(name, ...), JLOGGER.ENV)
    else jlog
}
