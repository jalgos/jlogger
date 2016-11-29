## logger that uses cat instead of print
## depends on util.R


JLogger <- setRefClass("JLogger", fields = list(m.level = "integer", m.file = "character"))

#' @title Logging levels
#' @name logging.levels
#' @description Constants for logging level
#' @export
JLOGGER.TRACE <- 1L

#' @rdname logging.levels
#' @export
JLOGGER.DEBUG <- 2L

#' @rdname logging.levels
#' @export
JLOGGER.INFO <- 3L

#' @rdname logging.levels
#' @export
JLOGGER.WARN <- 4L

#' @rdname logging.levels
#' @export
JLOGGER.ERROR <- 5L

#' @rdname logging.levels
#' @export
JLOGGER.FATAL <- 6L

#' @rdname logging.levels
#' @export
JLOGGER.LEVELS <- c("TRACE", "DEBUG", "INFO", "WARNING", "ERROR", "FATAL")

#' @rdname logging.levels
#' @export
JLOGGER.DEFAULT.LEVEL <- JLOGGER.DEBUG

JLOGGER.ENV <- new.env()

#' JLogger Factory
#'
#' Get a logger for the given handle. Creates it if it's missing.
#' @param name Handle for the JLogger
#' @param ... Parameters to be passes to the constructor of JLogger. The parameters are files, prefix, level, config, logconfig
#' @return A JLogger object
#' @export
JLoggerFactory <- function(name, ..., reset = FALSE)
{
    if(!name %in% ls(JLOGGER.ENV) || reset) assign(name, JLogger(name, ...), JLOGGER.ENV)
    get(name, JLOGGER.ENV)
}

#' JLoggerReset
#'
#' Resets all the JLoggers
#' @export
JLoggerReset <- function()
{
    rm(list = ls(env = JLOGGER.ENV), envir = JLOGGER.ENV)
}

#' Set an indivual JLogger
#'
#' Manually maps a logger to a handle
#' @param name handle to be used
#' @param jlogger logger to be mapped
#' @export
SetJLogger <- function(name, jlogger, ...)
{
    if(!"JLOGGER.ENV" %in% ls(.GlobalEnv)) JLOGGER.ENV <<- new.env()
    assign(name, jlogger, JLOGGER.ENV)
}

## We want the JLogger to be a ref class so the prefix can be changed without having to be propagated
## m.files can be several file if we want to write to several connections at the same time
#' JLogger
#'
#' Object that handles complex logging
#' @slot m.files buffers to which the logger writes to
#' @slot m.level debugging level
#' @slot m.name Handle
#' @slot m.prefix Prefix to print before messages
#' @exportClass JLogger
#' @export JLogger
JLogger <- setRefClass("JLogger",
                       fields = list(m.files = "character",
                                     m.level = "integer",
                                     m.name = "character",
                                     m.prefix = "character"))

JLOGGER.init <- function(name = "",
                         files = "",
                         prefix = name,
                         level,
                         config = jconfig::get.config(...),
                         ...,
                         logconfig = config$jlogger)
{
    m.name <<- name
    m.files <<- files
    if(missing(level)) level <- JLOGGER.getlevel(name, logconfig)
    m.level <<- level
    m.prefix <<- prefix
}

JLOGGER.show <- function()
{
    files <- m.files
    files[files == ""] <- "*console*"
    cat(class(.self), "name:", m.name, "prefix:", m.prefix, "level:", JLOGGER.LEVELS[m.level], "files:", paste(files, sep = "/ "), "\n")
}

JLogger$methods(initialize = JLOGGER.init,
                show = JLOGGER.show)

#We may want to flush
JLOGGER.flush <- function(jlfile)
{
   if(jlfile != "") cat("", file = jlfile)
}

JLOGGER.getlevel <- function(name, logconfig)
{
    if(missing(logconfig) || is.null(logconfig)) return(JLOGGER.DEFAULT.LEVEL)
    level <- 0L
    for(lv in JLOGGER.LEVELS)
    {
        level <- level + 1L
        lvnode <- logconfig[[lv]]
        if(is.null(lvnode)) next
        if(mgrep(lvnode, grep.fun = grepl, name)) return(level)
    }
    return(level + 1L)#won't log anything
}

#Not a true set will return a copy. Can override a few value but not the name and file
jlset <- function(jlogger, level, ...)
{
    if(missing(level)) level <- jlogger@level
    return(JLogger(jlogger$m.name, jlogger$m.file, level = level, ...))
}

#Is the logger quiet for the given level
JLOGGER.jlquiet <- function(jlogger, level, ...)
{
    if(is.null(jlogger)) return(TRUE)
    level < jlogger$m.level
}

#To use for object that can't be printed with cat. Uses write.table internally
JLOGGER.jlwrite <- function(jlfile, level, prefix, data, ..., endline = "\n")
{
    cat(as.character(Sys.time()), JLOGGER.LEVELS[level], prefix, ":", endline, file = jlfile, append = TRUE)
    suppressWarnings(write.table(data, ..., file = jlfile, append = TRUE))#Warns about appending column names to a file
    cat(endline, file = jlfile, append = TRUE)
}

#To print complicated objects not handle by cat to the console
JLOGGER.jlprint <- function(jlfile,
                            level,
                            prefix,
                            data,
                            ...,
                            cat.fun = cat,
                            print.fun = print,
                            endline = "\n")
{
    #This prints to the console so only id jlfile == ""
    if(jlfile != "") return()
    cat.fun(as.character(Sys.time()), JLOGGER.LEVELS[level], prefix, ":", endline, file = jlfile, append = TRUE, ...)
    print.fun(data, ...)
    cat.fun(endline, file = jlfile, append = TRUE, ...)
}

JLOGGER.jlog <- function(jlfile,
                         level,
                         ...,
                         prefix,
                         prechar = "",
                         endline = "\n",
                         cat.fun = cat,
                         all.rank) ## Here to be compatible with multiprocess case
{
    if(prechar != "") cat(prechar, file = jlfile, append = TRUE)
    cat.fun(as.character(Sys.time()), JLOGGER.LEVELS[level], prefix, ":", ..., endline, file = jlfile, append = TRUE)
}

JLOGGER.do <- function(jlogger, level, log.fun, ..., prefix = jlogger$m.prefix )
{
    if(JLOGGER.jlquiet(jlogger, level, ...)) return()
    lapply(jlogger$m.files, log.fun, prefix = prefix, level = level, ...)
    invisible()
}

jl.log.trace <- function(jlogger, ...) JLOGGER.do(jlogger, JLOGGER.TRACE, JLOGGER.jlog, ...)
jl.log.debug <- function(jlogger, ...) JLOGGER.do(jlogger, JLOGGER.DEBUG, JLOGGER.jlog, ...)
jl.log.info <- function(jlogger, ...) JLOGGER.do(jlogger, JLOGGER.INFO, JLOGGER.jlog, ...)
jl.log.warn <- function(jlogger, ...) JLOGGER.do(jlogger, JLOGGER.WARN, JLOGGER.jlog, ...)
jl.log.error <- function(jlogger, ...) JLOGGER.do(jlogger, JLOGGER.ERROR, JLOGGER.jlog, ...)
jl.log.fatal <- function(jlogger, ...) JLOGGER.do(jlogger, JLOGGER.FATAL, JLOGGER.jlog, ...)

jl.write.trace <- function(jlogger, ...) JLOGGER.do(jlogger, JLOGGER.TRACE, JLOGGER.jlwrite, ...)
jl.write.debug <- function(jlogger, ...) JLOGGER.do(jlogger, JLOGGER.DEBUG, JLOGGER.jlwrite, ...)
jl.write.info <- function(jlogger, ...) JLOGGER.do(jlogger, JLOGGER.INFO, JLOGGER.jlwrite, ...)
jl.write.warn <- function(jlogger, ...) JLOGGER.do(jlogger, JLOGGER.WARN, JLOGGER.jlwrite, ...)
jl.write.error <- function(jlogger, ...) JLOGGER.do(jlogger, JLOGGER.ERROR, JLOGGER.jlwrite, ...)
jl.write.fatal <- function(jlogger, ...) JLOGGER.do(jlogger, JLOGGER.FATAL, JLOGGER.jlwrite, ...)

jl.print.trace <- function(jlogger, ...) JLOGGER.do(jlogger, JLOGGER.TRACE, JLOGGER.jlprint, ...)
jl.print.debug <- function(jlogger, ...) JLOGGER.do(jlogger, JLOGGER.DEBUG, JLOGGER.jlprint, ...)
jl.print.info <- function(jlogger, ...) JLOGGER.do(jlogger, JLOGGER.INFO, JLOGGER.jlprint, ...)
jl.print.warn <- function(jlogger, ...) JLOGGER.do(jlogger, JLOGGER.WARN, JLOGGER.jlprint, ...)
jl.print.error <- function(jlogger, ...) JLOGGER.do(jlogger, JLOGGER.ERROR, JLOGGER.jlprint, ...)
jl.print.fatal <- function(jlogger, ...) JLOGGER.do(jlogger, JLOGGER.FATAL, JLOGGER.jlprint, ...)


#' @name logging.funs
#' @title Functions used to log
#'
#' @description
#' Utility function to log messages. Several kind of logging functions are provided for handling different use cases. Each type of function handles 6 levels of logging ranging from TRACE to FATAL.
#' @template package.description
#' @param jlogger The JLogger object. It's level combined with the functions level will decide whether the message is logged.
#' @param ... Objects to be printed. They will be handled by the lower level functions
#' @param prechar A character that can be printed before the message. Useful if you want to have a statement that erases itself.
#' @param endline What should be used as an endline delimiter. Defaults to newline.
                         
NULL

#' @rdname logging.funs
#' @export
setGeneric("jlog.trace", function(jlogger, ...) standardGeneric("jlog.trace"))

#' @rdname logging.funs
#' @export
setGeneric("jlwrite.trace", function(jlogger, ...) standardGeneric("jlwrite.trace"))

#' @rdname logging.funs
#' @export
setGeneric("jlprint.trace", function(jlogger, ...) standardGeneric("jlprint.trace"))

#' @rdname logging.funs
#' @export
setGeneric("jlog.debug", function(jlogger, ...) standardGeneric("jlog.debug"))

#' @rdname logging.funs
#' @export
setGeneric("jlwrite.debug", function(jlogger, ...) standardGeneric("jlwrite.debug"))

#' @rdname logging.funs
#' @export
setGeneric("jlog.info", function(jlogger, ...) standardGeneric("jlog.info"))

#' @rdname logging.funs
#' @export
setGeneric("jlprint.debug", function(jlogger, ...) standardGeneric("jlprint.debug"))

#' @rdname logging.funs
#' @export
setGeneric("jlwrite.info", function(jlogger, ...) standardGeneric("jlwrite.info"))

#' @rdname logging.funs
#' @export
setGeneric("jlprint.info", function(jlogger, ...) standardGeneric("jlprint.info"))

#' @rdname logging.funs
#' @export
setGeneric("jlwrite.warn", function(jlogger, ...) standardGeneric("jlwrite.warn"))

#' @rdname logging.funs
#' @export
setGeneric("jlog.warn", function(jlogger, ...) standardGeneric("jlog.warn"))

#' @rdname logging.funs
#' @export
setGeneric("jlprint.warn", function(jlogger, ...) standardGeneric("jlprint.warn"))

#' @rdname logging.funs
#' @export
setGeneric("jlog.error", function(jlogger, ...) standardGeneric("jlog.error"))

#' @rdname logging.funs
#' @export
setGeneric("jlwrite.error", function(jlogger, ...) standardGeneric("jlwrite.error"))

#' @rdname logging.funs
#' @export
setGeneric("jlprint.error", function(jlogger, ...) standardGeneric("jlprint.error"))

#' @rdname logging.funs
#' @export
setGeneric("jlog.fatal", function(jlogger, ...) standardGeneric("jlog.fatal"))

#' @rdname logging.funs
#' @export
setGeneric("jlwrite.fatal", function(jlogger, ...) standardGeneric("jlwrite.fatal"))

#' @rdname logging.funs
#' @export
setGeneric("jlprint.fatal", function(jlogger, ...) standardGeneric("jlprint.fatal"))

setMethod("jlog.trace", "JLogger", jl.log.trace)
setMethod("jlwrite.trace", "JLogger", jl.write.trace)
setMethod("jlprint.trace", "JLogger", jl.print.trace)

setMethod("jlog.debug", "JLogger", jl.log.debug)
setMethod("jlwrite.debug", "JLogger", jl.write.debug)
setMethod("jlprint.debug", "JLogger", jl.print.debug)

setMethod("jlog.info", "JLogger", jl.log.info)
setMethod("jlwrite.info", "JLogger", jl.write.info)
setMethod("jlprint.info", "JLogger", jl.print.info)

setMethod("jlog.warn", "JLogger", jl.log.warn)
setMethod("jlwrite.warn", "JLogger", jl.write.warn)
setMethod("jlprint.warn", "JLogger", jl.print.warn)

setMethod("jlog.error", "JLogger", jl.log.error)
setMethod("jlwrite.error", "JLogger", jl.write.error)
setMethod("jlprint.error", "JLogger", jl.print.error)

setMethod("jlog.fatal", "JLogger", jl.log.fatal)
setMethod("jlwrite.fatal", "JLogger", jl.write.fatal)
setMethod("jlprint.fatal", "JLogger", jl.print.fatal)

jl.flush <- function(jlogger)
{
    lapply(jlogger$m.files, JLOGGER.flush)
    invisible()
}
#' Flushing buffers
#'
#' Flushes the current buffers.
#' @param jlogger JLogger object
#' @export
setGeneric("jlflush", function(jlogger) standardGeneric("jlflush"))
setMethod("jlflush", "JLogger", jl.flush)

ljlog.trace <- function(jlogger, ...) lapply(jlogger, jlog.trace, ...)
ljlwrite.trace <- function(jlogger, ...) lapply(jlogger, jlwrite.trace, ...)
ljlprint.trace <- function(jlogger, ...) lapply(jlogger, jlprint.trace, ...)

ljlog.debug <- function(jlogger, ...) lapply(jlogger, jlog.debug, ...)
ljlwrite.debug <- function(jlogger, ...) lapply(jlogger, jlwrite.debug, ...)
ljlprint.debug <- function(jlogger, ...) lapply(jlogger, jlprint.debug, ...)

ljlog.info <- function(jlogger, ...) lapply(jlogger, jlog.info, ...)
ljlwrite.info <- function(jlogger, ...) lapply(jlogger, jlwrite.info, ...)
ljlprint.info <- function(jlogger, ...) lapply(jlogger, jlprint.info, ...)

ljlog.warn <- function(jlogger, ...) lapply(jlogger, jlog.warn, ...)
ljlwrite.warn <- function(jlogger, ...) lapply(jlogger, jlwrite.warn, ...)
ljlprint.warn <- function(jlogger, ...) lapply(jlogger, jlprint.warn, ...)

ljlog.error <- function(jlogger, ...) lapply(jlogger, jlog.error, ...)
ljlwrite.error <- function(jlogger, ...) lapply(jlogger, jlwrite.error, ...)
ljlprint.error <- function(jlogger, ...) lapply(jlogger, jlprint.error, ...)

ljlog.fatal <- function(jlogger, ...) lapply(jlogger, jlog.fatal, ...)
ljlwrite.fatal <- function(jlogger, ...) lapply(jlogger, jlwrite.fatal, ...)
ljlprint.fatal <- function(jlogger, ...) lapply(jlogger, jlprint.fatal, ...)

setMethod("jlog.trace", "list", ljlog.trace)
setMethod("jlwrite.trace", "list", ljlwrite.trace)
setMethod("jlprint.trace", "list", ljlprint.trace)

setMethod("jlog.debug", "list", ljlog.debug)
setMethod("jlwrite.debug", "list", ljlwrite.debug)
setMethod("jlprint.debug", "list", ljlprint.debug)

setMethod("jlog.info", "list", ljlog.info)
setMethod("jlwrite.info", "list", ljlwrite.info)
setMethod("jlprint.info", "list", ljlprint.info)

setMethod("jlog.warn", "list", ljlog.warn)
setMethod("jlwrite.warn", "list", ljlwrite.warn)
setMethod("jlprint.warn", "list", ljlprint.warn)

setMethod("jlog.error", "list", ljlog.error)
setMethod("jlwrite.error", "list", ljlwrite.error)
setMethod("jlprint.error", "list", ljlprint.error)

setMethod("jlog.fatal", "list", ljlog.fatal)
setMethod("jlwrite.fatal", "list", ljlwrite.fatal)
setMethod("jlprint.fatal", "list", ljlprint.fatal)

ljlflush <- function(jlogger) lapply(jlogger, jlflush)
setMethod("jlflush", "list", ljlflush)

setMethod("jlog.trace", "NULL", function(jlogger, ...){})
setMethod("jlwrite.trace", "NULL", function(jlogger, ...){})
setMethod("jlprint.trace", "NULL", function(jlogger, ...){})

setMethod("jlog.debug", "NULL", function(jlogger, ...){})
setMethod("jlwrite.debug", "NULL", function(jlogger, ...){})
setMethod("jlprint.debug", "NULL", function(jlogger, ...){})

setMethod("jlog.info", "NULL", function(jlogger, ...){})
setMethod("jlwrite.info", "NULL", function(jlogger, ...){})
setMethod("jlprint.info", "NULL", function(jlogger, ...){})

setMethod("jlog.warn", "NULL", function(jlogger, ...){})
setMethod("jlwrite.warn", "NULL", function(jlogger, ...){})
setMethod("jlprint.warn", "NULL", function(jlogger, ...){})

setMethod("jlog.error", "NULL", function(jlogger, ...){})
setMethod("jlwrite.error", "NULL", function(jlogger, ...){})
setMethod("jlprint.error", "NULL", function(jlogger, ...){})

setMethod("jlog.fatal", "NULL", function(jlogger, ...){})
setMethod("jlwrite.fatal", "NULL", function(jlogger, ...){})
setMethod("jlprint.fatal", "NULL", function(jlogger, ...){})
setMethod("jlflush", "NULL", function(jlogger){})


#' Getting buffers
#'
#' Get the buffers a JLogger writes to
#' @param logger JLogger object
#' @return Vector of file paths. Empty character is the console
#' @export
get.logfiles <- function(logger) logger$m.files

#' Getting current logging level
#'
#' Gets the logging level of a JLogger
#' @param logger JLogger object
#' @return Logging level
#' @export
get.logging.level <- function(logger) logger$m.level

#' Setting buffers
#'
#' Sets the log files of JLogger
#' @param logger JLogger object
#' @param files Vector of file paths. Empty character is the console
#' @export
set.logfiles <- function(logger,
                         files)
{
    if(is.character(logger)) logger <- JLoggerFactory(logger)
    logger$m.files <- unique(files)
}

#' Adding new buffers
#'
#' Adds log files to the JLogger files list
#' @param logger JLogger object
#' @param files Vector of file paths. Empty character is the console
#' @export
add.logfiles <- function(logger,
                         files)
{
    if(is.character(logger)) logger <- JLoggerFactory(logger)
    set.logfiles(logger, c(logger$m.files, files))
}

#' Setting logging level
#'
#' Sets the logging level of a JLogger
#' @param logger JLogger object
#' @param level a JLogger level
#' @export
set.logging.level <- function(logger,
                              level)
{
    if(is.character(logger)) logger <- JLoggerFactory(logger)
    logger$m.level <- level
}


#' Setting prefix
#'
#' Sets the logger's prefix (string that is printed after the date)
#' @param logger JLogger object
#' @param prefix a string
#' @export
set.logger.prefix <- function(logger,
                              prefix)
{
    if(is.character(logger)) logger <- JLoggerFactory(logger)
    logger$m.prefix <- prefix
}

###### utility function for logging more information ####

## Returns the name of the function it was called from
fname <- function(offset = 1)
{
    as.character(sys.calls()[[sys.nframe() - offset]])[1]
}

jlfname <- function() fname(8)
