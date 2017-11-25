## Thanks to: http://stackoverflow.com/questions/5947742/how-to-change-the-output-color-of-echo-in-linux

#' @rdname logging.levels
#' @export
JLOGGER.COLORS <- c(TRACE = '\33[0;35m',
                    DEBUG = '\33[0;34m',
                    INFO = '\33[1;32m',
                    WARNING = '\33[0;33m',
                    ERROR = '\33[1;31m',
                    FATAL =  '\33[1;31m',
                    BLACK = '\33[0;30m',
                    RED = '\33[0;31m',
                    GREEN = '\33[0;32m',
                    YELLOW = '\33[0;33m',
                    BLUE = '\33[0;34m',
                    MAGENTA = '\33[0;35m',
                    CYAN = '\33[0;36m',
                    WHITE = '\33[0;37m',
                    BRIGHT.BLACK = '\33[1;30m',
                    BRIGHT.RED = '\33[1;31m',
                    BRIGHT.GREEN = '\33[1;32m',
                    BRIGHT.YELLOW = '\33[1;33m',
                    BRIGHT.BLUE = '\33[1;34m',
                    BRIGHT.MAGENTA = '\33[1;35m',
                    BRIGHT.CYAN = '\33[1;36m',
                    BRIGHT.WHITE = '\33[1;37m',
                    ## shortcuts
                    N = '\33[0;30m',
                    R = '\33[0;31m',
                    G = '\33[0;32m',
                    Y = '\33[0;33m',
                    B = '\33[0;34m',
                    M = '\33[0;35m',
                    C = '\33[0;36m',
                    W = '\33[0;37m',
                    BN = '\33[1;30m',
                    BR = '\33[1;31m',
                    BG = '\33[1;32m',
                    BY = '\33[1;33m',
                    BB = '\33[1;34m',
                    BM = '\33[1;35m',
                    BC = '\33[1;36m',
                    BW = '\33[1;37m')

## Coloring
#' Coloring a string
#' 
#' Colors a word when printing
#' @param w string to color
#' @param color ASCII code for the color https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
#' @export
color.string <- function(w,
                         color)
{
    if(color %in% names(JLOGGER.COLORS))
        color <- get.color(color)
    paste(c(color, paste(w, collapse = " "), JLOGGER.STYLE.COLORS.RESET), collapse = '')
}

#' Colors
#'
#' Returns the ASCII code of a given
#' @param color.name One of (BRIGHT)?.?(RED, GREEN, YELLOW, BLUE, MAGENTA, CYAN, WHITE)
#' @export
get.color <- function(color.name)
{
    if(!missing(color.name))
        JLOGGER.COLORS[[color.name]]
    else
    {
        cat('available colors are:\n')
        lapply(setdiff(names(JLOGGER.COLORS), JLOGGER.LEVELS),
               function(x) cat(color.string(x, JLOGGER.COLORS[[x]]), '\n'))
        cat('\n')
    }
}

#' @export
setGeneric("%c%", function(word, color)
{
    sc <- substitute(color)
    if(is.name(sc))
        color <- as.character(sc)
    color.string(word, color)
})
