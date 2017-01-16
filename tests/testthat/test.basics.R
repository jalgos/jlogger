library(jlogger)

test_that("Logging function name works",
{
    ftest <- function(logger = jlogger::JLoggerFactory('test'))
    {
        jlog.debug(logger, "Where am I?", file.name(), function.name())
        print.fname(logger, TRUE)
        jlog.debug(logger, "Hello worms!")
    }
    
    ftest()
})

