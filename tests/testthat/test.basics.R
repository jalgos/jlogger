library(jlogger)
context("Jlogger functions")

# test_that("Logging function name works",
# {
#     ftest <- function(logger = jlogger::JLoggerFactory('test'))
#     {
#         jlog.debug(logger, "Where am I?", file.name(), function.name())
#         print.fname(logger, TRUE)
#         jlog.debug(logger, "Hello worms!")
#     }
#     ftest()
# })

rotate <- function(logger = jlogger::JLoggerFactory('test'), filename = "log.file")
{
    if (file.exists(filename))
        file.remove(filename)
    set.logfiles(logger, filename)
    set.logger.filesize(logger, 500L)
    i <- 0
    while (i < 10)
    {
        jlog.info(logger, "12345")
        i <- i + 1
    }
}

test_that("Log Rotate functions",
{
    rotate()
    expect_equal(file.size("log.file"), NA_integer_)
})