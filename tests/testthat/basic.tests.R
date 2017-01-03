ftest <- function(logger = jlogger::JLoggerFactory('test'))
{
    jlogger::jlog.debug(logger, "Where am I?", jlogger::file.name(), jlogger::function.name())
}
