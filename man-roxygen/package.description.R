#' @details JLoggers have several logging levels ranking as such: TRACE, DEBUG, INFO, WARNING, ERROR, FATAL. Establishing the logging level to a given rank will turn off messages of lower rank and print all other messages.\cr
#' For example setting the level to INFO will print infos, warnings, errors and fatal messages.
#' A good practice to use appropriate logging level could be the following. Trace messages are there to fully trace each action a complex algorithm takes. This is for development purposes.\cr
#' Debug messages are here for testing when a programs runs but breaks often. It allows the developper to have a good idea though not exhaustive of the actions the program took.\cr
#' Info messages are messages that you will want the end user to see. They should give information about the current state of the program.\cr
#' Warnings indicate that the program is facing an edge case, had to make assumptions about some data, had to convert some data, had to skip or make some extra steps, etc...\cr
#' Warnings can occur without impacting the functionning of the program.\cr
#' Error messages are to be put when the program is out of its range of function. It does not necessarily mean that the program cannot continue but it can mean that a subtask had to be aborted due to a dubious state.\cr
#' Fatal messages are the highest level of logging and they indicate that the program is about to stop due to an error it cannot recover from and anything more severe.\cr
#' JLoggers offer several ways of messaging. \code{jlog*} is a simple wrapper around \code{cat}, \code{jlwrite*} wraps \code{write} and \code{jlprint*} wrap \code{print}. Each function categories can be use to handle different types of data.\cr
