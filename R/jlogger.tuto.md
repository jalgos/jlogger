# JLOGGER

Gracefully handling logging is key when developing big projects
Logging has several purposes:
- outputing the result of a computation
- printing warning when the program does something the user does not necessarily expect
- printing detailed explanations of a program's error/ crash
- Providing the user with the current state of computation (e.g. "step i over n in function f"
- Helping debugging by tracing the successive calls and variable values to identify a bug

Furthermore we may want to output the logs either directly to the console or write them in files to be stored.

R has a few functions to handle printing objects, namely *print*, *message* and *cat*. However they have limited functionality.
That is why we built the *JLogger* class.
JLogger is built on top of cat and allows one to output to several files or to the console easily. JLogger statements can be turn on and off as it implements the well known concept of logging level (TRACE, DEBUG, INFO, WARNING, ERROR, FATAL)

JLogger are R5 objects. This is one of the few occurrences of R5 objects as their use is strongly discouraged.
Mutable objects where chosen here to persist changes made locally to a logger. For example if a condition is met in a function the logging level or logging file of a logger can be changed affecting the rest of the program.

## I) Use of logging

Logging should be used to give the user key information about the current of state of computation.
It is really important to print information about the size of the inputs, the current step in a recursive algorithm, measures about the outputs, edge cases for example.

Logging should be implemented from the start of the project.


It is very common, especially early in the development of a project, that we need to trace a computation in its tiniest details. This situation arises for debugging purposes mostly.

The way one will go about it would be to put *print* / *cat* statements in points of interest.

Let say we want to debug the following function we can insert a few cat statements

```r
f <- function()
{
    X = rnorm(1000)
    cat("Maximum value of X is:", max(X), "\n")
    D = rep(0, length(X) + 1)
    for(i in 1:length(X))
    {
        cat("Value of X at step i:", X[i], "\n")
        D[i + 1] = cos(D[i] + X[i])
    }
    cat("Median of D is:", median(D), "\n")
    sum(D)
}
f()
```
We will obtain a lot of information (we did not display it here) about the computation and monitor it easily now.
The problem is when we fixed the function the call in the for loop is of no interest but yet will take most of the logging space, making it hard to find relevant information for further debugging.

A solution would be to remove or comment the call to *cat* in the loop and proceed with the new version.
However if in the future our function gives us an unexpected output that requires an in-depth look at the function's computation we would have to put the statement back.
It can be easy to handle for a small number of functions but becomes a nightmare as the project gets bigger.

JLogger is a perfect fit here. Call to the logger can be turned on and off according to the logging level of the logger

## II) Using JLogger

First you need to source the file "jlogger.R"
The file can be found in the git project 'utils' along with other utility R functions.

 `install.git('git@datasaiyan.com:/home/git/repos/utils/util.R.git')`

Jlogger requires the file "util.R" as well (it uses function 'mgrep') :


```r
install.git('git@www.datasaiyan.com:utils/jsutils.git')
```



JLogger objects are created/ retrieved through the JLoggerFactory
Each logger is identified by a string id:


```r
logger = JLoggerFactory("jalgos")
```

If the logger named `jalgos` was not created before the above call will create a new instance. Otherwise it will return the object previously created
This makes it easy to use the logger from within any function without having to pass it around.

JLoggers can filter which statements they output to the log file (or console). It is achieved through the logging level
6 levels are implemented

```r
JLOGGER.LEVELS
```

```
## [1] "TRACE"   "DEBUG"   "INFO"    "WARNING" "ERROR"   "FATAL"
```
The above vectors defines the logging priority. For example if a logger level is set to 'JLOGGER.TRACE' any message will be output to the log file.
Conversely if it is set to 'JLOGGER.FATAL' only "fatal" messages will be print out.
Let's illustrate this behavior:


```r
logger$m.level = JLOGGER.INFO
```

The syntax for logging a message is:
`jlog.$priority($logger, $object1, $object2, ...., $objectn)`
The logger will check if the call has a priority greater than its priority and output a message that looks like
`date time loglevel prefix: $object1, $object2, ...., $objectn"`
No need to give the eol character or separate objects with spaces

```r
jlog.info(logger, "This is a test")
```

```
## 2016-06-16 16:34:37 INFO jalgos : This is a test
```
The logger gives a bit more context that can be useful in debugging or monitoring execution.

Since the logging level is set to info the following message won't be logged

```r
jlog.trace(logger, "Tracing")
```

```
## NULL
```


```r
my_heavy_comp <- function()
{
    logger = JLoggerFactory("jalgos")
    jlog.info(logger, "This computation is gonna take a long time")
    X = rnorm(1000)
    jlog.debug(logger, "Max of X:", max(X))
    D = 0
    for(x in X)
    {
        jlog.trace(logger, "I want to know exactly what is going on at each step of the loop:", "x:", x, "D:", D)
        D = sin(x + D)
        if(abs(D) < .01) jlog.warn(logger, "D is small:", D, ", you should know")
    }
    jlog.info(logger, "Computation is over. Return value:", D)
    D
}
```
Given this heavy function, setting logger level to info

```r
logger = JLoggerFactory("jalgos")
logger$m.level = JLOGGER.INFO
my_heavy_comp()
```

```
## 2016-06-16 16:34:37 INFO jalgos : This computation is gonna take a long time 
## 2016-06-16 16:34:37 WARNING jalgos : D is small: -0.009695392 , you should know 
## 2016-06-16 16:34:37 WARNING jalgos : D is small: 0.009678441 , you should know 
## 2016-06-16 16:34:37 WARNING jalgos : D is small: -0.005143874 , you should know 
## 2016-06-16 16:34:37 WARNING jalgos : D is small: 0.001947735 , you should know 
## 2016-06-16 16:34:37 WARNING jalgos : D is small: -0.005187589 , you should know 
## 2016-06-16 16:34:37 WARNING jalgos : D is small: -0.003408209 , you should know 
## 2016-06-16 16:34:38 WARNING jalgos : D is small: -0.006916986 , you should know 
## 2016-06-16 16:34:38 INFO jalgos : Computation is over. Return value: 0.239519
```

```
## [1] 0.239519
```

Now to warning:

```r
logger$m.level = JLOGGER.WARN
my_heavy_comp()
```

```
## 2016-06-16 16:34:38 WARNING jalgos : D is small: -0.001887302 , you should know 
## 2016-06-16 16:34:38 WARNING jalgos : D is small: 0.006683671 , you should know 
## 2016-06-16 16:34:38 WARNING jalgos : D is small: 0.0005033445 , you should know 
## 2016-06-16 16:34:38 WARNING jalgos : D is small: 0.008412488 , you should know 
## 2016-06-16 16:34:38 WARNING jalgos : D is small: -0.008596674 , you should know 
## 2016-06-16 16:34:38 WARNING jalgos : D is small: -0.006301923 , you should know 
## 2016-06-16 16:34:38 WARNING jalgos : D is small: -0.007640209 , you should know 
## 2016-06-16 16:34:38 WARNING jalgos : D is small: -0.004352238 , you should know 
## 2016-06-16 16:34:38 WARNING jalgos : D is small: -0.001684934 , you should know
```

```
## [1] 0.9821325
```

And now to trace:

```r
logger$m.level = JLOGGER.TRACE
my_heavy_comp()
```
In this last case, we will get an awful lot of content we did not display.
This example illustrates how messaging can be turned on and off easily
One should not refrain from using the logger. Appropriately logging relevant information really improves post-mortem debugging and greatly speeds up development

We provide two other ways of logging objects: `jlprint.*` and `jlwrite.*`.

`jlprint` is a wrapper around `print` and `jlwrite` is a wrapper around `write.table`. This allows you to log more complex objects that are not handled by cat.



```r
m = matrix(rnorm(100), 10, 10)
jlog.info(logger, m) ## Looks disastrous
```

```
## 2016-06-16 16:34:38 INFO jalgos : 0.6018115 -0.8695245 0.8760098 0.9234286 -0.3904474 -0.1624867 0.01771718 -0.389735 0.5755072 0.5867506 1.731458 -0.5268292 -0.7821828 -0.5755612 -0.4201484 0.2672047 -0.7302498 2.699499 0.9471331 0.1083734 -2.665278 -1.74357 -0.6895787 -0.2500003 0.721606 -1.7938 0.3548181 -1.154617 0.1133798 -0.02067213 1.372074 -0.316874 1.259541 -0.2099376 0.4379518 -1.288953 -0.559455 1.220172 0.9075606 0.1796256 -0.2259545 0.7637832 1.498003 -1.724214 -0.4744248 -0.5790666 1.391533 0.8452129 0.4888359 2.057598 0.9083759 0.2336213 1.44219 -2.010289 -0.3528223 -1.192101 -0.4859984 1.529827 0.5599454 -0.5855541 -0.6015762 0.9479154 -1.5338 -0.9292362 -0.4557787 -1.421487 -0.7084473 -0.624489 0.1828879 -0.7901241 2.964639 0.2778364 -0.1151965 -1.709949 -0.1322515 -1.021101 -0.8442775 -1.984643 0.9971386 1.458086 0.2818974 0.5778441 -1.927722 0.2349527 -1.530362 0.4280039 1.158254 -0.3236403 0.8761156 -0.5445655 1.13177 -1.182487 1.460312 0.02912489 -1.642304 -0.2487976 -1.320875 1.171583 0.4741245 -0.2494998
```

```r
jlprint.info(logger, m) ## Looks good
```

```
## 2016-06-16 16:34:38 INFO jalgos : 
##              [,1]       [,2]        [,3]       [,4]       [,5]       [,6]
##  [1,]  0.60181154  1.7314584 -2.66527751  1.3720738 -0.2259545  0.9083759
##  [2,] -0.86952446 -0.5268292 -1.74357044 -0.3168740  0.7637832  0.2336213
##  [3,]  0.87600976 -0.7821828 -0.68957870  1.2595414  1.4980026  1.4421897
##  [4,]  0.92342861 -0.5755612 -0.25000032 -0.2099376 -1.7242145 -2.0102894
##  [5,] -0.39044740 -0.4201484  0.72160602  0.4379518 -0.4744248 -0.3528223
##  [6,] -0.16248669  0.2672047 -1.79379957 -1.2889532 -0.5790666 -1.1921008
##  [7,]  0.01771718 -0.7302498  0.35481811 -0.5594550  1.3915326 -0.4859984
##  [8,] -0.38973497  2.6994991 -1.15461666  1.2201715  0.8452129  1.5298274
##  [9,]  0.57550715  0.9471331  0.11337985  0.9075606  0.4888359  0.5599454
## [10,]  0.58675062  0.1083734 -0.02067213  0.1796256  2.0575981 -0.5855541
##             [,7]       [,8]       [,9]       [,10]
##  [1,] -0.6015762  2.9646388  0.2818974  1.13177030
##  [2,]  0.9479154  0.2778364  0.5778441 -1.18248731
##  [3,] -1.5337997 -0.1151965 -1.9277218  1.46031247
##  [4,] -0.9292362 -1.7099492  0.2349527  0.02912489
##  [5,] -0.4557787 -0.1322515 -1.5303618 -1.64230370
##  [6,] -1.4214874 -1.0211012  0.4280039 -0.24879764
##  [7,] -0.7084473 -0.8442775  1.1582544 -1.32087524
##  [8,] -0.6244890 -1.9846429 -0.3236403  1.17158260
##  [9,]  0.1828879  0.9971386  0.8761156  0.47412452
## [10,] -0.7901241  1.4580858 -0.5445655 -0.24949975
```

```r
d = data.frame(i = 1:10, x = rnorm(10))
jlwrite.info(logger, d) ## Looks good too
```

```
## 2016-06-16 16:34:38 INFO jalgos : 
## "i" "x"
## "1" 1 0.273013907082653
## "2" 2 -2.31410763364822
## "3" 3 0.0486807150724941
## "4" 4 -0.0458313850877191
## "5" 5 -0.340298072676317
## "6" 6 1.04791815672035
## "7" 7 -0.692824618856236
## "8" 8 1.54332670925805
## "9" 9 -1.29182678052621
## "10" 10 -0.108131343447086
```

## III) Configuring JLogger

### Logging levels

Logging levels can be set directly:

```r
logger = JLoggerFactory("jalgos")
logger$m.level = JLOGGER.INFO
```

It can also be set through the function `set.logging.level`. `set.logging.level` handles logger objects and logger names :

```r
set.logging.level(logger, JLOGGER.INFO)
set.logging.level("jalgos", JLOGGER.INFO)
```

### Output files
The logger can output its messages to one or several files

The logging files can be access and overriden through the member `m.files` :


```r
logger$m.files
```

```
## [1] ""
```

This field is a character vector of the log file names.
The only exception is that the filename "" will log to the console. Log files can be set with `set.logfiles` and `add.logfiles`.

```r
set.logfiles(logger, #logger can be an object or a character string
             c("test.log"))
logger$m.files
```

```
## [1] "test.log"
```

```r
jlog.info(logger, "Test") ## Output to "test.log"
add.logfiles(logger, #DITTO
             "")
logger$m.files
```

```
## [1] "test.log" ""
```

```r
jlog.info(logger, "Test 2") ## Output to both "test.log" and the console
```

```
## 2016-06-16 16:34:38 INFO jalgos : Test 2
```
The logger will automatically append the new messages to the files.

logfiles can be flushed with `jlflush` function :

```r
jlflush(logger) #Flushing console isn't relevant, 'test.log' will however be flushed
```

### Prefix
The prefix is the string printed right after the date. It can be set directly through the member `logger$m.prefix` :

```r
logger$m.prefix = "Michael Jackson"
jlog.info(logger, "is bad")
```

```
## 2016-06-16 16:34:38 INFO Michael Jackson : is bad
```

Logger can be fully configured at inception. `JLoggerFactory` accepts additional arguments.
Namely `files`, `prefix` defaulting to the logger's name, `level`, etc.

```r
my_logger <- JLoggerFactory(name = "my logger",
                            files = c("", "file1.log", "file2.log"),
                            prefix = "make it funky",
                            level = JLOGGER.TRACE)
my_logger
```

```
## JLogger name: my logger prefix: make it funky level: TRACE files: *console* file1.log file2.log
```

```r
jlog.trace(my_logger, "yaay!")
```

```
## 2016-06-16 16:34:38 TRACE make it funky : yaay!
```

The default level is `JLOGGER.DEFAULT.LEVEL`

```r
JLOGGER.DEFAULT.LEVEL
```

```
## [1] 2
```

If changed the next jlogger instantiated without a specified level will be set to this level

```r
JLOGGER.DEFAULT.LEVEL = JLOGGER.ERROR
errlog = JLoggerFactory(name = "err logger")
errlog$m.level
```

```
## [1] 5
```

### Turning off logging completely
Logging can be turned off by setting a logging level higher than the highest logging level available. The highest level for now is JLOGGER.FATAL

```r
logger$m.level = JLOGGER.FATAL + 1L
jlog.fatal(logger, "1 = 2") # won't print
```

```
## NULL
```
The `jlog` functions accept `NULL` as input. In this case no message is printed.


```r
jlog.info(NULL, "1 != 1") ## Nothing
```

```
## NULL
```
It makes it easy to turn off all messaging without having to handle logging levels.


### Clearing registered loggers.
You can clear all created logger with `JLoggerReset`


```r
JLoggerReset() ## subsequent calls to JLoggerFactory will create a new instance
```

### Jlogger configuration
JLoggers can be configured through a config list. Logger configs are used to automatically configure the logging level.

You need to specify one entry by logging level. You don't need to specify all the levels. Each entry refers to a set of regular expressions.

Example:

```r
lconf <- list(ERROR = "err.*",
              INFO = c("jalgos", "inf.*"),
              TRACE = c("tracer", "overlogger"))
```
Now by setting the `logconfig` argument in `JLoggerFactory` to `lconf`, the name will be matched against the different regular expression and in case there is a match the logging level will be set to the corresponding level.

If a name doesn't match any regex in the config, the logger will be quiet for all levels.


```r
err_log <- JLoggerFactory(name = "err logger",
                          logconfig = lconf)

err_log ## "err logger"  matches "err.*" hence err_log's level is set to ERROR
```

```
## JLogger name: err logger prefix: err logger level: ERROR files: *console*
```

```r
jalgos_logger <- JLoggerFactory(name = "jalgos",
                          logconfig = lconf)
jalgos_logger ## "jalgos" matches an entry for INFO
```

```
## JLogger name: jalgos prefix: jalgos level: INFO files: *console*
```
