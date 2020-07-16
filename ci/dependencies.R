.libPaths("lib")


`.` <- list
jspackages <- .("utils" = .(c('jconfig', version = "v1.0.2")))

jsroot::dependencies(jspackages = jspackages,
                     cran.packages = .(
                         'RJSONIO'
                         #'track',
                         #'uuid'
                     ),
                     quiet = FALSE)

