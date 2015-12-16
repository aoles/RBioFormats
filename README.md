## Installation

```
# install.packages("devtools")
devtools::install_github("aoles/RBioFormats")
```

## Caveats

### The `java.lang.OutOfMemoryError` error

If you get the `java.lang.OutOfMemoryError: Java heap space` error, try increasing the maximum heap size by supplying the -Xmx parameter before the Java Virtual Machine is initialized. For example, use

    options( java.parameters = "-Xmx4g" )
    library( "RBioFormats" )

to override the default setting and assign 4 gigabytes of heap space to the Java environment.

### Use with BiocParallel

Each R process needs a separate JVM instance. For this, load the package in the parallelized function, e.g.,

    bplapply (files, function(f) {
      library(RBioFormats)
      ...
    })
    