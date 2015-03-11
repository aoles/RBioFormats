## Installation

```
# install.packages("devtools")
devtools::install_github("aoles/RBioFormats")
```

## Caveats

If you get the `java.lang.OutOfMemoryError: Java heap space` error, try increasing the maximum heap size by supplying the -Xmx parameter before the Java Virtual Machine is initialized. For example, use

    options( java.parameters = "-Xmx4g" )
    library( "RBioFormats" )

to override the default setting and assign 4 gigabytes of heap space to the Java environment.