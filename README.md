
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/muschellij2/RBioFormats.svg?branch=master&svg=true)](https://ci.appveyor.com/project/muschellij2/RBioFormats)
[![Travis build
status](https://travis-ci.com/muschellij2/RBioFormats.svg?branch=master)](https://travis-ci.com/muschellij2/RBioFormats)

## Installation

First, make sure you have
[JDK](http://www.oracle.com/technetwork/java/javase/downloads/index.html)
1.7 or higher installed. To install *RBioFormats* use the `biocLite`
installation script in order to resolve the dependency on the
Bioconductor package
*[EBImage](http://biocondcutor.org/packages/EBImage)*.

``` r
source("https://bioconductor.org/biocLite.R")
biocLite("aoles/RBioFormats") # You might need to first run `install.packages("devtools")`
```

### Mac OS X

Mac OS comes with a legacy Apple Java 6. In order to use *RBioFormats*,
you will need to update your Java installation to a newer version
provided by Oracle.

1.  Install [Oracle
    JDK](http://www.oracle.com/technetwork/java/javase/downloads/index.html).

2.  Update R Java configuration by executing from the command line (you
    might have to run it as a super user by prepending `sudo` depending
    on your installation).

<!-- end list -->

    R CMD javareconf

3.  Re-install *rJava* from sources in order to properly link to the
    non-system Java installation.

<!-- end list -->

``` r
install.packages("rJava", type="source")
```

You can verify your configuration by running the following commands.
This should return the Java version string corresponding to the one
downloaded and installed in step 1.

``` r
library(rJava)
.jinit()
.jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
## [1] "1.8.0_112-b16" 
```

## Documentation

For example use, see the [package
vignette](https://rawgit.com/aoles/RBioFormats/master/vignettes/RBioFormats.html).

## FAQ

See my [answers on Stack
Overflow](http://stackoverflow.com/search?q=user:A2792099+rbioformats).

## Caveats

### The `java.lang.OutOfMemoryError` error

If you get the `java.lang.OutOfMemoryError: Java heap space` error, try
increasing the maximum heap size by supplying the -Xmx parameter before
the Java Virtual Machine is initialized. For example, use

``` r
options( java.parameters = "-Xmx4g" )
library( "RBioFormats" )
```

to override the default setting and assign 4 gigabytes of heap space to
the Java environment.

Information about the current Java heap space limit can be retrieved by
`checkJavaMemory()`.

### Use with BiocParallel

Each R process needs a separate JVM instance. For this, load the package
in the parallelized function, e.g.,

``` r
bplapply (files, function(f) {
  library(RBioFormats)
  ...
})
```
