<!-- badges: start -->
  [![Travis build status](https://travis-ci.com/mayachen19/stat302package.svg?branch=master)](https://travis-ci.com/mayachen19/stat302package)
  [![Codecov test coverage](https://codecov.io/gh/mayachen19/stat302package/branch/master/graph/badge.svg)](https://codecov.io/gh/mayachen19/stat302package?branch=master)
  <!-- badges: end -->

# INSTALLATION

To download the STAT302Project2 use the code below
```{r}
#install.packages("devtools")
devtools::install_github("https://github.com/mayachen19/stat302package")
library(stat302package)
```

# USE

The vignette shows a tutorial on all functions supplied by the package for
statistical calculation and prediction. You can see the vignette by using
the following code.

```{r}
#install.packages("devtools")
devtools::install_github("mayachen19/stat302package", build_vignette = TRUE,
                          build_opts = c())
#Use this to view the vignette in the STAT302Project2 help
help(package = "stat302package", help_type = "html")
#Use this to view the vignette as a html file
utils::browseVignettes(package = "stat302package")
