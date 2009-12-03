# Stuff to do on loading the package
# Mark van der Loo 13.11.2009


.First.lib <- function(libname, pkgname)
{
  cat(paste(libname, "by Mark van der Loo\n"))
  cat("This package comes without any warranty and is free\n")
  cat("to use. If you use this package, please refer to\n\n")
  cat("@article{loo:2009,  
       author = {M.P.J. van der Loo},
       title  = {An outlier detection method for economic data},
       journal= {Submitted to The Journal of Official Statistics},
       year = {2009},
       pages = {??-??}
       }\n")
}
