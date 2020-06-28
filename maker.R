## Creates all the pdf files, correctly sized; type either 'make' or '
## R CMD BATCH maker.R' at the command line.  Alternatively, in an R
## session, type 'source("maker.R")'.  This is modelled on maker.R in
## the schwarzschild package.


source("usefulfuncs.R")
source("longcope.R")

`do` <- function(command, basename){
    ##  do() is just a convenience wrapper to create a pdf

  if(missing(basename)){
    basename <- sub('\\(.*$','',command)  # up to but not including brackets
}

    pdf(file=paste(basename,"pdf",sep="."), height=9,width=9)
    eval(parse(text=command))
    dev.off()
}

do("longcope()")
do("longcope_inverse()")
