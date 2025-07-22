#' gfiscamutils: A package for loading gfiscam model output, with plotting
#'   and table-making functions
#' @name gfiscamutils
"_PACKAGE"
NULL

#' iscam model
#'
#' Class definition
"model.class"

#' iscam models list
#'
#' Class definition
"model.lst.class"

#' iscam model RData file name
#'
#' character
"rdata.file"


#' iscam and ADMB output file patterns
#'
#' character vector
"output.files"

#' file name for log file piped to using shell call to iscam program
#'
#' character
"log.file"

#' Retrospective directory name
#'
#' character
"retro.dir"

#' string to append to a program shell call to pipe the output of stdout and stderr
#' to a file in DOS
#'
#' character
"dos.pipe.to.log"

#' string to append to a program shell call to append the output of stdout and stderr
#' to a file in DOS
#'
#' character
"dos.append.to.log"

#' string to append to a program shell call to pipe the output of stdout and stderr
#' to a file in Linux
#'
#' character
"linux.pipe.to.log"

#' string to append to a program shell call to append the output of stdout and stderr
#' to a file in Linux
#'
#' character
"linux.append.to.log"
