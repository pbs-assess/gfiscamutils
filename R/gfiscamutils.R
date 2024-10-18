#' gfiscamutils package
#'
#' See the README on
#' \href{https://github.com/pbs-assess/gfiscamutils#readme}{GitHub}
#'
#' @docType package
#' @name gfiscamutils

#' @import coda
#' @import cowplot
#' @import dplyr
#' @import forecast
#' @import gfdata
#' @import gfutilities
#' @import ggplot2
#' @import grid
#' @import here
#' @import knitr
#' @import purrr
#' @import readr
#' @import reshape2
#' @rawNamespace import(scales, except = c(col_factor, discard))
#' @import sp
#' @rawNamespace import(testthat, except = c(edition_get, is_null, local_edition, matches))
#' @import usethis
#' @import xtable
#' @import zoo
#' @importFrom coda heidel.diag mcmc geweke.diag spectrum0.ar
#' @importFrom cowplot ggdraw draw_label
#' @importFrom csasdown csas_table
#' @importFrom hake insert_row
#' @importFrom graphics barplot text
#' @importFrom grDevices colorRampPalette
#' @importFrom lubridate year
#' @importFrom patchwork wrap_plots
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom readr read_csv
#' @importFrom rlang .data sym
#' @importFrom stats acf as.formula cor.test quantile
#' @importFrom stringr str_wrap
#' @importFrom tibble enframe
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom utils capture.output

NULL

#' The iSCAM model class
#'
#' @description
#' This class defines a list as being input and output for a single iSCAM model.
#' The list must have the 'model_desc' attribute to qualify.
#' Models are assigned this class in [load_iscam_files()]
"mdl_cls"

#' The iSCAM model list class
#'
#' @description
#' This class defines a list as being a list of one or more iSCAM models.
#' Each element of the list must have class [mdl_cls] to qualify.
#' Model lists are assigned this class in [model_setup()]
"mdl_lst_cls"

#' The iSCAM model group class
#'
#' @description
#' This class defines a list as being a group of one or more iSCAM model lists.
#' Each element of the list must have class [mdl_lst_cls] to qualify.
#' Model lists are assigned this class in [model_setup()]
"mdl_grp_cls"

#' The iSCAM model palette to use for plots by default
#'
#' @description
#' This variable is the default palette to use for all plots in this
#' package. It must be defined as one of the list given by
#' [RColorBrewer::brewer.pal.info]
"iscam_palette"

#' The DOS append to log operator string
#'
#' @description
#' Used to append output to a log file when running DOS bat files from  within
#' R
"dos.append.to.log"

#' The DOS pipe to log operator string
#'
#' @description
#' Used to pipe output to a log file when running DOS bat files from  within
#' R
"dos.pipe.to.log"

#' The Linux (bash) append to log operator string
#'
#' @description
#' Used to append output to a log file when running bash scripts from  within
#' R
"linux.append.to.log"

#' The Linux (bash) pipe to log operator string
#'
#' @description
#' Used to pipe output to a log file when running bash scripts from  within
#' R
"linux.pipe.to.log"

#' The name of the log file used when running scripts
#'
#' @description
#' The name of the log file used when running scripts
"log.file"

#' The name of the ISCAM catch file (csv)
#'
#' @description
#' The name of the ISCAM catch file (csv)
"mcmc.catch.file"

#' The name of the ISCAM residuals file (csv)
#'
#' @description
#' The name of the ISCAM residuals file (csv)
"mcmc.index.standardized.resids.file"

#' The name of the ISCAM female selectivity file (csv)
#'
#' @description
#' The name of the ISCAM female selectivity file (csv)
"mcmc.sel.female.file"

#' The name of the ISCAM male selectivity file (csv)
#'
#' @description
#' The name of the ISCAM male selectivity file (csv)
"mcmc.sel.male.file"

#' The name of the ISCAM stock recruitment file (csv)
#'
#' @description
#' The name of the ISCAM stock recruitment file (csv)
"mcmc.sr.file"


#' The name of the ISCAM model class
#'
#' @description
#' The name of the ISCAM model class
"mdl_cls"

#' The name of the ISCAM model class
#'
#' @description
#' The name of the ISCAM model class
"model.class"

#' The name of the ISCAM model class
#'
#' @description
#' The name of the ISCAM model class
"mdl_lst_cls"

#' The name of the ISCAM model class
#'
#' @description
#' The name of the ISCAM model class
"model.lst.class"

#' A vector of ADMB output files for ISCAM
#'
#' @description
#' A vector of ADMB output files for ISCAM
"output.files"

#' Name of the model rdata file
#'
#' @description
#' Name of the model rdata file
"rdata.file"

#' Name of the retrospectives directory
#'
#' @description
#' Name of the retrospectives directory
"retro.dir"

# from: https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
# quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") utils::globalVariables(c("."))
