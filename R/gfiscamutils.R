#' gfiscamutils package
#'
#' See the README on
#' \href{https://github.com/pbs-assess/gfiscamutils#readme}{GitHub}
#'
#' @docType package
#' @name gfiscamutils

#' @importFrom rlang .data
#' @importFrom stats quantile
#' @importFrom utils capture.output
#'
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

# from: https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
# quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") utils::globalVariables(c("."))
