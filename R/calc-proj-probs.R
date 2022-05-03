#' Create decision table from iSCAM MCMC model output
#'
#' @description
#' Extract and calculate probabilities from the iscam projection model
#'
#' @details Extract and calculate probabilities from the projection model.
#'   Used for decision tables in the document (see make.decision.table())
#'   in tables-decisions.r
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param fixed_cutoffs A vector of catch cutoffs to use in decision tables
#'
#' @return A data frame which has its names formatted for latex
#' @export
calc_proj_probs <- function(model,
                            fixed_cutoffs){

  proj <- model$proj
  tac <- sort(unique(proj$TAC))
  browser()
  p <- proj$ctl.options
  s_yr <- p[rownames(p) == "syrmeanm", 1]
  e_yr <- p[rownames(p) == "nyrmeanm", 1] + 2
  e_yr_1 <- e_yr - 1
  e_yr_2 <- e_yr - 2
  fc <- fixed_cutoffs
  proj_dat <- data.frame()
  for(t in 1:length(tac)){
    d <- proj[proj$TAC == tac[t], ]
    d <- mcmc_thin(d, burnin, thin)
    n.row <- nrow(d)
    k <- c(tac[t] * 1000,
           length(which(d[,paste0("B", e_yr_1)] < d$X03B0)) / n.row,
           median(d[,paste0("B", e_yr_1)] / d$X03B0),
           length(which(d[,paste0("B", e_yr_1)] < (0.6 * d$B0))) / n.row)
    #length(which(d[,paste0("B", e_yr_1)] < d$X09B0)) / n.row,
    #median(d[,paste0("B", e_yr_1)] / d$X09B0))

    if(t == 1){
      col_names <- c(latex.mlc(c(e_yr_1,
                                 "TAC (t)")),
                     latex.mlc(c(paste0("P(SB_{",
                                        e_yr_1,
                                        "}<"),
                                 "0.3SB_0)"),
                               math.bold = TRUE),
                     latex.mlc(c(paste0("Med(SB_{",
                                        e_yr_1,
                                        "}/"),
                                 "0.3SB_0)"),
                               math.bold = TRUE),
                     latex.mlc(c(paste0("P(SB_{",
                                        e_yr_1,
                                        "}<"),
                                 "0.6SB_0)"),
                               math.bold = TRUE))
    }
    if(which.model == 2){
      k <- c(k,
             length(which(d[,paste0("B", e_yr_1)] < fc[which.stock])) / n.row,
             median(d[,paste0("B", e_yr_1)] / fc[which.stock]))
      if(t == 1){
        col_names <- c(col_names,
                       latex.mlc(c(paste0("P(SB_{",
                                          e_yr_1,
                                          "} <"),
                                   paste0(f(fc[which.stock] * 1000),
                                          "~t)")),
                                 math.bold = TRUE),
                       latex.mlc(c(paste0("Med(SB_{",
                                          e_yr_1,
                                          "} /"),
                                   paste0(f(fc[which.stock] * 1000),
                                          "~t)")),
                                 math.bold = TRUE))
      }
    }

    k <- c(k,
           length(which(d$UT > 0.2)) / n.row,
           length(which(d$UT > 0.1)) / n.row,
           # length(which(d$UT > 0.05)) / n.row,
           # length(which(d$UT > 0.03)) / n.row,
           # length(which(d$UT > 0.07)) / n.row,
           #length(which(d$UT > 0.08)) / n.row,
           # length(which(d$UT > 0.09)) / n.row,
           median(d$UT))
    if(t == 1){
      col_names <- c(col_names,
                     latex.mlc(c(paste0("P(U_{",
                                        e_yr_1,
                                        "}>"),
                                 "20\\%)"),
                               math.bold = TRUE),
                     latex.mlc(c(paste0("P(U_{",
                                        e_yr_1,
                                        "}>"),
                                 "10\\%)"),
                               math.bold = TRUE),
                     #  latex.mlc(c(paste0("P(U_{",
                     #                   e_yr_1,
                     #                  "}>"),
                     #         "5\\%)"),
                     #   math.bold = TRUE),
                     # latex.mlc(c(paste0("P(U_{",
                     #                    e_yr_1,
                     #                  "}>"),
                     #        "3\\%)"),
                     #  math.bold = TRUE),
                     #latex.mlc(c(paste0("P(U_{",
                     #                e_yr_1,
                     #              "}>"),
                     #   "7\\%)"),
                     # math.bold = TRUE),
                     # latex.mlc(c(paste0("P(U_{",
                     #                 e_yr_1,
                     #                "}>"),
                     #        "8\\%)"),
                     #     math.bold = TRUE),
                     #latex.mlc(c(paste0("P(U_{",
                     #        e_yr_1,
                     #          "}>"),
                     #  "9\\%)"),
                     #  math.bold = TRUE),
                     latex.math.bold(paste0("Med(U_{",
                                            e_yr_1,
                                            "})")))
    }
    proj_dat <- rbind(proj_dat, k)
  }
  colnames(proj_dat) <- col_names

  proj_dat
}
