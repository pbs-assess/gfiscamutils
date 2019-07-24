#' Make an xtable of likelihood values - Herring specific
#'
#' @param models
#' @param which 1 will put hg, prs, and cc in the table,
#' 2 will put sog and wcvi in the table
#' @param digits number of decimal places for the values
#' @param xcaption caption to appear in the calling document
#' @param xlabel the label used to reference the table in latex
#' @param font.size size of the font for the table
#' @param space.size size of the vertical spaces for the table
#' @param placement latex code for placement of the table in document
#'
#' @return
#' @export
make.likelihood.table <- function(models,
                                  which = 1,
                                  digits = 3,
                                  xcaption = "default",
                                  xlabel   = "default",
                                  font.size = 11,
                                  space.size = 12,
                                  placement = "H"){

  tab <- list()
  for(i in 1:length(models)){
    ## For each of the stocks 1 through 5
    ## Assume the first model is the base
    like.components <- lapply(models[[i]],
                              function(x){
                                nlvec <- x$mpd$nlvec
                                nl.cat <- nlvec[1, 1]
                                nl.surv1 <- nlvec[2, 1]
                                nl.surv2 <- nlvec[2, 2]
                                nl.age1 <- nlvec[3, 1]
                                nl.age2 <- nlvec[3, 2]
                                nl.age3 <- nlvec[3, 3]
                                nl.sr <- nlvec[4, 1]
                                list(nl.cat,
                                     nl.surv1,
                                     nl.surv2,
                                     nl.age1,
                                     nl.age2,
                                     nl.age3,
                                     nl.sr)})

    like <- lapply(models[[i]],
                   function(x){
                     nlvec <- as.vector(x$mpd$nlvec)
                     nlvec <- nlvec[!is.na(nlvec)]
                     nlvec <- nlvec[nlvec != 0]
                     -sum(nlvec)
                   })

    diffs <- lapply(like,
                    function(x){
                      like[[1]] - x
                    })
    ## Get the total number of parameters estimated for each model
    num.params <- lapply(models[[i]],
                         function(x){
                           ctl <- x$ctl
                           dat <- x$dat
                           params <- as.data.frame(ctl$params)
                           num.params <- x$mpd$NumParams
                           ## Selectivity parameters
                           ## sel data frame has one column for each gear and 10 rows:
                           ## 1  - selectivity type:
                           ##       1) logistic selectivity parameters
                           ##       2) selectivity coefficients
                           ##       3) a constant cubic spline with age-nodes
                           ##       4) a time varying cubic spline with age-nodes
                           ##       5) a time varying bicubic spline with age & year nodes
                           ##       6) fixed logistic (set isel_type=6, and estimation phase to -1)
                           ##       7) logistic function of body weight.
                           ##       8) logistic with weight deviations (3 parameters)
                           ##       11) logistic selectivity with 2 parameters based on mean length
                           ##       12) length-based selectivity coefficients with spline interpolation
                           ## 2  - Age/length at 50% selectivity (logistic)
                           ## 3  - STD at 50% selectivity (logistic)
                           ## 4  - No. of age nodes for each gear (0=ignore)
                           ## 5  - No. of year nodes for 2d spline(0=ignore)
                           ## 6  - Phase of estimation (-1 for fixed) If neg number, it reflects a
                           ##       mirroring of another gear's selectivity.
                           ## 7  - Penalty wt for 2nd differences w=1/(2*sig^2)
                           ## 8  - Penalty wt for dome-shaped w=1/(2*sig^2)
                           ## 9  - Penalty wt for time-varying selectivity
                           ## 10 - n_sel_blocks (number of selex blocks)

                           sel <- ctl$sel
                           indices <- dat$indices
                           indices.df <- as.data.frame(do.call(rbind, indices))
                           surv.gear.nums <- unique(indices.df$gear)
                           surv.sel <- as.data.frame(sel[,surv.gear.nums])
                           fish.sel <- as.data.frame(sel[,-surv.gear.nums])
                           ## Get number estimated by looking at the phase row in the sel data frame
                           surv.est <- surv.sel[6,]
                           surv.est <- sum(surv.est > 0)
                           fish.est <- fish.sel[6,]
                           fish.est <- sum(fish.est > 0)

                           ## Natural mortality parameters
                           num.m.params <- 0
                           misc.ctl <- as.data.frame(ctl$misc)
                           if(misc.ctl["mdevphase", 1] > 0){
                             ## Natural mortality was estimated.
                             num.m.params <- misc.ctl["mnumestnodes", 1]
                           }

                           ## Catchability  parameters
                           ## q is a data frame with 1 column for each survey and 3 rows:
                           ## 1 - prior type:
                           ##      0) Uniformative prior
                           ##      1) normal prior density for log(q)
                           ##      2) random walk in q
                           ## 2 - prior log(mean)
                           ## 3 - prior SD

                           q <- ctl$surv.q
                           num.inds <- ctl$num.indices

                           ## Fishing mortality and recruitment parameters
                           ##
                           par <- x$par
                           num.f.params <- length(par$log_ft_pars)
                           num.rec.params <- length(par$log_rec_devs)
                           num.init.rec.params <- length(par$init_log_rec_devs)
                           tot <- num.params +
                             surv.est +
                             fish.est +
                             num.inds +
                             num.f.params +
                             num.rec.params +
                             num.init.rec.params +
                             num.m.params
                         })
    aic <- lapply(1:length(like),
                  function(x){
                    2 * num.params[[x]] - 2 * like[[x]]
                  })

    like.comps <- sapply(like.components, unlist)
    tab[[i]] <- as.data.frame(do.call(cbind, list(-like.comps[1,],
                                                  -like.comps[2,],
                                                  -like.comps[3,],
                                                  -like.comps[4,],
                                                  -like.comps[5,],
                                                  -like.comps[6,],
                                                  -like.comps[7,],
                                                  unlist(like),
                                                  unlist(diffs),
                                                  unlist(num.params),
                                                  unlist(aic))))

    tab[[i]][, 1] <- f(tab[[i]][, 1], 2)
    tab[[i]][, 2] <- f(tab[[i]][, 2], 2)
    tab[[i]][, 4] <- f(tab[[i]][, 4], 2)
  }
  tab <- do.call(rbind,
                 tab)

  tab <- cbind(rep(c("Time-varying M", "Constant M"),
                   length(models)),
               tab)

  tab <- cbind(c("HG",
                 "HG",
                 "PRD",
                 "PRD",
                 "CC",
                 "CC",
                 "SOG",
                 "SOG",
                 "WCVI",
                 "WCVI",
                 "HG",
                 "HG",
                 "PRD",
                 "PRD",
                 "CC",
                 "CC",
                 "SOG",
                 "SOG",
                 "WCVI",
                 "WCVI"),
               c("AM2",
                 "AM2",
                 "AM2",
                 "AM2",
                 "AM2",
                 "AM2",
                 "AM2",
                 "AM2",
                 "AM2",
                 "AM2",
                 "AM1",
                 "AM1",
                 "AM1",
                 "AM1",
                 "AM1",
                 "AM1",
                 "AM1",
                 "AM1",
                 "AM1",
                 "AM1"),
               tab)

  colnames(tab) <- c(latex.bold("Area"),
                     latex.mlc(c("Model",
                                 "paramet-",
                                 "erization")),
                     latex.mlc(c("Model",
                                 "sensitivity")),
                     latex.mlc(c("Catch",
                                 "data",
                                 "likelihood")),
                     latex.mlc(c("Survey",
                                 "index 1",
                                 "likelihood")),
                     latex.mlc(c("Survey",
                                 "index 2",
                                 "likelihood")),
                     latex.mlc(c("Age comp",
                                 "data",
                                 "gear 1",
                                 "likelihood")),
                     latex.mlc(c("Age comp",
                                 "data",
                                 "gear 2",
                                 "likelihood")),
                     latex.mlc(c("Age comp",
                                 "data",
                                 "gear 3",
                                 "likelihood")),
                     latex.mlc(c("S-R relation",
                                 "likelihood")),
                     latex.mlc(c("Total",
                                 "likelihood")),
                     latex.mlc(c("Difference",
                                 "in",
                                 "total",
                                 "likelihood",
                                 "from",
                                 "AM2 TVM")),
                     latex.mlc(c("Number of",
                                 "estimated",
                                 "parameters")),
                     latex.bold("AIC"))

  addtorow <- list()
  addtorow$pos <- list(2, 4, 6, 8, 10, 12, 14, 16, 18)
  addtorow$command <- c("\\midrule ",
                        "\\midrule ",
                        "\\midrule ",
                        "\\midrule ",
                        "\\midrule ",
                        "\\midrule ",
                        "\\midrule ",
                        "\\midrule ",
                        "\\midrule ")

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        add.to.row = addtorow,
        booktabs = TRUE)
}
