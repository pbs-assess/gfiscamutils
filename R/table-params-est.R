#' Create a table parameter estimates and priors for iSCAM models
#'
#' @description
#' Create a table parameter estimates and priors for iSCAM models
#'
#' @return an xtable
#' @export
#' @importFrom gfutilities latex.bold latex.mlc latex.size.str get.align latex.perc
#' @importFrom xtable xtable
#' @importFrom rosettafish en2fr
table_params_est <- function(model,
                             digits = 3,
                             xcaption = "default",
                             xlabel   = "default",
                             font.size = 9,
                             space.size = 10,
                             placement = "H",
                             translate = FALSE){

  if(class(model) == mdl_lst_cls){
    model <- model[[1]]
    if(class(model) != msl_cls){
      stop("The structure of the model list is incorrect.")
    }
  }

  mc <- model$mcmccalcs
  p.quants <- mc$p.quants
  mcmc.names <- colnames(p.quants)

  ## Append MPD values
  mpd <- model$mpd
  mpd.names <- names(mpd)

  ## Remove selectivity parameters
  mcmc.names <- mcmc.names[-grep("sel.*", mcmc.names)]
  mcmc.names <- mcmc.names[-grep("s?bo", mcmc.names)]
  p.quants <- p.quants[,-grep("sel.*", colnames(p.quants))]
  p.quants <- p.quants[,-grep("bo", colnames(p.quants))]

  mpd.param.vals <- NULL
  for(pname in mcmc.names){
    ## This is hack code because iscam is not outputting the same parameter
    ##  names for MPD and MCMC runs
    if(pname == "h"){
      pname <- "steepness"
    }
    match.q <- grep("q[[:digit:]]+",
                    pname)
    q.pars <- mpd$q
    if(length(match.q) > 0){
      ## The parameter starts with "q"
      split.val <- strsplit(pname,
                            "[^[:digit:]]")[[1]]
      q.num <- as.numeric(split.val[length(split.val)])
      this.par <- q.pars[q.num]
    }else if(pname != "sbo"){
      ## Match the mcmc name with the mpd name. Q and selectivity are special
      ##  cases, they must be extracted from vectors and matrices respectively
      this.par <- mpd[match(pname, mpd.names)]
    }
    mpd.param.vals <- c(mpd.param.vals, this.par)
  }
  names(mpd.param.vals) <- mcmc.names
  tab <- rbind(p.quants, as.numeric(mpd.param.vals))
  row.n <- rownames(tab)
  row.n[length(row.n)] <- en2fr("MPD", translate)
  rownames(tab) <- row.n
  tab <- f(t(tab), digits)

  ## The next set of names only pertains to the ARF assessment, the q's
  ##  and sel's are modified to line up with each other.
  new.col <- c("$R_0$",
               "$h$",
               "$M$",
               "$\\overline{R}$",
               "$\\overline{R}_{\\mli{init}}$",
               "$\\rho$",
               "$\\vartheta$",
               "$q_1$",
               "$q_2$",
               "$\\tau$",
               "$\\sigma$")
  col.names <- colnames(tab)
  col.names <- latex.bold(latex.perc(col.names))
  col.names <- c(latex.bold(en2fr("Parameter", translate)), col.names)
  tab <- cbind(new.col, tab)
  colnames(tab) <- col.names

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
        booktabs = TRUE)
}
