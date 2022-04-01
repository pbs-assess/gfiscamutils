#' Plot priors and posteriors for the given isam model
#'
#' @param model An iscam model object
#' @param priors.only Plot the priors only? TRUE/FALSE
#'
#' @details Plots the priors overlaid on the posteriors for the given iscm model.
#'   If priors.only is TRUE, only the priors will be plotted.
#'   The values in the control file (model$ctl$params) for each
#'   prior are:
#'   1. ival  = initial value
#'   2. lb    = lower bound
#'   3. ub    = upper bound
#'   4. phz   = ADMB phase
#'   5. prior = prior distribution funnction
#'          0 = Uniform
#'          1 = normal    (p1=mu,p2=sig)
#'          2 = lognormal (p1=log(mu),p2=sig)
#'          3 = beta      (p1=alpha,p2=beta)
#'          4 = gamma     (p1=alpha,p2=beta)
#'   6. p1 (defined by 5 above)
#'   7. p2 (defined by 5 above)
#'
#' @return Nothing
#' @export
plot_priors_posts <- function(model,
                              priors_only = TRUE){

  if(class(model) != mdl_cls){
    stop("The `model` argument is not a gfiscamutils::mdl_cls class")
  }

  f_nms <- c(dunif, dnorm, dlnorm, dbeta, dgamma)

  mc <- model$mcmccalcs$params_log
  # Remove selectivity parameters, bo, vartheta, sigma, tau, bmsy from the posteriors
  mc <- mc %>%
    select(-contains(c("sel", "bo", "vartheta", "tau", "sigma", "bmsy")))
  post_nms <- names(mc)

  # Remove fixed parameters, and upper and lower bound, and phase information,
  # but keep initial value. Remove kappa if present
  prior_specs <- as_tibble(model$ctl$params, rownames = "param") %>%
    filter(phz > 0) %>%
    select(-c(lb, ub, phz)) %>%
    filter(param != "kappa")

  # Add the q parameters to the prior specs table
  q_params <- model$ctl$surv.q %>%
    t() %>%
    as_tibble() %>%
    mutate(param = paste0("q", row_number())) %>%
    mutate(ival = priormeanlog) %>%
    rename(prior = priortype) %>%
    rename(p1 = priormeanlog) %>%
    rename(p2 = priorsd) %>%
    select(param, ival, prior, p1, p2)

  prior_specs <- prior_specs %>%
    bind_rows(q_params)

  # Get MPD estimates for the parameters in the posteriors
  mpd <- model$mpd
  q_pat <- "^log_q_gear([1-9]+)$"
  mpd_vals <- imap_dbl(post_nms, ~{
    mle <- NULL
    if(.x == "log_m_sex1"){
      mle <- log(mpd$m[1])
    }else if(.x == "log_m_sex2"){
      mle <- log(mpd$m[2])
    }else if(.x == "h"){
      mle <- mpd$steepness
    }else if(length(grep(q_pat, .x)) > 0){
      num <- as.numeric(sub(q_pat, "\\1", .x))
      mle <- log(mpd$q[num])
    }else{
      mle <- as.numeric(mpd[.x])
    }
    mle
  }) %>%
    `names<-`(post_nms)

  # n_side <- get_rows_cols(length(post_nms))
  # par(mfrow = n_side,
  #     oma = c(2, 3, 1, 1),
  #     mai = c(0.2, 0.4, 0.3, 0.2))

  if(nrow(prior_specs) != length(post_nms)){
    stop("Number of rows in prior_specs and length of post_names are not the same, debug function")
  }

  specs <- prior_specs[3, ]
  prior_fn <- f_nms[[as.numeric(specs$prior + 1)]]
  xx <- list(p = mc[, 1],
             p1 = as.numeric(specs$p1),
             p2 = as.numeric(specs$p2),
             fn = prior_fn,
             nm = post_nms[1],
             mle = as.numeric(mpd_vals[1]))
  xx$nm <- get_latex_name(xx$nm)
  func <- function(x){xx$fn(x, xx$p1, xx$p2)}
  curve(func,
        from = xx$p1 - 4 * xx$p2,
        to = xx$p2 + 4 * xx$p2,
        xlab = "",
        ylab = "",
        col = "black",
        lwd = 2)

  plot_norm(alpha = 0.75, mean = xx$p1, sd = xx$p2)
browser()
  # g <- ggplot(data = data.frame(x = c(-6, 6)), aes(x)) +
  #   stat_function(fun = dnorm,
  #                 n = 100,
  #                 args = list(mean = xx$p1, sd = xx$p2),
  #                 geom = "area",
  #                 fill = "steelblue",
  #                 lwd = 1) +
  #   ylab("") +
  #   scale_y_continuous(breaks = NULL) +
  #   geom_vline(xintercept = xx$p1, color = "white", lwd = 0.5, lty = 2) +
  #   geom_vline(xintercept = xx$p1 - xx$p2, color = "white", lwd = 0.5, lty = 1) +
  #   geom_vline(xintercept = xx$p1 + xx$p2, color = "white", lwd = 0.5, lty = 1)



  # -----------------------------------------------
  g_lst <- imap(post_nms, ~{
    specs <- prior_specs[.y, ]
    prior_fn <- f_nms[[as.numeric(specs$prior + 1)]]
    xx <- list(p = mc[, .y],
               p1 = as.numeric(specs$p1),
               p2 = as.numeric(specs$p2),
               fn = prior_fn,
               nm = post_nms[.y],
               mle = as.numeric(mpd_vals[.y]))

    xx$nm <- get_latex_name(xx$nm)
    if(priors_only){
      func <- function(x){xx$fn(x, xx$p1, xx$p2)}
      g <- ggplot()
      if(specs$prior == 0){
        # Uniform, plot from p1-1 to p2+1
        curve(func,
              from = xx$p1 - 1,
              to = xx$p2 + 1,
              xlab = "",
              ylab = "",
              col = "black",
              lwd = 2)
      }else if(specs$prior == 1){
        ## Normal, plot from -(p1-p2*4) to (p1+p2*4)
        curve(func,
              from = xx$p1 - 4 * xx$p2,
              to = xx$p2 + 4 * xx$p2,
              xlab = "",
              ylab = "",
              col = "black",
              lwd = 2)
      }else{
        curve(func,
              xlab = "",
              ylab = "",
              col = "black",
              lwd = 2)
      }
      title(xx$nm)
    }else{
      plot_marg(xx,
                breaks = "sturges",
                col = "wheat")
    }
  })

  cowplot::plot_grid(plotlist = g_lst, nrow = 3)
}
