#' Priors and posteriors comparison plots for parameters in an MCMC run
#'
#' @details Plots the priors overlaid on the posteriors for the given iscam model.
#'   The values in the control file (model$ctl$params) for each
#'   prior are:
#'   1. ival  = initial value
#'   2. lb    = lower bound
#'   3. ub    = upper bound
#'   4. phz   = ADMB phase
#'   5. prior = prior distribution funnction
#'          0 = Uniform
#'          1 = normal    (p1 = mu, p2 = sig)
#'          2 = lognormal (p1 = log(mu), p2 = sig)
#'          3 = beta      (p1 = alpha, p2 = beta)
#'          4 = gamma     (p1 = alpha, p2 = beta)
#'   6. p1 (defined by 5 above)
#'   7. p2 (defined by 5 above)
#'
#' See [plot_fun()] for prior plot details including an documentation on the
#' vertical lines
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param priors_only Logical. If `TRUE`, plot the priors only. If `FALSE`,
#' plot both priors and associated posterior histograms
#' @param ... Additional arguments passed to [cowplot::plot_grid()]
#'
#' @return Nothing
#' @importFrom cowplot plot_grid
#' @export
plot_priors_posts_mcmc <- function(model,
                                   param_rm = c("sel", "bo", "vartheta", "tau", "sigma", "bmsy"),
                                   priors_only = FALSE,
                                   ...){

  if(class(model) != mdl_cls){
    if(class(model) == mdl_lst_cls){
      stop("The `model` argument is of class `gfiscamutils::mdl_lst_cls`. ",
           "This function requires that `model` be an object of class ",
           "`gfiscamutils::mdl_cls`. Select one element of the list (which ",
           "is a single model), and modify it like this, then call this function ",
           "again with `model` as your argument:\n\n",
           "model <- list(model)\n",
           "class(model) <- mdl_lst_cls\n")
    }
    stop("The `model` argument is not of class `gfiscamutils::mdl_cls`")
  }

  f_nms <- c(dunif, dnorm, dlnorm, dbeta, dgamma)

  mc <- model$mcmccalcs$params_log

  # Remove selectivity parameters, bo, vartheta, sigma, tau, bmsy from the posteriors
  mc <- mc %>%
    select(-contains(param_rm))
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
  # Not used currently but kept in case needed
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

  if(nrow(prior_specs) != length(post_nms)){
    stop("Number of rows in prior_specs and length of post_names are not the same, debug function")
  }

  g_lst <- imap(post_nms, ~{
    specs <- prior_specs[.y, ]
    prior_fn <- f_nms[[as.numeric(specs$prior + 1)]]
    xx <- list(p = mc[, .y],
               p1 = as.numeric(specs$p1),
               p2 = as.numeric(specs$p2),
               fn = prior_fn,
               nm = post_nms[.y],
               mle = as.numeric(mpd_vals[.y]))

    xx$nm <- get_fancy_name(xx$nm)
    func <- function(x){xx$fn(x, xx$p1, xx$p2)}
    if(specs$prior == 0){
      # Uniform
      g <- plot_fun(fun = dunif,
                    xlim = c(xx$p1, xx$p2),
                    title = xx$nm,
                    alpha = 0.9,
                    param_lst = list(min = xx$p1, max = xx$p2),
                    show_mean_line = FALSE,
                    show_sd_lines = FALSE)
      if(!priors_only){
        g_dat <- ggplot_build(g)
        ymax <- max(g_dat$data[[1]]$ymax)
        g <- g +
          geom_histogram(data = data.frame(xx$p), aes(x = xx.p, y = ..ncount.. * ymax),
                         color = "black",
                         fill = "khaki1",
                         alpha = 0.5,
                         bins = 30)
      }
    }else if(specs$prior == 1){
      # Normal
      g <- plot_fun(fun = dnorm,
                    xlim = c(-3, 3),
                    title = xx$nm,
                    alpha = 0.9,
                    param_lst = list(mean = xx$p1, sd = xx$p2),
                    show_mode_line = TRUE)
      if(!priors_only){
        g_dat <- ggplot_build(g)
        ymax <- max(g_dat$data[[1]]$ymax)
        g <- g +
          geom_histogram(data = data.frame(xx$p), aes(x = xx.p, y = ..ncount.. * ymax),
                         color = "black",
                         fill = "khaki1",
                         alpha = 0.5,
                         bins = 30)
      }
    }else if(specs$prior == 2){
      # Lognormal
      g <- plot_fun(fun = dlnorm,
                    xlim = c(log(xx$p1), log(xx$p2)),
                    title = xx$nm,
                    alpha = 0.9,
                    param_lst = list(mean = log(xx$p1), sd = log(xx$p2)),
                    show_mode_line = TRUE)
      if(!priors_only){
        g_dat <- ggplot_build(g)
        ymax <- max(g_dat$data[[1]]$ymax)
        g <- g +
          geom_histogram(data = data.frame(log(xx$p)), aes(x = xx.p, y = ..ncount.. * ymax),
                         color = "black",
                         fill = "khaki1",
                         alpha = 0.5,
                         bins = 30)
      }
    }else if(specs$prior %in% 3:4){
      # Beta
      g <- plot_fun(fun = ifelse(specs$prior == 3, dbeta, dgamma),
                    xlim = c(0.5, 1),
                    title = xx$nm,
                    alpha = 0.9,
                    param_lst = list(shape1 = xx$p1, shape2 = xx$p2),
                    show_mode_line = TRUE)
      if(!priors_only){
        g_dat <- ggplot_build(g)
        ymax <- max(g_dat$data[[1]]$ymax)
        g <- g +
          geom_histogram(data = data.frame(xx$p), aes(x = xx.p, y = ..ncount.. * ymax),
                         color = "black",
                         fill = "khaki1",
                         alpha = 0.5,
                         bins = 30)
      }
    }
    g
  })

  plot_grid(plotlist = g_lst, ...)
}
