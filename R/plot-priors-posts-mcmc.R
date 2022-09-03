#' Plot MCMC priors and posteriors for iSCAM models
#'
#' @description
#' Priors and posteriors comparison plots for parameters in an MCMC run
#'
#' @details Plots the priors overlaid on the posteriors for the given iscam model.
#'   The values in the control file (`model$ctl$params`) for each
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
#' See [plot_distribution()] for prior plot details including an documentation
#' on the vertical lines
#'
#' @inheritParams plot_traces_mcmc
#' @family MCMC diagnostics plots
#'
#' @importFrom cowplot plot_grid
#' @export
plot_priors_posts_mcmc <- function(model,
                                   param_rm = c("sel",
                                                "bo",
                                                "vartheta",
                                                "tau",
                                                "sigma"),
                                   priors_only = FALSE,
                                   text_title_size = 12,
                                   ...){

  if(is_iscam_model_list(model) && length(model) == 1){
    model <- model[[1]]
  }

  if(!is_iscam_model(model)){
    if(is_iscam_model_list(model)){
      stop("`model` is not an iscam model object, it is an iscam model ",
           "list object")
    }
    stop("`model` is not an iscam model object")
  }

  # Set up model description for the title
  model_desc <- as.character(attributes(model)$model_desc)

  f_nms <- c(dunif, dnorm, dlnorm, dbeta, dgamma)

  mc <- model$mcmccalcs$params_log

  # Remove selectivity parameters, bo, vartheta, sigma, tau, bmsy from the posteriors
  mc <- mc %>%
    select(-contains(param_rm)) %>%
    select(-contains("msy")) %>%
    select(-matches("SSB")) %>%
    select(-matches("f"))

  post_nms <- names(mc)

  # Remove fixed parameters, and upper and lower bound, and phase information,
  # but keep initial value. Remove kappa if present
  prior_specs <- as_tibble(model$ctl$params, rownames = "param")
  wch_fem_m <- prior_specs$param == "log_m_female"
  wch_mal_m <- prior_specs$param == "log_m_male"
  wch_m <- prior_specs$param == "log_m"
  fem_m_est <- prior_specs[wch_fem_m, ]$phz > 0
  mal_m_est <- prior_specs[wch_mal_m, ]$phz > 0
  m_est <- prior_specs[wch_m, ]$phz > 0
  fem_m_est <- ifelse(!length(fem_m_est), FALSE, fem_m_est)
  mal_m_est <- ifelse(!length(mal_m_est), FALSE, mal_m_est)
  m_est <- ifelse(!length(m_est), FALSE, m_est)

  prior_specs <- prior_specs  %>%
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

  # Remove M parameters from posteriors if it was not estimated
  if(!m_est && !mal_m_est){
    post_nms <- post_nms[post_nms != "log_m_sex1"]
  }
  if(!fem_m_est){
    post_nms <- post_nms[post_nms != "log_m_sex2"]
  }

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
               nm = post_nms[.y])

    xx$nm <- get_fancy_expr(xx$nm)
    func <- function(x){xx$fn(x, xx$p1, xx$p2)}
    if(specs$prior == 0){
      # Uniform
      minx <- min(xx$p1, xx$p)
      maxx <- max(xx$p2, xx$p)
      g <- plot_distribution(fun = dunif,
                             xlim = c(minx, maxx),
                             title = xx$nm,
                             alpha = 0.9,
                             param_lst = list(min = minx, max = maxx),
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
      minx <- min(-3, xx$p)
      maxx <- max(3, xx$p)
      g <- plot_distribution(fun = dnorm,
                             xlim = c(minx, maxx),
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
      g <- plot_distribution(fun = dlnorm,
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
    }else if(specs$prior == 3){
      # Beta
      g <- plot_distribution(fun = dbeta,
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
    }else if(specs$prior == 4){
      # Gamma
      minx <- min(0, xx$p)
      maxx <- max(5, xx$p)
      g <- plot_distribution(fun = dgamma,
                             xlim = c(minx, maxx),
                             title = xx$nm,
                             alpha = 0.9,
                             param_lst = list(shape = xx$p1, scale = xx$p2),
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

  if(is.null(text_title_size)){
    plot_grid(plotlist = g_lst, ...)
  }else{
    title <- ggdraw() +
      draw_label(tex(model_desc),
                 size = text_title_size,
                 x = 0.5)
    p <- plot_grid(plotlist = g_lst, ...)
    plot_grid(title, p, ncol = 1, rel_heights = c(0.05, 1), ...)
  }
}
