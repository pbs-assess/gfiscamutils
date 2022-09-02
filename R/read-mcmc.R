#' Read in the MCMC results from an iscam model, reformat them, and
#' remove some records as defined by `burnin` and `thin`
#'
#' @param model An iSCAM model object as created in [load_iscam_files()]
#' @param burnin The number of MCMC records to remove for burnin period
#' @param thin Remove every nth record for thinning of MCMC output
#' @param load_proj Load projection output
#' @param ... Extra arguments
#'
#' @return A list of data frames representing the MCMC output of the
#' iscam model, or NULL if there was a problem or there were no MCMC
#' output files
#' @importFrom stringr str_extract
#' @importFrom purrr imap
#' @export
read_mcmc <- function(model,
                      burnin = 1000,
                      thin = 1,
                      load_proj = TRUE,
                      ...){

  mcmc_dir <- model$mcmcpath

  if(is.null(mcmc_dir)){
    stop("`model` does not have `mcmcpath` set (model$mcmcpath is NULL)")
  }

  list_by <- function(d, by = "fleet"){
    mc <- mcmc_thin(d, burnin, thin)
    ngear <- model$dat$num.gears
    tmp <- map(seq_len(ngear), ~{
      mc %>% select(contains(paste0(by, .x)))
    }) %>%
      map(~{if(ncol(.x)) .x else NULL})
    tmp <- tmp %>%
      map(~{
        if(all(is.na(.x))) NULL else .x
      })
    tmp[sapply(tmp, is.null)] <- NULL
    # Make column names years only
    tmp <- tmp %>% map(~{
      names(.x) <- str_extract(names(.x), "[0-9]+$")
      .x
    })
    tmp
  }

  # List of files to load in, with associated type of data:
  # "default"     - thinning/burnin is done
  # "single"      - column names are converted to years, output is a data frame
  # "list"        - the data frame is broken into a list by the third item in
  #                 the list
  # "projections" - no processing is done, it is done in calc_mcmc() instead
  #                 which is sent to `list_by()` to extract by column names
  # "specialage"  - outputs which are matrices by year, gear, and sex such as
  #                 age fits or age residuals. There is a special format for
  #                 these files. See the mcmc_output() function in iscam
  #                 source (iscam.tpl file) for the output format
  # "specialsel"  - the same as "specialage" but for selectivity estimates
  fn_lst <- list(list(mcmc.file, "default"),
                 list(mcmc.biomass.file, "single"),
                 list(mcmc.recr.file, "single"),
                 list(mcmc.recr.devs.file, "single"),
                 list(mcmc.fishing.mort.file, "list", "fleet"),
                 list(mcmc.natural.mort.file, "list", "sex"),
                 list(mcmc.fishing.mort.u.file, "list", "fleet"),
                 list(mcmc.vuln.biomass.file, "list", "fleet"),
                 list(mcmc.index.fits.file, "list", "gear"),
                 list(mcmc.index.resids.file, "list", "gear"),
                 list(mcmc.index.standardized.resids.file, "list", "gear"),
                 list(mcmc.age.fits.file, "df_longer_age", "gear"),
                 list(mcmc.age.resids.file, "df_longer_age", "gear"),
                 list(mcmc.sel.file, "df_longer_sel", "gear"),
                 list(mcmc.proj.file, "projections"))

  # Names given to the return list elements. Must be same length as `fn_lst`
  nms <- c("params", "sbt", "rt", "rdev", "ft",
           "m", "ut", "vbt",
           "it", "epsilon", "std_epsilon",
           "agefits", "ageresids",
           "selest", "proj")

  if(length(nms) != length(fn_lst)){
    stop("Length of `fn_lst` must be the same as the length of `nms`")
  }

  k <- imap(fn_lst, ~{
    d <- NULL
    fn <- file.path(mcmc_dir, .x[1])
    if(file.exists(fn)){
      if(.x[[2]] == "df_longer_sel"){
        d <- load_longer(fn,
                         type = "sel",
                         gear_names = model$dat$gear_names,
                         burnin = burnin,
                         thin = thin)
      }else if(.x[[2]] == "df_longer_age"){
        d <- load_longer(fn,
                         type = "age",
                         gear_names = model$dat$age_gear_names,
                         burnin = burnin,
                         thin = thin)
      }else if(.x[[2]] == "default"){
        d <- read.csv(fn)
        d <- mcmc_thin(d, burnin, thin)
      }else if(.x[[2]] == "single"){
        d <- read.csv(fn)
        names(d) <- str_extract(names(d), "[0-9]+$")
        d <- mcmc_thin(d, burnin, thin)
      }else if(.x[[2]] == "list"){
        if(length(.x) < 3){
          stop("sublist ", .y, " does not have a third element which is ",
               "required when using 'list' for the second item")
        }
        d <- read.csv(fn)
        d <- list_by(d, by = .x[[3]])
        # Set column names for indices to actual years instead of numbers 1:ncol
        if(nms[.y] == "it" || nms[.y] == "epsilon" || nms[.y] == "std_epsilon"){
          # Set names and years for index fits
          d <- imap(d, ~{
            as_tibble(.x) %>%
              `names<-`(model$dat$indices[[.y]][, "iyr"])
          }) %>%
            `names<-`(model$dat$index_gear_names)
        }
      }else if(.x[[2]] == "projections"){
        # Do no processing
        d <- read.csv(fn)
      }else{
        stop("Sublist ", .y, " of fn_lst has an unimplmented value for its second element (", .x[[2]], ")")
      }
    }else{
      warning("File ", fn, " does not exist, check iSCAM output")
    }
    d
  }) %>% `names<-`(nms)

  k
}
