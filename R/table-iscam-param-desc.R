#' Create the ISCAM model parameter description table as found in the
#' Model Description appendix
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param ... Arguments passed to [csasdown::csas_table()]
#'
#' @return A [knitr::kable()] table
#' @export
table_iscam_param_desc <- function(model,
                                   bold_header = TRUE,
                                   ...){

  header <- c("Symbol", "Description")
  if(fr()){
    header <- c("Symbole", "Decsription")
  }
  sec_hdrs <- list()
  secs <- list()
  symbols <- list()

  sec_hdrs[1] <- "Indices"
  secs[[1]] <- list(ifelse(fr(), "Indice pour le sexe", "Index for sex"),
                    ifelse(fr(), "Indice pour l'\u00E2ge", "Index for age"),
                    ifelse(fr(), "Indice pour l'ann\u00E9e", "Index for year"),
                    ifelse(fr(), "Indice pour l'engrenage", "Index for gear"),
                    ifelse(fr(),
                           "Indice pour le bloc d'ann\u00E9es dans la s\u00E9lectivit\u00E9 variable dans le temps",
                           "Index for year block in time-varying selectivity"))
  symbols[[1]] <- list("$s$", "$a$", "$t$", "$k$", "$b$")

  sec_hdrs[2] <- ifelse(fr(),
                        "Dimensions du mod\u00E8le",
                        "Model dimensions")
  secs[[2]] <- list(ifelse(fr(), "Nombre de sexes", "Number of sexes"),
                    ifelse(fr(),
                           "Classe d'\u00E2ge la plus jeune et la plus \u00E2g\u00E9e ($A$ est un groupe plus)",
                           "Youngest and oldest age group ($A$ is a plus group"),
                    ifelse(fr(),
                           "Premi\u00E8re et derni\u00E8re ann\u00E9e des donn\u00E9es de captures",
                           "First and last year of catch data"),
                    ifelse(fr(),
                           "Nombre d'engins, y compris les engins d'enqu\u00EAte",
                           "Number of gears, including survey gears"))
  symbols[[2]] <- list("$S$", "$\\acute{a}$, $A$", "$\\acute{t}$, $T$", "$K$")

  sec_hdrs[3] <- ifelse(fr(),
                        "Observations (donn\u00E9es)",
                        "Observations (data)")
  secs[[3]] <- list(ifelse(fr(),
                           "captures en poids par engin $k$ en cours d'ann\u00E9e $t$",
                           "catches in weight by gear $k$ in year $t$"),
                    ifelse(fr(),
                           "indice d'abondance relative pour l'engin $k$ en cours d'ann\u00E9e $t$",
                           "relative abundance index for gear $k$ in year $t$"))
  symbols[[3]] <- list("$C_{k,t}$", "$I_{k,t}$")

  sec_hdrs[4] <- ifelse(fr(),
                        "Param\u00E8tres estim\u00E9s",
                        "Estimated parameters")
  secs[[4]] <- list(ifelse(fr(),
                           "recrute dans des conditions non p\u00EAch\u00E9es",
                           "recruits in unfished conditions"),
                    ifelse(fr(),
                        "Rugosit\u00E9 de la relation stock-recrutement",
                        "Steepness of the stock-recruitment relationship"),
                    ifelse(fr(),
                           "\u00C2ge moyen-$\\acute{a}$ recrutement \u00E0 partir de l'ann\u00E9e $\\acute{t}$ \u00E0 $T$",
                           "Average age-$\\acute{a}$ recruitment from year $\\acute{t}$ to $T$"),
                    ifelse(fr(),
                           "\u00C2ge moyen-$\\acute{a}-1$ \u00E0 $A$ recrutement pour l'initialisation",
                           "Average age-$\\acute{a} - 1$ to $A$ recruitment for initialization"),
                    ifelse(fr(),
                           "Taux instantan\u00E9 de mortalit\u00E9 naturelle pour le sexe $s$",
                           "Instantaneous natural mortality rate for sex $s$"),
                    ifelse(fr(),
                           "Param\u00E8tres de s\u00E9lectivit\u00E9 pour l'engrenage $k$, sexe $s$, bloc d'ann\u00E9es $b$",
                           "Selectivity parameters for gear $k$, sex $s$, year block $b$"),
                    ifelse(fr(),
                           linebreak("Logarithme de la mortalit\u00E9 instantan\u00E9e par p\u00EAche pour l'engin $k$, \n le sexe $s$, l'ann\u00E9e $t$"),
                           "Logarithm of the instantaneous fishing mortality for gear $k$, sex $s$, year $t$"),
                    ifelse(fr(),
                           "L'\u00E2ge-$\\acute{a}$ s'\u00E9carte de $\\bar{R}$ depuis des ann\u00E9es $\\acute{t}$ \u00E0 $T$",
                           "Age-$\\acute{a}$ deviates from $\\bar{R}$ for years $\\acute{t}$ to $T$"),
                    ifelse(fr(),
                           "L'\u00E2ge-$\\acute{a}$ s'\u00E9carte de $\\bar{R}_\\mathrm{init}$ depuis des ann\u00E9es $\\acute{t}$",
                           "Age-$\\acute{a}$ deviates from $\\bar{R}_\\mathrm{init}$ for year $\\acute{t}$"),
                    ifelse(fr(),
                           "Param\u00E8tre de capturabilit\u00E9 pour l'enqu\u00EAte $k$",
                           "Catchability parameter for survey $k$"),
                    ifelse(fr(),
                           "Fraction de la variance totale associ\u00E9e \u00E0 l'erreur d'observation",
                           "Fraction of the total variance associated with observation error"),
                    ifelse(fr(),
                           "Pr\u00E9cision totale (inverse de la variance) de l'erreur totale",
                           "Total precision (inverse of variance) of the total error"))
  symbols[[4]] <- list("$R_0$", "$h$", "$\\bar{R}$", "$\\bar{R}_\\mathrm{init}$", "$M_s$",
                       "$\\hat{a}_{k,s,b}, \\hat{\\gamma}_{k,s,b}$", "$\\Gamma_{k,s,t}$",
                       "$\\omega_t$", "$\\omega_\\mathrm{init,t}$", "$q_k$", "$\\rho$", "$\\vartheta^2$")

  sec_hdrs[5] <- ifelse(fr(),
                        "\u00C9carts types",
                        "Standard deviations")
  secs[[5]] <- list(ifelse(fr(),
                           "\u00C9cart-type pour les erreurs d'observation dans l'indice d'enqu\u00EAte",
                           "Standard deviation for observation errors in survey index"),
                    ifelse(fr(),
                           "\u00C9cart-type des erreurs de processus (\u00E9carts de recrutement)",
                           "Standard deviation in process errors (recruitment deviations)"),
                    ifelse(fr(),
                           "\u00C9cart-type des captures observ\u00E9es par engin",
                           "Standard deviation in observed catch by gear"))
  symbols[[5]] <- list("$\\sigma$", "$\\tau$", "$\\sigma_C$")

  sec_hdrs[6] <- ifelse(fr(),
                        "R\u00E9sidus",
                        "Residuals")
  secs[[6]] <- list(ifelse(fr(),
                           "Recrutement annuel r\u00E9siduel",
                           "Annual recruitment residual"),
                    ifelse(fr(),
                           "Erreur r\u00E9siduelle dans les captures pr\u00E9vues",
                           "Residual error in predicted catch"))
  symbols[[6]] <- list("$\\delta_t$", "$\\eta_t$")

  sec_hdrs[7] <- ifelse(fr(),
                        linebreak("Croissance fixe \\& \n param\u00E8tres de maturit\u00E9"),
                        linebreak("Fixed growth \\& maturity \n parameters"))
  secs[[7]] <- list(ifelse(fr(),
                           "Longueur asymptotique pour le sexe $s$",
                           "Asymptotic length for sex $s$"),
                    ifelse(fr(),
                           "Coefficient de croissance de Brody pour le sexe $s$",
                           "Brody growth coefficient for sex $s$"),
                    ifelse(fr(),
                           "\u00C2ge th\u00E9orique \u00E0 la longueur z\u00E9ro pour le sexe $s$",
                           "Theoretical age at zero length for sex $s$"),
                    ifelse(fr(),
                           "Scalaire dans l'allom\u00E9trie longueur-poids pour le sexe $s$",
                           "Scalar in length-weight allometry for sex $s$"),
                    ifelse(fr(),
                           "Param\u00E8tre de puissance dans l'allom\u00E9trie longueur-poids pour le sexe $s$",
                           "Power parameter in length-weight allometry for sex $s$"),
                    ifelse(fr(),
                           "\u00C2ge \u00E0 50 \\% pour la maturit\u00E9 sexuelle $s$",
                           "Age at 50\\% maturity for sex $s$"),
                    ifelse(fr(),
                           "\u00C9cart-type \u00E0 50 \\% de maturit\u00E9 pour le sexe $s$",
                           "Standard deviation at 50\\% maturity for sex $s$"))
  symbols[[7]] <- list("$l_{\\infty s}$", "$\\acute{k}_s$", "$t_{o s}$", "$\\acute{a}_s$",
                       "$\\acute{b}_s$", "$\\dot{a}_s$", "$\\dot{\\gamma}_s$")

  def_col <- unlist(secs) |>
    as_tibble()
  symbol_col <- unlist(symbols) |>
    as_tibble()

  xx <- symbol_col |>
    bind_cols(def_col)

  # Make header bold
  header <- paste0("\\textbf{", header, "}")
  names(xx) <- header

  # Make section labels bold
  sec_hdrs <- paste0("\\textbf{", sec_hdrs, "}")
  # Add section headers
  sec_lengths <- secs |> map_dbl(~{length(.x)})
  sec_inds <- c(1, sec_lengths)
  sec_inds <- sec_inds[-length(sec_inds)]
  sec_inds <- cumsum(sec_inds)

  for(i in seq_along(sec_hdrs)){
    xx <- hake::insert_row(xx,
                           c(paste0("\\textbf{", sec_hdrs[i],"}"), ""),
                           sec_inds[i] + i - 1)
  }

  k <- csas_table(xx,
                  align = rep("l", ncol(xx)),
                  col_names_align = rep("l", ncol(xx)),
                  format = "latex",
                  booktabs = TRUE,
                  linesep = "",
                  bold_header = bold_header,
                  ...)

  k
}
