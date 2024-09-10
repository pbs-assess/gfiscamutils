#' Create the ISCAM model parameter description table as found in the
#' Model Description appendix
#'
#' @param model An iscam model object (class [mdl_cls])
#' @return A [knitr::kable()] table
#' @export
table_iscam_param_desc <- function(model,
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
                    ifelse(fr(), "Indice pour l'âge", "Index for age"),
                    ifelse(fr(), "Indice pour l'année", "Index for year"),
                    ifelse(fr(), "Indice pour l'engrenage", "Index for gear"),
                    ifelse(fr(),
                           "Indice pour le bloc d'années dans la sélectivité variable dans le temps",
                           "Index for year block in time-varying selectivity"))
  symbols[[1]] <- list("$s$", "$a$", "$t$", "$k$", "$b$")

  sec_hdrs[2] <- ifelse(fr(),
                        "Dimensions du modèle",
                        "Model dimensions")
  secs[[2]] <- list(ifelse(fr(), "Nombre de sexes", "Number of sexes"),
                    ifelse(fr(),
                           "Classe d'âge la plus jeune et la plus âgée ($A$ est un groupe plus)",
                           "Youngest and oldest age group ($A$ is a plus group"),
                    ifelse(fr(),
                           "Première et dernière année des données de captures",
                           "First and last year of catch data"),
                    ifelse(fr(),
                           "Nombre d'engins, y compris les engins d'enquête",
                           "Number of gears, including survey gears"))
  symbols[[2]] <- list("$S$", "$\\acute{a}$, $A$", "$\\acute{t}$, $T$", "$K$")

  sec_hdrs[3] <- ifelse(fr(),
                        "Observations (données)",
                        "Observations (data)")
  secs[[3]] <- list(ifelse(fr(),
                           "captures en poids par engin $k$ en cours d'année $t$",
                           "catches in weight by gear $k$ in year $t$"),
                    ifelse(fr(),
                           "indice d'abondance relative pour l'engin $k$ en cours d'année $t$",
                           "relative abundance index for gear $k$ in year $t$"))
  symbols[[3]] <- list("$C_{k,t}$", "$I_{k,t}$")

  sec_hdrs[4] <- ifelse(fr(),
                        "Paramètres estimés",
                        "Estimated parameters")
  secs[[4]] <- list(ifelse(fr(),
                           "recrute dans des conditions non pêchées",
                           "recruits in unfished conditions"),
                    ifelse(fr(),
                        "Rugosité de la relation stock-recrutement",
                        "Steepness of the stock-recruitment relationship"),
                    ifelse(fr(),
                           "Âge moyen-$\\acute{a}$ recrutement à partir de l'année $\\acute{t}$ à $T$",
                           "Average age-$\\acute{a}$ recruitment from year $\\acute{t}$ to $T$"),
                    ifelse(fr(),
                           "Âge moyen-$\\acute{a}-1$ à $A$ recrutement pour l'initialisation",
                           "Average age-$\\acute{a} - 1$ to $A$ recruitment for initialization"),
                    ifelse(fr(),
                           "Taux instantané de mortalité naturelle pour le sexe $s$",
                           "Instantaneous natural mortality rate for sex $s$"),
                    ifelse(fr(),
                           "Paramètres de sélectivité pour l'engrenage $k$, sexe $s$, bloc d'années $b$",
                           "Selectivity parameters for gear $k$, sex $s$, year block $b$"),
                    ifelse(fr(),
                           linebreak("Logarithme de la mortalité instantanée par pêche pour l'engin $k$, \n le sexe $s$, l'année $t$"),
                           "Logarithm of the instantaneous fishing mortality for gear $k$, sex $s$, year $t$"),
                    ifelse(fr(),
                           "L'âge-$\\acute{a}$ s'écarte de $\\bar{R}$ depuis des années $\\acute{t}$ à $T$",
                           "Age-$\\acute{a}$ deviates from $\\bar{R}$ for years $\\acute{t}$ to $T$"),
                    ifelse(fr(),
                           "L'âge-$\\acute{a}$ s'écarte de $\\bar{R}_\\mathrm{init}$ depuis des années $\\acute{t}$",
                           "Age-$\\acute{a}$ deviates from $\\bar{R}_\\mathrm{init}$ for year $\\acute{t}$"),
                    ifelse(fr(),
                           "Paramètre de capturabilité pour l'enquête $k$",
                           "Catchability parameter for survey $k$"),
                    ifelse(fr(),
                           "Fraction de la variance totale associée à l'erreur d'observation",
                           "Fraction of the total variance associated with observation error"),
                    ifelse(fr(),
                           "Précision totale (inverse de la variance) de l'erreur totale",
                           "Total precision (inverse of variance) of the total error"))
  symbols[[4]] <- list("$R_0$", "$h$", "$\\bar{R}$", "$\\bar{R}_\\mathrm{init}$", "$M_s$",
                       "$\\hat{a}_{k,s,b}, \\hat{\\gamma}_{k,s,b}$", "$\\Gamma_{k,s,t}$",
                       "$\\omega_t$", "$\\omega_\\mathrm{init,t}$", "$q_k$", "$\\rho$", "$\\vartheta^2$")

  sec_hdrs[5] <- ifelse(fr(),
                        "Écarts types",
                        "Standard deviations")
  secs[[5]] <- list(ifelse(fr(),
                           "Écart-type pour les erreurs d'observation dans l'indice d'enquête",
                           "Standard deviation for observation errors in survey index"),
                    ifelse(fr(),
                           "Écart-type des erreurs de processus (écarts de recrutement)",
                           "Standard deviation in process errors (recruitment deviations)"),
                    ifelse(fr(),
                           "Écart-type des captures observées par engin",
                           "Standard deviation in observed catch by gear"))
  symbols[[5]] <- list("$\\sigma$", "$\\tau$", "$\\sigma_C$")

  sec_hdrs[6] <- ifelse(fr(),
                        "Résidus",
                        "Residuals")
  secs[[6]] <- list(ifelse(fr(),
                           "Recrutement annuel résiduel",
                           "Annual recruitment residual"),
                    ifelse(fr(),
                           "Erreur résiduelle dans les captures prévues",
                           "Residual error in predicted catch"))
  symbols[[6]] <- list("$\\delta_t$", "$\\eta_t$")

  sec_hdrs[7] <- ifelse(fr(),
                        linebreak("Croissance fixe \\& \n paramètres de maturité"),
                        linebreak("Fixed growth \\& maturity \n parameters"))
  secs[[7]] <- list(ifelse(fr(),
                           "Longueur asymptotique pour le sexe $s$",
                           "Asymptotic length for sex $s$"),
                    ifelse(fr(),
                           "Coefficient de croissance de Brody pour le sexe $s$",
                           "Brody growth coefficient for sex $s$"),
                    ifelse(fr(),
                           "Âge théorique à la longueur zéro pour le sexe $s$",
                           "Theoretical age at zero length for sex $s$"),
                    ifelse(fr(),
                           "Scalaire dans l'allométrie longueur-poids pour le sexe $s$",
                           "Scalar in length-weight allometry for sex $s$"),
                    ifelse(fr(),
                           "Paramètre de puissance dans l'allométrie longueur-poids pour le sexe $s$",
                           "Power parameter in length-weight allometry for sex $s$"),
                    ifelse(fr(),
                           "Âge à 50 \\% pour la maturité sexuelle $s$",
                           "Age at 50\\% maturity for sex $s$"),
                    ifelse(fr(),
                           "Écart-type à 50 \\% de maturité pour le sexe $s$",
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
                  format = "latex",
                  align = rep("l", ncol(xx)),
                  col_names_align = rep("l", ncol(xx)),
                  ...)

  k
}
