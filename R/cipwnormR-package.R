#' @title library(cipwnormR)
#'
#' @description According to the IUGS (International Union of Geological Science), 
#' Subcommission on the Systematics of Igneous Rocks, the primary classification of 
#' igneous rocks must be based according to their modal mineral composition, expressed in volume percent. 
#' Nevertheless, where these data are not available or can not be determined owing to 
#' fine-grained mineral assemblage, glassy content or changes in the original mineralogy, 
#' then other criteria based on chemical bulk composition may be used.
#' 
#' Computed from the chemical composition, the normative mineralogy is an alternative 
#' approach for mineralogical classification and useful for set up the naming of 
#' igneous rocks (as parts of the TAS classification, \code{\link{TAS_plot}}). 
#' The CIPW Norm is the most commonly used calculation algorithm to estimate the
#' standard mineral assemblages for igneous rocks \code{\link{cipw}}, generated 
#' over more than a hundred years ago and thereafter modified by some authors to 
#' the passage of the years (e.g., Verma et al., 2002). It is based upon 
#' assumptions about the order of mineral formation and known phase 
#' relationships of rocks and minerals, using simplified mineral formulas. 
#'
#'In the option \code{\link{cipw_trace}}, the trace elements that can be used 
#'are Ba, Cl, Co, Cr Cs, F, Li, Ni, Rb, S, Sr, V and Zr. Additionally, the 
#'oxides concentrations of Cr2O3, NiO and SO3 (expressed as wt. percent) are 
#'handled like trace elements as well. However Cr2O3 are preferable to Cr and 
#'NiO to Ni. Sulfur concentration when available is reported as either SO3 or S, 
#'in which case their separate identity should be maintained.   
#'
#' @details A list of documented functions may be viewed by typing
#'     \code{help(package='cipwnormR')}.
#'
#' @name cipwnormR
#' @aliases cipwnormR-package
#' @docType package
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end

## mockable bindings: start
## mockable bindings: end
NULL
