#' Data collection of igneous rocks from Central Andes
#' 
#' Compilation of more than 1500 major and trace element analysis of igneous rocks from Central Andes.
#' 
#' @format A `data.frame` with 1511 rows and 40 columns.
#' 
#' @details
#' This data set documents compositional variations of magmas since Jurassic time to Neogene period in the Andean orogeny.
#' 
#' @docType data
#' 
#' @usage data(Andes)
#' 
#' @source 
#' The database is also available in GEOROC (Geochemistry of Rocks of the Oceans and Continents). For full details see \url{http://georoc.mpch-mainz.gwdg.de/georoc/} . 
#' 
#' @references
#' Mamani, M., Worner, G., and Sempere, T. 2010. Geochemical variations in igneous rocks of the Central Andean orocline (13 S to 18 S): Tracing crustal thickening and magma generation through time and space. Bulletin of  Geological Society of America. v. 122, pp. 162--182.
#' 
#' @keywords datasets
#' @family datasets
#' @examples
#' data(Andes)
#' head(Andes)
"Andes"

#' The standard atomic weights
#' 
#' Data frame with symbols, atom names and standard atomic weights.
#' 
#' @format A data.frame with 92 rows and 3 columns, including the row names.
#' 
#' @details
#' This table is based on the 2011 table after the IUPAC (International Union of Pure and Applied Chemistry), Commission on Isotopic Abundances and Atomic Weights. It is important to note that row names are the symbol of the elements. 
#' 
#' @docType data
#' 
#' @usage data(AtomWeight)
#' 
#' @source 
#' \url{http://www.chem.qmul.ac.uk/iupac/AtWt/}
#'  
#' @references
#' Wieser, M.E., and others. 2013. Pure and Applied Chemistry. International Union of Pure and Applied Chemistry (IUPAC). v. 78, no. 11, pp. 2051--2066. 
#' 
#' @keywords datasets
#' @family datasets
#' @examples
#' data(AtomWeight)
#' AtomWeight['H', 'AWeight']
"AtomWeight"

#' The oxides molecular weights of normative minerals
#' 
#' The oxides molecular weights of normative minerals
#' 
#' @format A `data.frame` with 36 rows and 6 columns, including the row names.
#' 
#' @details
#' This table is calculated from the 2011 table values after the IUPAC (International Union of Pure and Applied Chemistry), Commission on Isotopic Abundances and Atomic Weights. It is important to note that row names are the name of the normative minerals. 
#' @docType data
#' 
#' @usage data(MinWeight)
#' 
#' @references
#' Wieser, M.E., and others. 2013. Pure and Applied Chemistry. International Union of Pure and Applied Chemistry (IUPAC). v. 78, no. 11, pp. 2051--2066. 
#' 
#' @keywords datasets
#' @family datasets
#' @examples
#' data(MinWeight)
#' MinWeight['Quartz','ConsWeight']
"MinWeight"

#' The molecular weights
#' 
#' Data frame with chemical nomenclature and molecular weights used in CIPW computations.
#' 
#' @format A `data.frame` with 26 rows and 3 columns, including the row names
#' 
#' @details
#' This table is calculated from the 2011 table values after the IUPAC (International Union of Pure and Applied Chemistry), Commission on Isotopic Abundances and Atomic Weights. It is important to note that row names are the chemical nomenclature of the oxides and the third column is the oxide weight rounded. 
#' @docType data
#' 
#' @usage data(OxiWeight)
#' 
#' @references
#' Wieser, M.E., and others. 2013. Pure and Applied Chemistry. International Union of Pure and Applied Chemistry (IUPAC). v. 78, no. 11, pp. 2051--2066. 
#' 
#' @keywords datasets
#' @family datasets
#' @examples
#' data(OxiWeight)
#' OxiWeight['SiO2','OWeight']
"OxiWeight"

#' Data collection of igneous rocks used in IUGSTAS software
#' 
#' Compilation of major elements data for 37 representative samples of volcanic rocks.
#' 
#' @format A `data.frame` with 37 rows and 20 columns
#' 
#' @details
#' This data is available from the Cambridge University Press, as a part of IUGSTAS software. 
#' @docType data
#' 
#' @usage data(TestTAS)
#' 
#' @references
#' Le Maitre, R.W et al. 2002. Igneous Rocks: A Classification and Glossary of Terms: Recommendations of the International Union of Geological Sciences, Subcommission on the Systematics of Igneous Rocks. Cambridge University Press, 2002, 252 pp.
#' 
#' @keywords datasets
#' @family datasets
#' @examples
#' data(TestTAS)
#' head(TestTAS)
"TestTAS"