#' Recalculate major elements on an anhydrous basis with Fe-oxidation adjustment
#'
#' Major oxides are recast to 100 \% (volatile-free). If only one of Fe2O3/FeO
#' is reported, total iron is split using either the TAS-based ratios of
#' Middlemost (1989) or the equations of Le Maitre (1976); the split follows
#' Pruseth (2009).
#'
#' @param x data.frame (or matrix) of whole-rock analyses in wt\%. Required columns:
#'   SiO2, TiO2, Al2O3, MnO, MgO, CaO, Na2O, K2O, P2O5 and at least one of
#'   Fe2O3/FeO. Optional: CO2 (or "CO2."), Sample (used as row names).
#' @param Volcanic logical (recycled to the number of samples). Volcanic or
#'   plutonic equations/names.
#' @param AdjTAS logical (recycled). \code{TRUE}: Middlemost (1989) TAS ratios;
#'   \code{FALSE}: Le Maitre (1976) equations.
#' @param Cancrinite logical. Allocate CO2 to sodium carbonate (cancrinite)?
#' @param Calcite logical. Allocate CO2 to calcite?
#' @param digits integer. Number of digits to round the output (no
#'   intermediate rounding).
#'
#' @return Returns a dataset of adjust oxide data on anhydrous basis: data.frame with Magma, Rock.TAS, Fe.ratio (Fe2O3/FeO, wt), the
#'   adjusted oxides and Total.adj. Zero values are returned as \code{NA}.
#'
#' @references
#' Le Maitre, R.W. 1976. Some problems of the projection of chemical data into mineralogical classifications. Contribution Mineralogical Petrology. v. 56, pp. 181--189.
#'
#' Middlemost, E.A.K. 1989. Iron oxidation ratios, norms and the classification of volcanic rocks. Chemical Geology. v. 77, pp. 19--26.
#'
#' Pruseth, K.L. (2009) J. Geol. Soc. India 73, 715-720.
#'
#' @export
#'
#' @examples
#' data(TestTAS)
#' adj_rock(TestTAS, digits = 3)
adj_rock <- function(x, Volcanic = TRUE, AdjTAS = TRUE,
                    Cancrinite = FALSE, Calcite = FALSE, digits = Inf) {
  DATA <- as.data.frame(x)
  n <- nrow(DATA)
  
  ## ---------------------------------------------------------------- input --
  ox <- c("SiO2", "TiO2", "Al2O3", "Fe2O3", "FeO", "MnO", "MgO", "CaO",
          "Na2O", "K2O", "P2O5")
  miss <- setdiff(ox, names(DATA))
  if (all(c("Fe2O3", "FeO") %in% miss)) stop("need at least one of Fe2O3 or FeO")
  miss <- setdiff(miss, c("Fe2O3", "FeO"))
  if (length(miss)) stop("missing column(s): ", paste(miss, collapse = ", "))
  
  getcol <- function(nm) {
    nm <- intersect(nm, names(DATA))
    if (!length(nm)) return(numeric(n))
    v <- as.numeric(DATA[[nm[1L]]])
    v[is.na(v)] <- 0
    v
  }
  X <- do.call(cbind, lapply(ox, getcol))
  co2 <- if (isTRUE(Cancrinite) || isTRUE(Calcite)) getcol(c("CO2", "CO2.")) else numeric(n)
  X <- cbind(X, co2)
  colnames(X) <- c(ox, "CO2")
  
  # anhydrous recast (LOI is not part of the oxide sum, so it is not needed)
  recast <- function(M) M * (100 / rowSums(M))
  N <- recast(X)
  
  S  <- N[, "SiO2"]
  NK <- N[, "Na2O"] + N[, "K2O"]
  TAS <- cbind(S, NK)
  
  ## ------------------------------------------------ TAS (Middlemost 1989) --
  # list(volcanic name, plutonic name, Fe2O3/FeO, polygon); later fields win
  # on shared boundaries (same order as before)
  fld <- function(v, p, r, a, b) list(v = v, p = p, r = r, poly = cbind(a, b))
  fields <- list(
    fld("Rhyolite", "Granite", 0.50, c(77, 100, 100, 69, 69), c(0, 0, 25, 25, 8)),
    fld("Phonolite", "Nepheline Syenite", 0.50, c(57.6, 69, 30), c(11.7, 17.73, 24.15)),
    fld("Trachyte/Trachydacite", "Quartz Monzonite", 0.50, c(63, 69, 69, 57.6), c(7, 8, 17.73, 11.7)),
    fld("Dacite", "Granodiorite", 0.40, c(63, 77, 69, 63), c(0, 0, 8, 7)),
    fld("Tephriphonolite", "Foid Syenite", 0.40, c(53, 57.6, 52.5, 48.4), c(9.3, 11.7, 14, 11.5)),
    fld("Trachyandesite", "Monzonite", 0.40, c(57, 63, 57.6, 53), c(5.9, 7, 11.7, 9.3)),
    fld("Andesite", "Diorite", 0.35, c(57, 63, 63, 57), c(0, 0, 7, 5.9)),
    fld("Phonotephrite", "Foid Syenite", 0.35, c(49.4, 53, 48.4, 45), c(7.3, 9.3, 11.5, 9.4)),
    fld("Basaltictrachyandesite", "Monzodiorite", 0.35, c(52, 57, 53, 49.4), c(5, 5.9, 9.3, 7.3)),
    fld("Basalticandesite", "Gabbrodiorite", 0.30, c(52, 57, 57, 52), c(0, 0, 5.9, 5)),
    # Tephrite/Basanite split at NK = 6; split point now lies exactly on the
    # Trachybasalt boundary (45,5)-(49.4,7.3), i.e. x = 45 + 4.4/2.3
    fld("Tephrite/Basanite", "Foid Monzogabbro/Diorite", 0.30,
        c(41, 41, 45, 49.4, 45 + 4.4 / 2.3), c(6, 7, 9.4, 7.3, 6)),
    fld("Trachybasalt", "Monzogabbro", 0.30, c(45, 52, 49.4), c(5, 5, 7.3)),
    fld("Basalt", "Gabbro", 0.20, c(45, 45, 52, 52), c(0, 5, 5, 0)),
    fld("Tephrite/Basanite", "Foid Monzogabbro/Diorite", 0.20,
        c(41, 41, 45 + 4.4 / 2.3, 45, 45), c(3, 6, 6, 5, 3)),
    fld("Picrobasalt", "Gabbro", 0.15, c(41, 45, 45, 41), c(0, 0, 3, 3)),
    # Foidite split at NK = 10; split point now lies on the Phonotephrite
    # boundary (45,9.4)-(48.4,11.5), i.e. x = 45 + 0.6 * 3.4/2.1, and the
    # lower part follows the vertex (45, 9.4)
    fld("Foidite", "Foidite", 0.40, c(45 + 0.6 * 3.4 / 2.1, 48.4, 52.5, 30, 0, 0),
        c(10, 11.5, 14, 24.15, 24.15, 10)),
    fld("Foidite", "Foidite", 0.30, c(0, 41, 45, 45 + 0.6 * 3.4 / 2.1, 0), c(7, 7, 9.4, 10, 10)),
    fld("Foidite", "Foidite", 0.20, c(0, 41, 41, 0), c(3, 3, 7, 7)),
    fld("Foidite", "Ijolite", 0.15, c(0, 41, 41, 0), c(0, 0, 3, 3))
  )
  
  R.Fe <- rep(NA_real_, n)
  rock.v <- rock.p <- rep(NA_character_, n)
  for (f in fields) {
    i <- which(pnt_in_poly(TAS, f$poly)$pip)   # which() drops NA (empty rows)
    R.Fe[i] <- f$r
    rock.v[i] <- f$v
    rock.p[i] <- f$p
  }
  
  ## ------------------------------------------------------------ magma type --
  Magma <- as.character(cut(S, c(-Inf, 45, 52, 66, Inf), right = FALSE,
                            labels = c("Ultrabasic", "Basic", "Intermediate", "Hypersilicic")))
  
  ## -------------------------------------------- Le Maitre (1976) equations --
  # these give FeO/(FeO + Fe2O3), converted here to Fe2O3/FeO
  volc <- rep_len(as.logical(Volcanic), n)
  adj  <- rep_len(as.logical(AdjTAS), n)
  r <- ifelse(volc, 0.93 - 0.0042 * S - 0.022 * NK,
              0.88 - 0.0016 * S - 0.027 * NK)
  Ox.Fe <- (1 - r) / r
  
  ## ------------------------------------------------------------- Fe ratio --
  Fe.ratio <- ifelse(adj & !is.na(R.Fe), R.Fe, Ox.Fe)  # unclassified -> Le Maitre
  meas <- X[, "Fe2O3"] > 0 & X[, "FeO"] > 0             # both analysed: keep them
  Fe.ratio[meas] <- X[meas, "Fe2O3"] / X[meas, "FeO"]
  
  ## ------------------------------------------- Fe split (Pruseth 2009) --
  # done on the raw values, so all oxides share the same basis before recast
  Fe2O3T <- X[, "Fe2O3"] + 1.111348 * X[, "FeO"]
  X[, "FeO"]   <- Fe2O3T / (Fe.ratio + 1.111348)
  X[, "Fe2O3"] <- Fe.ratio * X[, "FeO"]
  
  Y <- recast(X)
  colnames(Y) <- paste0(colnames(Y), ".adj")
  
  ## --------------------------------------------------------------- output --
  out <- data.frame(Magma = Magma,
                    Rock.TAS = ifelse(volc, rock.v, rock.p),
                    Fe.ratio = round(Fe.ratio, digits),
                    round(Y, digits),
                    Total.adj = round(rowSums(Y), digits),
                    stringsAsFactors = FALSE, check.names = FALSE)
  num <- vapply(out, is.numeric, logical(1L))
  out[num] <- lapply(out[num], function(v) replace(v, v == 0, NA))
  rownames(out) <- if (!is.null(DATA$Sample)) make.unique(as.character(DATA$Sample)) else NULL
  out
}
