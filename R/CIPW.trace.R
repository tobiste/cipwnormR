#####################################################################
#                         CIPW trace norm                           #
#                       (Verma, 2002, 2003)                         #
#####################################################################

#' CIPW trace norm
#'
#' CIPW norm including minor and trace elements after Verma (2002, 2003).
#' Trace cations substitute for the major ones (Mn, Ni, Co for Fe; Sr, Ba for
#' Ca; Rb, Cs for K; Li for Na; V for Cr) and additional minerals are
#' calculated (zircon, F-apatite, fluorite, halite, thenardite, pyrite,
#' chromite).
#'
#' @inheritParams adj_rock
#' @param x data.frame of analyses. Major oxides as in \code{\link{adj_rock}}
#'   (wt\%); optional Cr2O3, NiO, SO3 (wt\%) and Ba, Cl, Co, Cr, Cs, F (or
#'   "F."), Li, Ni, Rb, S, Sr, V, Zr (ppm). Missing columns are treated as 0.
#'
#' @returns data.frame with Magma, Rock.TAS, Fe.ratio, the anhydrous oxides
#'   including traces (wt\%), the normative minerals (wt\%) and derived
#'   parameters. Zeros are returned as \code{NA}.
#' @export
#' 
#' @family cipw
#'
#' @examples
#' data(Andes)
#' cipw_trace(Andes)
cipw_trace <- function(x, Volcanic = TRUE, AdjTAS = TRUE, Cancrinite = FALSE,
                       Calcite = FALSE, digits = Inf) {
  DATA <- as.data.frame(x)
  n <- nrow(DATA)
  wt <- .cipw_weights()
  OW <- wt$OxiWeight; AW <- wt$AtomWeight; MW <- wt$MinWeight
  O  <- AW["O", "AWeight"]
  
  getcol <- function(nm) {
    nm <- intersect(nm, names(DATA))
    if (!length(nm)) return(numeric(n))
    v <- as.numeric(DATA[[nm[1L]]])
    v[is.na(v)] <- 0
    v
  }
  
  ## ------------------------------------- Fe ratio and TAS (adj_rock) --
  fe <- adj_rock(DATA, Volcanic = Volcanic, AdjTAS = AdjTAS,
                Cancrinite = Cancrinite, Calcite = Calcite, digits = Inf)
  
  ## ---------------------------------------------- raw composition --
  maj <- c("SiO2", "TiO2", "Al2O3", "Fe2O3", "FeO", "MnO", "MgO", "CaO",
           "Na2O", "K2O", "P2O5")
  X <- vapply(maj, getcol, numeric(n))
  if (n == 1L) X <- matrix(X, 1L, dimnames = list(NULL, maj))
  co2 <- if (isTRUE(Cancrinite) || isTRUE(Calcite)) getcol(c("CO2", "CO2.")) else numeric(n)
  
  # ppm element -> wt% oxide (k = cations per formula unit)
  ppm2ox <- function(el, oxide, k) getcol(el) * OW[oxide, "OWeight"] / (k * AW[el, "AWeight"]) / 1e4
  cr2o3 <- getcol("Cr2O3"); cr2o3 <- ifelse(cr2o3 > 0, cr2o3, ppm2ox("Cr", "Cr2O3", 2))
  nio   <- getcol("NiO");   nio   <- ifelse(nio > 0, nio, ppm2ox("Ni", "NiO", 1))
  
  X <- cbind(X, CO2 = co2,
             Cr2O3 = cr2o3, NiO = nio, SO3 = getcol("SO3"),
             Cl = getcol("Cl") / 1e4, F = getcol(c("F", "F.")) / 1e4, S = getcol("S") / 1e4,
             BaO = ppm2ox("Ba", "BaO", 1), CoO = ppm2ox("Co", "CoO", 1),
             Cs2O = ppm2ox("Cs", "Cs2O", 2), Li2O = ppm2ox("Li", "Li2O", 2),
             Rb2O = ppm2ox("Rb", "Rb2O", 2), SrO = ppm2ox("Sr", "SrO", 1),
             V2O3 = ppm2ox("V", "V2O3", 2), ZrO2 = ppm2ox("Zr", "ZrO2", 1))
  
  ## ---------------------------------------- Fe split (Pruseth 2009) --
  R <- fe$Fe.ratio
  Fe2O3T <- X[, "Fe2O3"] + 1.111348 * X[, "FeO"]
  X[, "FeO"]   <- Fe2O3T / (R + 1.111348)
  X[, "Fe2O3"] <- R * X[, "FeO"]
  
  ## ------------------------------------------------ anhydrous recast --
  W <- X * (100 / rowSums(X))
  W[!is.finite(W)] <- 0
  
  ## ---------------------------------------------- mole proportions --
  oxc <- setdiff(colnames(W), c("Cl", "F", "S"))
  M <- W[, oxc, drop = FALSE] / rep(OW[oxc, "OWeight"], each = n)
  M <- cbind(M, Cl = W[, "Cl"] / AW["Cl", "AWeight"], F = W[, "F"] / AW["F", "AWeight"],
             S = W[, "S"] / AW["S", "AWeight"])
  
  # combined cations and their mean molecular weights
  grp <- function(main, subs) {
    tot <- rowSums(M[, c(main, subs), drop = FALSE])
    x   <- M[, c(main, subs), drop = FALSE] / ifelse(tot > 0, tot, 1)
    list(mol = tot, mw = drop(x %*% OW[c(main, subs), "OWeight"]))
  }
  FE <- grp("FeO", c("MnO", "NiO", "CoO"))
  CA <- grp("CaO", c("SrO", "BaO"))
  K  <- grp("K2O", c("Rb2O", "Cs2O"))
  NA_ <- grp("Na2O", "Li2O")
  CR <- grp("Cr2O3", "V2O3")
  # default weights when the group is absent (only matters for 0 * w)
  feo.corr <- ifelse(FE$mol > 0, FE$mw, OW["FeO", "OWeight"])
  cao.corr <- ifelse(CA$mol > 0, CA$mw, OW["CaO", "OWeight"])
  k2o.corr <- ifelse(K$mol > 0, K$mw, OW["K2O", "OWeight"])
  na2o.corr <- ifelse(NA_$mol > 0, NA_$mw, OW["Na2O", "OWeight"])
  cr2o3.corr <- ifelse(CR$mol > 0, CR$mw, OW["Cr2O3", "OWeight"])
  fe.corr <- feo.corr - O
  ca.corr <- cao.corr - O
  na.corr <- (na2o.corr - O) / 2
  
  feo <- FE$mol; cao <- CA$mol; k2o <- K$mol; na2o <- NA_$mol; cr2o3 <- CR$mol
  f <- M[, "F"]; cl <- M[, "Cl"]; s <- M[, "S"]; so3 <- M[, "SO3"]
  
  ## ------------------------------------------------------ allocation --
  # Zircon
  Z <- M[, "ZrO2"]
  
  # Apatite (F-apatite first, rest Ca-apatite)
  Ap <- pmin(M[, "P2O5"], 0.3 * cao)
  cao <- pmax(cao - Ap * 10 / 3, 0)
  FREE_P2O5 <- M[, "P2O5"] - Ap
  fAp <- pmin(f, 2 / 3 * Ap); f <- f - fAp
  ApCaF <- 1.5 * fAp
  ApCaO <- Ap - ApCaF
  FREEO_12 <- fAp / 2
  
  # Fluorite
  Fr <- pmin(f / 2, cao); cao <- cao - Fr
  FREE_F <- f - 2 * Fr
  FREEO_13 <- Fr
  
  # Halite (1 Na2O -> 2 NaCl)
  Hl <- pmin(cl, 2 * na2o); na2o <- na2o - Hl / 2
  FREE_Cl <- cl - Hl
  FREEO_14 <- Hl / 2
  
  # Thenardite
  Th <- pmin(na2o, so3); na2o <- na2o - Th
  FREE_SO3 <- so3 - Th
  
  # Pyrite
  Pr <- pmin(s / 2, feo); feo <- feo - Pr
  FREE_S <- s - 2 * Pr
  FREEO_15 <- Pr
  
  # Carbonates
  cb <- .cipw_carbonates(na2o, cao, M[, "CO2"], Cancrinite, Calcite)
  
  # Chromite
  Cm <- pmin(feo, cr2o3); feo <- feo - Cm
  FREE_Cr2O3 <- cr2o3 - Cm
  
  # Silicates, oxides, desilication (zircon consumes 1 SiO2)
  sl <- .cipw_silicates(M[, "SiO2"], M[, "TiO2"], M[, "Al2O3"], M[, "Fe2O3"], feo,
                        M[, "MgO"], cb$cao, cb$na2o, k2o, Y = Z)
  
  ## ---------------------------------------- normative minerals (wt%) --
  m3 <- function(nm) MW[nm, 3]
  m4 <- function(nm) MW[nm, 4]
  NM <- with(sl, cbind(
    Q = Q * m3("Quartz"), C = C * m3("Corundum"),
    Or = Or * (k2o.corr + m4("Orthoclase")),
    Ab = Ab * (na2o.corr + m4("Albite")),
    An = An * (cao.corr + m4("Anorthite")),
    Ne = Ne * (na2o.corr + m4("Nepheline")),
    Lc = Lc * (k2o.corr + m4("Leucite")),
    Kp = Kp * (k2o.corr + m4("Kaliophilite")),
    Z = Z * m3("Zircon"),
    Ac = Ac * (na2o.corr + m4("Acmite")),
    Ns = Ns * (na2o.corr + m4("Sodium metasilicate")),
    Ks = Ks * (k2o.corr + m4("Potassium metasilicate")),
    Di.Mg = Di * xmgr * (cao.corr + m4("Diopside-Mg")),
    Di.Fe = Di * xfer * (cao.corr + feo.corr + m4("Diopside-Ca")),
    Wo = Wo * (cao.corr + m4("Wollastonite")),
    En = Hy * xmgr * m3("Hypersthene-Mg"),
    Fs = Hy * xfer * (feo.corr + m4("Hypersthene-Fe")),
    Fo = Ol * xmgr * m3("Olivine-Mg"),
    Fa = Ol * xfer * (2 * feo.corr + m4("Olivine-Fe")),
    Dcs = Cs * (2 * cao.corr + m4("Dicalcium silicate")),
    Mt = Mt * (feo.corr + m4("Magnetite")),
    Il = Il * (feo.corr + m4("Ilmenite")),
    Hm = Hm * m3("Hematite"),
    Tn = Tn * (cao.corr + m4("Sphene")),
    Pf = Pf * (cao.corr + m4("Perovskite")),
    Ru = Ru * m3("Rutile"),
    `Ap-F` = ApCaF * (3 * cao.corr + ca.corr / 3 + m4("Apatite-F")),
    `Ap-Ca` = ApCaO * (10 / 3 * cao.corr + m4("Apatite-Ca")),
    Fr = Fr * (ca.corr + m4("Fluorite")),
    NaCl = Hl * (na.corr + AW["Cl", "AWeight"]),
    Th = Th * (na2o.corr + m4("Thenardite")),
    Pr = Pr * (fe.corr + 2 * AW["S", "AWeight"]),
    Nc = cb$Nc * (na2o.corr + m4("Sodium Carbonate")),
    Cc = cb$Cc * (cao.corr + m4("Calcite")),
    Cm = Cm * (feo.corr + cr2o3.corr),
    DEFSIO2 = -DEFSIO2 * OW["SiO2", "OWeight"],
    FREE_P2O5 = FREE_P2O5 * OW["P2O5", "OWeight"],
    FREE_F = FREE_F * AW["F", "AWeight"],
    FREE_Cl = FREE_Cl * AW["Cl", "AWeight"],
    FREE_SO3 = FREE_SO3 * OW["SO3", "OWeight"],
    FREE_S = FREE_S * AW["S", "AWeight"],
    FREE_CO2 = cb$FREE_CO2 * OW["CO2", "OWeight"],
    FREE_Cr2O3 = FREE_Cr2O3 * cr2o3.corr,
    # oxygen displaced by F, Cl and S (closes the mass balance)
    FREEO = O * (FREEO_12 + FREEO_13 + FREEO_14 + FREEO_15)
  ))
  NM <- cbind(NM, Sums.min = rowSums(NM))
  
  ## ------------------------------------------------------ parameters --
  P <- .cipw_parameters(W, M, NM, DensityRock(NM, MW))
  
  ## ---------------------------------------------------------- output --
  tr <- c("Cr2O3", "NiO", "SO3", "Cl", "F", "S", "BaO", "CoO", "Cs2O",
          "Li2O", "Rb2O", "SrO", "V2O3", "ZrO2")
  Wout <- W
  colnames(Wout) <- paste0(colnames(W), ".adj")
  out <- data.frame(fe[c("Magma", "Rock.TAS", "Fe.ratio")], Wout,
                    Total.adj = rowSums(W), Total.trace = rowSums(W[, tr, drop = FALSE]),
                    NM, P, check.names = FALSE)
  out <- .cipw_finish(out, digits)
  rownames(out) <- rownames(fe)
  out
}