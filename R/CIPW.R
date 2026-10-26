#####################################################################
#                           CIPW norm                               #
#                       (Verma, 2002, 2003)                         #
#####################################################################

# cache for the weight tables (loaded once per session)
.cipw_cache <- new.env(parent = emptyenv())

.cipw_weights <- function() {
  if (is.null(.cipw_cache$OxiWeight)) {
    utils::data(list = c("AtomWeight", "MinWeight", "OxiWeight"), envir = .cipw_cache)
  }
  .cipw_cache
}

.sdiv <- function(a, b) ifelse(b > 0, a / b, 0)   # safe division (0/0 -> 0)

#--------------------------------------------------------------------
# shared internals
#--------------------------------------------------------------------

# Sodium carbonate (cancrinite) then calcite; CO2 consumed in turn
.cipw_carbonates <- function(na2o, cao, co2, Cancrinite, Calcite) {
  Nc <- if (isTRUE(Cancrinite)) pmin(na2o, co2) else 0 * co2
  na2o <- na2o - Nc; co2 <- co2 - Nc
  Cc <- if (isTRUE(Calcite)) pmin(cao, co2) else 0 * co2
  list(Nc = Nc, Cc = Cc, na2o = na2o, cao = cao - Cc, FREE_CO2 = co2 - Cc)
}

# Ilmenite ... Kaliophilite: allocation and desilication (moles).
# Y = SiO2 already required by earlier minerals (e.g. zircon)
.cipw_silicates <- function(sio2, tio2, al2o3, fe2o3, feo, mgo, cao, na2o, k2o, Y = 0) {
  Il  <- pmin(feo, tio2);    feo <- feo - Il;       tio2 <- tio2 - Il
  Orp <- pmin(al2o3, k2o);   al2o3 <- al2o3 - Orp;  Ks <- k2o - Orp
  Abp <- pmin(al2o3, na2o);  al2o3 <- al2o3 - Abp;  na2o <- na2o - Abp
  Ac  <- pmin(na2o, fe2o3);  fe2o3 <- fe2o3 - Ac;   Ns <- na2o - Ac
  An  <- pmin(al2o3, cao);   cao <- cao - An;       C <- al2o3 - An
  Tnp <- pmin(cao, tio2);    cao <- cao - Tnp;      Ru <- tio2 - Tnp
  Mt  <- pmin(fe2o3, feo);   feo <- feo - Mt;       Hm <- fe2o3 - Mt
  
  femg <- feo + mgo
  xfer <- .sdiv(feo, femg)
  xmgr <- .sdiv(mgo, femg)
  Dip <- pmin(cao, femg); Wop <- cao - Dip; Hyp <- femg - Dip
  
  Y <- Y + 6 * Orp + Ks + 6 * Abp + 4 * Ac + Ns + 2 * An + Tnp + 2 * Dip + Wop + Hyp
  Q <- pmax(sio2 - Y, 0)
  D <- pmax(Y - sio2, 0)
  
  Ol <- pmin(D, Hyp / 2); Hy <- Hyp - 2 * Ol; D <- pmax(D - Ol, 0)       # Hy -> Ol
  Pf <- pmin(D, Tnp);     Tn <- Tnp - Pf;     D <- pmax(D - Pf, 0)       # Tn -> Pf
  Ne <- pmin(D / 4, Abp); Ab <- Abp - Ne;     D <- pmax(D - 4 * Ne, 0)   # Ab -> Ne
  Lc <- pmin(D / 2, Orp); Or <- Orp - Lc;     D <- pmax(D - 2 * Lc, 0)   # Or -> Lc
  Cs <- pmin(D, Wop / 2); Wo <- Wop - 2 * Cs; D <- pmax(D - Cs, 0)       # Wo -> Cs
  d  <- pmin(D, Dip)                                                     # Di -> Cs + Ol
  Cs <- Cs + d / 2; Ol <- Ol + d / 2; Di <- Dip - d; D <- pmax(D - d, 0)
  Kp <- pmin(D / 2, Lc);  Lc <- Lc - Kp;      D <- pmax(D - 2 * Kp, 0)   # Lc -> Kp
  
  list(Q = Q, C = C, Or = Or, Ab = Ab, An = An, Ne = Ne, Lc = Lc, Kp = Kp,
       Ac = Ac, Ns = Ns, Ks = Ks, Di = Di, Wo = Wo, Hy = Hy, Ol = Ol, Cs = Cs,
       Mt = Mt, Il = Il, Hm = Hm, Tn = Tn, Pf = Pf, Ru = Ru, DEFSIO2 = D,
       xfer = xfer, xmgr = xmgr)
}

# Derived parameters. W: wt% matrix, M: molar proportions (pure oxides),
# NM: normative minerals (wt%), dens: rock density
.cipw_parameters <- function(W, M, NM, dens) {
  g <- function(o) if (o %in% colnames(NM)) NM[, o] else 0
  FeOt <- W[, "FeO"] + 0.899809 * W[, "Fe2O3"]
  KN <- .sdiv(W[, "K2O"], W[, "Na2O"])
  alk <- ifelse(W[, "SiO2"] > 50 & KN > 1 & KN < 2.5,
                2 * W[, "Na2O"], W[, "Na2O"] + W[, "K2O"])
  A  <- M[, "Al2O3"]; NK <- M[, "Na2O"] + M[, "K2O"]
  data.frame(
    Salic = g("Q") + g("Or") + g("Ab") + g("An"),
    Femic = g("Di.Mg") + g("Di.Fe") + g("En") + g("Fs") + g("Fo") + g("Fa") +
      g("Mt") + g("Il") + g("Hm"),
    C.I. = g("An") + 2.1570577 * g("Di.Mg") + g("Fo") + 0.7007616 * g("En"),
    D.I. = g("Q") + g("Or") + g("Ab") + g("Ne") + g("Lc") + g("Kp"),
    S.I. = 100 * W[, "MgO"] / (W[, "MgO"] + W[, "Fe2O3"] + W[, "FeO"] + W[, "Na2O"] + W[, "K2O"]),
    A.R. = (W[, "Al2O3"] + W[, "CaO"] + alk) / (W[, "Al2O3"] + W[, "CaO"] - alk),
    Mg.number = 100 * M[, "MgO"] / (M[, "MgO"] + M[, "FeO"]),
    Fe.index = FeOt / (FeOt + W[, "MgO"]),
    MALI = W[, "Na2O"] + W[, "K2O"] - W[, "CaO"],
    ACNK = A / (M[, "CaO"] - (10 / 3) * M[, "P2O5"] + NK),
    ANK = A / NK,
    AI = A - NK,
    FSSI = (g("Q") - (g("Lc") + 2 * (g("Ne") + g("Kp")))) / 100,
    Density.rock = dens
  )
}

# round, turn 0 / non-finite into NA (numeric columns only)
.cipw_finish <- function(out, digits) {
  num <- vapply(out, is.numeric, logical(1L))
  out[num] <- lapply(out[num], function(v) {
    v <- round(v, digits)
    v[!is.finite(v) | v == 0] <- NA
    v
  })
  out
}

#' CIPW norm
#'
#' CIPW normalization after Verma (2002, 2003)
#'
#' @inheritParams adj_rock
#' @param x data.frame of major element analyses (see \code{\link{adj_rock}})
#'
#' @returns data.frame with the adjusted major elements (\code{adj_rock}),
#'   the normative minerals (wt\%) and derived parameters. Zeros are
#'   returned as \code{NA}.
#'
#' @export
#' @family cipw
#'
#' @examples
#' data(Andes)
#' cipw(Andes)
cipw <- function(x, Volcanic = TRUE, AdjTAS = TRUE, Cancrinite = FALSE,
                 Calcite = FALSE, digits = Inf) {
  maj <- adj_rock(x, Volcanic = Volcanic, AdjTAS = AdjTAS,
                 Cancrinite = Cancrinite, Calcite = Calcite, digits = Inf)
  
  ox <- c("SiO2", "TiO2", "Al2O3", "Fe2O3", "FeO", "MnO", "MgO", "CaO",
          "Na2O", "K2O", "P2O5", "CO2")
  W <- as.matrix(maj[paste0(ox, ".adj")])
  W[is.na(W)] <- 0
  colnames(W) <- ox
  
  wt <- .cipw_weights()
  OW <- wt$OxiWeight
  MW <- wt$MinWeight
  
  ## mole proportions
  M <- W / rep(OW[ox, "Rounded"], each = nrow(W))
  feo  <- M[, "FeO"] + M[, "MnO"]                # FeO* = FeO + MnO
  xmno <- .sdiv(M[, "MnO"], feo)
  xfeo <- .sdiv(M[, "FeO"], feo)
  
  ## apatite
  Ap <- pmin(M[, "P2O5"], 0.3 * M[, "CaO"])
  cao <- pmax(M[, "CaO"] - Ap * 10 / 3, 0)
  FREE_P2O5 <- M[, "P2O5"] - Ap
  
  ## carbonates
  cb <- .cipw_carbonates(M[, "Na2O"], cao, M[, "CO2"], Cancrinite, Calcite)
  
  ## silicates, oxides, desilication
  s <- .cipw_silicates(M[, "SiO2"], M[, "TiO2"], M[, "Al2O3"], M[, "Fe2O3"], feo,
                       M[, "MgO"], cb$cao, cb$na2o, M[, "K2O"])
  
  ## normative minerals (wt%)
  mw  <- function(nm) MW[nm, 3]
  feM <- OW["FeO", "OWeight"] * xfeo + OW["MnO", "OWeight"] * xmno   # (Fe,Mn)O
  
  NM <- with(s, cbind(
    Q = Q * mw("Quartz"), C = C * mw("Corundum"), Or = Or * mw("Orthoclase"),
    Ab = Ab * mw("Albite"), An = An * mw("Anorthite"), Ne = Ne * mw("Nepheline"),
    Lc = Lc * mw("Leucite"), Kp = Kp * mw("Kaliophilite"), Ac = Ac * mw("Acmite"),
    Ns = Ns * mw("Sodium metasilicate"), Ks = Ks * mw("Potassium metasilicate"),
    Di.Mg = Di * xmgr * mw("Diopside-Mg"),
    Di.Fe = Di * xfer * (feM + MW["Diopside-Fe", 4]),
    Wo = Wo * mw("Wollastonite"),
    En = Hy * xmgr * mw("Hypersthene-Mg"),
    Fs = Hy * xfer * (feM + MW["Hypersthene-Fe", 4]),
    Fo = Ol * xmgr * mw("Olivine-Mg"),
    Fa = Ol * xfer * (2 * feM + MW["Olivine-Fe", 4]),
    Cs = Cs * mw("Dicalcium silicate"),
    Mt = Mt * (feM + MW["Magnetite", 4]),
    Il = Il * (feM + MW["Ilmenite", 4]),
    Hm = Hm * mw("Hematite"), Tn = Tn * mw("Sphene"), Pf = Pf * mw("Perovskite"),
    Ru = Ru * mw("Rutile"), Ap = Ap * mw("Apatite-Ca"),
    Nc = cb$Nc * mw("Sodium Carbonate"), Cc = cb$Cc * mw("Calcite"),
    FREE_CO2 = cb$FREE_CO2 * OW["CO2", "OWeight"],
    DEFSIO2 = -DEFSIO2 * OW["SiO2", "OWeight"],
    FREE_P2O5 = FREE_P2O5 * OW["P2O5", "OWeight"]
  ))
  NM <- cbind(NM, Sums.min = rowSums(NM))
  
  P <- .cipw_parameters(W, M, NM, DensityRock(NM, MW))
  
  out <- .cipw_finish(data.frame(maj, NM, P, check.names = FALSE), digits)
  rownames(out) <- rownames(maj)
  out
}


#####################################################################
#            Calculate the theoretical density of rocks             #
#####################################################################

.dens_map <- c(
  Q = "Quartz", C = "Corundum", Or = "Orthoclase", Ab = "Albite",
  An = "Anorthite", Ne = "Nepheline", Lc = "Leucite", Kp = "Kaliophilite",
  Ac = "Acmite", Ns = "Sodium metasilicate", Ks = "Potassium metasilicate",
  Di.Mg = "Diopside-Mg", Di.Fe = "Diopside-Fe", Wo = "Wollastonite",
  En = "Hypersthene-Mg", Fs = "Hypersthene-Fe", Fo = "Olivine-Mg",
  Fa = "Olivine-Fe", Cs = "Dicalcium silicate", Dcs = "Dicalcium silicate",
  Mt = "Magnetite", Il = "Ilmenite", Hm = "Hematite", Tn = "Sphene",
  Pf = "Perovskite", Ru = "Rutile", Ap = "Apatite-Ca", `Ap-Ca` = "Apatite-Ca",
  `Ap-F` = "Apatite-F", Nc = "Sodium Carbonate", Cc = "Calcite",
  Z = "Zircon", Cm = "Chromite", NaCl = "Halite", Fr = "Fluorite",
  Pr = "Pyrite", Th = "Thenardite")

#' Theoretical rock density from the CIPW norm
#'
#' density: \eqn{\rho = \sum(w_i) / \sum(w_i / \rho_i)}, over all normative minerals
#' present in \code{Normative.minerals} with a known density.
#'
#' @param Normative.minerals matrix of normative minerals (wt\%)
#' @param MinWeight mineral table (column 5 = density)
#' @param map named character vector: column name -> row of \code{MinWeight}
#' @keywords internal
#' @noRd
DensityRock <- function(Normative.minerals, MinWeight, map = .dens_map) {
  map <- map[names(map) %in% colnames(Normative.minerals) & map %in% rownames(MinWeight)]
  rho <- MinWeight[map, 5]
  ok  <- is.finite(rho) & rho > 0
  map <- map[ok]; rho <- rho[ok]
  w <- Normative.minerals[, names(map), drop = FALSE]
  w[is.na(w)] <- 0
  rowSums(w) / rowSums(w / rep(rho, each = nrow(w)))
}

