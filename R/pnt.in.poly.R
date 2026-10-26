#' Point in Polygon
#' 
#' \code{pnt_in_poly} works out if 2D points lie within the boundaries of a
#' defined polygon. \cr \cr \bold{Note:} Points that lie on the boundaries of
#' the polygon or vertices are assumed to be within the polygon.
#' 
#' The algorithm implements a sum of the angles made between the test point and
#' each pair of points making up the polygon. The point is interior if the sum
#' is 2pi, otherwise, the point is exterior if the sum is 0. This works for
#' simple and complex polygons (with holes) given that the hole is defined with
#' a path made up of edges into and out of the hole. \cr \cr This sum of angles
#' is not able to consistently assign points that fall on vertices or on the
#' boundary of the polygon. The algorithm defined here assumes that points
#' falling on a boundary or polygon vertex are part of the polygon.
#' 
#' @param pnts a 2-column matrix or dataframe defining locations of the points
#' of interest
#' @param poly.pnts a 2-column matrix or dataframe defining the locations of
#' vertices of the polygon of interest
#' @return A 3-column dataframe where the first 2 columns are the original
#' locations of the points. The third column (names pip) is a vector of binary
#' values where 0 represents points not with the polygon and 1 within the
#' polygon.
#' @examples
#' #define the points and polygon
#' pnts = expand.grid(x=seq(1,6,0.1),y=seq(1,6,0.1))
#' polypnts = cbind(x=c(2,3,3.5,3.5,3,4,5,4,5,5,4,3,3,3,2,2,1,1,1,1,2),
#'     y=c(1,2,2.5,2,2,1,2,3,4,5,4,5,4,3,3,4,5,4,3,2,2))
#' 
#' #plot the polygon and all points to be checked
#' plot(rbind(polypnts, pnts))
#' polygon(polypnts,col='#99999990')
#' 
#' #create check which points fall within the polygon
#' out = pnt_in_poly(pnts,polypnts)
#' head(out)
#' 
#' #identify points not in the polygon with an X
#' points(out[which(out$pip==0),1:2],pch='X')
#' 
#' 
#' @export
pnt_in_poly <- function(pnts, poly.pnts, tol = sqrt(.Machine$double.eps)) {
  pnts <- as.matrix(pnts)
  poly.pnts <- as.matrix(poly.pnts)
  if (!(is.matrix(pnts) && is.matrix(poly.pnts)) ||
      ncol(pnts) != 2L || ncol(poly.pnts) != 2L)
    stop("pnts & poly.pnts must be a 2 column dataframe or matrix")
  
  # drop closing vertex if polygon is explicitly closed
  n <- nrow(poly.pnts)
  if (all(poly.pnts[1L, ] == poly.pnts[n, ])) {
    poly.pnts <- poly.pnts[-n, , drop = FALSE]
    n <- n - 1L
  }
  if (n < 3L) stop("polygon needs at least 3 distinct vertices")
  
  px <- pnts[, 1L]; py <- pnts[, 2L]
  vx <- poly.pnts[, 1L]; vy <- poly.pnts[, 2L]
  eps <- tol * max(diff(range(vx)), diff(range(vy)), 1)
  
  angle  <- numeric(length(px))
  on_bnd <- logical(length(px))
  
  for (i in seq_len(n)) {
    j <- if (i == n) 1L else i + 1L
    x1 <- vx[i] - px; y1 <- vy[i] - py      # point -> vertex i
    x2 <- vx[j] - px; y2 <- vy[j] - py      # point -> vertex j
    cr <- x1 * y2 - y1 * x2                 # cross product
    dt <- x1 * x2 + y1 * y2                 # dot product
    # on boundary: collinear with edge AND between its end points (incl. vertices)
    elen <- sqrt((vx[j] - vx[i])^2 + (vy[j] - vy[i])^2)
    on_bnd <- on_bnd | (abs(cr) <= eps * max(elen, eps) & dt <= eps^2)
    angle  <- angle + atan2(cr, dt)         # signed angle subtended by edge
  }
  
  # |sum| ~ 2*pi inside, ~ 0 outside; boundary counts as inside
  data.frame(pnts, pip = as.integer(on_bnd | abs(angle) > pi))
}