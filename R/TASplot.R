#####################################################################
#             Plot TAS diagram (Le Bas et al., 1986)                #
#####################################################################

# field boundaries (one path per id), vertices as in adj_rock()
# .tas_grid <- local({
#   p <- list(
#     list(x = c(41, 41, 45, 48.4, 52.5),     y = c(0, 7, 9.4, 11.5, 14)),
#     list(x = c(45, 45, 49.4, 53, 57.6, 61), y = c(0, 5, 7.3, 9.3, 11.7, 13.5)),
#     list(x = c(45, 52, 57, 63, 69),         y = c(5, 5, 5.9, 7, 8)),
#     list(x = c(45, 49.4, 52, 52),           y = c(9.4, 7.3, 5, 0)),
#     list(x = c(48.4, 53, 57, 57),           y = c(11.5, 9.3, 5.9, 0)),
#     list(x = c(52.5, 57.6, 63, 63),         y = c(14, 11.7, 7, 0)),
#     list(x = c(69, 69, 77.5),               y = c(13, 8, 0)),
#     list(x = c(41, 45),                     y = c(3, 3)),
#     list(x = c(61, 64),                     y = c(13.5, 15)),
#     list(x = c(52.5, 49),                   y = c(14, 15.5))
#   )
#   data.frame(id = rep(seq_along(p), lengths(lapply(p, `[[`, "x"))),
#              x = unlist(lapply(p, `[[`, "x")),
#              y = unlist(lapply(p, `[[`, "y")))
# })

# alkaline / subalkaline divide (Irvine & Baragar, 1971)
# .tas_irvine <- data.frame(
#   x = c(39.2, 40, 43.2, 45, 48, 50, 53.7, 55, 60, 65, 77),
#   y = c(0, 0.4, 2, 2.8, 4, 4.75, 6, 6.4, 8, 8.8, 10)
# )
# 
# .tas_labels <- data.frame(
#   x = c(74, 67, 60, 54.5, 48.5, 43, 64, 64, 57.5, 52.5, 48.86, 57.5, 53, 49, 45, 43, 42.5),
#   y = c(11, 4.5, 4.0, 3.5, 3.0, 2.0, 12.8, 10, 9, 7.5, 6, 15, 11.5, 9.3, 7.5, 5, 12.5),
#   label = c("R", "D", "A", "BA", "B", "PB", "T", "TD", "TA", "BTA", "TB",
#             "P", "TP", "PT", "Te", "Ba", "F")
# )

# #' TAS diagram
# #'
# #' Plots the total alkali - silica diagram (Le Bas et al., 1986) with the
# #' alkaline/subalkaline divide of Irvine & Baragar (1971). Analyses are
# #' recalculated with \code{\link{adj_rock}} before plotting.
# #'
# #' @param x data.frame of major element analyses (see \code{\link{adj_rock}})
# #' @param col,fill point colour
# #' @param  point size
# #' @param shape point shape
# #' @param gridcolor colour of the TAS field boundaries and labels
# #' @param alkcolor colour of the alkaline/subalkaline divide
# #' @param ... further arguments passed to \code{\link{adj_rock}}
# #'   (e.g. \code{Volcanic}, \code{AdjTAS})
# #'
# #' @returns NULL
# #' @export
# #'
# #' @examples
# #' data(TestTAS)
# #' TAS_plot(TestTAS, fill = "dodgerblue", shape = 21)
# #'
# #' data(Andes)
# #' TAS_plot(Andes, color = "darkgreen", shape = 21)
# TAS_plot <- function(x, color = "black", size = 2, shape = 16, fill = NA,
#                     gridcolor = "grey20", alkcolor = "darkred", ...) {
#   d <- adj_rock(x, ...)
#   TAS <- data.frame(S = d$SiO2.adj, NK = d$Na2O.adj + d$K2O.adj)
#   
#   ggplot(TAS, aes(x = .data$S, y = .data$NK)) +
#     geom_path(data = .tas_grid, aes(x = .data$x, y = .data$y, group = .data$id),
#               colour = gridcolor, inherit.aes = FALSE) +
#     geom_path(data = .tas_irvine, aes(x = .data$x, y = .data$y),
#               colour = alkcolor, linewidth = 1, inherit.aes = FALSE) +
#     annotate("text", x = .tas_labels$x, y = .tas_labels$y, label = .tas_labels$label,
#              size = 7, colour = gridcolor) +
#     annotate("text", x = c(75, 45), y = c(2, 10), label = c("Subalkaline", "Alkaline"),
#              colour = alkcolor, size = 6, fontface = "italic") +
#     geom_point(colour = color, size = size, fill = fill, shape = shape, na.rm = TRUE) +
#     labs(x = expression(SiO[2] ~ "(wt.adj %)"),
#          y = expression(Na[2] * O + K[2] * O ~ "(wt.adj %)"))
# }

#####################################################################
#             Plot TAS diagram (Le Bas et al., 1986)                #
#####################################################################

# field boundaries as one NA-separated polyline (vertices as in adj_rock())
.tas_grid <- local({
  p <- list(
    list(x = c(41, 41, 45, 48.4, 52.5),     y = c(0, 7, 9.4, 11.5, 14)),
    list(x = c(45, 45, 49.4, 53, 57.6, 61), y = c(0, 5, 7.3, 9.3, 11.7, 13.5)),
    list(x = c(45, 52, 57, 63, 69),         y = c(5, 5, 5.9, 7, 8)),
    list(x = c(45, 49.4, 52, 52),           y = c(9.4, 7.3, 5, 0)),
    list(x = c(48.4, 53, 57, 57),           y = c(11.5, 9.3, 5.9, 0)),
    list(x = c(52.5, 57.6, 63, 63),         y = c(14, 11.7, 7, 0)),
    list(x = c(69, 69, 77.5),               y = c(13, 8, 0)),
    list(x = c(41, 45),                     y = c(3, 3)),
    list(x = c(61, 64),                     y = c(13.5, 15)),
    list(x = c(52.5, 49),                   y = c(14, 15.5))
  )
  list(x = unlist(lapply(p, function(s) c(s$x, NA))),
       y = unlist(lapply(p, function(s) c(s$y, NA))))
})

# alkaline / subalkaline divide (Irvine & Baragar, 1971)
.tas_irvine <- list(
  x = c(39.2, 40, 43.2, 45, 48, 50, 53.7, 55, 60, 65, 77),
  y = c(0, 0.4, 2, 2.8, 4, 4.75, 6, 6.4, 8, 8.8, 10)
)

.tas_labels <- list(
  x = c(74, 67, 60, 54.5, 48.5, 43, 64, 64, 57.5, 52.5, 48.86, 57.5, 53, 49, 45, 43, 42.5),
  y = c(11, 4.5, 4.0, 3.5, 3.0, 2.0, 12.8, 10, 9, 7.5, 6, 15, 11.5, 9.3, 7.5, 5, 12.5),
  label = c("R", "D", "A", "BA", "B", "PB", "T", "TD", "TA", "BTA", "TB",
            "P", "TP", "PT", "Te", "Ba", "F")
)

#' TAS diagram
#'
#' Plots the total alkali - silica diagram (Le Bas et al., 1986) with the
#' alkaline/subalkaline divide of Irvine & Baragar (1971), using base
#' graphics. Analyses are recalculated with \code{\link{adj_rock}} before
#' plotting.
#'
#' @param x data.frame of major element analyses (see \code{\link{adj_rock}})
#' @param col point colour(s); recycled over samples
#' @param cex point size; recycled over samples
#' @param pch point symbol; recycled over samples
#' @param gridcolor colour of the TAS field boundaries and labels
#' @param alkcolor colour of the alkaline/subalkaline divide
#' @param labels logical. Draw the field abbreviations?
#' @param add logical. Add points to an existing TAS plot instead of
#'   drawing a new diagram?
#' @param xlim,ylim plot limits. Default covers the TAS grid and all samples.
#' @param ... further arguments passed to \code{\link{adj_rock}}
#'   (e.g. \code{Volcanic}, \code{AdjTAS})
#'
#' @returns invisibly, a data.frame with the plotted coordinates
#'   (\code{S} = SiO2, \code{NK} = Na2O + K2O, wt\%).
#' @export
#' @importFrom graphics plot.new plot.window axis box title lines text points par
#'
#' @examples
#' data(TestTAS)
#' TASplot(TestTAS, col = "red")
#'
#' data(Andes)
#' TASplot(Andes, col = "darkgreen")
#' TASplot(TestTAS, col = "red", add = TRUE)
TASplot <- function(x, col = par("col"), pch = par("pch"), cex = par("cex"),
                    gridcolor = "grey20", alkcolor = "darkred",
                    labels = TRUE, add = FALSE, xlim = NULL, ylim = NULL, ...) {
  d <- adj_rock(x, ...)
  TAS <- data.frame(S = d$SiO2.adj, NK = d$Na2O.adj + d$K2O.adj,
                    row.names = rownames(d))
  
  if (!add) {
    if (is.null(xlim)) xlim <- range(35, 80, TAS$S, na.rm = TRUE)
    if (is.null(ylim)) ylim <- range(0, 16, TAS$NK, na.rm = TRUE)
    
    plot.new()
    plot.window(xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i")
    axis(1); axis(2, las = 1); box()
    title(xlab = expression(SiO[2] ~ "(wt.adj %)"),
          ylab = expression(Na[2] * O + K[2] * O ~ "(wt.adj %)"))
    
    lines(.tas_grid, col = gridcolor)
    lines(.tas_irvine, col = alkcolor, lwd = 2)
    if (labels) {
      text(.tas_labels$x, .tas_labels$y, .tas_labels$label, col = gridcolor, cex = 1.2)
    }
    text(c(75, 45), c(2, 10), c("Subalkaline", "Alkaline"),
         col = alkcolor, font = 3, cex = 1.1)
  }
  
  points(TAS$S, TAS$NK, col = col, cex = cex, pch = pch)
  invisible(TAS)
}
