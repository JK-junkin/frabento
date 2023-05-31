#' @title Zoom in or out for specific facet panels
#' @description 任意のfacetパネルをズームする.
#' @param panel_ranges Limits of axes or an axis.
#' @param expand Add margin to each axes or an axis, Default: TRUE
#' @param default Is this the default coordinate system? If `FALSE` (the default),
#'   then replacing this coordinate system with another one creates a message alerting
#'   the user that the coordinate system is being replaced. If `TRUE`, that warning
#'   is suppressed. NOTE: This argument explanation was cited from ggplot2::coord_cartesian
#' @param clip Should drawing be clipped to the extent of the plot panel? A
#'   setting of `"on"` (the default) means yes, and a setting of `"off"`
#'   means no. In most cases, the default of `"on"` should not be changed,
#'   as setting `clip = "off"` can cause unexpected results. It allows
#'   drawing of data points anywhere on the plot, including in the plot margins. If
#'   limits are set via `xlim` and `ylim` and some data points fall outside those
#'   limits, then those data points may show up in places such as the axes, the
#'   legend, the plot title, or the plot margins. NOTE: This argument explanation
#'   was cited from ggplot2::coord_cartesian
#' @return ggproto object
#' @details DETAILS
#' @references
#'  \url{https://andburch.github.io/ggplot_facets}
#'  \url{https://stackoverflow.com/questions/63550588/ggplot2coord-cartesian-on-facets}
#' @examples
#' \dontrun{
#' if(interactive()){
#'  library(ggplot2)
#'  library(dplyr)
#'  library(frabento)
#'  
#'  theme_set(theme_linedraw(base_family = "Helvetica", base_line_size = 0.3) +
#'            theme(aspect.ratio = 1/1, legend.position = c(0.75, 0.25)))
#'  
#'  ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
#'      geom_point(aes(color = Species)) +
#'      geom_rect(data = .  %>% dplyr::filter(Species == "versicolor"),
#'                xmin = 5.5, xmax = 6.5, ymin = 2.5, ymax = 3,
#'                color = "red", size = 1, fill = NA) +
#'      facet_wrap(~ Species, scale = "free", ncol = 2) +
#'      coord_panel_ranges(panel_ranges = list(
#'          list(NULL),
#'          list(x = c(5.4, 6.6), y = c(2.4, 3.1)),
#'          list(NULL)
#'      ))
#'  }
#' }
#' @seealso
#'  \code{\link[ggplot2]{ggproto}},\code{\link[ggplot2]{coord_cartesian}}
#' @rdname coord_panel_ranges
#' @export
#' @import ggplot2

coord_panel_ranges <-
    function(panel_ranges, expand = TRUE, default = FALSE, clip = "on") {
    UniquePanelCoords <- ggplot2::ggproto(
      "UniquePanelCoords", ggplot2::CoordCartesian,

      num_of_panels = 1,
      panel_counter = 1,
      panel_ranges = NULL,

      setup_layout = function(self, layout, params) {
        self$num_of_panels <- length(unique(layout$PANEL))
        self$panel_counter <- 1
        layout
      },

      setup_panel_params =  function(self, scale_x, scale_y, params = list()) {
        if (!is.null(self$panel_ranges) & length(self$panel_ranges) != self$num_of_panels)
          stop("Number of panel ranges does not equal the number supplied")

        train_cartesian <- function(scale, limits, name, given_range = NULL) {
          if (is.null(given_range)) {
#         range <- ggplot2:::scale_range(scale, limits, self$expand)
              expansion <- ggplot2:::default_expansion(scale, expand = self$expand)
              range <- ggplot2:::expand_limits_scale(scale, expansion,
                                                     coord_limits = self$limits[[name]])
          } else {
              range <- given_range
          }

#       out <- scale$break_info(range)
#       out$arrange <- scale$axis_order()
#       names(out) <- paste(name, names(out), sep = ".")
#       out
          out <- list(
            ggplot2:::view_scale_primary(scale, limits, range),
            sec = ggplot2:::view_scale_secondary(scale, limits, range),
            arrange = scale$axis_order(),
            range = range
        )
        names(out) <- c(name, paste0(name, ".", names(out)[-1]))
        out
        }

        cur_panel_ranges <- self$panel_ranges[[self$panel_counter]]
        if (self$panel_counter < self$num_of_panels)
          self$panel_counter <- self$panel_counter + 1
        else
          self$panel_counter <- 1

        c(train_cartesian(scale_x, self$limits$x, "x", cur_panel_ranges$x),
          train_cartesian(scale_y, self$limits$y, "y", cur_panel_ranges$y))
      }
    )
    ggplot2::ggproto(NULL, UniquePanelCoords, panel_ranges = panel_ranges,
                     expand = expand, default = default, clip = clip)
}
