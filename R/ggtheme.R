#' The basic themes of tidyplot
#'
#' @param text.size The size of text in the figure. Default: 6 pt. The Cell, Nature and Science journals
#' required the size of text should between 5 to 7 pt.
#' @param line.size The size of lines in the figure. Default: 1 pt. The Cell, Nature and Science journals
#' required the minimum size of lines should be greater than 0.3 pt.
#' @param border.color The color of borders of figures. Default: black.
#' @param axis.line.x.color The color of x axis. Default, black, not used when border color is black.
#' @param axis.line.y.color The color of y axis. Default, black, not used when border color is black.
#' @param panel.grid.major.x.color The color of major grid lines at x panel, Default: gray.
#' @param panel.grid.major.y.color The color of major grid lines at y panel, Default: gray.
#' @param axis.text.x.angle The angle of text at x axis.
#' @param axis.text.x.hjust The adjustment of text at x axis. Used when angle is not 0.
#' @param axis.text.x.vjust The adjustment of text at x axis. Used when angle is not 0.
#'
#' @return A theme object to ggplot2.
#' @importFrom ggplot2 theme element_line element_text element_rect
#' @export
#'
#' @examples
#' library(ggplot2)
#' p0 <- ggplot2::ggplot(data = mpg, aes(x = trans, fill = factor(cyl))) + geom_bar(colour = 1, binwidth = 0.2)
#' p0 + theme_basic()
theme_basic <- function(text.size = 6,
                        line.size = 0.4672897,
                        border.color = '#000000',
                        axis.line.x.color = '#000000',
                        axis.line.y.color = '#000000',
                        panel.grid.major.x.color = '#e0e0e0',
                        panel.grid.major.y.color = '#e0e0e0',
                        axis.text.x.angle = 0,
                        axis.text.x.hjust = 0.5,
                        axis.text.x.vjust = 0){
  ggplot2::theme(
    line = element_line(colour = '#000000',
                        linewidth = line.size,
                        lineend = 'square'),
    text = element_text(family = 'sans',
                        colour = '#000000',
                        size = text.size,
                        face = 'plain'),
    #---axis theme
    axis.title = element_text(family = 'sans',
                              colour = '#000000',
                              size = text.size),
    axis.text.x = element_text(family = 'sans',
                               face = 'plain',
                               colour = '#000000',
                               size = text.size,
                               angle = axis.text.x.angle,
                               hjust = axis.text.x.hjust,
                               vjust = axis.text.x.vjust),
    axis.text.y = element_text(family = 'sans',
                               face = 'plain',
                               colour = '#000000',
                               size = text.size),
    axis.ticks = element_line(colour = '#000000',
                              linewidth = line.size,
                              lineend = 'square'),
    axis.line.x = element_line(colour = axis.line.x.color,
                               linewidth = line.size),
    axis.line.y = element_line(colour = axis.line.y.color,
                               linewidth = line.size),
    #---legend theme
    legend.background = element_rect(fill = NA,
                                     colour = '#000000',
                                     linewidth = line.size,
                                     linetype = 'dashed'),
    legend.key = element_rect(fill = NA,
                              colour = NA),
    legend.text = element_text(family = 'sans',
                               colour = '#000000',
                               size = text.size),
    legend.text.align = 0,
    legend.title = element_text(family = 'sans',
                                colour = '#000000',
                                size = text.size,
                                face = 'plain'),
    legend.title.align = 0,
    legend.box.background = element_rect(fill = NA,
                                         color = NA),
    #---panel theme
    panel.background = element_rect(fill = NA,
                                    colour = NA),
    panel.border = element_rect(fill = NA,
                                colour = border.color,
                                linewidth = line.size),
    panel.grid.major.x = element_line(colour = NA,
                                      linewidth = line.size/2,
                                      linetype = 'dashed',
                                      color = panel.grid.major.x.color),
    panel.grid.major.y = element_line(colour = NA,
                                      linewidth = line.size/2,
                                      linetype = 'dashed',
                                      color = panel.grid.major.y.color),
    panel.grid.minor = element_line(size=NA,
                                    colour = NA),
    plot.background = element_rect(fill = NA,
                                   colour = NA),
    plot.title = element_text(family = 'sans',
                              face = 'bold',
                              colour = '#000000',
                              size = text.size,
                              hjust = 0.5),
    #---strip
    strip.background = element_rect(fill = NA,
                                    colour = NA),
    strip.clip = 'off',
    strip.text = element_text(family = 'sans',
                              colour = '#000000',
                              size = text.size,
                              hjust = 0.5)
  )
}


######
#slope
######

#' The slope themes of tidyplot
#'
#' @param axis.text.x.angle The angle of text at x axis. Default:45.
#' @param axis.text.x.hjust The adjustment of text at x axis. Default: 1.
#' @param axis.text.x.vjust The adjustment of text at x axis. Default:1.
#' @param ... Other parameters passed to 'tidytheme_basic()'.
#'
#' @return A theme object to ggplot2.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p0 <- ggplot(data = mpg, aes(x = trans, fill = factor(cyl))) + geom_bar(colour = 1, binwidth = 0.2)
#' p0 + theme_slope()
theme_slope <- function(axis.text.x.angle = 45,
                        axis.text.x.hjust = 1,
                        axis.text.x.vjust = 1,
                        ...){
  theme_basic(axis.text.x.angle = axis.text.x.angle,
              axis.text.x.hjust = axis.text.x.hjust,
              axis.text.x.vjust = axis.text.x.vjust,
              ...)
}
######
#clean
######

#' The clean themes of tidyplot
#'
#' @param border.color The color of borders of figures. Default: NA.
#' @param axis.line.x.color The color of x axis. Default: black.
#' @param axis.line.y.color The color of y axis. Default: black.
#' @param panel.grid.major.x.color The color of major grid lines at x panel, Default: NA.
#' @param panel.grid.major.y.color The color of major grid lines at y panel, Default: NA.
#' @param ... Other parameters passed to 'tidytheme_basic()'.
#'
#' @return A theme object to ggplot2.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p0 <- ggplot(data = mpg, aes(x = trans, fill = factor(cyl))) + geom_bar(colour = 1, binwidth = 0.2)
#' p0 + theme_clean()
theme_clean <- function(border.color = NA,
                        axis.line.x.color ='#000000',
                        axis.line.y.color = '#000000',
                        panel.grid.major.x.color = NA,
                        panel.grid.major.y.color = NA,
                        ...){
  theme_basic(border.color = border.color,
              panel.grid.major.x.color = panel.grid.major.x.color,
              panel.grid.major.y.color = panel.grid.major.y.color,
              axis.line.x.color = axis.line.x.color,
              axis.line.y.color = axis.line.y.color,
              ...)
}


