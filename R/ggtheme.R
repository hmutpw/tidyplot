#' ggplot theme style
#'
#' @param text_size The text size
#' @param line_size The line size
#' @param axis.text.x.angle The angle of text in x axis.
#' @param text_color Text color for axis. Default: black.
#' @param line_color Text color for axis. Default: black.
#' @param axis.text.x.hjust Horizontal adjustment for x axis.
#' @param axis.text.x.vjust Vertical adjustment for x axis.
#' @param ... others parameters.
#'
#' @return A ggplot theme.
#'
#' @importFrom  ggplot2 theme element_line element_rect element_text
#'
#' @export
#'
#' @examples
#' simpletheme()
simpletheme <- function(text_size = 6,
                        text_color = "#000000",
                        line_color = "#000000",
                        line_size = 0.4672897,
                        axis.text.x.angle = 0,
                        axis.text.x.hjust = 0.5,
                        axis.text.x.vjust = 0.5,
                        ...){
  theme(line = element_line(size=line_size),
        text = element_text(size=text_size, color=text_color),
        panel.border = element_rect(size = line_size, fill=NA),
        panel.background = element_rect(fill=NA, color=NA),
        strip.background = element_rect(fill=NA, color=NA),
        strip.text = element_text(size=text_size,color=text_color),
        panel.grid.major = element_line(size=line_size/2, linetype = 8, color = "#9e9e9e"),
        panel.grid.minor = element_line(size=NA, color = NA),
        axis.ticks = element_line(size=line_size, color=line_color),
        axis.title.x = element_text(size=text_size),
        axis.text.x = element_text(size=text_size, angle=axis.text.x.angle, hjust=axis.text.x.hjust, vjust=axis.text.x.vjust, color=text_color),
        axis.text.y = element_text(size=text_size, color=text_color),
        legend.text = element_text(size = text_size),
        legend.title = element_text(size=text_size*1.5),
        plot.title = element_text(size=text_size, hjust=0.5,vjust=0.5), legend.position='right')
}

text_size.default <- 6
line_size.default <- 0.5/1.07

