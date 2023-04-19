

ggbars_simple <- function(){





}

ggbars_dodge <- function(stat = "identity",
                         position = "dodge",
                         width = 0.75,
                         ...){
  ggplot2::geom_bar(stat = stat, position = position, width = width,...)
}

ggbars_stack <- function(stat = "identity",
                         position = "stack",
                         ...){
  ggplot2::geom_bar(stat = stat, position = position,...)
}
ggbars_fill <- function(stat = "identity",
                        position = "fill",
                        ...){
  ggplot2::geom_bar(stat = stat, position = position,...)
}


ggbars_pie <- function(theta = "y",...){
  p <- ggplot2::geom_bar(stat="identity", width = 1, position="fill", ...)+
    ggplot2::coord_polar(theta = theta)
  p
}
