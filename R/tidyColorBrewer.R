material.color.set.names <- c("Developmental.basic",
                              "Developmental.BuRd",
                              "Developmental.full",
                              "heatmap.BuRd",
                              "heatmap.BuYlRd",
                              "heatmap.GnPR",
                              "heatmap.BuGnYl",
                              "heatmap.BuPu",
                              "heatmap.GnBuPu",
                              "Rainbow",
                              "Discrete")
##########
# 1. material palette colors
##########

#' Get material colors
#'
#' @param name The names of material colors.
#' @param n Number of colors you want to get. Default: 10, all color elements.
#'
#' @return A vector of colors
#' @export
#'
#' @examples
#' material.colors(name = "Red",n = 6)
material.colors <- function(name,
                            n = 10){
  material.color.names <- colnames(tidyplot::materialColorBrewer)
  if(!(name %in% material.color.names)){
    stop("The ",name," is not a valid color name!")
  }
  if(!(n>0 & n<15)){
    stop("The ",n," should be an integer between 1 and 10!")
  }
  colPal <- tidyplot::materialColorBrewer[1:n,name]
  unname(colPal[!is.na(colPal)])
}


##########
# 2. brewer colors
##########

#' Get colors from RColorBrewer package
#'
#' @param name The names of color set. It must be one of the colors in \pkg{RColorBrewer} package. We suggest using the
#' following colors for heatmap: RdYlBu, RdBu, YlGnBu, PRGn.
#' @param n Number of colors you want. We suggest you set it by default. Default: NULL, means using the raw color elements.
#'
#' @return A character vector of colors.
#' @export
#' @import RColorBrewer
#'
#' @examples
#' brewer.colors(name="RdBu")
brewer.colors <- function(name, n = NULL){
  brewer.color.names <- row.names(RColorBrewer::brewer.pal.info)
  if(!(name %in% brewer.color.names)){
    stop("The ",name," is not a valid color in RColorBrewer!")
  }
  if(is.null(n)){
   n <- RColorBrewer::brewer.pal.info[name,"maxcolors"]
  }
  colPal <- RColorBrewer::brewer.pal(n = n, name = name)
  unname(colPal)
}


##########
# 3. gradient colors
##########

#' Get gradient colors
#'
#' @param name The names of gradient color set. Developmental means gradient colors for sequential development. This is
#' designed for developmental process, for example, stem cell differentiation. We'll keep updating the beautiful colors.
#' Default: Developmental.basic.
#' @param brightness The brightness of colors, from 1 to 10. Default: 3
#' @param n Number of colors you want. We suggest you set it by default. Default: NULL, means using the raw color elements.
#'
#' @return A character vector of colors.
#' @export
#'
#' @examples
#' color_sequential <- material.color.set("Developmental.basic")
#' display.color(color_sequential, "Developmental.basic")
#'
material.color.set <- function(name = c("Developmental.basic",
                                        "Developmental.BuRd",
                                        "Developmental.full",
                                        "heatmap.BuRd",
                                        "heatmap.BuYlRd",
                                        "heatmap.GnPR",
                                        "heatmap.BuGnYl",
                                        "heatmap.BuPu",
                                        "heatmap.GnBuPu",
                                        "Rainbow",
                                        "Discrete"),
                               n = NULL,
                               brightness = 3L){
  name <- match.arg(name)
  if(! brightness %in% 1:10){
    stop('Parameter brightness must between 1 to 10')
  }
  colPal <- switch(name,
                   Developmental.BuRd = materialColorBrewer[brightness, c('Blue', 'Green', 'Yellow', 'Orange','Red')],
                   Developmental.basic = materialColorBrewer[brightness, c('Blue', 'Cyan', 'Green', 'Yellow', 'Orange', 'Red', 'Purple', 'Indigo')],
                   Developmental.full = materialColorBrewer[brightness, c('Blue', 'Light_blue', 'Cyan', 'Teal', 'Green', 'Light_green',
                                                                          'Lime', 'Yellow', 'Amber', 'Orange', 'Deep_orange', 'Red',
                                                                          'Pink', 'Purple', 'Deep_purple', 'Indigo')],
                   heatmap.BuRd = rev(brewer.colors(name = "RdBu", n = RColorBrewer::brewer.pal.info["RdBu","maxcolors"])),
                   heatmap.BuYlRd = rev(brewer.colors(name = "RdYlBu", n = RColorBrewer::brewer.pal.info["RdYlBu","maxcolors"])),
                   heatmap.GnPR = rev(brewer.colors(name = "PRGn", n = RColorBrewer::brewer.pal.info["PRGn","maxcolors"])),
                   heatmap.BuGnYl = rev(brewer.colors(name = "YlGnBu", n = RColorBrewer::brewer.pal.info["YlGnBu","maxcolors"])),
                   heatmap.BuPu = rev(brewer.colors(name = "BuPu", n = RColorBrewer::brewer.pal.info["BuPu","maxcolors"])),
                   heatmap.GnBuPu = rev(brewer.colors(name = "PuBuGn", n = RColorBrewer::brewer.pal.info["PuBuGn","maxcolors"])),
                   Rainbow = materialColorBrewer[brightness, c('Red', 'Orange', 'Yellow', 'Green', 'Blue', 'Indigo', 'Purple')],
                   Discrete = materialColorBrewer[brightness, c('Red','Blue','Green','Purple','Orange','Indigo','Yellow','Cyan','Brown',
                                                                'Pink','Light_blue','Light_green','Deep_purple','Amber','Teal',
                                                                'Deep_orange','Blue_grey','Lime','Grey')],
  )
  if(!is.null(n)){
    if(!is.integer(n)){
      stop(n, " must be integer")
    }
    if(n<1){
      stop(n, " must be positive integer!")
    }
    if(n > length(colPal)){
      n <- length(colPal)
      warning(n, " exceded maximum length, only use all the color elements!")
    }
    colPal <- colPal[1:n]
  }
  unname(colPal)
}

##########
# 4. display colors on figure
##########

#' Display colors
#'
#' @param colors A vector of colors. Must be character.
#' @param color_name The name of colors.
#'
#' @return An image.
#' @export
#' @importFrom graphics par image text
#'
#' @examples
#' display.color(colors = c("#90CAF9", "#80DEEA", "#A5D6A7", "#FFF59D", "#FFCC80", "#EF9A9A", "#CE93D8", "#9FA8DA"))
display.color <- function(colors, color_name = ""){
  n <- length(colors)
  graphics::par(mai=c(0.5,1.5,0.5,0.5))
  graphics::image(x=1:n, y=1, z=as.matrix(1:n), col=colors, xlab="", ylab = "", xaxt="n", yaxt="n", bty="n")
  graphics::text(x = 0,y = 1, labels = color_name, xpd=TRUE, adj=1)
}

##########
# 5. show all colors
##########

#' Display all the colors
#'
#' @param type The color set to display
#' @param brightness The brightness selected to display. Default: 3L, should between 1 and 10.
#'
#' @return A figure
#' @export
#' @importFrom graphics par image text
#'
#' @examples
#' display.color.all(type = "material.set")
display.color.all <- function(type = c("material.set","material"), brightness = 3L){
  type <- match.arg(arg = type)
  if(type=="material.set"){
    color.sets <- lapply(X = material.color.set.names, FUN = material.color.set, brightness = brightness)
    names(color.sets) <-material.color.set.names
    j=1
    graphics::par(mai=c(1,1.5,1,0.5))
    plot(1,1,xlim=c(0,19),ylim=c(0,length(color.sets)),type="n", axes=FALSE, bty="n", xlab="",ylab="")
    for(i in names(color.sets)){
      graphics::rect(xleft=0:(length(color.sets[[i]])-1), ybottom=j-1, xright=1:length(color.sets[[i]]), ytop=j-0.5, col=color.sets[[i]], border="light grey")
      graphics::text(x = -0.3,y = j-0.75, labels = i, xpd=TRUE, adj=1)
      j=j+1
    }
  }else{
    material.color <- tidyplot::materialColorBrewer[1:10,]
    graphics::par(mai=c(0.5,1.5,0.5,0.5))
    graphics::image(x = 1:(nrow(material.color)), y = 1:(ncol(material.color)),
                    z = matrix(1:length(material.color),nrow = nrow(material.color), ncol = ncol(material.color)),
                    col = material.color, xlab="",ylab="",xaxt="n",yaxt="n",bty="n")
    graphics::text(x = rep(0.2, ncol(material.color)),y = (1:ncol(material.color)), labels=colnames(material.color), xpd=TRUE, adj=1)
  }
}

##########
# 6. other tools
##########
#' Browse material color information
#'
#' @return a data.frame with color name and element number.
#' @export
#'
#' @examples
#' browse.material.colors()
browse.material.colors <- function(){
  colors <- as.data.frame(apply(tidyplot::materialColorBrewer,2,function(x){length(x[!is.na(x)])}))
  colnames(colors) <- "color.number"
  colors
}

#' Expand colors into the number you wanted
#'
#' @param color The color vector
#' @param n The number of colors you wanted
#'
#' @return A list of colors you wanted.
#' @export
#'
#' @examples
#' adjust.color.number(color = c("red","green", "green"), n= 10)

adjust.color.number <- function(color, n){
  grDevices::colorRampPalette(colors = color)(n)
}

