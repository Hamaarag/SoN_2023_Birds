#' plot_alpha_diversity
#' 
#' plot 3 boxplots of alpha diversity for variables such as abundance, richness or gma (geometric mean abundance). Requires use of the function
#' multiplot.R from the url http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#'
#' @param P The data table (or data frame) containing the count / incidence data, where the first columns represent the groups (e.g., unit, site, year, etc.,) and the following columns are the species names
#' @param x_val The variable that should be plotted on the x axis, in parenthesis, matching one of the names of the first columns (e.g., "year")
#' @param y_val The variable that should be plotted on the y axis, in parenthesis, matching one of the names of the first columns (e.g., "gma")
#' @param fill_val The grouping variable that should be plotted as face color, in parenthesis, matching one of the names of the first columns (e.g., "dunes")
#' @param alpha_val The degree of color transparency
#' @param xlab_val The label of the x axis
#' @param ylab_val The label of the y axis
#'
#' @return
#' @export
#'
#' @examples
#' # plot the chosen biodiversity metric, using a data table containing all national data
plot_alpha_diversity <- function (P,x_val="settlements", y_val="abundance", fill_val=NA,
                                  alpha_val=0.5, xlab_val="proximity", ylab_val="abundance") {
  
  require(rlang)
  require(ggplot2)
  require(devtools)
  if (!exists(x = "multiplot")) {
    source_url("https://raw.githubusercontent.com/ronchen1/ron_functions/d2d0bab4dc65f29e53d316ae4098d0469711905b/R/multiplot.R")
  }
  
  if (is.na(fill_val)) {
    p1 <- ggplot(P) + geom_boxplot( aes(x = .data[[x_val]], y = .data[[y_val]], alpha=alpha_val)) +
      xlab(xlab_val) + ylab(ylab_val) + scale_alpha(guide = 'none')
  } else {
    p1 <- ggplot(P) + geom_boxplot( aes(x = as.factor(.data[[x_val]]), y = .data[[y_val]], fill = as.factor(.data[[fill_val]]), alpha=alpha_val)) +
      xlab(xlab_val) + ylab(ylab_val) + scale_alpha(guide = 'none')
  }
  
  
  # this works as well, without need for rlang, but alpha_val must be given as a string
  # print( ggplot(P) + geom_boxplot( aes_string(x = x_val, y = y_val, fill = fill_val, alpha=alpha_val)) )
  
  p2 <-  ggplot(P) + geom_boxplot( aes(x = as.factor(year), y = .data[[y_val]], alpha=alpha_val)) +
    xlab("year") + ylab(ylab_val) + scale_alpha(guide = 'none')
  
  p3 <- ggplot(P) + geom_boxplot( aes(x = as.factor(.data[[x_val]]), y = .data[[y_val]], fill = as.factor(year), alpha=alpha_val)) +
    xlab(xlab_val) + ylab(ylab_val) + scale_fill_brewer(palette = "Dark2") + scale_alpha(guide = 'none')
  
  multiplot (p1,p2,p3, layout=matrix(c(1,2,3,3), nrow=2, byrow = TRUE))
}