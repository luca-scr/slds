#' @name ggmatplot
#' @aliases ggmatplot
#' 
#' @title ggplot Columns of Matrices
#' 
#' @description ggplot2 version of matplot base graphic
#' 
#' @param x vector for the x-axis variable
#' @param y a matrix or a data.frame for the y-axis variables
#' @param type character string indicating the type of plot for each column of y. Possible values are \code{"p"} for points, \code{"l"} for lines, \code{"b"} for both points and lines
#' @param pch character string or vector of 1-characters or integers for plotting characters
#' @param lty vector of line types
#' @param col vector of colors
#' @param legend boolean specifying if a legend should be added or not
#' @param add boolean specifying if the graph should be added to an existing plot. If \code{TRUE} it is added to the last ggplot graph, but a specific ggplot object can also be provided
#' @param \dots additional arguments
#'
#' @seealso matplot
#' 
#' @examples
#' 
#' x <- 0:50/50
#' sines <- outer(x, 1:4, function(x, y) sin(y*pi*x))
#' ggmatplot(x, sines)
#' ggmatplot(x, sines, type = "l")
#' ggmatplot(x, sines, type = "b", lty = 1)
#' 
#' 
#' # parallel coordinate plot
#' x = t(apply(iris[,1:4], 2, function(x) (x-min(x))/(max(x)-min(x))))
#' ggmatplot(x, type = "l", lty = 1, col = 1) +
#'   scale_x_continuous(labels = rownames(x))
#' ggmatplot(x, type = "l", lty = 1, 
#'           col = as.numeric(iris$Species)+1) +
#'           scale_x_continuous(labels = rownames(x))
#' 
#' # add to an existing ggplot graph
#' mod1 = lm(Ozone ~ Temp, data = airquality)
#' mod2 = lm(Ozone ~ poly(Temp,2), data = airquality)
#' mod3 = lm(Ozone ~ poly(Temp,3), data = airquality)
#' df = data.frame(Temp = seq(56, 97, by = 0.1))
#' df = cbind(df, pred1 = predict(mod1, df),
#'                pred2 = predict(mod2, df),
#'                pred3 = predict(mod3, df))
#' ggplot(data = airquality, aes(x = Temp, y = Ozone)) +
#'   geom_point()
#' ggmatplot(df$Temp, df[,-1], type = "l", lty = 1, col = 2:4, 
#'           add = TRUE, legend = TRUE)
#'
#' @importFrom ggplot2 ggplot last_plot
#' @importFrom tidyr pivot_longer
#' 
#' @export

ggmatplot <- function(x, y, type = "p", pch, lty, col, 
                      legend = FALSE, add = FALSE, ...)
{
  xlabel <- if(!missing(x)) deparse1(substitute(x))
  ylabel <- if(!missing(y)) deparse1(substitute(y))
  if(missing(x)) 
  {
    if(missing(y)) 
      stop("must specify at least one of 'x' and 'y'")
     else x <- seq_len(NROW(y))
  } else if(missing(y)) 
  {
    y <- x
    ylabel <- xlabel
    x <- seq_len(NROW(y))
    xlabel <- ""
  }
  x <- as.vector(x)
  y <- as.data.frame(y)
  nc <- NCOL(y)
  xy <- data.frame(x, y, check.names = FALSE)
  xy <- tidyr::pivot_longer(xy, cols = -1, 
                            names_to = "by", 
                            values_to = "value")
  xy$by <- factor(xy$by, levels = colnames(y))
  
  if(missing(pch)) pch <- as.character(seq(nc))
  pch <- rep(pch, nc)[1:nc]
  if(missing(lty)) lty <- seq(nc)
  lty <- rep(lty, nc)[1:nc]
  if(missing(col)) col <- palette()
  if(is.numeric(col)) col <- palette()[col]
  col <- rep(col, nc)[1:nc]
  
  if(is.logical(add))
  {
    if(add) 
    {
      plot <- ggplot2::last_plot()
    } else 
    {  
      plot <- ggplot2::ggplot() + 
        xlab(xlabel) + 
        ylab(ylabel) 
    }
  } else 
  {
    stopifnot(inherits(add, "ggplot"))
    plot <- add
  }
  
  if(any(type == c("p", "b")))
  { 
    plot <- plot + 
      geom_point(data = xy, 
                 aes_string(x = "x", y = "value", group = "by",
                            shape = "by", color = "by"))
  }
  
  if(any(type == c("l", "b")))
  {
    plot <- plot + 
      geom_line(data = xy, 
                aes_string(x = "x", y = "value", group = "by",
                           lty = "by", color = "by"))
  }
  
  plot <- plot + 
    scale_color_manual(name = element_blank(), values = col) + 
    scale_shape_manual(name = element_blank(), values = pch) +
    scale_linetype_manual(name = element_blank(), values = lty)
    # theme(legend.title = element_blank())
  
  if(!legend)
     plot <- plot + theme(legend.position = "none")
  
  return(plot)
}
