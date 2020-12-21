#' ASDplot
#'
#' @param data A dataframe object
#' @param x Variables' names (string)
#' @param y Standaradized differences values (integer)
#' @param shape Methods used to balance groups differences (factor)
#' @param methods A vector of methods names (string)
#' @param psize Shape size
#' @param intercept_position A cutoff to indicate significant differences
#' @param intercept_color Color of the cutoff bar
#' @param intercept_size The size of the cutoff bar
#' @param switch Whether to include the cutoff bar or not (T or F)
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param ytitle Y axis title
#'
#' @return A ggplot of ASD
#' @export
#'
#' @import scales
#' @import ggplot2
#'
ASDplot <- function(data,
                    x,
                    y,
                    shape,
                    methods=shape,
                    psize=5,
                    intercept_position=0.1,
                    intercept_color='black',
                    intercept_size=0.8,
                    switch=T,
                    title='',
                    subtitle='',
                    ytitle='Absolute Standardized Difference'
) {
  ggplot(data=data, aes(x=x, y=y, shape=factor(shape))) +
    geom_point(size=psize, aes(color=factor(shape)))+
    {if(switch)geom_hline(yintercept = intercept_position, color = intercept_color, size = intercept_size)}+
    coord_flip() +
    theme_bw() + theme(legend.key = element_blank()) +
    scale_y_continuous(labels = scales::percent, breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)) +
    labs(title=title,
         subtitle = subtitle,
         x='',
         y=ytitle,
         size="horsepower") +
    scale_color_discrete(' ',labels=methods) +
    scale_shape_discrete(' ',labels=methods) +
    theme(
      plot.title = element_text(face = 'bold',size = 22, hjust=0.5, margin = margin(t =5, r = 0, b = 20, l = 0)),
      legend.text = element_text(size=14),
      legend.key.height=unit(2,"line"),
      legend.key.width=unit(2,"line"),
      legend.title = element_text(size=18, face='bold'),
      axis.text.y = element_text(size=18, face = 'bold'),
      axis.text.x = element_text(size=18, face = 'bold'),
      axis.title.y = element_text(size =18, angle = 90, margin = margin(t = 0, r = 0, b = 0, l = 20)),
      axis.title.x = element_text(size =18, angle = 0,margin = margin(t = 20, r = 0, b = 5, l = 0)))
}
