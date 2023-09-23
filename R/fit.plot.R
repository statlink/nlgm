fit.plot <- function(mat) {

  colnames(mat) <- c("cases", "ti", "fit")
  mat <- as.data.frame(mat)

  requireNamespace("ggplot2")
  ggplot( mat, aes(x = ti, y = cases) ) +
  geom_point( size = 5, alpha = .1) + theme_bw() +
  geom_line( aes(x = ti, y = fit), size = 1.2 ) +
  theme( legend.position = ("bottom") ) +
  theme( axis.text.y = element_text(size = 12),
         axis.title.y = element_text(size = 14, face = "bold") ) +
  theme( axis.text.x = element_text(size = 16, face = "bold" ),
         axis.title.x = element_text(size = 16, face = "bold") ) +
  xlab( "Time (days)" ) +
  ylab( "Cases" )
}
