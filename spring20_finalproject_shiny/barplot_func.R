# Barplot function
bar_plot <- function(df, x) {
  plot_graph <- ggplot({{df}}, aes({{x}})) +
    geom_bar(aes(fill = x), show.legend = FALSE) +
    coord_flip()
  
  if(!is.numeric({{x}})) {
    stop()
  }
  else{
    plot_graph
  }
  return(plot_graph)
}