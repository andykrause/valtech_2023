 
### Theme for plotting indexes

theme_vtc <- function(
    base_size = 11,
    base_family = "",
    base_line_size = base_size/22,
    base_rect_size = base_size/22) {
  theme_bw(base_size = base_size, base_family = base_family,
           base_line_size = base_line_size, base_rect_size = base_rect_size) +
    theme(axis.ticks = element_blank(),
          axis.title = element_text(color = 'gray80'),
          axis.text = element_text(color = 'gray80'),
          legend.key = element_blank(),
          panel.border = element_blank(), strip.background = element_blank(),
          legend.position = 'bottom',
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          plot.caption = element_text(color = "gray10", face = "italic", hjust = 0, size = 8))
}

makeDeciles <- function(x){
  cut(x, quantile(x, probs = seq(0, 1, by = .1))) %>%
    as.factor() %>%
    as.numeric()
}
