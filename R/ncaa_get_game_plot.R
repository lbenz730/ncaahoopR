#' Plot all shots from one game
#'
#' Plots game shot locations from one game. DF can be obtained from
#' get_shot_locs
#'
#' @param shot_loc_df dataframe of shot locations
#' @param heatmap Use a density type heatmap
#' @import ggplot2
#' @import dplyr
#' @import cowplot
#' @export
#'
#'

get_game_plot <- function(shot_loc_df,heatmap=F){
  teams = unique(shot_loc_df$team_name)
  game_title = paste0(teams[1]," vs. ",teams[2])
  color = as.character(unique(shot_loc_df$color))
  date = unique(shot_loc_df$date)

  if(heatmap){
    full_title = paste0(game_title," \n ",date)
    title <- ggdraw() +
      draw_label(full_title,
                 fontface = 'bold')
    out <- split(shot_loc_df , f = shot_loc_df$team_name)
    p = get_team_plot(out[[1]],unique(out[[1]]$team_name),heatmap=T) +
      theme(legend.position="bottom")
    p2 = get_team_plot(out[[2]],unique(out[[2]]$team_name),heatmap=T) +
      labs(caption="") +
      theme(legend.position = "bottom")
    p3 = cowplot::plot_grid(p,p2)
    p1 = plot_grid(title, p3, ncol = 1, rel_heights = c(0.1, 1))
    return(p1)
  }

  p1 = ggplot() +
    geom_point(data = shot_loc_df,
               aes(
                 x = x,
                 y = y,
                 shape = outcome,
                 color = team_name),
               size = 3) +
    geom_polygon(data = court, aes(x = x, y = y, group = group), col = "gray") +
    geom_point(alpha = 0.2, size = 1.5) + coord_equal() +
    scale_color_manual(values = color) +
    xlab("") + ylab("")  +
    coord_flip() +
    theme_void() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_blank(),
      legend.position = "bottom",
      legend.direction = 'vertical',
      plot.title = element_text(size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(size = 8, hjust = 0),
      plot.background = element_rect(fill = 'cornsilk')) +
    labs(
      title = game_title,
      subtitle = date,
      color = 'Team',
      shape = 'Made',
      caption = "Meyappan Subbaiah (@msubbaiah1) Data Accessed via ncaahoopR")
  return(p1)
}
