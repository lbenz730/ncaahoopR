#' Plot all shots for a team
#'
#' Plots team shot locations from one game or multiple games. DF can be obtained from
#' get_shot_locs
#'
#' @param df dataframe of shot locations
#' @param team Team Name
#' @param heatmap Use a density type heatmap
#' @import ggplot2
#' @import dplyr
#' @export
#'
#'

get_team_plot <- function(df,team,heatmap=F){
  side_one = court %>% filter(side==1)
  team_shots = df %>% filter(team_name==team)
  ### flip shots if they are on the wrong side
  team_shots[team_shots$y > 47,"y"] = 47 - (team_shots[team_shots$y > 47,"y"] - 47)
  # only pick one color
  color = as.character(unique(team_shots$color))[1]

  if(heatmap){
    p1 = ggplot() +
      stat_density_2d(data = team_shots,
                      aes(x = x, y = y, fill = stat(density / max(density))),
                      geom = "raster", contour = FALSE, interpolate = TRUE, n = 200) +
      geom_polygon(data = side_one, aes(x = x, y = y, group = group), col = "gray") +
      geom_point(alpha = 0.2, size = 1.5) + coord_equal() +
      scale_color_manual(values = color) +
      xlab("") + ylab("")  +
      scale_fill_viridis_c("Shot Frequency    ",
                           limits = c(0, 1),
                           breaks = c(0, 1),
                           labels = c("lower", "higher"),
                           option = "plasma") +
      theme_void() +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        plot.caption = element_text(size = 8, hjust = 0)) +
      labs(
        title = paste0(team," shots"),
        shape = 'Made',
        caption = "Meyappan Subbaiah (@msubbaiah1) Data Accessed via ncaahoopR")
    return(p1)
  }

  p1 = ggplot() +
    geom_point(data = team_shots,
               aes(
                 x = x,
                 y = y,
                 shape = outcome),
               color = color,
               size = 3) +
    geom_polygon(data = side_one, aes(x = x, y = y, group = group), col = "gray") +
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
      plot.title = element_text(size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(size = 8, hjust = 0),
      plot.background = element_rect(fill = 'cornsilk')) +
    labs(
      title = paste0(team," shots"),
      shape = 'Made',
      caption = "Meyappan Subbaiah (@msubbaiah1) Data Accessed via ncaahoopR")
  return(p1)
}
