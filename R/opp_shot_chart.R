#' Plot all shots allowed by a team
#'
#' Plots team shot locations from one game or multiple games.
#'
#' @param game_ids Vector of ESPN game_ids
#' @param team Team Name
#' @param heatmap Use a density-type heatmap (Default = FALSE)
#' @export
opp_shot_chart <- function(game_ids, team, heatmap = F) {
  if(any(is.na(game_ids))) {
    error("game_ids missing with no default")
  }
  if(any(is.na(team))) {
    error("team missing with no default")
  }
  df <- get_shot_locs(game_ids)

  if(!is.null(df)) {
    team_shots <- df %>%
      dplyr::filter(
        !team_name %in% c(team, dict$ESPN_PBP[dict$ESPN == team])
      ) %>%
    dplyr::mutate(
      x_flipped = y,
      y_flipped = x,
      x_transformed = dplyr::case_when(
        shooting_team == "home" ~ -y,
        TRUE ~ y
      ),
      y_transformed = dplyr::case_when(
        shooting_team == "home" ~ -x,
        TRUE ~ x
      )
    ) %>%
      dplyr::select(-x, -y) %>%
      dplyr::rename(
        x = x_transformed,
        y = y_transformed
      )

    ### only pick one color
    color <- as.character(unique(team_shots$color))[1]

    if(heatmap){
      p1 <-
        sportyR::geom_basketball(
          league = "ncaa",
          color_updates = list(
            plot_background = NULL,
            defensive_half_court = "#d2ab6f00",
            offensive_half_court = "#d2ab6f00",
            court_apron = "#d2ab6f00",
            center_circle_outline = "gray",
            center_circle_fill = "#d2ab6f00",
            division_line = "gray",
            endline = "gray",
            sideline = "gray",
            two_point_range = c("#d2ab6f00", "#d2ab6f00"),
            three_point_line = c("gray", "#d2ab6f00"),
            painted_area = c("#d2ab6f00", "#d2ab6f00"),
            lane_boundary = c("gray", "#ff552e"),
            free_throw_circle_outline = "gray",
            free_throw_circle_fill = "#d2ab6f00",
            free_throw_circle_dash = "#d2ab6f00",
            lane_space_mark = "gray",
            inbounding_line = "gray",
            substitution_line = "gray",
            baseline_lower_defensive_box = "gray",
            lane_lower_defensive_box = "gray",
            team_bench_line = "gray",
            restricted_arc = "gray",
            backboard = "gray",
            basket_ring = "gray",
            net = "#ffffff80"
          ),
          rotation = 270,
          display_range = "offense"
        ) +
        ggplot2::stat_density_2d(
          data = team_shots,
          ggplot2::aes(
            x = x,
            y = y,
            fill = stat(density / max(density))
          ),
          geom = "raster",
          contour = FALSE,
          interpolate = TRUE,
          n = 200
        ) +
        ggplot2::scale_fill_viridis_c(
          "Shot Frequency",
          limits = c(0, 1),
          breaks = c(0, 1),
          labels = c("Lower", "Higher"),
          option = "plasma"
        ) +
        ggplot2::labs(
          title = paste(team, "Shots Against"),
          caption = "Meyappan Subbaiah (@msubbaiah1) Data Accessed via ncaahoopR"
        )
      p1 <- gginnards::move_layers(p1, "GeomPolygon", position = "top")
      return(p1)
    }

    p1 <-
      suppressMessages(
        sportyR::geom_basketball(
          league = "ncaa",
          color_updates = list(
            plot_background = "cornsilk",
            defensive_half_court = "cornsilk",
            offensive_half_court = "cornsilk",
            court_apron = "cornsilk",
            center_circle_outline = "gray",
            center_circle_fill = "cornsilk",
            division_line = "gray",
            endline = "gray",
            sideline = "gray",
            two_point_range = c("cornsilk", "cornsilk"),
            three_point_line = c("gray", "cornsilk"),
            painted_area = c("cornsilk", "cornsilk"),
            lane_boundary = c("gray", "cornsilk"),
            free_throw_circle_outline = "gray",
            free_throw_circle_fill = "cornsilk",
            free_throw_circle_dash = "cornsilk",
            lane_space_mark = "gray",
            inbounding_line = "gray",
            substitution_line = "gray",
            baseline_lower_defensive_box = "gray",
            lane_lower_defensive_box = "gray",
            team_bench_line = "gray",
            restricted_arc = "gray",
            backboard = "gray",
            basket_ring = "gray",
            net = "cornsilk"
          ),
          display_range = "offense",
          rotation = 270
        ) +
          ggplot2::geom_point(
            data = team_shots,
            ggplot2::aes(
              x = x,
              y = y,
              shape = stringr::str_to_title(outcome)
            ),
            color = color,
            size = 3
          ) +
          ggplot2::scale_color_manual(values = color) +
          ggplot2::theme(
            legend.background = ggplot2::element_rect(
              fill = "cornsilk",
              color = "cornsilk"
            ),
            legend.key = ggplot2::element_rect(
              fill = "cornsilk",
              color = "cornsilk"
            )
          ) +
          ggplot2::labs(
            title = paste(team, "Shots Against"),
            shape = "Shot Outcome",
            caption = "Meyappan Subbaiah (@msubbaiah1) Data Accessed via ncaahoopR"
          )
      )
    return(p1)
  }
}

