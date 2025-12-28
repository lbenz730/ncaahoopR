#' Get Shot Locations
#'
#' Extracts Shot location data for a specific game
#'
#' @param game_ids Vector of ESPN \code{game_ids}
#'
#' @return A data frame with shot details (including location) for given
#'   \code{game_ids}
#'
#' @importFrom rlang .data
#' @export
get_shot_locs <- function(game_ids) {
  if (any(is.na(game_ids))) {
    stop("game_ids missing with no default")
  }
  n_games <- length(game_ids)
  for (i in 1:n_games) {
    message(paste("Getting Shots for Game", i, "of", n_games))
    url <- paste0(
      "https://www.espn.com/mens-college-basketball/playbyplay?gameId=",
      game_ids[i]
    )

    ### Try and get PBP data
    txt <- try(RCurl::getURL(url), silent = TRUE)

    ### Check if PBP Data is Available
    if (class(txt) == "try-error") {
      message("No shot location data available for this game.")
      next
    } else {
      x <- strsplit(txt, '\\"plays\":')[[1]]
      if (length(x) > 1) {
        x <- x[2]
        tmp <- jsonlite::fromJSON(gsub(',\"tms\":\\{.*', "", x))
      } else {
        message("No shot location data available for this game.")
        next
      }
    }

    date <- gsub(
      "^.*-\\s+",
      "",
      gsub(
        "\\|.*$",
        "",
        strsplit(txt, "Men&#x27;s College Basketball Play-By-Play")[[1]][2]
      )
    )
    date <- as.Date(stripwhite(date), "%b %d, %Y")

    info <- jsonlite::fromJSON(
      gsub(
        ',\"ntrlSte.*$',
        "",
        gsub('^.*,\"tms\":', "", txt)
      )
    )

    total_df <- data.frame(
      "game_id" = game_ids[i],
      "date" = date,
      "shot_text" = tmp$text,
      "x" = tmp$coordinate$x,
      "y" = tmp$coordinate$y,
      "shooting_team" = tmp$homeAway,
      stringsAsFactors = FALSE
    )

    total_df <- total_df %>%
      dplyr::mutate(
        "outcome" = ifelse(grepl("made", .data$shot_text), "made", "missed"),
        "shooter" = stripwhite(gsub("made.*", "", .data$shot_text)),
        "shooter" = stripwhite(gsub("missed.*", "", .data$shooter)),
        "assisted" = stripwhite(
          gsub(
            ".{1}$",
            "",
            gsub(".*Assisted by", "", .data$shot_text)
          )
        ),
        "assisted" = stripwhite(
          ifelse(
            grepl("made", .data$assisted) | grepl("missed", .data$assisted),
            NA,
            .data$assisted
          )
        ),
        "three_pt" = grepl("Three Point", .data$shot_text)
      ) %>%
      dplyr::mutate(
        "x_transformed" = dplyr::case_when(
          stringr::str_detect(.data$shot_text, "Free Throw") == TRUE ~ 28,
          TRUE ~ .data$y - 41.75
        ),
        "y_transformed" = dplyr::case_when(
          stringr::str_detect(.data$shot_text, "Free Throw") == TRUE ~ 0,
          TRUE ~ .data$x - 25
        )
      ) %>%
      dplyr::mutate(
        "x" = ifelse(
          tmp$homeAway == "away",
          .data$x_transformed,
          -1 * .data$x_transformed
        ),
        "y" = ifelse(
          tmp$homeAway == "away",
          .data$y_transformed,
          -1 * .data$y_transformed
        )
      ) %>%
      dplyr::mutate(
        "color" = paste0(
          "#",
          ifelse(
            tmp$homeAway == "away",
            info$away$teamColor,
            info$home$teamColor
          )
        ),
        "team_name" = ifelse(
          tmp$homeAway == "away",
          info$away$shortDisplayName,
          info$home$shortDisplayName
        )
      ) %>%
      dplyr::select(
        -"x_transformed",
        -"y_transformed"
      )

    if (!exists("total_df_all")) {
      total_df_all <- total_df
    } else {
      total_df_all <- rbind(total_df_all, total_df)
    }
  }

  if (!exists("total_df_all")) {
    return(NULL)
  }
  return(total_df_all)
}
