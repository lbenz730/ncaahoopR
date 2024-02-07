### Function to clean PBP data
clean <- function(data, half, OTs) {
  cleaned <- 
    data %>% 
    dplyr::mutate('half' = period.number,
                  'time_remaining_half' = clock.displayValue,
                  'description' = text,
                  'home_score' = homeScore,
                  'away_score' = awayScore,
                  'action_team' = homeAway,
                  'scoring_play' = scoringPlay)
  if('isFoul' %in% names(cleaned)) {
    cleaned$foul <- cleaned$isFoul 
    cleaned$foul[is.na(cleaned$foul)] <- F
  } else {
    cleaned$foul <- F 
  }
  
  cleaned$scoring_play[is.na(cleaned$scoring_play)] <- F
  # cleaned$time_remaining_half[1] <- ifelse(half <= 2, "20:00", "5:00")
  mins <- suppressWarnings(as.numeric(gsub(":.*","", cleaned$time_remaining_half)))
  secs <- suppressWarnings(as.numeric(gsub(".*:","", cleaned$time_remaining_half)))
  cleaned$secs_remaining <- max(20 * (2 - half), 0) * 60 +
    5 * 60 * max((OTs * as.numeric(half <= 2)), ((OTs + 2 - half) * as.numeric(half > 2))) + 60 * mins + secs
  if(half == 1) {
    if(is.na(cleaned$scoring_play[1]) & cleaned$scoring_play[1]) {
      cleaned[1, c("home_score", "away_score")] <- c(0,0)
    }
  }
  
  cleaned <- 
    cleaned %>% 
    dplyr::select(half, time_remaining_half, secs_remaining, description, home_score, away_score, foul, scoring_play, action_team)
  
  return(cleaned)
}

### Function to extract full team names from PBP
extract_teams <- function(html) {
  z <- gsub('.*"tms":', '', html)
  z <- gsub(',"isConferenceGame".*$', '', z)
  z <- gsub('},"meta".*$', '', z)
  z <- gsub(',"ntrlSte":.*$', '', z)
  z <- jsonlite::fromJSON(z)
  
  
  teams <- stripwhite(c(z$home$shortDisplayName, 
                        z$away$shortDisplayName))
  return(teams)
}

