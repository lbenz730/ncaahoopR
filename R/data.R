#' A data frame with ESPN team names to be used in the ncaahoopR package.
#'
#' @docType data
#' @format A data frame with ESPN team names to be used in the ncaahoopR package.
#' \describe{
#'  \item{team}{the name of the team to be supplied to function in ncaahoopR package}
#'  \item{id}{team id; used in ESPN URLs}
#'  \item{link}{team URL link; used in ESPN URLs}
#'  \item{espn_abbrv}{the abbreviation of the team's name on ESPN}
#' }
#'
#' @details Simply look for the spelling of your deired team in the team column
#' to use in ncaahoopR functions.
#'
#' @examples
#' data(ids)
#'
"ids"

#' A data frame with team names from various college basketball sites
#'
#' @docType data
#' @format A data frame with team names from various college basketball sites
#' \describe{
#'  \item{NCAA}{the name of the team, as listed on the NCAA website}
#'  \item{ESPN}{the name of the team, as listed in ESPN URLs}
#'  \item{ESPN_PBP}{the name of the team, as listed on the ESPN Play-By-Play logs}
#'  \item{Warren_Nolan}{the name of the team, as listed on WarrenNolan.com}
#'  \item{Trank}{the name of the team, as listed on barttorvik.com}
#'  \item{name_247}{the name of the team, as listed on 247Sports.com}
#'  \item{conference}{the name of the conference of the team}
#'  \item{sref_link}{the slug for a team on sports reference}
#'  \item{sref_name}{the name of the team on sports reference}
#' }
#'
#' @details Convert between team names in various systems
#'
#' @examples
#' data(dict)
#'
"dict"

#' A data frame with team colors
#'
#' @docType data
#' @format A data frame with team colors
#' \describe{
#'  \item{ncaa_name}{the name of the team, as listed on the NCAA website (same as dict$NCAA)}
#'  \item{espn_name}{the name of the team, as listed in ESPN URLs (same as dict$ESPN)}
#'  \item{primary_color}{hexcode for team's primary color}
#'  \item{secondary_color}{hexcode for team's secondary color, when available}
#'  \item{tertiary_color}{hexcode for team's tertiary color, when available}
#'  \item{color_4}{hexcode for team's 4th color, when available}
#'  \item{color_5}{hexcode for team's 5th color, when available}
#'  \item{color_6}{hexcode for team's 6th color, when available}
#'  \item{logo_url}{the link to a the team's logo}
#'  \item{color_3}{an alternate specification for a team's tertiary color, when available}
#'  \item{conference}{the conference to which a team belongs}
#' }
#'
#' @details Convert between team names in various systems
#'
#' @examples
#' data(ncaa_colors)
#'
"ncaa_colors"
