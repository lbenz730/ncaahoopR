#' All the available soccer leagues and tournaments with ESPN scoreboard urls
#'
#' @docType data
#' @format A data frame with ESPN team names to be used in the ncaahoopR package.
#' \describe{
#'  \item{team}{the name of the team to be suplied to function in ncaahoopR package}
#'  \item{id}{team id; used in ESPN URLs}
#'  \item{link}{team URL link; used in ESPN URLs}
#' }
#'
#' @details Simply look for the spelling of your deired team in the team column
#' to use in ncaahoopR functions.
#'
#' @examples
#' data(ids)
#'
"ids"

#' All the available soccer leagues and tournaments with ESPN scoreboard urls
#'
#' @docType data
#' @format A data frame with team names from various college basketball sites
#' \describe{
#'  \item{NCAA}{the name of the team, as listed on the NCAA website}
#'  \item{ESPN}{the name of the team, as listed on the ESPN URLs}
#'  \item{ESPN_PBP}{the name of the team, as listed on the ESPN Play-By-Play logs}
#'  \item{Warren_Nolan}{the name of the team, as listed on WarrenNolan.com}
#'  \item{Trank}{the name of the team, as listed on barttorvik.com}
#'  \item{name_247}{the name of the team, as listed on 247Sports.com}
#' }
#'
#' @details Convert between team names in various systems
#'
#' @examples
#' data(dict)
#'
"dict"
