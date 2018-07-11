#' @title getNBA
#'
#' @description Calculates the most recent playoff appearance for every NBA team based on data from \url{https://www.basketball-reference.com}
#'
#' @export
#' @importFrom utils tail
#'
#' @examples
#' getNBA()
getNBA <- function(){
all_seasons <- c()
for(x in 1998:2018){
  team_url <- paste0("https://www.basketball-reference.com/leagues/NBA_", as.character(x), ".html")
  team_html <- xml2::read_html(team_url)
  team_html_tables <- rvest::html_nodes(team_html, "table")

  Eastern_conference_season_table <- rvest::html_table(team_html_tables[[1]], fill=T)
  Eastern_conference_season_table <- subset(Eastern_conference_season_table, grepl("\\*", Eastern_conference_season_table[, 1]))
  Eastern_conf_teams <- unlist(strsplit(Eastern_conference_season_table[, 1], "\\*"))

  Western_conference_season_table <- rvest::html_table(team_html_tables[[2]], fill=T)
  Western_conference_season_table <- subset(Western_conference_season_table, grepl("\\*", Western_conference_season_table[, 1]))
  Western_conf_teams <- unlist(strsplit(Western_conference_season_table[, 1], "\\*"))

  combined_conf <- data.frame(Team = c(Eastern_conf_teams, Western_conf_teams), Season = x)
  combined_conf <- subset(combined_conf, !grepl("[0-9]\\)", combined_conf$Team))
  all_seasons <- rbind(all_seasons, combined_conf)
}

most_recent_NBA <- all_seasons[order(all_seasons$Season), ]
most_recent_NBA <- by(most_recent_NBA, most_recent_NBA["Team"], tail, n=1)

return(Reduce(rbind, most_recent_NBA))
}
