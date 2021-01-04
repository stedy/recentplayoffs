#' @title getNHL
#'
#' @description Calculates most recent playoff appearence for all NHL teams from 1998 to present from the site \url{https://www.hockey-reference.com}.
#' Does not pull data for 2004-05 due to the NHL strike
#'
#' @export
#' @examples
#' getNHL()
getNHL <- function(){
  all_seasons <- c()
  current_year <- as.numeric(format(Sys.Date(), "%Y")) - 1
  for(x in c(1998:2004, 2006:current_year)){
    team_url <- paste0("https://www.hockey-reference.com/leagues/NHL_", as.character(x), ".html")
    team_html <- xml2::read_html(team_url)
    team_tables <- rvest::html_nodes(team_html, "table")

    Eastern_conference_table <- rvest::html_table(team_tables[[1]], fill=T)
    Eastern_conference_table <- subset(Eastern_conference_table, grepl("\\*", Eastern_conference_table[, 1]))
    Eastern_conf_teams <- unlist(strsplit(Eastern_conference_table[, 1], "\\*"))

    Western_conference_table <- rvest::html_table(team_tables[[2]], fill=T)
    Western_conference_table <- subset(Western_conference_table, grepl("\\*", Western_conference_table[, 1]))
    Western_conf_teams <- unlist(strsplit(Western_conference_table[, 1], "\\*"))

    combined_conf <- data.frame(Team = c(Eastern_conf_teams, Western_conf_teams),
                                Season = x)
    combined_conf <- subset(combined_conf, !grepl("[0-9]\\)", combined_conf$Team))
    all_seasons <- rbind(all_seasons, combined_conf)
  }

  most_recent_NHL <- all_seasons[order(all_seasons$Season), ]
  most_recent_NHL <- by(most_recent_NHL, most_recent_NHL["Team"], tail, n=1)
  return(Reduce(rbind, most_recent_NHL))
}
