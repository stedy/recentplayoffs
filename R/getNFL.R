#' @title getNFL
#'
#' @description Calculates the most recent NFL playoff appearance date for all NFL teams based on data from Wikipedia \url{https://en.wikipedia.org/wiki/National_Football_League}
#' @export
#'
#' @examples
#' getNFL()
getNFL <- function(){
  all_seasons <- c()
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  matches <- c()
  for(x in c(1998:current_year)){
    team_url <- paste0("https://www.pro-football-reference.com/years/", as.character(x), "/")
    team_html <- xml2::read_html(team_url)
    team_tables <- rvest::html_nodes(team_html, "table")

    AFC_table <- as.data.frame(rvest::html_table(team_tables[[1]], fill=T))
    AFC_table <- subset(AFC_table, grepl("\\*|\\+", AFC_table[, 1]))
    AFC_teams <- unlist(strsplit(AFC_table[, 1], "\\*|\\+"))

    NFC_table <- as.data.frame(rvest::html_table(team_tables[[2]], fill=T))
    NFC_table <- subset(NFC_table, grepl("\\*|\\+", NFC_table[, 1]))
    NFC_teams <- unlist(strsplit(NFC_table[, 1], "\\*|\\+"))

    combined_conf <- data.frame(Team = c(AFC_teams, NFC_teams),
                                Season = x)
    all_seasons <- rbind(all_seasons, combined_conf)
  }

  most_recent_NFL <- all_seasons[order(all_seasons$Season), ]
  most_recent_NFL <- by(most_recent_NFL, most_recent_NFL["Team"], tail, n=1)
  return(Reduce(rbind, most_recent_NFL))
}
