#' @title getNFL
#'
#' @description Calculates the most recent NFL playoff appearance date for all NFL teams based on data from Wikipedia \url{https://en.wikipedia.org/wiki/National_Football_League}
#' @export
#'
#' @examples
#' getNFL()
getNFL <- function(){

NFL_html <- xml2::read_html("https://en.wikipedia.org/wiki/National_Football_League")
NFL_html_tables <- rvest::html_nodes(NFL_html, "table")
NFL_team_table <- rvest::html_table(NFL_html_tables[[4]], fill=T)
names(NFL_team_table)[2] <- "Team"
NFL_team_table <- subset(NFL_team_table, !grepl(paste(c("Conference", "club"), collapse = "|"), NFL_team_table$Team))
NFL_team_table$Team <- gsub("[[A-Z]]", "", NFL_team_table$Team)
NFL_team_table$Team <- gsub("[[:punct:]]", "", NFL_team_table$Team)

most_recent <- c()
for(x in NFL_team_table$Team[1:32]) {
  slug <- paste0("https://en.wikipedia.org/wiki/", gsub(" ", "_", x))

team_html <- xml2::read_html(slug)
team_tables <- rvest::html_nodes(team_html, "table")
team_table1 <- as.data.frame(rvest::html_table(team_tables[[1]], fill = T))

if(nrow(team_table1) <2){
  team_table1 <- rvest::html_table(team_tables[[2]], fill = T)
}

years <- team_table1[grep("NFL", team_table1[, 1]), 1]
temp_recent_max <- max(as.numeric(unlist(strsplit(years, ", "))), na.rm = T)
most_recent <- rbind(most_recent, c(x, temp_recent_max))
}

most_recent <- data.frame(most_recent)

names(most_recent)[1:2] <- c("Team", "Season")
return(most_recent)
}
