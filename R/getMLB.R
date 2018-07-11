#' @title getMLB
#'
#' @description This function calculates the most recent MLB playoff appearance date for all MLB teams based on data from Wikipedia \url{https://en.wikipedia.org/wiki/Major_League_Baseball}
#'
#' @export
#' @examples
#' getMLB()
getMLB <- function(){
MLB_teams_html <- xml2::read_html("https://en.wikipedia.org/wiki/Major_League_Baseball")
MLB_teams_html_tables <- rvest:: html_nodes(MLB_teams_html, "table")
MLB_teams_html_table <- rvest::html_table(MLB_teams_html_tables[[3]], fill = T)
MLB_teams_html_table <- subset(MLB_teams_html_table, !grepl("League", MLB_teams_html_table$Team))
MLB_teams_html_table$Team = ifelse(MLB_teams_html_table$Team == "Texas Rangers", "Texas Rangers (baseball)",
                                   MLB_teams_html_table$Team)

playoffs <- paste(c("Division titles", "Wild card"), collapse="|")

most_recent_MLB <- c()
for(x in MLB_teams_html_table$Team){
  slug <- paste0("https://en.wikipedia.org/wiki/", gsub(" ", "_", x))

  team_html <- xml2::read_html(slug)
  team_tables <- rvest::html_nodes(team_html, "table")
  team_table1 <- rvest::html_table(team_tables[[1]], fill=T)

  if(nrow(team_table1) <2){
    team_table1 <- rvest::html_table(team_tables[[2]], fill = T)
  }

  names(team_table1) <- c("classification", "value")

  temp_results <- subset(team_table1, grepl(playoffs, team_table1$classification))
  temp_results$value <- gsub(paste(c(" Central: ", "East: "), collapse="|"), "", temp_results$value)

  temp_values <- c()
  for(y in temp_results$value){
    temp_values <- c(temp_values, substring(y, seq(1, nchar(y), 4), seq(4, nchar(y), 4)))
  }
  most_recent_MLB <- rbind(most_recent_MLB, c(x, max(as.numeric(temp_values), na.rm=T)))
}

most_recent_MLB <- data.frame(most_recent_MLB)

names(most_recent_MLB)[1:2] <- c("Team", "Season")
return(most_recent_MLB)
}
