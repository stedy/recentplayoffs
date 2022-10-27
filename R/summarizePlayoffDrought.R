#' @title summarizePlayoffDrought
#' @description Summarizes longest playoff drought by league for the 4 major US professional sports leagues
#'
#' @importFrom utils head
#' @export
#'
#' @param defunct Should defunct teams be omitted?
summarizePlayoffDrought <- function(defunct = NULL){

  defunct_teams <- c("Seattle SuperSonics", "New Jersey Nets", "Mighty Ducks of Anaheim", "Atlanta Thrashers",
                     "St. Louis Rams", "Washington Redskins", "San Diego Chargers", "Oakland Raiders")

  NFL <- getNFL()
  NFL$Season <- as.numeric(as.character(NFL$Season))
  NFL$League <- "NFL"
  NFL <- NFL[which(NFL$Season == min(NFL$Season)), ]

  MLB <- getMLB()
  MLB$Season <- as.numeric(as.character(MLB$Season))
  MLB$League <- "MLB"
  MLB <- MLB[which(MLB$Season == min(MLB$Season)), ]

  NBA <- getNBA()
  NBA$Season <- as.numeric(as.character(NBA$Season))
  NBA$League <- "NBA"
  if(!is.null(defunct)){
    NBA <- subset(NBA, !grepl(paste(defunct_teams, collapse = "|"), NBA$Team))
  }
  NBA <- NBA[which(NBA$Season == min(NBA$Season)), ]

  NHL <- getNHL()
  NHL$Season <- as.numeric(as.character(NHL$Season))
  NHL$League <- "NHL"
  if(!is.null(defunct)){
    NHL <- subset(NHL, !grepl(paste(defunct_teams, collapse = "|"), NHL$Team))
  }
  NHL <- NHL[which(NHL$Season == min(NHL$Season)), ]

  all_sports <- rbind.data.frame(NFL, MLB, NBA, NHL)
  return(all_sports)
}
