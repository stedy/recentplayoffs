#' @title summarizePlayoffDrought
#' @description Summarizes longest playoff drought by league for the 4 major US professional sports leagues
#'
#' @importFrom utils head
#' @export
#'
#' @param defunct Should defunct teams be omitted?
summarizePlayoffDrought <- function(defunct = NULL){

  defunct_teams <- c("Seattle SuperSonics", "New Jersey Nets", "Mighty Ducks of Anaheim", "Atlanta Thrashers")

  NFL <- getNFL()
  NFL$season <- as.numeric(as.character(NFL$season))
  NFL$League <- "NFL"

  MLB <- getMLB()
  MLB$season <- as.numeric(as.character(MLB$season))
  MLB$League <- "MLB"

  NBA <- getNBA()
  NBA$season <- as.numeric(as.character(NBA$season))
  NBA$League <- "NBA"

  NHL <- getNHL()
  NHL$season <- as.numeric(as.character(NHL$season))
  NHL$League <- "NHL"

  all_sports <- rbind.data.frame(NFL, MLB, NBA, NHL)
  all_sports <- all_sports[order(all_sports$season), ]
  if(!is.null(defunct)){
    all_sports <- subset(all_sports, !grepl(paste(defunct_teams, collapse = "|"), all_sports$Team))
  }
  longest <- by(all_sports, all_sports["League"], head, n=1)
  return(Reduce(rbind, longest))
}
