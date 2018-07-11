#' @title summarizePlayoffDrought
#' @description Summarizes longest playoff drought by league for the 4 major US professional sports leagues
#'
#' @importFrom utils head
#' @export
#'
summarizePlayoffDrought <- function(){
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
  longest <- by(all_sports, all_sports["League"], head, n=1)
  return(Reduce(rbind, longest))
}
