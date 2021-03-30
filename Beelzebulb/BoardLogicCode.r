library(tidyverse)

# First test board
df1 <- tibble(
  c1 = list(
    tibble(n = 0, e = 0, s = 0, w = 0),
    tibble(n = 1, e = 1, s = 1, w = 0),
    tibble(n = 0, e = 0, s = 0, w = 0)
  ),
  c2 = list(
    tibble(n = 0, e = 0, s = 0, w = 0),
    tibble(n = 0, e = 0, s = 1, w = 1),
    tibble(n = 1, e = 1, s = 0, w = 0)
  ),
  c3 = list(
    tibble(n = 0, e = 0, s = 0, w = 0),
    tibble(n = 0, e = 0, s = 0, w = 0),
    tibble(n = 0, e = 1, s = 0, w = 1)
  ),
  c4 = list(
    tibble(n = 0, e = 0, s = 0, w = 0),
    tibble(n = 0, e = 1, s = 1, w = 0),
    tibble(n = 1, e = 0, s = 0, w = 1)
  ),
  c5 = list(
    tibble(n = 0, e = 0, s = 0, w = 0),
    tibble(n = 1, e = 0, s = 1, w = 1),
    tibble(n = 0, e = 0, s = 0, w = 0)
  )
)
# (N, E, S, W)

cellCheck <- function(df, col, row, prevConn){
  print(paste0(col, row, prevConn))
  if (col == 1 & row == 2){
    return(TRUE)
  }
  cell <- df[[col]][[row]]
  for (cell_dir in c("n", "s", "e", "w")){
    if (cell_dir != prevConn){
      if (cell[cell_dir] == 1){
        if (cell_dir == "n"){return(cellCheck(df, col, row-1, "s"))}
        if (cell_dir == "s"){return(cellCheck(df, col, row+1, "n"))}
        if (cell_dir == "e"){return(cellCheck(df, col+1, row, "w"))}
        if (cell_dir == "w"){return(cellCheck(df, col-1, row, "e"))}
      }
    }
  }
}

boardLogic <- function(board){
  # First step: check whether cells right next to bulb have any connection first
  if (df1[[4]][[2]][["e"]]==1){if (cellCheck(df1, 4, 2, "e")){return("GAME END")}}
  if (df1[[5]][[1]][["s"]]==1){if (cellCheck(df1, 5, 1, "s")){return("GAME END")}}
  if (df1[[5]][[3]][["n"]]==1){if (cellCheck(df1, 5, 3, "n")){return("GAME END")}}
  # Then, recursively check 
  return(FALSE)
}
print(boardLogic(df1))
