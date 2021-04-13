library(stringr)
library(dplyr)
library(shiny)
library(DBI)
library(jsonlite)
library(shinydashboard)
library(rsconnect)
library(shinyjs)

#Global Variables 
boardState <- tibble(
  c1 = list(
    tibble(n = 0, e = 0, s = 0, w = 0),
    tibble(n = 1, e = 1, s = 1, w = 0),
    tibble(n = 0, e = 0, s = 0, w = 0)
  ),
  c2 = list(
    tibble(n = 0, e = 0, s = 0, w = 0),
    tibble(n = 0, e = 1, s = 0, w = 1),
    tibble(n = 0, e = 0, s = 0, w = 0)
  ),
  c3 = list(
    tibble(n = 0, e = 0, s = 0, w = 0),
    tibble(n = 0, e = 1, s = 0, w = 1),
    tibble(n = 0, e = 0, s = 0, w = 0)
  ),
  c4 = list(
    tibble(n = 0, e = 0, s = 0, w = 0),
    tibble(n = 0, e = 1, s = 0, w = 1),
    tibble(n = 0, e = 0, s = 0, w = 0)
  ),
  c5 = list(
    tibble(n = 0, e = 0, s = 0, w = 0),
    tibble(n = 1, e = 0, s = 1, w = 1),
    tibble(n = 0, e = 0, s = 0, w = 0)
  )
)

cellCheckHelper <- function(df, col, row, dirToCheck){
  firstConnection <- boardState[[col]][[row]][[dirToCheck]]
  if (firstConnection == "1" | firstConnection == 1){
    firstConnection <- 1
  }
  else{
    return(FALSE)
  }
  tryCatch(expr={
    if (dirToCheck == "n"){
      if (firstConnection & as.numeric(boardState[[col]][[row-1]][["s"]])){
        return(c(col,row-1, "s"))
      }
    }
    else if (dirToCheck == "s"){
      if (firstConnection & as.numeric(boardState[[col]][[row+1]][["n"]])){
        return(c(col,row+1, "n"))
      }
    }
    else if (dirToCheck == "e"){
      if (firstConnection & as.numeric(boardState[[col+1]][[row]][["w"]])){
        return(c(col+1,row, "w"))
      }
    }
    else if (dirToCheck == "w"){
      if (firstConnection & as.numeric(boardState[[col-1]][[row]][["e"]])){
        return(c(col-1,row, "e"))
      }
    }
  }, error = function(e){return(FALSE)})
  return(FALSE)
}

cellCheck <- function(df, col, row, prevConn){
  if (col == 1 & row == 2){
    return(TRUE)
  }
  for (dir in c("n", "s", "e", "w")){
    if (dir != prevConn){
      connected <- cellCheckHelper(df, col, row, dir)
      print(connected)
      if (connected != FALSE){
        return(cellCheck(df, as.numeric(connected[[1]]), as.numeric(connected[[2]]), connected[[3]]))
      }
    }
  }
  return(FALSE)
}

boardLogic <- function(board){
  # First step: check whether cells right next to bulb have any connection first
  return(cellCheck(boardState, 5, 2, "e"))
  # Then, recursively check 
}

newBoard <- function(board){
  boardState <<- tibble(
    c1 = list(
      tibble(n = 0, e = 0, s = 0, w = 0),
      tibble(n = 0, e = 1, s = 1, w = 1),
      tibble(n = 0, e = 0, s = 0, w = 0)
    ),
    c2 = list(
      tibble(n = 0, e = 0, s = 0, w = 0),
      tibble(n = 0, e = 0, s = 0, w = 0),
      tibble(n = 0, e = 0, s = 0, w = 0)
    ),
    c3 = list(
      tibble(n = 0, e = 0, s = 0, w = 0),
      tibble(n = 0, e = 0, s = 0, w = 0),
      tibble(n = 0, e = 0, s = 0, w = 0)
    ),
    c4 = list(
      tibble(n = 0, e = 0, s = 0, w = 0),
      tibble(n = 0, e = 0, s = 0, w = 0),
      tibble(n = 0, e = 0, s = 0, w = 0)
    ),
    c5 = list(
      tibble(n = 0, e = 0, s = 0, w = 0),
      tibble(n = 1, e = 0, s = 1, w = 1),
      tibble(n = 0, e = 0, s = 0, w = 0)
    )
  )
  conn <- getAWSConnection()
  for (i in c(1:15)){
    if (i == 6){
      dbExecute(conn, "UPDATE GameState SET img_num = 2, n = 1, s = 1, w = 0, e = 1 WHERE cell_number=6")
    }
    else if (i == 10){
      dbExecute(conn, "UPDATE GameState SET img_num = 3, n = 1, s = 1, w = 1, e = 0 WHERE cell_number=10")
    }
    else{
      dbExecute(conn, sprintf("UPDATE GameState SET img_num = 1, n = 0, s = 0, w = 0, e = 0 WHERE cell_number=%d", i))
    }
  }
  dbExecute(conn, "UPDATE StartGame SET start = 0")
  dbDisconnect(conn)
}

# AWS Connection
getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "student055",
    host = "esddbinstance-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "student055",
    password = "gVU4KpWT")
  conn
}

# General
nextPlayer <- function(turn){
  conn <- getAWSConnection()
  dbExecute(conn, sprintf("UPDATE TurnNumber SET turn = %d", turn+1))
  dbDisconnect(conn)
  return(turn+1)

}

startGame <- function(){
  conn <- getAWSConnection()
  dbExecute(conn, sprintf("UPDATE StartGame SET start = %d", 1))
  dbDisconnect(conn)
  return(1)
}

checkCell <- function(gridrow, gridcol){
  conn <- getAWSConnection()
  cell <- (gridrow-1)*5 + gridcol
  query_template <- sqlInterpolate(conn, "SELECT img_num FROM GameState WHERE cell_number = ?cell", cell = cell)
  query_cell <- dbGetQuery(conn, query_template)
  dbDisconnect(conn)
  return(query_cell[1, 1])
}

# REGISTER
createNewPlayerQuery <- function(conn,username,password){
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholder for  password
  querytemplate <- "INSERT INTO LeaderPlayer (playername,password) VALUES (?id1,?id2);" ## Edit the Database
  query<- sqlInterpolate(conn, querytemplate,id1=username,id2=password)
}

registerPlayer <- function(username, password){
  #open the connection
  conn <- getAWSConnection()
  query <- createNewPlayerQuery(conn, username, password)
  # This query could fail to run properly so we wrap it in a loop with tryCatch()
  success <- FALSE
  iter <- 0
  MAXITER <- 5
  while(!success & iter < MAXITER){
    iter <- iter+1
    tryCatch(
      {  # This is not a SELECT query so we use dbExecute
        result <- dbExecute(conn,query)
        success <- TRUE
      }, error=function(cond){print("registerPlayer: ERROR")
        print(cond)
        # The query failed, likely because of a duplicate playername
        #playername <- getRandomPlayerName(conn)
        # query <- createNewPlayerQuery(conn,playername,password)
      },
      warning=function(cond){print("registerPlayer: WARNING")
        print(cond)},
      finally = {print(paste0("Iteration ",iter," done."))
      }
    )
  } # end while loop
  # This may not have been successful
  if (!success) playername = NULL
  #Close the connection
  dbDisconnect(conn)
  username
}

registerModal <- function(failed = FALSE) {
  modalDialog(
    title = "Register an Account!",
    textInput("username1", "Enter a Username:"),
    passwordInput("password1", "Enter your Password:"),
    passwordInput("password2", "Confirm by re-entering the new password:"),
    "If successful, your account will be created with the specified Username and Password.",
    if (failed)
      div(tags$b("The passwords do not match. Try again.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("registerOK", "OK")
    )
  )
}

# LOGIN
loginModal <- function(failed = FALSE) {
  modalDialog(
    title = "Login",
    textInput("username2", "Enter your Username", ""),
    passwordInput("password3", "Enter your password:"),
    if (failed)
      div(tags$b("There is no registered player with that name and password. Try again or re-register.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("loginOK", "OK")
    )
  )
}

getPlayerID <- function(username,password){
  conn <- getAWSConnection()
  querytemplate <- "SELECT * FROM LeaderPlayer WHERE playername=?id1 AND password=?id2;"
  query<- sqlInterpolate(conn, querytemplate,id1=username,id2=password)
  result <- dbGetQuery(conn,query)
  if (nrow(result)==1){
    playerid <- result$playerid[1]
  } else {
    playerid <- 0
  }
  dbDisconnect(conn)
  playerid
}

# Game Lobby
gameLobby <- function(totalPlayers, failed = FALSE){
  if(totalPlayers > 3){
    modalDialog(
      title = "Players Currently In Waiting Room:",
      tableOutput("gameLobbyTable"),
      footer = tagList(
        actionButton("entergame", "Start Game")
      )
    )
  }else{
    modalDialog(
      title = "Players Currently In Waiting Room:",
      p("Waiting for 4 players in the lobby to start the game..."),
      tableOutput("gameLobbyTable"),
      footer = tagList(
        actionButton("exitLobby", "Quit Lobby")
      )
    )
  }
}

assignRoles <- function(players){
  roles <- c("Engineer", "Engineer", "Engineer", "Imposter")
  assigned <- sample(roles,length(roles),replace=FALSE)
  players["Roles"] <- assigned
  return(players)
}

getRole <- function(userid){
  conn <- getAWSConnection()
  q <- "SELECT Roles from GamePlayers WHERE PlayerID = ?userid"
  query <- sqlInterpolate(conn, q, userid=userid)
  role_vector <- dbGetQuery(conn, query)
  dbDisconnect(conn)
  role <- role_vector[1, 1]
  if(role == "Engineer"){
    return("Engineer! Work with others to reach the lightbulb!")
  }else{
    return("Imposter! Prevent Engineers from reaching the lightbulb!")
  }
}

#Get Physics Question
getPhysicsQn <- function(){
  conn <- getAWSConnection()
  qna <- dbGetQuery(conn, "SELECT qn_id, question, ans_id FROM PhysicsQn ORDER BY RAND() LIMIT 1")
  qid <- qna[1, 1]
  qn <- qna[1, 2]
  aid <- qna[1, 3]
  dbDisconnect(conn)
  return_list = list("qn" = qn, "aid" = aid, "qid" = qid)
  return(return_list)
}

getOptions <- function(qid){
  conn <- getAWSConnection()
  opt <- sqlInterpolate(conn, "SELECT choices, ans_id FROM PhysicsAns WHERE qn_id = ?qnid ORDER BY RAND()", qnid = qid)
  q_opt <- dbGetQuery(conn, opt)
  dbDisconnect(conn)
  return(q_opt)
}

getAnswer <- function(aid){
  conn <- getAWSConnection()
  opt <- sqlInterpolate(conn, "SELECT choices FROM PhysicsAns WHERE ans_id = ?aid ORDER BY RAND()", aid = aid)
  ans <- dbGetQuery(conn, opt)
  dbDisconnect(conn)
  return(ans[1, 1])
}

# Physics Question
answerPhysicsQn <- function(qn, q_opt, failed = FALSE){
  modalDialog(
    title = "Physics Question:",
    p(qn),
    lapply(1:nrow(q_opt), function(i){
      p(sprintf("%d ) %s", i, q_opt[i, 1]))
    }),
    selectInput("optionSelect", "Which Option is your answer?", choices = c(1:4), selected = 1),
    footer = tagList(
      actionButton("confirmOption", "Confirm"),
      modalButton("Cancel")
    )
  )
}

physicsResult <- function(result, ans){
  if(result == "correct"){
    modalDialog(
      title = "You got the right answer!",
      strong("You will Draw a Card.."),
      p("Please press the button below to continue on to your Action Phase to place a card!"),
      footer = tagList(
        actionButton("correctCont", "Continue")
      )
    )
  }else{
    modalDialog(
      title = "Your Answer is Wrong",
      strong("The correct answer is:"),
      p(ans),
      p("For getting the question wrong, you will be moving to the end phase"),
      footer = tagList(
        actionButton("wrongCont", "Continue")
      )
    )
  }
}


#Get Handcards
getHandCards <- function(userid){
  conn <- getAWSConnection()
  q_temp <- "SELECT Card_ID, Card_Name FROM HandCards WHERE PlayerID = ?userid"
  query_HandCards <- sqlInterpolate(conn, q_temp, userid = userid)
  exec_query <- dbGetQuery(conn, query_HandCards)
  dbDisconnect(conn)
  return(exec_query)
}

drawCard <- function(userid, numcards){
  conn <- getAWSConnection()
  #Draw Card
  query_draw_cards <- dbGetQuery(conn, sprintf("SELECT row_names, Card_ID, Card_Name FROM CardDeck ORDER BY RAND() LIMIT %d", numcards))
  # Assigning Draw Card to table
  querytemplate_drawcards <- "INSERT INTO HandCards (PlayerID, Card_ID, Card_Name) VALUES"
  sql_qry <- paste0(querytemplate_drawcards, paste(sprintf("(%d, %d, '%s')",
                                                           as.numeric(userid),
                                                           as.numeric(query_draw_cards$Card_ID),
                                                           query_draw_cards$Card_Name), collapse = ","))
  send_query <- dbExecute(conn, sql_qry)
  # Deleting Drawn card from the table
  for(i in 1:nrow(query_draw_cards)){
    remove_template <- "DELETE FROM CardDeck where row_names = ?row"
    qrc <- sqlInterpolate(conn, remove_template, row = query_draw_cards[i, 1])
    # print(qrc)
    query_remove_card <- dbExecute(conn, qrc)
  }
  dbDisconnect(conn)
}

# Choose Card Modal
chooseCard <- function(pos, handCards, failed=FALSE){
  cards <- nrow(handCards)
  modalDialog(
    title = "Choose Your Card",
    p("Which card would you like to play?"),
    lapply(1:cards, function(i){
      img(src='RedStoneSmall.png')
      switch(sprintf("%d", handCards[i, 1]),
             '1001' = {img(src='Wire_Designs-01.png')},
             '1002' = {img(src='Wire_Designs-02.png')},
             '1003' = {img(src='Wire_Designs-03.png')},
             '1004' = {img(src='Wire_Designs-04.png')},
             '1005' = {img(src='Wire_Designs-05.png')},
             {img(src='RedStoneSmall.png')}
      )
    }),
    
    selectInput("cardSelect", "Choice of Card", choices = handCards$Card_Name, selected = handCards$Card_Name[1]),
    footer = tagList(
      actionButton("confirmCard", "Confirm"),
      modalButton("Cancel")
    )
  )
}

removeCard <- function(userid, cardSelect){
  conn <- getAWSConnection()
  remove_template <- "DELETE FROM HandCards WHERE PlayerID = ?userid AND Card_Name = ?cardName LIMIT 1"
  qrc <- sqlInterpolate(conn, remove_template, userid = userid, cardName = cardSelect)
  query_remove_card <- dbExecute(conn, qrc)
  dbDisconnect(conn)
}

#Checking End Game Condition
checkWin <- function(){
  conn <- getAWSConnection()
  boardState <-dbGetQuery(conn, "SELECT * FROM GameState")
  dbDisconnect(conn)
  print(boardLogic(boardState))
  return(boardLogic(boardState))
}

checkLoss <- function(){
  conn <- getAWSConnection()
  cardDeck <- nrow(dbGetQuery(conn, "SELECT * FROM CardDeck"))
  boardState <- dbGetQuery(conn, "SELECT * FROM GameState")
  dbDisconnect(conn)
  if(cardDeck == 0 || nrow(boardState[boardState$img_num == 1, ]) == 0){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#End Game Modal
gameEnd <- function(failed = FALSE){
  modalDialog(
    title = "End Game Results",
    p("testing Input"),
    tableOutput("resultTable"),
    footer = tagList(
      # modalButton("Cancel"),
      actionButton("backToHome", "OK")
    )
  )
}

getEndTable <- function(imposter, failed = FALSE){
  conn <- getAWSConnection()
  tb <- dbGetQuery(conn, "SELECT * FROM GamePlayers")
  dbDisconnect(conn)
  if(imposter == "LOSE"){
    playerWin <- tb[tb$Roles == "Engineer", ] %>% mutate(Outcome = "Win")
    playerLose <- tb[tb$Roles == "Imposter", ] %>% mutate(Outcome = "Lose")
    tb2 <- rbind(playerWin, playerLose)
  }else if(imposter == "WIN"){
    playerLose <- tb[tb$Roles == "Engineer", ] %>% mutate(Outcome = "Lose")
    playerWin <- tb[tb$Roles == "Imposter", ] %>% mutate(Outcome = "Win")
    tb2 <- rbind(playerWin, playerLose)
  }
  return(tb2)
}

############################################ SERVER ################################################
server <- function(input, output, session) {
  # vals$turn is to know which player is next. Different from how many turns have passed.
  vals <- reactiveValues(password = NULL, userid=NULL,username=NULL, lobby=NULL, playercolor=1, turn = 0, gameStart = NULL, players = setNames(data.frame(matrix(ncol = 2, nrow = 0)), c('PlayerID', 'Username')))
  physics <- reactiveValues(qid = NULL, aid = NULL, qn = NULL, q_opt = NULL)
  pieces <- matrix(rep(0,3*5),nrow=3,ncol=5,byrow=TRUE)
  gamevals <- reactiveValues(turncount=0,pieces=pieces)
  
  output$playerturn <- renderText("The Game has yet to start.")
  output$assignedRole <- renderText("You have yet to be assigned a role for the game.")
  

  output$status <- renderText({
    if (is.null(vals$username))
      "Not logged in yet."
    else
      paste("Welcome back", vals$username)
  })
  
  
  getPlayerTurn <- function(){
    #open the connection
    conn <- getAWSConnection()
    turn <- dbGetQuery(conn, "SELECT turn FROM TurnNumber")[1, 1]
    dbDisconnect(conn)
    if(nrow(vals$players) != 0){
      vals$turn <- turn
      print(vals$players[(vals$turn%%4)+1, 2])
      output$playerturn <- renderText(paste(sprintf("%s's turn", vals$players[(vals$turn%%4)+1, 2])))
      return(vals$players[(vals$turn%%4)+1, 2])
    }
  }
  
  ## Register
  observeEvent(input$register, showModal(registerModal(failed=FALSE)))
  observeEvent(input$registerOK, {
    # Check that password1 exists and it matches password2
    if (str_length(input$password1) >0 && (input$password1 == input$password2)) {
      #store the password and close the dialog
      vals$password <- input$password1
      #print(vals$password) # for debugging
      vals$username = registerPlayer(input$username1, vals$password)
      if (!is.null(vals$username)){
        vals$userid <- getPlayerID(vals$username,vals$password)
      }
      #print(vals$playerid) # for debugging
      removeModal()
    } else {
      showModal(passwordModal(failed = TRUE))
    }
  })
  
  ## Login 
  observeEvent(input$login, showModal(loginModal(failed=FALSE)))
  observeEvent(input$loginOK, {
    # Get the playerID and check if it is valid
    playerid <- getPlayerID(input$username2,input$password3)
    if (playerid>0) {
      vals$userid <- playerid
      vals$username <- input$username2
      removeModal()
      removeUI(selector = "#login")
      removeUI(selector = "#register")
    } else {
      showModal(loginModal(failed = TRUE))
    }
  })
  
  ## Logout 
  
  ## Enter Game Lobby
  output$buttonGameLobby <- renderUI({
    req(vals$userid)
    tagList(
      actionButton("enterGameLobby", "Enter Game Lobby")
      # actionButton("logout", "Log Out")
    )
  })
  
  output$backToGame <- renderUI({
    req(vals$gameStart)
    tagList(
      actionButton("backToGame", "Back to Game Board")
    )
  })
  
  observeEvent(input$enterGameLobby, {
    #Inserting Player into GameLobby Table
    conn <- getAWSConnection()
    querytemplate <- "INSERT INTO GameLobby (PlayerID, Username) VALUES (?playerid, ?username)"
    query <- sqlInterpolate(conn, querytemplate, playerid = vals$userid, username = vals$username)
    result <- dbExecute(conn, query)
    #Obtaining the GameLobby table from database
    vals$players <- dbGetQuery(conn, "SELECT PlayerID, Username FROM GameLobby LIMIT 4")
    dbDisconnect(conn)
    totalPlayers <- nrow(vals$players)
    showModal(gameLobby(totalPlayers = totalPlayers, failed = FALSE))
  })
  
  observeEvent(input$exitLobby, {
    conn <- getAWSConnection()
    querytemplate <- "DELETE FROM GameLobby WHERE PlayerID = ?id"
    qrc <- sqlInterpolate(conn, querytemplate, id = vals$userid)
    query <- dbExecute(conn, qrc)
    dbDisconnect(conn)
    removeModal()
  })
  
  refresh <- function(page = NULL){
    conn <- getAWSConnection()
    lobby <- dbGetQuery(conn, "SELECT PlayerID, Username FROM GameLobby")
    dbDisconnect(conn)
    if (page == "HomePage"){
      print(paste0("There are ", nrow(lobby), " People in the lobby"))
      if(nrow(lobby) > 3){
        removeUI(selector = "#enterGameLobby")
        output$LobbyFull <- renderUI({
          req(vals$userid)
          tagList(
            p("There is either a Game Ongoing or more than 3 people in the Game Lobby")
          )
        })
      }else if(nrow(lobby) < 4){
        removeUI(selector = "#LobbyFull")
        output$buttonGameLobby <- renderUI({
          req(vals$userid)
          tagList(
            actionButton("enterGameLobby", "Enter Game Lobby")
            # actionButton("logout", "Log Out")
          )
        })
      }
    }else if(page == "GameLobby"){
      lobbyTable <- lobby[1:4, ]
      # print(lobby)
      vals$players <- lobbyTable
      output$gameLobbyTable <- renderTable(lobbyTable)
    }
    
  }
  
  checkGS <- function(){
    conn <- getAWSConnection()
    hasGameStarted <- dbGetQuery(conn, "SELECT start FROM StartGame")[1, 1]
    if(hasGameStarted == 1){
      updateTabsetPanel(session, "tabs", selected = "game")
      drawCard(vals$userid, 3)
      removeModal()
      Sys.sleep(2)
      dbExecute(conn, sprintf("UPDATE StartGame SET start = %d", 0))
    }
    dbDisconnect(conn)
  }
  
  ## Game Tab 
  observeEvent(input$backToGame, {
    updateTabsetPanel(session, "tabs", selected = "game")
  })
  
  ### Board Image
  renderCell <- function(gridrow,gridcol,cell_num=1){
    renderImage({
      #select the icon appropriate for this cell
      #imageid <- 1
      #if (!is.null(gamevals$pieces)) #imageid <- cell_num#gamevals$pieces[gridrow,gridcol]+1
      imgsrc=switch(cell_num,"www/blanksmall.png","www/battery.png","www/lightbulb.png","www/Wire_Designs-01.png","www/Wire_Designs-02.png","www/Wire_Designs-03.png","www/Wire_Designs-04.png", "www/Wire_Designs-05.png")
      # Unfortunately, we are not able to re-size the image and still have the click event work.
      # So the image files must have exactly the size we want.
      # Also, z-order works only if 'position' is set.
      list(src=imgsrc,style="position:relative;z-order:999")
    },deleteFile=FALSE)
  }
  
  updateGameState <- function(){
    conn <- getAWSConnection()
  result <- dbGetQuery(conn, "SELECT * FROM GameState")
    dbDisconnect(conn)
    result$img_num <- as.numeric(result$img_num)
    # for ( i in 1:15) {print(result[["img_num"]][[i]])}
    output$cell11 <- renderCell(1,1,result[["img_num"]][[1]])
    output$cell12 <- renderCell(1,2,result[["img_num"]][[2]])
    output$cell13 <- renderCell(1,3,result[["img_num"]][[3]])
    output$cell14 <- renderCell(1,4,result[["img_num"]][[4]])
    output$cell15 <- renderCell(1,5,result[["img_num"]][[5]])
    output$cell21 <- renderCell(2,1,result[["img_num"]][[6]])
    output$cell22 <- renderCell(2,2,result[["img_num"]][[7]])
    output$cell23 <- renderCell(2,3,result[["img_num"]][[8]])
    output$cell24 <- renderCell(2,4,result[["img_num"]][[9]])
    output$cell25 <- renderCell(2,5,result[["img_num"]][[10]])
    output$cell31 <- renderCell(3,1,result[["img_num"]][[11]])
    output$cell32 <- renderCell(3,2,result[["img_num"]][[12]])
    output$cell33 <- renderCell(3,3,result[["img_num"]][[13]])
    output$cell34 <- renderCell(3,4,result[["img_num"]][[14]])
    output$cell35 <- renderCell(3,5,result[["img_num"]][[15]])
    boardState <<- tibble(
      c1 = list(
        tibble(n = result[["n"]][[1]], e = result[["e"]][[1]], s = result[["s"]][[1]], w = result[["w"]][[1]]),
        tibble(n = result[["n"]][[6]], e = result[["e"]][[6]], s = result[["s"]][[6]], w = result[["w"]][[6]]),
        tibble(n = result[["n"]][[11]], e = result[["e"]][[11]], s = result[["s"]][[11]], w = result[["w"]][[11]])
      ),
      c2 = list(
        tibble(n = result[["n"]][[2]], e = result[["e"]][[2]], s = result[["s"]][[2]], w = result[["w"]][[2]]),
        tibble(n = result[["n"]][[7]], e = result[["e"]][[7]], s = result[["s"]][[7]], w = result[["w"]][[7]]),
        tibble(n = result[["n"]][[12]], e = result[["e"]][[12]], s = result[["s"]][[12]], w = result[["w"]][[12]])
      ),
      c3 = list(
        tibble(n = result[["n"]][[3]], e = result[["e"]][[3]], s = result[["s"]][[3]], w = result[["w"]][[3]]),
        tibble(n = result[["n"]][[8]], e = result[["e"]][[8]], s = result[["s"]][[8]], w = result[["w"]][[8]]),
        tibble(n = result[["n"]][[13]], e = result[["e"]][[13]], s = result[["s"]][[13]], w = result[["w"]][[13]])
      ),
      c4 = list(
        tibble(n = result[["n"]][[4]], e = result[["e"]][[4]], s = result[["s"]][[4]], w = result[["w"]][[4]]),
        tibble(n = result[["n"]][[9]], e = result[["e"]][[9]], s = result[["s"]][[9]], w = result[["w"]][[9]]),
        tibble(n = result[["n"]][[14]], e = result[["e"]][[14]], s = result[["s"]][[14]], w = result[["w"]][[14]])
      ),
      c5 = list(
        tibble(n = result[["n"]][[5]], e = result[["e"]][[5]], s = result[["s"]][[5]], w = result[["w"]][[5]]),
        tibble(n = result[["n"]][[10]], e = result[["e"]][[10]], s = result[["s"]][[10]], w = result[["w"]][[10]]),
        tibble(n = result[["n"]][[15]], e = result[["e"]][[15]], s = result[["s"]][[15]], w = result[["w"]][[15]])
      )
    )
  }
  
  retriveBoard <- function(){
    conn <- getAWSConnection()
    result <- dbGetQuery(conn, "SELECT * FROM GameState")
    dbDisconnect(conn)
  }
  
    processClickEvent <- function(gridrow,gridcol){
    # If it is not this player's turn or if the cell is occupied, then ignore the click
    if(vals$username == getPlayerTurn()){
      if (checkCell(gridrow, gridcol) == 1){
        current_row <<- gridrow
        current_col <<- gridcol
        qna <- getPhysicsQn()
        physics$qid <- qna$qid
        physics$aid <- qna$aid
        physics$qn <- qna$qn
        physics$q_opt <- getOptions(physics$qid)
        showModal(answerPhysicsQn(qn = physics$qn, q_opt = physics$q_opt[, 1, drop=FALSE], failed=FALSE))
        req(vals$playerid)
        if ((gamevals$pieces[gridrow,gridcol]==0)&&(vals$turnstate==as.integer(vals$playercolor))){
          #change the state of the game
          gamevals$pieces[gridrow,gridcol] <- as.integer(vals$playercolor) # as.integer necessary because vals$playercolor is actually a string
          gamevals$turncount <- gamevals$turncount+1
          newstate <- list(turncount=gamevals$turncount,pieces=gamevals$pieces)
          # check for end of game
          if (gamevals$turncount>MAXTURNS){
            vals$turnstate <- 0 # turnstate=0 signals end of the game
          } else{
            #switch turnstate between player colors
            if (vals$turnstate==1)vals$turnstate <- 2 else vals$turnstate <- 1
          }
          updateGame(vals$playerid,vals$gamevariantid,vals$turnstate,newstate)
        }
      }
    }
  }
  
  observeEvent(input$click11,{processClickEvent(1,1)})
  observeEvent(input$click12,{processClickEvent(1,2)})
  observeEvent(input$click13,{processClickEvent(1,3)})
  observeEvent(input$click14,{processClickEvent(1,4)})
  observeEvent(input$click15,{processClickEvent(1,5)})
  observeEvent(input$click21,{processClickEvent(2,1)})
  observeEvent(input$click22,{processClickEvent(2,2)})
  observeEvent(input$click23,{processClickEvent(2,3)})
  observeEvent(input$click24,{processClickEvent(2,4)})
  observeEvent(input$click25,{processClickEvent(2,5)})
  observeEvent(input$click31,{processClickEvent(3,1)})
  observeEvent(input$click32,{processClickEvent(3,2)})
  observeEvent(input$click33,{processClickEvent(3,3)})
  observeEvent(input$click34,{processClickEvent(3,4)})
  observeEvent(input$click35,{processClickEvent(3,5)})
  
  #Giving Each Cell IDs and their Click function
  observeEvent(input$entergame, {
    vals$gameStart <- startGame()
    updateTabsetPanel(session, "tabs", selected = "game")
    removeUI(selector = "#enterGameLobby")
    #Inserting Players into GamePlayers
    conn <- getAWSConnection()
    vals$players <- assignRoles(vals$players)
    dbWriteTable(conn, "GamePlayers", vals$players, overwrite=TRUE)
    dbDisconnect(conn)
    #Update the Game Turn number
    output$playerturn <- renderText(paste(sprintf("%s's turn", vals$players[vals$turn %% 4, 2])))
    output$assignedRole <- renderText(sprintf("You are an %s", getRole(vals$userid)))
    #Resetting Game Lobby (Cant Truncate here, other people needs to enter)
    # qry_truncate <- "TRUNCATE GameLobby"
    # exec_trunctate <- dbExecute(conn, qry_truncate)
    updateGameState()
  })
  
  

  
  # Reaction to Choice of Physics Question's Answer
  observeEvent(input$confirmOption, {
    # print(input$optionSelect)
    choice <- input$optionSelect
    ans_id <- physics$q_opt[choice, 2]
    # print(ans_id)
    # print(physics$aid)
    ans <- getAnswer(physics$aid)
    if(ans_id == physics$aid){showModal(physicsResult("correct", ans))}else{showModal(physicsResult("wrong", ans))}
  })
  
  # Reacting to correct Answer of Physics Question
  observeEvent(input$correctCont, {
    # Draw A card First
    drawCard(vals$userid, 1)
    #Get hand Cards
    exec_query <- getHandCards(userid = vals$userid)
    showModal(chooseCard(handCards = exec_query, failed=FALSE))
  })
  
  # Reacting to Wrong Answer of Physics Question
  observeEvent(input$wrongCont, {
    #Get Handcards
    exec_query <- getHandCards(userid = vals$userid)
    showModal(chooseCard(handCards = exec_query, failed=FALSE))
  })
  
  
  ### Choosing Card
  observeEvent(input$confirmCard, {
    # print(paste(current_row, current_col, input$cardSelect))
    num_id <- case_when(
      input$cardSelect == "Wire_1" ~ 4,
      input$cardSelect == "Wire_2" ~ 5,
      input$cardSelect == "Wire_3" ~ 6,
      input$cardSelect == "Wire_4" ~ 7,
      input$cardSelect == "Wire_5" ~ 8
    )
    cell_number <- case_when(
      current_row == 1 ~ case_when(
        current_col == 1 ~ 1,
        current_col == 2 ~ 2,
        current_col == 3 ~ 3,
        current_col == 4 ~ 4,
        current_col == 5 ~ 5
      ),
      current_row == 2 ~ case_when(
        current_col == 1 ~ 6,
        current_col == 2 ~ 7,
        current_col == 3 ~ 8,
        current_col == 4 ~ 9,
        current_col == 5 ~ 10
      ),
      current_row == 3 ~ case_when(
        current_col == 1 ~ 11,
        current_col == 2 ~ 12,
        current_col == 3 ~ 13,
        current_col == 4 ~ 14,
        current_col == 5 ~ 15
      )
    )
    if (num_id == 4){
      boardState[[current_col]][[current_row]] <- tibble(n = 0, e = 0, s = 1, w = 1)
    }
    else if (num_id == 5){
      boardState[[current_col]][[current_row]] <- tibble(n = 0, e = 1, s = 0, w = 1)
    }
    else if (num_id == 6){
      boardState[[current_col]][[current_row]] <- tibble(n = 1, e = 1, s = 0, w = 0)
    }
    else if (num_id == 7){
      boardState[[current_col]][[current_row]] <- tibble(n = 1, e = 0, s = 0, w = 1)
    }
    else if (num_id == 8){
      boardState[[current_col]][[current_row]] <- tibble(n = 0, e = 1, s = 1, w = 0)
    }
    conn <- getAWSConnection()
    query <- sprintf("UPDATE GameState SET img_num=%i, n=%i, s=%i, e=%i, w=%i WHERE cell_number=%i", num_id, boardState[[current_col]][[current_row]][['n']], boardState[[current_col]][[current_row]][['s']], boardState[[current_col]][[current_row]][['e']], boardState[[current_col]][[current_row]][['w']], cell_number)
    result <- dbExecute(conn,query)
    dbDisconnect(conn)
    #Remove Card From Hand
    removeCard(vals$userid, input$cardSelect)
    # Update Turn
    print(vals$turn)
    vals$turn <- nextPlayer(vals$turn)
    removeModal()
    updateGameState()
    
    # Check Win Condition
    if(checkWin()){
      endtable <- getEndTable(imposter = "LOSE")
      output$resultTable <- renderTable(endtable)
      showModal(gameEnd(failed=FALSE))
      newBoard(boardState)
      return()
    }
    # Check Loss Condition
    if(checkLoss()){
      endtable <- getEndTable(imposter = "WIN")
      output$resultTable <- renderTable(endtable)
      showModal(gameEnd(failed=FALSE))
      newBoard(boardState)
      return()
    }
  })
  

  observeEvent(input$backToHome, {
    updateTabsetPanel(session, "tabs", selected = "home")
    removeModal()
    # resetDatabase()
  })
  
  observe({
    invalidateLater(3000, session)
    isolate({updateGameState()})
  })
  

  observe({
    invalidateLater(1000, session)
    isolate({refresh("GameLobby")})
    isolate({refresh("HomePage")})
    isolate({getPlayerTurn()})
  })

  observe({
    invalidateLater(1000, session)
    isolate({checkGS()})
  })
  
}

