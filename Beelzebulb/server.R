library(stringr)
library(shiny)
library(DBI)
library(jsonlite)
library(shinydashboard)
library(rsconnect)

#Global Variables 
turn <- 0


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

checkPhase <- function(){
  conn <- getAWSConnection()
  query <- "SELECT phase FROM Phase"
  phase <- dbGetQuery(conn, query)[1, 1]
  dbDisconnect(conn)
  return(phase)
}

updatePhase <- function(update){
  conn <- getAWSConnection()
  dbExecute(conn, sprintf("UPDATE Phase SET phase = %d", update))
  dbDisconnect(conn)
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
refresh <- function(page = NULL){
  if(page == "GameLobby"){
    conn <- getAWSConnection()
    lobby <- dbGetQuery(conn, "SELECT PlayerID, Username FROM GameLobby LIMIT 4")
    dbDisconnect(conn)
    return(lobby)
  }
}

gameLobby <- function(totalPlayers, failed = FALSE){
  if(totalPlayers > 3){
    modalDialog(
      title = "Players Currently In Waiting Room:",
      tableOutput("gameLobbyTable"),
      footer = tagList(
        actionButton("entergame", "Start Game"),
        modalButton("Cancel")
      )
    )
  }else{
    modalDialog(
      title = "Players Currently In Waiting Room:",
      tableOutput("gameLobbyTable"),
      footer = tagList(
        actionButton("refreshLobby", "Refresh"),
        modalButton("Cancel")
      )
    )
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
  opt <- sqlInterpolate(conn, "SELECT choice, ans_id FROM PhysicsAns WHERE qn_id = ?qnid ORDER BY RAND()", qnid = qid)
  q_opt <- dbGetQuery(conn, opt)
  dbDisconnect(conn)
  return(q_opt)
}

getAnswer <- function(aid){
  conn <- getAWSConnection()
  opt <- sqlInterpolate(conn, "SELECT choice FROM PhysicsAns WHERE ans_id = ?aid ORDER BY RAND()", aid = aid)
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
  q_temp <- "SELECT Card_ID, Card_Name FROM HandCards WHERE ?userid"
  query_HandCards <- sqlInterpolate(conn, q_temp, userid = userid)
  exec_query <- dbGetQuery(conn, query_HandCards)
  dbDisconnect(conn)
  return(exec_query)
}

# Choose Card Modal
chooseCard <- function(handCards, failed=FALSE){
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



#End Game Modal
gameEnd <- function(failed = FALSE){
  modalDialog(
    title = "End Game Results",
    p("testing Input"),
    tableOutput("resultTable"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("backToLobby", "OK")
    )
  )
}


############################################ SERVER ################################################
server <- function(input, output, session) {
  vals <- reactiveValues(password = NULL, userid=NULL,username=NULL, lobby=NULL)
  physics <- reactiveValues(qid = NULL, aid = NULL, qn = NULL, q_opt = NULL)
  
  output$playerturn <- renderText("The game has yet to start.")
  output$gamePhase <- renderText("Please wait for the game to begin...")
  
  output$status <- renderText({
    if (is.null(vals$username))
      "Not logged in yet."
    else
      paste("Welcome back", vals$username)
  })
  
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
  
  output$gameLobbyTable <- renderTable(vals$players)
  
  observeEvent(input$refreshLobby, {
    vals$players <- refresh("GameLobby")
    output$gameLobbyTable <- renderTable(vals$players)
  })
  
  ## Game Tab 
  
  ### Board Image
  renderCell <- function(){
    renderImage({list(src="www/blanksmall.png",style="position:relative;z-order:999") },
                deleteFile=FALSE)
  }
  
  #Giving Each Cell IDs and their Click function
  observeEvent(input$entergame, {
    updateTabsetPanel(session, "tabs", selected = "game")
    #Inserting Players into GamePlayers
    conn <- getAWSConnection()
    dbWriteTable(conn, "GamePlayers", vals$players, overwrite=TRUE)
    #Update the Game Turn number
    turn <- turn + 1
    dbExecute(conn, sprintf("UPDATE TurnNumber SET turn = %d", turn))
    output$playerturn <- renderText(paste(sprintf("%s's turn", vals$players[turn %% 4, 2])))
    #Resetting Game Lobby (Cant Truncate here, other people needs to enter)
    # qry_truncate <- "TRUNCATE GameLobby"
    # exec_trunctate <- dbExecute(conn, qry_truncate)
    
    #Inserting Initial Hand Cards from Database
    query_draw_cards <- dbGetQuery(conn, "SELECT row_names, Card_ID, Card_Name FROM CardDeck ORDER BY RAND() LIMIT 5")
    playerid <- vals$userid
    query_draw_cards["PlayerID"] <- playerid
    querytemplate_drawcards <- "INSERT INTO HandCards (PlayerID, Card_ID, Card_Name) VALUES"
    sql_qry <- paste0(querytemplate_drawcards, paste(sprintf("(%d, %d, '%s')",
                                                             as.numeric(query_draw_cards$PlayerID),
                                                             as.numeric(query_draw_cards$Card_ID),
                                                             query_draw_cards$Card_Name), collapse = ","))
    send_query <- dbExecute(conn, sql_qry)
    dbDisconnect(conn)
    removeModal()
    # Buttons for the players to Click
    lapply(1:4, function(i){
      lapply(1:4, function(j){
        cell_id <- sprintf("cell%d%d", i, j)
        output[[cell_id]] <- renderCell()
        click_id <- sprintf("click%d%d", i, j)
        observeEvent(input[[click_id]], {
          qna <- getPhysicsQn()
          physics$qid <- qna$qid
          physics$aid <- qna$aid
          physics$qn <- qna$qn
          physics$q_opt <- getOptions(physics$qid)
          showModal(answerPhysicsQn(qn = physics$qn, q_opt = physics$q_opt[, 1, drop=FALSE], failed=FALSE))
          # 
        })
      })
    })
    #Shifting to the first person's standby phase
    output$gamePhase <- renderText("Standby Phase")
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
    # renderCell(cell_id, card_select = output$cardSelect)
  })
  
  
  

  ## End Game Results
  observeEvent(input$endGameResult, showModal(gameEnd(failed=FALSE)))
  
  hasGameEnded <- TRUE
  if (hasGameEnded == TRUE){
  allplayers <- c("Player1", "Player2", "Player3", "Player4")
  role <- c("Normal Player", "Normal Player", "Normal Player", "Imposter")
  outcome <- c("Win", "Win", "Win", "Lose")
  df <- data.frame(Players = allplayers,
                   Roles = role,
                   Outcome = outcome)
  output$resultTable <- renderTable(df)
  }
}










