library(stringr)
library(shiny)
library(DBI)
library(jsonlite)
library(shinydashboard)
library(rsconnect)

# Added Functions

# AWS Connection #

getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "student055",
    host = "esddbinstance-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "student055",
    password = "gVU4KpWT")
  conn
}

# REGISTER #

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

# Choose Card Modal
chooseCard <- function(handCards, failed=FALSE){
  cards <- nrow(handCards)
  modalDialog(
    title = "Choose Your Card",
    p("Which card would you like to play?"),
    lapply(1:cards, function(i){
      img(src='RedStoneSmall.png')
      switch(sprintf("%d", handCards[i, 1]),
             '1001' = {img(src='RedStoneSmall.png')},
             '1002' = {img(src='RedStoneSmall.png')},
             '1003' = {img(src='BlueStoneSmall.png')},
             '1004' = {img(src='BlueStoneSmall.png')},
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
  vals <- reactiveValues(password = NULL,userid=NULL,username=NULL, lobby=NULL)
  
  
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
    vals$lobby<- dbGetQuery(conn, "SELECT PlayerID, Username FROM GameLobby LIMIT 4")
    dbDisconnect(conn)
    totalPlayers <- nrow(vals$lobby)
    showModal(gameLobby(totalPlayers = totalPlayers, failed = FALSE))
    # 
  })
  
  output$gameLobbyTable <- renderTable(vals$lobby)
  
  observeEvent(input$refreshLobby, {
    conn <- getAWSConnection()
    vals$lobby<- dbGetQuery(conn, "SELECT PlayerID, Username FROM GameLobby LIMIT 4")
    dbDisconnect(conn)
    output$gameLobbyTable <- renderTable(vals$lobby)
  })
  
  observeEvent(input$entergame, {
    updateTabsetPanel(session, "tabs", selected = "game")
    removeModal()
  })
  
  ## Game Tab 
  
  ### Board Image
  renderCell <- function(){
    renderImage({list(src="www/blanksmall.png",style="position:relative;z-order:999") },
                deleteFile=FALSE)
  }
  
  #Giving Each Cell IDs and their Click function
  lapply(1:4, function(i){
    lapply(1:4, function(j){
      id <- sprintf("cell%d%d", i, j)
      output[[id]] <- renderCell()
      click_id <- sprintf("click%d%d", i, j)
      observeEvent(input[[click_id]], {
        conn <- getAWSConnection()
        q_temp <- "SELECT Card_ID, Card_Name FROM HandCards WHERE ?userid"
        query_HandCards <- sqlInterpolate(conn, q_temp, userid = vals$userid)
        exec_query <- dbGetQuery(conn, query_HandCards)
        dbDisconnect(conn)
        print(exec_query)
        showModal(chooseCard(handCards = exec_query, failed=FALSE))
        })
    })
  })
  
  ### Choosing Card
  observeEvent(input$confirmCard, {
    return(NULL)
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










