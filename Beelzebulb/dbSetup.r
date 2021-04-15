library(DBI)
library(stringr)

username <- "student012"
getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "student012",
    host = "esddbinstance-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "student012",
    password = "7QhE7cZH")
  conn
}

prepGame <- function(){
  conn <- getAWSConnection()
  dbExecute(conn, sprintf("CREATE TABLE %s.CardDeck (
    row_names TEXT NULL,
    Card_ID DOUBLE NULL,
    Card_Name TEXT NULL
  )", username))
  
  dbExecute(conn, sprintf("CREATE TABLE %s.GameLobby (
    PlayerID INT NULL,
    Username TEXT NULL
  )", username))
  
  dbExecute(conn, sprintf("CREATE TABLE %s.GamePlayers (
    row_names TEXT NULL,
    PlayerID INT NULL,
    Username TEXT NULL,
    Roles TEXT NULL
  )", username))
  
  dbExecute(conn, sprintf("CREATE TABLE %s.GameState (
    cell_number varchar(100) NULL,
    img_num varchar(100) NULL,
    n varchar(100) NULL,
    s varchar(100) NULL,
    e varchar(100) NULL,
    w varchar(100) NULL
  )", username))
  
  dbExecute(conn, sprintf("CREATE TABLE %s.HandCards (
    PlayerID INT NULL,
    Card_ID INT NULL,
    Card_Name varchar(100) NULL
  )", username))
  
  dbExecute(conn, sprintf("CREATE TABLE %s.PhysicsAns (
    row_names INT NULL,
    qn_id INT NULL,
    ans_id INT NULL,
    choices varchar(1024) NULL
  )", username))
  
  dbExecute(conn, sprintf("CREATE TABLE %s.PhysicsQn (
    qn_id INT NULL,
    question varchar(300) NULL,
    ans_id INT NULL
  )", username))
  
  dbExecute(conn, sprintf("CREATE TABLE %s.StartGame (
    start INT NULL
  )", username))
  
  dbExecute(conn, sprintf("CREATE TABLE %s.TurnNumber (
    turn INT NULL,
    totalTurn INT NULL
  )", username))
  
  # Adding contents for CardDeck, ie. filling up the deck
  for (i in c(1:6)){
    querytemplate <- "INSERT INTO CardDeck (row_names, Card_ID, Card_Name) VALUES (?id1,?id2,?id3); " ## Edit the Database
    query<- sqlInterpolate(conn, querytemplate,id1=i,id2=1001,id3="Wire_1" )
    dbExecute(conn, query)
  }
  for (i in c(7:12)){
    querytemplate <- "INSERT INTO CardDeck (row_names, Card_ID, Card_Name) VALUES (?id1,?id2,?id3); " ## Edit the Database
    query<- sqlInterpolate(conn, querytemplate,id1=i,id2=1002,id3="Wire_2" )
    dbExecute(conn, query)
  }
  for (i in c(13:18)){
    querytemplate <- "INSERT INTO CardDeck (row_names, Card_ID, Card_Name) VALUES (?id1,?id2,?id3); " ## Edit the Database
    query<- sqlInterpolate(conn, querytemplate,id1=i,id2=1003,id3="Wire_3" )
    dbExecute(conn, query)
  }
  for (i in c(19:24)){
    querytemplate <- "INSERT INTO CardDeck (row_names, Card_ID, Card_Name) VALUES (?id1,?id2,?id3); " ## Edit the Database
    query<- sqlInterpolate(conn, querytemplate,id1=i,id2=1004,id3="Wire_4" )
    dbExecute(conn, query)
  }
  for (i in c(25:30)){
    querytemplate <- "INSERT INTO CardDeck (row_names, Card_ID, Card_Name) VALUES (?id1,?id2,?id3); " ## Edit the Database
    query<- sqlInterpolate(conn, querytemplate,id1=i,id2=1005,id3="Wire_5" )
    dbExecute(conn, query)
  }
  
  # Adding contents for the default board, in GameState
  for (i in c(1:15)){
    if (i == 6){
      dbExecute(conn, "INSERT INTO GameState (img_num, n, s, w, e, cell_number) VALUES (2, 1, 1, 0, 1, 6)")
    }
    else if (i == 10){
      dbExecute(conn, "INSERT INTO GameState (img_num, n, s, w, e, cell_number) VALUES (3, 1, 1, 1, 0, 10)")
    }
    else{
      dbExecute(conn, sprintf("INSERT INTO GameState (img_num, n, s, w, e, cell_number) VALUES (1, 0, 0, 0, 0, %d)", i))
    }
  }
  
  # Adding StartGame to 0
  dbExecute(conn, "INSERT INTO StartGame (start) VALUES (0)")
  
  # Initializing TurnNumber
  dbExecute(conn, "INSERT INTO TurnNumber (turn, totalTurn) VALUES (0, 0)")
  
  
  dbDisconnect(conn)
}

prepGame()