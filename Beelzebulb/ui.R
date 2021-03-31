library(stringr)
library(shiny)
library(DBI)
library(jsonlite)
library(shinydashboard)
library(rsconnect)


header <- dashboardHeader(
  title = "Beelzebulb",
  dropdownMenu(type = "messages",
               messageItem(
                 from = "Sales Dept",
                 message = "Sales are steady this month."
               ),
               messageItem(
                 from = "New User",
                 message = "How do I register?",
                 icon = icon("question"),
                 time = "13:45"
               ),
               messageItem(
                 from = "Support",
                 message = "The new server is ready.",
                 icon = icon("life-ring"),
                 time = "2014-12-01"
               )
            ),
  dropdownMenu(type = "tasks", badgeStatus = "success",
               taskItem(value = 90, color = "green",
                        "Documentation"
               ),
               taskItem(value = 17, color = "aqua",
                        "Project X"
               ),
               taskItem(value = 75, color = "yellow",
                        "Server deployment"
               ),
               taskItem(value = 80, color = "red",
                        "Overall project"
               )
            )
  )

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Home Page", tabName = "home", icon = icon("home")),
    menuItem("Game", tabName = "game", icon = icon("dice")),
    menuItem("Instructions", tabName = "instruct", icon = icon("compass")),
    menuItem("Testing Screen (Test)", tabName = "test", icon = icon("question"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "home",
            h1("Welcome to Beelzebulb!"),
            h3("The R-Shiny game where we teach you about Electrical Circuits!"),
            verbatimTextOutput("status"),
            actionButton("register", "Register"),
            actionButton("login", "Login"),
            HTML("<p></p>"),
            uiOutput("buttonGameLobby")
            ),
    
    tabItem(tabName = "game",
            h1("This is the Game Lobby to Game Tab"),
            verbatimTextOutput("playerturn"),
            verbatimTextOutput("gamePhase"),
            fluidRow(
              # the trick here is to make the gameboard image 'position:absolute;z-order:0'; 
              # Then, to superimpose images, style them to be 'position:relative;z-order:999'
              img(src='FantasyMap.jpg',style="position:absolute;z-order:0",width="100%",height="300px"),
              lapply(1:3, function(i){
                lapply(1:4, function(j){
                  column(3,
                  imageOutput(paste0("cell", i, j),height="100px",width="100px",click=paste0("click", i, j),inline=TRUE)
                  )
                })
              })
            )
          ),

    tabItem(tabName = "instruct",
            h2("Widgets tab content"),
            p("Instruction goes here")
          ),
    tabItem(tabName = "test",
            h2("Testing Goes Here"),
            h3("End Game Screen (Result) with a return to lobby button"),
            actionButton("endGameResult", "End Game Results"),
            h4("Drawing of cards for each players"),
            actionButton("drawCards", "Draw Cards"),
            h3("Placing of Cards on the board"),
            h4("Edit the game tab with the board and the cells available to be clicked"),
            h3("Answering of Physics Question"),
            h4("Popup with a random physics question and the multiple choice answers with submit button"),
            h3("Defeat Condition Logic"),
            h4("Running out of cards")
          )
        )
)

ui <- dashboardPage(
  skin = "yellow",
  header, 
  sidebar, 
  body
  )
