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
            h2("This is the Game Lobby to Game Tab"),
            verbatimTextOutput("playerturn"),
            # verbatimTextOutput("gamePhase"),
            verbatimTextOutput("assignedRole"),
            fluidRow(
              box(
                title = "A Very Simple Board Game",width=12,
                htmlOutput("playercolorchoice"),
                uiOutput("moreControls"),
                # the trick here is to make the gameboard image 'position:absolute;z-order:0'; 
                # Then, to superimpose images, style them to be 'position:relative;z-order:999'
                img(src='FantasyMap.jpg',style="position:absolute;z-order:0",width="500px",height="300px"),
                imageOutput("cell11",height="100px",width="100px",click="click11",inline=TRUE), # height and width are for the containing div, not the image itself
                imageOutput("cell12",height="100px",width="100px",click="click12",inline=TRUE),  # height and width are for the containing div, not the image itself
                imageOutput("cell13",height="100px",width="100px",click="click13",inline=TRUE), # height and width are for the containing div, not the image itself
                imageOutput("cell14",height="100px",width="100px",click="click14",inline=TRUE),  # height and width are for the containing div, not the image itself
                imageOutput("cell15",height="100px",width="100px",click="click15",inline=TRUE),
                tags$br(),
                imageOutput("cell21",height="100px",width="100px",click="click21",inline=TRUE), # height and width are for the containing div, not the image itself
                imageOutput("cell22",height="100px",width="100px",click="click22",inline=TRUE),  # height and width are for the containing div, not the image itself
                imageOutput("cell23",height="100px",width="100px",click="click23",inline=TRUE), # height and width are for the containing div, not the image itself
                imageOutput("cell24",height="100px",width="100px",click="click24",inline=TRUE),  # height and width are for the containing div, not the image itself
                imageOutput("cell25",height="100px",width="100px",click="click25",inline=TRUE),
                tags$br(),
                imageOutput("cell31",height="100px",width="100px",click="click31",inline=TRUE), # height and width are for the containing div, not the image itself
                imageOutput("cell32",height="100px",width="100px",click="click32",inline=TRUE),  # height and width are for the containing div, not the image itself
                imageOutput("cell33",height="100px",width="100px",click="click33",inline=TRUE), # height and width are for the containing div, not the image itself
                imageOutput("cell34",height="100px",width="100px",click="click34",inline=TRUE),  # height and width are for the containing div, not the image itself
                imageOutput("cell35",height="100px",width="100px",click="click35",inline=TRUE),
                tags$br(),
                p("ESD Fantasy Map and Game Pieces by Tan Yi Lin")
              )
            ),
            actionButton("refreshGame", "Refresh")
    ),
    
    tabItem(tabName = "instruct",
            h2("Beelzebulb's Game Instructions"),
            p("Welcome to Beelzebulb! A 4 Player Game in which 3 Players work together as Engineers 
              in attempt to connect wires from the battery all the way to the light bulb! The fourth
              player would play the role of an Imposter, whereby he tries to stop the 3 Engineers from
              reaching the lightbulb."),
            p("Roles of the Players would not be disclosed. It would only appear on each individual's
              screen."),
            h3("The Game is comprised of 3 Phases : Standby Phase, Action Phase, End Phase."),
            h4("Standby Phase"),
            p("In the Standby Phase, players would be given a multiple choice physics question.
              Answering the question right allows the player to draw a card. Failure to answer correctly
              results in the player not being able to draw a card."),
            h4("Action Phase"),
            p("In this phase, players would have to choose a card from their hand and place it onto the board."),
            h4("End Phase"),
            p("During this phase, the board status would be checked. Upon connection to the bulb or
              the deck running out of cards, the game will end and the conclusion of the game would
              be shown.")
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
