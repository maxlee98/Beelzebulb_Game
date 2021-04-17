library(stringr)
library(shiny)
library(DBI)
library(shinydashboard)
library(rsconnect)
library(shinyjs)

header <- dashboardHeader(
  title = "Beelzebulb"
)

sidebar <- dashboardSidebar(
  useShinyjs(),
  
  sidebarMenu(
    id = "tabs",
    menuItem("Home Page", tabName = "home", icon = icon("home")),
    menuItem("Game", tabName = "game", icon = icon("dice")) %>% shinyjs::hidden(),
    menuItem("Instructions", tabName = "instruct", icon = icon("compass"))
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
            uiOutput("buttonGameLobby"),
            uiOutput("backToGame"),
            uiOutput("LobbyFull")
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
                img(src='Fix_Wiring.png',style="position:absolute;z-order:0",width="500px",height="300px"),
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
                p("ESD Fantasy Map and Game Pieces by Tan Yi Lin"),
              ),
              fluidRow(box(
                img(src='Wire_Designs-01.png',style="position:relative;x-order:0",width="50px",height="50px"),
                htmlOutput("wire1",inline=TRUE),
                img(src='Wire_Designs-02.png',style="position:relative;x-order:1",width="50px",height="50px"),
                htmlOutput("wire2",inline=TRUE),
                img(src='Wire_Designs-03.png',style="position:relative;x-order:2",width="50px",height="50px"),
                htmlOutput("wire3",inline=TRUE),
                img(src='Wire_Designs-04.png',style="position:relative;x-order:3",width="50px",height="50px"),
                htmlOutput("wire4",inline=TRUE),
                img(src='Wire_Designs-05.png',style="position:relative;x-order:4",width="50px",height="50px"),
                htmlOutput("wire5",inline=TRUE)
                ))
            )
    ),
    
    tabItem(tabName = "instruct",
            h2("Beelzebulb's Game Instructions"),
            p("Welcome to Beelzebulb, a 4 Player Game in which 3 Players work together as Engineers 
              in attempt to connect wires from the battery all the way to the light bulb! The fourth
              player would play the role of an Imposter, whereby he tries to stop the 3 Engineers from
              reaching the lightbulb."),
            p("Roles of the Players would not be disclosed. It would only appear on each individual's
              screen."),
            p("Your hand cards are displayed at the bottom of the screen"),
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
              be shown."),
            br(),
            br(),
            p("Attributions:"),
            p("Board Map (Among Us, Electrical Task) : https://among-us.fandom.com/wiki/Fix_Wiring"),
            p("Lightbulb : https://www.vecteezy.com/free-vector/light-bulb, Light Bulb Vectors by Vecteezy"),
            p("Battery: https://www.vecteezy.com/free-vector/battery, Battery Vectors by Vecteezy")
            
    )
  )
)

ui <- dashboardPage(
  skin = "yellow",
  header, 
  sidebar, 
  body
)
