# Beelzebulb_Game
Rshiny Game for Beelzeulb

Workload Split : 
Function	Description	Person I/C	Deadline (end of)	Done?
Login	Able for user to autheticate to server	Max	done	Yes
Registration	Able to send details for new account creation to server and record it in database	Max	done	Yes
Lobby	Create a Page that checks whether a new player has joined and all the players that currently waiting	Tianshu	Week 10	Yes
Lobby State Change	Functionality when player enters/leaves the lobby and to allow first player to start the game	Tianshu	Week 10	Yes
Turn Order RNG	Assigns a random number to each Player and orders the players according to their given number	Tianshu	Week 10	Yes
Cards in the game	Create the cards that are to be used in the game	Tianshu	Week 10	
Cards for each player	Create a function that assigns the number of cards to each player	YaoDe/Tianshu	Week 11	
Card Drawn RNG	Create a function that randomly assigns cards from the deck to the player's hands	YaoDe/Tianshu	Week 11	
Deck Order RNG	Shuffle a deck of 20 cards	YaoDe	Week 11	
Physics Questions	Create a database of physics questions to choose from (relating to electrical circuits)	YaoDe	Week 10	Yes
Physics Questions RNG	Function to randomly pick a physics question from the database	YaoDe	Week 10	
Board state	Check what is the current status of the board (battery at [2,1], bulb at [2, 5]) 	Yong Sheng	Week 10	Yes
Board state display	Display the Board's image along with cells that can be clicked on by the Players (UI)	Yong Sheng	Week 10	Yes
Updating board state	Sending information for every card that is played by the Players to the server	Yong Sheng	Week 10	Yes
Updating board state display	Update the display of the board with every card played (UI)	Yong Sheng	Week 10	Yes
Drawing of card	Randomiser for a drawing of card from the deck, deck -1 card.	Max	Week 10	Yes
Placing of card	Clicking on cell causes a popup, allowing to go back, and displaying all possible cards that can be played on that cell	Max	Week 10	Yes
Answering of physics question	A Popup showing the physics question randomly given and its MCQ answers. Sends answer to server	Max	Week 10	Yes
Move to next player	Updates that a player's turn have ended, and on to the next one	Max/Yong Sheng	Week 11	Yes
Wire logic	A logic for the backend to check what kind of cards can be placed in accordance to that wire card	Yong Sheng	Week 11	Yes
Victory condition logic	Check if there is a path to reach from the starting point to the light bulb	Yong Sheng	Week 11	Yes
Defeat condition logic	Check if there are still any cards left in the deck, if not, end the game (If cards == 0, end game)	Max	By Week 11	Yes
Game end screen	Display an ending screen with the result of the game	Max	By Week 11	Yes
Return to lobby	Allows players to revert back to the lobby from the end game screen.	Max	By Week 11	Yes
