* Replace cards with state from server -> load current game
* Send state from frontend to server
* ?Websocket in order to notify clients
* ?Logged in players
* Extend servant server to hold the state variable (Nat)


# User management
* When a new user requests a game generate a new user for the game and send an id token
* Hold a map on the server which associates each user with the game score
* Increase the game score when a user finds a set
