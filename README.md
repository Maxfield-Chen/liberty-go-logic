# Liberty Go Logic (LGL)

A Haskell implementation of the board game Baduk. Uses a simple ExceptT + StateT monad transformer stack and relies on the Ghosts of Departed Proofs technique to prove that moves are valid before being placed. 

This library only deals with the game logic and does not provide any sort of interface aside from the raw functions.

This library is designed to be used in combination with [Liberty Go Server](https://github.com/Maxfield-Chen/liberty-go-server) which handles the client / server / docs generation. 

Made with love by Maxfield Chen (dryc)
