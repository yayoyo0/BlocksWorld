# BlocksWorld
BlocksWorld using Scheme

## All credits of the implementation to Luis Eduardo Rodríguez Ramírez

Implemented on DrRacket

**Usage**
- Run the command "putOn(Block Block)".

**Inventory of functions**
- (start WIDTH HEIGHT) => Start the canvas in a new window
- (robot-arm) => Draw robot arm in canvas
- (floor) => Draw floor in canvas
- (drawCube Block) => Draw cube in canvas
- (clearCube Block) => Erase cube from canvas
- (parseItem Item) => Parse the name of the block to print it in the canvas
- (parseInput UserInput) => Parse user input to defined columns
- (checkRow Column) => Checks the last row available
- (findSpace) => Finds an empty column
- (onTop Block) => Finds if there is something above the blocks trying to be moved
- (onTopList Block) => Parse the onTop blocks into a list to be processed
- (moveBlock Block Column Row) => Move block in canvas
- (moveHelper List) => Helper function to move blocks
- (putOn Block Block) => Move block to another position with the input of the user
- (blockInit) => Process the user input to specify the position of the blocks
- (init) => Initialization function
