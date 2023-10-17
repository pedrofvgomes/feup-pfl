# Bounce

## Introduction and Definitions
Bounce is a two-player game played on a square board of any even size.<br>Initially, the board's corner squares are empty and the rest is filled with a checkerboard pattern of red and blue checkers.

![initial_board](img/img1.jpg)

- **Group** - Monocolored interconnected group of checkers

## Objective
The first player to have all their checkers in a single group wins. For example, Blue wins in the image below.

![objective](img/img2.jpg)

## How To Play
Starting with Red, players take turns moving one of their checkers to an empty square, only if that move counts as a _legal move_. If there's no legal moves available, the player is forced to remove a checker of their choice.
- **Legal move** - A move is considered legal if the checker becomes part of a larger group after it's moved.


## Examples
- In the image below, the Blue player moves the checker from square 1 to square 2. Since the groups' sizes are, respectively, 11 and 20, this counts as a _legal move_.

![legal_move](img/img3.jpg)

- In the image below, the Red player has their checkers divided in 3 groups, with sizes 14, 13 and 4. The only possible moves require moving a checker to the smaller group, so none of them is legal. This being said, the player should remove a checker of their choice in order to conclude their turn.

![remove_checker](img/img4.jpg)