library("roxygen2")
library("devtools")
library("dplyr")
#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
} 



#' @title select a door in the Monty Hall Problem game.
#' 
#' @description `select_door()` generates three doors a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'   
#' @details Before the contestant can open a door the doors must be generated 
#'   from a sample of three options in a vector called doors that variable.
#'   
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns one selection from the vector doors valued 1 through 3.
#' 
#' @examples select_door()
#' 
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   The host cannot select the car door and must start with a goat door.
#'
#' @description
#'   `open_goat_door(game, a.pick)` automatically assigns a goat door to the host 
#'   leaves the remaining door options to the contestant
#'
#' @details
#'   Hosts selection can only be a goat door not a car door. the other 2 doors 
#'   are left for the contestant. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns selection from the contestant.
#'
#' @examples
#'   open_goat_door(game, a.pick)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  # if contestant selected car,
  # randomly select one of two goats 
  if( game[ a.pick ] == "car" )
  { 
    goat.doors <- doors[ game != "car" ] 
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  { 
    opened.door <- doors[ game != "car" & doors != a.pick ] 
  }
  return( opened.door ) # number between 1 and 3
}



#' @title
#'   The contestant has a chance to change their door selection
#'
#' @description
#'   `change_door(stay=T, opened.door, a.pick)` creates an if then statement that passes the information  
#'   from the previous selection to a function. 
#' @details
#'   If the contestant stays its their final pick, if they want to switch 
#'   another door is selected.
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns the final selection.
#'
#' @examples
#'   change_door(stay=T, opened.door, a.pick)
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3) 
  
  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ] 
  }
  
  return( final.pick )  # number between 1 and 3
}



#' @title
#'   Determines if the contestant is a winner
#'
#' @description
#'   `determine_winner(final.pick,game)` reads the final pick from the game.  
#'   from the previous selection to a function. 
#' @details
#'   If the contestants door choice has a car paired with it then they win if not 
#'   they lose
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns Win or Lose
#'
#' @examples
#'   determine_winner(final.pick,game)
#'
#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}



#' @title
#'   creates a strategy to categorize the amount of times switching results in a win or lose
#'   and staying results in a win or lose. 
#' @description
#'   `play_game()`compiles the game data into a data frame  
#'    
#' @details
#'   The data is randomized from the selections of doors and the option to stay
#'   and switch from the stay or switch the game determines if the contestant wins or loses.
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a table of strategy outcomes.
#'
#' @examples
#'   play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )
  
  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )
  
  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}



#' @title
#'   Determines how many times a simulated game can win or lose with a stay or switch strategy
#'
#' @description
#'   `play_n_games(n = 100)`compiles the game data into a data frame and gives probability of chances
#'    
#' @details
#'   The data is randomized from the selections of doors and the option to stay
#'   and switch from the stay or switch the game determines if the contestant wins or loses.
#'   that data is compiled after a loop for 100 then that data is put into a table
#'   the probability of the results are then displayed
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a table of strategy outcomes and their probability.
#'
#' @examples
#'   play_n_games(n = 100)
#'
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1
  
  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )
  
  table( results.df ) %>% 
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>% 
    print()
  
  return( results.df )
  
}

