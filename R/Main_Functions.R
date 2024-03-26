#' Create a Deck of Playing Cards
#' 
#' @description
#' This function generates a deck of playing cards.
#'
#' @param num An integer specifying the number of decks to create (default is 1).
#' @return A data frame representing a deck of playing cards with columns for values, suits, lookup, and points.
#' 
#' @details
#' The deck consists of 52 cards, including numeric values from 2 to 10 and face cards (A, Q, K, J) for each of the four suits (Spades, Clubs, Diamonds, Hearts).
#' Each card has a unique identifier obtained by concatenating its value and suit.
#' Numeric values are assigned points equal to their face value, while face cards (Q, K, J) are assigned 10 points.
#' @export
#' @examples
#' create_deck()
#' create_deck(2)
#' 
create_deck <- function(num = 1) {
  deck = expand.grid(values = c(2:10, "A", "Q", "K", "J"), suits = c("S", "C", "D", "H"), stringsAsFactors = FALSE)
  
  res = do.call("rbind", replicate(num, deck, simplify = FALSE)) 
  #res = data.table::rbindlist(replicate(num, deck, simplify = FALSE))
  
  res$lookup = paste0(res$values, res$suits)
  res$point = vector('integer', dim(res)[1])
  res$point[res$values %in% c("Q", "K", "J")] = 10
  res$point[res$values %in% c(2:10)] = 2:10
  res$point[res$values == "A"] = 1
  
  return(res)
}

#' Deal cards to the dealer.
#' 
#' @description This function deals cards to the dealer from the deck.
#' 
#' @param deck The deck of cards.
#' @param player The player to whom cards are dealt.
#' 
#' @return Cards dealt to the dealer.
#' 
#' @examples
#' deck <- create_deck()
#' player_hand <- player(deck)
#' dealer_hand <- dealer1(deck, player_hand)
#' 
#' @export
dealer1 <- function(deck,player) {
  deck <- update_deck_user(deck, player)
  dealer <- deck[sample(nrow(deck), 2, replace = F), ]
  return(dealer)
}

#' Deal cards to the player.
#' 
#' @description This function deals cards to the player from the deck.
#' 
#' @param deck The deck of cards.
#' 
#' @return Cards dealt to the player.
#' 
#' @examples
#' deck <- create_deck()
#' player_hand <- player(deck)
#' 
#' @export
player <- function(deck) { #' dealer deck passed'
  player <- deck[sample(nrow(deck), 2, replace = F), ]
  return(player)
}

#' Hit the player with an additional card.
#' 
#' @description This function gives an additional card to the player from the deck.
#' 
#' @param player The current cards held by the player.
#' @param deck The deck of cards.
#' 
#' @return Updated player's hand after drawing a card.
#' 
#' @examples
#' deck <- create_deck()
#' player_hand <- player(deck)
#' player_hand <- hit_player(player_hand, deck)
#' 
#' @export
hit_player <- function(player, deck) {
  hit <- deck[sample(nrow(deck), 1, replace = F), ]
  return(rbind(player, hit))
}

#' Hit the dealer with an additional card.
#' 
#' @description This function gives an additional card to the dealer from the deck.
#' 
#' @param dealer The current cards held by the dealer.
#' @param deck The deck of cards.
#' 
#' @return Updated dealer's hand after drawing a card.
#' 
#' @examples
#' deck <- create_deck()
#' dealer_hand <- dealer1(deck, player(deck))
#' dealer_hand <- hit_dealer(dealer_hand, deck)
#' 
#' @export
hit_dealer <- function(dealer, deck) {
  hit <- deck[sample(nrow(deck), 1, replace = F), ]
  return(rbind(dealer, hit))
}

#' Update the deck after dealing cards to the dealer.
#' 
#' @description This function updates the deck by removing cards dealt to the dealer.
#' 
#' @param deck The original deck of cards.
#' @param dealer The cards dealt to the dealer.
#' 
#' @return The updated deck after removing cards dealt to the dealer.
#' 
#' @examples
#' deck <- create_deck()
#' dealer_hand <- dealer1(deck, player(deck))
#' updated_deck <- update_deck_dealer(deck, dealer_hand)
#' 
#' @export
update_deck_dealer <- function(deck, dealer) {
  deck[!(row.names(deck) %in% row.names(dealer)), ]
}

#' Update the deck after dealing cards to the user.
#' 
#' @description This function updates the deck by removing cards dealt to the player.
#' 
#' @param deck The original deck of cards.
#' @param player The cards dealt to the player.
#' 
#' @return The updated deck after removing cards dealt to the player.
#' 
#' @examples
#' deck <- create_deck()
#' player_hand <- player(deck)
#' updated_deck <- update_deck_user(deck, player_hand)
#' 
#' @export
update_deck_user <- function(deck, player) {
  deck[!(row.names(deck) %in% row.names(player)), ]
}

#' Update points based on the outcome of the game.
#' 
#' @description This function updates the player's points based on the game outcome.
#' 
#' @param points The current points of the player.
#' @param gamble The amount of points gambled.
#' @param todo The outcome of the game (win or loss).
#' 
#' @return Updated points after considering the outcome of the game.
#' 
#' @examples
#' points <- 100
#' points <- update_points(points, 10, "win")
#' 
#' @export
update_points <- function(points, gamble, todo) {
  if (todo == "win") {
    points <- points + gamble
  }
  if (todo == "lost") {
    points <- points - gamble
  }
  points
}

#' Check if the dealer should keep drawing cards.
#' 
#' @description This function determines if the dealer should draw more cards.
#' 
#' @param dealer The cards held by the dealer.
#' 
#' @return Logical value indicating whether the dealer should keep drawing cards.
#' 
#' @examples
#' deck <- create_deck()
#' dealer_hand <- dealer1(deck, player(deck))
#' keep_going_dealer(dealer_hand)
#' 
#' @export
keep_going_dealer <- function(dealer) {
  if ((sum(dealer$point) < 17) | (any(dealer$value %in% c("A")) & (sum(dealer$point) - 1 < 10))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Check if the dealer has won.
#' 
#' @description This function checks if the dealer has won the game.
#' 
#' @param dealer The cards held by the dealer.
#' 
#' @return Logical value indicating whether the dealer has won.
#' 
#' @examples
#' deck <- create_deck()
#' dealer_hand <- dealer1(deck, player(deck))
#' did_dealer_win(dealer_hand)
#' 
#' @export
did_dealer_win <- function(dealer) {
  if ((sum(dealer$point) == 21) | (any(dealer$value %in% c("A")) & (sum(dealer$point) - 1) == 10)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Check if the player should keep drawing cards.
#' 
#' @description This function determines if the player should draw more cards.
#' 
#' @param user The cards held by the player.
#' 
#' @return Logical value indicating whether the player should keep drawing cards.
#' 
#' @examples
#' deck <- create_deck()
#' player_hand <- player(deck)
#' keep_going_user(player_hand)
#' 
#' @export
keep_going_user <- function(user) {
  if ((sum(user$point) < 21) | (any(user$value %in% c("A")) & (sum(user$point) - 1 < 10))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Check if the player has won.
#' 
#' @description This function checks if the player has won the game.
#' 
#' @param user The cards held by the player.
#' 
#' @return Logical value indicating whether the player has won.
#' 
#' @examples
#' deck <- create_deck()
#' player_hand <- player(deck)
#' did_user_win(player_hand)
#' 
#' @export
did_user_win <- function(user) {
  if ((sum(user$point) == 21) | (any(user$value %in% c("A")) & (sum(user$point) - 1) == 10)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Compare the user's and dealer's hands to determine the winner.
#' 
#' @description This function compares the user's and dealer's hands to determine the winner.
#' 
#' @param user The cards held by the user.
#' @param dealer The cards held by the dealer.
#' 
#' @return Logical value indicating whether the user has won the game.
#' 
#' @examples
#' deck <- create_deck()
#' user_hand <- player(deck)
#' dealer_hand <- dealer1(deck, player(deck))
#' user_dealer_comparison(user_hand, dealer_hand)
#' 
#' @export
user_dealer_comparison <- function(user, dealer) {
  if (did_user_win(user)){
    return(TRUE)
  }
  else if (any(user$value %in% c("A")) & sum(user$point) - 1 <= 10 & sum(dealer$point) <= 21){
    return(sum(user$point) + 10  > sum(dealer$point))
  }
  else if (sum(user$point) > sum(dealer$point) & sum(user$point) <= 21) {
    return(TRUE)
  }
  else if (sum(user$point) <= 21 & sum(dealer$point) > 21){
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}