# BlackJackPack R Package

## Introduction

This package provides a set of functions to simulate a game of Blackjack in R. It includes functions to create a deck of playing cards, deal cards to the dealer and player, allow players to hit and update their points, determine if the player or dealer has won, and compare hands to determine the winner.

## Installation

You can install this package from GitHub using the `devtools` package:

```r
devtools::install_github("AlexanderHolmes0/BlackJackRPack")
```

## Usage

```r
library(BlackJackPack)

# Create a deck
deck <- create_deck()

# Deal cards to player and dealer
player_hand <- player(deck)
dealer_hand <- dealer1(deck, player_hand)

# Player hits
player_hand <- hit_player(player_hand, deck)

# Check if player should continue hitting
keep_going_user(player_hand)

# Check if dealer should continue hitting
keep_going_dealer(dealer_hand)

# Compare player and dealer hands
user_dealer_comparison(player_hand, dealer_hand)
```

## Have Fun!

Check out the BlackJack Shiny App to play a game of Blackjack using this package!

