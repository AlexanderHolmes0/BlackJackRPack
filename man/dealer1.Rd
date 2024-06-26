% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Main_Functions.R
\name{dealer1}
\alias{dealer1}
\title{Deal cards to the dealer.}
\usage{
dealer1(deck, player)
}
\arguments{
\item{deck}{The deck of cards.}

\item{player}{The player to whom cards are dealt.}
}
\value{
Cards dealt to the dealer.
}
\description{
This function deals cards to the dealer from the deck.
}
\examples{
deck <- create_deck()
player_hand <- player(deck)
dealer_hand <- dealer1(deck, player_hand)

}
