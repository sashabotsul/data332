rm(list=ls())
die<- c(1, 2, 3, 4, 5, 6)

#other practice
head(deck)
deck[1,1]
deck[1, c(1,2,3)]
new<- deck[1, c(1,2,3)]
deck[1,]

#exercise1
deal <- function(cards) {
  cards[1,]
}
deal(deck)

#other practice
deck2 <- deck[1:52,]
head(deck2)

deck3 <- deck[c(2,1,3:52),]
head(deck3)

#exercise2
shuffle <- function(cards) {
  random <- sample(1:52, size=52)
  cards[random, ]
}
deal(deck)
deck2 <- shuffle(deck)
deal(deck2)

#other practice
deck$value
mean(deck$value)

lst <- list(numbers = c(1,2), logical=TRUE, strings = c('a', 'b', 'c'))
lst
lst$numbers
