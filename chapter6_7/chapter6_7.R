#CHAPTER6

#other practice
deal(deck)
ls(globalenv())
head(globalenv()$deck, 3)

#exercise1
deal <- function() {
  deck[1,]
}
deal() #yes it returns an answer

#other practice
DECK <- deck
deck<- deck[-1,]
head(deck,3)

#exercise2
deal <- function(){
  card<-deck[1,]
  assign('deck', deck[-1,], envir = globalenv())
  card
}
deal()

#exercise3
shuffle <- function(cards){
  random <- sample(1:52, size=52)
  assign("deck", DECK[random,], envir = globalenv())
} 
shuffle()
deal()

#CHAPTER7

#other practice
get_symbols <- function() {
  wheel <- c('DD', '7', 'BBB', 'BB', 'B', 'C', '0')
  sample(wheel, size=3, replace=TRUE,
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
} 
get_symbols()

#quiza:  will return a 2
#quizb: will return a 2
#quizc: will return a 2

#exercise1
symbols<- c('B', 'BB', 'BBB')
symbols
symbols[1] ==symbols[2] &symbols[2] == symbols[3]

symbols<- c('7','7','7')
symbols
symbols[1] ==symbols[2] &symbols[2] == symbols[3]

#exercise2
symbols<- c('B', 'BB', 'BBB')
symbols
all(symbols %in% c('B', 'BB', 'BBB'))

#other practice
payouts <- c('DD'=100, '7'=80, 'BBB'=40, 'BB'=25, 'B'=10, 'C'=10, '0'=0)
payouts

symbols <- c('7', '7', '7')
symbols[1]
payouts[symbols[1]]

#exercise3
symbols <-c('C', 'D', 'C')
symbols == 'C'
#challenge
sum(symbols == 'C')
#exercise4: prize multiplied by 2 to the power of diamonds
diamonds <- sum(symbols == 'DD')
prize * 2^ diamonds

score<- function(symbols) {
#identify case
same <- symbols[1] ==symbols[2] &symbols[2] == symbols[3]
bars <- symbols %in% c('B', 'BB', 'BBB')

#get prize
if(same) {
  payouts <- c('DD'=100, '7'=80, 'BBB'=40, 'BB'=25, 'B'=10, 'C'=10, '0'=0)
  prize<- unname(payouts[symbols[1]])
} else if (all(bars)) {
  prize <- 5
} else {
  cherries <- sum(symbols == 'C')
  prize<- c(0,2,5)[cherries+1]
}
#adjust for diamonds
diamonds <- sum(symbols == 'DD')
prize * 2^ diamonds
}

play <- function() {
  symbols <- get_symbols()
  print(symbols)
  score(symbols)
} 
play()
