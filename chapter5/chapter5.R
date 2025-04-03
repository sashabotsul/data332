rm(list=ls())
die<- c(1, 2, 3, 4, 5, 6)
head(deck)

deck2 <- deck

#other practice
vec <- c(0,0,0,0,0,0)
vec[1] <- 1000
vec

deck2$new <- 1:52
head(deck2)

deck2$new <- NULL
head(deck2)

deck2[c(13, 26, 39, 52),]

deck2$value[c(13, 26, 39, 52)] <- 14
shuffle <- function(cards) {
  random <- sample(1:52, size=52)
  cards[random, ]
}
deck3 <- shuffle(deck)
head(deck3)

#exercise1
deck2$face
deck2$face == 'ace'
 
sum(deck2$face == 'ace')


#other practice
deck4 <- deck
deck4$value <- 0

#exercise2
deck4$suit == 'hearts'
deck4$value[deck4$suit == 'hearts']
deck4$value[deck4$suit == 'hearts'] <- 1
deck4$value[deck4$suit == 'hearts']

#other practice - boolean
queenOfspades <- deck4$face == 'queen' & deck4$suit == 'spades'
deck4[queenOfspades,]
deck4$value[queenOfspades] <- 13
deck4[queenOfspades,]

#exercise3
w <- c(-1,0,1)
x <- c(5, 15)
y <- 'February'
z <- c('Monday', 'Tuesday', 'Friday')

w > 0
x >10 & x<20
y == 'February'
all(z %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))

#other practice
deck5 <- deck
facecard <- deck5$face %in% c('king', 'queen', 'jack')
deck5[facecard,]
deck5$value[facecard]<- 10
deck5$value[deck5$face == 'ace'] <- NA
head(deck5, 13)
