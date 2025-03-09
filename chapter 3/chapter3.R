rm(list=ls())
is.vector(die)
int<- 1L
text<- "ace"
die<- c(1, 2, 3, 4, 5, 6)
typeof(die)
text<- c("Hello", "World")
print(text)
#exercise1: 1 is a number and "1" and "one" are strings
#exercise2: character vector

#other practice
hand <- c("ace", "king", "queen", "jack", "ten")
print(hand)

names(die)<- c("one", "two", "three", "four", "five", "six")
print(names(die))

names(die)<- NULL

dim(die)<- c(2,3)
print(die)

m <- matrix(die, nrow=2)
print(m)

#exercise3
hand1 <- c("ace", "spades", "king", "spades", "queen", "spades", "jack", "spades", "ten", "spades")

m<- matrix(hand1, nrow=5, byrow=TRUE)
print(m)

#other practice
now <- Sys.time()
print(now)

unclass(now)

gender <- factor(c('male', 'female', 'female', 'male'))
typeof(gender)
attributes(gender)

#exercise4 - they are all strings
card <- c('ace', 'heart', 1)
card

#exercise5
card<- list('ace', 'heart', 1)
card

#other practice
df <- data.frame(face= c('ace', 'two', 'six'),
      suit = c('clubs', 'clubs', 'clubs'), value = c(1, 2, 3))
df

write.csv(deck, file = 'cards.csv', row.names = FALSE)
