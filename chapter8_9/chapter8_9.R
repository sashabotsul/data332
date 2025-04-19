#CHAPTER 8

#other practice
play()
one_play <- play()
attributes(deck)

attr(one_play, 'symbols') <- c('B', '0', 'B')
attributes(one_play)

#exercise1
play <- function() {
  symbols <- get_symbols()
  prize <- score(symbols)
  attr(prize, 'symbols') <- symbols
  prize
}
play()

#other practice
slot_display <- function(prize) {
  symbols <- attr(prize, 'symbols')
  symbols <- paste(symbols, collapse='')
  string <- paste(symbols, prize, sep='\n$')
  cat(string)
}
slot_display(one_play)

#exercise2
print.slots <- function(x, ...){
  slot_display(x)
}
print.slots(one_play)

#exercise3
play <- function(){
  symbols <- get_symbols()
  structure(score(symbols), symbols=symbols, class = 'slots')
}
class(play())
play()

#CHAPTER 9

#other practice
rolls<- expand.grid(die, die)
rolls

#exercise1
wheel<- c('DD', '7', 'BBB', 'BB', 'B', 'C', '0')
combos<- expand.grid(wheel, wheel, wheel, stringsAsFactors = FALSE)
combos

#past code
get_symbols <- function() {
  wheel <- c('DD', '7', 'BBB', 'BB', 'B', 'C', '0')
  sample(wheel, size=3, replace=TRUE,
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
} 
#exercise2
prob <- c('DD'= 0.03, '7'= 0.03, 'BBB'= 0.06, 'BB'= 0.1, 'B'=0.25, 'C'= 0.01, '0'= 0.52)

#exercise3
combos$prob1 <- prob[combos$Var1]
combos$prob2 <- prob[combos$Var2]
combos$prob3 <- prob[combos$Var3]

head(combos, 3)

#exercise4
combos$prob <- combos$prob1*combos$prob2*combos$prob3
head(combos, 3)
sum(combos$prob)

#other practice
for (value in c('My', 'first', 'for', 'loop')){
  print('one run')
}
for (value in c('My', 'second', 'for', 'loop')){
  print(value)
}
chars<- vector(length=4)
words<- c('My', 'fourth', 'for', 'loop')
for(i in 1:4){
  chars[i]<- words[i]
}
chars

combos$prize<- NA
head(combos,3)

#exercise5
for(i in 1:nrow(combos)){
  symbols<- c(combos[i,1], combos[i, 2], combos[i, 3])
  combos$prize[i]<- score(symbols)
}
head(combos,3)

#other practice
sum(combos$prize*combos$prob)

#challenge
score<- function(symbols) {
  diamonds <- sum(symbols == 'DD')
  cherries <- sum(symbols == 'C')
  
  slots<-(symbols[symbols!= 'DD'])
  same <- length(unique(slots)) ==1
  bars <- slots %in% c('B', 'BB', 'BBB')
  
  if(diamonds == 3) {
    prize<- 100
  } else if(same){
    payouts <- c('7'=80, 'BBB'=40, 'BB'=25, 'B'=10, 'C'=10, '0'=0)
    prize<- unname(payouts[slots[1]])
  }else if(all(bars)) {
    prize <- 5
  } else if(cherries>0) {
    prize<- c(0,2,5)[cherries+diamonds+1]
  }else{
    prize=0
  }
  prize*2^diamonds
}

#exercise6
for(i in 1:nrow(combos)){
  symbols<- c(combos[i,1], combos[i, 2], combos[i, 3])
  combos$prize[i]<- score(symbols)
}
head(combos,3)

sum(combos$prize*combos$prob)

#other practice
plays_till_broke <- function(start_with){
  cash<- start_with
  n<-0
  while(cash>0) {
    cash<- cash - 1 +play()
    n<- n+1
  }
  n
}
plays_till_broke(100)

plays_till_broke <- function(start_with){
  cash<- start_with
  n<-0
  repeat{
    cash<- cash -1 +play()
    n<- n+1
    if(cash<= 0){
      break
    }
  }
  n
}
plays_till_broke(100)
