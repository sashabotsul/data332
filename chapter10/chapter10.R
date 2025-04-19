#other practice
abs_loop<- function(vec){
  for(i in 1:length(vec)){
    if(vec[i]<0){
      vec[i]<- -vec[i]
    }
  }
  vec
}
abs_sets<- function(vec){
  negs<- vec<0
  vec[negs]<- vec[negs]*-1
  vec
}
long<- rep(c(-1,1), 5000000)
system.time(abs_loop(long))
system.time(abs_sets(long))

#exercise1
system.time(abs(long))

#exercise2
change_symbols<- function(vec){
  for(i in 1:length(vec)){
    if(vec[i]=='DD'){
      vec[i]<- 'joker'
    }else if(vec[i]== 'C'){
      vec[i]<- 'ace'
    }else if(vec[i]=='7'){
      vec[i]<- 'king'
    }else if(vec[i]== 'B'){
      vec[i]<- 'queen'
    }else if(vec[i]=='BB'){
      vec[i]<- 'jack'
    }else if(vec[i]== 'BBB'){
      vec[i]<- 'ten'
    }else{
      vec[i]<- 'nine'
    }
  }
  vec
}
vec<- c('DD', 'C', '7', 'B', 'BB', 'BBB', '0')
change_symbols(vec)
many<- rep(vec, 1000000)
system.time(change_symbols(many))

  #vectorized
change_vec<- function(vec){
  vec[vec=='DD']<- 'joker'
  vec[vec=='C']<- 'ace'
  vec[vec='7']<- 'king'
  vec[vec=='B']<- 'queen'
  vec[vec=='BB']<- 'jack'
  vec[vec=='BBB']<- 'ten'
  vec[vec=='0']<- 'nine'
  
  vec
}
system.time(change_vec(many))
  #lookup table
change_vec2<- function(vec){
  tb<- c('DD'= 'joker','C'= 'ace', '7'= 'king', 'BBB'= 'queen',
         'BB'= 'jack', 'B'='ten', '0'= 'nine')
  unname(tb[vec])
}
system.time(change_vec2(many))

#other practice
get_many_symbols <- function(n) {
  wheel <- c('DD', '7', 'BBB', 'BB', 'B', 'C', '0')
  vec<- sample(wheel, size=3*n, replace=TRUE,
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
  matrix(vec, ncol=3)
} 
get_many_symbols(5)

play_many<- function(n){
  symb_mat<- get_many_symbols(n=n)
  data.frame(w1=symb_mat[,1], w2=symb_mat[,2],
             w3= symb_mat[,3], prize=score_many(symb_mat))
}
play_many(4)

#exercise3
score_many<- function(symbols) {
  #step1
  diamonds <- rowSums(symbols == 'DD')
  cherries <- rowSums(symbols == 'C')
  prize<- c(0,2,5)[cherries+diamonds+1]
  prize[!cherries]<- 0
  
  #step2
  same<- symbols[,1]==symbols[,2] &
    symbols[,2]==symbols[,3]
  payoffs<- c('DD'=100, '7'=80, 'BBB'=40, 'BB'=25, 'B'=10, 'C'=10, '0'=0)
  prize[same]<- payoffs[symbols[same,1]]
  
  #step3
  bars<- symbols=='B'| symbols=='BB'| symbols=='BBB'
  all_bars<- bars[,1] & bars[,2] & bars[,3]& !same
  prize[all_bars]<- 5
  
  #step4
  two_wilds<- diamonds ==2
  
  one<- two_wilds & symbols[,1] != symbols[,2] & symbols[,2]== symbols[,3]
  two<- two_wilds & symbols[,1] != symbols[,2] & symbols[,1]== symbols[,3]
  three<- two_wilds & symbols[,1] == symbols[,2] & symbols[,2]!= symbols[,3]
  
  prize[one]<- payoffs[symbols[one, 1]]
  prize[two]<- payoffs[symbols[two, 2]]
  prize[three]<- payoffs[symbols[three, 3]]
  
  one_wild<- diamonds==1
  wild_bars<- one_wild & (rowSums(bars)==2)
  prize[wild_bars]<- 5
  
  one<- one_wild & symbols[,1] == symbols[,2]
  two<- one_wild & symbols[,2] == symbols[,3]
  three<- one_wild & symbols[,3] == symbols[,1]
  prize[one]<- payoffs[symbols[one, 1]]
  prize[two]<- payoffs[symbols[two, 2]]
  prize[three]<- payoffs[symbols[three, 3]]
  
  #step5
  unname(prize*2^diamonds)
}
system.time(play_many(1000000))
