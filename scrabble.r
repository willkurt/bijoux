
#scrabble word score
sws <- function(w){
  sum(sapply(strsplit(w,"")[[1]],sls))
  
}

#scrabble letter score
sls <- function(l){
  if(l %in% c("e","a","i","o","n","r","t","l","s","u")){return(1)} 
  else if (l %in% c("d","g")){ return(2)}
  else if (l %in% c("b","c","m","p")){ return(3)}
  else if (l %in% c("f","h","v","w","y")){ return(4)}
  else if (l %in% c("k")){ return(5)}
  else if (l %in% c("j","x")){ return(8)}
  else if (l %in% c("q","z")){ return(10)}
  else{return(0)}
}

#based on this calculator http://solfire.com/scrabble/index.php
sws.test <- function(){
  if(sws("the") == 6 &
      sws("quick") == 20 &
      sws("brown") == 10 &
      sws("fox") == 13 &
      sws("jumps") == 16 &
      sws("over") == 7 &
      sws("lazy") == 16 &
      sws("dog") == 5){
        return("pass!")
      } else {
        return("fail!")
      }
}