
letter.dists <- table(c(rep('a',9),
                        rep('b',2),
                        rep('c',2),
                        rep('d',4),
                        rep('e',12),
                        rep('f',2),
                        rep('g',3),
                        rep('h',2),
                        rep('i',9),
                        rep('j',1),
                        rep('k',1),
                        rep('l',4),
                        rep('m',2),
                        rep('n',6),
                        rep('o',8),
                        rep('p',2),
                        rep('q',1),
                        rep('r',6),
                        rep('s',4),
                        rep('t',6),
                        rep('u',4),
                        rep('v',2),
                        rep('w',2),
                        rep('x',1),
                        rep('y',2),
                        rep('z',1)
                        ))

#scrabble word score
sws <- function(w){
  wv <- strsplit(w,"")[[1]]
  init.score <- sum(sapply(wv,sls))
  init.score - impossible.points(wv)
}


impossible.points <- function(wv){
  wt <- table(wv)
  sum(sapply(names(wt),function(l){
    ifelse(wt[l] > letter.dists[l],(wt[l]-letter.dists[l])*sls(l),0)
  }))
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


sws.test <- function(){
  if(sws("the") == 6 &
      sws("quick") == 20 &
      sws("brown") == 10 &
      sws("fox") == 13 &
      sws("jumps") == 16 &
      sws("over") == 7 &
      sws("lazy") == 16 &
      sws("dog") == 5 &
      #these are cases where the word necessitates the use of wildcards
      #only 4 allowed
       sws("ddddddun") == 10 &   #max of 4 Ds
       sws("huuuuuuuuuurr") == 10 & #max of 4 Us
       sws("ssssssssss") == 4 &   #max of 4 Cs
       sws("llllllollllll") == 5 & #max of 4 Ls
      
       #only 3 allowed 
       sws("gggggggrrr") == (3*sls("g"))+(3*sls("r")) &   #max of 3 Gs
      
      #only 2 allowed
      sws("bbbbb") == (2*sls("b")) & #max of 2 Bs
      sws("cccp") ==  (2*sls("c")+sls("p")) &   #max of 2 Cs      
      sws("mmmmmmmm") == (2*sls("m")) & #max of 2 Ms
      sws("ppppppplease") == (2*sls("p")+sls("l")+2*sls("e")+sls("a")+sls("s")) &   #max of 2 Ps
      sws("fffffudge") == 2*sls("f")+sls("u")+sls("d")+sls("g")+sls("e") & #max of 2 Fs
      sws("hhhhhhhmmmmmm") ==  2*sls("m") + 2*sls("h") &   #max of 2 Hs       
      sws("vvvvvvvvvv") == 2*sls("v") & #max of 2 Vs
      sws("wwwwwwwhat") == 2*sls("w")+sls("h")+sls("a")+sls("t") &   #max of 2 Ws
      sws("whyyyyyyyyy") == 2*sls("y")+sls("w")+sls("h") & #max of 2 Ys
      #only 1 allowed
      sws("xxx") == sls("x") & #max of 1 X
      sws("kicked") == sls("k")+sls("i")+sls("c")+sls("e")+sls("d") & #max of 1 K
       sws("jjzzz") == sls("z")+sls("j") & #max of 1 J
       sws("quiq") == sls("q")+sls("u")+sls("i") &   #max of 1 Q
      sws("zzz") == sls("z")     #max of 1 Z
     ){
        return(TRUE)
      } else {
        return(FALSE)
      }
}

if(!sws.test()){
  print("warning! There appear to be bugs in the scrabble functions")
}