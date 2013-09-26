#from http://www.r-bloggers.com/ascii-code-table-in-r/
asc <- function(x) { strtoi(charToRaw(x),16L) }

#Unix/FreeBSD word list
words <- scan('words.txt',what="",sep="\n")
words <- tolower(words)
#only 3 letter or greater words are interesting
words <- words[which(sapply(words,function(w)nchar(w)>2))]

in.alpha.order <- function(w){
  wasc <- asc(w)
  all(wasc == wasc[order(wasc)])  
}

in.rev.alpha.order <- function(w){
  wasc <- asc(w)
  all(wasc == wasc[rev(order(wasc))])  
}

in.order <- which(sapply(words,in.alpha.order))
in.rev.order <- which(sapply(words,in.rev.alpha.order))
ordered.words <- words[in.order]
rev.ordered.words <- words[in.rev.order]
write(ordered.words,file="ordered_words.txt")
write(words[in.order],file="rev_ordered_words.txt")