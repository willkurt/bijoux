


srcdir <- function(file){
  paste("./word_sources",file,sep="/")
}
outdir <- function(file){
  paste("./results",file,sep="/")
}

#from http://www.r-bloggers.com/ascii-code-table-in-r/
asc <- function(x) { strtoi(charToRaw(x),16L) }

#freebsd/unix words
words <- scan(srcdir("words.txt"),what="",sep="\n")

#Official Scrabble words 'twl06'
scrabble.words <- scan(srcdir('twl06.txt'),what="",sep="\n")
scrabble.words <- tolower(scrabble.words)

in.alpha.order <- function(w,f=function(x){x}){
  wasc <- asc(w)
  all(wasc == wasc[f(order(wasc))]) 
}

get.in.order.words <- function(ws,f=function(x){x}){
  #only 3 letter or greater words are interesting to me
  ws <- ws[which(sapply(ws,function(w)nchar(w)>2))]
  in.order <- which(sapply(ws,in.alpha.order,f))
  ws[in.order]
}

ordered.words <- get.in.order.words(words)
rev.ordered.words <- get.in.order.words(words,f=rev)

write(ordered.words,file=outdir("ordered_words.txt"))
write(rev.ordered.words,file=outdir("rev_ordered_words.txt"))

#for the scrabble fans out there
source('scrabble.r')
scrabble.ordered.words <- get.in.order.words(scrabble.words)
scrabble.rev.ordered.words <- get.in.order.words(scrabble.words,f=rev)

ordered.scores <- sapply(scrabble.ordered.words,sws)
rev.ordered.scores <- sapply(scrabble.rev.ordered.words,sws)

fill <- function(w,l){
  d <- l-nchar(w)
  paste(c(w,rep(' ',d)),sep="",collapse="")
}

format.words <- function(ws){
  sapply(ws,fill,max(sapply(ws,nchar)+1))
}

combine <- function(ws,s){
  or <-  rev(order(s))
  ws <- format.words(ws)
  paste(ws[or],s[or],sep=" : ",collapse="\n")
}

write(combine(scrabble.ordered.words,ordered.scores),file=outdir("scrabble_ordered.txt"))
write(combine(scrabble.rev.ordered.words,rev.ordered.scores),file=outdir("scrabble_rev_ordered.txt"))