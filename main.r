library(ScrabbleScore)
srcdir <- function(file){
  paste("./word_sources",file,sep="/")
}
outdir <- function(file){
  paste("./results",file,sep="/")
}

#freebsd/unix words
words <- scan(srcdir("words.txt"),what="",sep="\n")

#Official Scrabble words 'twl06'
data(twl06)

in.alpha.order <- function(w,f=function(x){x}){
  wasc <- strsplit(w,"")[[1]]
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

scrabble.ordered.words <- get.in.order.words(twl06)
scrabble.rev.ordered.words <- get.in.order.words(twl06,f=rev)

ordered.scores <- sws(scrabble.ordered.words)
rev.ordered.scores <- sws(scrabble.rev.ordered.words)

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