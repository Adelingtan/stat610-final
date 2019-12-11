textdata<-readr::read_lines("sherlock.txt")

stop_words<-T
one_textdata <- tau::textcnt(
  if(stop_words==T) {
    textdata=gsub("[^[:alnum:] ]", "",textdata)
    tm::removeWords(tm::scan_tokenizer(textdata), tm::stopwords("english"))}
  else {
    tm::scan_tokenizer(textdata)
  }, split = "[[:space:][:punct:][:digit:]]+",method = "string", n = 1L, lower = 1L)

one_textdata <- plyr::ldply(one_textdata, data.frame) 
colnames(one_textdata)<-c("word", "frequency")
one_textdata$text_percent <- one_textdata$frequency / sum(one_textdata$frequency)*100
new_onegrame_textdata <- one_textdata[order(-one_textdata$frequency),]


#bigram_textdata<-lapply(booklist, function(x) tm::PlainTextDocument(readr::read_lines(file = x, 
#                                                                                      progress = interactive()), id = basename(tempfile()), 
#                                                                    language = "en", description = "Book Files"))

bigram_textdata <- tau::textcnt(
  if(stop_words==T) { 
    textdata=gsub("[^[:alnum:] ]", "",textdata)
    tm::removeWords(tm::scan_tokenizer(textdata),tm::stopwords("english"))}
  else {
    tm::scan_tokenizer(textdata)
  }, split = "[[:space:][:punct:][:digit:]]+",method = "string", n = 2L, lower = 1L)

bigram_textdata <- plyr::ldply(bigram_textdata, data.frame) 
colnames(bigram_textdata)<-c("word", "frequency")
bigram_textdata$text_percent <- bigram_textdata$frequency / sum(bigram_textdata$frequency)*100
new_bigram_textdata <- bigram_textdata[order(-bigram_textdata$frequency),]
head(new_bigram_textdata)

bigword<-data.frame(words_bi=new_bigram_textdata$word,new_bigram_textdata$text_percent) 

bigword<-bigword %>% separate(words_bi, c("first_word", "word"),sep = "[^[:alnum:]]+")

oneword<-data.frame(new_onegrame_textdata$word,new_onegrame_textdata$text_percent)
colnames(oneword) <- c('word','one_percent')
words_dictionary<-merge(bigword, new_onegrame_textdata,by='word')
words_dictionary$joint<-words_dictionary$new_bigram_textdata.text_percent/words_dictionary$text_percent
words_dictionary<-subset(words_dictionary, select = 1:3)
colnames(words_dictionary)<-c('word','first_word','joint_prob')



generate<-function(init,words_dictionary){
  sentence<- c()
  while(is.na(init)==FALSE){
    sentence<-c(sentence,init)
    secondword<-words_dictionary[words_dictionary$first_word==init,]
    list_second<- setdiff(secondword[order(secondword$joint_prob,decreasing = TRUE),]$word,sentence)
    init<-list_second[1]
  }
  return(toupper(paste(unlist(sentence), collapse=' ')))
}

init=new_onegrame_textdata$word[1]
generate(init,words_dictionary)
