
#' @param text_data (txt) content read from read_lines
#' @return (data_frame) with column names "word", "frequency", and 'text_percent'

make_unigram<-function(text_data){
  stop_words<-T
  one_textdata <- tau::textcnt(
    if(stop_words==T) {
      textdata=gsub("[^[:alnum:] ]", "",text_data)
      tm::removeWords(tm::scan_tokenizer(text_data), tm::stopwords("english"))}
    else {
      tm::scan_tokenizer(text_data)
    }, split = "[[:space:][:punct:][:digit:]]+",method = "string", n = 1L, lower = 1L)
  
  one_textdata <- plyr::ldply(one_textdata, data.frame) 
  colnames(one_textdata)<-c("word", "frequency")
  one_textdata$text_percent <- one_textdata$frequency / sum(one_textdata$frequency)*100
  new_onegrame_textdata <- one_textdata[order(-one_textdata$frequency),]
  return(new_onegrame_textdata)
}

#bigram_textdata<-lapply(booklist, function(x) tm::PlainTextDocument(readr::read_lines(file = x, 
#                                                                                      progress = interactive()), id = basename(tempfile()), 
#                                                                    language = "en", description = "Book Files"))

#' @param text_data (txt) content read from read_lines
#' @return (data_frame) with column names "word", "frequency", and 'text_percent'
make_bigram<-function(text_data){
  bigram_textdata <- tau::textcnt(
    if(stop_words==T) { 
      text_data=gsub("[^[:alnum:] ]", "",text_data)
      tm::removeWords(tm::scan_tokenizer(text_data),tm::stopwords("english"))}
    else {
      tm::scan_tokenizer(text_data)
    }, split = "[[:space:][:punct:][:digit:]]+",method = "string", n = 2L, lower = 1L)
  
  bigram_textdata <- plyr::ldply(bigram_textdata, data.frame) 
  colnames(bigram_textdata)<-c("word", "frequency")
  bigram_textdata$text_percent <- bigram_textdata$frequency / sum(bigram_textdata$frequency)*100
  new_bigram_textdata <- bigram_textdata[order(-bigram_textdata$frequency),]
  return(new_bigram_textdata)
}

#' @param bigram.df (data_frame) with column names 'word','text_percent'
#' #' @param onegrame.df (data_frame) "word", "frequency", and 'text_percent'
#' @return (data_frame) with column'word','second','condition_prob'
create_dictionary<-function(bigram.df,onegrame.df){
  bigword<-data.frame(words_bi=bigram.df$word,bigram.df$text_percent) 
  bigword<-bigword %>% separate(words_bi, c("word", "second"),sep = "[^[:alnum:]]+")
  oneword<-data.frame(onegrame.df$word,onegrame.df$text_percent)
  colnames(oneword) <- c('word','one_percent')
  words_dictionary<-merge(bigword, onegrame.df,by='word')
  words_dictionary$condition<-words_dictionary$bigram.df.text_percent/words_dictionary$text_percent
  words_dictionary<-subset(words_dictionary, select = 1:3)
  colnames(words_dictionary)<-c('word','second','condition_prob')
  return(words_dictionary)
}

#' @param init (string) with length of 1
#' @param create_dictionary (dataframe) 'word','second','condition_prob'
#' @return (string)
generate<-function(init,create_dictionary){
  sentence<- c()
  while(is.na(init)==FALSE){
    sentence<-c(sentence,init)
    secondword<-create_dictionary[create_dictionary$word==init,]
    list_second<- setdiff(secondword[order(secondword$condition_prob,decreasing = TRUE),]$second,sentence)
    init<-list_second[1]
  }
  return((paste(unlist(sentence), collapse=' ')))
}

# read the file
textdata<-readr::read_lines("sherlock.txt")

## run the function
unigrame.df<-make_unigram(textdata)
bigrame.df<-make_bigram(textdata)
dictionary<-create_dictionary(bigrame.df,unigrame.df)
head(dictionary)
init=unigrame.df$word[1]
generate(init,dictionary)
