# STAT-S 610 Final Project
# TO RUN: testthat::test_dir('.')

# --- setup --- #
# load package functions and functions we want to test


library(testthat)
library(devtools)
import::here(make_unigram, make_bigram, create_dictionary, generate,
             .from = 'finalRcode.R')


context("The output text is generate from Adventures of Sherlock Holmes by Arthur Conan Doyle ")
context("------------------------------------------------------------------------------------- ")

# read files
textdata<-readr::read_lines("sherlock.txt")
## run the function
unigrame.df<-make_unigram(textdata)
bigrame.df<-make_bigram(textdata)
dictionary<-create_dictionary(bigrame.df,unigrame.df)
init=unigrame.df$word[1]

context(toupper(generate(init,dictionary)))
