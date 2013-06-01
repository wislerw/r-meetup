# calculate the different NGrams and the score each one has
# functun requires a string to parse and a length of the gram

NGram <- function(str.to.parse, n = 3){
  #setup the output
  rslt <- data.frame(gram = character(0), gram.ct = integer(0)
                     , stringsAsFactors = FALSE)
  # get rid of uppercase; whitespace; and numbers
  str.to.parse <- tolower(gsub("\\W|\\d", "", str.to.parse))
  
  # the string must still have a postitive length after being sanitized
  if(length(str.to.parse) > 0){
    # iterate through each charater in the string
    for(i in n:nchar(str.to.parse)){
      # grab a substring of gram length
      sub.str <- substr(str.to.parse, i - n + 1, i)
      
      #check if we have encountered the string before
      if(sub.str %in% rslt$gram){
        # if so add to it's counter
        rslt[rslt$gram == sub.str,"gram.ct"] <-
          as.integer(rslt[rslt$gram == sub.str,"gram.ct"]) + 1
      }else{
        # add the new string to the dictionary
        rslt[dim(rslt)[1]+1,] <- cbind(sub.str, 1)
      }
    }
  }
  # after all have been countered compute the frequency for the column
  rslt$gram.ct <- as.integer(rslt$gram.ct)
  rslt$freq <- rslt$gram.ct / sum(rslt$gram.ct)
  return(rslt)
}

# calculate the difference between two sets of n-grams
# function takes two string and how long the count should be
NGramDist <- function(str.a, str.b, n = 3){
  # calculate the gram probabilities for each
  set1 <- NGram(str.a, n)
  set2 <- NGram(str.b, n)
  d <- 0
  
  # for each unique gram in phrase 1
  for(i in 1:dim(set1)[1]){
    current.gram <- set1[i,"gram"]
    # find the frequence of the gram in each word set
    freq.a.g <- set1[i,"freq"]
    freq.b.g <- set2[set2$gram == current.gram, "freq"]
    # if it is missing from word b then use 0
    if(length(freq.b.g) == 0 ){
      freq.b.g <- 0
    }
    # calculate a measure for this gram based on the frequencies
    # add the gram measure to a running total for the word
    d <- d + (
      (2*(freq.a.g - freq.b.g) / (freq.a.g + freq.b.g)) ^ 2
    )
  }
  
  # calc the final score based on how many (non-unique+unique) grams existed
  return(d / (4 * sum(set1$gram.ct)))
}


dyn.load("NGram.so")

# original R code test
ptm <- proc.time()
for(i in 1:4900){
  NGramDist("LN PROJECTS 10 OZ", "LN PROJECTS 10OZ 24PK", 5)
}
proc.time() - ptm

# c code test
ptm <- proc.time()
for(i in 1:490000){
  str.a <- tolower("LN PROJECTS 10OZ 24PK")
  str.b <- tolower("LN PROJECTS 10 OZ")
  .Call("NGramLetters", str.a, str.b, 5)
}
proc.time() - ptm
