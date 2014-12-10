# Do not forget to let the following libraries in the Dependency R packages section:
#   tm
#   wordcloud
#   RCurl

books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

download.file.https2 <- function(httpsurl, destfile=NULL){
  data <- getURLContent(httpsurl, followlocation=T, binary=T, ssl.verifypeer = FALSE)
  if(is.null(destfile)){
    destfile = tempfile("downloaded")
  }
  to.write = file(destfile, "wb")

  writeBin(as.raw(data), to.write)
}

download.file.https2("https://github.com/rstudio/shiny-examples/blob/master/082-word-cloud/merchant.txt.gz?raw=true", "merchant.txt.gz")
download.file.https2("https://github.com/rstudio/shiny-examples/blob/master/082-word-cloud/romeo.txt.gz?raw=true", "romeo.txt.gz")
download.file.https2("https://github.com/rstudio/shiny-examples/blob/master/082-word-cloud/summer.txt.gz?raw=true", "summer.txt.gz")

books.func <- function(){ 
   names(books)
}

books.mapper <- function(x) {
 list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")[[x]]
}

getTermMatrix <- function(book) {
  if (!(book %in% books))
    stop("Unknown book")

  text <- readLines(sprintf("./%s.txt.gz", book),
    encoding="UTF-8")

  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
         c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))

  myDTM = TermDocumentMatrix(myCorpus,
              control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
}

outputplot <- function(freq, max, book){
  wordcloud(names(matrix), matrix, scale=c(4,0.5),
                min.freq = freq, max.words=max,
                colors=brewer.pal(8, "Dark2"))
}
