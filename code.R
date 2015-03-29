# Do not forget to add the following libraries in the "Dependency R packages" section:
#   tm
#   wordcloud
#   RCurl

# Utility function to download a file
download.to.file <- function(httpsurl, destfile) {
  data <- getURLContent(httpsurl, followlocation = T, binary = T, ssl.verifypeer = F)
  file.connection <- file(destfile, "wb")
  writeBin(as.raw(data), file.connection)
  close(file.connection)
}

# Download the text of the three works
download.to.file("https://s3-eu-west-1.amazonaws.com/intuitics-public/merchant.txt.gz", "merchant.txt.gz")
download.to.file("https://s3-eu-west-1.amazonaws.com/intuitics-public/romeo.txt.gz", "romeo.txt.gz")
download.to.file("https://s3-eu-west-1.amazonaws.com/intuitics-public/summer.txt.gz", "summer.txt.gz")

# Map the name of each work to its file
books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

# Return the names to show in the drop-down list
get.book.names <- function(){ 
  names(books)
}

# Draw the word cloud
draw.plot <- function(book, min.freq, max.words) {
  # Get the filename from the selected book title
  filename <- books[[book]]
  
  # Read the compressed file containing the text for the selected book
  text <- readLines(sprintf("./%s.txt.gz", filename), encoding="UTF-8")
  
  # Clear the text by converting it to lower case, removing punctuation, numbers, and commonly used words
  corpus = Corpus(VectorSource(text))
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, removeWords, c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  
  # Convert the result into an integer vector listing each word's frequenty (the words are in the names attribute)
  tdm = TermDocumentMatrix(corpus, control = list(minWordLength = 1))
  word.freq = sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
  
  # Draw the word cloud
  wordcloud(names(word.freq), word.freq, scale = c(4,0.5),
            min.freq = min.freq, max.words = max.words,
            colors = brewer.pal(8, "Dark2"))
}

# Set default for application start
book <- "A Mid Summer Night's Dream"
min.freq <- 20
max.words <- 20