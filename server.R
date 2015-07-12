library(shiny)
require(tm)
require(RColorBrewer)
require(wordcloud)


x <- 0

shinyServer(
  function(input, output){
    output$filetable <- renderTable({
      inFile <- input$file
      if (is.null(inFile))
        return(NULL)
      inFile})
    
    readText <- reactive({
      inFile <- input$file
      if (is.null(inFile))
        return(NULL)
      upload.path <- substr(inFile$datapath, 1, nchar(inFile$datapath)-1)
      myCorpus <- Corpus(DirSource(upload.path))
    })

    x <<- x + 1
    #output$count <- renderText({paste("You are the user number:", x)})
    output$results <- renderPlot({
      input$goButton
      isolate(
      b <- readText())
      #input <- b[[1]]
      input <- b
      if (is.null(input))
        return("No file uploaded")
      input <- tm_map(input, stripWhitespace)
      ## Remove Punctuation
      input <- tm_map(input, removePunctuation)
      
      # Convert to Lower Case
      lower_input <- tm_map(input, tolower)
      
      # Remove Stopwords
      input <- tm_map(lower_input, removeWords, stopwords("english"))
      
      ## Skip some less informative words
      skipWords <- function(x) removeWords(x, c("name", "ive", "cant", "dont","wont", "im", "hi", "ctl"))
      input <- tm_map(input, skipWords)
      
      # Creating A Term - Document Matrix with frequencies as entries
      dtm <- DocumentTermMatrix(input)
      
      # Find terms with frequency greater than 3
      #findFreqTerms(dtm,3)
      # Creating a term-document matrix with tf-idf scores
      #dtm.tf-idf <- DocumentTermMatrix(input, control = list(weighting = weightTfIdf))
      
      ##################################################
      ## CREATING WORDCLOUD
      ##############################################
      m <- as.matrix(dtm)
      v <- sort(colSums(m), decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)
      table(d$freq)
      
      pal <- brewer.pal(8, "Dark2")
      #pdf("wordcloud.pdf", width=1280,height=800)
      #wordcloud(d$word,d$freq, scale=c(8,.2),min.freq=2, max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal)
      wordcloud(d$word,d$freq, scale=c(8,.2),min.freq=3, max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal)
      dev.off()

    })
  }
)