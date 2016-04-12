library( XML)
library( RCurl)

html <- htmlParse("http://stackoverflow.com/questions/tagged/r?sort=newest&pagesize=50")

#usernames
usernames <- function( html){
  path<-"//div[@class = 'user-details']"
  nodes <- getNodeSet( html, path)
  return( sapply( strsplit( xmlSApply(nodes, xmlValue, trim = TRUE), "\r\n"), `[[`, 1))}

#when
timestmps <- function( html){
  path <- "//div[@class = 'user-action-time']/span/@title"
  nodes <- getNodeSet( html, path)
  times <- unlist(nodes)
  names( times) = NULL
  return( times)}

#title
titles <- function( html){
  path <- "//div[@class = 'summary']/h3/a[@class = 'question-hyperlink']/text()"
  nodes <- getNodeSet( html, path)
  return( xmlSApply( nodes, xmlValue))}

reps <- function( html){
  path <- "//div[@class = 'user-details']//span[@class = 'reputation-score']/text()"
  nodes <- getNodeSet( html, path)
  interject <- xmlSApply( nodes, xmlValue)
  path2 <- "//div[@class = 'user-details']"
  nodes2 <- getNodeSet(html, path2)
  ref <- sapply( strsplit( xmlSApply(nodes2, xmlValue, trim = TRUE), "\r\n"), function(p) length(p)>1)
  ref[which(ref == TRUE)] <- interject
  ref[which(ref == "FALSE")] <- NA
  return(ref)}

#views
views <- function( html){
  path <- "//div[@class = 'statscontainer']/div[starts-with(@class, 'views')]/@title"
  nodes <- getNodeSet( html, path)
  views <- unlist( strsplit( unlist (nodes), " views"))
  names( views) = NULL
  return( views)}

#answers
answers <- function( html){
  path <- "//div[@class = 'stats']/div/strong/text()"
  nodes <- getNodeSet( html, path)
  return( xmlSApply( nodes, xmlValue))}

#votes
votes <- function( html){
  path <- "//div[@class = 'votes']/span/strong/text()"
  nodes <- getNodeSet( html, path)
  return( xmlSApply( nodes, xmlValue))}

#hyperlink
links <- function( html){
  path <- "//a[@class = 'question-hyperlink']/@href"
  nodes <- getNodeSet( html, path)
  links <- unlist( nodes)
  links <- getRelativeURL( links, docName( html))
  names( links)= NULL
  return( links)}

#id
id <- function( html){
  path <- "//div[@class = 'question-summary']/@id"
  nodes <- getNodeSet( html, path)
  idz <- unlist(strsplit(unlist(nodes), "-"))[seq(3,3*50,3)]
  names(idz) = NULL
  return( idz)}

#nextURL
getNextURL = function( html){
  nxt = unique(unlist(getNodeSet(html, "//a[@rel = 'next']/@href")))
  return( getRelativeURL(nxt, docName(html)))}

#tags
tags <- function( html){
  path = "//div[starts-with(@class,'tags')]"
  nodes <- getNodeSet( html, path)
  return( xmlSApply( nodes, xmlValue, trim = TRUE))}

#scrape a page
pagescrape = function( html){
  data.frame(
    id = id( html),
    date = timestmps( html),
    tags = tags( html),
    title = titles( html),
    url = links( html),
    views = views( html),
    votes = votes( html),
    answers = answers( html),
    user = usernames( html),
    reputation = reps( html))}

a <- pagescrape(html)

#scrape all tha pagez
ayyyylmao =
  function( tag ,pages)
  {
    ans = NULL
    page = 1
    tag <- sub( " ", "-", tag)
    beg <- "http://stackoverflow.com/questions/tagged/"
    end <- "?sort=newest&pagesize=50"
    u <- paste( beg, tag, end, sep="")
    u <- htmlParse( u)
    while(TRUE) {
      d = pagescrape(u)
      ans = rbind(ans,d)
      u = getNextURL(u)
      u = htmlParse(u)
      page = 1 + page
      if(length(u) == 0 | page > pages)
        break
    }
    
    return(ans)
  }


#FIRST ARGUMENT MUST BE A CHARACTER // SPACES ONLY IN BETWEEN WORDS ex: 'string concatenation'
ayyyylmao( "r", 3)

#Distribution of Questions answered

load("C:/Users/Ali/Downloads/rQAs (1).rda")

answers <- rQAs[ which( rQAs$type == "answer"),]
answers.by.user <- as.numeric( table( answers$user))

plot( table(answers.by.user), type = "h", 
      xlab = "Number of Questions Answered", 
      main = "Dist. of Questions Answered by User",
      ylab = "Amount of Users")
plot( table(answers.by.user), type = "h", 
      main = "Dist. of Questions Answered by User",
      xlab = "Number of Questions Answered", 
      ylab = "Amount of Users",
      ylim = range(0,20), 
      xlim = range(1,150))

#What are the most common tags?

tags.4.days =
  function( u ,pages)
  {
    ans = NULL
    page = 1
    while(TRUE) {
      tags = tags(u)
      ans = c(ans,tags)
      u = getNextURL(u)
      u = htmlParse(u)
      page = 1 + page
      if(length(u) == 0 | page > pages)
        break
    }
    
    return(ans)
  }

master.tags <- tags.4.days(html,50)
totques <- length(master.tags)
master.tags.raw <- unlist( strsplit(master.tags, " "))
com.tags <- head( sort( table( master.tags), decreasing = TRUE), 25)
dotchart(com.tags[2:25], main = "Most Common Tags", xlab = "Frequency of Questions with Tag")

#How many questions involve XML, HTML, or Web Scraping?

length(grep("xml|html|web-scraping", master.tags, ignore.case=TRUE))/2500

xmlSApply(rQAs$text, xmlValue)
xmlSApply( htmlParse(rQAs[1,6]), xmlValue)
xmlSApply( getNodeSet(xmlParse(rQAs[1,6]), "//text()"), xmlValue)
q.ind <- which(rQAs$type=="question")
involve <- sapply(1:10004, function(q){
  text <- xmlSApply( getNodeSet(xmlParse(rQAs[q.ind[q],6]), "//text()"), xmlValue)
  result <- grep("xml|html|web([ -])?scraping", text, ignore.case=TRUE)
  return(result>0)
})