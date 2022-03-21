library(rvest)
library(httr)


generateFilenameFromURL <- function(url){
  filename <- gsub("[:./?,&=]", "_", url)
  filename <- paste0(filename, ".html")
  return(filename)
}

#by default will attempt to get from cache
getHTML <- function(url, useCache = T){
  filename <- generateFilenameFromURL(url)
  
  #attempt to get from the cache
  #check whether the url has been cached before 
  #(i.e. the filename generated from (a) exists)
  if (useCache && file.exists(filename)){
    html <- readChar(filename, file.info(filename)$size)
    return (html)
  }
  
  #download if cache is not found
  print("#downloading...")
  req <- GET(url)
  html <- content(req)
  html <- as.character(html)
  
  #cache the file
  write(html, filename)
  
  return (html)
}

#extract each link to each product
getLinks <- function(url){
  #url <- 'https://www.etsy.com/sg-en/c/vintage/jewelry/rings/midi-rings?explicit=0&ship_to=SG&q=&ref=pagination&page='
    #get the url to each page=1,2,3
    page <- read_html(getHTML(url))
    #get each product link
    links <- page %>% html_nodes("a.listing-link.wt-display-inline-block") %>% html_attr("href")
  return(links)
}


#for testing purpose
getLinks('https://www.etsy.com/sg-en/c/vintage/jewelry/rings/midi-rings?explicit=0&ship_to=SG&q=&ref=pagination&page=1')

#extract individual item info
extractEntry <- function(url){
  #url<- "https://www.etsy.com/sg-en/listing/247020065/rowboat-alone-at-sea-watercolor-archival"
  
  page <- read_html(getHTML(url))
  url <- url
  #Complete implementation...
  title <- page %>% html_node("#listing-page-cart .wt-break-word") %>% html_text()
  title <- gsub("Read the full title[\r\n]|\n|", "", title)
  title <- trimws(title)
  
  price <- page %>% html_nodes('p[class ^= "wt-text-title-03 wt-mr-xs-2"]') %>% html_text()
  price <- gsub("[\r\n]|\n|", "", price)
  price <- gsub("[^0-9.-]", "", price)
  price <- trimws(price)
  as.numeric(price)
  
  reviews <- page %>% html_node(".wt-mr-xs-2.wt-text-body-03") %>% html_text()
  reviews <- gsub("[\r\n]|\n|", "", reviews)
  reviews <- gsub("[^0-9,-]", "", reviews)
  reviews <- gsub("[^[:alnum:]]", "",reviews)
  as.numeric(reviews)
  
  quantity_sold <- page %>% html_node("#listing-page-cart .wt-flex-wrap .wt-text-caption") %>% html_text()
  quantity_sold <- gsub("[\r\n]|\n|", "", quantity_sold)
  quantity_sold <- gsub("[^0-9,-]", "", quantity_sold)
  quantity_sold <- gsub("[^[:alnum:]]", "",quantity_sold)
  as.numeric(quantity_sold)
  
  location <- page %>% html_node(".wt-grid__item-xs-12.wt-text-caption") %>% html_text()
  location <- gsub("[\r\n]|\n|", "", location)
  location <- gsub("Dispatches from", "", location)
  location <- trimws(location)
  
  deliverycost <- page %>% html_node("#shipping-variant-div .wt-mb-xs-4 div .wt-line-height-tight") %>% html_text()
  deliverycost <- gsub("SGD", "", deliverycost)
  deliverycost <- trimws(deliverycost)
  
  seller_name <- page %>% html_node("#desktop_shop_owners_parent .wt-mb-xs-1") %>% html_text()
  
  return(data.frame(Name_of_product =title, price = price, delivery_cost = deliverycost, reviews = reviews, quantity_sold = quantity_sold, dispatch_from = location, seller_name = seller_name, url = url))
}

#for testing purposes
extractEntry('https://www.etsy.com/sg-en/listing/693385095/silver-ring-set-armat-3-silver-rings')

#define an empty data frame for storing the article details (access upon creating csv first time)
entrydf <- data.frame(Name_of_product=character(), price=numeric(), delivery_cost = character(), reviews =numeric(), quantity_sold =numeric(), dispatch_from = character(),seller_name = character(), url = character()) 

#loop thru each page to get each product informations
url2 <- "https://www.etsy.com/sg-en/c/art-and-collectibles/painting/watercolor?ref=pagination&page="
for(page_result in seq(from = 1, to = 15)){
  links <- paste0(url2,page_result)
  #get the first 5 entries 
  links2 <- getLinks(links)
  for (url in links2){ 
    #sleep 0.2 second 
    Sys.sleep(0.2) 
    
    entrydf <- rbind(entrydf, extractEntry(url)) 
    #entrydf[is.na(entrydf)] = 0
  } 
}
#entrydf$delivery_cost[is.na(entrydf$delivery_cost)] = 0

View(entrydf) 
write.csv(entrydf, "etsy.csv")

etsy <- read.csv("~/NUS/y3sem1/web mining/ass2/etsy.csv")
View(etsy)

mean(etsy$price)

#part c
#currently most expensive item
class(entrydf$price)
etsy$price <- as.numeric(etsy$price)
exp <-  etsy$Name_of_product[which(etsy$price == max(etsy$price))]
exp

#most commonly sold item
etsy$quantity_sold <- as.numeric(etsy$quantity_sold)
commonitemsold <- etsy$Name_of_product[which(etsy$quantity_sold == max(etsy$quantity_sold, na.rm = T))]
commonitemsold

#commonly sold item using word cloud
library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(tm)
itemsolddf <- etsy[,c('Name_of_product','quantity_sold')]
itemsolddf$quantity_sold[is.na(itemsolddf$quantity_sold)] = 0
itemsolddf <- itemsolddf %>% arrange(desc(quantity_sold))
class(itemsolddf)
set.seed(1234)
wordcloud(words = itemsolddf$Name_of_product, freq = itemsolddf$quantity_sold, min.freq = 100, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

#simple linear regression
model1<- lm(reviews~quantity_sold, data = etsy)
summary(model1)
#multi lr
model2<- lm(reviews~quantity_sold+price, data = etsy)
summary(model2)


#classification tree
library(tree)
library(rpart)
etsy$reviews <- as.numeric(etsy$reviews)
etsy$quantity_sold <- as.numeric(etsy$quantity_sold)
etsy$price <- as.numeric(etsy$price)
ctree <- rpart(I(log(quantity_sold))~reviews+price, etsy)
plot(ctree, uniform = TRUE, margin = 0.1)
text(ctree, use.n = TRUE, all = TRUE, cex = 0.7)



cor(etsy$price, etsy$reviews,use="complete.obs",method = c("pearson"))
#a weak negative linear relationship

library(plyr)
library(ggplot2)
freqtable<-as.data.frame(count(etsy, 'price'))
ggplot(freqtable, aes(x=price)) + geom_histogram(binwidth=50, colour="black", fill="white")
etsy2<-etsy[-573,]
freqtable2<-as.data.frame(count(etsy2, 'price'))
ggplot(freqtable2, aes(x=price)) + geom_histogram(binwidth=20, colour="black", fill="white")


freqtable3<-as.data.frame(count(etsy, 'reviews'))
ggplot(freqtable3, aes(x=reviews)) + geom_histogram(binwidth=50, colour="black", fill="white")

boxplot(etsy$price, horizontal=TRUE, main="price in etsy")

boxplot(etsy2$price, horizontal=TRUE, main="price in etsy")

price<-as.vector(etsy2$price)
hist(price, breaks=20)








