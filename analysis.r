#imported data set. chose the "Datafiniti_Womens_Shoes.csv" file.
shoes <- read.csv('Datafiniti_Womens_Shoes.csv')
#explored data set. total 10,000 records from 129 brands.
#spotted duplicated rows. all priced in USD.
library(tidyverse)
glimpse(shoes)
length(unique(shoes$id))
length(unique(shoes$brand))
length(unique(shoes$prices.currency))

#clean the data
wshoes <- shoes[ ,c("id","brand","imageURLs","prices.amountMax","prices.amountMin","prices.currency","prices.isSale")] %>% distinct()
glimpse(wshoes)

#Find the average price for each brands.

#For the price > 100. The most expensive brand is Red Wing. 
wshoes %>% group_by(brand) %>% summarize(count=length(id),avg_price=mean((prices.amountMin+prices.amountMax)/2)) %>% filter(avg_price>=100) %>% top_n(20) %>% arrange(desc(avg_price)) %>% ggplot(aes(brand,avg_price)) + geom_col(aes(fill=brand))+theme(legend.position="none")+theme(axis.text.x= element_text(angle = 45,hjust=1,vjust=1))

#For the price between 50 - 100. the most expensive brand is Olukai.
wshoes %>% group_by(brand) %>% summarize(count=length(id),avg_price=mean((prices.amountMin+prices.amountMax)/2)) %>% filter(avg_price<100 & avg_price >= 50) %>% top_n(20) %>% arrange(desc(avg_price)) %>% ggplot(aes(brand,avg_price)) + geom_col(aes(fill=brand))+theme(legend.position="none")+theme(axis.text.x= element_text(angle = 45,hjust=1,vjust=1))

#For the price below 50. the most expensive brand is Keds.
wshoes %>% group_by(brand) %>% summarize(count=length(id),avg_price=mean((prices.amountMin+prices.amountMax)/2)) %>% filter(avg_price<50) %>% top_n(20) %>% arrange(desc(avg_price)) %>% ggplot(aes(brand,avg_price)) + geom_col(aes(fill=brand))+theme(legend.position="none")+theme(axis.text.x= element_text(angle = 45,hjust=1,vjust=1))

#Find the average price range for each brands.Lowa's price varied the most. 
wshoes %>% group_by(brand) %>% summarize(count=length(id),price_range=mean(prices.amountMax-prices.amountMin)) %>% arrange(desc(price_range)) 


#wordcloud by brand count. 

library(wordcloud)
library(RColorBrewer)
word <- wshoes %>% group_by(brand) %>% summarize(count=length(id)) %>% arrange(desc(count))
set.seed(500)
wordcloud(words = words$brand, freq = words$count,colors=brewer.pal(8, "Dark2"))
