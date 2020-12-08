# Trump-Tweet-Project
This project will be focused on data collecting and data manipulation. The data was collected from Trump Twitter Archive. (http://www.trumptwitterarchive.com/archive) 
Date 2016-01-01 to 2020-10-04


As the US election approaches, we see President Donald Trump has been tweeting more often. In this project, I will be analyzing Trump's tweet from 2016-01-01 to 2020-10-04. I will be looking at his tweeting pattern in terms of time and date, and hopefully I will come up with some interesting Trump's tweeting pattern. 
This project is inspired by [David Robinson's blog](http://varianceexplained.org/r/trump-tweets/) and [Nathan Taback's report](http://utstat.toronto.edu/~nathan/teaching/sta4002/Class4/trumptweets-students.html). The data is collected from [here](http://www.trumptwitterarchive.com/). 

All codes have been placed below. To view the final product (outcome & graphs) please download the WORD document named "trump-project.docx". 

# Data
The data is collected from Trump Tweet Archive created by Brendan Brown and I copied and pasted the data from 2016-01-01 to 2020-10-04 to txt file. 
```{r message=FALSE, warning=FALSE}
library(data.table)
trump <- fread("C:/Users/sheri/Desktop/Data/trump.txt")
```
# Exploring the data
We notice that Trump used a variety of devices to access twitter. Apart from that, we find the tweet content are sometimes websites, which we are not interested in. So let's first  take a look at the devices Trump has used. 
```{r message=FALSE, warning=FALSE}
library(dplyr)
trump %>% count(source) %>% arrange(desc(n))
```
In fact, the following graph shows a more detailed version of devices used in a time series. 
```{r message=FALSE, warning=FALSE}
library(ggplot2)
library(lubridate)
tweet <- trump %>% 
  mutate(created_at =as.POSIXct(created_at, format = "%m-%d-%Y %H:%M:%S"))
tweet %>% count(source, year = year(created_at)) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(year, percent, color = source)) +
  geom_line()
  
```   

Here we notice that while the usage of most of devices stay unchanged, that of iPhone and Android changes dramatically. The change of iphone usage may be explained by Trump using Twitter more frequently with his phone but why is there a decrease in the use of Android? Now I will set a constraint on the timeline and investigate what happened from 2016 to 2017. 

# Trump's tweeting pattern
In fact, according to articles back in 2017, Trump has been reported posting slightly negative content with his Android device while positive tweets with iPhone. After such an exposure, Trump stopped using Android devices. Now let's take a closer look and find the difference between them. 

Since Trump's last tweet with Android was on Mar.25, 2017, we are limiting his tweet to those before this date and categorize all other devices into "others" and focus on only iPhone and Android. 

```{r message=FALSE, warning=FALSE}
library(stringr)
tweet_new <- 
  tweet %>% 
   mutate(created_at =as.POSIXct(created_at, format = "%m-%d-%Y %H:%M:%S")) %>%
  filter(created_at <= "2017-03-26") %>%
  mutate(source = ifelse(str_detect(source, "iPhone"), "iPhone", ifelse(str_detect(source, "Android"), "Android", "Others"))) %>%
  filter(source %in% c("iPhone", "Android"))
```

Some of Trump's text contains an external link, so let's take a look on how they distributed across two devices. 
```{r}
tweet_link <- 
  tweet_new %>% 
  count(source, link = ifelse(str_detect(text, "http"), "Contains links", "No links"))
ggplot(tweet_link, aes(x=source, y=n, fill=link))+
  geom_bar(stat = "identity")
```

Similarly,the way he uses @ when referring to other people are also different but not as much as the one above.However, it appears that Trump @ people more frequently when he used Android phone. 
```{r}
tweet_new %>% 
  count(source, direct = ifelse(str_detect(text, "@"), "Contains @", "No @")) %>%
  ggplot(aes(x=source, y=n, fill = direct)) +
  geom_bar(stat = "identity", position = "dodge")
```

# Tweet words and sentiments

```{r message=FALSE, warning=FALSE}
library(tidytext)
tweet_comparison <- 
tweet_new %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z0-9]", " ")) %>%
  unnest_tokens(word, text) %>% 
  #With tokenization transform sentences into words
  filter(!word %in% stop_words$word & !word %in% c("rt","cruz", "amp", "â")) %>%
  #some words appears to be random so they need to be removed
  count(source, word) %>%
  arrange(source, desc(n))
```
Now we have found the words Trump used most frequently in different devices. We can plot them side by side for a visual comparison. 
```{r message=FALSE, warning=FALSE}
tweet_comparison %>% 
  top_n(30) %>% 
  ggplot(aes(x=reorder(word, n), y=n, fill=source)) +
  geom_bar(stat = "identity") +
  xlab("Tweet word") +
  coord_flip() +
  facet_grid(. ~source)
```
We can see Trump used his Android devices to tweet words including "clinton", "cnn" and "bad" but not with his iPhone. Thus, I suspect that he had other agents to tweet for him on a separate device.

```{r message=FALSE, warning=FALSE}
library(tidytext)
#sentiments is a dataframe containing a list of words with their sentiments (positive or negative).
head(sentiments)
tweet_comparison %>%
  inner_join(sentiments, by = "word") %>%
  group_by(sentiment, source) %>%
  summarise(count = sum(n))%>%
  ggplot(aes(sentiment, count, fill=source)) +
  geom_bar(stat = "identity", position = "dodge")
```

According to diagram above, when it comes to words associated with negative sentiments, Trump used his Android phone more often. 
```{r}
# incomplete
#get number of Android negative word/total negative word per day 
tweet_new %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z0-9]", " ")) %>%
  unnest_tokens(word, text) %>% 
  filter(!word %in% stop_words$word & !word %in% c("rt","cruz", "amp", "â")) %>%
  inner_join(sentiments, by = "word") %>%
  filter(sentiment == "negative") %>%
  group_by(date(created_at)) %>%
  mutate(sth = n())
```


# Tweet hour and sentiments
In fact, we can take a closer look at how he tweeted in during different hours. When it comes to tweeting negative words, Android phone usage outperformed that of iPhone. The peak of usage is around noon. 
```{r}
tweet_new %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z0-9]", " ")) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  inner_join(sentiments, by = "word") %>%
  filter(sentiment == "negative") %>%
  count(source, hour = hour(created_at)) %>%
  ggplot(aes(hour, n, color = source)) +
  geom_line()
```

If we disregard the time limit adn look at how Trump used his iPhone since 2016 to present, around 10% of his negative words come from 11-13 o'clock. 
```{r message=FALSE, warning=FALSE}
trump %>%
  mutate(created_at =as.POSIXct(created_at, format = "%m-%d-%Y %H:%M:%S")) %>%
  mutate(source = ifelse(str_detect(source, "iPhone"), "iPhone", "Others")) %>%
  filter(source == "iPhone") %>%
  count(source, hour = hour(created_at)) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line()
```
I wanted to explore whether Trump experienced similar emotions at fixed period of time, but the result below shows that during lunch time, he tweets both positive and negative words and there is no apparent difference between the two. 
```{r message=FALSE, warning=FALSE}
trump %>%
  mutate(created_at =as.POSIXct(created_at, format = "%m-%d-%Y %H:%M:%S")) %>%
  mutate(source = ifelse(str_detect(source, "iPhone"), "iPhone", "Others")) %>%
  filter(source == "iPhone") %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z0-9]", " ")) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  inner_join(sentiments, by = "word") %>%
  count(sentiment, hour = hour(created_at)) %>%
  ggplot(aes(hour, n, color = sentiment)) +
  geom_line()
```

So I took a more detailed look on emotions but still the data doesn't verify my hypothesis. 
```{r}
library(textdata)
sentiment_detail <- get_sentiments("nrc")
trump %>%
  mutate(created_at =as.POSIXct(created_at, format = "%m-%d-%Y %H:%M:%S")) %>%
  mutate(source = ifelse(str_detect(source, "iPhone"), "iPhone", "Others")) %>%
  filter(source == "iPhone") %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z0-9]", " ")) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  inner_join(sentiment_detail, by = "word") %>%
  count(sentiment, hour = hour(created_at)) %>%
  ggplot(aes(hour, n, color = sentiment)) +
  geom_line() +
  facet_grid(sentiment~.)
```

However, I found an interesting pattern regarding Trump's sleep schedule. Assume he is the only one with his Twitter account on his iPhone, Trump sleeps very late and gets up early and it seems that he only sleeps for around 5 hours. 
