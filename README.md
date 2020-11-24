# Trump-Tweet-Project
This project will be focused on data collecting and data manipulation. The data was collected from Trump Twitter Archive. (http://www.trumptwitterarchive.com/archive) 
Date 2016-01-01 to 2020-10-04


As the US election approaches, we see President Donald Trump has been tweeting more often. In this project, I will be analyzing Trump's tweet from 2016-01-01 to 2020-10-04. I will be looking at his tweeting pattern in terms of time and date, and hopefully I will come up with some interesting Trump's tweeting pattern. 
This project is inspired by [David Robinson's blog](http://varianceexplained.org/r/trump-tweets/) and [Nathan Taback's report](http://utstat.toronto.edu/~nathan/teaching/sta4002/Class4/trumptweets-students.html). The data is collected from [here](http://www.trumptwitterarchive.com/). 

# Data
The data is collected from Trump Tweet Archive created by Brendan Brown and I copied and pasted the data from 2016-01-01 to 2020-10-04 to txt file. 
```{r message=FALSE, warning=FALSE}
library(data.table)
trump <- fread("C:/Users/sheri/Desktop/Data/trump.txt")
```
