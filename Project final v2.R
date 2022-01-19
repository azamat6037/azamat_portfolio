# Optional package installation if not already installed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, tidyverse, maps, mapproj, readr, RedditExtractoR, tidytext, SnowballC, wordcloud, dplyr)

# Importing all the required packages to our project 
library(readxl)
library(tidyverse)
library(maps)
library(mapproj)
library(readr)
library(RedditExtractoR)
library(tidytext)
library(tm)
library(SnowballC)
library(wordcloud)
library(dplyr)

# Importing partially cleaned data sets from excel and csv files

Business_filings_2021 <- read_excel("Business filings 2021.xlsx", 
                                    sheet = "2001-2021 years")

states <- read_csv("states.csv")

Population_2001_2010 <- read_delim("Population 2001-2010.csv", 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

Population_2010_2020 <- read_delim("Population 2010-2020.csv", 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

population <- Population_2001_2010 %>% 
                  filter(ORIGIN==0) %>% 
                  select(-POPESTIMATE2010, -STATE, -REGION, -DIVISION) %>%
                  left_join(Population_2010_2020, by = c("NAME" = "NAME"))


usa_tbl <- map_data("state") %>% as_tibble()


# Joining datasets 
data_selected <- Business_filings_2021 %>%
  left_join(states, by=c("region" = "Code" )) %>%
  mutate(state = tolower(State)) %>%
  left_join(population, by = c("State" = "NAME")) %>%
  right_join(usa_tbl, by = c("state" = "region"))  %>% 
  as_tibble()



# Data wrangling

# Getting the first look at the data
#summary(data_selected)
#str(data_selected)
#colnames(data_selected)
#unique(data_selected)


#function for selecting 1 year

year_function <- function(x = 2020){
  return(
     df <-  data_selected %>%
      select(toString(paste("total", sep="_", x)), 
             toString(paste("total_business", sep="_", x)), 
             toString(paste("total_nonbusiness", sep="_", x)),
             toString(paste("POPESTIMATE", sep="", x)), 
             state, long, lat, group, subregion) %>%
             as_tibble()
  )
}

# function to get descriptive statistics for 3 columns

total_stats_function <- function(x){
  
  selected <- year_function(x = year)[1:3]
  return(summary(selected)) 
}

#### Running year and year_function gives the map for the selected year

###Column_of_interest 
# total_ratio - by default 
# business_ratio 
# nonbusiness_ratio

year <- 2017


total_stats_function()


year_function(x = year) %>%
  mutate(total_ratio=as.numeric(unlist(lapply(100*year_function(x = year)[1]/year_function(x = year)[4], as.numeric))), 
       business_ratio=as.numeric(unlist(lapply(100*year_function(x = year)[2]/year_function(x = year)[4], as.numeric))), 
       nonbusiness_ratio=as.numeric(unlist(lapply(100*year_function(x = year)[3]/year_function(x = year)[4], as.numeric)))) %>%
  as_tibble() %>%
  ggplot(aes(long, lat, group = subregion)) + 
  geom_map(
    aes(map_id = state),
    map = usa_tbl,
    color = "gray80", fill = "gray30", size = 0.3
  ) + 
  coord_map("ortho", orientation = c(25, -100, 0)) +
  geom_polygon(aes(group = group, fill = total_ratio ), color = "black") + 
  scale_fill_gradient2(low="green", mid="lightgreen", high="red") +
  theme_minimal() + 
  labs(
    title = paste("Bankruptcy rates map in", year),
    x = "", y = "", fill = ""
  ) +
  theme (
    plot.title = element_text(size = 25, face = "bold", color = "black")
  )
    


################ Pivoting the data and  using it in the bar chart


state <- "UT"

wide_business <- Business_filings_2021 %>% 
  filter(region == state) %>% 
  select(region, contains("total_business_20")) %>% 
  pivot_longer(!region, names_to = "years", values_to = "business_filings") %>%
  mutate( year = str_replace_all(years, "total_business_", ""))

wide_nonbusiness <- Business_filings_2021 %>% 
  filter(region == state) %>% 
  select(region, contains("total_nonbusiness_20")) %>% 
  pivot_longer(!region, names_to = "years", values_to = "nonbusiness_filings") %>%
  mutate( year = str_replace_all(years, "total_nonbusiness_", ""))


bar_data <- wide_business %>%
  left_join(wide_nonbusiness, by = c("year" = "year")) %>%
  mutate(total =  business_filings + nonbusiness_filings) %>%
  as_tibble()
  
bar_data %>%
  ggplot(aes(x = year, y = business_filings+nonbusiness_filings, fill = business_filings)) +
  geom_bar(stat = "identity") +
  labs(title = paste("Bankruptcy filings in", state), x = "Years", y = "Number of filings")


##### Additional code which shows how we arrived to US map #####





# Using map_data() function we can use our date to illustrate in ggplot2 
?map_data()

#### Creating first World map (Example)

# Now we start creating our world map, using region="world" filter we see all countries
world_tbl <- map_data("world") %>%
  as_tibble()

# We can see long & lat and country names which will be our mapping variable
world_tbl

world_map <- world_tbl %>%
  ggplot()+ 
  # we use geom_map function which creates base/(x and y axis) for the map long/lat
  geom_map(
    aes(long, lat, map_id = region),
    map = world_tbl, # we use world_tbl which was created before and contains world map data
    colour = "black", fill = "aquamarine4", size = 0.3 # specifying some aesthetics
  )
# now we have map of the world 
world_map



# we need map only for the United States 
# we start the process using coord_map which gives us the spherical format
?coord_map

world_map +
  coord_map("ortho", orientation = c(25, -100, 0)) # focusing approximately on US using orientation

# to get proper state borders we use usa_tbl 

us_map <- usa_tbl %>%
  ggplot(aes(long, lat, map_id = region))+ 
  geom_map(
    map = usa_tbl, # now we use usa_tbl instead of world_tbl 
    colour = "black", fill = "aquamarine4", size = 0.2 # specifying some aesthetics
  ) + 
  coord_map("ortho", orientation = c(25, -100, 0))

us_map














######################################## Web scraping part

# Scraping subreddit:Bankruptcy, trying to scrape all but Reddit has limit. We scrape until the limit
top_bankruptcy_urls <- find_thread_urls(subreddit="Bankruptcy", sort_by="top", period = "all")
str(top_bankruptcy_urls)

# Tokenizing text column or separating each word for each row
top_bankruptcy_urls <- top_bankruptcy_urls %>%
  unnest_tokens(word, text)


# Counting top words in the text 
top_bankruptcy_urls %>%
  count(word, sort=TRUE)

# Remowing stopwords from our data
top_bankruptcy_urls <- top_bankruptcy_urls %>%   
  anti_join(get_stopwords("en"))

top_bankruptcy_urls <- top_bankruptcy_urls %>%
  mutate(stem = wordStem(word, language = "english"))

wordcount <- top_bankruptcy_urls %>%
                    count(stem, sort=TRUE)

colorpalette <- c("Sea green", "Green", "Darkgreen")

wordcloud(words=wordcount$stem, freq=wordcount$n, max.words=150, colors=colorpalette)
  
  
  