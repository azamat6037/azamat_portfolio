rm(list=ls())

library(readxl)
library(readr)
library(tidyverse)
library(lubridate) #date
library(Hmisc) #describe function
library(moments) #kurtosis and skewness
library(jtools) #export_sums reg outputs put together
library(ggpubr) #put multiple graphs together
library(lmtest) #Newey west 
library(sandwich) #Newey west



#### Importing data ####

#set the working directory from which the files will be read from
setwd("C:/Users/azama/Desktop/Thesis data/top200")

#create a list of the files from your target directory
file_list <- list.files(path="C:/Users/azama/Desktop/Thesis data/top200")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
dataset <- data.frame()

#had to specify columns to get rid of the total column
for (i in 1:length(file_list)){
  temp_data <- read_csv(file_list[i]) #each file will be read in, specify which columns you need read in to avoid any errors
  temp_data$Class <- gsub(".csv", "", file_list[i]) #clean the data as needed, in this case I am creating a new column that indicates which file each row of data came from
  dataset <- rbind(dataset, temp_data) #for each iteration, bind the new data to the building dataset
  
}


all_coins_with_ids <- read_csv("C:/Users/azama/Desktop/Thesis data/category/all_coins_with_ids.csv")


dataset <- dataset %>%
  filter( !is.na(market_cap) ) %>%         
  rename(close = price) %>% 
  left_join(all_coins_with_ids, by = c( "Class" = "id" )) %>% 
  mutate( date = as.Date(date) ) %>%
  rename( symbol = symbol_x ) %>% 
  select(date, close, total_volume, market_cap, Class, coin_name, symbol, rank, consensus)



dataset %>% 
  filter(Class != coin_name)


names(dataset) <- tolower(names(dataset))



sapply(dataset, function(x) sum(is.na(x)))
sapply(dataset, function(x) sum(is.infinite(x)))
sapply(dataset, function(x) sum(is.null(x)))












#### 




#checking data merge sanity

colnames(dataset)

summary(dataset)

unique(dataset$class)
unique(dataset$consensus)




#checking for null values

sapply(dataset, function(x) sum(is.na(x)))
sapply(dataset, function(x) sum(is.infinite(x)))
sapply(dataset, function(x) sum(is.null(x)))


####




#### Data preparation for models ####

#creating variables


dataset <- dataset %>% 
  group_by(class) %>% 
  mutate( c_daily_return = log( close/lag( close, order_by = date) ) ) %>% 
  filter(!is.na(c_daily_return) & is.finite(c_daily_return))



sapply(dataset, function(x) sum(is.na(x)))
sapply(dataset, function(x) sum(is.infinite(x)))




#### Data samples

data_2019 <- dataset %>%
  filter(date <= as.Date('2019-12-31'))

data_2020 <- dataset %>% 
  filter(date >= as.Date('2020-01-01') & date <= as.Date('2020-12-31'))

data_2021 <- dataset %>% 
  filter(date >= as.Date('2021-01-01') & date <= as.Date('2021-12-31'))

data_2022 <- dataset %>% 
  filter(date >= as.Date('2022-01-01') )

data_2019_2022 <- dataset 


summary(data_2019)
summary(data_2020)
summary(data_2021)
summary(data_2022)
summary(data_2019_2022)




#daily_market_returns calculation

data_2019 <- data_2019 %>% 
  group_by(date) %>% 
  mutate(m_daily_return = sum(c_daily_return)/n_distinct(class),
         row_number = row_number() )

data_2020 <- data_2020 %>% 
  group_by(date) %>%
  mutate(m_daily_return = sum(c_daily_return)/n_distinct(class),
         row_number = row_number() )

data_2021 <- data_2021 %>% 
  group_by(date) %>% 
  mutate(m_daily_return = sum(c_daily_return)/n_distinct(class),
         row_number = row_number() )

data_2022 <- data_2022 %>% 
  group_by(date) %>% 
  mutate(m_daily_return = sum(c_daily_return)/n_distinct(class),
         row_number = row_number() )

data_2019_2022 <- data_2019_2022 %>% 
  group_by(date) %>% 
  mutate(m_daily_return = sum(c_daily_return)/n_distinct(class),
         row_number = row_number() )


describe(data_2019$m_daily_return)
describe(data_2020$m_daily_return)
describe(data_2021$m_daily_return)
describe(data_2022$m_daily_return)
describe(data_2019_2022$m_daily_return)



####



#### CSSD ####

#cssd calculation

data_2019 <- data_2019 %>%
  group_by(date) %>% 
  mutate(cssd = sqrt( (sum( (c_daily_return - m_daily_return)^2 ))/(n_distinct(class) - 1)  ))


data_2020 <- data_2020 %>%
  group_by(date) %>% 
  mutate(cssd = sqrt( (sum( (c_daily_return - m_daily_return)^2 ))/(n_distinct(class) - 1)  ) )
  
  
data_2021 <- data_2021 %>%
  group_by(date) %>% 
  mutate(cssd = sqrt( (sum( (c_daily_return - m_daily_return)^2 ))/(n_distinct(class) - 1)  ) )


data_2022 <- data_2022 %>%
  group_by(date) %>% 
  mutate(cssd = sqrt( (sum( (c_daily_return - m_daily_return)^2 ))/(n_distinct(class) - 1)  ) )
  

data_2019_2022 <- data_2019_2022 %>%
  group_by(date) %>% 
  mutate(cssd = sqrt( (sum( (c_daily_return - m_daily_return)^2 ))/(n_distinct(class) - 1)  ) )


  

hist(data_2019$m_daily_return)
hist(data_2020$m_daily_return)
hist(data_2021$m_daily_return)
hist(data_2022$m_daily_return)
hist(data_2019_2022$m_daily_return)





#quantile dummy variables

data_2019 <- data_2019 %>%
  mutate(up      = ifelse( m_daily_return >= quantile(data_2019$m_daily_return, 0.95), 1, 0 ), 
         down    = ifelse( m_daily_return <= quantile(data_2019$m_daily_return, 0.05), 1, 0 ),
         up_99   = ifelse( m_daily_return >= quantile(data_2019$m_daily_return, 0.99), 1, 0 ), 
         down_01 = ifelse( m_daily_return <= quantile(data_2019$m_daily_return, 0.01), 1, 0 ))


data_2020 <- data_2020 %>%
  mutate(up      = ifelse( m_daily_return >= quantile(data_2020$m_daily_return, 0.95), 1, 0 ), 
         down    = ifelse( m_daily_return <= quantile(data_2020$m_daily_return, 0.05), 1, 0 ),
         up_99   = ifelse( m_daily_return >= quantile(data_2020$m_daily_return, 0.99), 1, 0 ), 
         down_01 = ifelse( m_daily_return <= quantile(data_2020$m_daily_return, 0.01), 1, 0 ))


data_2021 <- data_2021 %>%
  mutate(up      = ifelse( m_daily_return >= quantile(data_2021$m_daily_return, 0.95), 1, 0 ), 
         down    = ifelse( m_daily_return <= quantile(data_2021$m_daily_return, 0.05), 1, 0 ),
         up_99   = ifelse( m_daily_return >= quantile(data_2021$m_daily_return, 0.99), 1, 0 ), 
         down_01 = ifelse( m_daily_return <= quantile(data_2021$m_daily_return, 0.01), 1, 0 ))


data_2022 <- data_2022 %>%
  mutate(up      = ifelse( m_daily_return >= quantile(data_2022$m_daily_return, 0.95), 1, 0 ), 
         down    = ifelse( m_daily_return <= quantile(data_2022$m_daily_return, 0.05), 1, 0 ),
         up_99   = ifelse( m_daily_return >= quantile(data_2022$m_daily_return, 0.99), 1, 0 ), 
         down_01 = ifelse( m_daily_return <= quantile(data_2022$m_daily_return, 0.01), 1, 0 ))


data_2019_2022 <- data_2019_2022 %>%
  mutate(up      = ifelse( m_daily_return >= quantile(data_2019_2022$m_daily_return, 0.95), 1, 0 ), 
         down    = ifelse( m_daily_return <= quantile(data_2019_2022$m_daily_return, 0.05), 1, 0 ),
         up_99   = ifelse( m_daily_return >= quantile(data_2019_2022$m_daily_return, 0.99), 1, 0 ), 
         down_01 = ifelse( m_daily_return <= quantile(data_2019_2022$m_daily_return, 0.01), 1, 0 ))



#1 Reg
#CSSD_OLS regressing CSSD returns wrt extreme Market returns (up/down)

cssd_up_down_2019 <- lm(cssd ~ up + down, data = data_2019 %>% filter(row_number==1) )
summary(cssd_up_down_2019)

cssd_up_down_2020 <- lm(cssd ~ up + down, data = data_2020 %>% filter(row_number==1) )
summary(cssd_up_down_2020)

cssd_up_down_2021 <- lm(cssd ~ up + down, data = data_2021 %>% filter(row_number==1) )
summary(cssd_up_down_2021)

cssd_up_down_2022 <- lm(cssd ~ up + down, data = data_2022 %>% filter(row_number==1) )
summary(cssd_up_down_2022)

cssd_up_down_2019_2022 <- lm(cssd ~ up + down, data = data_2019_2022 %>% filter(row_number==1) )
summary(cssd_up_down_2019_2022)






#CSSD_OLS regressing CSSD returns wrt extreme Market returns (up_90/down_10)

cssd_up_99_down_01_2019 <- lm(cssd ~ up_99 + down_01, data = data_2019 %>% filter(row_number==1 ) )
summary(cssd_up_99_down_01_2019)

cssd_up_99_down_01_2020 <- lm(cssd ~ up_99 + down_01, data = data_2020 %>% filter(row_number==1 ) )
summary(cssd_up_99_down_01_2020)

cssd_up_99_down_01_2021 <- lm(cssd ~ up_99 + down_01, data = data_2021 %>% filter(row_number==1 ) )
summary(cssd_up_99_down_01_2021)

cssd_up_99_down_01_2022 <- lm(cssd ~ up_99 + down_01, data = data_2022 %>% filter(row_number==1) )
summary(cssd_up_99_down_01_2022)

cssd_up_99_down_01_2019_2022 <- lm(cssd ~ up_99 + down_01, data = data_2019_2022 %>% filter(row_number==1) )
summary(cssd_up_99_down_01_2019_2022)



####




#### CSAD ####

#csad calculation

data_2019 <- data_2019 %>%
  group_by(date) %>% 
  mutate(csad = (sum( abs(c_daily_return - m_daily_return) ) / n_distinct(class) ),
         m_daily_return_sqr = m_daily_return^2,
         down_csad = ifelse(m_daily_return >= 0, 0, 1))

data_2020 <- data_2020 %>%
  group_by(date) %>% 
  mutate(csad = (sum(abs(c_daily_return - m_daily_return)) / n_distinct(class) ),
         m_daily_return_sqr = m_daily_return^2,
         down_csad = ifelse(m_daily_return >= 0, 0, 1))

data_2021 <- data_2021 %>%
  group_by(date) %>% 
  mutate(csad = (sum(abs(c_daily_return - m_daily_return)) / n_distinct(class) ),
         m_daily_return_sqr = m_daily_return^2,
         down_csad = ifelse(m_daily_return >= 0, 0, 1))

data_2022 <- data_2022 %>%
  group_by(date) %>% 
  mutate(csad = (sum(abs(c_daily_return - m_daily_return)) / n_distinct(class) ),
         m_daily_return_sqr = m_daily_return^2,
         down_csad = ifelse(m_daily_return >= 0, 0, 1))

data_2019_2022 <- data_2019_2022 %>%
  group_by(date) %>% 
  mutate(csad = (sum(abs(c_daily_return - m_daily_return)) / n_distinct(class) ),
         m_daily_return_sqr = m_daily_return^2,
         down_csad = ifelse(m_daily_return >= 0, 0, 1))


sapply(data_2019, function(x) sum(is.na(x)))
sapply(data_2020, function(x) sum(is.na(x)))
sapply(data_2021, function(x) sum(is.na(x)))
sapply(data_2022, function(x) sum(is.na(x)))
sapply(data_2019_2022, function(x) sum(is.na(x)))



#2 Reg
#CSAD_OLS regressing CSAD returns wrt Market returns

csad_2019 <- lm(csad ~ m_daily_return + I( abs(m_daily_return) ) + m_daily_return_sqr, data = data_2019 %>% filter(row_number==1 ) )
summary(csad_2019)

csad_2020 <- lm(csad ~ m_daily_return + I( abs(m_daily_return) ) + m_daily_return_sqr, data = data_2020 %>% filter(row_number==1 ) )
summary(csad_2020)

csad_2021 <- lm(csad ~ m_daily_return + I( abs(m_daily_return) ) + m_daily_return_sqr, data = data_2021 %>% filter(row_number==1 ) ) 
summary(csad_2021)

csad_2022 <- lm(csad ~ m_daily_return + I( abs(m_daily_return) ) + m_daily_return_sqr, data = data_2022 %>% filter(row_number==1 ) )
summary(csad_2022)

csad_2019_2022 <- lm(csad ~ m_daily_return + I( abs(m_daily_return) ) + m_daily_return_sqr, data = data_2019_2022 %>% filter(row_number==1 ) %>% arrange(desc(date) ) )
summary(csad_2019_2022)
coeftest(csad_2019_2022, vcov=NeweyWest(csad_2019_2022, verbose=T))


  
#3 Reg
#CSAD_OLS regressing CSAD returns wrt Market returns and extremes (up/down)

csad_up_down_2019 <- lm(csad ~ I( (1-down_csad)*m_daily_return ) + I( down_csad*m_daily_return ) 
                             + I( (1-down_csad)*m_daily_return_sqr ) + I( down_csad*m_daily_return_sqr ), 
                               data = data_2019 %>% filter(row_number==1 ) )
summary(csad_up_down_2019)

csad_up_down_2020 <- lm(csad ~ I( (1-down_csad)*m_daily_return ) + I( down_csad*m_daily_return ) 
                             + I( (1-down_csad)*m_daily_return_sqr ) + I( down_csad*m_daily_return_sqr ), 
                               data = data_2020 %>% filter(row_number==1 ) )
summary(csad_up_down_2020)

csad_up_down_2021 <- lm(csad ~ I( (1-down_csad)*m_daily_return ) + I( down_csad*m_daily_return ) 
                             + I( (1-down_csad)*m_daily_return_sqr ) + I( down_csad*m_daily_return_sqr ), 
                               data = data_2021 %>% filter(row_number==1 ) )
summary(csad_up_down_2021)

csad_up_down_2022 <- lm(csad ~ I( (1-down_csad)*m_daily_return ) + I( down_csad*m_daily_return ) 
                             + I( (1-down_csad)*m_daily_return_sqr ) + I( down_csad*m_daily_return_sqr ), 
                               data = data_2022 %>% filter(row_number==1 ) )
summary(csad_up_down_2022)

csad_up_down_2019_2022 <- lm(csad ~ I( (1-down_csad)*m_daily_return ) + I( down_csad*m_daily_return ) 
                                  + I( (1-down_csad)*m_daily_return_sqr ) + I( down_csad*m_daily_return_sqr ), 
                                    data = data_2019_2022 %>% filter(row_number==1 ) )
summary(csad_up_down_2019_2022)




#4 Reg 
#CSAD with respect to market returns and COVID period

data_2020 <- data_2020 %>% 
  mutate( d_covid = ifelse( between( date, as.Date('2020-02-01'), as.Date('2020-04-30') ), 1, 0) )




data_2019_2022 <- data_2019_2022 %>% 
  mutate( d_covid = ifelse( between( date, as.Date('2020-02-01'), as.Date('2020-04-30') ), 1, 0) )




csad_2020_covid <- lm(csad ~ m_daily_return + I( abs(m_daily_return) ) + 
                             m_daily_return_sqr + I( d_covid*m_daily_return_sqr ), 
                             data = data_2020 %>% filter(row_number==1 ) )
summary(csad_2020_covid)


csad_2021_covid <- lm(csad ~ m_daily_return + I( abs(m_daily_return) ) + 
                             m_daily_return_sqr + I( d_covid*m_daily_return_sqr ), 
                             data = data_2021 %>% filter(row_number==1 ) )
summary(csad_2021_covid)


csad_2022_covid <- lm(csad ~ m_daily_return + I( abs(m_daily_return) ) + 
                             m_daily_return_sqr + I( d_covid*m_daily_return_sqr ), 
                             data = data_2022 %>% filter(row_number==1 ) )
summary(csad_2022_covid)


csad_2019_2022_covid <- lm(csad ~ m_daily_return + I( abs(m_daily_return) ) + 
                                  m_daily_return_sqr + I( d_covid*m_daily_return_sqr ),
                                  data = data_2019_2022 %>% filter(row_number==1 ) )
summary(csad_2019_2022_covid)





#CSAD_OLS regressing CSAD returns wrt Market returns and extremes (up/down) by big and small coins

#sample for big and small coins
big_coins = c("bitcoin", "ethereum", "tether", "usd-coin", "binancecoin", "ripple", "binance-usd", "cardano", "solana", "dogecoin")


data_2019 <- data_2019 %>%
  group_by(date) %>%
  mutate(big = ifelse(class %in% big_coins, 1, 0)) %>% 
  mutate(m_daily_return_big        = ifelse(big == 1, mean(c_daily_return)/n_distinct(class), 0),
         m_daily_return_small      = ifelse(big == 0, mean(c_daily_return)/n_distinct(class), 0), 
         csad_big                  = ifelse(big == 1, abs( c_daily_return - m_daily_return_big)   / n_distinct(class), 0),
         csad_small                = ifelse(big == 0, abs( c_daily_return - m_daily_return_small) / n_distinct(class), 0),
         m_daily_return_sqr_big    = ifelse(big == 1, m_daily_return_big^2,   0),
         m_daily_return_sqr_small  = ifelse(big == 0, m_daily_return_small^2, 0),
         down_csad_big             = ifelse(big == 1  & m_daily_return_big >= 0,   0, 1),
         down_csad_small           = ifelse(big == 0  & m_daily_return_small >= 0, 0, 1))


data_2020 <- data_2020 %>%
  group_by(date) %>%
  mutate(big = ifelse(class %in% big_coins, 1, 0)) %>% 
  mutate(m_daily_return_big        = ifelse(big == 1, mean(c_daily_return)/n_distinct(class), 0),
         m_daily_return_small      = ifelse(big == 0, mean(c_daily_return)/n_distinct(class), 0), 
         csad_big                  = ifelse(big == 1, abs( c_daily_return - m_daily_return_big)   / n_distinct(class), 0),
         csad_small                = ifelse(big == 0, abs( c_daily_return - m_daily_return_small) / n_distinct(class), 0),
         m_daily_return_sqr_big    = ifelse(big == 1, m_daily_return_big^2,   0),
         m_daily_return_sqr_small  = ifelse(big == 0, m_daily_return_small^2, 0),
         down_csad_big             = ifelse(big == 1  & m_daily_return_big >= 0,   0, 1),
         down_csad_small           = ifelse(big == 0  & m_daily_return_small >= 0, 0, 1))


data_2021 <- data_2021 %>%
  group_by(date) %>%
  mutate(big = ifelse(class %in% big_coins, 1, 0)) %>% 
  mutate(m_daily_return_big        = ifelse(big == 1, mean(c_daily_return)/n_distinct(class), 0),
         m_daily_return_small      = ifelse(big == 0, mean(c_daily_return)/n_distinct(class), 0), 
         csad_big                  = ifelse(big == 1, abs( c_daily_return - m_daily_return_big)   / n_distinct(class), 0),
         csad_small                = ifelse(big == 0, abs( c_daily_return - m_daily_return_small) / n_distinct(class), 0),
         m_daily_return_sqr_big    = ifelse(big == 1, m_daily_return_big^2,   0),
         m_daily_return_sqr_small  = ifelse(big == 0, m_daily_return_small^2, 0),
         down_csad_big             = ifelse(big == 1  & m_daily_return_big >= 0,   0, 1),
         down_csad_small           = ifelse(big == 0  & m_daily_return_small >= 0, 0, 1))


data_2022 <- data_2022 %>%
  group_by(date) %>%
  mutate(big = ifelse(class %in% big_coins, 1, 0)) %>%  
  mutate(m_daily_return_big        = ifelse(big == 1, mean(c_daily_return)/n_distinct(class), 0),
         m_daily_return_small      = ifelse(big == 0, mean(c_daily_return)/n_distinct(class), 0), 
         csad_big                  = ifelse(big == 1, abs( c_daily_return - m_daily_return_big)   / n_distinct(class), 0),
         csad_small                = ifelse(big == 0, abs( c_daily_return - m_daily_return_small) / n_distinct(class), 0),
         m_daily_return_sqr_big    = ifelse(big == 1, m_daily_return_big^2,   0),
         m_daily_return_sqr_small  = ifelse(big == 0, m_daily_return_small^2, 0),
         down_csad_big             = ifelse(big == 1  & m_daily_return_big >= 0,   0, 1),
         down_csad_small           = ifelse(big == 0  & m_daily_return_small >= 0, 0, 1))


data_2019_2022 <- data_2019_2022 %>%
  group_by(date) %>%
  mutate(big = ifelse(class %in% big_coins, 1, 0)) %>%  
  mutate(m_daily_return_big        = ifelse(big == 1, mean(c_daily_return)/n_distinct(class), 0),
         m_daily_return_small      = ifelse(big == 0, mean(c_daily_return)/n_distinct(class), 0), 
         csad_big                  = ifelse(big == 1, abs( c_daily_return - m_daily_return_big)   / n_distinct(class), 0),
         csad_small                = ifelse(big == 0, abs( c_daily_return - m_daily_return_small) / n_distinct(class), 0),
         m_daily_return_sqr_big    = ifelse(big == 1, m_daily_return_big^2,   0),
         m_daily_return_sqr_small  = ifelse(big == 0, m_daily_return_small^2, 0),
         down_csad_big             = ifelse(big == 1  & m_daily_return_big >= 0,   0, 1),
         down_csad_small           = ifelse(big == 0  & m_daily_return_small >= 0, 0, 1))







#5 Reg
#CSAD_OLS regressing CSAD returns wrt Market returns and extremes (up/down) by small and big crypto currencies (Adverse herding)

csad_big_small_2019 <- lm(csad_small ~ I( (1-down_csad_small)*m_daily_return_small )    + I( down_csad_small*m_daily_return_small ) 
                                     + I( (1-down_csad_small)*m_daily_return_sqr_small) + I( down_csad_small*m_daily_return_sqr_small )
                                     + csad_big 
                                     + I( (1-down_csad_big)*m_daily_return_sqr_big) + I(down_csad_big*m_daily_return_sqr_big)
                                     , data = data_2019 %>% group_by(date, big) %>% filter(row_number()==1 ) )
summary(csad_big_small_2019)



csad_big_small_2020 <- lm(csad_small ~ I( (1-down_csad_small)*m_daily_return_small )    + I( down_csad_small*m_daily_return_small ) 
                                     + I( (1-down_csad_small)*m_daily_return_sqr_small) + I( down_csad_small*m_daily_return_sqr_small )
                                     + csad_big 
                                     + I( (1-down_csad_big)*m_daily_return_sqr_big) + I(down_csad_big*m_daily_return_sqr_big)
                                     , data = data_2020 %>% group_by(date, big) %>% filter(row_number()==1 ) )
summary(csad_big_small_2020)



csad_big_small_2021 <- lm(csad_small ~ I( (1-down_csad_small)*m_daily_return_small )    + I( down_csad_small*m_daily_return_small ) 
                                     + I( (1-down_csad_small)*m_daily_return_sqr_small) + I( down_csad_small*m_daily_return_sqr_small )
                                     + csad_big 
                                     + I( (1-down_csad_big)*m_daily_return_sqr_big) + I(down_csad_big*m_daily_return_sqr_big)
                                     , data = data_2021 %>% group_by(date, big) %>% filter(row_number()==1 ) )
summary(csad_big_small_2021)



csad_big_small_2022 <- lm(csad_small ~ I( (1-down_csad_small)*m_daily_return_small )    + I( down_csad_small*m_daily_return_small ) 
                                     + I( (1-down_csad_small)*m_daily_return_sqr_small) + I( down_csad_small*m_daily_return_sqr_small )
                                     + csad_big 
                                     + I( (1-down_csad_big)*m_daily_return_sqr_big) + I(down_csad_big*m_daily_return_sqr_big)
                                     , data = data_2022 %>% group_by(date, big) %>% filter(row_number()==1 ) )
summary(csad_big_small_2022)



csad_big_small_2019_2022 <- lm(csad_small ~ I( (1-down_csad_small)*m_daily_return_small )    + I( down_csad_small*m_daily_return_small ) 
                                          + I( (1-down_csad_small)*m_daily_return_sqr_small) + I( down_csad_small*m_daily_return_sqr_small )
                                          + csad_big 
                                          + I( (1-down_csad_big)*m_daily_return_sqr_big) + I(down_csad_big*m_daily_return_sqr_big)
                                          , data = data_2019_2022 %>% group_by(date, big) %>% filter(row_number()==1 ) )
summary(csad_big_small_2019_2022)












#### Consensus ####

#CSSD consensus == pow 

data_2019_2022_pow_2 <- data_2019_2022 %>%
  select(date, close, c_daily_return, consensus, rank, class, symbol) %>% 
  filter( consensus == "pow" ) %>%
  group_by(date) %>% 
  mutate(m_daily_return = sum(c_daily_return)/n_distinct(class),
         cssd = sqrt( (sum( (c_daily_return - m_daily_return)^2 ) )/(n_distinct(class) - 1)  ),  
         csad = (sum( abs(c_daily_return - m_daily_return) ) )/ n_distinct(class),
         down_csad = ifelse(m_daily_return >= 0, 0, 1) )


data_2019_2022_pow <- data_2019_2022_pow_2 %>% 
  mutate(up      = ifelse( m_daily_return >= quantile(data_2019_2022_pow_2$m_daily_return, 0.95), 1, 0 ), 
         down    = ifelse( m_daily_return <= quantile(data_2019_2022_pow_2$m_daily_return, 0.05), 1, 0 ),
         up_99   = ifelse( m_daily_return >= quantile(data_2019_2022_pow_2$m_daily_return, 0.99), 1, 0 ), 
         down_01 = ifelse( m_daily_return <= quantile(data_2019_2022_pow_2$m_daily_return, 0.01), 1, 0 ) ) %>% 
  filter( row_number() == 1 )


#1
cssd_up_down_2019_2022_pow <- lm(cssd ~ up + down, data = data_2019_2022_pow )
summary(cssd_up_down_2019_2022_pow)
        
cssd_up_down_2019_2022_pow_2 <- lm(cssd ~ up_99 + down_01, data = data_2019_2022_pow )
summary(cssd_up_down_2019_2022_pow_2)


#2
csad_2019_2022_pow <- lm(csad ~ m_daily_return + I( abs(m_daily_return) ) + I( m_daily_return^2 ), data = data_2019_2022_pow)
summary(csad_2019_2022_pow)


#3
csad_up_down_2019_2022_pow <- lm(csad ~ I( (1-down_csad)*m_daily_return ) + I( down_csad*m_daily_return ) 
                                      + I( (1-down_csad)*(m_daily_return^2) ) + I( down_csad*(m_daily_return^2) ), 
                                        data = data_2019_2022_pow )
summary(csad_up_down_2019_2022_pow)


#4 covid-19
data_2020_pow <- data_2019_2022_pow %>%
  filter(date >= as.Date('2020-01-01') & date <= as.Date('2020-12-31')) %>%  
  mutate( d_covid = ifelse( between( date, as.Date('2020-02-01'), as.Date('2020-04-30') ), 1, 0) )

data_2019_2022_pow <- data_2019_2022_pow %>% 
  mutate( d_covid = ifelse( between( date, as.Date('2020-02-01'), as.Date('2020-04-30') ), 1, 0) )

csad_2020_covid_pow <- lm(csad ~ m_daily_return + I( abs(m_daily_return) ) + 
                                 I(m_daily_return^2) + I( d_covid*(m_daily_return^2) ), 
                                 data = data_2020_pow )
summary(csad_2020_covid_pow)


csad_2019_2022_covid_pow <- lm(csad ~ m_daily_return + I( abs(m_daily_return) ) + 
                                      I(m_daily_return^2) + I( d_covid*(m_daily_return^2) ), 
                                      data = data_2019_2022_pow )
summary(csad_2019_2022_covid_pow)










#CSSD consensus == pos 

data_2019_2022_pos_2 <- data_2019_2022 %>%
  select(date, close, c_daily_return, consensus, rank, class, symbol) %>% 
  filter( consensus == "pos" ) %>%
  group_by(date) %>% 
  mutate(m_daily_return = sum(c_daily_return)/n_distinct(class),
         cssd = sqrt( (sum( (c_daily_return - m_daily_return)^2 ) )/(n_distinct(class) - 1)  ),  
         csad = (sum( abs(c_daily_return - m_daily_return) ) )/ n_distinct(class),
         down_csad = ifelse(m_daily_return >= 0, 0, 1))


data_2019_2022_pos <- data_2019_2022_pos_2 %>%
  mutate(up      = ifelse( m_daily_return >= quantile(data_2019_2022_pos_2$m_daily_return, 0.95), 1, 0 ), 
         down    = ifelse( m_daily_return <= quantile(data_2019_2022_pos_2$m_daily_return, 0.05), 1, 0 ),
         up_99   = ifelse( m_daily_return >= quantile(data_2019_2022_pos_2$m_daily_return, 0.99), 1, 0 ), 
         down_01 = ifelse( m_daily_return <= quantile(data_2019_2022_pos_2$m_daily_return, 0.01), 1, 0 ) ) %>% 
  filter( row_number() == 1 )


#1
cssd_up_down_2019_2022_pos <- lm(cssd ~ up + down, data = data_2019_2022_pos )
summary(cssd_up_down_2019_2022_pos)

cssd_up_down_2019_2022_pos_2 <- lm(cssd ~ up_99 + down_01, data = data_2019_2022_pos )
summary(cssd_up_down_2019_2022_pos_2)


#2
csad_2019_2022_pos <- lm(csad ~ m_daily_return + I( abs(m_daily_return) ) + I( m_daily_return^2 ), data = data_2019_2022_pos)
summary(csad_2019_2022_pos)


#3
csad_up_down_2019_2022_pos <- lm(csad ~ I( (1-down_csad)*m_daily_return ) + I( down_csad*m_daily_return ) 
                                 + I( (1-down_csad)*(m_daily_return^2) ) + I( down_csad*(m_daily_return^2) ), 
                                 data = data_2019_2022_pos )
summary(csad_up_down_2019_2022_pos)


#4 covid-19
data_2020_pos <- data_2019_2022_pos %>%
  filter(date >= as.Date('2020-01-01') & date <= as.Date('2020-12-31')) %>%  
  mutate( d_covid = ifelse( between( date, as.Date('2020-02-01'), as.Date('2020-04-30') ), 1, 0) )

data_2019_2022_pos <- data_2019_2022_pos %>% 
  mutate( d_covid = ifelse( between( date, as.Date('2020-02-01'), as.Date('2020-04-30') ), 1, 0) )

csad_2020_covid_pos <- lm(csad ~ m_daily_return + I( abs(m_daily_return) ) + 
                                 I(m_daily_return^2) + I( d_covid*(m_daily_return^2) ), 
                                 data = data_2020_pos )
summary(csad_2020_covid_pos)


csad_2019_2022_covid_pos <- lm(csad ~ m_daily_return + I( abs(m_daily_return) ) + 
                                      I(m_daily_return^2) + I( d_covid*(m_daily_return^2) ), 
                                      data = data_2019_2022_pos )
summary(csad_2019_2022_covid_pos)










#CSSD consensus == token 

data_2019_2022_tok_2 <- data_2019_2022 %>%
  select(date, close, c_daily_return, consensus, rank, class, symbol) %>% 
  filter( consensus == "token" ) %>%
  group_by(date) %>% 
  mutate(m_daily_return = sum(c_daily_return)/n_distinct(class),
         cssd = sqrt( (sum( (c_daily_return - m_daily_return)^2 ) )/(n_distinct(class) - 1)  ),  
         csad = (sum( abs(c_daily_return - m_daily_return) ) )/ n_distinct(class),
         down_csad = ifelse(m_daily_return >= 0, 0, 1))


data_2019_2022_tok <- data_2019_2022_tok_2 %>%
  mutate(up      = ifelse( m_daily_return >= quantile(data_2019_2022_tok_2$m_daily_return, 0.95), 1, 0 ), 
         down    = ifelse( m_daily_return <= quantile(data_2019_2022_tok_2$m_daily_return, 0.05), 1, 0 ),
         up_99   = ifelse( m_daily_return >= quantile(data_2019_2022_tok_2$m_daily_return, 0.99), 1, 0 ), 
         down_01 = ifelse( m_daily_return <= quantile(data_2019_2022_tok_2$m_daily_return, 0.01), 1, 0 ) ) %>% 
  filter( row_number() == 1 )


#1
cssd_up_down_2019_2022_tok <- lm(cssd ~ up + down, data = data_2019_2022_tok )
summary(cssd_up_down_2019_2022_tok)

cssd_up_down_2019_2022_tok_2 <- lm(cssd ~ up_99 + down_01, data = data_2019_2022_tok )
summary(cssd_up_down_2019_2022_tok_2)


#2
csad_2019_2022_tok <- lm(csad ~ m_daily_return + I( abs(m_daily_return) ) + I( m_daily_return^2 ), data = data_2019_2022_tok)
summary(csad_2019_2022_tok)


#3
csad_up_down_2019_2022_tok <- lm(csad ~ I( (1-down_csad)*m_daily_return ) + I( down_csad*m_daily_return ) +
                                        I( (1-down_csad)*(m_daily_return^2) ) + I( down_csad*(m_daily_return^2) ), 
                                        data = data_2019_2022_tok )
summary(csad_up_down_2019_2022_tok)


#4 covid-19
data_2020_tok <- data_2019_2022_tok %>%
  filter(date >= as.Date('2020-01-01') & date <= as.Date('2020-12-31')) %>%  
  mutate( d_covid = ifelse( between( date, as.Date('2020-02-01'), as.Date('2020-04-30') ), 1, 0) )

data_2019_2022_tok <- data_2019_2022_tok %>% 
  mutate( d_covid = ifelse( between( date, as.Date('2020-02-01'), as.Date('2020-04-30') ), 1, 0) )

csad_2020_covid_tok <- lm(csad ~ m_daily_return + I( abs(m_daily_return) ) + 
                                 I(m_daily_return^2) + I( d_covid*(m_daily_return^2) ), 
                                 data = data_2020_tok )
summary(csad_2020_covid_tok)


csad_2019_2022_covid_tok <- lm(csad ~ m_daily_return + I( abs(m_daily_return) ) + 
                                 I(m_daily_return^2) + I( d_covid*(m_daily_return^2) ), 
                               data = data_2019_2022_tok )
summary(csad_2019_2022_covid_tok)








###







#### Robustness analysis by cap-weights ####


#recreating variables based on weighted market returns

data_2019 <- data_2019 %>% 
  group_by(date) %>%
  mutate(m_daily_return_w = sum(c_daily_return * (market_cap/sum(market_cap)) ),
         cssd_w = sqrt( (sum( (c_daily_return - m_daily_return_w)^2 ))/(n_distinct(class) - 1)  ),
         up_w = ifelse( m_daily_return_w >= quantile(data_2019$m_daily_return_w, 0.95), 1, 0 ), 
         down_w = ifelse( m_daily_return_w <= quantile(data_2019$m_daily_return_w, 0.05), 1, 0 ),
         csad_w = (sum(abs(c_daily_return - m_daily_return_w)) / n_distinct(class) ),
         down_csad_w = ifelse(m_daily_return_w >= 0, 0, 1))


data_2020 <- data_2020 %>% 
  group_by(date) %>%
  mutate(m_daily_return_w = sum(c_daily_return * (market_cap/sum(market_cap)) ), 
         cssd_w = sqrt( (sum( (c_daily_return - m_daily_return_w)^2 ))/(n_distinct(class) - 1)  ),
         up_w = ifelse( m_daily_return_w >= quantile(data_2020$m_daily_return_w, 0.95), 1, 0 ), 
         down_w = ifelse( m_daily_return_w <= quantile(data_2020$m_daily_return_w, 0.05), 1, 0 ),
         csad_w = (sum(abs(c_daily_return - m_daily_return_w)) / n_distinct(class) ),
         down_csad_w = ifelse(m_daily_return_w >= 0, 0, 1))


data_2021 <- data_2021 %>% 
  group_by(date) %>% 
  mutate(m_daily_return_w = sum(c_daily_return * (market_cap/sum(market_cap)) ),
         cssd_w = sqrt( (sum( (c_daily_return - m_daily_return_w)^2 ))/(n_distinct(class) - 1)  ),
         up_w = ifelse( m_daily_return_w >= quantile(data_2021$m_daily_return_w, 0.95), 1, 0 ), 
         down_w = ifelse( m_daily_return_w <= quantile(data_2021$m_daily_return_w, 0.05), 1, 0 ),
         csad_w = (sum(abs(c_daily_return - m_daily_return_w)) / n_distinct(class) ),
         down_csad_w = ifelse(m_daily_return_w >= 0, 0, 1))


data_2022 <- data_2022 %>% 
  group_by(date) %>% 
  mutate(m_daily_return_w = sum(c_daily_return * (market_cap/sum(market_cap)) ),
         cssd_w = sqrt( (sum( (c_daily_return - m_daily_return_w)^2 ))/(n_distinct(class) - 1)  ),
         up_w = ifelse( m_daily_return_w >= quantile(data_2022$m_daily_return_w, 0.95), 1, 0 ), 
         down_w = ifelse( m_daily_return_w <= quantile(data_2022$m_daily_return_w, 0.05), 1, 0 ),
         csad_w = (sum(abs(c_daily_return - m_daily_return_w)) / n_distinct(class) ),
         down_csad_w = ifelse(m_daily_return_w >= 0, 0, 1))


data_2019_2022 <- data_2019_2022 %>% 
  group_by(date) %>% 
  mutate(m_daily_return_w = sum(c_daily_return * (market_cap/sum(market_cap)) ),
         cssd_w = sqrt( (sum( (c_daily_return - m_daily_return_w)^2 ))/(n_distinct(class) - 1)  ),
         up_w = ifelse( m_daily_return_w >= quantile(data_2019_2022$m_daily_return_w, 0.95), 1, 0 ), 
         down_w = ifelse( m_daily_return_w <= quantile(data_2019_2022$m_daily_return_w, 0.05), 1, 0 ),
         csad_w = (sum(abs(c_daily_return - m_daily_return_w)) / n_distinct(class) ),
         down_csad_w = ifelse(m_daily_return_w >= 0, 0, 1))



#1 Reg for robustness check
#CSSD wrt cap-weighted market extremes (up/down)

cssd_up_down_2019_w <- lm(cssd_w ~ up_w + down_w, data = data_2019 %>% filter(row_number==1) )
summary(cssd_up_down_2019_w)

cssd_up_down_2020_w <- lm(cssd_w ~ up_w + down_w, data = data_2020 %>% filter(row_number==1) )
summary(cssd_up_down_2020_w)

cssd_up_down_2021_w <- lm(cssd_w ~ up_w + down_w, data = data_2021 %>% filter(row_number==1))
summary(cssd_up_down_2021_w)

cssd_up_down_2022_w <- lm(cssd_w ~ up_w + down_w, data = data_2022 %>% filter(row_number==1) )
summary(cssd_up_down_2022_w)

cssd_up_down_2019_2022_w <- lm(cssd_w ~ up_w + down_w, data = data_2019_2022 %>% filter(row_number==1) )
summary(cssd_up_down_2019_2022_w)



#2a Reg for robustness check
#CSAD wrt cap-weighted market returns

csad_2019_w <- lm(csad_w ~ m_daily_return_w + I(abs(m_daily_return_w)) + I(m_daily_return_w^2), 
                           data = data_2019 %>% filter(row_number==1) )
summary(csad_2019_w)

csad_2020_w <- lm(csad_w ~ m_daily_return_w + I(abs(m_daily_return_w)) + I(m_daily_return_w^2), 
                           data = data_2020 %>% filter(row_number==1) )
summary(csad_2020_w)

csad_2021_w <- lm(csad_w ~ m_daily_return_w + I(abs(m_daily_return_w)) + I(m_daily_return_w^2), 
                           data = data_2021 %>% filter(row_number==1) )
summary(csad_2021_w)

csad_2022_w <- lm(csad_w ~ m_daily_return_w + I(abs(m_daily_return_w)) + I(m_daily_return_w^2), 
                           data = data_2022 %>% filter(row_number==1) )
summary(csad_2022_w)

csad_2019_2022_w <- lm(csad_w ~ m_daily_return_w + I(abs(m_daily_return_w)) + I(m_daily_return_w^2), data = data_2019_2022 )
summary(csad_2019_2022_w)



#2b Reg for robustness check
#CSAD wrt cap-weighted market returns and up/downs and covid-19

csad_2019_2022_covid_w <- lm(csad ~ m_daily_return_w + I( abs(m_daily_return_w) ) + 
                                    I(m_daily_return_w^2) + I( d_covid*(m_daily_return_w^2) ),
                                    data = data_2019_2022 %>% filter(row_number==1 ) )
summary(csad_2019_2022_covid_w)



#3 Reg for robustness check
#CSAD wrt cap-weighted market returns and up/downs 

csad_up_down_2019_w <- lm(csad_w ~ I((1-down_csad_w)*m_daily_return_w)   + I(down_csad_w*m_daily_return_w) + 
                                   I((1-down_csad_w)*m_daily_return_w^2) + I(down_csad_w*m_daily_return_w^2), 
                                   data = data_2019 %>% filter(row_number==1) )
summary(csad_up_down_2019_w)


csad_up_down_2020_w <- lm(csad_w ~ I((1-down_csad_w)*m_daily_return_w)   + I(down_csad_w*m_daily_return_w) + 
                                   I((1-down_csad_w)*m_daily_return_w^2) + I(down_csad_w*m_daily_return_w^2), 
                                   data = data_2020 %>% filter(row_number==1) )
summary(csad_up_down_2020_w)


csad_up_down_2021_w <- lm(csad_w ~ I((1-down_csad_w)*m_daily_return_w)   + I(down_csad_w*m_daily_return_w) + 
                                   I((1-down_csad_w)*m_daily_return_w^2) + I(down_csad_w*m_daily_return_w^2), 
                                   data = data_2021 %>% filter(row_number==1) )
summary(csad_up_down_2021_w)


csad_up_down_2022_w <- lm(csad_w ~ I((1-down_csad_w)*m_daily_return_w)   + I(down_csad_w*m_daily_return_w) + 
                                   I((1-down_csad_w)*m_daily_return_w^2) + I(down_csad_w*m_daily_return_w^2), 
                                   data = data_2022 %>% filter(row_number==1) )
summary(csad_up_down_2022_w)


csad_up_down_2019_2022_w <- lm(csad_w ~ I((1-down_csad_w)*m_daily_return_w)   + I(down_csad_w*m_daily_return_w) + 
                                        I((1-down_csad_w)*m_daily_return_w^2) + I(down_csad_w*m_daily_return_w^2), 
                                        data = data_2019_2022 %>% filter(row_number==1) )
summary(csad_up_down_2019_2022_w)




#CSAD wrt cap-weighted market returns without bitcoin and up/downs 

#market without bitcoin 


data_2019 <- data_2019 %>%
  group_by(date) %>%
  mutate(btc = ifelse(class == "bitcoin", 1, 0),
         no_btc = ifelse(class != "bitcoin", 1, 0) ) %>% 
  mutate(m_daily_return_nb        = sum( c_daily_return * (market_cap/sum(market_cap*no_btc) ) ) ,
         csad_nb                  = no_btc * sum( abs( c_daily_return - m_daily_return_nb) )   / n_distinct(class),
         m_daily_return_sqr_nb    = no_btc * m_daily_return_nb^2,
         down_csad_nb             = ifelse( btc == 1  | m_daily_return_nb >= 0, 0, 1 ),
         btc_daily_return         = btc * c_daily_return,
         btc_daily_return_sqr     = btc * ( c_daily_return )^2,
         down_csad_btc            = ifelse( btc_daily_return >= 0, 0, 1) )
         

data_2020 <- data_2020 %>%
  group_by(date) %>%
  mutate(btc = ifelse(class == "bitcoin", 1, 0),
         no_btc = ifelse(class != "bitcoin", 1, 0) ) %>% 
  mutate(m_daily_return_nb        = sum( c_daily_return * (market_cap/sum(market_cap*no_btc) ) ) ,
         csad_nb                  = no_btc * sum( abs( c_daily_return - m_daily_return_nb) )   / n_distinct(class),
         m_daily_return_sqr_nb    = no_btc * m_daily_return_nb^2,
         down_csad_nb             = ifelse( btc == 1  | m_daily_return_nb >= 0, 0, 1 ),
         btc_daily_return         = btc * c_daily_return,
         btc_daily_return_sqr     = btc * ( c_daily_return )^2,
         down_csad_btc            = ifelse( btc_daily_return >= 0, 0, 1) )


data_2021 <- data_2021 %>%
  group_by(date) %>%
  mutate(btc = ifelse(class == "bitcoin", 1, 0),
         no_btc = ifelse(class != "bitcoin", 1, 0) ) %>% 
  mutate(m_daily_return_nb        = sum( c_daily_return * (market_cap/sum(market_cap*no_btc) ) ) ,
         csad_nb                  = no_btc * sum( abs( c_daily_return - m_daily_return_nb) )   / n_distinct(class),
         m_daily_return_sqr_nb    = no_btc * m_daily_return_nb^2,
         down_csad_nb             = ifelse( btc == 1  | m_daily_return_nb >= 0, 0, 1 ),
         btc_daily_return         = btc * c_daily_return,
         btc_daily_return_sqr     = btc * ( c_daily_return )^2,
         down_csad_btc            = ifelse( btc_daily_return >= 0, 0, 1) )


data_2022 <- data_2022 %>%
  group_by(date) %>%
  mutate(btc = ifelse(class == "bitcoin", 1, 0),
         no_btc = ifelse(class != "bitcoin", 1, 0) ) %>% 
  mutate(m_daily_return_nb        = sum( c_daily_return * (market_cap/sum(market_cap*no_btc) ) ) ,
         csad_nb                  = no_btc * sum( abs( c_daily_return - m_daily_return_nb) )   / n_distinct(class),
         m_daily_return_sqr_nb    = no_btc * m_daily_return_nb^2,
         down_csad_nb             = ifelse( btc == 1  | m_daily_return_nb >= 0, 0, 1 ),
         btc_daily_return         = btc * c_daily_return,
         btc_daily_return_sqr     = btc * ( c_daily_return )^2,
         down_csad_btc            = ifelse( btc_daily_return >= 0, 0, 1) )


data_2019_2022 <- data_2019_2022 %>%
  group_by(date) %>%
  mutate(btc = ifelse(class == "bitcoin", 1, 0),
         no_btc = ifelse(class != "bitcoin", 1, 0) ) %>% 
  mutate(m_daily_return_nb        = sum( c_daily_return * (market_cap/sum(market_cap*no_btc) ) ) ,
         csad_nb                  = no_btc * sum( abs( c_daily_return - m_daily_return_nb) )   / n_distinct(class),
         m_daily_return_sqr_nb    = no_btc * m_daily_return_nb^2,
         down_csad_nb             = ifelse( btc == 1  | m_daily_return_nb >= 0, 0, 1 ),
         btc_daily_return         = btc * c_daily_return,
         btc_daily_return_sqr     = btc * ( c_daily_return )^2,
         down_csad_btc            = ifelse( btc_daily_return >= 0, 0, 1) )


#4 Reg for robustness check
#CSAD wrt Market returns and extremes (up/down) by small and big crypto currencies

csad_2019_nb_w <- lm(csad_nb ~ I((1-down_csad_nb)*m_daily_return_nb) + I(down_csad_nb*m_daily_return_nb) 
                             + I((1-down_csad_nb)*m_daily_return_sqr_nb) + I(down_csad_nb*m_daily_return_sqr_nb)
                             + I((1-down_csad_btc)*btc_daily_return_sqr) + I(down_csad_btc*btc_daily_return_sqr)
                             , data = data_2019 %>% 
                                      group_by(date, btc) %>% 
                                      mutate( rank_2 = row_number() ) %>% 
                                      filter( rank_2 == 1) )
summary(csad_2019_nb_w)



csad_2020_nb_w <- lm(csad_nb ~ I((1-down_csad_nb)*m_daily_return_nb) + I(down_csad_nb*m_daily_return_nb) 
                             + I((1-down_csad_nb)*m_daily_return_sqr_nb) + I(down_csad_nb*m_daily_return_sqr_nb)
                             + I((1-down_csad_btc)*btc_daily_return_sqr) + I(down_csad_btc*btc_daily_return_sqr)
                             , data = data_2020 %>% 
                                        group_by(date, btc) %>% 
                                        mutate( rank_2 = row_number() ) %>% 
                                        filter( rank_2 == 1) )
summary(csad_2020_nb_w)


csad_2021_nb_w <- lm(csad_nb ~ I((1-down_csad_nb)*m_daily_return_nb) + I(down_csad_nb*m_daily_return_nb) 
                             + I((1-down_csad_nb)*m_daily_return_sqr_nb) + I(down_csad_nb*m_daily_return_sqr_nb)
                             + I((1-down_csad_btc)*btc_daily_return_sqr) + I(down_csad_btc*btc_daily_return_sqr)
                             , data = data_2021 %>% 
                                         group_by(date, btc) %>% 
                                         mutate( rank_2 = row_number() ) %>% 
                                         filter( rank_2 == 1) )
summary(csad_2021_nb_w)


csad_2022_nb_w <- lm(csad_nb ~ I((1-down_csad_nb)*m_daily_return_nb) + I(down_csad_nb*m_daily_return_nb) 
                             + I((1-down_csad_nb)*m_daily_return_sqr_nb) + I(down_csad_nb*m_daily_return_sqr_nb)
                             + I((1-down_csad_btc)*btc_daily_return_sqr) + I(down_csad_btc*btc_daily_return_sqr)
                             , data = data_2022 %>% 
                                         group_by(date, btc) %>% 
                                         mutate( rank_2 = row_number() ) %>% 
                                         filter( rank_2 == 1) )
summary(csad_2022_nb_w)


csad_2019_2022_nb_w <- lm(csad_nb ~ I((1-down_csad_nb)*m_daily_return_nb) + I(down_csad_nb*m_daily_return_nb) 
                                  + I((1-down_csad_nb)*m_daily_return_sqr_nb) + I(down_csad_nb*m_daily_return_sqr_nb)
                                  + I((1-down_csad_btc)*btc_daily_return_sqr) + I(down_csad_btc*btc_daily_return_sqr)
                                  , data = data_2019 %>% 
                                        group_by(date, btc) %>% 
                                        mutate( rank_2 = row_number() ) %>% 
                                        filter( rank_2 == 1) )
summary(csad_2019_2022_nb_w)



####




#### Plots ####

#Descriptive summary statistics 

data_2019$dataset = '2019'
data_2020$dataset = '2020'
data_2021$dataset = '2021'
data_2022$dataset = '2022'
data_2019_2022$dataset = '2019_2022'

descriptive_df <- bind_rows(data_2019, data_2020, data_2021, data_2022, data_2019_2022) %>% 
  group_by( dataset ) %>%    
  summarise(mean = round(mean(m_daily_return), 7),
            median = round(median(m_daily_return), 7),
            std_dev = round(sd(m_daily_return), 7),
            kurtosis = round(kurtosis(m_daily_return), 7),
            skewness = round(skewness(m_daily_return), 7),
            min = round(min(m_daily_return), 7),
            max = round(max(m_daily_return), 7),
            c_count = n_distinct(class) )

descriptive_df <- data.frame(descriptive_df)
descriptive_df

write.table(descriptive_df, file = "sumstats.txt", sep = ",", quote = FALSE, row.names = F)




#Descriptive summary statistics CSSD 

descriptive_df_cssd <- bind_rows(data_2019, data_2020, data_2021, data_2022, data_2019_2022) %>% 
  group_by( dataset ) %>%    
  summarise(mean = round(mean(cssd), 7),
            median = round(median(cssd), 7),
            std_dev = round(sd(cssd), 7),
            kurtosis = round(kurtosis(cssd), 7),
            skewness = round(skewness(cssd), 7),
            min = round(min(cssd), 7),
            max = round(max(cssd), 7) )

descriptive_df_cssd <- data.frame(descriptive_df_cssd)
descriptive_df_cssd

write.table(descriptive_df_cssd, file = "sumstats_cssd.txt", sep = ",", quote = FALSE, row.names = F)





#Descriptive summary statistics CSAD 

descriptive_df_csad <- bind_rows(data_2019, data_2020, data_2021, data_2022, data_2019_2022) %>% 
  group_by( dataset ) %>%    
  summarise(mean = round(mean(csad), 7),
            median = round(median(csad), 7),
            std_dev = round(sd(csad), 7),
            kurtosis = round(kurtosis(csad), 7),
            skewness = round(skewness(csad), 7),
            min = round(min(csad), 7),
            max = round(max(csad), 7) )

descriptive_df_csad <- data.frame(descriptive_df_csad)
descriptive_df_csad

write.table(descriptive_df_csad, file = "sumstats_csad.txt", sep = ",", quote = FALSE, row.names = F)





#Descriptive plot
 
attach(mtcars)
par(mfrow=c(2,3))
hist(data_2019$m_daily_return, col="light blue", main="Daily market returns in 2019", xlab="m_daily_return" )
hist(data_2020$m_daily_return, col="light blue", main="Daily market returns in 2020", xlab="m_daily_return" )
hist(data_2021$m_daily_return, col="light blue", main="Daily market returns in 2021", xlab="m_daily_return" )
hist(data_2022$m_daily_return, col="light blue", main="Daily market returns in 2022", xlab="m_daily_return" )
hist(data_2019_2022$m_daily_return, col="light blue", main="Daily market returns in 2019-2022", xlab="m_daily_return" )




coins_over_time <- data_2019_2022 %>%
  group_by(date) %>% 
  summarise(number_of_coins = n_distinct(class)) %>% 
  ggplot(aes(x = date, y = number_of_coins)) + 
  geom_line(aes(colour="Number of coins"), size =1) + 
  labs(title = "Number of coins in the dataset over time", x = "Date", y = "Number of coins") + 
  theme(plot.title = element_text(hjust = 0.5))

coins_over_time




data_2019_2022 %>%
  ggplot(aes(x = date)) +  
  geom_line(aes(y=m_daily_return), size =1) +
  geom_line(aes(y=csad), size =1)
  

  labs(title = "Number of coins in the dataset over time", x = "Date", y = "Number of coins") + 
  theme(plot.title = element_text(hjust = 0.5))







#Scatter plot for CSSD and market returns

scat_2019 <- data_2019 %>%
  filter(row_number==1 ) %>% 
  ggplot(aes(x = m_daily_return, y = cssd )) +
  geom_point(color="dark green") +
  labs(title = "2019", x = "Market daily returns", y = "CSSD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, colour = "grey")
  

scat_2020 <- data_2020 %>%
  filter(row_number==1 ) %>% 
  ggplot(aes(x = m_daily_return, y = cssd )) +
  geom_point(color="dark green") +
  labs(title = "2020", x = "Market daily returns", y = "CSSD") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept = 0, colour = "grey")


scat_2021 <- data_2021 %>%
  filter(row_number==1 ) %>% 
  ggplot(aes(x = m_daily_return, y = cssd )) +
  geom_point(color="dark green") +
  labs(title = "2021", x = "Market daily returns", y = "CSSD") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept = 0, colour = "grey")


scat_2022 <- data_2022 %>%
  filter(row_number==1 ) %>% 
  ggplot(aes(x = m_daily_return, y = cssd )) +
  geom_point(color="dark green") +
  labs(title = "2022", x = "Market daily returns", y = "CSSD") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept = 0, colour = "grey")


scat_2019_2022 <- data_2019_2022 %>%
  filter(row_number==1 ) %>% 
  ggplot(aes(x = m_daily_return, y = cssd )) +
  geom_point(color="dark green") +
  labs(title = "2019-2022", x = "Market daily returns", y = "CSSD") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept = 0, colour = "grey")



scatter_plots <- ggarrange(scat_2019, scat_2020, scat_2021, scat_2022, scat_2019_2022, 
                           ncol = 3, nrow = 2)

scatter_plots


#Scatterplot for CSAD and daily market returns

scat_2019_csad <- data_2019 %>%
  filter(row_number==1 ) %>% 
  ggplot(aes(x = m_daily_return, y = csad )) +
  geom_point(color="dark green") +
  labs(title = "2019", x = "Market daily returns", y = "CSAD") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_vline(xintercept = 0, colour = "grey")


scat_2020_csad <- data_2020 %>%
  filter(row_number==1 ) %>% 
  ggplot(aes(x = m_daily_return, y = csad )) +
  geom_point(color="dark green") +
  labs(title = "2020", x = "Market daily returns", y = "CSAD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, colour = "grey")


scat_2021_csad <- data_2021 %>%
  filter(row_number==1 ) %>% 
  ggplot(aes(x = m_daily_return, y = csad )) +
  geom_point(color="dark green") +
  labs(title = "2021", x = "Market daily returns", y = "CSAD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, colour = "grey")


scat_2022_csad <- data_2022 %>% 
  filter(row_number==1 ) %>% 
  ggplot(aes(x = m_daily_return, y = csad )) +
  geom_point(color="dark green") +
  labs(title = "2022", x = "Market daily returns", y = "CSAD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, colour = "grey")


scat_2019_2022_csad <- data_2019_2022 %>% 
  filter(row_number==1 ) %>% 
  ggplot(aes(x = m_daily_return, y = csad )) +
  geom_point(color="dark green") +
  labs(title = "2019-2022", x = "Market daily returns", y = "CSAD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, colour = "grey")



scatter_plots_csad <- ggarrange(scat_2019_csad, scat_2020_csad, scat_2021_csad,
                                scat_2022_csad, scat_2019_2022_csad, 
                                ncol = 3, nrow = 2)

scatter_plots_csad




#covid-19 scatterplot

scat_csad_2020_covid <- data_2020 %>%
  filter(row_number==1 ) %>%
  mutate(d_covid_period = ifelse(d_covid == 1, 'covid', 'non-covid') ) %>% 
  ggplot(aes(x = m_daily_return, y = csad, color = d_covid_period )) +
  geom_point() +
  scale_color_manual(values = c("covid" = "purple", "non-covid" = "dark green" )) +
  labs(title = "2020  CSAD", x = "Market daily returns", y = "CSAD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, colour = "grey")


scat_csad_2019_2022_covid <- data_2019_2022 %>%
  filter(row_number==1 ) %>%
  mutate(d_covid_period = ifelse(d_covid == 1, 'covid', 'non-covid') ) %>% 
  ggplot(aes(x = m_daily_return, y = csad, color = d_covid_period )) +
  geom_point() +
  scale_color_manual(values = c("covid" = "purple", "non-covid" = "dark green" )) +
  labs(title = "2019-2020  CSAD", x = "Market daily returns", y = "CSAD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, colour = "grey")






scat_cssd_2020_covid <- data_2020 %>%
  filter(row_number==1 ) %>%
  mutate(d_covid_period = ifelse(d_covid == 1, 'covid', 'non-covid') ) %>% 
  ggplot(aes(x = m_daily_return, y = cssd, color = d_covid_period )) +
  geom_point() +
  scale_color_manual(values = c("covid" = "purple", "non-covid" = "dark green" )) +
  labs(title = "2020  CSSD", x = "Market daily returns", y = "CSSD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, colour = "grey")


scat_cssd_2019_2022_covid <- data_2019_2022 %>%
  filter(row_number==1 ) %>%
  mutate(d_covid_period = ifelse(d_covid == 1, 'covid', 'non-covid') ) %>% 
  ggplot(aes(x = m_daily_return, y = cssd, color = d_covid_period )) +
  geom_point() +
  scale_color_manual(values = c("covid" = "purple", "non-covid" = "dark green" )) +
  labs(title = "2019-2020  CSSD", x = "Market daily returns", y = "CSSD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, colour = "grey")






scatter_plots_cssd_covid <- ggarrange(scat_cssd_2020_covid, scat_cssd_2019_2022_covid,
                                      scat_csad_2020_covid, scat_csad_2019_2022_covid,
                                      ncol = 2, nrow = 2)

scatter_plots_cssd_covid





####








#### Consensus plots ####


#Descriptive summary statistics by consensus 2019-2022



descriptive_df_consensus <- bind_rows(data_2019_2022_pow_2, data_2019_2022_pos_2, data_2019_2022_tok_2 ) %>% 
  group_by( consensus ) %>%
  summarise(mean = round(mean(m_daily_return), 7),
            median = round(median(m_daily_return), 7),
            std_dev = round(sd(m_daily_return), 7),
            kurtosis = round(kurtosis(m_daily_return), 7),
            skewness = round(skewness(m_daily_return), 7),
            min = round(min(m_daily_return), 7),
            max = round(max(m_daily_return), 7),
            c_count = n_distinct(class) )

nrow(data_2019_2022_pow)
nrow(data_2019_2022_pos)
nrow(data_2019_2022_tok)


descriptive_df_consensus <- data.frame(descriptive_df_consensus)
descriptive_df_consensus

write.table(descriptive_df_consensus, file = "sumstats_consensus.txt", sep = ",", quote = FALSE, row.names = F)





#Descriptive summary statistics CSSD CSAD 

descriptive_df_cssd_consensus <- bind_rows(data_2019_2022_pow, data_2019_2022_pos, data_2019_2022_tok) %>% 
  group_by( consensus ) %>%    
  summarise(mean = round(mean(cssd), 7),
            median = round(median(cssd), 7),
            std_dev = round(sd(cssd), 7),
            kurtosis = round(kurtosis(cssd), 7),
            skewness = round(skewness(cssd), 7),
            min = round(min(cssd), 7),
            max = round(max(cssd), 7) )

descriptive_df_cssd_consensus <- data.frame(descriptive_df_cssd_consensus)
descriptive_df_cssd_consensus

write.table(descriptive_df_cssd_consensus, file = "sumstats_cssd_consensus.txt", sep = ",", quote = FALSE, row.names = F)





descriptive_df_csad_consensus <- bind_rows(data_2019_2022_pow, data_2019_2022_pos, data_2019_2022_tok) %>% 
  group_by( consensus ) %>%    
  summarise(mean = round(mean(csad), 7),
            median = round(median(csad), 7),
            std_dev = round(sd(csad), 7),
            kurtosis = round(kurtosis(csad), 7),
            skewness = round(skewness(csad), 7),
            min = round(min(csad), 7),
            max = round(max(csad), 7) )

descriptive_df_csad_consensus <- data.frame(descriptive_df_csad_consensus)
descriptive_df_csad_consensus

write.table(descriptive_df_csad_consensus, file = "sumstats_csad_consensus.txt", sep = ",", quote = FALSE, row.names = F)




#Consensus Histograms

attach(mtcars)
par(mfrow=c(1,3))
hist(data_2019_2022_pow_2$m_daily_return, col="light blue", main="Daily market returns in 2019-2022 POW", xlab="m_daily_return" )
hist(data_2019_2022_pos_2$m_daily_return, col="light blue", main="Daily market returns in 2019-2022 POS", xlab="m_daily_return" )
hist(data_2019_2022_tok_2$m_daily_return, col="light blue", main="Daily market returns in 2019-2022 TOKEN", xlab="m_daily_return" )





#CSSD consensus
scat_cssd_2019_consensus <- bind_rows(data_2019_2022_pow, data_2019_2022_pos, data_2019_2022_tok ) %>%
  filter(between(date, as.Date("2019-01-01"), as.Date("2019-12-31") )) %>%
  ggplot(aes(x = m_daily_return, y = cssd, color = consensus )) +
  geom_point( alpha=0.70 ) +
  scale_color_manual(values = c("pow" = "red", "pos" = "blue", "token" = "dark green" )) +
  labs(title = "2019  CSSD", x = "Market daily returns", y = "CSSD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, colour = "grey")



scat_cssd_2020_consensus <- bind_rows(data_2019_2022_pow, data_2019_2022_pos, data_2019_2022_tok ) %>%
  filter(between(date, as.Date("2020-01-01"), as.Date("2020-12-31") )) %>%
  ggplot(aes(x = m_daily_return, y = cssd, color = consensus )) +
  geom_point( alpha=0.70 ) +
  scale_color_manual(values = c("pow" = "red", "pos" = "blue", "token" = "dark green" )) +
  labs(title = "2020  CSSD", x = "Market daily returns", y = "CSSD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, colour = "grey")



scat_cssd_2021_consensus <- bind_rows(data_2019_2022_pow, data_2019_2022_pos, data_2019_2022_tok ) %>%
  filter(between(date, as.Date("2021-01-01"), as.Date("2021-12-31") )) %>%
  ggplot(aes(x = m_daily_return, y = cssd, color = consensus )) +
  geom_point( alpha=0.70 ) +
  scale_color_manual(values = c("pow" = "red", "pos" = "blue", "token" = "dark green" )) +
  labs(title = "2021  CSSD", x = "Market daily returns", y = "CSSD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, colour = "grey")



scat_cssd_2022_consensus <- bind_rows(data_2019_2022_pow, data_2019_2022_pos, data_2019_2022_tok ) %>%
  filter(between(date, as.Date("2022-01-01"), as.Date("2022-12-31") )) %>%
  ggplot(aes(x = m_daily_return, y = cssd, color = consensus )) +
  geom_point( alpha=0.70 ) +
  scale_color_manual(values = c("pow" = "red", "pos" = "blue", "token" = "dark green" )) +
  labs(title = "2022  CSSD", x = "Market daily returns", y = "CSSD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, colour = "grey")



scat_cssd_2019_2022_consensus <- bind_rows(data_2019_2022_pow, data_2019_2022_pos, data_2019_2022_tok ) %>%
  ggplot(aes(x = m_daily_return, y = cssd, color = consensus )) +
  geom_point( alpha=0.70 ) +
  scale_color_manual(values = c("pow" = "red", "pos" = "blue", "token" = "dark green" )) +
  labs(title = "2019-2020  CSSD", x = "Market daily returns", y = "CSSD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, colour = "grey")



scatter_plots_cssd_consensus <- ggarrange(scat_cssd_2019_consensus, scat_cssd_2020_consensus, scat_cssd_2021_consensus,
                                          scat_cssd_2022_consensus, scat_cssd_2019_2022_consensus, 
                                          ncol = 3, nrow = 2)

scatter_plots_cssd_consensus








#CSAD consensus
scat_csad_2019_consensus <- bind_rows(data_2019_2022_pow, data_2019_2022_pos, data_2019_2022_tok ) %>%
  filter(between(date, as.Date("2019-01-01"), as.Date("2019-12-31") )) %>% 
  ggplot(aes(x = m_daily_return, y = csad, color = consensus )) +
  geom_point( alpha=0.70 ) +
  scale_color_manual(values = c("pow" = "red", "pos" = "blue", "token" = "dark green" )) +
  labs(title = "2019  CSAD", x = "Market daily returns", y = "CSAD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, colour = "grey")



scat_csad_2020_consensus <- bind_rows(data_2019_2022_pow, data_2019_2022_pos, data_2019_2022_tok ) %>%
  filter(between(date, as.Date("2020-01-01"), as.Date("2020-12-31") )) %>% 
  ggplot(aes(x = m_daily_return, y = csad, color = consensus )) +
  geom_point( alpha=0.70 ) +
  scale_color_manual(values = c("pow" = "red", "pos" = "blue", "token" = "dark green" )) +
  labs(title = "2020  CSAD", x = "Market daily returns", y = "CSAD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, colour = "grey")



scat_csad_2021_consensus <- bind_rows(data_2019_2022_pow, data_2019_2022_pos, data_2019_2022_tok ) %>%
  filter(between(date, as.Date("2021-01-01"), as.Date("2021-12-31") )) %>% 
  ggplot(aes(x = m_daily_return, y = csad, color = consensus )) +
  geom_point( alpha=0.70 ) +
  scale_color_manual(values = c("pow" = "red", "pos" = "blue", "token" = "dark green" )) +
  labs(title = "2021  CSAD", x = "Market daily returns", y = "CSAD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, colour = "grey")



scat_csad_2022_consensus <- bind_rows(data_2019_2022_pow, data_2019_2022_pos, data_2019_2022_tok ) %>%
  filter(between(date, as.Date("2022-01-01"), as.Date("2022-12-31") )) %>% 
  ggplot(aes(x = m_daily_return, y = csad, color = consensus )) +
  geom_point( alpha=0.70 ) +
  scale_color_manual(values = c("pow" = "red", "pos" = "blue", "token" = "dark green" )) +
  labs(title = "2022  CSAD", x = "Market daily returns", y = "CSAD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, colour = "grey")



scat_csad_2019_2022_consensus <- bind_rows(data_2019_2022_pow, data_2019_2022_pos, data_2019_2022_tok ) %>%
  ggplot(aes(x = m_daily_return, y = csad, color = consensus )) +
  geom_point( alpha=0.70 ) +
  scale_color_manual(values = c("pow" = "red", "pos" = "blue", "token" = "dark green" )) +
  labs(title = "2019-2020  CSAD", x = "Market daily returns", y = "CSAD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, colour = "grey")




scatter_plots_csad_consensus <- ggarrange(scat_csad_2019_consensus, scat_csad_2020_consensus, scat_csad_2021_consensus,
                                          scat_csad_2022_consensus, scat_csad_2019_2022_consensus, 
                                          ncol = 3, nrow = 2)

scatter_plots_csad_consensus







#number of coins over time by consensus

coins_over_time_consensus <- data_2019_2022 %>%
  group_by(date, consensus) %>%
  summarise(count = n_distinct(class)) %>% 
  ggplot( aes(x = date, y = count, color=consensus) ) +
  scale_color_manual(values = c("pow" = "red", "pos" = "blue", "token" = "dark green" )) +
  labs(title = "2019-2020 Number of coins by consensus", x = "Date", y = "Number of distinct coins") +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5))

coins_over_time_consensus




####










#### Covid scatter plots v2 ####

scat_csad_2020_covid_2 <- data_2020 %>%
  filter(row_number==1 ) %>%
  mutate(d_covid_period = ifelse(between( date, as.Date('2020-02-01'), as.Date('2020-02-29') ), 'covid_feb',
                                 ifelse( between( date, as.Date('2020-03-01'), as.Date('2020-03-31') ) , 'covid_mar' ,
                                         ifelse(between( date, as.Date('2020-04-01'), as.Date('2020-04-30') ), 'covid_apr', 
                                                'non-covid' )) ) ) %>% 
  arrange(d_covid) %>% 
  ggplot(aes(x = m_daily_return, y = csad, color = d_covid_period )) +
  geom_point() +
  scale_color_manual(values = c("covid_feb" = "red", "covid_mar" = "green", "covid_apr" = "blue", "non-covid" = "dark grey" )) +
  labs(title = "2020  CSAD", x = "Market daily returns", y = "CSAD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, colour = "grey")


scat_csad_2019_2022_covid_2 <- data_2019_2022 %>%
  filter(row_number==1 ) %>%
  mutate(d_covid_period = ifelse(between( date, as.Date('2020-02-01'), as.Date('2020-02-29') ), 'covid_feb',
                                 ifelse( between( date, as.Date('2020-03-01'), as.Date('2020-03-31') ) , 'covid_mar' ,
                                         ifelse(between( date, as.Date('2020-04-01'), as.Date('2020-04-30') ), 'covid_apr', 
                                                'non-covid' )) ) ) %>% 
  arrange(d_covid) %>% 
  ggplot(aes(x = m_daily_return, y = csad, color = d_covid_period )) +
  geom_point() +
  scale_color_manual(values = c("covid_feb" = "red", "covid_mar" = "green", "covid_apr" = "blue", "non-covid" = "dark grey" )) +
  labs(title = "2019-2020  CSAD", x = "Market daily returns", y = "CSAD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, colour = "grey")






scat_cssd_2020_covid_2 <- data_2020 %>%
  filter(row_number==1 ) %>%
  mutate(d_covid_period = ifelse(between( date, as.Date('2020-02-01'), as.Date('2020-02-29') ), 'covid_feb',
                                 ifelse( between( date, as.Date('2020-03-01'), as.Date('2020-03-31') ) , 'covid_mar' ,
                                         ifelse(between( date, as.Date('2020-04-01'), as.Date('2020-04-30') ), 'covid_apr', 
                                                'non-covid' )) ) ) %>%
  arrange(d_covid) %>% 
  ggplot(aes(x = m_daily_return, y = cssd, color = d_covid_period )) +
  geom_point() +
  scale_color_manual(values = c("covid_feb" = "red", "covid_mar" = "green", "covid_apr" = "blue", "non-covid" = "dark grey" )) +
  labs(title = "2020  CSSD", x = "Market daily returns", y = "CSSD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, colour = "grey")


scat_cssd_2019_2022_covid_2 <- data_2019_2022 %>%
  filter(row_number==1 ) %>%
  mutate(d_covid_period = ifelse(between( date, as.Date('2020-02-01'), as.Date('2020-02-29') ), 'covid_feb',
                                 ifelse( between( date, as.Date('2020-03-01'), as.Date('2020-03-31') ) , 'covid_mar' ,
                                         ifelse(between( date, as.Date('2020-04-01'), as.Date('2020-04-30') ), 'covid_apr', 
                                                'non-covid' )) )) %>%
  arrange(d_covid) %>% 
  ggplot() +
  geom_point(aes(x = m_daily_return, y = cssd, color = d_covid_period ) ) +
  scale_color_manual(values = c("covid_feb" = "red", "covid_mar" = "green", "covid_apr" = "blue", "non-covid" = "dark grey" )) +
  labs(title = "2019-2020  CSSD", x = "Market daily returns", y = "CSSD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, colour = "grey")






scatter_plots_cssd_covid_2 <- ggarrange(scat_cssd_2020_covid_2, scat_cssd_2019_2022_covid_2,
                                        scat_csad_2020_covid_2, scat_csad_2019_2022_covid_2,
                                        ncol = 2, nrow = 2)

scatter_plots_cssd_covid_2






###







#### Reg results ####


#Gathering regression outputs 2019-2022
export_summs( coeftest(cssd_up_down_2019_2022, vcov=NeweyWest(cssd_up_down_2019_2022)), 
              coeftest(csad_2019_2022, vcov=NeweyWest(csad_2019_2022)), 
              coeftest(csad_2019_2022_covid, vcov=NeweyWest(csad_2019_2022_covid)),
              coeftest(csad_up_down_2019_2022, vcov=NeweyWest(csad_up_down_2019_2022)),
              coeftest(csad_big_small_2019_2022, vcov=NeweyWest(csad_big_small_2019_2022)),
              digits = 5, to.file = "docx", file.name = "2019-2020 regs.docx")



export_summs( cssd_up_down_2019_2022_pow, #1
              cssd_up_down_2019_2022_pos,
              cssd_up_down_2019_2022_tok,
              digits = 5, to.file = "docx", file.name = "2019-2020 consensus cssd regs.docx")


export_summs( csad_2019_2022_pow, #2
              csad_2019_2022_pos,
              csad_2019_2022_tok,
              digits = 5, to.file = "docx", file.name = "2019-2020 consensus csad m regs.docx")


export_summs( coeftest(csad_2020_covid_pow, vcov=NeweyWest(csad_2020_covid_pow)), #3
              coeftest(csad_2019_2022_covid_pow, vcov=NeweyWest(csad_2019_2022_covid_pow)),
              coeftest(csad_2020_covid_pos, vcov=NeweyWest(csad_2020_covid_pos)),
              coeftest(csad_2019_2022_covid_pos, vcov=NeweyWest(csad_2019_2022_covid_pos)),
              coeftest(csad_2020_covid_tok, vcov=NeweyWest(csad_2020_covid_tok)),
              coeftest(csad_2019_2022_covid_tok, vcov=NeweyWest(csad_2019_2022_covid_tok)),
              digits = 5, to.file = "docx", file.name = "2019-2020 consensus csad covid regs.docx")


export_summs( csad_up_down_2019_2022_pow, #4
              csad_up_down_2019_2022_pos,
              csad_up_down_2019_2022_tok,
              digits = 5, to.file = "docx", file.name = "2019-2020 consensus csad up_down regs.docx")



export_summs( coeftest(cssd_up_down_2019_2022_pow, vcov=NeweyWest(cssd_up_down_2019_2022_pow)), #Model 1
              coeftest(cssd_up_down_2019_2022_pos, vcov=NeweyWest(cssd_up_down_2019_2022_pos)),
              coeftest(cssd_up_down_2019_2022_tok, vcov=NeweyWest(cssd_up_down_2019_2022_tok)),
              coeftest(csad_2019_2022_pow, vcov=NeweyWest(csad_2019_2022_pow)), #Model 2
              coeftest(csad_2019_2022_pos, vcov=NeweyWest(csad_2019_2022_pos)),
              coeftest(csad_2019_2022_tok, vcov=NeweyWest(csad_2019_2022_tok)),
              coeftest(csad_up_down_2019_2022_pow, vcov=NeweyWest(csad_up_down_2019_2022_pow)), #Model 4
              coeftest(csad_up_down_2019_2022_pos, vcov=NeweyWest(csad_up_down_2019_2022_pos)),
              coeftest(csad_up_down_2019_2022_tok, vcov=NeweyWest(csad_up_down_2019_2022_tok)),
              digits = 5, to.file = "docx", file.name = "2019-2020 consensus 1_2_4.docx")




export_summs(csad_2020_covid, 
             digits = 5, to.file = "docx", file.name = "2020 covid.docx")


###








#### Reg results by years ####


#CSSD 
export_summs( coeftest(cssd_up_down_2019, vcov=NeweyWest(cssd_up_down_2019)), 
              coeftest(cssd_up_down_2020, vcov=NeweyWest(cssd_up_down_2020)), 
              coeftest(cssd_up_down_2021, vcov=NeweyWest(cssd_up_down_2021)),
              coeftest(cssd_up_down_2022, vcov=NeweyWest(cssd_up_down_2022)),
              coeftest(cssd_up_down_2019_2022, vcov=NeweyWest(cssd_up_down_2019_2022)),
              digits = 5, to.file = "docx", file.name = "cssd_up_down_all_years regs.docx")



#CSAD wrt to market returns
export_summs( coeftest(csad_2019, vcov=NeweyWest(csad_2019)), 
              coeftest(csad_2020, vcov=NeweyWest(csad_2020)), 
              coeftest(csad_2021, vcov=NeweyWest(csad_2021)),
              coeftest(csad_2022, vcov=NeweyWest(csad_2022)),
              coeftest(csad_2019_2022, vcov=NeweyWest(csad_2019_2022)),
              digits = 5, to.file = "docx", file.name = "csad_all_years regs.docx")



#CSAD wrt to market returns and ups/downs
export_summs( coeftest(csad_up_down_2019, vcov=NeweyWest(csad_up_down_2019)), 
              coeftest(csad_up_down_2020, vcov=NeweyWest(csad_up_down_2020)), 
              coeftest(csad_up_down_2021, vcov=NeweyWest(csad_up_down_2021)),
              coeftest(csad_up_down_2022, vcov=NeweyWest(csad_up_down_2022)),
              coeftest(csad_up_down_2019_2022, vcov=NeweyWest(csad_up_down_2019_2022)),
              digits = 5, to.file = "docx", file.name = "csad_up_down_all_years regs.docx")


export_summs( csad_up_down_2019,
              csad_up_down_2020,
              csad_up_down_2021,
              csad_up_down_2022,
              csad_up_down_2019_2022,
              digits = 5, to.file = "docx", file.name = "csad_up_down_all_years regs.docx")




#CSAD wrt to big and small currencies
export_summs( coeftest(csad_big_small_2019, vcov=NeweyWest(csad_big_small_2019)), 
              coeftest(csad_big_small_2020, vcov=NeweyWest(csad_big_small_2020)), 
              coeftest(csad_big_small_2021, vcov=NeweyWest(csad_big_small_2021)),
              coeftest(csad_big_small_2022, vcov=NeweyWest(csad_big_small_2022)),
              coeftest(csad_big_small_2019_2022, vcov=NeweyWest(csad_big_small_2019_2022)),
              digits = 5, to.file = "docx", file.name = "csad_up_down_all_years regs.docx")




###










#### Reg results of weighted ####

export_summs( coeftest(cssd_up_down_2019_2022_w, vcov=NeweyWest(cssd_up_down_2019_2022_w)), 
              coeftest(csad_2019_2022_w, vcov=NeweyWest(csad_2019_2022_w)), 
              coeftest(csad_2019_2022_covid_w, vcov=NeweyWest(csad_2019_2022_covid_w)),
              coeftest(csad_up_down_2019_2022_w, vcov=NeweyWest(csad_up_down_2019_2022_w)),
              coeftest(csad_2019_2022_nb_w, vcov=NeweyWest(csad_2019_2022_nb_w)),
              digits = 5, to.file = "docx", file.name = "2019-2020 weighted regs.docx")


export_summs( cssd_up_down_2019_2022_w, 
              csad_2019_2022_w, 
              csad_2019_2022_covid_w,
              csad_up_down_2019_2022_w,
              csad_2019_2022_nb_w,
              digits = 5, to.file = "docx", file.name = "2019-2020 weighted regs.docx")


###
