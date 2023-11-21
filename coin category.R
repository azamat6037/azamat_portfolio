#creating crypto category
library(reshape2)
library(xlsx)


top200 <- read_excel("C:/Users/azama/Desktop/coingecko categories/top200.xlsx")


ethereum_ecosystem <- read_excel("C:/Users/azama/Desktop/coingecko categories/ethereum ecosystem.xlsx", 
                                 sheet = "ethereum_ecosystem")

smart_contract_platform <- read_excel("C:/Users/azama/Desktop/coingecko categories/ethereum ecosystem.xlsx", 
                                 sheet = "smart-contract-platform")

binance_smart_chain <- read_excel("C:/Users/azama/Desktop/coingecko categories/ethereum ecosystem.xlsx", 
                                      sheet = "binance-smart-chain")

polygon_ecosystem <- read_excel("C:/Users/azama/Desktop/coingecko categories/ethereum ecosystem.xlsx", 
                                 sheet = "polygon-ecosystem")

avalanche_ecosystem <- read_excel("C:/Users/azama/Desktop/coingecko categories/ethereum ecosystem.xlsx", 
                                sheet = "avalanche-ecosystem")


df_coin_list <- read_csv("C:/Users/azama/Desktop/Untitled Folder/df_coin_list.csv")



category <- data.frame(ethereum_ecosystem) %>% 
  mutate(category_1 = "ethereum_ecosystem") %>% 
  select(rank=X., name, symbol, category_1) %>% 
  left_join(df_coin_list, by = c("name" = "name")) %>% 
  select(rank, name, id, symbol = symbol.x, category_1)



category <- top200 %>% 
  left_join(df_coin_list, by = c("name" = "name")) %>% 
  left_join(ethereum_ecosystem %>% 
              mutate(category_1 = "ethereum_ecosystem" ), 
            by = c("name" = "name")) %>% 
  left_join(smart_contract_platform %>%  
              mutate(category_2 = "smart_contract_platform"), 
            by = c("name" = "name")) %>%
  left_join(binance_smart_chain %>%  
              mutate(category_3 = "binance_smart_chain"), 
            by = c("name" = "name")) %>%
  left_join(polygon_ecosystem %>%  
              mutate(category_4 = "polygon_ecosystem"), 
            by = c("name" = "name")) %>%
  left_join(avalanche_ecosystem %>%  
              mutate(category_5 = "avalanche_ecosystem"), 
            by = c("name" = "name")) %>%
  select(rank, name, id, symbol = symbol.x, category_1, category_2, category_3, category_4, category_5) %>% 
  mutate( category = coalesce(category_1, category_2, category_3, category_4, category_5, NA) )




#####



pow <- read_excel("C:/Users/azama/Desktop/coingecko categories/consensus.xlsx", 
                        sheet = "pow") %>% 
  mutate(symbol = gsub(" logo", "", Name), name = lead( Name ), algorythm = "pow") %>%
  filter(!is.na(number)) %>% 
  select(symbol, name, algorythm)


pos <- read_excel("C:/Users/azama/Desktop/coingecko categories/consensus.xlsx", 
                  sheet = "pos") %>% 
  mutate(symbol = gsub(" logo", "", Name), name = lead( Name ), algorythm = "pos") %>%
  filter(!is.na(number)) %>% 
  select(symbol, name, algorythm)

dag <- read_excel("C:/Users/azama/Desktop/coingecko categories/consensus.xlsx", 
                  sheet = "dag") %>% 
  mutate(symbol = gsub(" logo", "", Name), name = lead( Name ), algorythm = "dag" ) %>%
  filter(!is.na(number)) %>% 
  select(symbol, name, algorythm)

dpos <- read_excel("C:/Users/azama/Desktop/coingecko categories/consensus.xlsx", 
              sheet = "dpos") %>% 
  mutate(symbol = gsub(" logo", "", Name), name = lead( Name ), algorythm = "dpos" ) %>%
  filter(!is.na(number)) %>% 
  select(symbol, name, algorythm)


algorithm_2 <- rbind( pow, pos, dag, dpos ) %>% 
  mutate(symbol = tolower(symbol))




#####----------------------------------------------------------------------


  
bnb_consensus <- read_excel("C:/Users/azama/Desktop/coingecko categories/bnb_consensus.xlsx") %>% 
  mutate(symbol = tolower(symbol) )
  

  
algorithm <- top200 %>% 
  left_join( df_coin_list, by = c("name" = "name") ) %>%
  left_join( bnb_consensus, by = c("symbol.y" = "symbol") ) %>% 
  select(rank, name = name.x, symbol = symbol.y, consensus)


write.xlsx(algorithm, file = "top200_consensus", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

