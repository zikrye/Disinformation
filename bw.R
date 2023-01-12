library(readr)
#EM777 <- read_csv("EM777.csv", skip = 5)
#View(EM777)
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(stringr)

china <- read_excel("raw_data/china.xlsx", skip = 5)
ukraine <- read_excel("raw_data/ukraine.xlsx", skip = 5)
russian <- read_excel("raw_data/russian.xlsx", skip = 5)
israel <- read_csv("raw_data/israel.csv")
canada <- read_excel("raw_data/canada.xlsx", skip = 5)

putin <- read_csv("raw_data/putin.csv", skip = 5)
snake <- read_excel("raw_data/snake.xlsx", skip = 5)
johnson <- read_excel("raw_data/johnson.xlsx", skip = 5)
germany <- read_csv("raw_data/germany.csv", skip = 5)
korea <- read_csv("raw_data/korea.csv", skip = 5)

japan <- read_excel("raw_data/japan.xlsx", skip = 5) # 25
australia <- read_csv("raw_data/australia.csv", skip = 5) # 57
biden <- read_csv("raw_data/biden.csv", skip = 5) # 2660
oil <- read_excel("raw_data/oil.xlsx", skip = 5) # 9293
carmakers <- read_csv("raw_data/carmakers.csv", skip = 5) # 41

# select columns  
names(china)
column <- c("Date", "Full Text", "Sentiment", "Language", "Country Code", "Country", "City Code", "City")

israel$Title[1]
china$`Full Text`[1]

c <- china[, column]
z <- ukraine[, column]
r <- russian[, column]
i <- israel[, column]
ca <- canada[, column]

p <- putin[, column]
s <- snake[, column]
j <- johnson[, column]
g <- germany[, column]
k <- korea[, column]

ja <- japan[, column]
au <- australia[, column]
b <- biden[, column]
o <- oil[, column]
car <- carmakers[, column]

write_excel_csv(car,"selected/car.csv")

# combine fifteen sheets into one sheet
list_tweets <- list("china" = c, "ukraine" = z, "russian" = r, "israel" = i, "canada" = ca, 
                    "putin" = p, "snake" = s, "johnson" = j, "germany" = g, "korea" = k, 
                    "japan" = ja, "australia" = au, "biden" = b, "oil" = o, "carmakers" = car)
write.xlsx(list_tweets,"bw_tweets.xlsx")


# count by countries
c_c <- china %>% drop_na(Country) %>% count(Country, sort=TRUE)
c_u <- ukraine %>% drop_na(Country) %>% count(Country, sort=TRUE)
c_r <- russian %>% drop_na(Country) %>% count(Country, sort=TRUE)
c_i <- israel %>% drop_na(Country) %>% count(Country, sort=TRUE)
c_ca <- canada %>% drop_na(Country) %>% count(Country, sort=TRUE)

c_p <- putin %>% drop_na(Country) %>% count(Country, sort=TRUE)
c_s <- snake %>% drop_na(Country) %>% count(Country, sort=TRUE)
c_j <- johnson %>% drop_na(Country) %>% count(Country, sort=TRUE)
c_g <- germany %>% drop_na(Country) %>% count(Country, sort=TRUE)
c_k <- korea %>% drop_na(Country) %>% count(Country, sort=TRUE)

c_ja <- japan %>% drop_na(Country) %>% count(Country, sort=TRUE)
c_au <- australia %>% drop_na(Country) %>% count(Country, sort=TRUE)
c_b <- biden %>% drop_na(Country) %>% count(Country, sort=TRUE)
c_o <- oil %>% drop_na(Country) %>% count(Country, sort=TRUE)
c_car <- carmakers %>% drop_na(Country) %>% count(Country, sort=TRUE)

#c_df <- merge(c_c, c_u, by = "Country", all = TRUE)

# count the number of tweets for each country and for each topic, write in one sheet
list_countries <- list("china" = c_c, "ukraine" = c_u, "russian" = c_r, "israel" = c_i, "canada" = c_ca, 
                    "putin" = c_p, "snake" = c_s, "johnson" = c_j, "germany" = c_g, "korea" = c_k, 
                    "japan" = c_ja, "australia" = c_au, "biden" = c_b, "oil" = c_o, "carmakers" = c_car)
write.xlsx(list_countries,"country list.xlsx")

# number of tweets count for all topics
f <- function(x){
  list(nrow(x))
}
x <- list(china = c, ukraine = z, russian = r, israel = r, canada = ca, 
          putin = p, snake = s, jphnson = j, germany = g, korea = k, 
          japan = j, australia = au, biden = b, oil = o, carmakers = car)
df <- data.frame(topics = names(lapply(x, f)), counts = unlist(lapply(x, f), use.names = FALSE))
#write.csv(df,"number of tweets.csv", row.names = FALSE)

y <- list(china = c_c, ukraine = c_u, russian = c_r, israel = c_r, canada = c_ca, 
          putin = c_p, snake = c_s, jphnson = c_j, germany = c_g, korea = c_k, 
          japan = c_j, australia = c_au, biden = c_b, oil = c_o, carmakers = c_car)
df1 <- data.frame(topics = names(lapply(y, f)), counts = unlist(lapply(y, f), use.names = FALSE))
#write.csv(df1,"number of countries.csv", row.names = FALSE)

list_number <- list("tweet" = df, "country" = df1)
write.xlsx(list_number,"number.xlsx")

###--------------------

sum(str_detect(japan$Title, "Covid"))

#sample rows
#with country information
s_au <- australia %>% drop_na(Country)
set.seed(1)
s_car <- carmakers[,c('Date', 'Country', 'Title')][sample(nrow(carmakers[,c('Date', 'Country', 'Title')]), 41), ] # dim(biden)[1]
write.xlsx(s_car,"sample/s_car.xlsx")



change <- read_excel("sample/s_au.xlsx")
change[change$relative == 2, ]$relative <- 1
write.xlsx(change,"sample/s_au.xlsx")


# mark tweets without RT

# try <- ukraine %>% count(Title, sort = TRUE)
# write.xlsx(try,"zlensky_unique tweets count.xlsx")
# try$Title[1]

set.seed(1)
d_c <- china[-which(str_detect(china$Title, "RT")), ]
d_c <- d_c[,c('Date', 'Country', 'Title')][sample(nrow(d_c[,c('Date', 'Country', 'Title')]), 117), ] 
write.xlsx(d_c,"d_sample/d_c.xlsx")

# count related tweets after excluding RT
dr_r <- read_excel("d_sample/d_r.xlsx")
dr_u %>% count(Relative) 
dr_ja %>% count(Relative)
dr_au %>% count(Relative) 
dr_b %>% count(Relative)
dr_o %>% count(Relative)
dr_car %>% count(Relative)
dr_p %>% count(Relative)
dr_s %>% count(Relative)
dr_j %>% count(Relative)
dr_g %>% count(Relative)
dr_k %>% count(Relative)
dr_ca %>% count(Relative)
dr_i %>% count(Relative)
dr_r %>% count(Relative)
dr_c %>% count(Relative)

# output all topics to be marked
mark_list <- list("china" = dr_c, "ukraine" = dr_u, "russian" = dr_r, "israel" = dr_i, "canada" = dr_ca, 
                       "putin" = dr_p, "snake" = dr_s, "johnson" = dr_j, "germany" = dr_g, "korea" = dr_k, 
                       "japan" = dr_ja, "australia" = dr_au, "biden" = dr_b, "oil" = dr_o, "carmakers" = dr_car)
write.xlsx(mark_list,"bw_relative.xlsx")
