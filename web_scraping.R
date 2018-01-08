### Static sites

library(rvest)
# URL страницы
# https://ru.wikipedia.org/wiki/Субъекты_Российской_Федерации
wiki_url <- "https://ru.wikipedia.org/wiki/%D0%A1%D1%83%D0%B1%D1%8A%D0%B5%D0%BA%D1%82%D1%8B_%D0%A0%D0%BE%D1%81%D1%81%D0%B8%D0%B9%D1%81%D0%BA%D0%BE%D0%B9_%D0%A4%D0%B5%D0%B4%D0%B5%D1%80%D0%B0%D1%86%D0%B8%D0%B8"

# SelectorGadget
# XPath, который мы вручную взяли из исходного кода HTML-страницы
wiki_xpath <- '//*[@id="mw-content-text"]/div/table[4]'

wiki_table <- wiki_url %>% read_html() %>% html_node(xpath = wiki_xpath) %>% html_table()
head(wiki_table)

### Dynamic sites

library(httr)
library(jsonlite)

bgr_url <- 'http://bus.gov.ru/public-rating/api/analytics/rateValue?critEntityId=12&criterionCode=1&groupId=251&indRatingId=134349&isOrg=true&pgmu=true&ratingYear=2017&scopesActivity=2&sourceAgencyId=262082'

# Делаем запрос по адресу и сохраняем ответ в resp:
resp <- GET(bgr_url)
str(resp)


# Попутно указываем кодировку и выбираем единственный элемент списка $rateValues
df <- fromJSON(content(resp, "text", encoding = "UTF-8"))$rateValues
str(df)

# Отбираем то, что надо
df <- as.data.frame(t(data.frame(rate_name = df$rate$rateName, rate_value = df[,c("rateValue")])), stringsAsFactors = F)
# Причесываем переменные и их названия
row.names(df) <- NULL
colnames(df) <- df[1, ]
df <- df[-1, ]  
str(df)

## Цикл

bus.gov.ru_get_schools_list <- function(page_number) {
  
  path <- paste0(
    "http://bus.gov.ru/public-rating/api/topOrganizations/tableData?groupId=251&groupIdStr=%D0%BE%D1%80%D0%B3%D0%B0%D0%BD%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D0%B8,+%D0%BE%D1%81%D1%83%D1%89%D0%B5%D1%81%D1%82%D0%B2%D0%BB%D1%8F%D1%8E%D1%89%D0%B8%D0%B5+%D0%BE%D0%B1%D1%80%D0%B0%D0%B7%D0%BE%D0%B2%D0%B0%D1%82%D0%B5%D0%BB%D1%8C%D0%BD%D1%83%D1%8E+%D0%B4%D0%B5%D1%8F%D1%82%D0%B5%D0%BB%D1%8C%D0%BD%D0%BE%D1%81%D1%82%D1%8C&orgName=&page=",
    page_number,
    "&pageSize=10&ppoId=21499&ppoIdStr=%D0%A1%D0%B0%D0%BD%D0%BA%D1%82-%D0%9F%D0%B5%D1%82%D0%B5%D1%80%D0%B1%D1%83%D1%80%D0%B3&scopeActivity=2")
  
  # set cookies for region subsetting
  resp <- GET(path, set_cookies(
    homeRegionId = "5277347",
    homeRegionFullName="%D0%A1%D0%B0%D0%BD%D0%BA%D1%82-%D0%9F%D0%B5%D1%82%D0%B5%D1%80%D0%B1%D1%83%D1%80%D0%B3",
    homeRegionName="%D0%A1%D0%B0%D0%BD%D0%BA%D1%82-%D0%9F%D0%B5%D1%82%D0%B5%D1%80%D0%B1%D1%83%D1%80%D0%B3"))
  
  data <- fromJSON(content(resp, "text"))$records
  
  return(data.frame(id = data$organization$sourceAgency$id, name = data$organization$fullName, address = data$organization$address$fullAddress))
  
}

df <- bus.gov.ru_get_schools_list(1)

df

for (i in 2:4) {
  print(i)
  df <- rbind(df, bus.gov.ru_get_schools_list(i))
  Sys.sleep(1)
}

df

### RSelenium

library(RSelenium)
library(rvest)

url_nba <- "https://projects.fivethirtyeight.com/2017-nba-predictions/"

# Заводим движок!
rD <- rsDriver(port=4444L,browser="firefox")
remDr <- rD$client

# Переходим на нашу страницу
remDr$navigate(url_nba)

message("NAVIGATE")

# Нажимаем на бокс и находим десятую опцию (April 14 before playoffs)
# XPath был найдет с помощью всё того же просмотрщика исходного кода
webElem <- remDr$findElement(using = 'xpath', value = "//*[@id='forecast-selector']/div[2]/select/option[10]")

message("webelem")
# Говорим браузеру - нажми на это.
webElem$clickElement()
message("click")

# После нажатия сохраняем код страницы.
webpage <- remDr$getPageSource()[[1]]

# Закрываем браузер и останавливаем RSelenium
remDr$close()
rD[["server"]]$stop() 

# Теперь передаём наше хозяйство в rvest и извлекаем таблицу
webpage_nba <- read_html(webpage) %>% html_table(fill = TRUE)

# rvest извлёк три разные таблицы из страницы. Нам нужна последняя:
df <- webpage_nba[[3]]

# Приводим датафрейм в порядок
names(df) <- df[3,]
df <- tail(df,-3)
df <- head(df,-4)
df <- df[ , -which(names(df) == "NA")]
df