library(dplyr)

# 匯入資料檔 https://goo.gl/OGrBX7
t1 <- read.csv("data/105上半年台中市各地非營業用戶售電量.csv", stringsAsFactors = FALSE)
t2 <- read.csv("data/105上半年台北市各地非營業用戶售電量.csv", stringsAsFactors = FALSE)
t3 <- read.csv("data/105上半年台南市各地非營業用戶售電量.csv", stringsAsFactors = FALSE)
t4 <- read.csv("data/105上半年新北市各地非營業用戶售電量.csv", stringsAsFactors = FALSE)
t5 <- read.csv("data/105上半年桃園市各地非營業用戶售電量.csv", stringsAsFactors = FALSE)
t6 <- read.csv("data/105上半年高雄市各地非營業用戶售電量.csv", stringsAsFactors = FALSE)

# 新增一個city欄位，避免不同都市相同行政區無法識別
t1 <- mutate(t1, city="台中市")
t2 <- mutate(t2, city="台北市")
t3 <- mutate(t3, city="台南市")
t4 <- mutate(t4, city="新北市")
t5 <- mutate(t5, city="桃園市")
t6 <- mutate(t6, city="高雄市")

raw <- bind_rows(t1,t2,t3,t4,t5,t6) # 合併六都資料
head(raw)

# 依據都市/行政區/村里/一級發佈區，擷取連續兩個月的售電資料
tp1 <- filter(raw, Ym%in%c(10501, 10502)) %>%
  group_by(code1, cunli, area.code, area, city) %>% 
  summarise(gen_0102=sum(gen, na.rm = TRUE)) %>% group_by()

tp2 <- filter(raw, Ym%in%c(10503, 10504)) %>%
  group_by(code1, cunli, area.code, area, city) %>% 
  summarise(gen_0304=sum(gen, na.rm = TRUE)) %>% group_by()

tp3 <- filter(raw, Ym%in%c(10505, 10506)) %>%
  group_by(code1, cunli, area.code, area, city) %>% 
  summarise(gen_0506=sum(gen, na.rm = TRUE)) %>% group_by()

# 合併1-2月、3-4月、5-6月的資料
out <- left_join(tp1, tp2) %>% left_join(tp3)

# 處理村里的 group_by (因為有10個一級發佈區有跨兩個村里的問題)
# repeated_code1 <- (table(out$code1)>1) %>% which %>% names
# filter(out, code1%in%repeated_code1)

out <- group_by(out, city, area, code1) %>%
  summarise(cunli=paste(cunli, collapse = "/"), 
            gen_0102=sum(gen_0102, na.rm = TRUE), 
            gen_0304=sum(gen_0304, na.rm = TRUE), 
            gen_0506=sum(gen_0506, na.rm = TRUE)) %>%
  group_by()

# 輸出六都售電資料，encodeing="utf8"
write.csv(out, "data/CODE1_105年六都售電.csv", row.names = F) 

library(foreign)
# 全國一級發佈區，已轉換成 WGS84_EPGS:4326 經緯度座標，encoding="big5"
dat <- read.dbf("data/TW_CODE1_WGS84_4326/TW_CODE1_WGS84_4326.dbf", as.is = TRUE)
dat$TOWN <- iconv(dat$TOWN, "big5") 
dat$COUNTY <- iconv(dat$COUNTY, "big5") 

tmp <- left_join(dat, out, by=c("CODE1"="code1")) 
write.dbf(tmp, "data/map.dbf") 
# 將結果重新命名為 CODE1_TaiPower_WGS84_4326，並上傳到 carto 畫地圖。

