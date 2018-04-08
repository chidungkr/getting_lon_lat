

library(tidyverse)
library(magrittr)
library(stringr)


# Hàm lấy dữ liệu địa lí của các dự án bất động sản: 
get_long_lat_data <- function(x) {
  m <- readLines(x) 
  m <- m[str_detect(m, "li lat=")]
  m <- m[!str_detect(m, "add")]
  
  m %>% str_split(" ") %>% unlist() -> k
  
  k[str_detect(k, "lon|lat")] %>% 
    matrix(ncol = 2, byrow = TRUE) %>% 
    as.data.frame() -> lon_lat
  
  lon_lat %<>% mutate(lat = str_replace_all(V1, "[^0-9]", ""), 
                      lon = str_replace_all(V2, "[^0-9]", ""), 
                      lat = str_sub(lat, 1, 13) %>% as.numeric() / 10^11, 
                      lon = str_sub(lon, 1, 13) %>% as.numeric() / 10^10) %>% 
    select(lon, lat)
  return(lon_lat)
  
}

lon_lat <- lapply(paste0("https://batdongsan.com.vn/can-ho-chung-cu-ha-noi/p", 1:43), get_long_lat_data)
lon_lat <- do.call("bind_rows", lon_lat)



# Vị trí của các dự án này trên bản đồ: 
library(ggmap)
map <- get_map(location = "Hanoi", zoom = 12)

ggmap(map) + 
  geom_point(data = lon_lat, 
             aes(x = lon, y = lat), color = "red", size = 3, alpha = 0.4) + 
  theme(axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white")) + 
  labs(title = "Location of Real Estate Projects in Hanoi", 
       subtitle = "Data Source: https://batdongsan.com.vn/")