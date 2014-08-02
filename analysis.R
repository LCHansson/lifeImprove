library(stringr)
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(ggvis)
library(lubridate)

run = FALSE

## Load data ----
db <- src_sqlite("data//tickmate-backup-20140725.db")
tracking <- tbl(db, "sqlite_sequence")
ticks <- tbl(db, "ticks")
tracks <- tbl(db, "tracks")
meta <- tbl(db, "android_metadata")

ticks_db <- ticks %>%
  collect() %>%
  mutate(month = month + 1,
         date = as.Date(paste(year, month, day, sep = "-"))
  ) %>%
  rename(c("_id" = "id", "_track_id" = "track_id"))

tracks_db <- tracks %>%
  collect() %>%
  rename(c("_id" = "track_id")) %>%
  mutate(name = str_replace_all(str_replace_all(name, " ", "_"), ":", ""))

ticks_db <- ticks_db %>%
  left_join(tracks_db, by = "track_id")



## Inspect data ----
analysdata <- ticks_db %>%
  select(name, date) %>%
  arrange(date) %>%
  group_by(name, date) %>%
  mutate(dummy = 1) %>%
  filter(row_number() == 1) %>%
  spread(key = name, value = dummy, fill = 0)
  

## Analysis ----
if (run == TRUE) {
  analysdata %>%
    ggvis(~date, ~Sov_minst_7_timmar_efter_dagen) %>%
    layer_lines() %>%
    layer_lines()
  
  
  
  lm(`Sov minst 7 timmar efter dagen` ~ `Ingen dator efter 23:00`, data = analysdata)
  df <- data.frame(
    track_id = c("a", "b", "a", "b"),
    date = c(1, 1, 2, 3),
    value = 1,
    val2 = 1:4
  )
  
  df %>%
    select(date, track_id) %>%
    group_by(date) %>%
    spread(date, track_id, fill = 0)
  
  a <- df %>%
    group_by(date) %>%
    do(b = unique(.$track_id))
  
  
  stocks <- data.frame(
    time = as.Date('2009-01-01') + 0:9,
    X = rnorm(10, 0, 1),
    Y = rnorm(10, 0, 2),
    Z = rnorm(10, 0, 4)
  ) %>% tbl_df()
  stocksm <- stocks %>% gather(stock, price, -time)
  stocksm %>% spread(stock, price)
  stocksm %>% spread(time, price)
}