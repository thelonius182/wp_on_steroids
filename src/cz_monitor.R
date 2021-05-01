# 
# WP-on-steroids
  
library(digest)
library(readr)
library(fs)
library(futile.logger)
library(yaml)
library(lubridate)
library(magrittr)
library(stringr)

source("src/wpos_utilities.R", encoding = "UTF-8")

# init cfg
wpos_config <- read_yaml("config.yaml")

# init logging
path.wpos_log <- paste(wpos_config$log_home, "wpos.log", sep = "/")
lg_ini <- flog.appender(appender.file(path.wpos_log), "wpos")
flog.info("= = = = = WP-monitor START = = = = =", name = "wpos")

# set file to watch
# - for production: manually set testfile = none
if (wpos_config$test_file == "none") {
  
  wpos_today <- now(tzone = "Europe/Amsterdam")
  # change hour to start-of-broadcast 
  hour(wpos_today) <- wpos_config$broadcast_start_hour
  # build doc-name that will hold the playlist tracks
  pl_name <- format_ISO8601(wpos_today, precision = "ymdh") %>% 
    str_remove_all("-") %>% 
    str_replace("T", "_") %>% 
    paste0(".txt")
  path.playlist <- paste(wpos_config$dir_to_watch, pl_name, sep = "/")
  
  if (!file_exists(path = path.playlist)) {
    file_create(path = path.playlist)
  }
  
} else {
  path.playlist <- paste(wpos_config$dir_to_watch, wpos_config$test_file, sep = "/")
}

# create rest of start situation
path.playlist_lock <- paste(wpos_config$dir_to_watch, ".lock", sep = "/")
file_create(path = path.playlist_lock)
playlist <- read_lines(file = path.playlist)
playlist.md5 <- digest(object = playlist)
playlist.md5_sav <- playlist.md5
POLL_INTERVAL <- wpos_config$polling_interval_in_secs

# start monitoring, every POLL_INTERVAL seconds
flog.info(paste("now watching: ", path.playlist), name = "wpos")

while (file_exists(path = path.playlist_lock)) {
  
  # get current file contents & hash
  playlist <- read_lines(file = path.playlist)
  playlist.md5 <- digest(object = playlist)
  
  # file non-empty and different hash: update WordPress
  if (file_size(path = path.playlist) > 0 && playlist.md5 != playlist.md5_sav) {
    playlist.md5_sav <- playlist.md5
    update_wp(playlist)
  }
  
  Sys.sleep(time = POLL_INTERVAL)
}

flog.info("= = = = = WP-monitor STOP  = = = = =", name = "wpos")
