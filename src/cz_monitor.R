# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
#
# Script: WP-on-steroids 
# Version 1.0: CZ/LvdA, 2021-05-01
#
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
  
library(digest)
library(readr)
library(fs)
library(futile.logger)
library(yaml)
library(lubridate)
library(magrittr)
library(stringr)
library(keyring)
library(tibble)
# library(RMariaDB)
library(DBI)

# init functions ----
source("src/wpos_utilities.R", encoding = "UTF-8")

# init cfg ----
wpos_config <- read_yaml("config.yaml")

# init logging ----
path.wpos_log <- paste(wpos_config$log_home, "wpos.log", sep = "/")
lg_ini <- flog.appender(appender.file(path.wpos_log), "wpos")
flog.info("= = = = = WP-monitor START = = = = =", name = "wpos")

# init file to watch ----
# wpos_today <- ymd_hms("2021-05-30 00:00:00", tz = "Europe/Amsterdam")
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

# init file hash ----
playlist <- read_lines(file = path.playlist)
playlist.md5 <- digest(object = playlist)
playlist.md5_sav <- playlist.md5

# init polling interval ----
POLL_INTERVAL <- wpos_config$polling_interval_in_secs

# init stop-time ----
wpos_stop <- now(tzone = "Europe/Amsterdam")
hour(wpos_stop) <- wpos_config$mon_stop_h
minute(wpos_stop) <- wpos_config$mon_stop_m
second(wpos_stop) <- 0L

# Connect to database ----
flog.info("connecting to WordPress-DB on %s", wpos_config$wp_env, name = "wpos")
wp_conn <- get_wp_conn(wpos_config$wp_env)

sql_post_key <- str_remove(pl_name, "\\.txt")

sel_stmt01 <-
  sprintf("SELECT cz_id FROM salsa_plws_gidsweek where pgmStart = '%s';",
          sql_post_key)

dsSql01 <- dbGetQuery(wp_conn, sel_stmt01)

# === TESTESTEST !!!
# dsSql01$cz_id <- 432782 # Framework 2018-02-26 05:00:00
# === TESTESTEST !!!

# start monitoring ----
flog.info("watching %s until %s, id = %s",
          path.playlist,
          wpos_stop,
          dsSql01$cz_id,
          name = "wpos")

# let the show begin ----
while (TRUE) {
  
  # + game over ----
  if (now(tzone = "Europe/Amsterdam") > wpos_stop) {
    flog.info("stopping ...", name = "wpos")
    break
  }
  
  # + get current content ----
  playlist <- read_lines(file = path.playlist)
  playlist.md5 <- digest(object = playlist)
  
  # + snooze if no change ----
  if (file_size(path = path.playlist) == 0 ||
      playlist.md5 == playlist.md5_sav) {
    # + . wait a few moments ----
    Sys.sleep(time = POLL_INTERVAL)
    next
  }
  
  # check db-connection and reconnect first, if needed
  if (!grh_conn_sts_valid()) {
    flog.info("lost connection to WordPress-DB on %s; retry...", wpos_config$wp_env, name = "wpos")
    wp_conn <- get_wp_conn(wpos_config$wp_env)
  }
  
  n_mon_upd_errs = update_wp()
  
      # update succesful
      if (n_mon_upd_errs == 0) {
        break
      }
      
      flog.info("update failed, re-trying...", name = "wpos")
      
      # don't retry beyond end of the show
      if (now(tzone = "Europe/Amsterdam") > wpos_stop) {
        flog.info("show is over: stop", name = "wpos")
        break
      }
      
      # reconnect
      wp_conn <- get_wp_conn(wpos_config$wp_env)
      
      # connection-type=S4 indicates a valid connection; other types indicate failure
      if (typeof(wp_conn) != "S4") {
        # + . wait a few moments ----
        Sys.sleep(time = 10.0)
        
        next
      }
      
      flog.info("OK ... connection re-established", name = "wpos")
      break
      
    }
    
    # + wait a few moments ----
    Sys.sleep(time = POLL_INTERVAL)
  }
  
  # disconnect from DB
  dbDisconnect(wp_conn)
  flog.info("disconnected from WordPress-DB", name = "wpos")
}

# monitoring stopped----
flog.info("= = = = = WP-monitor STOP  = = = = =", name = "wpos")
