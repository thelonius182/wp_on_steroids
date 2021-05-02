playlist2postdate <- function(playlist) {
  tmp_date <- playlist %>% str_sub(0, 8)
  tmp_time <- playlist %>% str_sub(11, 13) %>% paste0(":00:00")
  result <- paste0(tmp_date, " ", tmp_time) %>% ymd_hms
}

get_wallclock <- function(pm_cum_tijd, pm_playlist) {
  cum_tijd_ts <- paste0("2019-01-01 ", pm_cum_tijd) %>% ymd_hms
  start_clock <- pm_playlist %>% str_sub(12, 13) %>% as.integer
  wallclcok_ts <- cum_tijd_ts + hours(start_clock)
  wallclock_ts_rounded <- wallclcok_ts %>% round_date("minute")
  wallclock <- wallclock_ts_rounded %>% as.character %>% str_sub(12, 16)
}

get_wp_conn <- function() {
  db_type <- "prd"
  db_host <- key_get(service = paste0("sql-wp", db_type, "_host"))
  db_user <- key_get(service = paste0("sql-wp", db_type, "_user"))
  db_password <- key_get(service = paste0("sql-wp", db_type, "_pwd"))
  db_name <- key_get(service = paste0("sql-wp", db_type, "_db"))
  db_port <- 3306
  db_table <- "cz.wp_posts"
  
  result <- tryCatch( {
    grh_conn <- dbConnect(drv = MySQL(), user = db_user, password = db_password,
                          dbname = db_name, host = db_host, port = db_port)
  },
  error = function(cond) {
    flog.error("Wordpress database onbereikbaar (dev: check PuTTY)", name = "wpos")
    return("connection-error")
  }
  )
  return(result)
}

update_wp <- function() {

  # standard summary
  sql_gidstekst <- "Experimenteel, avant-garde, industrial, ambient en electronics.\n<!--more-->\n&nbsp;\n&nbsp;\n"
  
  # tracks
  tib_pl <- playlist %>% as_tibble()
  names(tib_pl) <- "pl_line"
  
  for (cur_pl_line in tib_pl$pl_line) {

    sql_gidstekst <- paste0(sql_gidstekst,
                            dbEscapeStrings(wp_conn, enc2native(cur_pl_line)),
                            "\n")
  }
  
  upd_stmt02 <-
    sprintf("update wp_posts set post_content = '%s' where id = %i;",
            sql_gidstekst,
            dsSql01$cz_id)
  
  dbGetQuery(wp_conn, upd_stmt02)
  
  flog.info("Gids bijgewerkt: %s", pl_name, name = "wpos")
}