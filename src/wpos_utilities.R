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

get_wp_conn <- function(wp_env) {
  
  # TEST
  # wp_env <- "dev"
  # TEST
  
  db_host <- key_get(service = paste0("sql-wp", wp_env, "_host"))
  db_user <- key_get(service = paste0("sql-wp", wp_env, "_user"))
  db_password <-
    key_get(service = paste0("sql-wp", wp_env, "_pwd"))
  db_name <- key_get(service = paste0("sql-wp", wp_env, "_db"))
  db_port <- 3305
  try_err_grh_conn <- FALSE
  try_err_grh_conn_msg <- NULL
    
  result <- tryCatch({
    grh_conn <-
      dbConnect(
        drv = MySQL(),
        user = db_user,
        password = db_password,
        dbname = db_name,
        host = db_host,
        port = db_port
      )
  },
  error = function(db_err) {
    try_err_grh_conn <<- TRUE
    try_err_grh_conn_msg <<- db_err
  })
  
  if (try_err_grh_conn) {
    flog.error(try_err_grh_conn_msg, name = "wpos")
    # return something having a type unequal to "S4", e.g. "character"
    result <- "wp-db no connection"
  }
  
  return(result)
}

update_wp <- function() {
  
  n_upd_errs <- 0

  # standard summary
  sql_gidstekst.1 <- "Experimenteel, avant-garde, industrial, ambient en electronics.\n<!--more-->\n&nbsp;\n&nbsp;\n"
  sql_gidstekst.2 <- paste(playlist, collapse = "\n")
  sql_gidstekst <- paste(sql_gidstekst.1, sql_gidstekst.2, sep = "\n") %>% str_replace_all("[']", "&#39;")

  upd_stmt02 <-
    sprintf("update wp_posts set post_content = convert(cast(convert('%s' using latin1) as binary) using utf8mb4) where id = %i;",
            sql_gidstekst,
            dsSql01$cz_id)

  try_err_upd02 <- FALSE
  try_err_upd02_msg <- NULL
  
  tryCatch({
    dbExecute(wp_conn, upd_stmt02)
  },
  error = function(db_err) {
    try_err_upd02 <<- TRUE
    try_err_upd02_msg <<-db_err
  })
  
  if (try_err_upd02) {
    flog.error(try_err_upd02_msg, name = "wpos")
    n_upd_errs <- 1
    
  } else {
    flog.info("Gids bijgewerkt: %s", pl_name, name = "wpos")
  }
  
  return(n_upd_errs)
}
