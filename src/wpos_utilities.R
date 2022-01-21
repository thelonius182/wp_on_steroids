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

grh_conn_sts_valid <- function() {
  
  result <- TRUE
  grh_conn_sts_err <- FALSE
  grh_conn_sts_err_msg <- NULL
  
  sql_result <- tryCatch(
    dsSql01 <- dbGetQuery(grh_conn, "show status where Variable_name = 'Connections';"),
    error = function(db_err) {
      grh_conn_sts_err <<- TRUE
      grh_conn_sts_err_msg <<- db_err
    }
  )
  
  if (grh_conn_sts_err) {
    flog.error(grh_conn_sts_err_msg, name = "wpos")
    result <- FALSE
  }
  
  return(result)
}

get_wp_conn <- function(wp_env) {
  # TEST
  wp_env <- "dev"
  # TEST
  
  wp_db_name <- paste0("wp", wp_env, "_mariadb")
  grh_conn <- NULL
  
  while (TRUE) {
    # try to connect unless end of the show
    if (now(tzone = "Europe/Amsterdam") > wpos_stop) {
      flog.info("show is over: stop", name = "wpos")
      break
    }
    
    grh_conn_err <- FALSE
    grh_conn_err_msg <- NULL
    
    grh_conn <- tryCatch(
      dbConnect(odbc::odbc(), wp_db_name, timeout = 10),
      error = function(db_err) {
        grh_conn_err <<- TRUE
        grh_conn_err_msg <<- db_err
      }
    )
    
    if (grh_conn_err) {
      flog.error(
        "couldn't connect to greenhost WP-database on %s (S4-check); retry... (msg=%s)",
        wp_env,
        grh_conn_err_msg,
        name = "wpos"
      )
      next
    }
    
    if (typeof(grh_conn) != "S4") {
      flog.error("couldn't connect to greenhost WP-database on %s (S4-check); retry...",
                 wp_env,
                 name = "wpos")
      next
    }
    
    # connection established
    flog.info("connected to greenhost WP-database on %s", wp_env, name = "wpos")
    break
  }

  return(grh_conn)
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

  db_upd02_err <- FALSE
  db_upd02_err_msg <- NULL
  
  tryCatch({
    dbExecute(wp_conn, upd_stmt02)
  },
  error = function(db_err) {
    db_upd02_err <<- TRUE
    db_upd02_err_msg <<-db_err
  })
  
  if (db_upd02_err) {
    flog.error(db_upd02_err_msg, name = "wpos")
    n_upd_errs <- 1
    
  } else {
    flog.info("Gids bijgewerkt: %s", pl_name, name = "wpos")
  }
  
  return(n_upd_errs)
}
