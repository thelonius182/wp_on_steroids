update_wp <- function(cz_playlist) {
  flog.info(paste("file updated:", playlist.md5), name = "wpos")
}

get_wp_conn <- function(wp_env) {
  db_type <- wp_env
  db_host <- key_get(service = paste0("sql-wp", db_type, "_host"))
  db_user <- key_get(service = paste0("sql-wp", db_type, "_user"))
  db_password <- key_get(service = paste0("sql-wp", db_type, "_pwd"))
  db_name <- key_get(service = paste0("sql-wp", db_type, "_db"))
  db_port <- 3306
  db_table <- "cz.wp_posts"
  
  result <- tryCatch( {
    wp_conn_prd <- dbConnect(drv = MySQL(), user = db_user, password = db_password,
                             dbname = db_name, host = db_host, port = db_port)
    },
    error = function(cond) {
      flog.error("Wordpress database onbereikbaar (dev: check PuTTY)", name = "wpos")
      return("connection-error")
    }
  )
  return(result)
}
