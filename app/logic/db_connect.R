box::use(
  pool[poolCreate, dbPool],
  RPostgres[Postgres],
  DBI[dbConnect],
  shinyalert[shinyalert],
  config,
)

#' @export
create_con <- function() {

  bd <- config$get('bd')

  dbConnect(Postgres(),
            dbname = bd$name,
            host = bd$host,
            user = bd$user,
            password = bd$pwd)
}

#' @export
create_db_pool <- function() {

  poolCreate(create_con, minSize = 2, idleTimeout = 600)

}


#' @export
pool_db <- function() {

  bd <- config$get('bd')

  dbPool(
    Postgres(),
    dbname = bd$name,
    host = bd$host,
    user = bd$user,
    password = bd$pwd
  )

}
