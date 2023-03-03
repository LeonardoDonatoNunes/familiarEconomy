box::use(
  pool[poolCreate, dbPool],
  RPostgres[Postgres],
  DBI[dbConnect],
  shinyalert[shinyalert],
)

#' @export
create_con <- function() {

  dbConnect(Postgres(),
            dbname = Sys.getenv("DB_POSTGRES_NAME"),
            host = Sys.getenv("DB_POSTGRES_HOST"),
            user = Sys.getenv("DB_POSTGRES_USER_NAME"),
            password = Sys.getenv("DB_POSTGRES_USER_PWD"),
            port= Sys.getenv("DB_POSTGRES_PORT"))
}

#' @export
create_db_pool <- function() {

  poolCreate(create_con, minSize = 2, idleTimeout = 600)

}


#' @export
pool_db <- function() {

  dbPool(
    Postgres(),
    dbname = Sys.getenv("DB_POSTGRES_NAME"),
    host = Sys.getenv("DB_POSTGRES_HOST"),
    user = Sys.getenv("DB_POSTGRES_USER_NAME"),
    password = Sys.getenv("DB_POSTGRES_USER_PWD"),
    port= Sys.getenv("DB_POSTGRES_PORT")
  )

}
