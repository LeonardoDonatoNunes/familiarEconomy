box::use(
  DBI[dbExecute, dbGetQuery],
  glue[glue, glue_collapse],
  purrr[map_chr],
  stats[setNames],
  pool[poolCreate, poolCheckout, poolReturn],
  dbx[dbxUpdate, dbxInsert],
  dplyr[filter, select]
)


box::use(
  app/logic/aux
)



#' @export
upsert_df <- function(db_pool, nome_tabela, df) {

  con <- poolCheckout(db_pool)
  resp <- list(status = FALSE)

  tryCatch({

    aux_insert <-
      df |>
      filter(is.na(id)) |>
      select(-id)

    aux_update <-
      df |>
      filter(!is.na(id))

    if (nrow(aux_insert) > 0) {

      dbxInsert(con, nome_tabela, records = aux_insert)

    }

    if (nrow(aux_update) > 0) {

      dbxUpdate(con, nome_tabela, records = aux_update, where_cols = 'id')

    }

    resp <- list(status = TRUE)

  }, error = function(e) {

    resp <<- list(status = FALSE, error = paste0("Erro ao fazer upserte de ", nome_tabela, ". ERRO: ", e))

  }, finally = {
    poolReturn(con)
  })

  return(resp)

}

#' @export
delete_from_tabela <- function(db_pool, tabela, id) {

  con <- poolCheckout(db_pool)
  dbExecute(con, glue("delete from {tabela} where id = {id}"))
  poolReturn(con)

}


#' @export
get_usuario <- function(db_pool,id = NULL, nome = NULL) {

  con <- poolCheckout(db_pool)

  where <- aux$create_where(c(id = id, nome = nome))
  statement <- glue::glue("select *, FALSE as add, FALSE as new from usuario{where};")
  resp <- DBI::dbGetQuery(con, statement)

  poolReturn(con)

  return(resp)
}


#' @export
get_receita <- function(db_pool, id = NULL, nome = NULL, ano_ref = NULL, mes_ref = NULL) {

  con <- poolCheckout(db_pool)

  where <- aux$create_where(c(r.id = id, r.nome = nome, r.ano_ref = ano_ref, r.mes_ref = mes_ref))
  statement <- glue("
        select r.*,
               u.nome as usuario
          from receita r
     left join usuario u on r.usuario_id = u.id{where};")

  resp <- dbGetQuery(con, statement)

  poolReturn(con)

  return(resp)
}

#' @export
get_cat_geral <- function(db_pool, id = NULL) {

  con <- poolCheckout(db_pool)

  where <- aux$create_where(c(id = id))

  statement <- glue("select *, FALSE as add, FALSE as new from categoria_despesa_geral{where}")


  resp <- dbGetQuery(con, statement)

  poolReturn(con)

  return(resp)

}


#' @export
get_cat_espec <- function(db_pool, id = NULL, cat_geral_id = NULL, usuario_id = NULL) {

  con <- poolCheckout(db_pool)

  where <- aux$create_where(c(cde.id = id, cde.cat_geral_id = cat_geral_id, cde.usuario_id = usuario_id))

  statement <- glue("
          select cde.*,
                 cdg.nome as nome_cat_geral,
                 cdg.dividido,
                 cdg.mensal,
                 u.nome as usuario,
                 FALSE as add,
                 FALSE as new
            from categoria_despesa_especifica cde
       left join usuario u on cde.usuario_id = u.id
       left join categoria_despesa_geral cdg on cde.cat_geral_id = cdg.id{where}")


  resp <- dbGetQuery(con, statement)

  poolReturn(con)

  return(resp)

}

#' @export
get_pagamento <- function(db_pool, id = NULL, cat_espec_id = NULL, ano_ref = NULL, mes_ref = NULL, usuario_id = NULL) {

  con <- poolCheckout(db_pool)

  where <- aux$create_where(c(p.id = id, p.cat_espec_id = cat_espec_id, p.ano_ref = ano_ref, p.mes_ref = mes_ref, p.usuario_id = usuario_id))

  statement <- glue("
          select p.*,
                 u.nome as usuario,
                 cde.cat_geral_id,
                 cdg.nome as geral,
                 cde.nome as especifica,
                 cdg.dividido,
                 cdg.mensal,
                 FALSE as add,
                 FALSE as new
            from pagamento p
            left join usuario u on p.usuario_id = u.id
            left join categoria_despesa_especifica cde on p.cat_espec_id = cde.id
            left join categoria_despesa_geral cdg on cde.cat_geral_id = cdg.id{where}")


  resp <- dbGetQuery(con, statement)

  poolReturn(con)

  return(resp)

}


#' @export
vct_cat_geral <- function(db_pool, df = NULL) {

  if (is.null(df)) {

    df <- get_cat_geral(db_pool)

  }

  df$id |> setNames(df$nome)

}

#' @export
vct_cat_espec <- function(db_pool, df = NULL) {

  if (is.null(df)) {
    df <- get_cat_espec(db_pool)
  }

  df$id |> setNames(df$nome)

}
