box::use(
  reactable[colDef],
  dplyr[relocate, mutate, select],
  glue[glue],
)

box::use(
  app/logic/dados,
  app/logic/aux_geral,
)


#' @export
tbl_usuario <- function(db_pool, df = NULL){

  if (is.null(df)) {
    df <- dados$get_usuario(db_pool)
  }

  df |>
    mutate(add = !add) |>
    select(-new) |>
    aux_geral$reactable_padrao(
      id = colDef(show = FALSE),
      nome = colDef(name = "Nome do usuário", align = 'left'),
      add = colDef(name = "Salvo", cell = aux_geral$create_check_col, style = aux_geral$lgl_style, minWidth = 50, maxWidth = 100),
      .selection = colDef(show = FALSE)
    )
}

#' @export
tbl_receita <- function(db_pool, df = NULL){

  if (is.null(df)) {
    df <- dados$get_receita(db_pool)
  }

  df |>
    mutate(add = !add) |>
    select(-new) |>
    relocate(valor, .after = 'usuario') |>
    aux_geral$reactable_padrao(
      .selection = colDef(show = FALSE),
      id = colDef(show = FALSE),
      usuario_id = colDef(show = FALSE),
      nome = colDef(name = "Nome da receita", align = 'left', footer = "Total"),
      mensal = colDef(cell = aux_geral$create_check_col, style = aux_geral$lgl_style, name = "Receita mensal"),
      valor = colDef(cell = aux_geral$real_fmt, name = "Valor", footer = aux_geral$real_fmt(sum(df$valor))),
      data_ini = colDef(name = 'Início da receita'),
      data_fim = colDef(name = 'Fim da receita', na = "Receita atual"),
      add = colDef(name = "Salvo", cell = aux_geral$create_check_col, style = aux_geral$lgl_style, minWidth = 50, maxWidth = 100),
      usuario = colDef(name = "Usuário")
    )
}

#' @export
tbl_pagamentos <- function(db_pool, df = NULL) {

  if (is.null(df)) {
    df <- dados$get_pagamento(db_pool)
  }

  df_usuario <- dados$get_usuario(db_pool)

  df_aux <-
    df |>
    mutate(add = !add) |>
    mutate(
      valor_parcial = ifelse(dividido == 1, valor/nrow(df_usuario), valor ),
      parcelas = glue("{parcela_numero}/{numero_parcelas}"),
      parcelas = ifelse(mensal, "M", parcelas)) |>
    select(geral, especifica, mensal, dividido, ano_ref, mes_ref, usuario, parcelas,valor_parcial, valor, add)

  df_aux |>
    aux_geral$reactable_padrao(
      .selection = colDef(show = FALSE),
      ano_ref = colDef(name = "Ano"),
      mes_ref = colDef(cell = aux_geral$lbl_mes, name = "Mes"),
      parcelas = colDef(cell = aux_geral$set_class_parcela, name = "P", minWidth = 60, maxWidth = 70),
      valor = colDef(cell = aux_geral$real_fmt, name = "Valor", footer = aux_geral$real_fmt(sum(df_aux$valor))),
      valor_parcial = colDef(cell = aux_geral$real_fmt, name = "Valor parcial", footer = aux_geral$real_fmt(sum(df_aux$valor_parcial))),
      usuario = colDef(name = "Usuário"),
      geral = colDef(name = "Categoria geral", footer = 'Total', align = 'left'),
      especifica = colDef(name = "Categoria específica"),
      dividido = colDef(cell = aux_geral$create_check_col, style = aux_geral$lgl_style, name = "Dividido"),
      mensal = colDef(cell = aux_geral$create_check_col, style = aux_geral$lgl_style, name = "Mensal"),
      add = colDef(name = "Salvo", cell = aux_geral$create_check_col, style = aux_geral$lgl_style, minWidth = 50, maxWidth = 100)
    )

}



#' @export
tbl_cat_geral <- function(db_pool, df = NULL) {

  if (is.null(df)) {
    df <- dados$get_cat_geral(db_pool)
  }

  df |>
    mutate(add = !add) |>
    select(-new) |>
    select(-id) |>
    aux_geral$reactable_padrao(
      .selection = colDef(show = FALSE),
      nome = colDef(minWidth = 100, maxWidth = 150, name = "Nome", align = 'left'),
      mensal = colDef(minWidth = 100, maxWidth = 150, cell = aux_geral$create_check_col, style = aux_geral$lgl_style, name = "Gasto mensal"),
      dividido = colDef(minWidth = 100, maxWidth = 150, cell = aux_geral$create_check_col, style = aux_geral$lgl_style, name = "Dividido entre usuários"),
      descricao = colDef(name = "Descrição do tipo geral de gasto", cell = aux_geral$with_tooltip_by_size),
      add = colDef(name = "Salvo", cell = aux_geral$create_check_col, style = aux_geral$lgl_style, minWidth = 50, maxWidth = 100)
    )

}

#' @export
tbl_cat_espec <- function(db_pool, df = NULL) {

  if (is.null(df)) {
    df <- dados$get_cat_espec(db_pool)
  }

  df |>
    mutate(add = !add) |>
    select(-new) |>
    select(nome, nome_cat_geral, usuario, dividido, mensal, descricao, add) |>
    aux_geral$reactable_padrao(
      .selection = colDef(show = FALSE),
      nome = colDef(minWidth = 50, maxWidth = 150, name = "Nome", align = 'left'),
      nome_cat_geral = colDef(minWidth = 50, maxWidth = 150, name = "Categoria geral"),
      usuario = colDef(minWidth = 100, maxWidth = 200, name = "Usuário"),
      dividido = colDef(minWidth = 100, maxWidth = 150, cell = aux_geral$create_check_col, style = aux_geral$lgl_style, name = "Dividido"),
      mensal = colDef(minWidth = 100, maxWidth = 150, cell = aux_geral$create_check_col, style = aux_geral$lgl_style, name = "Mensal"),
      descricao = colDef(name = "Descrição", cell = aux_geral$with_tooltip_by_size),
      add = colDef(name = "Salvo", cell = aux_geral$create_check_col, style = aux_geral$lgl_style, minWidth = 50, maxWidth = 100)
    )

}
