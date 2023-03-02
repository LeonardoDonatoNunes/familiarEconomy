box::use(
  echarts4r,
  dplyr[filter, pull, mutate, select, add_row, group_by, summarise, ungroup],
  glue[glue],
)

box::use(
  app/logic/db_connect,
  app/logic/dados,
  app/logic/aux,
)

conectar_db <- FALSE

if (conectar_db) {

  db_pool <- db_connect$create_db_pool()

}

#' @export
receita_despesa <- function(db_pool, ano_ref = aux$ano_ultimo_mes(), mes_ref = aux$ultimo_mes()) {

  df <- dados$get_pagamento(db_pool, ano_ref = ano_ref, mes_ref = as.numeric(mes_ref))

  df_aux <-
    df |>
    filter(mensal) |>
    group_by(ano_ref, mes_ref) |>
    summarise(valor = sum(valor)) |>
    ungroup() |>
    mutate(data = as.Date(glue("{ano_ref}-{mes_ref}-01")))

  datas <- seq(df_aux$data, df_aux$data + months(6), by = "month")

  df_aux <-
    data.frame(
      data = datas,
      valor = df_aux$valor,
      receitas = 3500
    )

  df_parcelas <- df |>
    filter(!mensal, parcela_numero < numero_parcelas) |>
    mutate(parcelas = numero_parcelas - (1-parcela_numero)) |>
    select(parcelas, valor)


  for (i in 1:nrow(df_parcelas)) {

    npar <- df_parcelas[i,]$parcelas
    valor <- df_parcelas[i,]$valor
    zeros <- nrow(df_aux) - npar
    vetor <- c(rep(valor, npar), rep(0, zeros))

    df_aux <-
      df_aux |>
      mutate(valor = valor + vetor)

  }


  df_aux |>
    echarts4r$e_charts(data) |> # initialise and set x
    echarts4r$e_line(valor, smooth = TRUE) |>
    echarts4r$e_labels() |>
    echarts4r$e_axis_labels(x = "Data", y = "Valor (R$)") |>
    echarts4r$e_area(receitas) |>  # add area
    echarts4r$e_title("Despesas Próximos 6 meses") |>  # Add title & subtitle
    echarts4r$e_theme("infographic") |>  # theme
    echarts4r$e_legend(right = 0) |>  # move legend to the bottom
    echarts4r$e_tooltip(trigger = "axis") # tooltip
}
