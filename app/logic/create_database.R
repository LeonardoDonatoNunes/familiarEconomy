box::use(
  DBI[dbExecute],
  pool[poolCheckout, poolReturn],
)


if (FALSE) {

  box::use(app/logic/db_connect)

  db_pool <- db_connect$pool_db()

}



#' @export
create_table_usuario <- function(db_pool) {

  con <- poolCheckout(db_pool)

  dbExecute(con, 'drop table if exists usuario cascade;')
  dbExecute(con, '
    CREATE TABLE usuario (
      id serial NOT NULL,
      nome varchar NOT NULL,
      CONSTRAINT usuario_pk PRIMARY KEY (id),
      CONSTRAINT usuario_un UNIQUE (id)
    );
    ')

  dbExecute(con, 'CREATE INDEX usuario_id_idx ON usuario (id);')

  poolReturn(con)

}

#' @export
create_table_receita <- function(db_pool) {

  con <- poolCheckout(db_pool)

  dbExecute(con, 'drop table if exists receita cascade;')
  dbExecute(con, '
            CREATE TABLE receita (
            	id serial NOT NULL,
            	nome varchar NULL,
            	usuario_id int NULL,
            	mensal boolean NULL,
            	valor real NULL,
              data_ini date NULL,
              data_fim date NULL,
            	CONSTRAINT receita_pk PRIMARY KEY (id),
            	CONSTRAINT receita_un UNIQUE (id),
            	CONSTRAINT receita_fk FOREIGN KEY (usuario_id) REFERENCES usuario(id) ON DELETE CASCADE ON UPDATE CASCADE
    );')

  dbExecute(con, 'CREATE INDEX receita_id_idx ON receita (id);')


  poolReturn(con)
}

#' @export
create_table_categoria_despesa_geral <- function(db_pool) {

  con <- poolCheckout(db_pool)

  dbExecute(con, 'drop table if exists categoria_despesa_geral cascade;')
  dbExecute(con, '
    CREATE TABLE categoria_despesa_geral (
    	id serial NOT NULL,
    	nome varchar NULL,
    	mensal boolean NULL,
    	dividido boolean NULL,
    	descricao varchar NULL,
    	CONSTRAINT categoria_despesa_geral_pk PRIMARY KEY (id),
    	CONSTRAINT categoria_despesa_geral_un UNIQUE (id));
              ')
  dbExecute(con, 'CREATE INDEX categoria_despesa_geral_id_idx ON categoria_despesa_geral (id);')

  poolReturn(con)

}

#' @export
create_table_categoria_despesa_especifica <- function(db_pool) {

  con <- poolCheckout(db_pool)

  dbExecute(con, 'drop table if exists categoria_despesa_especifica cascade;')
  dbExecute(con, '
            CREATE TABLE categoria_despesa_especifica (
            	id serial NOT NULL,
            	nome varchar NULL,
            	cat_geral_id int NULL,
            	usuario_id int NULL,
            	descricao varchar NULL,
            	CONSTRAINT categoria_despesa_especifica_pk PRIMARY KEY (id),
            	CONSTRAINT categoria_despesa_especifica_un UNIQUE (id),
            	CONSTRAINT categoria_despesa_especifica_fk FOREIGN KEY (usuario_id) REFERENCES usuario(id) ON DELETE CASCADE ON UPDATE CASCADE,
            	CONSTRAINT categoria_despesa_especifica_fk_1 FOREIGN KEY (cat_geral_id) REFERENCES categoria_despesa_geral(id) ON DELETE CASCADE ON UPDATE CASCADE);
          ')

  dbExecute(con, 'CREATE INDEX categoria_despesa_especifica_id_idx ON categoria_despesa_especifica (id);')


  poolReturn(con)

}

#' @export
create_table_pagamento <- function(db_pool) {

  con <- poolCheckout(db_pool)

  dbExecute(con, 'drop table if exists pagamento cascade;')
  dbExecute(con, '
            CREATE TABLE pagamento (
              	id serial NOT NULL,
                usuario_id int NULL,
              	cat_espec_id int NULL,
              	ano_ref int NULL,
              	mes_ref int NULL,
              	valor real NULL,
              	numero_parcelas int NULL,
              	parcela_numero int NULL,
              	CONSTRAINT pagamento_pk PRIMARY KEY (id),
              	CONSTRAINT pagamento_un UNIQUE (id),
                CONSTRAINT pagamento_usuario_fk FOREIGN KEY (usuario_id) REFERENCES usuario (id) ON DELETE CASCADE ON UPDATE CASCADE,
	              CONSTRAINT pagamento_cat_espec_fk FOREIGN KEY (cat_espec_id) REFERENCES categoria_despesa_especifica(id) ON DELETE CASCADE ON UPDATE CASCADE
      );')

  dbExecute(con, 'CREATE INDEX pagamento_id_idx ON pagamento (id);')


  poolReturn(con)
}



#' @export
create_data_base <- function(db_pool) {
  create_table_usuario(db_pool)
  create_table_receita(db_pool)
  create_table_categoria_despesa_geral(db_pool)
  create_table_categoria_despesa_especifica(db_pool)
  create_table_pagamento(db_pool)
}
