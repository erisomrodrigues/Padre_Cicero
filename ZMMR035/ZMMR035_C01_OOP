*&---------------------------------------------------------------------*
*& Include          ZMMR035_C01
*&---------------------------------------------------------------------*
CLASS zcl_main_report IMPLEMENTATION.
  METHOD init.
    select_dados( ).
    show_data( ).
  ENDMETHOD.

  "Buscando todos os fornecedores dos pedidos
  METHOD select_dados.
    SELECT DISTINCT ekko~lifnr,
                lfa1~name1
        FROM        ekko
        INNER JOIN  ekpo
        ON          ekpo~ebeln = ekko~ebeln
        INNER JOIN  lfa1
        ON          lfa1~lifnr = ekko~lifnr
        INNER JOIN  mara
        ON          mara~matnr = ekpo~matnr
        INNER JOIN  makt
        ON          makt~matnr = mara~matnr
        INNER JOIN  mard
        ON          mard~matnr = mara~matnr
        WHERE       ekko~bsart  IN ('ZATA','ZTIN','ZVAR','ZADV')
        AND         makt~spras ='P'
        AND         lfa1~lifnr            IN  @so_lifnr
        AND         mara~bismt            IN  @so_bismt
        AND         mara~matnr            IN  @so_matnr
        AND         mara~mfrpn            IN  @so_mfrpn
        AND         mara~matkl            IN  @so_matkl
        AND         mara~zzcategoria      IN  @so_categ
        AND         mara~zzsubcategoria   IN  @so_subct
        AND         mara~mstae            IN  @so_mstae
        AND         mara~prdha            IN  @so_prdha
        AND         mard~werks            IN  @so_werks
        GROUP BY    mara~matnr, makt~maktx, mara~mfrpn,
                    ekko~lifnr, lfa1~name1, mara~bismt,
                    mara~matkl
        INTO TABLE @lt_checkbox.

    IF sy-subrc <> 0.
      MESSAGE 'SEM DADOS' TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.

  METHOD show_data.
    TRY.
        cl_salv_table=>factory(
*          EXPORTING
*            list_display   =     'X'
*    r_container    =
*    container_name =
          IMPORTING
            r_salv_table   = lo_salv
          CHANGING
            t_table        = lt_checkbox ).

        "Otimizando o tamanho das colunas
        lo_columns = lo_salv->get_columns( ).
        lo_columns->set_optimize( abap_true ).
*        lo_column = lo_columns->get_column( 'CHECKBOX' ).
*        lo_column->set_short_text( ' ' ).

        "Setando o nome da coluna de Nome Fornecedor
        lo_column = lo_columns->get_column( columnname = 'NAME1' ).
        lo_column->set_short_text( 'Nom. Forn' ).
        lo_column->set_medium_text( 'Nom. Fornecedor' ).
        lo_column->set_long_text( 'Nome Fornecedor' ).

        "Setando o nome da coluna de Código do fornecedor
        lo_column = lo_columns->get_column( columnname = 'LIFNR' ).
        lo_column->set_short_text( 'Cód. Forn' ).
        lo_column->set_medium_text( 'Cod. Fornecedor' ).
        lo_column->set_long_text( 'Código do Fornecedor' ).

        "Chamando o ALV
        lo_salv->display( ).

      CATCH cx_salv_msg.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
