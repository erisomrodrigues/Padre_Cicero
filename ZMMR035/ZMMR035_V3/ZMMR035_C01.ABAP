*&---------------------------------------------------------------------*
*& Include          ZMMR035_C01
*&---------------------------------------------------------------------*
CLASS zcl_main_report IMPLEMENTATION.
  METHOD init.
    select_dados( ).
    process_data( ).
    show_data( ).
  ENDMETHOD.
  "Buscando todos os fornecedores para PopUp
  METHOD select_dados.
    SELECT lfa1~lifnr,
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
           GROUP BY    lfa1~lifnr, lfa1~name1
           INTO CORRESPONDING FIELDS OF TABLE @lt_checkbox.

    CLEAR ls_dcat.
    ls_dcat-reptext_ddic = ' '.
    ls_dcat-fieldname = 'CHECK'.
    ls_dcat-tabname = 'LT_CHECKBOX'.
    ls_dcat-outputlen = '1'.
    APPEND ls_dcat TO ls_lcat.

    CLEAR ls_dcat.
    ls_dcat-reptext_ddic = 'CÓD.FORNECEDOR'.
    ls_dcat-fieldname = 'LIFNR'.
    ls_dcat-tabname = 'LT_CHECKBOX'.
    ls_dcat-outputlen = '18'.
    APPEND ls_dcat TO ls_lcat.

    CLEAR ls_dcat.
    ls_dcat-reptext_ddic = ' FORNECEDOR'.
    ls_dcat-fieldname = 'NAME1'.
    ls_dcat-tabname = 'LT_CHECKBOX'.
    ls_dcat-outputlen = '40'.
    APPEND ls_dcat TO ls_lcat.

    CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
      EXPORTING
        i_title              = 'TESTE POPUP'
        i_selection          = 'X'
        i_allow_no_selection = 'X'
        i_zebra              = 'X'
*       I_SCREEN_START_COLUMN         = 0
*       I_SCREEN_START_LINE  = 0
*       I_SCREEN_END_COLUMN  = 0
*       I_SCREEN_END_LINE    = 0
        i_checkbox_fieldname = 'CHECK'
        i_linemark_fieldname = 'X'
*       I_SCROLL_TO_SEL_LINE = 'X'
        i_tabname            = 'LT_CHECKBOX'
*       I_STRUCTURE_NAME     =
        it_fieldcat          = ls_lcat
*       IT_EXCLUDING         =
*       I_CALLBACK_PROGRAM   =
*       I_CALLBACK_USER_COMMAND       =
*       IS_PRIVATE           =
*     IMPORTING
**         ES_SELFIELD          =
*       e_exit               = l_exit
      TABLES
        t_outtab             = lt_checkbox
      EXCEPTIONS
        program_error        = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    DATA: wa_r_checkbox LIKE LINE OF r_sel_checkbox.
    FIELD-SYMBOLS: <lt_checkbox> LIKE LINE OF lt_checkbox.

    wa_r_checkbox-sign = 'I'.
    wa_r_checkbox-option = 'EQ'.
    LOOP AT lt_checkbox ASSIGNING <lt_checkbox> WHERE check = 'X'.
      wa_r_checkbox-low = <lt_checkbox>-lifnr.
      APPEND wa_r_checkbox TO r_sel_checkbox.
    ENDLOOP.

    SELECT DISTINCT mara~matnr,
                    mara~mfrpn,
                    makt~maktx,
                    ekko~lifnr,
                    lfa1~name1,
                    mara~bismt,
                    mara~matkl
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
            AND         lfa1~lifnr            IN  @r_sel_checkbox
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
            INTO TABLE @lt_pedido.

*    IF line_exists( lt_checkbox[ lifnr = lt_pedido ).

*      ENDIF.

*   LOOP AT lT_pedido TO ls_pedido.
*     ls_checkbox-lifnr = lS_pedido-lifnr.
*     ls_checkbox-name1 = ls_pedido-name1.
*   ENDLOOP.

    SORT lt_pedido ASCENDING BY matnr.
  ENDMETHOD.

  METHOD process_data.
    "Definindo as datas
    lv_dtatu = sy-datum.
    lv_dtfin = sy-datum.

    lv_dtatu = lv_dtatu - 1.

    lv_dtam = lv_dtatu(6) - 5.

    lv_dtfin(6) = lv_dtam.
    lv_dtfin+6(2) = 01.

    "Inserindo informações dos pedidos
    LOOP AT lt_pedido INTO ls_pedido.
      TRY.
          ls_final-matnr = ls_pedido-matnr.
          ls_final-mfrpn = ls_pedido-mfrpn.
          ls_final-maktx = ls_pedido-maktx.
          ls_final-lifnr = ls_pedido-lifnr.
          ls_final-name1 = ls_pedido-name1.
          ls_final-bismt = ls_pedido-bismt.
          ls_final-matkl = ls_pedido-matkl.
          APPEND ls_final TO lt_final.

          SELECT  SINGLE SUM( menge ) AS qtdtl
                  FROM mseg
                  WHERE bwart IN ('261','601')
                  AND matnr = @ls_pedido-matnr
                  AND budat_mkpf BETWEEN @lv_dtfin AND @lv_dtatu
                  INTO @DATA(lt_msaid).

          ls_final-qtdtl = lt_msaid / 5.

          SELECT DISTINCT ekpo~matnr,
                          ekko~aedat AS dtucp,
                          division( ekpo~brtwr, ekpo~menge, 2 ) AS vrucp
                          FROM ekko
                          INNER JOIN ekpo
                          ON ekko~ebeln = ekpo~ebeln
                          WHERE ekko~bsart IN ( 'ZATA', 'ZTIN', 'ZVAR', 'ZAVD' )
                          AND ekpo~matnr = @ls_pedido-matnr
                          GROUP BY ekpo~matnr, ekko~aedat,brtwr, menge
                          INTO TABLE @DATA(lt_dtucp).

          SORT lt_dtucp DESCENDING BY dtucp.

          "Inserindo as datas das ultimas compras
          READ TABLE lt_dtucp INTO DATA(ls_dtucp) INDEX 1.
          ls_final-dtucp = ls_dtucp-dtucp.
          ls_final-vrucp = ls_dtucp-vrucp.

        CATCH cx_root.
      ENDTRY.
    ENDLOOP.

    DELETE lt_final WHERE vrucp IS INITIAL.

    LOOP AT lt_final ASSIGNING FIELD-SYMBOL(<fs_final>) GROUP BY ( bismt = <fs_final>-bismt ) ASSIGNING FIELD-SYMBOL(<fs_t_group>).
      FREE ls_ganhador.

      LOOP AT GROUP <fs_t_group> ASSIGNING FIELD-SYMBOL(<fs_group>).
        IF ls_ganhador IS INITIAL.
          ls_ganhador = <fs_group>.
        ELSEIF <fs_group>-vrucp < ls_ganhador-vrucp.
          ls_ganhador = <fs_group>.
        ENDIF.
      ENDLOOP.

      READ TABLE lt_final ASSIGNING FIELD-SYMBOL(<fs_ganhador>) WITH KEY matnr = ls_ganhador-matnr
                                                                         mfrpn = ls_ganhador-mfrpn
                                                                         maktx = ls_ganhador-maktx
                                                                         lifnr = ls_ganhador-lifnr.
      IF sy-subrc = 0.
        <fs_ganhador>-ganhador = ls_ganhador-name1.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD show_data.






    TRY.
        cl_salv_table=>factory(
EXPORTING
list_display   = if_salv_c_bool_sap=>false
IMPORTING
r_salv_table = lo_salv
CHANGING
t_table      = lt_final
).

        "Otimizando o tamanho das colunas
        lo_columns = lo_salv->get_columns( ).
        lo_columns->set_optimize( abap_true ).

        "Setando o nome da coluna de Código Material
        lo_column = lo_columns->get_column( columnname = 'MATNR' ).
        lo_column->set_short_text( 'Cód. Mat' ).
        lo_column->set_medium_text( 'Cód. Material' ).
        lo_column->set_long_text( 'Código Material' ).

        "Setando o nome da coluna de Referência
        lo_column = lo_columns->get_column( columnname = 'MFRPN' ).
        lo_column->set_short_text( 'Ref' ).
        lo_column->set_medium_text( 'Referência' ).
        lo_column->set_long_text( 'Referência' ).

        "Setando o nome da coluna de Descrição Material
        lo_column = lo_columns->get_column( columnname = 'MAKTX' ).
        lo_column->set_short_text( 'Desc. Mat.' ).
        lo_column->set_medium_text( 'Desc. Material' ).
        lo_column->set_long_text( 'Descrição Material' ).

        "Setando o nome da coluna de Código do fornecedor
        lo_column = lo_columns->get_column( columnname = 'LIFNR' ).
        lo_column->set_short_text( 'Cód. Forn' ).
        lo_column->set_medium_text( 'Cod. Fornecedor' ).
        lo_column->set_long_text( 'Código do Fornecedor' ).

        "Setando o nome da coluna de Nome Fornecedor
        lo_column = lo_columns->get_column( columnname = 'NAME1' ).
        lo_column->set_short_text( 'Nom. Forn' ).
        lo_column->set_medium_text( 'Nom. Fornecedor' ).
        lo_column->set_long_text( 'Nome Fornecedor' ).

        "Setando o nome da coluna de Agrupador
        lo_column = lo_columns->get_column( columnname = 'BISMT' ).
        lo_column->set_short_text( 'Agrupador' ).
        lo_column->set_medium_text( 'Agrupador').
        lo_column->set_long_text( 'Agrupador').

        "Setando o nome da coluna de Categoria
        lo_column = lo_columns->get_column( columnname = 'MATKL' ).
        lo_column->set_short_text('Grp. Merc').
        lo_column->set_medium_text('Grupo merc').
        lo_column->set_long_text('Grupo de mercadoria').

        "Setando o nome da coluna de Data da ultima compra
        lo_column = lo_columns->get_column( columnname = 'DTUCP' ).
        lo_column->set_short_text( 'Dt. Comp' ).
        lo_column->set_medium_text( 'Dt Ultima Compra' ).
        lo_column->set_long_text( 'Data Ultima compra' ).

        "Setando o nome da coluna de Valor ultima compra
        lo_column = lo_columns->get_column( columnname = 'VRUCP' ).
        lo_column->set_short_text('Vr Comp').
        lo_column->set_medium_text('Vr.Ult Comp').
        lo_column->set_long_text('Valor ultima compra').

        "Setando o nome da coluna de Media da quantidade de saída
        lo_column = lo_columns->get_column( columnname = 'QTDTL' ).
        lo_column->set_short_text( 'Média' ).
        lo_column->set_medium_text( 'Média' ).
        lo_column->set_long_text( 'Média' ).

*      setando o nome da coluna de ganhador
        lo_column = lo_columns->get_column( columnname = 'GANHADOR' ).
        lo_column->set_short_text( 'Ganhador' ).
        lo_column->set_medium_text( 'Ganhador' ).
        lo_column->set_long_text( 'Ganhador' ).

        "Ativando todas as funções do ALV
        lo_funct = lo_salv->get_functions( ).
        lo_funct->set_all( abap_true ).

        "Mostrando o ALV para o usuário
        lo_salv->display( ).

      CATCH cx_root INTO DATA(lx_err).
        DATA(lv_txt) = lx_err->get_text( ).
        MESSAGE lv_txt TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
