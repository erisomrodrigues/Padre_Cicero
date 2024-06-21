*&---------------------------------------------------------------------*
*& Include          ZMMR035_C01
*&---------------------------------------------------------------------*
CLASS zcl_main_report IMPLEMENTATION.
  METHOD init.
    select_dados( ).
    process_data( ).
    show_data( ).
  ENDMETHOD.

  METHOD select_dados.

    DATA lv_cancel TYPE char1.

    "Buscando todos os fornecedores para PopUp
    SELECT DISTINCT mara~matnr,
                    mara~mfrpn,
                    makt~maktx,
                    lfa1~lifnr,
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
        GROUP BY    mara~matnr, makt~maktx, mara~mfrpn, lfa1~lifnr, lfa1~name1, mara~bismt, mara~matkl
        INTO CORRESPONDING FIELDS OF TABLE @gt_compras.

    LOOP AT gt_compras ASSIGNING FIELD-SYMBOL(<fs_compras>).
      wa_checkbox-lifnr = <fs_compras>-lifnr.
      wa_checkbox-name1 = <fs_compras>-name1.
      APPEND wa_checkbox TO gt_checkbox.
    ENDLOOP.

    SORT gt_checkbox ASCENDING BY name1 lifnr.

    DELETE ADJACENT DUPLICATES FROM gt_checkbox COMPARING lifnr.

    "Definições do Layout do PopUp
    IF gt_checkbox IS NOT INITIAL.
      CLEAR gs_dcat.
      gs_dcat-reptext_ddic = ' '.
      gs_dcat-fieldname = 'CHECK'.
      gs_dcat-tabname = 'GT_CHECKBOX'.
      gs_dcat-outputlen = '1'.
      APPEND gs_dcat TO gs_lcat.

      CLEAR gs_dcat.
      gs_dcat-reptext_ddic = 'COD.FORNECEDOR'.
      gs_dcat-fieldname = 'LIFNR'.
      gs_dcat-tabname = 'GT_CHECKBOX'.
      gs_dcat-outputlen = '18'.
      APPEND gs_dcat TO gs_lcat.

      CLEAR gs_dcat.
      gs_dcat-reptext_ddic = 'FORNECEDOR'.
      gs_dcat-fieldname = 'NAME1'.
      gs_dcat-tabname = 'GT_CHECKBOX'.
      gs_dcat-outputlen = '40'.
      APPEND gs_dcat TO gs_lcat.

      CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
        EXPORTING
          i_title              = 'Lista de Fornecedores'
          i_selection          = 'X'
          i_allow_no_selection = 'X'
          i_zebra              = 'X'
          i_checkbox_fieldname = 'CHECK'
          i_linemark_fieldname = 'X'
          i_tabname            = 'GT_CHECKBOX'
          it_fieldcat          = gs_lcat
        IMPORTING
          e_exit               = lv_cancel
        TABLES
          t_outtab             = gt_checkbox
        EXCEPTIONS
          program_error        = 1
          OTHERS               = 2.

      DATA: lr_checkbox LIKE LINE OF r_checkbox. "Range para armazenar os códigos dos fornecedores

      lr_checkbox-sign = 'I'.
      lr_checkbox-option = 'EQ'.

      DELETE gt_checkbox WHERE check <> 'X'.

      "Tratamento para quando selecionar mais de 4 registros
      IF lines( gt_checkbox ) > 4.
        MESSAGE 'Selecione Até 4 Registros' TYPE 'I'.
        FREE gt_checkbox.
        me->select_dados( ).
      ELSEIF lv_cancel = 'X'.
        CALL SELECTION-SCREEN '1000'.
        FREE lv_cancel.
      ENDIF.

      "Transferindo os Códigos do Fornecedores para o range
      LOOP AT gt_checkbox ASSIGNING FIELD-SYMBOL(<fs_checkbox>).
        lr_checkbox-low = <fs_checkbox>-lifnr.
        APPEND lr_checkbox TO r_checkbox.
      ENDLOOP.

    ENDIF.

    "Buscar os dados dos pedidos de compras
    SELECT DISTINCT matnr,
                    mfrpn,
                    maktx,
                    lifnr,
                    name1,
                    bismt,
                    matkl
            FROM @gt_compras AS at_pedido
            WHERE lifnr IN @r_checkbox
            INTO TABLE @gt_pedido.

    SORT gt_pedido ASCENDING BY bismt.
  ENDMETHOD.

  METHOD process_data.
    "Definindo as datas do SELECT de buscar a Media de saida
    DATA(lv_dtatu) = sy-datum.       "Data Atual
    DATA(lv_dtfin) = sy-datum.       "Data Final
    DATA(lv_dtam) = lv_dtatu(6) - 5. "Data Ano/Mês

    DATA: lv_ganhado TYPE ekpo-brtwr.
    DATA: lv_agrup TYPE mara-bismt.

    lv_dtatu = lv_dtatu - 1.
    lv_dtfin(6) = lv_dtam.
    lv_dtfin+6(2) = 01.

    FREE lv_agrup.

    "Inserindo informações a tabela final do ALV
    LOOP AT gt_pedido INTO wa_pedido.
      IF lv_agrup <> wa_pedido-bismt.
        APPEND INITIAL LINE TO gt_final ASSIGNING FIELD-SYMBOL(<fs_final>).
        lv_agrup = wa_pedido-bismt.
        FREE lv_ganhado.
      ENDIF.

      "Consultar a quantidade de saida dentro de 5 meses para montar a Media
      SELECT  SINGLE SUM( menge ) AS qtdtl
              FROM mseg
              WHERE bwart IN ('261','601')
              AND matnr = @wa_pedido-matnr
              AND budat_mkpf BETWEEN @lv_dtfin AND @lv_dtatu
              INTO @DATA(lt_msaid).

      "Buscando a data da ultima compra e quantidade de produtos
      SELECT DISTINCT ekpo~matnr,
                      ekko~aedat AS dtucp,
                      division( ekpo~brtwr, ekpo~menge, 2 ) AS vrucp
                      FROM ekko
                      INNER JOIN ekpo
                      ON ekko~ebeln = ekpo~ebeln
                      WHERE ekko~bsart IN ( 'ZATA', 'ZTIN', 'ZVAR', 'ZAVD' )
                      AND ekpo~matnr = @wa_pedido-matnr
                      GROUP BY ekpo~matnr, ekko~aedat,brtwr, menge
                      INTO TABLE @gt_dadoscp.

      SORT gt_dadoscp DESCENDING BY datcp.
      DELETE ADJACENT DUPLICATES FROM gt_dadoscp COMPARING matnr.

      READ TABLE gt_dadoscp INTO wa_dadoscp INDEX 1.
      wa_final-dtulcp1 = wa_dadoscp-datcp.
      wa_final-vrulcp1 = wa_dadoscp-valcp.
      wa_final-mdvend1 = lt_msaid / 5.

      IF lv_ganhado IS INITIAL.
        lv_ganhado = wa_dadoscp-valcp.
        <fs_final>-ganhado = wa_pedido-name1.
      ENDIF.

      IF wa_dadoscp-valcp < lv_ganhado.
        <fs_final>-ganhado = wa_pedido-name1.
      ENDIF.

      "Inserindo os valores a tabela final
      IF <fs_final>-nomfor1 IS INITIAL.
        <fs_final>-desc_mt = wa_pedido-maktx.
        <fs_final>-agrupad = wa_pedido-bismt.
        <fs_final>-nomfor1 = wa_pedido-name1.
        <fs_final>-codmat1 = wa_pedido-matnr.
        <fs_final>-dtulcp1 = wa_dadoscp-datcp.
        <fs_final>-vrulcp1 = wa_dadoscp-valcp.
        <fs_final>-mdvend1 = lt_msaid / 5.
      ELSEIF <fs_final>-nomfor2 IS INITIAL.
        <fs_final>-nomfor2 = wa_pedido-name1.
        <fs_final>-codmat2 = wa_pedido-matnr.
        <fs_final>-dtulcp2 = wa_dadoscp-datcp.
        <fs_final>-vrulcp2 = wa_dadoscp-valcp.
        <fs_final>-mdvend2 = lt_msaid / 5.
      ELSEIF <fs_final>-nomfor3 IS INITIAL.
        <fs_final>-nomfor3 = wa_pedido-name1.
        <fs_final>-codmat3 = wa_pedido-matnr.
        <fs_final>-dtulcp3 = wa_dadoscp-datcp.
        <fs_final>-vrulcp3 = wa_dadoscp-valcp.
        <fs_final>-mdvend3 = lt_msaid / 5.
      ELSE.
        <fs_final>-nomfor4 = wa_pedido-name1.
        <fs_final>-codmat4 = wa_pedido-matnr.
        <fs_final>-dtulcp4 = wa_dadoscp-datcp.
        <fs_final>-vrulcp4 = wa_dadoscp-valcp.
        <fs_final>-mdvend4 = lt_msaid / 5.
      ENDIF.
    ENDLOOP.

    DELETE gt_final WHERE vrulcp1 IS INITIAL.
  ENDMETHOD.

  METHOD show_data.

    TRY.
        cl_salv_table=>factory( EXPORTING
                                list_display   = if_salv_c_bool_sap=>false
                                IMPORTING
                                r_salv_table = lo_salv
                                CHANGING
                                t_table      = gt_final
                                ).

        "Otimizando o tamanho das colunas
        lo_columns = lo_salv->get_columns( ).
        lo_columns->set_optimize( abap_true ).

        "Definições do Layout do ALV final
        lo_column = lo_columns->get_column( columnname = 'DESC_MT' ).
        lo_column->set_short_text( 'DESC.M' ).
        lo_column->set_medium_text( 'DESC. MAT' ).
        lo_column->set_long_text( 'DESC. Material' ).

        lo_column = lo_columns->get_column( columnname = 'AGRUPAD' ).
        lo_column->set_short_text( 'AGRP' ).
        lo_column->set_medium_text( 'AGRUPADOR' ).
        lo_column->set_long_text( 'AGRUPADOR' ).

        "1ª Fornecedor
        lo_column = lo_columns->get_column( columnname = 'NOMFOR1' ).
        lo_column->set_short_text( 'FORN1' ).
        lo_column->set_medium_text( 'FORNEC1' ).
        lo_column->set_long_text( 'FORNECEDOR1' ).

        lo_column = lo_columns->get_column( columnname = 'CODMAT1' ).
        lo_column->set_short_text( 'CÓD.MT1' ).
        lo_column->set_medium_text( 'CÓD. MAT1' ).
        lo_column->set_long_text( 'CÓD. MATERIAL1' ).

        lo_column = lo_columns->get_column( columnname = 'VRULCP1' ).
        lo_column->set_short_text( 'VR.UL CP1' ).
        lo_column->set_medium_text( 'VR.ULT. CP1' ).
        lo_column->set_long_text( 'VR. ULT. COMP1' ).

        lo_column = lo_columns->get_column( columnname = 'DTULCP1' ).
        lo_column->set_short_text( 'DT.UL CP1' ).
        lo_column->set_medium_text( 'DT. ULT. CP1').
        lo_column->set_long_text( 'DT. ULT. COMP1').

        lo_column = lo_columns->get_column( columnname = 'MDVEND1' ).
        lo_column->set_short_text('MD.VEND1').
        lo_column->set_medium_text('MD. VENDAS1').
        lo_column->set_long_text('MÉDIA VENDAS1').

        "2ª Fornecedor
        lo_column = lo_columns->get_column( columnname = 'NOMFOR2' ).
        lo_column->set_short_text( 'FORN2' ).
        lo_column->set_medium_text( 'FORNEC2' ).
        lo_column->set_long_text( 'FORNECEDOR2' ).

        lo_column = lo_columns->get_column( columnname = 'CODMAT2' ).
        lo_column->set_short_text( 'CÓD.MT2' ).
        lo_column->set_medium_text( 'CÓD. MAT2' ).
        lo_column->set_long_text( 'CÓD. MATERIAL2' ).

        lo_column = lo_columns->get_column( columnname = 'VRULCP2' ).
        lo_column->set_short_text( 'VR.UL CP2' ).
        lo_column->set_medium_text( 'VR.ULT. CP2' ).
        lo_column->set_long_text( 'VR. ULT. COMP2' ).

        lo_column = lo_columns->get_column( columnname = 'DTULCP2' ).
        lo_column->set_short_text( 'DT.UL CP2' ).
        lo_column->set_medium_text( 'DT. ULT. CP2').
        lo_column->set_long_text( 'DT. ULT. COMP2').

        lo_column = lo_columns->get_column( columnname = 'MDVEND2' ).
        lo_column->set_short_text('MD.VEND2').
        lo_column->set_medium_text('MD. VENDAS2').
        lo_column->set_long_text('MÉDIA VENDAS2').

        "3ª Fornecedor
        lo_column = lo_columns->get_column( columnname = 'NOMFOR3' ).
        lo_column->set_short_text( 'FORN3' ).
        lo_column->set_medium_text( 'FORNEC3' ).
        lo_column->set_long_text( 'FORNECEDOR3' ).

        lo_column = lo_columns->get_column( columnname = 'CODMAT3' ).
        lo_column->set_short_text( 'CÓD.MT3' ).
        lo_column->set_medium_text( 'CÓD. MAT3' ).
        lo_column->set_long_text( 'CÓD. MATERIAL3' ).

        lo_column = lo_columns->get_column( columnname = 'VRULCP3' ).
        lo_column->set_short_text( 'VR.UL CP3' ).
        lo_column->set_medium_text( 'VR.ULT. CP3' ).
        lo_column->set_long_text( 'VR. ULT. COMP3' ).

        lo_column = lo_columns->get_column( columnname = 'DTULCP3' ).
        lo_column->set_short_text( 'DT.UL CP3' ).
        lo_column->set_medium_text( 'DT. ULT. CP3').
        lo_column->set_long_text( 'DT. ULT. COMP3').

        lo_column = lo_columns->get_column( columnname = 'MDVEND3' ).
        lo_column->set_short_text('MD.VEND3').
        lo_column->set_medium_text('MD. VENDAS3').
        lo_column->set_long_text('MÉDIA VENDAS3').

        "4 Fornecedor
        lo_column = lo_columns->get_column( columnname = 'NOMFOR4' ).
        lo_column->set_short_text( 'FORN4' ).
        lo_column->set_medium_text( 'FORNEC4' ).
        lo_column->set_long_text( 'FORNECEDOR4' ).

        lo_column = lo_columns->get_column( columnname = 'CODMAT4' ).
        lo_column->set_short_text( 'CÓD.MT4' ).
        lo_column->set_medium_text( 'CÓD. MAT4' ).
        lo_column->set_long_text( 'CÓD. MATERIAL4' ).

        lo_column = lo_columns->get_column( columnname = 'VRULCP4' ).
        lo_column->set_short_text( 'VR.UL CP4' ).
        lo_column->set_medium_text( 'VR.ULT. CP4' ).
        lo_column->set_long_text( 'VR. ULT. COMP4' ).

        lo_column = lo_columns->get_column( columnname = 'DTULCP4' ).
        lo_column->set_short_text( 'DT.UL CP4' ).
        lo_column->set_medium_text( 'DT. ULT. CP4').
        lo_column->set_long_text( 'DT. ULT. COMP4').

        lo_column = lo_columns->get_column( columnname = 'MDVEND4' ).
        lo_column->set_short_text('MD.VEND4').
        lo_column->set_medium_text('MD. VENDAS4').
        lo_column->set_long_text('MÉDIA VENDAS4').

        "setando o nome da coluna de ganhador
        lo_column = lo_columns->get_column( columnname = 'GANHADO' ).
        lo_column->set_short_text( 'Gdor' ).
        lo_column->set_medium_text( 'Ganhado' ).
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
