*&---------------------------------------------------------------------*
*& Include          ZMMR035_C01
*&---------------------------------------------------------------------*
START-OF-SELECTION.

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
        INTO TABLE @DATA(lt_pedid).

SORT lt_pedid ASCENDING BY matnr.

DATA(lv_dtatu) = sy-datum.
DATA(lv_dtfin) = sy-datum.

lv_dtatu = lv_dtatu - 1.

DATA(lv_dtam) = lv_dtatu(6) - 5.

lv_dtfin(6) = lv_dtam.
lv_dtfin+6(2) = 01.

  LOOP AT lt_pedid INTO DATA(ls_pedid).

    TRY.
        ls_final-matnr = ls_pedid-matnr.
        ls_final-mfrpn = ls_pedid-mfrpn.
        ls_final-maktx = ls_pedid-maktx.
        ls_final-lifnr = ls_pedid-lifnr.
        ls_final-name1 = ls_pedid-name1.
        ls_final-bismt = ls_pedid-bismt.
        ls_final-matkl = ls_pedid-matkl.

APPEND ls_final TO lt_final.

SELECT  SINGLE SUM( menge ) AS qtdtl
                FROM mseg
                WHERE bwart IN ('261','601')
                AND matnr = @ls_pedid-matnr
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
                AND ekpo~matnr = @ls_pedid-matnr
                GROUP BY ekpo~matnr, ekko~aedat,brtwr, menge
                INTO TABLE @DATA(lt_dtucp).

SORT lt_dtucp DESCENDING BY dtucp.

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

**      setando o nome da coluna de percentual
*      lo_column = lo_columns->get_column( columnname = 'PERCENTUAL' ).
*      lo_column->set_short_text( 'Percentual' ).
*      lo_column->set_medium_text( 'Percentual' ).
*      lo_column->set_long_text( 'Percentual' ).


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
