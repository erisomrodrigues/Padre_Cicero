*&---------------------------------------------------------------------*
*& Include          ZSDR067_C01
*&---------------------------------------------------------------------*
CLASS lcl_main_report IMPLEMENTATION.
  METHOD init.
    CASE 'X'.
      WHEN rb_anali.
        IF s_pstdat IS INITIAL.
          MESSAGE 'Por favor, preencher a data' TYPE 'I' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
      WHEN cb_excel.
        IF cb_excel IS NOT INITIAL AND p_file IS INITIAL.
          MESSAGE 'Para esse tipo de execução, preencher o diretório' TYPE 'I' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
    ENDCASE.
    data_select( ).
  ENDMETHOD.

  METHOD data_select.
    DATA: lv_count TYPE i.
******************************   Consultar todas as notas com IPI ou sem IPI   *********************
*** APPC - SD. 494365 - Erisom Almeida - 28/02/2024 - Inicio
    SELECT DISTINCT lin~docnum,
                    lin~itmnum,
                    lin~nfnett,
                    tax~taxgrp,
                    tax~taxval
            FROM          j_1bnfdoc   AS doc
            INNER JOIN    j_1bnflin   AS lin
            ON            lin~docnum  = doc~docnum
            LEFT JOIN     j_1bnfstx   AS tax
            ON            lin~docnum  = tax~docnum
            AND           lin~itmnum  = tax~itmnum
            WHERE         doc~direct  IN @s_direct "Direção do movimento de mercadorias
            AND           doc~pstdat  IN @s_pstdat "Data documento
            AND           doc~branch  IN @s_branch "Local de negócios
            AND           doc~cancel  = @cb_canc
            AND           doc~doctyp  IN ('1','6','7')
            AND           doc~docstat IN ('1')
            AND           lin~cfop    <> 0
            GROUP BY      lin~docnum, lin~itmnum,lin~nfnett,tax~taxgrp,tax~taxval
            INTO TABLE @lt_stx.

    SORT lt_stx BY docnum itmnum.
    FREE lt_gstx.

    LOOP AT lt_stx ASSIGNING FIELD-SYMBOL(<fs_stx>)
                   GROUP BY ( docnum = <fs_stx>-docnum )
      ASSIGNING FIELD-SYMBOL(<fs_t_stx>).

      FREE lv_item_aux.
      " Criando uma linha em branco para ser preenchida pelo Field-Symbol
      IF lt_gstx IS INITIAL.
        APPEND INITIAL LINE TO lt_gstx ASSIGNING FIELD-SYMBOL(<fs_gstx>).
        <fs_gstx>-docnum = <fs_t_stx>-docnum.
      ELSEIF <fs_gstx> IS ASSIGNED AND <fs_gstx>-docnum <> <fs_t_stx>-docnum.
        APPEND INITIAL LINE TO lt_gstx ASSIGNING <fs_gstx>.
        <fs_gstx>-docnum = <fs_t_stx>-docnum.
      ENDIF.

      " Agrupando os dados p/ serem inseridos na tabela
      LOOP AT GROUP <fs_t_stx> ASSIGNING FIELD-SYMBOL(<fs_g_stx>).
        IF lv_item_aux <> <fs_g_stx>-itmnum.
          lv_item_aux = <fs_g_stx>-itmnum.
          <fs_gstx>-nfnett = <fs_gstx>-nfnett + <fs_g_stx>-nfnett.
        ENDIF.
        IF <fs_g_stx>-taxgrp = 'IPI' OR <fs_g_stx>-taxgrp = 'ICST' OR <fs_g_stx>-taxgrp = ''.
          <fs_gstx>-taxval = <fs_gstx>-taxval + <fs_g_stx>-taxval.
        ENDIF.
      ENDLOOP.
      <fs_gstx>-total = <fs_gstx>-nfnett + <fs_gstx>-taxval.
    ENDLOOP.
***************************************** Geração do Relatório Sintetico (Resumido) **********************************
    IF rb_sinte IS NOT INITIAL.
*** APPC - HML - Busca as notas se estão canceladas ou não - 24/03/2023
      SELECT DISTINCT j_1bnfdoc~branch,              " Local de negócios
                      j_1bnflin~cfop,                " Código e extensão CFOP
                      SUM( coalesce( j_1bnflin~nfnett, 0 ) + coalesce( tax~taxval, 0 ) ) AS som
        FROM j_1bnfdoc
        INNER JOIN j_1bnflin
        ON ( j_1bnflin~docnum = j_1bnfdoc~docnum )
        INNER JOIN @lt_gstx AS tax
        ON ( tax~docnum = j_1bnfdoc~docnum )
        WHERE j_1bnfdoc~direct IN @s_direct "Direção do movimento de mercadorias
        AND   j_1bnfdoc~pstdat IN @s_pstdat "Data documento
        AND   j_1bnfdoc~branch IN @s_branch "Local de negócios
        AND   j_1bnfdoc~cancel  = @cb_canc
        AND   j_1bnfdoc~doctyp  IN ('1','6','7')
        AND   j_1bnfdoc~docstat = '1'
        GROUP BY j_1bnfdoc~branch, j_1bnflin~cfop
        HAVING ( j_1bnflin~cfop > 0 )
        ORDER BY j_1bnfdoc~branch, j_1bnflin~cfop
        INTO TABLE @lt_sinte.
    ELSEIF rb_anali = 'X' .
**************************************** Geração do Relatório Analitico (Ampro) ****************************************
*** APPC - SD. 494365 - Erisom Almeida - 28/02/2024 - Inicio
      SELECT DISTINCT j_1bnfdoc~docnum,  " Nº documento
                      j_1bnfdoc~nftype, " Ctg.de nota fiscal
                      j_1bnfdoc~direct, " Direção do movimento de mercadorias
                      j_1bnfdoc~docdat, " Data do documento
                      j_1bnfdoc~pstdat, " Data de lançamento
                      som~total,       " Valor total incluindo todos os impostos
                      j_1bnfdoc~model,  " Modelo da nota fiscal
                      j_1bnfdoc~series, " Séries
                      j_1bnfdoc~branch, " Local de negócios
                      j_1bnfdoc~parid,  " Identificação do parceiro (cliente, fornecedor, loc.negócio)
                      j_1bnfdoc~nfenum, " Número de nota fiscal eletrônica
                      j_1bnfdoc~natop,  " Natureza da Operação
                      j_1bnfdoc~inco1,  " Incoterms
                      j_1bnflin~cfop,   " Código e extensão CFOP
                      j_1bnfdoc~regio,  " Região (estado federal, estado federado, província, condado)
                      j_1bnfdoc~name1,  " Nome do cliente
        concat(       j_1bnfe_active~regio,
        concat(       j_1bnfe_active~nfyear,
        concat(       j_1bnfe_active~nfmonth,
        concat(       j_1bnfe_active~stcd1,
        concat(       j_1bnfe_active~model,
        concat(       j_1bnfe_active~serie,
        concat(       j_1bnfe_active~nfnum9,
        concat(       j_1bnfe_active~tpemis,
        concat(       j_1bnfe_active~docnum9, j_1bnfe_active~cdv ) ) ) ) ) ) ) ) ) AS chave
        FROM j_1bnfdoc
        INNER JOIN @lt_gstx AS som               "APPC - SD. 494365 - Erisom Almeida - 07/02/2024 - 14/02/2024
        ON ( som~docnum = j_1bnfdoc~docnum )
        INNER JOIN j_1bnflin
        ON ( j_1bnfdoc~docnum = j_1bnflin~docnum )
        LEFT JOIN j_1bnfe_active
        ON ( j_1bnfe_active~docnum = j_1bnfdoc~docnum )
        WHERE j_1bnfdoc~direct IN @s_direct "Direção do movimento de mercadorias
        AND   j_1bnfdoc~pstdat IN @s_pstdat "Data documento
        AND   j_1bnfdoc~branch IN @s_branch "Local de negócios
        AND   j_1bnfdoc~cancel  = @cb_canc
        AND   j_1bnfdoc~doctyp  IN ('1','6','7') "APPC - SD. 494365 - Erisom Almeida - 07/02/2024 - 14/02/2024
        AND   j_1bnfdoc~docstat = '1'
        AND   j_1bnflin~cfop    > 0
        ORDER BY j_1bnfdoc~docnum ASCENDING
        INTO TABLE @lt_excel.
** APPC - AERA - MM.647328 - 28/08/2024 - Inicio
***************************************** Geração do Relatório Analitico Pelos Produtos ****************************
    ELSE.

      SELECT    matnr,
                budat_mkpf,
                lbkum
            FROM mseg
            INNER JOIN t001w
            ON ( t001w~werks = mseg~werks )
            WHERE mjahr = '2020'
            AND t001w~j_1bbranch IN @s_branch
            INTO TABLE @DATA(lt_mseg).

      IF sy-subrc = 0.
        SORT lt_mseg DESCENDING  BY budat_mkpf.
        DELETE ADJACENT DUPLICATES FROM lt_mseg.
      ENDIF.


      SELECT DISTINCT j_1bnfdoc~docnum,  " Nº documento
            j_1bnfdoc~nftype, " Ctg.de nota fiscal
            j_1bnflin~refitm, " Fornecedor
            j_1bnflin~matnr,  " Produto
            j_1bnflin~maktx,  " Aplicação
            j_1bnflin~menge,  " Quantidade
            j_1bnflin~meins,  " Unidade de Medida
            j_1bnflin~netpr,  " Preço Líquido
            j_1bnflin~netwr,  " Preço Bruto
            j_1bnfdoc~direct, " Direção do movimento de mercadorias
            j_1bnfdoc~docdat, " Data do documento
            j_1bnfdoc~pstdat, " Data de lançamento
            som~total,        " Valor total incluindo todos os impostos
            j_1bnfdoc~model,  " Modelo da nota fiscal
            j_1bnfdoc~series, " Séries
            j_1bnfdoc~branch, " Local de negócios
            j_1bnfdoc~parid,  " Identificação do parceiro (cliente, fornecedor, loc.negócio)
            j_1bnfdoc~nfenum, " Número de nota fiscal eletrônica
            j_1bnfdoc~natop,  " Natureza da Operação
            j_1bnfdoc~inco1,  " Incoterms
            j_1bnflin~cfop,   " Código e extensão CFOP
            j_1bnfdoc~regio,  " Região (estado federal, estado federado, província, condado)
            j_1bnfdoc~name1,  " Nome do cliente
            t030~konts,       " Conta Razão
            j_1bnflin~nbm,
   concat(       j_1bnfe_active~regio,
   concat(       j_1bnfe_active~nfyear,
   concat(       j_1bnfe_active~nfmonth,
   concat(       j_1bnfe_active~stcd1,
   concat(       j_1bnfe_active~model,
   concat(       j_1bnfe_active~serie,
   concat(       j_1bnfe_active~nfnum9,
   concat(       j_1bnfe_active~tpemis,
   concat(       j_1bnfe_active~docnum9, j_1bnfe_active~cdv ) ) ) ) ) ) ) ) ) AS chave
   FROM j_1bnfdoc
   INNER JOIN @lt_gstx AS som               "APPC - SD. 494365 - Erisom Almeida - 07/02/2024 - 14/02/2024
   ON ( som~docnum = j_1bnfdoc~docnum )
   INNER JOIN j_1bnflin
   ON ( j_1bnfdoc~docnum = j_1bnflin~docnum )
   LEFT JOIN j_1bnfe_active
   ON ( j_1bnfe_active~docnum = j_1bnfdoc~docnum )
   LEFT JOIN mbew
   ON ( mbew~matnr = j_1bnflin~matnr )
   LEFT JOIN t030
   ON ( t030~bklas = mbew~bklas )
   WHERE j_1bnfdoc~direct IN @s_direct "Direção do movimento de mercadorias
   AND   j_1bnfdoc~pstdat IN @s_pstdat "Data documento
   AND   j_1bnfdoc~branch IN @s_branch "Local de negócios
   AND   j_1bnfdoc~cancel  = @cb_canc
   AND   j_1bnfdoc~doctyp  IN ('1','6','7') "APPC - SD. 494365 - Erisom Almeida - 07/02/2024 - 14/02/2024
   AND   t030~ktopl = '0050'
   AND   t030~ktosl = 'BSX'
   AND   j_1bnfdoc~docstat = '1'
   AND   j_1bnflin~cfop    > 0
   ORDER BY j_1bnfdoc~docnum ASCENDING
   INTO TABLE @lt_a_prd.


      SORT lt_a_prd BY docnum refitm.
** APPC - AERA - MM.647326 - 28/08/2024 - Fim
    ENDIF.

    IF sy-subrc = 0.
      IF rb_anali IS NOT INITIAL.
        IF cb_excel = 'X'.
          gerar_excel( ).
        ELSE.
          data_display( ).
        ENDIF.
      ELSE.
        IF cb_excel = 'X'.
          gerar_excel( ).
        ELSE.
          IF sy-uname = 'DEV01'.
            gerar_txt( ).
            BREAK-POINT.
            DATA: lv_count_print    TYPE i,
                  lc_formulario     TYPE tdsfname   VALUE 'ZMMSF_CONTROLE_PRODUCAO_MAT',
                  lv_fm_name        TYPE rs38l_fnam,
                  lv_matnr_old      TYPE matnr,
                  lv_produto        TYPE char50,
                  lv_ncm            TYPE steuc,
                  lv_unidade        TYPE meins,
                  lv_saldo          TYPE j_1bnetqty,
                  ls_output         TYPE ssfcompop,
                  ls_parameters     TYPE ssfctrlop,
*                  ls_otfdata        TYPE ssfcrescl,
                  ls_output_options TYPE ssfcresop,
                  ls_linha01        TYPE  zmms_controle,
                  ls_linha02        TYPE  zmms_controle,
                  ls_linha03        TYPE  zmms_controle,
                  ls_linha04        TYPE  zmms_controle,
                  ls_linha05        TYPE  zmms_controle,
                  ls_linha06        TYPE  zmms_controle,
                  ls_linha07        TYPE  zmms_controle,
                  ls_linha08        TYPE  zmms_controle,
                  ls_linha09        TYPE  zmms_controle,
                  ls_linha10        TYPE  zmms_controle,
                  ls_linha11        TYPE  zmms_controle,
                  ls_linha12        TYPE  zmms_controle,
                  ls_linha13        TYPE  zmms_controle,
                  ls_linha14        TYPE  zmms_controle,
                  ls_linha15        TYPE  zmms_controle,
                  ls_linha16        TYPE  zmms_controle,
                  ls_linha17        TYPE  zmms_controle,
                  ls_linha18        TYPE  zmms_controle,
                  ls_linha19        TYPE  zmms_controle.

            CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
              EXPORTING
                formname           = lc_formulario
              IMPORTING
                fm_name            = lv_fm_name
              EXCEPTIONS
                no_form            = 1
                no_function_module = 2
                OTHERS             = 3.

            SORT lt_a_prd ASCENDING BY matnr.

            LOOP AT lt_a_prd ASSIGNING FIELD-SYMBOL(<fs_prd>).
              IF lv_matnr_old <> <fs_prd>-matnr.
                lv_matnr_old = <fs_prd>-matnr.
                DATA(lv_lines) = REDUCE i( INIT l_x = 0 FOR <fs_count> IN lt_a_prd WHERE ( matnr = <fs_prd>-matnr )
                                           NEXT l_x = l_x + 1 ).
                ADD 1 TO lv_count_print.
                CONCATENATE <fs_prd>-matnr <fs_prd>-maktx INTO lv_produto SEPARATED BY space.
                lv_unidade = <fs_prd>-meins.
                lv_ncm = <fs_prd>-ncm.

                READ TABLE lt_mseg ASSIGNING FIELD-SYMBOL(<fs_mseg>) WITH KEY matnr = <fs_prd>-matnr.

                IF sy-subrc = 0.
                  ls_linha01-estoque_quantidade = <fs_mseg>-lbkum.
                ENDIF.

                lv_saldo = ls_linha01-estoque_quantidade.

                ls_linha02-especie = <fs_prd>-model.
                ls_linha02-serie_subserie = <fs_prd>-series.
                ls_linha02-numero = <fs_prd>-docnum.
                ls_linha02-dia = <fs_prd>-docdat+6(2).
                ls_linha02-mes = <fs_prd>-docdat+4(2).
                ls_linha02-contabil = <fs_prd>-konts.

                IF <fs_prd>-direct = 1.
                  lv_saldo = lv_saldo + <fs_prd>-menge.
                  ls_linha02-estoque_quantidade = lv_saldo.
                  ls_linha02-valor = <fs_prd>-netwr.
                  ls_linha02-quantidade = <fs_prd>-menge.
                ELSEIF <fs_prd>-direct = 2.
                  lv_saldo = lv_saldo - <fs_prd>-menge.
                  ls_linha02-estoque_quantidade = lv_saldo.
                  ls_linha02-valor_saida = <fs_prd>-netwr.
                  ls_linha02-quantidade_diversas = <fs_prd>-menge.
                ENDIF.
              ELSE.
                IF lv_lines > 19.

                ELSE.
                  ADD 1 TO lv_count_print.

                  IF ls_linha03 IS INITIAL.
                    ls_linha03-especie = <fs_prd>-model.
                    ls_linha03-serie_subserie = <fs_prd>-series.
                    ls_linha03-numero = <fs_prd>-docnum.
                    ls_linha03-dia = <fs_prd>-docdat+6(2).
                    ls_linha03-mes = <fs_prd>-docdat+4(2).
                    ls_linha03-contabil = <fs_prd>-konts.

                    IF <fs_prd>-direct = 1.
                      lv_saldo = lv_saldo + <fs_prd>-menge.
                      ls_linha03-estoque_quantidade = lv_saldo.
                      ls_linha03-valor = <fs_prd>-netwr.
                      ls_linha03-quantidade = <fs_prd>-menge.
                    ELSEIF <fs_prd>-direct = 2.
                      lv_saldo = lv_saldo - <fs_prd>-menge.
                      ls_linha03-estoque_quantidade = lv_saldo.
                      ls_linha03-valor_saida = <fs_prd>-netwr.
                      ls_linha03-quantidade_diversas = <fs_prd>-menge.
                    ENDIF.
                  ELSEIF ls_linha04 IS INITIAL.
                    ls_linha04-especie = <fs_prd>-model.
                    ls_linha04-serie_subserie = <fs_prd>-series.
                    ls_linha04-numero = <fs_prd>-docnum.
                    ls_linha04-dia = <fs_prd>-docdat+6(2).
                    ls_linha04-mes = <fs_prd>-docdat+4(2).
                    ls_linha04-contabil = <fs_prd>-konts.

                    IF <fs_prd>-direct = 1.
                      lv_saldo = lv_saldo + <fs_prd>-menge.
                      ls_linha04-estoque_quantidade = lv_saldo.
                      ls_linha04-valor = <fs_prd>-netwr.
                      ls_linha04-quantidade = <fs_prd>-menge.
                    ELSEIF <fs_prd>-direct = 2.
                      lv_saldo = lv_saldo - <fs_prd>-menge.
                      ls_linha04-estoque_quantidade = lv_saldo.
                      ls_linha04-valor_saida = <fs_prd>-netwr.
                      ls_linha04-quantidade_diversas = <fs_prd>-menge.
                    ENDIF.
                  ELSEIF ls_linha05 IS INITIAL.
                    ls_linha05-especie = <fs_prd>-model.
                    ls_linha05-serie_subserie = <fs_prd>-series.
                    ls_linha05-numero = <fs_prd>-docnum.
                    ls_linha05-dia = <fs_prd>-docdat+6(2).
                    ls_linha05-mes = <fs_prd>-docdat+4(2).
                    ls_linha05-contabil = <fs_prd>-konts.

                    IF <fs_prd>-direct = 1.
                      lv_saldo = lv_saldo + <fs_prd>-menge.
                      ls_linha05-estoque_quantidade = lv_saldo.
                      ls_linha05-valor = <fs_prd>-netwr.
                      ls_linha05-quantidade = <fs_prd>-menge.
                    ELSEIF <fs_prd>-direct = 2.
                      lv_saldo = lv_saldo - <fs_prd>-menge.
                      ls_linha05-estoque_quantidade = lv_saldo.
                      ls_linha05-valor_saida = <fs_prd>-netwr.
                      ls_linha05-quantidade_diversas = <fs_prd>-menge.
                    ENDIF.
                  ELSEIF ls_linha06 IS INITIAL.
                    ls_linha06-especie = <fs_prd>-model.
                    ls_linha06-serie_subserie = <fs_prd>-series.
                    ls_linha06-numero = <fs_prd>-docnum.
                    ls_linha06-dia = <fs_prd>-docdat+6(2).
                    ls_linha06-mes = <fs_prd>-docdat+4(2).
                    ls_linha06-contabil = <fs_prd>-konts.

                    IF <fs_prd>-direct = 1.
                      lv_saldo = lv_saldo + <fs_prd>-menge.
                      ls_linha06-estoque_quantidade = lv_saldo.
                      ls_linha06-valor = <fs_prd>-netwr.
                      ls_linha06-quantidade = <fs_prd>-menge.
                    ELSEIF <fs_prd>-direct = 2.
                      lv_saldo = lv_saldo - <fs_prd>-menge.
                      ls_linha06-estoque_quantidade = lv_saldo.
                      ls_linha06-valor_saida = <fs_prd>-netwr.
                      ls_linha06-quantidade_diversas = <fs_prd>-menge.
                    ENDIF.
                  ELSEIF ls_linha07 IS INITIAL.
                    ls_linha07-especie = <fs_prd>-model.
                    ls_linha07-serie_subserie = <fs_prd>-series.
                    ls_linha07-numero = <fs_prd>-docnum.
                    ls_linha07-dia = <fs_prd>-docdat+6(2).
                    ls_linha07-mes = <fs_prd>-docdat+4(2).
                    ls_linha07-contabil = <fs_prd>-konts.

                    IF <fs_prd>-direct = 1.
                      lv_saldo = lv_saldo + <fs_prd>-menge.
                      ls_linha07-estoque_quantidade = lv_saldo.
                      ls_linha07-valor = <fs_prd>-netwr.
                      ls_linha07-quantidade = <fs_prd>-menge.
                    ELSEIF <fs_prd>-direct = 2.
                      lv_saldo = lv_saldo - <fs_prd>-menge.
                      ls_linha07-estoque_quantidade = lv_saldo.
                      ls_linha07-valor_saida = <fs_prd>-netwr.
                      ls_linha07-quantidade_diversas = <fs_prd>-menge.
                    ENDIF.
                  ELSEIF ls_linha08 IS INITIAL.
                    ls_linha08-especie = <fs_prd>-model.
                    ls_linha08-serie_subserie = <fs_prd>-series.
                    ls_linha08-numero = <fs_prd>-docnum.
                    ls_linha08-dia = <fs_prd>-docdat+6(2).
                    ls_linha08-mes = <fs_prd>-docdat+4(2).
                    ls_linha08-contabil = <fs_prd>-konts.

                    IF <fs_prd>-direct = 1.
                      lv_saldo = lv_saldo + <fs_prd>-menge.
                      ls_linha08-estoque_quantidade = lv_saldo.
                      ls_linha08-valor = <fs_prd>-netwr.
                      ls_linha08-quantidade = <fs_prd>-menge.
                    ELSEIF <fs_prd>-direct = 2.
                      lv_saldo = lv_saldo - <fs_prd>-menge.
                      ls_linha08-estoque_quantidade = lv_saldo.
                      ls_linha08-valor_saida = <fs_prd>-netwr.
                      ls_linha08-quantidade_diversas = <fs_prd>-menge.
                    ENDIF.
                  ELSEIF ls_linha09 IS INITIAL.
                    ls_linha09-especie = <fs_prd>-model.
                    ls_linha09-serie_subserie = <fs_prd>-series.
                    ls_linha09-numero = <fs_prd>-docnum.
                    ls_linha09-dia = <fs_prd>-docdat+6(2).
                    ls_linha09-mes = <fs_prd>-docdat+4(2).
                    ls_linha09-contabil = <fs_prd>-konts.

                    IF <fs_prd>-direct = 1.
                      lv_saldo = lv_saldo + <fs_prd>-menge.
                      ls_linha09-estoque_quantidade = lv_saldo.
                      ls_linha09-valor = <fs_prd>-netwr.
                      ls_linha09-quantidade = <fs_prd>-menge.
                    ELSEIF <fs_prd>-direct = 2.
                      lv_saldo = lv_saldo - <fs_prd>-menge.
                      ls_linha09-estoque_quantidade = lv_saldo.
                      ls_linha09-valor_saida = <fs_prd>-netwr.
                      ls_linha09-quantidade_diversas = <fs_prd>-menge.
                    ENDIF.
                  ELSEIF ls_linha10 IS INITIAL.
                    ls_linha10-especie = <fs_prd>-model.
                    ls_linha10-serie_subserie = <fs_prd>-series.
                    ls_linha10-numero = <fs_prd>-docnum.
                    ls_linha10-dia = <fs_prd>-docdat+6(2).
                    ls_linha10-mes = <fs_prd>-docdat+4(2).
                    ls_linha10-contabil = <fs_prd>-konts.

                    IF <fs_prd>-direct = 1.
                      lv_saldo = lv_saldo + <fs_prd>-menge.
                      ls_linha10-estoque_quantidade = lv_saldo.
                      ls_linha10-valor = <fs_prd>-netwr.
                      ls_linha10-quantidade = <fs_prd>-menge.
                    ELSEIF <fs_prd>-direct = 2.
                      lv_saldo = lv_saldo - <fs_prd>-menge.
                      ls_linha10-estoque_quantidade = lv_saldo.
                      ls_linha10-valor_saida = <fs_prd>-netwr.
                      ls_linha10-quantidade_diversas = <fs_prd>-menge.
                    ENDIF.
                  ELSEIF ls_linha11 IS INITIAL.
                    ls_linha11-especie = <fs_prd>-model.
                    ls_linha11-serie_subserie = <fs_prd>-series.
                    ls_linha11-numero = <fs_prd>-docnum.
                    ls_linha11-dia = <fs_prd>-docdat+6(2).
                    ls_linha11-mes = <fs_prd>-docdat+4(2).
                    ls_linha11-contabil = <fs_prd>-konts.

                    IF <fs_prd>-direct = 1.
                      lv_saldo = lv_saldo + <fs_prd>-menge.
                      ls_linha11-estoque_quantidade = lv_saldo.
                      ls_linha11-valor = <fs_prd>-netwr.
                      ls_linha11-quantidade = <fs_prd>-menge.
                    ELSEIF <fs_prd>-direct = 2.
                      lv_saldo = lv_saldo - <fs_prd>-menge.
                      ls_linha11-estoque_quantidade = lv_saldo.
                      ls_linha11-valor_saida = <fs_prd>-netwr.
                      ls_linha11-quantidade_diversas = <fs_prd>-menge.
                    ENDIF.
                  ELSEIF ls_linha12 IS INITIAL.
                    ls_linha12-especie = <fs_prd>-model.
                    ls_linha12-serie_subserie = <fs_prd>-series.
                    ls_linha12-numero = <fs_prd>-docnum.
                    ls_linha12-dia = <fs_prd>-docdat+6(2).
                    ls_linha12-mes = <fs_prd>-docdat+4(2).
                    ls_linha12-contabil = <fs_prd>-konts.

                    IF <fs_prd>-direct = 1.
                      lv_saldo = lv_saldo + <fs_prd>-menge.
                      ls_linha12-estoque_quantidade = lv_saldo.
                      ls_linha12-valor = <fs_prd>-netwr.
                      ls_linha12-quantidade = <fs_prd>-menge.
                    ELSEIF <fs_prd>-direct = 2.
                      lv_saldo = lv_saldo - <fs_prd>-menge.
                      ls_linha12-estoque_quantidade = lv_saldo.
                      ls_linha12-valor_saida = <fs_prd>-netwr.
                      ls_linha12-quantidade_diversas = <fs_prd>-menge.
                    ENDIF.
                  ELSEIF ls_linha13 IS INITIAL.
                    ls_linha13-especie = <fs_prd>-model.
                    ls_linha13-serie_subserie = <fs_prd>-series.
                    ls_linha13-numero = <fs_prd>-docnum.
                    ls_linha13-dia = <fs_prd>-docdat+6(2).
                    ls_linha13-mes = <fs_prd>-docdat+4(2).
                    ls_linha13-contabil = <fs_prd>-konts.

                    IF <fs_prd>-direct = 1.
                      lv_saldo = lv_saldo + <fs_prd>-menge.
                      ls_linha13-estoque_quantidade = lv_saldo.
                      ls_linha13-valor = <fs_prd>-netwr.
                      ls_linha13-quantidade = <fs_prd>-menge.
                    ELSEIF <fs_prd>-direct = 2.
                      lv_saldo = lv_saldo - <fs_prd>-menge.
                      ls_linha13-estoque_quantidade = lv_saldo.
                      ls_linha13-valor_saida = <fs_prd>-netwr.
                      ls_linha13-quantidade_diversas = <fs_prd>-menge.
                    ENDIF.
                  ELSEIF ls_linha14 IS INITIAL.
                    ls_linha14-especie = <fs_prd>-model.
                    ls_linha14-serie_subserie = <fs_prd>-series.
                    ls_linha14-numero = <fs_prd>-docnum.
                    ls_linha14-dia = <fs_prd>-docdat+6(2).
                    ls_linha14-mes = <fs_prd>-docdat+4(2).
                    ls_linha14-contabil = <fs_prd>-konts.

                    IF <fs_prd>-direct = 1.
                      lv_saldo = lv_saldo + <fs_prd>-menge.
                      ls_linha14-estoque_quantidade = lv_saldo.
                      ls_linha14-valor = <fs_prd>-netwr.
                      ls_linha14-quantidade = <fs_prd>-menge.
                    ELSEIF <fs_prd>-direct = 2.
                      lv_saldo = lv_saldo - <fs_prd>-menge.
                      ls_linha14-estoque_quantidade = lv_saldo.
                      ls_linha14-valor_saida = <fs_prd>-netwr.
                      ls_linha14-quantidade_diversas = <fs_prd>-menge.
                    ENDIF.
                  ELSEIF ls_linha15 IS INITIAL.
                    ls_linha15-especie = <fs_prd>-model.
                    ls_linha15-serie_subserie = <fs_prd>-series.
                    ls_linha15-numero = <fs_prd>-docnum.
                    ls_linha15-dia = <fs_prd>-docdat+6(2).
                    ls_linha15-mes = <fs_prd>-docdat+4(2).
                    ls_linha15-contabil = <fs_prd>-konts.

                    IF <fs_prd>-direct = 1.
                      lv_saldo = lv_saldo + <fs_prd>-menge.
                      ls_linha15-estoque_quantidade = lv_saldo.
                      ls_linha15-valor = <fs_prd>-netwr.
                      ls_linha15-quantidade = <fs_prd>-menge.
                    ELSEIF <fs_prd>-direct = 2.
                      lv_saldo = lv_saldo - <fs_prd>-menge.
                      ls_linha15-estoque_quantidade = lv_saldo.
                      ls_linha15-valor_saida = <fs_prd>-netwr.
                      ls_linha15-quantidade_diversas = <fs_prd>-menge.
                    ENDIF.
                  ELSEIF ls_linha16 IS INITIAL.
                    ls_linha16-especie = <fs_prd>-model.
                    ls_linha16-serie_subserie = <fs_prd>-series.
                    ls_linha16-numero = <fs_prd>-docnum.
                    ls_linha16-dia = <fs_prd>-docdat+6(2).
                    ls_linha16-mes = <fs_prd>-docdat+4(2).
                    ls_linha16-contabil = <fs_prd>-konts.

                    IF <fs_prd>-direct = 1.
                      lv_saldo = lv_saldo + <fs_prd>-menge.
                      ls_linha16-estoque_quantidade = lv_saldo.
                      ls_linha16-valor = <fs_prd>-netwr.
                      ls_linha16-quantidade = <fs_prd>-menge.
                    ELSEIF <fs_prd>-direct = 2.
                      lv_saldo = lv_saldo - <fs_prd>-menge.
                      ls_linha16-estoque_quantidade = lv_saldo.
                      ls_linha16-valor_saida = <fs_prd>-netwr.
                      ls_linha16-quantidade_diversas = <fs_prd>-menge.
                    ENDIF.
                  ELSEIF ls_linha17 IS INITIAL.
                    ls_linha17-especie = <fs_prd>-model.
                    ls_linha17-serie_subserie = <fs_prd>-series.
                    ls_linha17-numero = <fs_prd>-docnum.
                    ls_linha17-dia = <fs_prd>-docdat+6(2).
                    ls_linha17-mes = <fs_prd>-docdat+4(2).
                    ls_linha17-contabil = <fs_prd>-konts.

                    IF <fs_prd>-direct = 1.
                      lv_saldo = lv_saldo + <fs_prd>-menge.
                      ls_linha17-estoque_quantidade = lv_saldo.
                      ls_linha17-valor = <fs_prd>-netwr.
                      ls_linha17-quantidade = <fs_prd>-menge.
                    ELSEIF <fs_prd>-direct = 2.
                      lv_saldo = lv_saldo - <fs_prd>-menge.
                      ls_linha17-estoque_quantidade = lv_saldo.
                      ls_linha17-valor_saida = <fs_prd>-netwr.
                      ls_linha17-quantidade_diversas = <fs_prd>-menge.
                    ENDIF.
                  ELSEIF ls_linha18 IS INITIAL.
                    ls_linha18-especie = <fs_prd>-model.
                    ls_linha18-serie_subserie = <fs_prd>-series.
                    ls_linha18-numero = <fs_prd>-docnum.
                    ls_linha18-dia = <fs_prd>-docdat+6(2).
                    ls_linha18-mes = <fs_prd>-docdat+4(2).
                    ls_linha18-contabil = <fs_prd>-konts.

                    IF <fs_prd>-direct = 1.
                      lv_saldo = lv_saldo + <fs_prd>-menge.
                      ls_linha18-estoque_quantidade = lv_saldo.
                      ls_linha18-valor = <fs_prd>-netwr.
                      ls_linha18-quantidade = <fs_prd>-menge.
                    ELSEIF <fs_prd>-direct = 2.
                      lv_saldo = lv_saldo - <fs_prd>-menge.
                      ls_linha18-estoque_quantidade = lv_saldo.
                      ls_linha18-valor_saida = <fs_prd>-netwr.
                      ls_linha18-quantidade_diversas = <fs_prd>-menge.
                    ENDIF.
                  ELSEIF ls_linha19 IS INITIAL.
                    ls_linha19-especie = <fs_prd>-model.
                    ls_linha19-serie_subserie = <fs_prd>-series.
                    ls_linha19-numero = <fs_prd>-docnum.
                    ls_linha19-dia = <fs_prd>-docdat+6(2).
                    ls_linha19-mes = <fs_prd>-docdat+4(2).
                    ls_linha19-contabil = <fs_prd>-konts.

                    IF <fs_prd>-direct = 1.
                      lv_saldo = lv_saldo + <fs_prd>-menge.
                      ls_linha19-estoque_quantidade = lv_saldo.
                      ls_linha19-valor = <fs_prd>-netwr.
                      ls_linha19-quantidade = <fs_prd>-menge.
                    ELSEIF <fs_prd>-direct = 2.
                      lv_saldo = lv_saldo - <fs_prd>-menge.
                      ls_linha19-estoque_quantidade = lv_saldo.
                      ls_linha19-valor_saida = <fs_prd>-netwr.
                      ls_linha19-quantidade_diversas = <fs_prd>-menge.
                    ENDIF.

                  ENDIF.

                  IF lv_lines = lv_count_print.

                    ls_output-tddest     = 'LOCL'.
                    ls_output-tdnewid    = abap_true.        " Nova ordem
                    ls_output-tdimmed    = abap_true.        " Saída imediata
                    ls_output-tddelete   = abap_false.       " Eliminar após saída
                    ls_output-tdcovtitle = <fs_prd>-matnr.   " Titulo do Spool

                    ls_parameters-device     = 'PRINTER'.    " Impressora
                    ls_parameters-no_dialog  = abap_true.
                    ls_parameters-getotf     = abap_true.

                    CALL FUNCTION lv_fm_name
                      EXPORTING
                        control_parameters = ls_parameters
                        output_options     = ls_output
                        user_settings      = abap_false
                        iv_produto         = lv_produto
                        iv_unidade         = <fs_prd>-meins
                        iv_ncm             = lv_ncm
                        is_linha01         = ls_linha01
                        is_linha02         = ls_linha02
                        is_linha03         = ls_linha03
                        is_linha04         = ls_linha04
                        is_linha05         = ls_linha05
                        is_linha06         = ls_linha06
                        is_linha07         = ls_linha07
                        is_linha08         = ls_linha08
                        is_linha09         = ls_linha09
                        is_linha10         = ls_linha10
                        is_linha11         = ls_linha11
                        is_linha12         = ls_linha12
                        is_linha13         = ls_linha13
                        is_linha14         = ls_linha14
                        is_linha15         = ls_linha15
                        is_linha16         = ls_linha16
                        is_linha17         = ls_linha17
                        is_linha18         = ls_linha18
                        is_linha19         = ls_linha19
                      IMPORTING
                        job_output_info    = ls_otfdata
                        job_output_options = ls_output_options
*                      TABLES
*                       it_boletos         = lt_boletos
                      EXCEPTIONS
                        formatting_error   = 1
                        internal_error     = 2
                        send_error         = 3
                        user_canceled      = 4
                        OTHERS             = 5.

                    FREE: lv_count_print, lv_matnr_old, ls_parameters, ls_output.

                  ENDIF.

                ENDIF.

              ENDIF.



            ENDLOOP.
          ELSE.
            data_display( ).
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE 'Dados não encontrado' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

  ENDMETHOD.

  METHOD busca_pasta.

    "Chama método para selecionar diretório
    CALL METHOD cl_gui_frontend_services=>directory_browse
      EXPORTING
        window_title         = 'Selecione a pasta desejada para upload do Excel'
        initial_folder       = 'C:/'
      CHANGING
        selected_folder      = lv_folder
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.

    IF sy-subrc = 0.
      "Escreve a data em formato correto
      WRITE sy-datum TO lv_data.
      CONCATENATE lv_folder '\Relatório_dia_' lv_data '.xlsx' INTO p_file. "APPC - DAM - Alteração no tipo do arquivo
    ELSE.
      MESSAGE e836(sd) WITH TEXT-005.
    ENDIF.

  ENDMETHOD.

  METHOD gerar_excel.
*** APPC - DAM - Alteração no tipo do arquivo - inicio 23/03/2023
    DATA: mt_fcat    TYPE lvc_t_fcat,
          m_version  TYPE string,
          m_file_ty  TYPE salv_bs_constant,
          m_flavour  TYPE string,
          lv_xstring TYPE xstring,
          lv_size    TYPE i,
          lt_bintab  TYPE solix_tab,
          mt_data    TYPE REF TO data.

    IF lt_excel IS NOT INITIAL.
      GET REFERENCE OF lt_excel INTO mt_data.
    ELSE.
      GET REFERENCE OF lt_sinte INTO mt_data.
    ENDIF.

*    APPEND INITIAL LINE TO lt_head ASSIGNING FIELD-SYMBOL(<fs_head>).
*    <fs_head>-nome_cabecalho = 'Nº_documento'.

    lv_filename = p_file.

    TRY.
        IF lt_excel IS NOT INITIAL.
          cl_salv_table=>factory(
            EXPORTING
              list_display   = if_salv_c_bool_sap=>false
            IMPORTING
              r_salv_table   = mo_salv_table
            CHANGING
              t_table        = lt_excel
          ).
        ELSEIF lt_sinte IS NOT INITIAL. " APPC - Erisom Almeida - MM.647326 - 28/08/2024
          cl_salv_table=>factory(
          EXPORTING
            list_display   = if_salv_c_bool_sap=>false
          IMPORTING
            r_salv_table   = mo_salv_table
          CHANGING
            t_table        = lt_sinte
        ).
** APPC - Erisom Almeida - MM.647326 - 28/08/2024 - Inicio
        ELSE.
          cl_salv_table=>factory(
            EXPORTING
              list_display   = if_salv_c_bool_sap=>false

            IMPORTING
              r_salv_table   = mo_salv_table
            CHANGING
              t_table        = lt_a_prd
          ).

        ENDIF.
** APPC - Erisom Almeida - MM.647326 - 28/08/2024 - Fim
      CATCH cx_salv_msg.
    ENDTRY.

    lo_columns = mo_salv_table->get_columns( ).
    lo_agg    = mo_salv_table->get_aggregations( ).
    mt_fcat   = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                  r_columns      = lo_columns
                  r_aggregations = lo_agg
                ).

    IF cl_salv_bs_a_xml_base=>get_version( ) = if_salv_bs_xml=>version_25 OR
       cl_salv_bs_a_xml_base=>get_version( ) = if_salv_bs_xml=>version_26.
      lo_resu_data = cl_salv_ex_util=>factory_result_data_table(
                      r_data   = mt_data
                      t_fieldcatalog = mt_fcat ).
    ENDIF.

    CASE cl_salv_bs_a_xml_base=>get_version( ).
      WHEN if_salv_bs_xml=>version_25.
        m_version = if_salv_bs_xml=>version_25.
      WHEN if_salv_bs_xml=>version_26.
        m_version = if_salv_bs_xml=>version_26.
    ENDCASE.

    m_file_ty = if_salv_bs_xml=>c_type_xlsx.

    m_flavour = if_salv_bs_c_tt=>c_tt_xml_flavour_export.

    CALL METHOD cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform(
      EXPORTING
        xml_type      = m_file_ty
        xml_version   = m_version
        r_result_data = lo_resu_data
        xml_flavour   = m_flavour
        gui_type      = if_salv_bs_xml=>c_gui_type_gui
      IMPORTING
        xml           = lv_xstring ).

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_xstring
      IMPORTING
        output_length = lv_size
      TABLES
        binary_tab    = lt_bintab.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = lv_size
        filename                  = lv_filename
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = lt_bintab
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24
           ).

*** APPC - DAM - Alteração no tipo do arquivo - Fim 28/03/2023

  ENDMETHOD.

  METHOD data_display.

    IF mo_salv_table IS NOT INITIAL.
      mo_salv_table->close_screen( ).
      CLEAR mo_salv_table.
    ENDIF.

    TRY.

        IF lt_sinte IS NOT INITIAL.
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table = mo_salv_table
            CHANGING
              t_table      = lt_sinte ).
        ELSEIF lt_excel IS NOT INITIAL. " APPC - MM.647326 - Erisom Almeida - 29/08/2024
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table = mo_salv_table
            CHANGING
              t_table      = lt_excel ).
** APPC - MM.647326 - Erisom Almeida - 29/08/2024 - Inicio
        ELSE.
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table   = mo_salv_table
            CHANGING
              t_table        = lt_a_prd
          ).
** APPC - MM.647326 - Erisom Almeida - 29/08/2024 - Fim
        ENDIF.
      CATCH cx_salv_msg INTO DATA(lo_alv_cx).
        MESSAGE lo_alv_cx->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
    ENDTRY.

    IF rb_anali IS NOT INITIAL.

      mo_salv_table->set_screen_status( pfstatus      = 'ZSTS_GUI_1000'
                                        report        = sy-repid
                                        set_functions = mo_salv_table->c_functions_all ).

      lo_functions = mo_salv_table->get_functions( ).
      lo_functions->set_all( abap_true ).

      lo_display = mo_salv_table->get_display_settings( ).
      lo_display->set_striped_pattern( abap_true ).

      lo_selects = mo_salv_table->get_selections( ).
      lo_selects->set_selection_mode( if_salv_c_selection_mode=>row_column ).

      lo_columns = mo_salv_table->get_columns( ).
      lo_columns->set_optimize( abap_true ).

      "Modificação Colunas
      change_columns( lo_columns ).

      "Adição de layout
      lo_layout     = mo_salv_table->get_layout( ).
      ls_key-report = sy-repid.

      lo_layout->set_key( ls_key ).
      IF p_layout IS NOT INITIAL.
        lo_layout->set_initial_layout( p_layout ).
      ENDIF.
      lo_layout->set_default( abap_true ).
      lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

      mo_salv_table->display( ).

    ELSE. "APPC - DAM - Alterando o ALV para mostrar apenas o subtotal por região - 23/03/2023
      mo_salv_table->set_screen_status( pfstatus      = 'ZSTS_GUI_1000'
                                      report        = sy-repid
                                      set_functions = mo_salv_table->c_functions_all ).

      lo_functions = mo_salv_table->get_functions( ).
      lo_functions->set_all( abap_true ).

      lo_display = mo_salv_table->get_display_settings( ).
      lo_display->set_striped_pattern( abap_true ).

      lo_selects = mo_salv_table->get_selections( ).
      lo_selects->set_selection_mode( if_salv_c_selection_mode=>row_column ).

      lo_columns = mo_salv_table->get_columns( ).
      lo_columns->set_optimize( abap_true ).

      "Modificação Colunas
      change_columns( lo_columns ).

      "Adição de layout
      lo_layout     = mo_salv_table->get_layout( ).
      ls_key-report = sy-repid.

      lo_agg = mo_salv_table->get_aggregations( ).

      CALL METHOD mo_salv_table->get_sorts
        RECEIVING
          value = lo_sort.

      lo_layout->set_key( ls_key ).
      IF p_layout IS NOT INITIAL.
        lo_layout->set_initial_layout( p_layout ).
      ENDIF.
      lo_layout->set_default( abap_true ).
      lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

      mo_salv_table->display( ).

    ENDIF.

  ENDMETHOD.

  METHOD change_columns.

    "Atribui as colunas do ALV
    lt_columns_table = io_columns->get( ).

    LOOP AT lt_columns_table INTO DATA(column).
      CASE column-columnname.
        WHEN 'CHAVE'.
          column-r_column->set_short_text( '' ).
          column-r_column->set_medium_text( '' ).
          column-r_column->set_long_text( 'Chave de acesso' ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD f4_layout.

    DATA: ls_layout TYPE salv_s_layout_info,
          ls_key    TYPE salv_s_layout_key.

    ls_key-report = sy-repid.

    ls_layout = cl_salv_layout_service=>f4_layouts(
      s_key    = ls_key
      restrict = iv_restrict ).

    cv_layout = ls_layout-layout.

  ENDMETHOD.

  METHOD gerar_txt.

    DATA: lt_tb     TYPE TABLE OF string,
          lv_quant  TYPE char20,
          lv_valor1 TYPE char50,
          lv_valor2 TYPE char50,
          lv_valor3 TYPE char50.

    CONCATENATE lv_arquivo 'Auditória' sy-datum '.txt' INTO lv_arquivo.

    LOOP AT lt_a_prd ASSIGNING FIELD-SYMBOL(<fs_a_prd>).
      lv_quant = <fs_a_prd>-menge.
      lv_valor1 = <fs_a_prd>-netpr.
      lv_valor2 = <fs_a_prd>-netwr.
      lv_valor3 = <fs_a_prd>-nfnett.
      CONDENSE:lv_quant, lv_valor1, lv_valor2, lv_valor3 NO-GAPS.

      APPEND INITIAL LINE TO lt_tb ASSIGNING FIELD-SYMBOL(<fs_tb>).
      CONCATENATE <fs_a_prd>-docnum ';' <fs_a_prd>-nftype ';' <fs_a_prd>-refitm ';' <fs_a_prd>-matnr ';' <fs_a_prd>-maktx ';' lv_quant ';' <fs_a_prd>-meins ';'
      lv_valor1 ';' lv_valor2 ';' <fs_a_prd>-direct ';' <fs_a_prd>-docdat ';' <fs_a_prd>-pstdat ';' lv_valor3 ';' <fs_a_prd>-model ';' <fs_a_prd>-series ';'
      <fs_a_prd>-branch ';' <fs_a_prd>-parid ';' <fs_a_prd>-nfenum ';' <fs_a_prd>-natop ';' <fs_a_prd>-inco1 ';' <fs_a_prd>-cfop ';' <fs_a_prd>-regio ';' <fs_a_prd>-name1 ';'
      <fs_a_prd>-konts ';' <fs_a_prd>-ncm ';' <fs_a_prd>-chave ';' INTO <fs_tb>.
    ENDLOOP.

    OPEN DATASET lv_arquivo FOR OUTPUT IN BINARY MODE.
    IF sy-subrc = 0.
      DATA(lv_linha) = concat_lines_of( sep = |\r\n| table = lt_tb ).
      DATA(lv_convert) = cl_abap_codepage=>convert_to( lv_linha ).

      TRANSFER lv_convert TO lv_arquivo.

      CLOSE DATASET lv_arquivo.
    ENDIF.

  ENDMETHOD.

  METHOD gerar_pdf.

    DATA: lv_tamanho TYPE i,
          lt_otf     TYPE TABLE OF itcoo,
          lt_pdf_tab TYPE TABLE OF tline.

    lt_otf = ls_otfdata-otfdata.

    "Realiza a conversão OTF
    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
        max_linewidth         = 132
      IMPORTING
        bin_filesize          = lv_tamanho
      TABLES
        otf                   = lt_otf
        lines                 = lt_pdf_tab
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        err_bad_otf           = 4.

    IF sy-subrc = 0.
*      TRANSFER
    ENDIF.

  ENDMETHOD.

ENDCLASS.
