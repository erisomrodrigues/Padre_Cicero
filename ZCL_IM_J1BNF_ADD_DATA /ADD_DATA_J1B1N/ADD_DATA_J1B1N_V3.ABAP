  METHOD if_j_1bnf_add_data~add_data_j1b1n.
    "Variáveis
    DATA: lv_cnpj      TYPE j_1bwfield-cgc_number,
          lv_field     TYPE string,
          lv_vlr       TYPE c LENGTH 30,
          lv_trib      TYPE j_1bnfstx-taxval,
          lv_icsp      TYPE j_1bnfstx-taxval,
          lv_icep      TYPE j_1bnfstx-taxval,
          lv_icap      TYPE j_1bnfstx-taxval,
          lv_numtp     TYPE mara-numtp,
          lv_ean12(12).

    DATA: ls_zsdt012  TYPE zsdt012,
          lt_zsdt012  TYPE TABLE OF zsdt012,
          lv_vbeln    TYPE vbfa-vbeln,
          lv_vbelv    TYPE vbfa-vbelv,
          ls_vbak     TYPE vbak,
          lv_text(15),
          lv_value    TYPE j_1bnfstx-taxval,
          lv_vbeln_ax TYPE vbrk-vbeln.

    DATA: ls_nflin TYPE j_1bnflin,
          ls_nfstx TYPE j_1bnfstx,
          lt_nfstx TYPE TABLE OF j_1bnfstx,
          ls_vbrp  TYPE vbrpvb,
          lt_komv  TYPE TABLE OF komv,
          lt_komvx TYPE TABLE OF komv,
          ls_komv  TYPE komv.

    "Range
    DATA: lr_taxtyp TYPE RANGE OF j_1bnfstx-taxtyp.

    "Ponteiros
    FIELD-SYMBOLS: <field> TYPE any,
                   <item>  LIKE LINE OF et_item,
                   <pay>   LIKE LINE OF et_payment,
                   <tax>   LIKE LINE OF it_nfstx.

    FIELD-SYMBOLS: <wkomv> TYPE ANY TABLE.

    "Objetos
    DATA: lo_stvarv TYPE REF TO zcl_read_stvarv.


    UNASSIGN: <item>, <pay>.
    CLEAR: lv_cnpj, lv_field, lv_vlr, lv_trib, lv_icsp, lv_icep,
           lv_icap, lo_stvarv.
    REFRESH: lr_taxtyp.

    DATA: lt_dados TYPE TABLE OF konw.

    FIELD-SYMBOLS: <fs_tkomp> TYPE komp,
                   <fs_tkomk> TYPE komk.

    ASSIGN ('(SAPLV60A)TKOMP') TO <fs_tkomp>.
    ASSIGN ('(SAPLV60A)TKOMK') TO <fs_tkomk>.

    IF <fs_tkomp> IS ASSIGNED AND <fs_tkomk> IS ASSIGNED.

      SELECT b~kstbw
             b~kbetr
        FROM a978 AS a
        INNER JOIN konw AS b
        ON b~knumh = a~knumh
        INNER JOIN konp AS c
        ON c~knumh = a~knumh
        INTO CORRESPONDING FIELDS OF TABLE lt_dados
        WHERE a~datbi     >= sy-datum
        AND   a~datab     <= sy-datum
        AND   a~kschl      = 'ZPDP'
        AND   a~xsubt      = <fs_tkomk>-xsubt
        AND   a~txreg_sf   = <fs_tkomp>-txreg_sf
        AND   a~txreg_st   = <fs_tkomp>-txreg_st
        AND   c~loevm_ko   = ''.

      IF lt_dados IS NOT INITIAL.

        DATA(lv_prodepe) = 'X'.

      ENDIF.

    ENDIF.
*
**** APPC - SD - HML - Determina prodepe - Inicio 17/02/2023
*    SELECT COUNT(*)
*     FROM tvarvc
*     WHERE name = 'ZSD_CONV_CFOP_PE'
*     AND   low =  @is_header-regio
*     AND   low  = ( SELECT regio
*                     FROM t001w
*                     WHERE j_1bbranch = @is_header-branch )
*     INTO @DATA(lv_prodepe).
**** APPC - SD - HML - Determina prodepe  - Fim 17/02/2023

    IF sy-tcode <> 'ZMM018'  AND
       sy-tcode <> 'ZSDR049' AND
       sy-tcode <> 'ZSD044'  AND
       sy-tcode <> 'ZSD045'  AND
       sy-tcode <> 'ZSDR050'.

      "Modelo 65 - Dados para NFC-e
      IF is_header-model = '65'.

        MOVE-CORRESPONDING is_header TO es_header.

        "IND_FINAL - NF-e ID do consumidor final
        IF is_header-ind_final IS INITIAL.
          es_header-ind_final = '0'.
          IF is_header-ind_iedest = '9' OR
             is_header-cpf IS NOT INITIAL.
            es_header-ind_final = '1'.
          ENDIF.
        ENDIF.

        "IND_PRES - NF-e Presença do consumidor final
        es_header-ind_pres = '1'. "Pessoal

        "ID_DEST - NF-e Identificador para local de destino
        es_header-id_dest = '1'.

        "MODFRETE - Modo Frete
        es_header-modfrete = '9'.

      ENDIF.

      "Dados adicionais na J1B1N para modelo NF 55
      IF is_header-model = '55'.
***APPC - SD - HML - INICIO 15/03/2023
        LOOP AT et_item ASSIGNING <item>.

*         APPC - SD.502110 - Ericky Fernandes - INÍCIO - 19/02/2024
          ASSIGN it_nflin[ itmnum = <item>-itmnum ] TO FIELD-SYMBOL(<lfs_nflin>).
          IF sy-subrc = 0.
            "Preenche o código benefício fiscal estado
            CALL FUNCTION 'ZSDF_GET_TAX_BENEFIT_CODE'
              EXPORTING
                iv_werks  = <lfs_nflin>-werks
                iv_cfop   = <lfs_nflin>-cfop
              IMPORTING
                ev_cbenef = <item>-cbenef.
          ENDIF.
*         APPC - SD.502110 - Ericky Fernandes - Fim - 19/02/2024

          IF <item>-cean IS INITIAL.
            <item>-cean      = zcl_im_j1bnf_add_data=>no_ean.
            <item>-cean_trib = <item>-cean.
          ELSE.
            <item>-cean_trib = <item>-cean.

            "Tratamento para EAN UPC (12 digitos)
            READ TABLE it_nflin INTO ls_nflin WITH KEY itmnum = <item>-itmnum.
            IF sy-subrc = 0.

              SELECT SINGLE numtp
                INTO lv_numtp
                FROM mara
               WHERE matnr = ls_nflin-matnr.

              IF lv_numtp = 'UC'. "UPC
                lv_ean12 = <item>-cean.
                SHIFT lv_ean12 RIGHT DELETING TRAILING space.
                TRANSLATE lv_ean12 USING ' 0'.
                <item>-cean = lv_ean12.
                <item>-cean_trib = <item>-cean.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
***APPC - SD - HML - FIM 15/03/2023
        "Dados Adicionais
        "Inserir na tag infcpl os textos das mensagens da nota
        LOOP AT it_nfftx ASSIGNING FIELD-SYMBOL(<fs_nfftx>).
          IF es_header-infcpl IS INITIAL.
            es_header-infcpl = <fs_nfftx>-message.
          ELSE.
            es_header-infcpl = es_header-infcpl &&
                               '/' &&
                               <fs_nfftx>-message.
          ENDIF.
        ENDLOOP.

        IF lv_prodepe IS NOT INITIAL.
          LOOP AT et_item ASSIGNING <item>.
            IF line_exists( it_nfstx[ taxtyp = 'ICM3' ] ).
              <item>-vbcefet   = it_nfstx[ taxtyp = 'ICM3' itmnum = <item>-itmnum ]-base.
              <item>-picmsefet = it_nfstx[ taxtyp = 'ICM3' itmnum = <item>-itmnum ]-rate.
              <item>-vicmsefet = it_nfstx[ taxtyp = 'ICM3' itmnum = <item>-itmnum ]-taxval.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ELSE.

*EAN Vazios -> "Sem GTIN"
      "Percorre a tabela de itens em busca de EAN vazio.
      LOOP AT et_item ASSIGNING <item>.
        IF <item>-cean IS INITIAL.
          <item>-cean      = zcl_im_j1bnf_add_data=>no_ean.
          <item>-cean_trib = <item>-cean.
        ELSE.
          <item>-cean_trib = <item>-cean.

          "Tratamento para EAN UPC (12 digitos)
          READ TABLE it_nflin INTO ls_nflin WITH KEY itmnum = <item>-itmnum.
          IF sy-subrc = 0.

            SELECT SINGLE numtp
              INTO lv_numtp
              FROM mara
             WHERE matnr = ls_nflin-matnr.

            IF lv_numtp = 'UC'. "UPC
              lv_ean12 = <item>-cean.
              SHIFT lv_ean12 RIGHT DELETING TRAILING space.
              TRANSLATE lv_ean12 USING ' 0'.
              <item>-cean = lv_ean12.
              <item>-cean_trib = <item>-cean.
            ENDIF.

          ENDIF.

*        TRY.
*            DATA: lv_ean(13).
*            lv_ean = <item>-cean.
*            UNPACK lv_ean TO lv_ean.
*            <item>-cean      = lv_ean.
*            <item>-cean_trib = lv_ean.
*          CATCH cx_root.
*        ENDTRY.
        ENDIF.
        IF line_exists( it_nfstx[ taxtyp = 'ICM3' ] ) AND lv_prodepe IS NOT INITIAL.
          <item>-vbcefet   = it_nfstx[ taxtyp = 'ICM3' itmnum = <item>-itmnum ]-base.
          <item>-picmsefet = it_nfstx[ taxtyp = 'ICM3' itmnum = <item>-itmnum ]-rate.
          <item>-vicmsefet = it_nfstx[ taxtyp = 'ICM3' itmnum = <item>-itmnum ]-taxval.
        ENDIF.
      ENDLOOP.
* Dados Adicional.

      "Buscando configurações da TVARV para Aliq. ICMS Reduzida
      CREATE OBJECT lo_stvarv EXPORTING prefix = 'ZNFE' separator = '.'.

      lo_stvarv->get_seloption( EXPORTING suffix = 'TIPOIMPOSTO_DADOS_ADIC'
                                IMPORTING value  = lr_taxtyp ).
      "Percorre a tabela de impostos.
      LOOP AT it_nfstx ASSIGNING <tax>.
        "Adiciona os tributos para ter um valor aproximado dos mesmos.
        ADD <tax>-taxval TO lv_trib.
        "Verifica se existe o tipo de imposto para Aliq. ICMS Reduzida.
        IF <tax>-taxtyp IN lr_taxtyp.
          IF <tax>-taxval LT 0.
            CLEAR: lv_field.
            "Monta dinamicamente a variável que irá receber o valor.
            lv_field = 'LV_' && <tax>-taxtyp.
            ASSIGN (lv_field) TO <field>.
            <field> = <tax>-taxval.
          ENDIF.
        ENDIF.
      ENDLOOP.

      "Verifica se existe valor para "Aliq. ICMS Reduzida" e atribui
      "texto nos dados adicionais.
      IF lv_icsp IS NOT INITIAL OR
         lv_icep IS NOT INITIAL OR
         lv_icap IS NOT INITIAL.
        CLEAR: lv_field, lv_vlr.
        lv_field = zcl_im_j1bnf_add_data=>aliq_icms_red_val.

        WRITE lv_icsp TO lv_vlr CURRENCY 'BRL'. CONDENSE lv_vlr.
        REPLACE '&' IN lv_field WITH lv_vlr. CLEAR lv_vlr.
        WRITE lv_icep TO lv_vlr CURRENCY 'BRL'. CONDENSE lv_vlr.
        REPLACE '&' IN lv_field WITH lv_vlr. CLEAR lv_vlr.
        WRITE lv_icap TO lv_vlr CURRENCY 'BRL'. CONDENSE lv_vlr.
        REPLACE '&' IN lv_field WITH lv_vlr. CLEAR lv_vlr.

        es_header-infcpl = zcl_im_j1bnf_add_data=>aliq_icms_red &&
                           cl_abap_char_utilities=>cr_lf &&
                           lv_field.
      ENDIF.

      "Verifica se existe valor para "Tributos aproximados" e atribui
      "texto nos dados adicionais.
      IF lv_trib IS NOT INITIAL.
        CLEAR: lv_vlr.
        WRITE lv_trib TO lv_vlr CURRENCY 'BRL'. CONDENSE lv_vlr.
        es_header-infcpl = zcl_im_j1bnf_add_data=>tributos &&
                           space &&
                           lv_vlr.
      ENDIF.

      "GFX - FAGP - Dados de impostos da ordem de venda - Tag Infcpl
      IF is_header-model = '55'.

*        READ TABLE it_vbrp INTO ls_vbrp INDEX 1.
*        IF sy-subrc = 0.
*
*          "Obter tipo da ordem de venda da NFe
*          SELECT SINGLE *
*            INTO ls_vbak
*            FROM vbak
*           WHERE vbeln = ls_vbrp-aubel.
*
*          "Seleciona os codigos de impostos para garantia
*          IF NOT ls_vbak-auart IS INITIAL.
*            SELECT *
*              INTO TABLE lt_zsdt012
*              FROM zsdt012
*             WHERE auart = ls_vbak-auart.
*          ENDIF.
*
*          ASSIGN ('(SAPLJ1BG)WKOMV[]') TO <wkomv>.
*          IF sy-subrc = 0.
*            lt_komv[] = <wkomv>[].
*          ENDIF.
*
*          "Insere nas informações complementares os textos de impostos
*          IF NOT lt_zsdt012[] IS INITIAL.
*
*            LOOP AT lt_zsdt012 INTO ls_zsdt012.
*
*              CLEAR lv_value.
*              lt_nfstx[] = it_nfstx[].
*
*              "Tratamento para calculo de cada imposto
*              CASE ls_zsdt012-cod_imposto.
*
*                WHEN 'IPI_OUTRAS'.
*                  lv_text = 'Valor IPI:'.
*                  LOOP AT it_nflin INTO ls_nflin.
*                    lv_value = lv_value + ls_nflin-nfoth.
*                  ENDLOOP.
*
*                WHEN 'BS_ICMS_ST'.
*                  lv_text = 'Base ICMS ST:'.
*                  DELETE lt_nfstx WHERE taxtyp <> 'ICS3'.
*                  LOOP AT lt_nfstx INTO ls_nfstx.
*                    lv_value = lv_value + ls_nfstx-base.
*                  ENDLOOP.
*
*                WHEN 'VL_ICMS_ST'.
*                  lv_text = 'Valor ICMS ST:'.
*                  DELETE lt_nfstx WHERE taxtyp <> 'ICS3'.
*                  LOOP AT lt_nfstx INTO ls_nfstx.
*                    lv_value = lv_value + ls_nfstx-taxval.
*                  ENDLOOP.
*
*                WHEN 'ICMS_ORI'.
*                  lv_text = 'Valor ICMS ORI:'.
*                  DELETE lt_nfstx WHERE taxtyp <> 'ICEP'.
*                  LOOP AT lt_nfstx INTO ls_nfstx.
*                    lv_value = lv_value + ls_nfstx-taxval.
*                  ENDLOOP.
*
*                WHEN 'ICMS_DES'.
*                  lv_text = 'Valor ICMS DES:'.
*                  DELETE lt_nfstx WHERE taxtyp <> 'ICAP'.
*                  LOOP AT lt_nfstx INTO ls_nfstx.
*                    lv_value = lv_value + ls_nfstx-taxval.
*                  ENDLOOP.
*
*                WHEN 'ICMS_FCP'.
*                  lv_text = 'Valor ICMS FCP:'.
*                  DELETE lt_nfstx WHERE taxtyp <> 'ICSP'.
*                  LOOP AT lt_nfstx INTO ls_nfstx.
*                    lv_value = lv_value + ls_nfstx-taxval.
*                  ENDLOOP.
*
*                WHEN 'VL_ICMS'.
*                  lv_text = 'Valor ICMS:'.
*                  DELETE lt_nfstx WHERE taxtyp <> 'ICM3'.
*                  LOOP AT lt_nfstx INTO ls_nfstx.
*                    lv_value = lv_value + ls_nfstx-taxval.
*                  ENDLOOP.
*
*                WHEN 'IPI_DEV_G'.
*                  lv_text = 'Valor IPI:'.
*                  lt_komvx[] = lt_komv[].
*                  DELETE lt_komvx WHERE kschl <> 'BX23'.
*                  LOOP AT lt_komvx INTO ls_komv.
*                    lv_value = lv_value + ls_komv-kwert.
*                  ENDLOOP.
*
*                WHEN 'ICMS_DEV_G'.
*                  lv_text = 'Valor ICMS ST:'.
*                  lt_komvx[] = lt_komv[].
*                  DELETE lt_komvx WHERE kschl <> 'BX41'.
*                  LOOP AT lt_komvx INTO ls_komv.
*                    lv_value = lv_value + ls_komv-kwert.
*                  ENDLOOP.
*
*                WHEN 'ICMSST_FCP'.
*                  lv_text = 'Valor FCP-ST:'.
*                  DELETE lt_nfstx WHERE taxtyp <> 'ICFP'.
*                  LOOP AT lt_nfstx INTO ls_nfstx.
*                    lv_value = lv_value + ls_nfstx-taxval.
*                  ENDLOOP.
*
*              ENDCASE.
*
*              "Adicionar na tag infcpl
*              IF NOT lv_value IS INITIAL.
*                CLEAR: lv_vlr.
*                WRITE lv_value TO lv_vlr CURRENCY 'BRL'. CONDENSE lv_vlr.
*                es_header-infcpl = es_header-infcpl &&
*                                   ' | ' &&
*                                   lv_text &&
*                                   lv_vlr.
*              ENDIF.
*
*            ENDLOOP.
*
*          ENDIF.
*        ENDIF.

      ENDIF.

*Payment
      "Ajuste para tabela Payment - TEMPORÁRIO
      IF et_payment[] IS INITIAL.
        APPEND INITIAL LINE TO et_payment ASSIGNING <pay>.
        IF <pay> IS ASSIGNED.
***** GFX - JECA - 11.09.2018 - Ajuste Venda e devolução
          IF is_header-nftype EQ 'Z3' OR is_header-nftype EQ 'Z4' OR is_header-nftype EQ 'Z5' OR is_header-nftype EQ 'Z7' OR
             is_header-nftype EQ 'ZG' OR is_header-nftype EQ 'YB' OR is_header-nftype EQ 'E4' OR is_header-nftype EQ 'R4'.

            <pay>-t_pag = '90'.
            <pay>-v_pag = ''.

          ELSE.
            <pay>-t_pag      = '15'.
            <pay>-v_pag      = is_header-nftot.
          ENDIF.
* APPC - SD.537330 - Erisom Almeida - 08/05/2024 - Inicio
          SELECT SINGLE tp_integra
                        FROM zsdt097
                        WHERE regio = @is_header-regio
                        AND t_pag   = @<pay>-t_pag INTO @<pay>-tp_integra.
* APPC - SD.537330 - Erisom Almeida - 08/05/2024 - Fim
* Appc - SD.537330 - Erisom Almeida - 25/04/2024 - Inicio
          IF <pay>-t_pag NE '03' OR <pay>-t_pag NE '04' OR <pay>-t_pag NE '10' OR <pay>-t_pag NE '11' OR <pay>-t_pag NE '12' OR <pay>-t_pag NE '13' OR
             <pay>-t_pag NE '15' OR <pay>-t_pag NE '17' OR <pay>-t_pag NE '18'.

            <pay>-tp_integra = '1'.
            <pay>-t_band     = 99.
            <pay>-c_aut      = 99999999999999999999.
            CALL FUNCTION 'J_1BREAD_BRANCH_DATA' "Recuperar o CNPJ.
              EXPORTING
                branch            = is_header-branch
                bukrs             = is_header-bukrs
              IMPORTING
                cgc_number        = <pay>-cnpj
              EXCEPTIONS
                branch_not_found  = 1
                address_not_found = 2
                company_not_found = 3
                OTHERS            = 4.

            CLEAR: <pay>-tp_integra,<pay>-t_band,<pay>-c_aut,<pay>-cnpj.
          ENDIF.
        ENDIF.
      ENDIF.

      "Modelo 65 - Dados para NFC-e
      IF is_header-model = '65'.

        MOVE-CORRESPONDING is_header TO es_header.

        "IND_FINAL - NF-e ID do consumidor final
        IF is_header-ind_final IS INITIAL.
          es_header-ind_final = '0'.
          IF is_header-ind_iedest = '9' OR
             is_header-cpf IS NOT INITIAL.
            es_header-ind_final = '1'.
          ENDIF.
        ENDIF.

        "IND_PRES - NF-e Presença do consumidor final
        es_header-ind_pres = '1'. "Pessoal

        "ID_DEST - NF-e Identificador para local de destino
        es_header-id_dest = '1'.

        "MODFRETE - Modo Frete
        es_header-modfrete = '9'.

      ENDIF.

*    IF is_header-model = '55' OR is_header-model = '59' OR is_header-model = '65'.
*
*
*    ENDIF.

    ENDIF.

**** GFX - LGMO - Atualização NT 2018.005 - 25/04/2019 - Início ****

    "Responsável Técnico
    SELECT SINGLE c~uf,
                  c~cnpj,
                  c~contact,
                  c~email,
                  c~phone,
                  c~idcsrt,
                  c~csrt
      FROM j_1bbranch AS a INNER JOIN
           adrc       AS b ON ( a~adrnr = b~addrnumber ) INNER JOIN
           zsdt015    AS c ON ( c~uf = b~region )
      INTO @DATA(ls_zsdt015)
      WHERE a~bukrs  = @is_header-bukrs
        AND a~branch = @is_header-branch.

    IF sy-subrc IS INITIAL.

      es_tec_resp-cnpj      = ls_zsdt015-cnpj   .
      es_tec_resp-contact   = ls_zsdt015-contact.
      es_tec_resp-email     = ls_zsdt015-email  .
      es_tec_resp-phone     = ls_zsdt015-phone  .
      es_tec_resp-idcsrt    = ls_zsdt015-idcsrt .
      es_tec_resp-csrt      = ls_zsdt015-csrt   .

    ENDIF.

**** GFX - LGMO - Atualização NT 2018.005 - 25/04/2019 - FIM    ****

  ENDMETHOD.
