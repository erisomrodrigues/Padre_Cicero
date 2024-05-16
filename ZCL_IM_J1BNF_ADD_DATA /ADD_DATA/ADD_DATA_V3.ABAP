  METHOD if_j_1bnf_add_data~add_data.

    TYPES: BEGIN OF ty_formpag,                                  "NT 2020.006
             marc(1)            TYPE c,                          "NT 2020.006
             ident_cartao       TYPE zfie011-ident_cartao,       "NT 2020.006
             dcr_meio_pagamento TYPE zfit008-dcr_meio_pagamento, "NT 2020.006
             dmbtr              TYPE bsid-dmbtr,                 "NT 2020.006
             num_apr_cheque     TYPE zfit006-num_apr_cheque,     "NT 2020.006
             cmc7               TYPE zfit006-cmc7,               "NT 2020.006
             data_bom_para      TYPE zfit006-data_bom_para,      "NT 2020.006
             tp_pagto_cartao    TYPE zfit006-tp_pagto_cartao,    "NT 2020.006
             modalidade         TYPE zfit006-modalidade,         "NT 2020.006
             cartao             TYPE zfit006-cartao,             "NT 2020.006
             cartao_tef         TYPE zfit006-cartao_tef,         "NT 2020.006
             cartao_c2d_ok      TYPE zfit006-cartao_c2d_ok,      "NT 2020.006
             parcelas           TYPE zfit006-parcelas,           "NT 2020.006
             apr_pos            TYPE zfit006-apr_pos,            "NT 2020.006
             cod_adq            TYPE zfit006-cod_adq,            "NT 2020.006
             cliente            TYPE zfit006-cliente,            "NT 2020.006
             serial             TYPE zfit015-serial,             "NT 2020.006
             estabelecimento    TYPE zfit015-estabelecimento,    "NT 2020.006
             data_venda         TYPE sy-datum,                   "NT 2020.006
             bin(40)            TYPE c,                          "NT 2020.006
             donocartao(40)     TYPE c,                          "NT 2020.006
             dtexpiracao(40)    TYPE c,                          "NT 2020.006
             adquirente(40)     TYPE c,                          "NT 2020.006
             qtrodigitos(40)    TYPE c,                          "NT 2020.006
             num_transacao_nsu  TYPE zfie011-num_transacao_nsu,  "NT 2020.006
             cod_autorizacao    TYPE zfie011-cod_autorizacao,    "NT 2020.006
             nsu_host           TYPE zfie011-nsu_host,           "NT 2020.006
           END OF ty_formpag.                                    "NT 2020.006

    TYPES: ty_t_formpag TYPE TABLE OF ty_formpag WITH DEFAULT KEY.   "NT 2020.006

    "Variáveis
    DATA: lv_cnpj      TYPE j_1bwfield-cgc_number,
          lv_field     TYPE string,
          lv_vlr       TYPE c LENGTH 30,
          lv_trib      TYPE j_1bnfstx-taxval,
          lv_icsp      TYPE j_1bnfstx-taxval,
          lv_icep      TYPE j_1bnfstx-taxval,
          lv_icap      TYPE j_1bnfstx-taxval,
          lv_numtp     TYPE mara-numtp,
          lv_counter   TYPE j_1b_nf_counter4,    "NT 2020.006
          lv_codadq    TYPE /gfxdsi/ecodadq,     "NT 2020.006
          lv_stcd1     TYPE stcd1,               "NT 2020.006
          lv_val_pag   TYPE j_1bnfdoc-nftot,     "NT 2020.006
          lv_ean12(12),
          lv_dtvenc    TYPE sy-datum, "APPC - RMS - 23/03/2022
          lv_zbd1t     TYPE bsid-zbd1t, "APPC - RMS - 23/03/2022
          lv_vlrfecop  TYPE j_1bnfstx-taxval, "APPC - RMS - 10/06/2022
          lv_contador  TYPE j_1bnftradenotes-counter, "APPC - RMS - 23/03/2022
          lv_frete     TYPE j_1bnetfre. "APPC - SD.442166 - Hemilly Maria - 09/10/2023

    DATA: ls_zsdt012  TYPE zsdt012,
          lt_zsdt012  TYPE TABLE OF zsdt012,
          lv_vbeln    TYPE vbfa-vbeln,
          lv_vbelv    TYPE vbfa-vbelv,
          ls_vbak     TYPE vbak,
          lv_text(15),
          lv_value    TYPE j_1bnfstx-taxval,
          lv_vbeln_ax TYPE vbrk-vbeln,
          lv_docs     TYPE string, "GFX - JBRS - DEVK922552 - 17/08/2021
*         APPC - RMS - 23/03/2022
          lv_numline  TYPE numc3,
          lv_count    TYPE int8,
          lt_dias     TYPE TABLE OF t052,
          lv_parcelas TYPE i,
          lv_vlrtotst TYPE j_1bnftradenotes-vdup,
          lv_vlrtodif TYPE j_1bnftradenotes-vdup,
          lt_wkomv    TYPE TABLE OF komv.
*         APPC - RMS - 23/03/2022

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

    FIELD-SYMBOLS: <wkomv>        TYPE ANY TABLE,
*** GFX - JBRS - INICIO - DEVK916724 - 21/02/2020 - Ajuste nos dados de pagamento do cupom
                   <fs_t_formpag> TYPE ty_t_formpag.
*** GFX - JBRS - FIM - DEVK916724 - 21/02/2020 - Ajuste nos dados de pagamento do cupom

    "Objetos
    DATA: lo_stvarv TYPE REF TO zcl_read_stvarv.

    UNASSIGN: <item>, <pay>.
    CLEAR: lv_cnpj, lv_field, lv_vlr, lv_trib, lv_icsp, lv_icep,
           lv_icap, lo_stvarv.
    REFRESH: lr_taxtyp.

*   APPC - SD.467874 - Ericky Fernandes - INÍCIO - 06/02/2024
    "Busca a tabela KOMV da pilha
    ASSIGN ('(SAPLJ1BG)WKOMV[]') TO <wkomv>.
    IF sy-subrc = 0.
      lt_komv[] = <wkomv>[].
    ENDIF.
*   APPC - SD.467874 - Ericky Fernandes - FIM - 06/02/2024

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

***APPC - SD.442166 - Hemilly Maria - INICIO - 09/10/2023
          lv_frete = ls_nflin-netfre + lv_frete.
***APPC - SD.442166 - Hemilly Maria - FIM - 09/10/2023

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

*     APPC - SD.456913 - Ericky Fernandes - INÍCIO - 17/12/2023
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
*     APPC - SD.456913 - Ericky Fernandes - FIM - 17/12/2023

*     APPC - SD.467874 - Ericky Fernandes - INÍCIO - 06/02/2024
      "Verifica se deve inserir o código da conta
      IF line_exists( lt_komv[ kvsl1 = 'ZDV' ] ).
        <item>-cod_cta = lt_komv[ kvsl1 = 'ZDV' ]-sakn1.
      ENDIF.
*     APPC - SD.467874 - Ericky Fernandes - FIM - 06/02/2024

    ENDLOOP.

    "Limpa a tabela para proxima lógica
    FREE lt_komv. " APPC - SD.467874 - Ericky Fernandes - 06/02/2024

*    "Preencher PIPIDEVOL p/ Devolução de Garantia
*    LOOP AT it_nflin INTO ls_nflin.
*      IF ls_nflin-reftyp = 'BI' AND
*         ls_nflin-pipidevol IS INITIAL AND
*         is_header-doctyp = '6' AND
*         is_header-direct = '2'.
*        ls_nflin-pipidevol = '100'.
*        MODIFY it_nflin FROM ls_nflin.
*      ENDIF.
*    ENDLOOP.

* Dados Adicional.

    "Buscando configurações da TVARV para Aliq. ICMS Reduzida
    CREATE OBJECT lo_stvarv EXPORTING prefix = 'ZNFE' separator = '.'.

    lo_stvarv->get_seloption( EXPORTING suffix = 'TIPOIMPOSTO_DADOS_ADIC'
                              IMPORTING value  = lr_taxtyp ).

*** GFX - DFLC - F-PACI185 - Início 08/01/2021
    LOOP AT et_item ASSIGNING FIELD-SYMBOL(<ls_et_item_aux>).
      CLEAR <ls_et_item_aux>-vtottrib.
    ENDLOOP.
*** GFX - DFLC - F-PACI185 - Fim 08/01/2021

    " < --- GFX - JEBS - F-PACI294 - INICIO - 17/03/2023 --- > "

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

    " < --- GFX - JEBS - F-PACI294 - FIM - 17/03/2023 --- > "

    IF lv_prodepe IS INITIAL.

      "Percorre a tabela de impostos.
      LOOP AT it_nfstx ASSIGNING <tax>.
        "Adiciona os tributos para ter um valor aproximado dos mesmos.
        ADD <tax>-taxval TO lv_trib.
*** GFX - JBRS - INICIO - 10/12/2019
        READ TABLE et_item ASSIGNING FIELD-SYMBOL(<fs_et_item>) WITH KEY itmnum = <tax>-itmnum.
        IF sy-subrc = 0.
          <fs_et_item>-vtottrib = <fs_et_item>-vtottrib + <tax>-taxval.
        ENDIF.
*** GFX - JBRS - FIM - 10/12/2019
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

    ENDIF.

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

      READ TABLE it_vbrp INTO ls_vbrp INDEX 1.
      IF sy-subrc = 0.

        "Obter tipo da ordem de venda da NFe
        SELECT SINGLE *
          INTO ls_vbak
          FROM vbak
         WHERE vbeln = ls_vbrp-aubel.

*** GFX - JBRS - INICIO - DEVK916724 - 27/02/2020
      ELSE.
        lv_vbeln = CONV #( is_mkpf-xblnr ).

        "Seleciona o tipo de documento de vendas
        SELECT SINGLE b~auart
          FROM lips AS a
          INNER JOIN j_1bsdica AS b
            ON ( b~pstyv = a~pstyv )
          WHERE a~vbeln = @lv_vbeln
          INTO @ls_vbak-auart.

        FREE: lv_vbeln.
      ENDIF.
*** GFX - JBRS - FIM - DEVK916724 - 27/02/2020

      "Seleciona os codigos de impostos para garantia
      IF NOT ls_vbak-auart IS INITIAL.
        SELECT *
          INTO TABLE lt_zsdt012
          FROM zsdt012
         WHERE auart = ls_vbak-auart.
      ENDIF.

      ASSIGN ('(SAPLJ1BG)WKOMV[]') TO <wkomv>.
      IF sy-subrc = 0.
        lt_komv[] = <wkomv>[].
      ENDIF.

* APPC - RMS - INICIO - 22/03/2022
      IF is_vbrk-zlsch = 'A' OR is_vbrk-zlsch = 'D'.

        DATA: lv_vlrtot TYPE j_1b_billing_org_value,
              lv_vlrdes TYPE j_1b_discount_value,
              lv_vlrliq TYPE j_1b_billing_net_value,
              lv_vlrpar TYPE j_1bnflin-netwrt.

        LOOP AT it_vbrp ASSIGNING FIELD-SYMBOL(<fs_vbrp>).

          DATA(lv_remessa) = <fs_vbrp>-vgbel.

        ENDLOOP.

*      SELECT SINGLE vgbel
*        FROM lips
*        WHERE VBELN = @lv_remessa
*        INTO @DATA(lv_orvend).

        SELECT SINGLE knumv
          FROM vbak
          INNER JOIN lips
          ON  ( lips~vbeln = @lv_remessa )
          WHERE vbak~vbeln = lips~vgbel
          INTO @DATA(lv_knumv).

        READ TABLE it_vbrp ASSIGNING FIELD-SYMBOL(<fs_vbrpline>) INDEX 1.

        IF  sy-subrc = 0.

*        "Selecionando valor total da NF
          LOOP AT lt_komv ASSIGNING FIELD-SYMBOL(<fs_vlrtot>) WHERE knumv = <fs_vbrpline>-vbeln AND kschl = 'ZDVE' AND kawrt > 0 AND kbetr = 0 .

            lv_vlrtot = lv_vlrtot + <fs_vlrtot>-kawrt.

          ENDLOOP.

          IF <fs_vlrtot> IS ASSIGNED.
            "Preenchendo o valor de desconto da NF
            LOOP AT lt_komv ASSIGNING FIELD-SYMBOL(<fs_vlr_desc>) WHERE knumv = <fs_vbrpline>-vbeln AND kschl = 'ZDVE' AND kbetr <> 0.

              lv_vlrdes = lv_vlrdes + ( <fs_vlr_desc>-kbetr * -1 ).

            ENDLOOP.

          ENDIF.

          "Passando o valor do ST para a variável
          LOOP AT lt_komv ASSIGNING FIELD-SYMBOL(<fs_vlr_st>) WHERE knumv = <fs_vbrpline>-vbeln AND kschl = 'ZCS1' AND kwert > 0.

            lv_vlrtotst = lv_vlrtotst + <fs_vlr_st>-kwert.

          ENDLOOP.

          IF <fs_vlr_st> IS ASSIGNED.
            "Buscando valor minimo do ST
            SELECT SINGLE b~ztag1
                , a~zwels
                , a~zterm
                , a~netwr
              FROM zsdt016 AS a
              INNER JOIN t052 AS b
              ON ( a~zterm = b~zterm )
              INTO @DATA(ls_t052).
          ENDIF.

*         APPC - SD.455716 - Ericky Fernandes - 17/11/2023 - Início
          "Buscando o valor do FECOP
          lv_vlrfecop = REDUCE j_1btaxval( INIT x = '0.00' FOR <lfs_nfstx_fecop> IN it_nfstx
                         WHERE ( taxtyp = 'ICFP' ) NEXT x = x + <lfs_nfstx_fecop>-taxval ).
*         APPC - SD.455716 - Ericky Fernandes - 17/11/2023 - Fim

          "Com a condição de pagamento seleciona os dias
          SELECT t052~zterm,
                 t052~ztag1
            FROM t052s
            INNER JOIN t052
            ON ( t052~zterm = t052s~ratzt )
            WHERE t052s~zterm = @is_vbrk-zterm
            INTO TABLE @lt_dias.

          "Caso não encontre na T052S, ele vai buscar diretamente na t052
          IF sy-subrc IS NOT INITIAL AND is_vbrk-zlsch = 'A' OR is_vbrk-zlsch = 'D'.

            SELECT t052~zterm,
                   t052~ztag1
              FROM t052
              WHERE zterm = @is_vbrk-zterm
                AND ztag1 <> ''
              INTO TABLE @lt_dias.

          ENDIF.

          "Verifica se encontrou o prazo do pagamento
          IF lt_dias IS NOT INITIAL.
            SORT lt_dias ASCENDING BY ztag1 zterm.

            "Verifica se é atacado ou varejo
            IF is_vbrk-vtweg = '10'.

              "Verifica se haverá consumo de crédito
              SELECT SINGLE val_credit
                FROM zsdt027
                WHERE vbeln = @is_vbrk-belnr
                INTO @DATA(lv_dmbtr_c).

              "Verifica se haverá consumo de crédito
*            IF sy-subrc IS NOT INITIAL.
              IF sy-subrc = 0.
                es_header-vorig = lv_vlrtot + lv_vlrtotst + lv_vlrfecop - lv_dmbtr_c + lv_frete. "APPC - DAM - adição frete - 16/05/23 / APPC - SD.442166 - Hemilly Maria - 09/10/2023
              ELSE.
                es_header-vorig = lv_vlrtot + lv_vlrtotst + lv_vlrfecop + lv_frete. "APPC - DAM - adição frete - 16/05/23 / APPC - SD.442166 - Hemilly Maria - 09/10/2023
              ENDIF.

              "Preenchendo o valor do ST ao valor total da nota
              es_header-nfat  = is_vbrk-vbeln.
              es_header-vdesc = lv_vlrdes.
              es_header-vliq  = ( es_header-vorig - es_header-vdesc ).

              "Verifica se existe valor de ST
              IF <fs_vlr_st> IS ASSIGNED.
                APPEND INITIAL LINE TO et_tradenotes ASSIGNING FIELD-SYMBOL(<fs_tradenotes_st>).

                lv_zbd1t = ls_t052-ztag1.

                CALL FUNCTION 'NET_DUE_DATE_GET'
                  EXPORTING
                    i_zfbdt = is_vbrk-fkdat
                    i_zbd1t = lv_zbd1t
                    i_zbd2t = ''
                    i_zbd3t = ''
                    i_shkzg = ''
                    i_rebzg = ''
                    i_koart = 'D'
                  IMPORTING
                    e_faedt = lv_dtvenc.
*            Preenchendo data de vencimento
                <fs_tradenotes_st>-vdup  = lv_vlrtotst.
                <fs_tradenotes_st>-dvenc = lv_dtvenc.

                FREE lv_dtvenc.
                FREE lv_zbd1t.
              ENDIF.

              "Adicionando as demais linhas ao tradenotes
              LOOP AT lt_dias ASSIGNING FIELD-SYMBOL(<fs_dias>).
                APPEND INITIAL LINE TO et_tradenotes.
                lv_parcelas = lv_parcelas + 1.
              ENDLOOP.

              "Calculando o valor das parcelas
              lv_vlrliq = lv_vlrtot - lv_vlrdes - lv_dmbtr_c + lv_frete. "APPC - DAM - adição frete - 16/05/23 / APPC - SD.442166 - Hemilly Maria - 09/10/2023
              lv_vlrpar = lv_vlrliq / lv_parcelas.

              "Preenchendo tradenotes com as demais informações
              LOOP AT et_tradenotes ASSIGNING FIELD-SYMBOL(<fs_tradenotes>) WHERE dvenc IS INITIAL.
                ADD 1 TO  lv_count.
                READ TABLE lt_dias ASSIGNING FIELD-SYMBOL(<fs_days>) INDEX lv_count.

                "Passa o valor da diferença para a ultima parcela
                IF lv_count = lines( lt_dias ).
                  lv_vlrpar = lv_vlrliq - lv_vlrtodif.
                ENDIF.

                lv_zbd1t = <fs_days>-zterm.

                CALL FUNCTION 'NET_DUE_DATE_GET'
                  EXPORTING
                    i_zfbdt = is_vbrk-fkdat
                    i_zbd1t = lv_zbd1t
                    i_zbd2t = ''
                    i_zbd3t = ''
                    i_shkzg = ''
                    i_rebzg = ''
                    i_koart = 'D'
                  IMPORTING
                    e_faedt = lv_dtvenc.

                <fs_tradenotes>-dvenc = lv_dtvenc.
                <fs_tradenotes>-vdup  = lv_vlrpar.
                lv_vlrtodif = lv_vlrtodif + <fs_tradenotes>-vdup.

              ENDLOOP.

              SORT et_tradenotes ASCENDING BY dvenc.

              FREE lv_numline.
              FREE lv_contador.

              "Adicionando o contador e o número da duplicata
              LOOP AT et_tradenotes ASSIGNING FIELD-SYMBOL(<fs_counter>).

                <fs_counter>-counter = lv_contador + 1.
                lv_numline           = lv_numline  + 1.

                <fs_counter>-ndup    = lv_numline.

                lv_contador          = lv_contador + 1.

              ENDLOOP.

              "Adicionando o valor do FECOP a primeira parcela
              IF lv_vlrfecop IS NOT INITIAL.

                READ TABLE et_tradenotes ASSIGNING FIELD-SYMBOL(<fs_frtpar>) INDEX 1.
                IF sy-subrc = 0.
                  <fs_frtpar>-vdup = <fs_frtpar>-vdup + lv_vlrfecop.
                ENDIF.

              ENDIF.

*            ENDIF.

            ELSE."Tratativa para preenchimento VAREJO

              "Preenchendo o valor do ST ao valor total da nota
              es_header-nfat = is_vbrk-vbeln.
              es_header-vorig = lv_vlrtot + lv_vlrtotst + lv_vlrfecop.
              es_header-vdesc = lv_vlrdes.
              es_header-vliq  = es_header-vorig - es_header-vdesc.

              "Adicionando as demais linhas ao tradenotes
              UNASSIGN <fs_dias>.
              LOOP AT lt_dias ASSIGNING <fs_dias>.
                APPEND INITIAL LINE TO et_tradenotes.
                lv_parcelas = lv_parcelas + 1.
              ENDLOOP.

              "Calculando o valor das parcelas
              lv_vlrliq = es_header-vorig - lv_vlrdes.
              lv_vlrpar = lv_vlrliq / lv_parcelas.

              "Preenchendo tradenotes com as demais informações
              UNASSIGN <fs_tradenotes>.
              LOOP AT et_tradenotes ASSIGNING <fs_tradenotes>.
                ADD 1 TO lv_count.

                UNASSIGN <fs_days>.
                READ TABLE lt_dias ASSIGNING <fs_days> INDEX lv_count.

                "Passa o valor da diferença para a ultima parcela
                IF lv_count = lines( lt_dias ).
                  lv_vlrpar = lv_vlrliq - lv_vlrtodif.
                ENDIF.

                lv_zbd1t = <fs_days>-zterm.

                CALL FUNCTION 'NET_DUE_DATE_GET'
                  EXPORTING
                    i_zfbdt = is_vbrk-fkdat
                    i_zbd1t = lv_zbd1t
                    i_zbd2t = ''
                    i_zbd3t = ''
                    i_shkzg = ''
                    i_rebzg = ''
                    i_koart = 'D'
                  IMPORTING
                    e_faedt = lv_dtvenc.

                <fs_tradenotes>-dvenc = lv_dtvenc.
                <fs_tradenotes>-vdup  = lv_vlrpar.
                lv_vlrtodif = lv_vlrtodif + <fs_tradenotes>-vdup.

              ENDLOOP.

              SORT et_tradenotes ASCENDING BY dvenc.

              FREE lv_numline.
              FREE lv_contador.

              "Adicionando o contador e o número da duplicata
              UNASSIGN <fs_counter>.
              LOOP AT et_tradenotes ASSIGNING <fs_counter>.

                <fs_counter>-counter = lv_contador + 1.
                lv_numline           = lv_numline  + 1.

                <fs_counter>-ndup    = lv_numline.

                lv_contador          = lv_contador + 1.

              ENDLOOP.

              "Adicionando o valor do FECOP a primeira parcela
              IF lv_vlrfecop IS NOT INITIAL.

                READ TABLE et_tradenotes ASSIGNING FIELD-SYMBOL(<fs_frtpar2>) INDEX 1.
                IF sy-subrc = 0.
                  <fs_frtpar2>-vdup = <fs_frtpar2>-vdup + lv_vlrfecop.
                ENDIF.

              ENDIF.

            ENDIF.
          ENDIF.
*      ENDIF.
        ENDIF.
      ENDIF.
* APPC - RMS - FIM - 29/03/2022

      "Insere nas informações complementares os textos de impostos
      IF NOT lt_zsdt012[] IS INITIAL.

        LOOP AT lt_zsdt012 INTO ls_zsdt012.

          CLEAR lv_value.
          lt_nfstx[] = it_nfstx[].

          "Tratamento para calculo de cada imposto
          CASE ls_zsdt012-cod_imposto.

            WHEN 'IPI_OUTRAS'.
              lv_text = 'Valor IPI:'.
              LOOP AT it_nflin INTO ls_nflin.
                lv_value = lv_value + ls_nflin-nfoth.
              ENDLOOP.

            WHEN 'BS_ICMS_ST'.
              lv_text = 'Base ICMS ST:'.
              DELETE lt_nfstx WHERE taxtyp <> 'ICS3'.
              LOOP AT lt_nfstx INTO ls_nfstx.
                lv_value = lv_value + ls_nfstx-base.
              ENDLOOP.

            WHEN 'VL_ICMS_ST'.
              lv_text = 'Valor ICMS ST:'.
              DELETE lt_nfstx WHERE taxtyp <> 'ICS3'.
              LOOP AT lt_nfstx INTO ls_nfstx.
                lv_value = lv_value + ls_nfstx-taxval.
              ENDLOOP.

            WHEN 'ICMS_ORI'.
              lv_text = 'Valor ICMS ORI:'.
              DELETE lt_nfstx WHERE taxtyp <> 'ICEP'.
              LOOP AT lt_nfstx INTO ls_nfstx.
                lv_value = lv_value + ls_nfstx-taxval.
              ENDLOOP.

            WHEN 'ICMS_DES'.
              lv_text = 'Valor ICMS DES:'.
              DELETE lt_nfstx WHERE taxtyp <> 'ICAP'.
              LOOP AT lt_nfstx INTO ls_nfstx.
                lv_value = lv_value + ls_nfstx-taxval.
              ENDLOOP.

            WHEN 'ICMS_FCP'.
              lv_text = 'Valor ICMS FCP:'.
              DELETE lt_nfstx WHERE taxtyp <> 'ICSP'.
              LOOP AT lt_nfstx INTO ls_nfstx.
                lv_value = lv_value + ls_nfstx-taxval.
              ENDLOOP.

            WHEN 'VL_ICMS'.
              lv_text = 'Valor ICMS:'.
              DELETE lt_nfstx WHERE taxtyp <> 'ICM3'.
              LOOP AT lt_nfstx INTO ls_nfstx.
                lv_value = lv_value + ls_nfstx-taxval.
              ENDLOOP.

            WHEN 'IPI_DEV_G'.
              lv_text = 'Valor IPI:'.
              lt_komvx[] = lt_komv[].
              DELETE lt_komvx WHERE kschl <> 'BX23'.
              LOOP AT lt_komvx INTO ls_komv.
                lv_value = lv_value + ls_komv-kwert.
              ENDLOOP.

            WHEN 'ICMS_DEV_G'.
              lv_text = 'Valor ICMS ST:'.
              lt_komvx[] = lt_komv[].
              DELETE lt_komvx WHERE kschl <> 'BX41'.
              LOOP AT lt_komvx INTO ls_komv.
                lv_value = lv_value + ls_komv-kwert.
              ENDLOOP.

            WHEN 'ICMSST_FCP'.
              lv_text = 'Valor FCP-ST:'.
              DELETE lt_nfstx WHERE taxtyp <> 'ICFP'.
              LOOP AT lt_nfstx INTO ls_nfstx.
                lv_value = lv_value + ls_nfstx-taxval.
              ENDLOOP.

          ENDCASE.

          "Adicionar na tag infcpl
          IF NOT lv_value IS INITIAL.
            CLEAR: lv_vlr.
            WRITE lv_value TO lv_vlr CURRENCY 'BRL'. CONDENSE lv_vlr.
            es_header-infcpl = es_header-infcpl &&
                               ' | ' &&
                               lv_text &&
                               lv_vlr.
          ENDIF.

        ENDLOOP.

*** GFX - DFLC - 11/11/2020 - F-PACI153 - início
        IF line_exists( lt_zsdt012[ cod_imposto = 'VR_BRT_VDA' ] ).

          CLEAR lv_value.

          lv_text = 'Vr Vda Bruta:'.
          lt_komvx[] = lt_komv[].
          DELETE lt_komvx WHERE kschl <> 'ZPVE'.
          LOOP AT lt_komvx INTO ls_komv.
            lv_value = lv_value + ls_komv-kwert.
          ENDLOOP.

          CLEAR: lv_vlr.

          WRITE lv_value TO lv_vlr CURRENCY 'BRL'. CONDENSE lv_vlr.

          es_header-infcpl = es_header-infcpl &&
                             ' | ' &&
                             |Vr Vda Bruta: { lv_vlr }|.

        ENDIF.

        IF line_exists( lt_zsdt012[ cod_imposto = 'VR_DES_VDA' ] ).

          CLEAR lv_value.

          lt_komvx[] = lt_komv[].
          DELETE lt_komvx WHERE kschl <> 'ZDVE'.
          IF lt_komvx IS INITIAL.
            lv_value = 0.
          ELSE.
            LOOP AT lt_komvx INTO ls_komv.
              lv_value = lv_value + ls_komv-kwert.
            ENDLOOP.
          ENDIF.

          CLEAR: lv_vlr.
          lv_value = lv_value * -1.
          WRITE lv_value TO lv_vlr CURRENCY 'BRL'. CONDENSE lv_vlr.

          es_header-infcpl = es_header-infcpl &&
                             ' | ' &&
                             |Vr Desc: { lv_vlr }|.

        ENDIF.

        IF line_exists( lt_zsdt012[ cod_imposto = '%_DES_VDA' ] ).

          CLEAR lv_value.

          "Primeiro calculo
          lt_komvx[] = lt_komv[].
          DELETE lt_komvx WHERE kschl <> 'ZPVE'.
          LOOP AT lt_komvx INTO ls_komv.
            lv_value = lv_value + ls_komv-kwert.
          ENDLOOP.

          DATA(lv_value2) = lv_value.
          CLEAR lv_value2.

          "Segundo calculo
          lt_komvx[] = lt_komv[].
          DELETE lt_komvx WHERE kschl <> 'ZDVE'.
          IF lt_komvx IS INITIAL.
            lv_value2 = 0.
          ELSE.
            LOOP AT lt_komvx INTO ls_komv.
              lv_value2 = lv_value2 + ls_komv-kwert.
            ENDLOOP.
          ENDIF.

          "Terceiro Calculo
          IF lv_value IS NOT INITIAL.

            lv_value = ( lv_value2 / lv_value ) * 100.

          ENDIF.

          lv_value = lv_value * -1.

          es_header-infcpl = es_header-infcpl &&
                             ' | ' &&
                             |% Desc: { lv_value }|.

          CLEAR lv_value.

        ENDIF.

*** GFX - DFLC - 11/11/2020 - F-PACI153 - Fim

      ENDIF.

*** GFX - JBRS - INICIO - DEVK916724 - 27/02/2020
*    ENDIF.
*** GFX - JBRS - FIM - DEVK916724 - 27/02/2020

    ENDIF.

*Payment
*** GFX - SFSS - SD - DEVK920580 - Início - 23/03/2021                                                                 "NT 2020.006
    "NT 2020.006
    FIELD-SYMBOLS: <fs_zfie004> TYPE zfie004.                                                                          "NT 2020.006
    "NT 2020.006
    es_header-indintermed = '0'.                                                                                       "NT 2020.006
    "NT 2020.006
    "Busca a tabela do caixa original                                                                                  "NT 2020.006
    ASSIGN ('(SAPMZFI002)GT_FORMPAG') TO <fs_t_formpag>.                                                               "NT 2020.006
    IF <fs_t_formpag> IS NOT ASSIGNED.                                                                                 "NT 2020.006
      "Busca a tabela do caixa X                                                                                       "NT 2020.006
      ASSIGN ('(SAPMZFI002X)GT_FORMPAG') TO <fs_t_formpag>.                                                            "NT 2020.006
    ENDIF.                                                                                                             "NT 2020.006
    "Busca linha selecionada para pagamento                                                                            "NT 2020.006
    ASSIGN ('(SAPMZFI002)GE_ZFIE004') TO <fs_zfie004>.                                                                 "NT 2020.006
    IF <fs_zfie004> IS NOT ASSIGNED.                                                                                   "NT 2020.006
      ASSIGN ('(SAPMZFI002X)GE_ZFIE004') TO <fs_zfie004>.                                                              "NT 2020.006
    ENDIF.                                                                                                             "NT 2020.006
*    NT 2020.006
    "Ajuste para tabela Payment - TEMPORÁRIO
    IF et_payment[] IS INITIAL.
      UNASSIGN <pay>.
      APPEND INITIAL LINE TO et_payment ASSIGNING <pay>.
      IF <pay> IS ASSIGNED.
***** GFX - JECA - 11.09.2018 - Ajuste Venda e devolução*
        IF is_header-nftype EQ 'Z3' OR is_header-nftype EQ 'Z4' OR is_header-nftype EQ 'Z5' OR is_header-nftype EQ 'Z7' OR
           is_header-nftype EQ 'ZG' OR is_header-nftype EQ 'YB' OR is_header-nftype EQ 'E4' OR is_header-nftype EQ 'R4' OR is_header-nftype EQ 'ZT'.

          <pay>-t_pag = '90'.
          <pay>-v_pag = ''.

        ELSE.
          <pay>-t_pag      = '15'.
          <pay>-v_pag      = is_header-nftot.
        ENDIF.
*       APPC - SD.537330 - Erisom Almeida - Inicio - 08/05/2024
        "Busca o tipo de integração dacastrado na tabela e inseri na tp_integra
        SELECT SINGLE tp_integra
                      FROM zsdt097
                      WHERE regio = @is_header-regio
                      AND t_pag = @<pay>-t_pag  INTO @<pay>-tp_integra.
*       APPC - SD.537330 - Erisom Almeida - Fim - 08/05/2024
*       Appc - SD.537330 - Erisom Almeida - Inicio - 25/04/2024
        IF <pay>-t_pag NE '03' OR <pay>-t_pag NE '04' OR <pay>-t_pag NE '10' OR <pay>-t_pag NE '11' OR <pay>-t_pag NE '12' OR <pay>-t_pag NE '13' OR
           <pay>-t_pag NE '15' OR <pay>-t_pag NE '17' OR <pay>-t_pag NE '18'.

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
*  APPC - SD.537330 - Erisom Almeida - Fim - 06/05/2024
        ENDIF.
      ENDIF.
    ENDIF.

    "se não encontrado nada na STVARV, então seguir essa nova solução desenhada abaixo                                                           "NT 2020.006
    IF <fs_t_formpag> IS ASSIGNED AND <fs_zfie004> IS ASSIGNED.                                                                                "NT 2020.006
      "NT 2020.006
      "Seleciona o código da bandeira                                                                                                              "NT 2020.006
      SELECT bandeira                                                                                                                              "NT 2020.006
            ,cod_bandeira_sitef                                                                                                                    "NT 2020.006
        FROM zfit012                                                                                                                               "NT 2020.006
        INTO TABLE @DATA(lt_bandeira).                                                                                                             "NT 2020.006
      "NT 2020.006
      IF lines( <fs_t_formpag> ) > 0.                                                                                                              "NT 2020.006
        "NT 2020.006
        "Percorre os itens encontrados                                                                                                             "NT 2020.006
        LOOP AT <fs_t_formpag> ASSIGNING FIELD-SYMBOL(<fs_pgto>).                                                                                  "NT 2020.006
          UNASSIGN <pay>.                                                                                                                          "NT 2020.006
          APPEND INITIAL LINE TO et_payment ASSIGNING <pay>.                                                                                       "NT 2020.006
          "NT 2020.006
          ADD 1 TO lv_counter.                                                                                                                     "NT 2020.006
          <pay>-counter = lv_counter.                                                                                                              "NT 2020.006
          <pay>-v_pag = <fs_pgto>-dmbtr.                                                                                                           "NT 2020.006
          "NT 2020.006
          IF <fs_pgto>-apr_pos IS NOT INITIAL.                                                                                                     "NT 2020.006
            <pay>-c_aut = <fs_pgto>-apr_pos.                                                                                                       "NT 2020.006
          ELSEIF <fs_pgto>-cod_autorizacao IS NOT INITIAL OR <fs_pgto>-cod_autorizacao NE 0.                                                       "NT 2020.006
            <pay>-c_aut = <fs_pgto>-cod_autorizacao.                                                                                               "NT 2020.006
          ENDIF.                                                                                                                                   "NT 2020.006
          "NT 2020.006
          <pay>-tp_integra = COND #( WHEN <fs_pgto>-tp_pagto_cartao = 'TEF' THEN '1'                                                               "NT 2020.006
                                     ELSE '2' ). "POS ou outros                                                                                    "NT 2020.006
          "NT 2020.006
          IF ( <fs_pgto>-adquirente IS NOT INITIAL OR <fs_pgto>-cod_adq IS NOT INITIAL ) AND                                                                                               "NT 2020.006
            ( <fs_pgto>-dcr_meio_pagamento = 'CARTÃO DE CRÉDITO' OR <fs_pgto>-dcr_meio_pagamento = 'CARTÃO DE DÉBITO' ).                           "NT 2020.006
            "NT 2020.006
            lv_codadq = COND #( WHEN <fs_pgto>-adquirente IS NOT INITIAL
                                 THEN <fs_pgto>-adquirente
                                 ELSE <fs_pgto>-cod_adq ).                                                                                         "NT 2020.006
            "NT 2020.006
            "Busca o CNPJ da adquirente                                                                                                            "NT 2020.006
            SELECT SINGLE cnpj                                                                                                                     "NT 2020.006
              FROM zmfetc003                                                                                                                       "NT 2020.006
              INTO lv_stcd1                                                                                                                        "NT 2020.006
              WHERE cod_adq = lv_codadq.                                                                                                           "NT 2020.006
            "NT 2020.006
            <pay>-cnpj = lv_stcd1.                                                                                                                 "NT 2020.006
            "NT 2020.006
            FREE: lv_stcd1, lv_codadq.                                                                                                             "NT 2020.006
          ENDIF.                                                                                                                                   "NT 2020.006

          "NT 2020.006
          "DE/PARA - Meios de pagamento do caixa Z
          SELECT SINGLE pag_sap
            FROM zsdt059
            INTO <pay>-t_pag
            WHERE pag_caixa = <fs_pgto>-dcr_meio_pagamento.

          "NT 2020.006
          READ TABLE lt_bandeira ASSIGNING FIELD-SYMBOL(<fs_bandeira>) WITH KEY bandeira = <fs_pgto>-cartao.                                       "NT 2020.006
          IF sy-subrc      IS INITIAL AND
             <fs_bandeira> IS ASSIGNED.                                                                                    "NT 2020.006
*            SHIFT <fs_bandeira>-cod_bandeira_sitef LEFT DELETING LEADING '0'.
*            <pay>-t_band = <fs_bandeira>-cod_bandeira_sitef+3(2).                                                                                       "NT 2020.006

            SELECT SINGLE tband
              FROM zsdt057
              INTO <pay>-t_band
              WHERE bandeira = <fs_bandeira>-bandeira.

          ELSE.                                                                                                                                    "NT 2020.006
            <pay>-t_band = '99'.                                                                                                                   "NT 2020.006
          ENDIF.

          IF <fs_pgto>-dcr_meio_pagamento <> 'CARTÃO DE CRÉDITO' AND
             <fs_pgto>-dcr_meio_pagamento <> 'CARTÃO DE DÉBITO'.
            <pay>-t_band = ''.
          ENDIF.
          "NT 2020.006
          "NT 2020.006
        ENDLOOP.                                                                                                                                   "NT 2020.006
        "NT 2020.006
      ELSE.                                                                                                                                        "NT 2020.006
        "NT 2020.006
        UNASSIGN <pay>.                                                                                                                            "NT 2020.006
        APPEND INITIAL LINE TO et_payment ASSIGNING <pay>.                                                                                         "NT 2020.006
        "NT 2020.006
        ADD 1 TO lv_counter.                                                                                                                       "NT 2020.006
        <pay>-counter    = lv_counter.                                                                                                             "NT 2020.006
        <pay>-v_pag      = <fs_zfie004>-netwr.                                                                                                     "NT 2020.006
        <pay>-tp_integra = '2'. "POS ou outros

        "NT 2020.006
        "DE/PARA - Meios de pagamento standard
        SELECT SINGLE pag_nota
          FROM zsdt060
          INTO <pay>-t_pag
          WHERE zlsch = <fs_zfie004>-zlsch.

        "NT 2020.006
      ENDIF.                                                                                                                                       "NT 2020.006
      "NT 2020.006
    ELSE.                                                                                                                                          "NT 2020.006
      "NT 2020.006
      IF is_header-nftype EQ 'Z3' OR is_header-nftype EQ 'Z4' OR is_header-nftype EQ 'Z5' OR is_header-nftype EQ 'Z7' OR                           "NT 2020.006
         is_header-nftype EQ 'ZG' OR is_header-nftype EQ 'YB' OR is_header-nftype EQ 'E4' OR is_header-nftype EQ 'R4' OR is_header-nftype EQ 'ZT'. "NT 2020.006
        "NT 2020.006
        UNASSIGN <pay>.                                                                                                                            "NT 2020.006
        ADD 1 TO lv_counter.

        READ TABLE et_payment TRANSPORTING NO FIELDS WITH KEY counter = lv_counter.
        IF sy-subrc <> 0.

          APPEND INITIAL LINE TO et_payment ASSIGNING <pay>.                                                                                         "NT 2020.006
          <pay>-counter = lv_counter.                                                                                                                "NT 2020.006
          <pay>-t_pag   = '90'.                                                                                                                      "NT 2020.006
          <pay>-v_pag   = ''.                                                                                                                        "NT 2020.006

        ENDIF.

      ELSE.

        CHECK ls_vbak-vbeln IS NOT INITIAL.

        "Verifica se a nota emitida é do SysPDV
        SELECT frpag_syspdv "Forma de pagamento SysPDV
              ,codband_pdv  "Cod. Bandeira SysPDV
              ,codaut       "Cod. de autorização
              ,nsuhost      "Nsu Host
              ,codadq       "Cod. Adquirente
              ,netwr        "Valor liquido
          FROM zsdt039
          INTO TABLE @DATA(lt_zsdt039)
          WHERE ordem = @ls_vbak-vbeln.

        IF sy-subrc = 0.
          FREE: lv_counter.
          LOOP AT lt_zsdt039 ASSIGNING FIELD-SYMBOL(<fs_zsdt039>).

            "Seleciona o CNPJ
            SELECT SINGLE credenc
              FROM zsdt041
              INTO @DATA(lv_credenc)
              WHERE codadq = @<fs_zsdt039>-codadq.

            "Seleciona a forma de pagamento SAP
            SELECT SINGLE pag_sap
              FROM zsdt062
              INTO @DATA(lv_pag_sap)
              WHERE pag_pdv = @<fs_zsdt039>-frpag_syspdv.

            "Cartão de crédito ou Débito
            IF lv_pag_sap = '03' OR
               lv_pag_sap = '04'.

              "Seleciona a bandeira do cartão no SAP
              SELECT SINGLE band_sap
                FROM zsdt063
                INTO @DATA(lv_band_sap)
                WHERE band_pdv = @<fs_zsdt039>-codband_pdv.

            ENDIF.

            APPEND INITIAL LINE TO et_payment ASSIGNING <pay>.
            ADD 1 TO lv_counter.                                                                                                                       "NT 2020.006
            <pay>-counter = lv_counter.
            <pay>-tp_integra = COND #( WHEN <fs_zsdt039>-frpag_syspdv = '3' OR
                                            <fs_zsdt039>-frpag_syspdv = '4'
                                         THEN '1'
                                         ELSE '2' ).

            <pay>-v_pag      = COND #( WHEN <pay>-t_pag <> '90'
                                         THEN <fs_zsdt039>-netwr ).

            <pay>-t_band     = COND #( WHEN lv_pag_sap = '03' OR
                                            lv_pag_sap = '04'
                                         THEN lv_band_sap
                                         ELSE '' ).

            <pay>-cnpj       = lv_credenc.
            <pay>-c_aut      = <fs_zsdt039>-codaut.
            <pay>-t_pag      = lv_pag_sap.
            <pay>-c_aut      = <fs_zsdt039>-codaut.

            FREE: lv_credenc,
                  lv_pag_sap.

          ENDLOOP.

        ELSEIF is_vbrk IS NOT INITIAL.

          UNASSIGN <pay>.
          APPEND INITIAL LINE TO et_payment ASSIGNING <pay>.
          <pay>-cnpj       = is_header-cnpj_bupla.                        "Armazena o CNPJ
          "DE/PARA - Meios de pagamento standard
          SELECT SINGLE pag_nota
            FROM zsdt060
            INTO <pay>-t_pag                                              "Armazena o tipo de pagamento
            WHERE zlsch = is_vbrk-zlsch.

          <pay>-v_pag      = COND #( WHEN <pay>-t_pag <> '90'             "Armazena o valor pago
                                      THEN is_header-nftot ).

*         appc - sd.537330 - erisom almeida - Inicio - 10/05/2024
          "Busca o tipo de integração cadastrado na tabela
          SELECT SINGLE tp_integra
                        FROM zsdt097
                        WHERE regio = @is_header-regio
                        AND t_pag = @<pay>-t_pag  INTO @<pay>-tp_integra. "Armazena o tipo de integração
*         APPC - SD.537330 - Erisom Almeida - Fim - 10/05/2024

        ENDIF.

      ENDIF.

    ENDIF.                                                                                                                                         "NT 2020.006
    "NT 2020.006

**** GFX - SFSS - SD - DEVK920580 - Fim - 23/03/2021                                                                                                "NT 2020.006

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

      es_header-indintermed = '0'.                                                                                       "NT 2020.006

    ENDIF.

    IF et_payment IS NOT INITIAL.

      "Soma os valores do pagamento
      LOOP AT et_payment ASSIGNING <pay>.

        lv_val_pag = lv_val_pag + <pay>-v_pag.

      ENDLOOP.

      "Calcula troco
      es_header-v_troco = COND #( WHEN lv_val_pag > is_header-nftot
                                   THEN ( lv_val_pag - is_header-nftot ) ).

    ENDIF.
*    IF is_header-model = '55' OR is_header-model = '59' OR is_header-model = '65'.
*
*
*    ENDIF.

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

*** GFX - JBRS - INICIO - 19/11/2019
    "Variáveis
    DATA: lv_ebeln      TYPE mseg-ebeln,
          lv_vgbel      TYPE vbrp-vgbel,
          lv_kwert      TYPE prcd_elements-kwert,
          lv_name1      TYPE name1_gp,
          lv_refkey     TYPE j_1bnflin-refkey,
          lv_obs_manual TYPE string,
          lv_texto      TYPE string,
          lv_name       TYPE thead-tdname,
          lv_rem        TYPE mblnr.

    "Tabelas
    DATA: lt_tline TYPE TABLE OF tline.

    "Objetos
    DATA: lo_monta_obs_xml TYPE REF TO zcl_monta_obs_xml.

    "Field Symbols
    FIELD-SYMBOLS: <fs_v_xblnr_bkpf> TYPE bkpf-xblnr.

    READ TABLE it_nflin INTO DATA(ls_bnflin) INDEX 1.
    IF sy-subrc = 0.

      LOOP AT lt_komv ASSIGNING FIELD-SYMBOL(<fs_komv>) WHERE kschl = 'BX4A'
                                                        AND   kappl = 'V'.
        lv_kwert = lv_kwert + <fs_komv>-kwert.
      ENDLOOP.

      "Tipo de documento
      CASE ls_bnflin-reftyp.
        WHEN 'BI'. "Documento de faturamento
          "Remessa
          READ TABLE it_vbrp INTO DATA(ls_vbrps) INDEX 1.
          IF sy-subrc = 0.
            lv_vgbel = ls_vbrps-vgbel.
          ENDIF.

          "Vendendor
          READ TABLE it_partner INTO DATA(ls_partner) WITH KEY parvw = 'ZV'.
          IF sy-subrc = 0.
            lv_name1 = ls_partner-name1.
          ENDIF.

          lv_refkey = ls_bnflin-refkey.

          CREATE OBJECT lo_monta_obs_xml
            EXPORTING
              iv_refkey = lv_refkey        "  Referência ao documento de origem
              iv_reftyp = ls_bnflin-reftyp " Tipo referência
              iv_name1  = lv_name1         " Vendedor
              iv_vgbel  = lv_vgbel         " Nº documento do documento de referência
              iv_ebeln  = lv_ebeln         " Nº do documento de compras
              iv_kwert  = lv_kwert         " Valor da condição
              iv_taxlw1 = ls_bnflin-taxlw1
              iv_taxlw2 = ls_bnflin-taxlw2.

          "Inicialização
          lo_monta_obs_xml->init(
          EXPORTING
            iv_parid = |{ is_header-parid ALPHA = IN }|
            iv_regio = CONV #( is_header-regio )
            CHANGING
              cv_msg_completo = es_header-infcpl ).

        WHEN OTHERS.

          "Seleciona o Pedido de compra
          READ TABLE it_mseg INTO DATA(ls_mseg) INDEX 1.
          IF sy-subrc = 0.
            lv_ebeln = ls_mseg-ebeln.
          ENDIF.

          "Seleciona o Documento de material
          SELECT COUNT(*)
              FROM ekbe AS a
              INNER JOIN ekko AS b
                ON ( b~ebeln = a~ebeln )
              WHERE a~ebeln = @lv_ebeln
              AND   a~vgabe = '8'
              AND   b~bsart IN ( 'UB', 'ZUB', 'ZUB1', 'ZUB2' ).

          IF sy-subrc = 0.

            TRY .
                ASSIGN ('(SAPLJ1BF)WA_XMSEG-XBLNR_MKPF') TO <fs_v_xblnr_bkpf>.
                IF <fs_v_xblnr_bkpf> IS ASSIGNED.
                  lv_rem = CONV #( <fs_v_xblnr_bkpf> ).
                ENDIF.
              CATCH cx_root.
            ENDTRY.

            CREATE OBJECT lo_monta_obs_xml
              EXPORTING
                iv_refkey = lv_refkey
                iv_reftyp = 'MD'
                iv_name1  = lv_name1
                iv_vgbel  = lv_vgbel
                iv_ebeln  = lv_ebeln
                iv_kwert  = lv_kwert
                iv_taxlw1 = ls_bnflin-taxlw1
                iv_taxlw2 = ls_bnflin-taxlw2
                iv_rem    = lv_rem.

            lo_monta_obs_xml->init(
              EXPORTING
                it_fftx_tab = it_nfftx
                iv_parid = |{ is_header-parid ALPHA = IN }|
                iv_regio = CONV #( is_header-regio )
              CHANGING
                cv_msg_completo = es_header-infcpl ).

          ENDIF.

      ENDCASE.

    ENDIF.
*** GFX - JBRS - FIM - 19/11/2019

*** GFX - JBRS - INICIO - 18/12/2019

    "Inserção do Volume no XML
    READ TABLE et_transvol ASSIGNING FIELD-SYMBOL(<fs_transvol>) INDEX 1.
    IF sy-subrc = 0.

      "Atribui o volume
      <fs_transvol>-qvol = is_header-anzpk.

    ELSE.

      "Insere registro
      APPEND INITIAL LINE TO et_transvol ASSIGNING FIELD-SYMBOL(<fs_ap_transvol>).
      <fs_ap_transvol>-qvol = is_header-anzpk.

    ENDIF.

*** GFX - JBRS - FIM - 18/12/2019

*** GFX - JBRS - INICIO - 09/01/2020 - Concatenação do campo Descrição do material com referência do material.

    FIELD-SYMBOLS: <fs_wnfstx>     LIKE LINE OF it_nfstx,
                   <fs_tab_wnfstx> TYPE j_1bnfstx_tab.

    IF it_nflin IS NOT INITIAL.

      "Seleciona a Referência do produto
      SELECT matnr
            ,mfrpn
        FROM mara
        INTO TABLE @DATA(lt_mara)
        FOR ALL ENTRIES IN @it_nflin
        WHERE matnr = @it_nflin-matnr.

      IF sy-subrc = 0.

        LOOP AT et_item ASSIGNING FIELD-SYMBOL(<fs_item>).

          FREE ls_nflin.
          "Seleciona informações do item
          READ TABLE it_nflin INTO ls_nflin WITH KEY itmnum = <fs_item>-itmnum.
          IF sy-subrc = 0.

            ASSIGN lt_mara[ matnr = ls_nflin-matnr ] TO FIELD-SYMBOL(<fs_mara>).
            IF sy-subrc = 0.
              CONCATENATE ls_nflin-maktx '-' <fs_mara>-mfrpn INTO <fs_item>-xprod SEPARATED BY space.
            ENDIF.

*** GFX - JBRS - DEVK922552 - Início - 17/08/2021

            "Tipo de documento
*            IF ls_nflin-reftyp = 'BI'.
*
*              TRY.
*                  "Ordem de venda
*                  ASSIGN ('(SAPLJ1BG)RVBAK-VBELN') TO FIELD-SYMBOL(<fs_vbeln>).
*                CATCH cx_root INTO DATA(lcx_root).
*              ENDTRY.
*
*              "Remessa
*              READ TABLE it_vbrp INTO DATA(ls_vbrpx) INDEX 1.
*              IF sy-subrc = 0.
*                lv_vgbel = ls_vbrpx-vgbel.
*              ENDIF.
*
*              "Fatura
*              DATA(lv_fatura) = ls_nflin-refkey.
*
*              IF <fs_vbeln> IS ASSIGNED.
*
*                lv_docs = <fs_vbeln>.
*
*              ENDIF.
*
*              lv_docs = COND #( WHEN lv_docs IS NOT INITIAL
*                                  THEN lv_docs && '/' && lv_vgbel && '/' && lv_fatura
*                                  ELSE lv_vgbel && '/' && lv_fatura ).
*
*              CONCATENATE <fs_item>-xprod
*                          lv_docs
*                          INTO <fs_item>-xprod SEPARATED BY space.
*
*            ENDIF.
*
*            FREE: lv_docs,
*                  lv_vgbel.

*** GFX - JBRS - DEVK922552 - Fim - 17/08/2021

          ENDIF.
*** APPC - SD.453446 - Hemilly Maria - Inicio - 11/12/2023
          IF line_exists( it_nfstx[ taxtyp = 'ICM3' ] ) AND lv_prodepe IS NOT INITIAL.
            ASSIGN ('(SAPLJ1BG)WNFSTX[]') TO <fs_tab_wnfstx>.
            IF <fs_tab_wnfstx> IS ASSIGNED.
              READ TABLE <fs_tab_wnfstx>[] ASSIGNING <fs_wnfstx> WITH KEY taxtyp = 'ICM3'.
              IF sy-subrc = 0.
                <fs_wnfstx>-taxtyp = 'ZPM3'.
              ENDIF.
              <fs_item>-vbcefet   = it_nfstx[ taxtyp = 'ZPM3' itmnum = <fs_item>-itmnum ]-base.
              <fs_item>-picmsefet = it_nfstx[ taxtyp = 'ZPM3' itmnum = <fs_item>-itmnum ]-rate.
              <fs_item>-vicmsefet = it_nfstx[ taxtyp = 'ZPM3' itmnum = <fs_item>-itmnum ]-taxval.
            ENDIF.

          ENDIF.
*** APPC - SD.453446 - Hemilly Maria - Fim - 11/12/2023
        ENDLOOP.
      ENDIF.

    ENDIF.

    " < --- GFX - JEBS - F-PACI294 - INICIO - 10/03/2023 --- > "
    UNASSIGN <fs_tab_wnfstx>. "APPC

    ASSIGN ('(SAPLJ1BG)WNFSTX[]') TO <fs_tab_wnfstx>.

    IF <fs_tab_wnfstx> IS ASSIGNED AND lv_prodepe = 'X'.

      LOOP AT et_item ASSIGNING FIELD-SYMBOL(<fs_item_2>).

        " Faz a leitura na tabela pra encontrar a linha do ZMP3
        READ TABLE <fs_tab_wnfstx>[] ASSIGNING <fs_wnfstx> WITH KEY taxtyp = 'ZPM3'
                                                                    itmnum = <fs_item_2>-itmnum.

        " Se encontrar
        IF sy-subrc = 0.
***APPC - SD.453446 - Hemilly Maria - Inicio - 28/11/2023
          <fs_wnfstx>-stattx = 'X'.
***APPC - SD.453446 - Hemilly Maria - Fim - 28/11/2023
        ENDIF.

        UNASSIGN <fs_wnfstx>.

        " Faz a leitura na tabela pra encontrar a linha do ICS3
        READ TABLE <fs_tab_wnfstx>[] ASSIGNING <fs_wnfstx> WITH KEY taxtyp = 'ICS3'
                                                                    itmnum = <fs_item_2>-itmnum.

        " Se encontrar
        IF sy-subrc = 0.
          IF <fs_zfie004> IS NOT ASSIGNED.
            IF <pay>-v_pag IS NOT INITIAL OR lv_val_pag IS NOT INITIAL.
              <pay>-v_pag = <pay>-v_pag - <fs_wnfstx>-taxval.
              lv_val_pag = <pay>-v_pag.
            ENDIF.
          ENDIF.

***APPC - SD.453446 - Hemilly Maria - Inicio - 28/11/2023
          APPEND INITIAL LINE TO <fs_tab_wnfstx>[] ASSIGNING FIELD-SYMBOL(<fs_add_wnfstx>).
          MOVE-CORRESPONDING <fs_wnfstx> TO <fs_add_wnfstx>.
          <fs_add_wnfstx>-taxtyp = 'ZPST'.
          <fs_add_wnfstx>-taxval = <fs_add_wnfstx>-taxval * -1.
          <fs_add_wnfstx>-stattx = 'X'.
          <fs_wnfstx>-stattx     = 'X'.
***APPC - SD.453446 - Hemilly Maria - Fim - 28/11/2023
        ENDIF.

        UNASSIGN <fs_wnfstx>.

      ENDLOOP.

    ENDIF.

    " < --- GFX - JEBS - F-PACI294 - FIM - 10/03/2023 --- > "

*** GFX - JBRS - FIM - 09/01/2020 - Concatenação do campo Descrição do material com referência do material.

*** GFX - JBRS - INICIO - DEVK916724 - 21/02/2020 - Ajuste nos dados de pagamento do cupom
    "Modelos de cupom fiscal
*    IF is_header-model = '59'.
*
*      TRY.
*          "Seleciona dados do cupom fiscal que está na fila que veio pelo o caixa
*          ASSIGN ('(SAPMZFI002)GT_FORMPAG') TO <fs_t_formpag>.
*          IF <fs_t_formpag> IS ASSIGNED.
*
*            CALL FUNCTION 'ZFM_SD_DADOS_PGTO_CUPOM'
*              EXPORTING
*                it_formpag = <fs_t_formpag>
*                is_header  = is_header
*              TABLES
*                t_payment  = et_payment.
*
*          ENDIF.
*
*        CATCH cx_root.
*      ENDTRY.
*
*    ENDIF.
*** GFX - JBRS - FIM - DEVK916724 - 21/02/2020 - Ajuste nos dados de pagamento do cupom

  ENDMETHOD.
