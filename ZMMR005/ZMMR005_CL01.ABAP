*&---------------------------------------------------------------------*
*& Include          ZMMR004CL01
*&---------------------------------------------------------------------*

CLASS lcl_report IMPLEMENTATION.

  METHOD botao.

    cria_botao( ).

  ENDMETHOD.

  METHOD cria_botao.

*  Estrutura para descrever o botão
    DATA: wa_button TYPE smp_dyntxt.

*  Nome do Botão
    wa_button-text      = 'Ver Relatório'.
*  Ícone do Botão
    wa_button-icon_id   = icon_execute_object.
*  Texto que aparecerá ao lado do ícone (pode ser vazio)
    wa_button-icon_text = 'Ver Relatório'.
*  Quickinfo (aparece quando o user passar o mouse sobre o botao)
    wa_button-quickinfo = 'Ver Relatório'.
*  Associa essas propriedades com a função 1
    sscrfields-functxt_01 = wa_button.

*  Nome do Botão
    wa_button-text      = 'Imprimir'.
*  Ícone do Botão
    wa_button-icon_id   = icon_print.
*  Texto que aparecerá ao lado do ícone (pode ser vazio)
    wa_button-icon_text = 'Imprimir'.
*  Quickinfo (aparece quando o user passar o mouse sobre o botao)
    wa_button-quickinfo = 'Imprimir'.
*  Associa essas propriedades com a função 1
    sscrfields-functxt_02 = wa_button.

    DATA: gt_exclude TYPE TABLE OF rsexfcode,
          ge_exclude TYPE rsexfcode.

    ge_exclude-fcode  = 'ONLI'.
    APPEND ge_exclude TO gt_exclude.

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = '%_00'
        p_program = 'RSSYSTDB'
      TABLES
        p_exclude = gt_exclude[].

  ENDMETHOD.

  METHOD inicializa.

    inicializa_campos( ).

  ENDMETHOD.


  METHOD start.

    verifica_campos( ).

    CHECK gv_erro IS INITIAL.

    seleciona_dados( ).

  ENDMETHOD.

  METHOD inicializa_campos.

    DATA: lv_dataux TYPE datum.

    LOOP AT SCREEN.
      IF screen-name = 'P_REFREL'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
*
*    CONCATENATE sy-datum+2(2) sy-datum+4(2) sy-datum+6(2) sy-uzeit(4)
*      INTO p_refrel.

*** GFX - JBRS - 19/02/2020 - INICIO
    DATA: lv_number  TYPE i.


*** GFX - DFLC - Início 04/03/2020

    CONCATENATE sy-datum+2(2) sy-datum+4(2) sy-datum+6(2) sy-uzeit(6)
      INTO p_refrel.

    gv_refrel = p_refrel.

*** GFX - DFLC- Fim 04/03/2020

*    DO.
*
*      CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
*        EXPORTING
*          object           = 'ZMM001'
*        EXCEPTIONS
*          foreign_lock     = 1
*          object_not_found = 2
*          system_failure   = 3
*          OTHERS           = 4.
*
*      IF sy-subrc <> 0.
*        CONTINUE.
*      ENDIF.
*
*      CALL FUNCTION 'NUMBER_GET_NEXT'
*        EXPORTING
*          nr_range_nr             = '01'
*          object                  = 'ZMM001'
*          quantity                = '1'
*        IMPORTING
*          number                  = lv_number
*        EXCEPTIONS
*          interval_not_found      = 1
*          number_range_not_intern = 2
*          object_not_found        = 3
*          quantity_is_0           = 4
*          quantity_is_not_1       = 5
*          internal_overflow       = 6
*          OTHERS                  = 7.
*
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
*
*      CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
*        EXPORTING
*          object = 'ZMM001'.
*
*      IF sy-subrc = 0.
*        p_refrel = lv_number.
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = p_refrel
*          IMPORTING
*            output = p_refrel.
*
*        gv_refrel = p_refrel.
*
*        EXIT.
*      ENDIF.
*
*    ENDDO.
*** GFX - JBRS - 19/02/2020 - FIM
    lv_dataux = sy-datum.
*    lv_dataux+6(2) = '01'.

    CALL FUNCTION 'OIUPR_DATE_SUBTRACT_MONTH'
      EXPORTING
        date       = lv_dataux
        months     = 5
      IMPORTING
        date_e     = s_rgvnd-low
      EXCEPTIONS
        date_error = 1
        OTHERS     = 2.

    s_rgvnd-sign   = ''.
    s_rgvnd-option = ''.
    s_rgvnd-low    = s_rgvnd-low.
    s_rgvnd-high   = lv_dataux.
    APPEND s_rgvnd TO s_rgvnd.

    CALL FUNCTION 'HR_HU_ADD_MONTH_TO_DATE'
      EXPORTING
        months  = 3
        olddate = lv_dataux
      IMPORTING
        newdate = s_necom-low.

    s_necom-sign   = ''.
    s_necom-option = ''.
    s_necom-low    = s_necom-low.
    APPEND s_necom TO s_necom.

  ENDMETHOD.
  METHOD verifica_campos.

    DATA: vl_lin        TYPE i,
          vl_no_day     TYPE i,
          vl_no_month   TYPE i,
          vl_no_year    TYPE i,
          vl_no_cal_day TYPE i,
          vl_parc1      TYPE i,
          vl_parc2      TYPE i,
          vl_e_days     TYPE vtbbewe-atage,
          vl_e_months   TYPE vtbbewe-atage,
          vl_e_years    TYPE vtbbewe-atage,
          vl_dias       TYPE utst_abap_types-dec_10,
          ls_t001w      TYPE t001w.

    CLEAR gv_erro.

    IF s_lifnr[] IS INITIAL.
      "Campo & obrigatório.
      MESSAGE s001(zmm01) WITH 'Fornecedor/Fabricante' DISPLAY LIKE 'E'.
      gv_erro = 'X'.
      EXIT.
    ENDIF.

    IF s_cd[] IS INITIAL.
      "Campo & obrigatório.
      MESSAGE s001(zmm01) WITH 'CDs para Análise' DISPLAY LIKE 'E'.
      gv_erro = 'X'.
      EXIT.
    ENDIF.

    IF s_cddet[] IS INITIAL.
      "Campo & obrigatório.
      MESSAGE s001(zmm01) WITH 'CD Determinante' DISPLAY LIKE 'E'.
      gv_erro = 'X'.
      EXIT.
    ENDIF.

*    IF s_lojas[] IS INITIAL.
*      "Campo & obrigatório.
*      MESSAGE s001(zmm01) WITH 'Lojas para Análise' DISPLAY LIKE 'E'.
*      gv_erro = 'X'.
*      EXIT.
*    ENDIF.

    IF s_lgort[] IS INITIAL.
      "Campo & obrigatório.
      MESSAGE s001(zmm01) WITH 'Depósito' DISPLAY LIKE 'E'.
      gv_erro = 'X'.
      EXIT.
    ENDIF.

    "Verifica se existe mais de 2 CD's
    DESCRIBE TABLE s_cd LINES vl_lin.

    IF vl_lin GT 3.
      "Selecione apenas & CD's!
      MESSAGE s011(zmm01) DISPLAY LIKE 'E' WITH '3'.
      gv_erro = 'X'.
      EXIT.
    ENDIF.

    IF sy-ucomm	EQ 'FC01'.
      SELECT SINGLE * INTO gs_zmmt007
        FROM zmmt007
        WHERE refrel EQ p_refrel.

      IF sy-subrc EQ 0.
        "Refêrencia & já cadastrada, aguarde 1 minuto e gere novamente
        MESSAGE s026(zmm01) WITH p_refrel DISPLAY LIKE 'E'.
        gv_erro = 'X'.
        EXIT.
      ENDIF.
    ENDIF.

*    LOOP AT s_cd INTO s_werks_line.
*      SELECT SINGLE * INTO ls_t001w
*        FROM t001w
*        WHERE werks    EQ s_werks_line-low
*          AND nodetype EQ 'DC'.
*
*      IF sy-subrc NE 0.
*        "Centro informado & não é um CD!
*        MESSAGE s023(zmm01) WITH s_werks_line-low DISPLAY LIKE 'E'.
*        gv_erro = 'X'.
*        EXIT.
*      ENDIF.
*    ENDLOOP.

    CHECK gv_erro IS INITIAL.

    LOOP AT s_lojas INTO s_werks_line.
      SELECT SINGLE * INTO ls_t001w
        FROM t001w
        WHERE werks    EQ s_werks_line-low
          AND nodetype EQ ' '.

      IF sy-subrc NE 0.
        "Centro informado & não é uma Loja!
        MESSAGE s024(zmm01) WITH s_werks_line-low DISPLAY LIKE 'E'.
        gv_erro = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.

    CHECK gv_erro IS INITIAL.

    "Verifica se a o intervalo de data é maior que 6 mese
    CALL FUNCTION 'HR_AUPBS_MONTH_DAY'
      EXPORTING
        beg_da     = s_rgvnd-low
        end_da     = s_rgvnd-high
      IMPORTING
        no_day     = vl_no_day
        no_month   = vl_no_month
        no_year    = vl_no_year
        no_cal_day = vl_no_cal_day.

    CLEAR gv_mm_media.
    IF vl_no_year  GT 0 OR
       vl_no_month GT 4.
      "Mês/Ano maior que o permitido!
      "Range maior que 6 meses, mude a data de início e/ou fim!
      MESSAGE s012(zmm01) DISPLAY LIKE 'E'.
      gv_erro = 'X'.
      EXIT.
    ELSE.
      gv_mm_media = vl_no_month + 1.
    ENDIF.

    "Verifica Se data é menor que atual
    IF s_necom-low LT sy-datum.
      "A data não pode ser menor que hoje!
      MESSAGE s013(zmm01) DISPLAY LIKE 'E'.
      gv_erro = 'X'.
      EXIT.
    ENDIF.

    CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
      EXPORTING
        i_date_from = sy-datum
        i_date_to   = s_necom-low
      IMPORTING
        e_days      = vl_e_days
        e_months    = vl_e_months
        e_years     = vl_e_years.

    vl_dias = vl_e_days / 30.

    gv_nc_media = trunc( vl_dias ).

*    CALL FUNCTION 'HR_AUPBS_MONTH_DAY'
*      EXPORTING
*        beg_da     = sy-datum
*        end_da     = s_necom-low
*      IMPORTING
*        no_day     = vl_no_day
*        no_month   = vl_no_month
*        no_year    = vl_no_year
*        no_cal_day = vl_no_cal_day.
*
*    CLEAR gv_nc_media.
*
*    vl_parc1 = ( vl_no_year * 12 ) + vl_no_month.
*    vl_parc2 = ( vl_no_day / 30 ).
*
*    gv_nc_media = vl_parc1 + vl_parc2.

  ENDMETHOD.

  METHOD seleciona_dados.

*** GFX - DFLC - Melhorias/refatoração 17/03/2020
    DATA: lv_data_aux  TYPE sy-datum,
          lv_cont      TYPE i,
          lv_cont2     TYPE i,
          lv_mes(2)    TYPE n,
          lv_ano(4)    TYPE n,
          lv_index(1)  TYPE n,
          lv_campo(50) TYPE c.
****Deivison Ferreira - INICIO 21/10/2020
    DATA: v_desconto TYPE zmmt022-brtwr.

*** GFX - JBRS - INICIO - 26/07/2019
    "Ranger
    DATA: lr_werks     TYPE RANGE OF marc-werks,
          lr_matnr     TYPE RANGE OF mseg-matnr,
          lr_matnr_aux TYPE RANGE OF mseg-matnr.
    "Tables
    DATA: lt_tvarvc     TYPE TABLE OF tvarvc,
          lt_zmmt016    TYPE TABLE OF zmmt016,
          lv_menge      TYPE zmmt016-zqtd_venda,
          lt_consumomat TYPE TABLE OF ty_consumomat.
*** GFX - JBRS - FIM - 26/07/2019
*** GFX - JBRS - INICIO - 19/11/2019
    DATA: lr_bsart TYPE RANGE OF ekko-bsart.
*** LD Consultoria  - inicio
    DATA: lv_kposn TYPE prcd_elements-kposn.

    "Constantes
    CONSTANTS: lc_pedido_atacado     TYPE esart VALUE 'ZATA', " APPC - MM.577809 – Ericky Fernandes - 24/06/2024
               lc_pedido_divergencia TYPE esart VALUE 'ZDIV', " APPC - MM.577809 – Ericky Fernandes - 24/06/2024
               lc_ped_varejo_distrib TYPE esart VALUE 'ZVAD'. " APPC - MM.577809 – Ericky Fernandes - 24/06/2024

    APPEND INITIAL LINE TO lr_bsart ASSIGNING FIELD-SYMBOL(<fs_bsart>).
    <fs_bsart>-sign   = 'I'.
    <fs_bsart>-option = 'EQ'.
    <fs_bsart>-low    = 'ZATA'.

    APPEND INITIAL LINE TO lr_bsart ASSIGNING <fs_bsart>.
    <fs_bsart>-sign   = 'I'.
    <fs_bsart>-option = 'EQ'.
    <fs_bsart>-low    = 'ZTIN'.

    APPEND INITIAL LINE TO lr_bsart ASSIGNING <fs_bsart>.
    <fs_bsart>-sign   = 'I'.
    <fs_bsart>-option = 'EQ'.
    <fs_bsart>-low    = 'ZBON'.
*** GFX - JBRS - FIM - 19/11/2019

    FIELD-SYMBOLS: <fs_campo> TYPE any.

    CLEAR: r_werks, r_werks[], gv_cd1, gv_cd2, gv_cd3.
    LOOP AT s_cd INTO s_werks_line.
      IF sy-tabix EQ 1.
        gv_cd1 = s_werks_line-low.
      ENDIF.
      IF sy-tabix EQ 2.
        gv_cd2 = s_werks_line-low.
      ENDIF.
      IF sy-tabix EQ 3.
        gv_cd3 = s_werks_line-low.
      ENDIF.
      r_werks_line = s_werks_line.
      APPEND r_werks_line TO r_werks.
    ENDLOOP.

    LOOP AT s_lojas INTO s_werks_line.
      IF sy-tabix EQ 1.
        gv_lojas = s_werks_line-low.
      ELSE.
        CONCATENATE gv_lojas ';' s_werks_line-low INTO gv_lojas.
      ENDIF.
      r_werks_line = s_werks_line.
      APPEND r_werks_line TO r_werks.
    ENDLOOP.

    LOOP AT s_lgort INTO s_lgort_line.
      IF sy-tabix EQ 1.
        gv_lgort = s_lgort_line-low.
      ELSE.
        CONCATENATE gv_lgort ';' s_lgort_line-low INTO gv_lgort.
      ENDIF.
    ENDLOOP.

    LOOP AT s_ekgrp INTO s_ekgrp_line.
      IF sy-tabix EQ 1.
        gv_ekgrp = s_ekgrp_line-low.
      ELSE.
        CONCATENATE gv_ekgrp ';' s_ekgrp_line-low INTO gv_ekgrp.
      ENDIF.
    ENDLOOP.

    LOOP AT s_matkl INTO s_matkl_line.
      IF sy-tabix EQ 1.
        gv_matkl = s_matkl_line-low.
      ELSE.
        CONCATENATE gv_matkl ';' s_matkl_line-low INTO gv_matkl.
      ENDIF.
    ENDLOOP.

    LOOP AT s_prdha INTO s_prdha_line.
      IF sy-tabix EQ 1.
        gv_prdha = s_prdha_line-low.
      ELSE.
        CONCATENATE gv_prdha ';' s_prdha_line-low INTO gv_prdha.
      ENDIF.
    ENDLOOP.

    LOOP AT s_matnr INTO s_matnr_line.
      WRITE s_matnr_line-low TO s_matnr_line-low.
      IF sy-tabix EQ 1.
        gv_matnr = s_matnr_line-low.
      ELSE.
        CONCATENATE gv_matnr ';' s_matnr_line-low INTO gv_matnr.
      ENDIF.
    ENDLOOP.

    LOOP AT s_bismt INTO s_bismt_line.
      IF sy-tabix EQ 1.
        gv_bismt = s_bismt_line-low.
      ELSE.
        CONCATENATE gv_bismt ';' s_bismt_line-low INTO gv_bismt.
      ENDIF.
    ENDLOOP.

    SELECT * INTO TABLE gt_tvarvc
      FROM tvarvc
      WHERE name EQ 'ZMM_ZMM011_BWART'.

    CLEAR: r_bwart, r_bwart[].
    LOOP AT gt_tvarvc INTO gs_tvarvc.

      r_bwart_line-sign   = 'I'.
      r_bwart_line-option = 'EQ'.
      r_bwart_line-low    = gs_tvarvc-low.
      APPEND r_bwart_line TO r_bwart.

    ENDLOOP.

    SELECT * INTO TABLE gt_tvarvc
      FROM tvarvc
      WHERE name EQ 'ZMM_ZMM011_LGORT'.

    CLEAR: r_lgort, r_lgort[].
    LOOP AT gt_tvarvc INTO gs_tvarvc.

      r_lgort_line-sign   = 'I'.
      r_lgort_line-option = 'EQ'.
      r_lgort_line-low    = gs_tvarvc-low.
      APPEND r_lgort_line TO r_lgort.

    ENDLOOP.

    SELECT * INTO TABLE gt_tvarvc
      FROM tvarvc
      WHERE name EQ 'ZMM_ZMM011_LGORT_CONF'.

    CLEAR: r_lgort_conf, r_lgort_conf[].
    LOOP AT gt_tvarvc INTO gs_tvarvc.

      r_lgort_line-sign   = 'I'.
      r_lgort_line-option = 'EQ'.
      r_lgort_line-low    = gs_tvarvc-low.
      APPEND r_lgort_line TO r_lgort_conf.

    ENDLOOP.

    SELECT * INTO TABLE gt_tvarvc
      FROM tvarvc
      WHERE name EQ 'ZMM_ZMM011_MTART'.

    CLEAR: r_mtart, r_mtart[].
    LOOP AT gt_tvarvc INTO gs_tvarvc.

      r_mtart_line-sign   = 'I'.
      r_mtart_line-option = 'EQ'.
      r_mtart_line-low    = gs_tvarvc-low.
      APPEND r_mtart_line TO r_mtart.

    ENDLOOP.

*** GFX - JBRS - INICIO - 26/07/2019
    "Seleciona a filial
    SELECT low
           high
      INTO CORRESPONDING FIELDS OF TABLE lt_tvarvc
      FROM tvarvc
      WHERE name = 'ZMM_ZMM011_FORN_EXC'.
*** GFX - JBRS - INICIO - 26/07/2019

    SELECT SINGLE * INTO gs_lfa1
      FROM lfa1
      WHERE lifnr IN s_lifnr.

    IF sy-subrc NE 0.
      "Fornecedor & não cadastrado!
      MESSAGE s020(zmm01) DISPLAY LIKE 'E'.
      gv_erro = 'X'.
      EXIT.
    ENDIF.
*{   MODIFY         DEVK908476         GFX - JVRS - Ajuste ZMM011 v2
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 25
        text       = 'Obtendo os dados dos materiais'.

    CLEAR: gt_material, gt_material[].

*   APPC - MM.32019 – Ericky Fernandes - 14/08/2023 - INÍCIO
    "Verifica se o material não foi preenchido
    IF s_matnr IS INITIAL.
      "Percorre as hierarquia de produtos
      LOOP AT s_prdha ASSIGNING FIELD-SYMBOL(<lfs_prdha>).
        <lfs_prdha>-option = 'CP'.
        <lfs_prdha>-low    = |{ <lfs_prdha>-low }*|.
      ENDLOOP.
    ENDIF.
*   APPC - MM.32019 – Ericky Fernandes - 14/08/2023 - FIM

    " Materiais
    SELECT mara~matnr
           mara~matkl
           mara~prdha
           mara~bismt
           mara~mstae
           mara~mfrpn
           mara~zz_lifnr_ref
           mara~meins
           mara~bstme
           mara~ersda
           makt~maktx
           marc~ekgrp
           marc~periv
           marc~perkz
           eord~lifnr
           marc~werks
           eord~ekorg
      INTO CORRESPONDING FIELDS OF TABLE gt_material
      FROM mara
      INNER JOIN makt
             ON ( makt~matnr = mara~matnr )
      INNER JOIN marc
             ON ( marc~matnr = mara~matnr )
      INNER JOIN eord
             ON ( eord~matnr = marc~matnr
             AND  eord~werks = marc~werks )
      WHERE mara~matnr IN s_matnr
        AND mara~matkl IN s_matkl
        AND mara~prdha IN s_prdha
        AND mara~bismt IN s_bismt
        AND mara~mtart IN r_mtart
        AND mara~mstae IN s_mstae
        AND mara~lvorm EQ ''
        AND marc~ekgrp IN s_ekgrp
        AND eord~notkz EQ ''
        AND eord~lifnr IN s_lifnr
        AND marc~werks IN r_werks
        AND makt~spras EQ sy-langu.

    IF gt_material[] IS INITIAL.
      "Nenhum registro foi encontrado.
      MESSAGE s014(zmm01) DISPLAY LIKE 'E'.
      gv_erro = 'X'.
      EXIT.
    ENDIF.

*   APPC - MM.577809 – Ericky Fernandes - 05/06/2024 - Início
    "Busca o valor os dados da última compra
    SELECT ekpo~matnr, " Produto
           ekpo~brtwr, " Valor bruto
           ekpo~menge, " Qtd.pedido
      MAX( ekko~aedat ) AS creationdate " Data de criação do pedido
    FROM ekpo
    INNER JOIN ekko
    ON ekko~ebeln = ekpo~ebeln
    WHERE ekpo~matnr IN @s_matnr
    AND   ekpo~infnr <> @space
    AND   ekko~bsart IN ( @lc_pedido_atacado, @lc_pedido_divergencia, @lc_ped_varejo_distrib )
    GROUP BY ekpo~matnr, ekpo~brtwr, ekpo~menge
    INTO TABLE @DATA(lt_ultima_compra).

    SORT lt_ultima_compra DESCENDING BY creationdate.
    DELETE ADJACENT DUPLICATES FROM lt_ultima_compra COMPARING matnr.
*   APPC - MM.577809 – Ericky Fernandes - 05/06/2024 - Fim

    SORT gt_material BY matnr werks.

    FREE lr_matnr.

*** GFX - DFLC - Início 02/03/2020

    DATA(lt_materiais) = gt_material.

    SORT lt_materiais BY matnr ASCENDING.

    DELETE ADJACENT DUPLICATES FROM lt_materiais COMPARING matnr.

    DATA(lv_count) = lines( lt_materiais ).

    DATA(lv_index_r) = 0.
    "Quebra range caso houver mais do que 15.000 registros
    IF lv_count > 15000.
      FREE lv_count.

      lv_index_r = 0.
      DO.

        lv_index_r = lv_index_r + 1.

        TRY .

            APPEND VALUE #( sign   = 'I'
                            option = 'EQ'
                            low    = lt_materiais[ lv_index_r ]-matnr  ) TO lr_matnr_aux.

            lv_count = lines( lr_matnr_aux[] ).

            IF lv_count = 15000.

              SELECT DISTINCT b~lifnr
                         ,a~matnr
                         ,a~werks
                         ,a~budat
                         ,a~zqtd_venda
                  FROM zmmt016 AS a
                  INNER JOIN eord AS b
                    ON ( b~matnr = a~matnr )
                  APPENDING CORRESPONDING FIELDS OF TABLE @lt_zmmt016
                  WHERE a~matnr IN @lr_matnr_aux
                  AND   a~werks IN @r_werks[]
                  AND   b~lifnr IN @s_lifnr.

              APPEND LINES OF lr_matnr_aux[] TO lr_matnr.

              FREE lr_matnr_aux.

            ENDIF.

          CATCH cx_sy_itab_line_not_found.

            IF lr_matnr_aux[] IS NOT INITIAL.

              SELECT DISTINCT b~lifnr
                       ,a~matnr
                       ,a~werks
                       ,a~budat
                       ,a~zqtd_venda
                FROM zmmt016 AS a
                INNER JOIN eord AS b
                  ON ( b~matnr = a~matnr )
                APPENDING CORRESPONDING FIELDS OF TABLE @lt_zmmt016
                WHERE a~matnr IN @lr_matnr_aux
                AND   a~werks IN @r_werks[]
                AND   b~lifnr IN @s_lifnr.

              APPEND LINES OF lr_matnr_aux TO lr_matnr.

              FREE lr_matnr_aux.

            ENDIF.

            EXIT.

          CATCH cx_root.

            "Trata outros erros.

        ENDTRY.

      ENDDO.

    ELSE.

      lr_matnr = VALUE #( FOR l_matnr IN lt_materiais
                          ( sign   = 'I'
                            option = 'EQ'
                            low    = l_matnr-matnr )
                        ).

      "Seleciona a relação de vendas Material/Mês
      SELECT DISTINCT b~lifnr
                     ,a~matnr
                     ,a~werks
                     ,a~budat
                     ,a~zqtd_venda
        FROM zmmt016 AS a
        INNER JOIN eord AS b
          ON ( b~matnr = a~matnr )
        INTO CORRESPONDING FIELDS OF TABLE @lt_zmmt016
        WHERE a~matnr IN @lr_matnr
        AND   a~werks IN @r_werks[]
        AND   b~lifnr IN @s_lifnr.

    ENDIF.


    DELETE ADJACENT DUPLICATES FROM lr_matnr[] COMPARING low.

    lr_matnr_aux = lr_matnr.

    "Calculo dos meses
    DATA(lv_mes1) = s_rgvnd-low.
    lv_mes1+6(2)  = '01'.

    DATA(lv_mes2) = COND datum( WHEN lv_mes1+4(2) = '12' THEN |{ lv_mes1(4) + 1 }01{ lv_mes1+6(2) }|
                                WHEN lv_mes1+4(2) < 9    THEN |{ lv_mes1(4) }0{ lv_mes1+4(2) + 01 }01|
                                                         ELSE |{ lv_mes1(4) }{ lv_mes1+4(2) + 01 }01| ).

    DATA(lv_mes3) = COND datum( WHEN lv_mes2+4(2) = '12' THEN |{ lv_mes2(4) + 1 }01{ lv_mes2+6(2) }|
                                WHEN lv_mes2+4(2) < 9    THEN |{ lv_mes2(4) }0{ lv_mes2+4(2) + 01 }01|
                                                         ELSE |{ lv_mes2(4) }{ lv_mes2+4(2) + 01 }01| ).

    DATA(lv_mes4) = COND datum( WHEN lv_mes3+4(2) = '12' THEN |{ lv_mes3(4) + 1 }01{ lv_mes3+6(2) }|
                                WHEN lv_mes3+4(2) < 9    THEN |{ lv_mes3(4) }0{ lv_mes3+4(2) + 01 }01|
                                                         ELSE |{ lv_mes3(4) }{ lv_mes3+4(2) + 01 }01| ).

    DATA(lv_mes5) = COND datum( WHEN lv_mes4+4(2) = '12' THEN |{ lv_mes4(4) + 1 }01{ lv_mes4+6(2) }|
                                WHEN lv_mes4+4(2) < 9    THEN |{ lv_mes4(4) }0{ lv_mes4+4(2) + 01 }01|
                                                         ELSE |{ lv_mes4(4) }{ lv_mes4+4(2) + 01 }01| ).

*    DATA(lv_mes6) = COND datum( WHEN lv_mes5+4(2) = '12' THEN |{ lv_mes5(4) + 1 }01{ lv_mes5+6(2) }|
*                                WHEN lv_mes5+4(2) < 9    THEN |{ lv_mes5(4) }0{ lv_mes5+4(2) + 01 }01|
*                                                         ELSE |{ lv_mes5(4) }{ lv_mes5+4(2) + 01 }01| ).


    CLEAR: gt_consumomat, gt_consumomat[], gs_consumomat.

    IF p_hist = 'X'.

      LOOP AT lt_zmmt016 ASSIGNING FIELD-SYMBOL(<fs_zmmt016>) WHERE lifnr IN s_lifnr
                                                                AND matnr IN lr_matnr
                                                                AND werks IN r_werks
                                                                AND ( budat(6) = lv_mes1(6)
                                                                   OR budat(6) = lv_mes2(6)
                                                                   OR budat(6) = lv_mes3(6)
                                                                   OR budat(6) = lv_mes4(6)
                                                                   OR budat(6) = lv_mes5(6) )
                                                                   "OR budat(6) = lv_mes6(6) )
        GROUP BY <fs_zmmt016>-matnr ASCENDING.

        gs_consumomat-matnr = <fs_zmmt016>-matnr.

        LOOP AT GROUP <fs_zmmt016> ASSIGNING FIELD-SYMBOL(<fs_zmms016_aux>).

          DATA(lv_mes_calc) = COND i( WHEN <fs_zmms016_aux>-budat(6) = lv_mes1(6)  THEN 1
                                      WHEN <fs_zmms016_aux>-budat(6) = lv_mes2(6)  THEN 2
                                      WHEN <fs_zmms016_aux>-budat(6) = lv_mes3(6)  THEN 3
                                      WHEN <fs_zmms016_aux>-budat(6) = lv_mes4(6)  THEN 4
                                      WHEN <fs_zmms016_aux>-budat(6) = lv_mes5(6)  THEN 5 ).
          "WHEN <fs_zmms016_aux>-budat(6) = lv_mes6(6)  THEN 6 ).

          DATA(lv_assign) =  |GS_CONSUMOMAT-VENDA_MES{ lv_mes_calc }|.

          ASSIGN (lv_assign) TO <fs_campo>.

          IF <fs_campo> IS ASSIGNED.

            <fs_campo> = <fs_campo> + <fs_zmms016_aux>-zqtd_venda.

          ENDIF.

        ENDLOOP.

        DELETE lr_matnr_aux WHERE low = gs_consumomat-matnr.

        COLLECT gs_consumomat INTO gt_consumomat.
        FREE: gs_consumomat.
        UNASSIGN <fs_campo>.

      ENDLOOP.

      IF lr_matnr_aux[] IS NOT INITIAL.

        lt_consumomat = VALUE #( FOR lr_mat IN lr_matnr_aux
                                  ( matnr  = lr_mat-low )
                                ).

        APPEND LINES OF lt_consumomat TO gt_consumomat.

        SORT gt_consumomat BY matnr ASCENDING.

        CLEAR: lt_consumomat,
               lr_matnr_aux.

        lr_matnr_aux = lr_matnr.

      ELSE.

        lr_matnr_aux = lr_matnr.

      ENDIF.

    ENDIF.

    CLEAR: gt_marm, gt_marm[].

    SELECT matnr
          ,umrez
          ,meinh
        FROM marm
        INTO TABLE @DATA(lt_marm)
        WHERE matnr IN @lr_matnr.

    SELECT matnr
          ,meins
          ,bstme
      FROM mara
      INTO TABLE @DATA(lt_mara)
      WHERE matnr IN @lr_matnr.

    IF sy-subrc = 0.
*      LOOP AT lt_mara ASSIGNING FIELD-SYMBOL(<fs_mara>).
*
*        APPEND INITIAL LINE TO gt_marm ASSIGNING FIELD-SYMBOL(<fs_marm>).
*        <fs_marm>-matnr = <fs_mara>-matnr.
*
*        IF <fs_mara>-bstme IS INITIAL.
*          READ TABLE lt_marm ASSIGNING FIELD-SYMBOL(<fs_r_marm_me>) WITH KEY matnr = <fs_mara>-matnr
*                                                                             meinh = <fs_mara>-meins.
*          IF sy-subrc = 0.
*            <fs_marm>-meinh = CONV #( <fs_mara>-meins ).
*            <fs_marm>-umrez = <fs_r_marm_me>-umrez.
*          ENDIF.
*        ELSE.
*          READ TABLE lt_marm ASSIGNING FIELD-SYMBOL(<fs_r_marm_bst>) WITH KEY matnr = <fs_mara>-matnr
*                                                                              meinh = <fs_mara>-bstme.
*          IF sy-subrc = 0.
*            <fs_marm>-meinh = CONV #( <fs_mara>-bstme ).
*            <fs_marm>-umrez = <fs_r_marm_bst>-umrez.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.

      gt_marm = VALUE #( FOR ls_mr IN lt_mara
                         LET s_marm = COND #( WHEN ls_mr-bstme IS INITIAL AND line_exists( lt_marm[ matnr = ls_mr-matnr meinh = ls_mr-meins ] )
                                                THEN lt_marm[ matnr = ls_mr-matnr
                                                              meinh = ls_mr-meins ]
                                              WHEN line_exists( lt_marm[ matnr = ls_mr-matnr meinh = ls_mr-bstme ] )
                                                THEN lt_marm[ matnr = ls_mr-matnr
                                                              meinh = ls_mr-bstme ] )
                         IN ( matnr = ls_mr-matnr
                              meinh = CONV #( COND #( WHEN ls_mr-bstme IS INITIAL AND s_marm IS NOT INITIAL
                                                        THEN ls_mr-meins
                                                      WHEN ls_mr-bstme IS NOT INITIAL AND s_marm IS INITIAL
                                                        THEN ls_mr-bstme ) )
                              umrez = COND #( WHEN s_marm IS NOT INITIAL
                                                THEN s_marm-umrez )
                             )
                       ).

    ENDIF.

*** GFX - JBRS - 06/02/2020 - FIM
    SORT gt_marm BY matnr.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 50
        text       = 'Obtendo os dados dos materiais semelhantes'.

    CLEAR: gt_semelhante, gt_semelhante[].
    IF rb_cs IS NOT INITIAL.

      DATA(lt_material) = gt_material.

      DELETE ADJACENT DUPLICATES FROM lt_material COMPARING bismt.

      IF lt_material IS NOT INITIAL.

        SELECT matnr bismt
          INTO TABLE gt_semelhante
          FROM mara
          FOR ALL ENTRIES IN lt_material
          WHERE bismt = lt_material-bismt
          AND   bismt <> ''.

      ENDIF.

    ENDIF.

    SORT gt_semelhante BY matnr bismt.

*    LOOP AT gt_semelhante INTO gs_semelhante.
*
*      CLEAR gs_material.
*      READ TABLE gt_material INTO gs_material WITH KEY matnr = gs_semelhante-matnr
*                                                       bismt = gs_semelhante-bismt
*                                                       BINARY SEARCH.
*
*      IF sy-subrc EQ 0.
*        DELETE gt_semelhante.
*      ENDIF.
*
*    ENDLOOP.

    IF gt_semelhante IS NOT INITIAL.

      DATA(lt_semelhantes) = gt_semelhante.

      lt_semelhantes = VALUE #( FOR s_sem IN gt_semelhante
                                LET s_mat = COND #( WHEN line_exists( gt_material[ matnr = gs_semelhante-matnr bismt = gs_semelhante-bismt ] )
                                                      THEN gt_material[ matnr = gs_semelhante-matnr
                                                                        bismt = gs_semelhante-bismt ] )

                                IN ( matnr = COND #( WHEN s_mat IS INITIAL
                                                       THEN s_sem-matnr )
                                     bismt = COND #( WHEN s_mat IS INITIAL
                                                       THEN s_sem-bismt )
                                     maktx = COND #( WHEN s_mat IS INITIAL
                                                       THEN s_sem-maktx )
                                   )
                              ).

      DELETE lt_semelhantes WHERE matnr IS INITIAL.

      gt_semelhante = lt_semelhantes.

    ENDIF.

    FREE lr_matnr.

    IF gt_semelhante IS NOT INITIAL.

      CLEAR: gt_matsem_aux, gt_matsem_aux[].
      SELECT mara~matnr
             mara~bismt
             mara~meins
             mara~mstae
             mara~matkl
             mara~prdha
             mara~mfrpn
             mara~zz_lifnr_ref
             mara~ersda
             marc~ekgrp
             makt~maktx
             marc~werks
        INTO CORRESPONDING FIELDS OF TABLE gt_matsem_aux
        FROM mara
        INNER JOIN marc
               ON ( marc~matnr = mara~matnr )
        INNER JOIN makt
               ON ( makt~matnr = mara~matnr )
        FOR ALL ENTRIES IN gt_semelhante
        WHERE mara~matnr EQ gt_semelhante-matnr
          AND mara~lvorm EQ ''
          AND marc~werks IN r_werks
          AND marc~ekgrp IN s_ekgrp
          AND makt~spras EQ sy-langu.

      SORT gt_matsem_aux BY matnr werks.

      CLEAR: gt_matsem, gt_matsem[].
      SELECT mara~matnr
             mara~bismt
             mara~meins
             mara~mstae
             mara~matkl
             mara~prdha
             mara~mfrpn
             mara~zz_lifnr_ref
             mara~ersda
             marc~ekgrp
             makt~maktx
             marc~werks
             eord~lifnr
             eord~ekorg
        INTO CORRESPONDING FIELDS OF TABLE gt_matsem
        FROM mara
        INNER JOIN marc
               ON ( marc~matnr = mara~matnr )
        INNER JOIN eord
               ON ( eord~matnr = marc~matnr
               AND  eord~werks = marc~werks )
        INNER JOIN makt
               ON ( makt~matnr = mara~matnr )
        FOR ALL ENTRIES IN gt_semelhante
        WHERE mara~matnr EQ gt_semelhante-matnr
          AND mara~lvorm EQ ''
          AND eord~werks IN r_werks
          AND marc~ekgrp IN s_ekgrp
          AND makt~spras EQ sy-langu.

      SORT gt_matsem BY matnr werks.

      lr_matnr = VALUE #( FOR ls_mat_s IN gt_matsem
                                ( sign   = 'I'
                                  option = 'EQ'
                                  low    = ls_mat_s-matnr )
                          ).

      DELETE ADJACENT DUPLICATES FROM lr_matnr COMPARING low.

      SELECT matnr
            ,umrez
            ,meinh
          FROM marm
          INTO TABLE @DATA(lt_marm_sem)
          WHERE matnr IN @lr_matnr.

      SELECT matnr
            ,meins
            ,bstme
        FROM mara
        INTO TABLE @DATA(lt_mara_sem)
        WHERE matnr IN @lr_matnr.

      IF sy-subrc = 0.
*        LOOP AT lt_mara_sem ASSIGNING FIELD-SYMBOL(<fs_mara_sem>).
*
*          APPEND INITIAL LINE TO gt_marm ASSIGNING FIELD-SYMBOL(<fs_marm_sem>).
*          <fs_marm_sem>-matnr = <fs_mara_sem>-matnr.
*
*          IF <fs_mara_sem>-bstme IS INITIAL.
*            READ TABLE lt_marm_sem ASSIGNING FIELD-SYMBOL(<fs_r_marm_sem_m>) WITH KEY matnr = <fs_mara_sem>-matnr
*                                                                                      meinh = <fs_mara_sem>-meins.
*            IF sy-subrc = 0.
*              <fs_marm_sem>-meinh = CONV #( <fs_mara_sem>-meins ).
*              <fs_marm_sem>-umrez = <fs_r_marm_sem_m>-umrez.
*            ENDIF.
*          ELSE.
*            READ TABLE lt_marm_sem ASSIGNING FIELD-SYMBOL(<fs_r_marm_sem_bst>) WITH KEY matnr = <fs_mara_sem>-matnr
*                                                                                        meinh = <fs_mara_sem>-bstme.
*            IF sy-subrc = 0.
*              <fs_marm_sem>-meinh = CONV #( <fs_mara_sem>-bstme ).
*              <fs_marm_sem>-umrez = <fs_r_marm_sem_bst>-umrez.
*            ENDIF.
*          ENDIF.
*        ENDLOOP.

        "Adição de mais campo na  marm
        DATA(lt_marm_s) = gt_marm.

        lt_marm_s = VALUE #( FOR ls_mr_s  IN lt_mara_sem
                             LET s_marm_s = COND #( WHEN ls_mr_s-bstme IS INITIAL AND line_exists( lt_marm_sem[ matnr = ls_mr_s-matnr meinh = ls_mr_s-meins ] )
                                                     THEN lt_marm_sem[ matnr = ls_mr_s-matnr
                                                                       meinh = ls_mr_s-meins ]
                                                    WHEN line_exists( lt_marm_sem[ matnr = ls_mr_s-matnr meinh = ls_mr_s-bstme ] )
                                                     THEN lt_marm_sem[ matnr = ls_mr_s-matnr
                                                                       meinh = ls_mr_s-bstme ] )
                             IN ( matnr = ls_mr_s-matnr
                                  meinh = CONV #( COND #( WHEN ls_mr_s-bstme IS INITIAL AND s_marm_s IS NOT INITIAL
                                                           THEN ls_mr_s-meins
                                                          WHEN ls_mr_s-bstme IS NOT INITIAL AND s_marm_s IS INITIAL
                                                           THEN ls_mr_s-bstme ) )
                                  umrez = COND #( WHEN s_marm_s IS NOT INITIAL
                                                   THEN s_marm_s-umrez )
                                )
                             ).

        APPEND LINES OF lt_marm_s TO gt_marm.

      ENDIF.

    ENDIF.

    CLEAR: gt_consumosem, gt_consumosem[], gs_consumosem.

    IF p_hist EQ 'X'.

      LOOP AT gt_matsem INTO gs_matsem WHERE werks IN s_lojas.

        gs_consumosem-matnr = gs_matsem-matnr.

        CLEAR: gt_ges_verb_tab, gt_ges_verb_tab[].

        CALL FUNCTION 'CONSUMPTION_READ_FOR_MM'
          EXPORTING
            matnr        = gs_matsem-matnr
            werks        = gs_matsem-werks
            perkz        = 'M'
            periv        = '24'
          TABLES
            ges_verb_tab = gt_ges_verb_tab
          EXCEPTIONS
            wrong_call   = 1
            not_found    = 2
            OTHERS       = 3.

        IF NOT gt_ges_verb_tab[] IS INITIAL.
*          lv_data_aux = s_rgvnd-low.
*          lv_data_aux+6(2) = '01'.
*          lv_index = 0.
*          DO 6 TIMES.
*            ADD 1 TO lv_index.
*            CONCATENATE 'GS_CONSUMOSEM-VENDA_MES'
*                        lv_index
*                        INTO lv_campo.
*
*            ASSIGN (lv_campo) TO <fs_campo>.
*            READ TABLE gt_ges_verb_tab INTO gs_ges_verb_tab WITH KEY ertag = lv_data_aux.
*
*            IF sy-subrc EQ 0.
*              <fs_campo> = gs_ges_verb_tab-vbwrt.
*            ENDIF.
*
*            lv_mes = lv_data_aux+4(2).
*            lv_ano = lv_data_aux(4).
*            IF lv_mes EQ '12'.
*              lv_mes = 01.
*              lv_ano = lv_ano + 1.
*            ELSE.
*              lv_mes = lv_mes + 1.
*            ENDIF.
*            lv_data_aux+4(2) = lv_mes.
*            lv_data_aux(4)   = lv_ano.
*
*          ENDDO.

          LOOP AT gt_ges_verb_tab ASSIGNING FIELD-SYMBOL(<ls_verb>) WHERE ertag = lv_mes1
                                                                       OR ertag = lv_mes2
                                                                       OR ertag = lv_mes3
                                                                       OR ertag = lv_mes4
                                                                       OR ertag = lv_mes5.
            "OR ertag = lv_mes6.


            DATA(lv_mes_calc_s) = COND i( WHEN <ls_verb>-ertag = lv_mes1 THEN 1
                                          WHEN <ls_verb>-ertag = lv_mes2 THEN 2
                                          WHEN <ls_verb>-ertag = lv_mes3 THEN 3
                                          WHEN <ls_verb>-ertag = lv_mes4 THEN 4
                                          WHEN <ls_verb>-ertag = lv_mes5 THEN 5 ).
            "WHEN <ls_verb>-ertag = lv_mes6 THEN 6 ).

            DATA(lv_assign_sem) =  |GS_CONSUMOSEM-VENDA_MES{ lv_mes_calc_s }|.

            ASSIGN (lv_assign_sem) TO <fs_campo>.

            IF <fs_campo> IS ASSIGNED.

              <fs_campo> = <fs_campo> + <ls_verb>-vbwrt.

            ENDIF.

          ENDLOOP.

        ENDIF.

        COLLECT gs_consumosem INTO gt_consumosem.
        FREE: gs_consumosem, gs_ges_verb_tab.
        UNASSIGN <fs_campo>.

      ENDLOOP.

    ENDIF.

    IF p_real EQ 'X'.
      "MSEG PARA MATERIAIS
*      SELECT budat_mkpf
*             matnr
*             werks
*             menge
*             bwart
*             meins
*             mblnr
*             mjahr
*             zeile
*             sjahr
*             smbln
*             smblp
*        INTO CORRESPONDING FIELDS OF TABLE gt_mseg
*        FROM mseg
*         FOR ALL ENTRIES IN GT_MATERIAL
*        WHERE budat_mkpf IN s_lojas
*          AND matnr = GT_MATERIAL-MATNR
*          AND werks IN r_werks
*          AND bwart IN r_bwart.

      SELECT budat_mkpf
             matnr
             werks
             menge
             bwart
             meins
             mblnr
             mjahr
             zeile
             sjahr
             smbln
             smblp
        INTO CORRESPONDING FIELDS OF TABLE gt_mseg
        FROM mseg
        WHERE budat_mkpf IN s_lojas
          AND matnr      IN lr_matnr_aux
          AND werks      IN r_werks
          AND bwart      IN r_bwart.

      CLEAR: gt_msegsum, gt_msegsum[], gs_msegsum.
      CLEAR: gt_matmeins, gt_matmeins[], gs_matmeins.

*      LOOP AT gt_mseg INTO gs_mseg.
*
*        IF NOT gs_mseg-smbln IS INITIAL.
*          DELETE gt_mseg WHERE mblnr = gs_mseg-smbln
*                           AND mjahr = gs_mseg-sjahr
*                           AND zeile = gs_mseg-smblp.
*          DELETE gt_mseg.
*          CONTINUE.
*        ENDIF.
*
*        "Sumarizar registros por mes
*        gs_mseg-werks           = ''.
*        gs_mseg-bwart           = ''.
*        gs_mseg-budat_mkpf+6(2) = '01'.
*        gs_msegsum              = gs_mseg.
*
*        COLLECT gs_msegsum INTO gt_msegsum.
*
*        gs_matmeins-matnr = gs_msegsum-matnr.
*        gs_matmeins-meins = gs_msegsum-meins.
*        COLLECT gs_matmeins INTO gt_matmeins.
*
*      ENDLOOP.

      LOOP AT gt_mseg INTO gs_mseg WHERE smbln IS NOT INITIAL.

        DELETE gt_mseg WHERE mblnr = gs_mseg-smbln
                         AND mjahr = gs_mseg-sjahr
                         AND zeile = gs_mseg-smblp.
        DELETE gt_mseg.

      ENDLOOP.

      DATA(lt_mseg) = gt_mseg.

      SORT lt_mseg BY budat_mkpf(6)
                      matnr
                      meins
                      mblnr
                      mjahr
                      zeile.

      DELETE ADJACENT DUPLICATES FROM lt_mseg COMPARING budat_mkpf(6)
                                                        matnr
                                                        meins
                                                        mblnr
                                                        mjahr
                                                        zeile.

      gt_msegsum = VALUE #( FOR s_mseg IN lt_mseg
                            ( matnr      = s_mseg-matnr
                              meins      = s_mseg-meins
                              budat_mkpf = |{ s_mseg-budat_mkpf(6) }01|
                              menge      = REDUCE #( INIT v_m TYPE menge_d FOR s_mseg_m IN gt_mseg WHERE ( budat_mkpf(6) = s_mseg-budat_mkpf(6)
                                                                                                     AND   matnr         = s_mseg-matnr
                                                                                                     AND   meins         = s_mseg-meins
                                                                                                     AND   mblnr         = s_mseg-mblnr
                                                                                                     AND   mjahr         = s_mseg-mjahr
                                                                                                     AND   zeile         = s_mseg-zeile )
                                                     NEXT v_m = v_m + s_mseg_m-menge )
                             )
                           ).

      SORT gt_msegsum BY matnr meins.

      gt_matmeins = VALUE #( FOR l_s IN gt_msegsum
                                  ( matnr = l_s-matnr
                                    meins = l_s-meins )
                           ).

      DELETE ADJACENT DUPLICATES FROM gt_matmeins COMPARING ALL FIELDS.

*      LOOP AT gt_msegsum INTO gs_msegsum.
*
*        gs_matmeins-matnr = gs_msegsum-matnr.
*        gs_matmeins-meins = gs_msegsum-meins.
*        COLLECT gs_matmeins INTO gt_matmeins.
*
*      ENDLOOP.

      CLEAR: gt_msegmesmat, gt_msegmesmat[], gs_msegmesmat.
      LOOP AT gt_matmeins INTO gs_matmeins.

        gs_msegmesmat-matnr = gs_msegsum-matnr.
        gs_msegmesmat-meins = gs_msegsum-meins.
*        lv_data_aux = s_rgvnd-low.
*        lv_data_aux+6(2) = '01'.
*        lv_index = 0.
*        DO 6 TIMES.
*          ADD 1 TO lv_index.
*          CONCATENATE 'GS_MSEGMESMAT-VENDA_MES'
*                      lv_index
*                      INTO lv_campo.
*
*          ASSIGN (lv_campo) TO <fs_campo>.
*          READ TABLE gt_msegsum INTO gs_msegsum WITH KEY budat_mkpf = lv_data_aux.
*
*          IF sy-subrc EQ 0.
*            <fs_campo> = gs_msegsum-menge.
*          ENDIF.
*
*          lv_mes = lv_data_aux+4(2).
*          lv_ano = lv_data_aux(4).
*          IF lv_mes EQ '12'.
*            lv_mes = 01.
*            lv_ano = lv_ano + 1.
*          ELSE.
*            lv_mes = lv_mes + 1.
*          ENDIF.
*          lv_data_aux+4(2) = lv_mes.
*          lv_data_aux(4)   = lv_ano.
*
*        ENDDO.

        LOOP AT gt_msegsum ASSIGNING FIELD-SYMBOL(<ls_sum>) WHERE budat_mkpf = lv_mes1
                                                               OR budat_mkpf = lv_mes2
                                                               OR budat_mkpf = lv_mes3
                                                               OR budat_mkpf = lv_mes4
                                                               OR budat_mkpf = lv_mes5.
          "OR budat_mkpf = lv_mes6.

          DATA(lv_mes_calc_mse) = COND i( WHEN <ls_sum>-budat_mkpf = lv_mes1 THEN 1
                                          WHEN <ls_sum>-budat_mkpf = lv_mes2 THEN 2
                                          WHEN <ls_sum>-budat_mkpf = lv_mes3 THEN 3
                                          WHEN <ls_sum>-budat_mkpf = lv_mes4 THEN 4
                                          WHEN <ls_sum>-budat_mkpf = lv_mes5 THEN 5 ).
          "WHEN <ls_sum>-budat_mkpf = lv_mes6 THEN 6 ).

          DATA(lv_assign_mse) =  |GS_MSEGMESMAT-VENDA_MES{ lv_mes_calc_mse }|.

          ASSIGN (lv_assign_mse) TO <fs_campo>.

          IF <fs_campo> IS ASSIGNED.

            <fs_campo> = <fs_campo> + <ls_sum>-menge.

          ENDIF.

        ENDLOOP.

        COLLECT gs_msegmesmat INTO gt_msegmesmat.

      ENDLOOP.

      IF gt_matsem IS NOT INITIAL.

        "MSEG PARA SEMELHANTES
*        SELECT budat_mkpf
*               matnr
*               werks
*               menge
*               bwart
*               meins
*               mblnr
*               mjahr
*               zeile
*               sjahr
*               smbln
*               smblp
*          INTO CORRESPONDING FIELDS OF TABLE gt_mseg
*          FROM mseg
*          FOR ALL ENTRIES IN gt_matsem
*          WHERE budat_mkpf IN s_lojas
*            AND matnr EQ gt_matsem-matnr
*            AND werks IN r_werks
*            AND bwart IN r_bwart.

        SELECT budat_mkpf
               matnr
               werks
               menge
               bwart
               meins
               mblnr
               mjahr
               zeile
               sjahr
               smbln
               smblp
          INTO CORRESPONDING FIELDS OF TABLE gt_mseg
          FROM mseg
          WHERE budat_mkpf IN s_lojas
            AND matnr IN lr_matnr
            AND werks IN r_werks
            AND bwart IN r_bwart.

      ENDIF.

      CLEAR: gt_msegsum, gt_msegsum[], gs_msegsum.
      CLEAR: gt_matmeins, gt_matmeins[].
*      LOOP AT gt_mseg INTO gs_mseg.
*
*        IF NOT gs_mseg-smbln IS INITIAL.
*          DELETE gt_mseg WHERE mblnr = gs_mseg-smbln
*                           AND mjahr = gs_mseg-sjahr
*                           AND zeile = gs_mseg-smblp.
*          DELETE gt_mseg.
*          CONTINUE.
*        ENDIF.
*
*        "Sumarizar registros por mes
*        gs_mseg-werks           = ''.
*        gs_mseg-bwart           = ''.
*        gs_mseg-budat_mkpf+6(2) = '01'.
*        gs_msegsum              = gs_mseg.
*
*        COLLECT gs_msegsum INTO gt_msegsum.
*
*        gs_matmeins-matnr = gs_msegsum-matnr.
*        gs_matmeins-meins = gs_msegsum-meins.
*        COLLECT gs_matmeins INTO gt_matmeins.
*
*      ENDLOOP.

      LOOP AT gt_mseg INTO gs_mseg WHERE smbln IS NOT INITIAL.

        DELETE gt_mseg WHERE mblnr = gs_mseg-smbln
                         AND mjahr = gs_mseg-sjahr
                         AND zeile = gs_mseg-smblp.
        DELETE gt_mseg.

      ENDLOOP.

      DATA(lt_mseg_s) = gt_mseg.

      SORT lt_mseg_s BY budat_mkpf(6)
                        matnr
                        meins
                        mblnr
                        mjahr
                        zeile.

      DELETE ADJACENT DUPLICATES FROM lt_mseg_s COMPARING budat_mkpf(6)
                                                          matnr
                                                          meins
                                                          mblnr
                                                          mjahr
                                                          zeile.

      gt_msegsum = VALUE #( FOR s_mseg IN lt_mseg_s
                            ( matnr      = s_mseg-matnr
                              meins      = s_mseg-meins
                              budat_mkpf = |{ s_mseg-budat_mkpf(6) }01|
                              menge      = REDUCE #( INIT v_m TYPE menge_d FOR s_mseg_m IN gt_mseg WHERE ( budat_mkpf(6) = s_mseg-budat_mkpf(6)
                                                                                                     AND   matnr         = s_mseg-matnr
                                                                                                     AND   meins         = s_mseg-meins
                                                                                                     AND   mblnr         = s_mseg-mblnr
                                                                                                     AND   mjahr         = s_mseg-mjahr
                                                                                                     AND   zeile         = s_mseg-zeile )
                                                     NEXT v_m = v_m + s_mseg_m-menge )
                             )
                           ).

      SORT gt_msegsum BY matnr meins.

      gt_matmeins = VALUE #( FOR l_s IN gt_msegsum
                                  ( matnr = l_s-matnr
                                    meins = l_s-meins )
                           ).

      DELETE ADJACENT DUPLICATES FROM gt_matmeins COMPARING ALL FIELDS.

*      LOOP AT gt_msegsum INTO gs_msegsum.
*
*        gs_matmeins-matnr = gs_msegsum-matnr.
*        gs_matmeins-meins = gs_msegsum-meins.
*        COLLECT gs_matmeins INTO gt_matmeins.
*
*      ENDLOOP.

      CLEAR: gt_msegmessem, gt_msegmessem[], gs_msegmessem.
      LOOP AT gt_matmeins INTO gs_matmeins.

        gs_msegmessem-matnr = gs_msegsum-matnr.
        gs_msegmessem-meins = gs_msegsum-meins.
*        lv_data_aux = s_rgvnd-low.
*        lv_data_aux+6(2) = '01'.
*        lv_index = 0.
*        DO 6 TIMES.
*          ADD 1 TO lv_index.
*          CONCATENATE 'GS_MSEGMESSEM-VENDA_MES'
*                      lv_index
*                      INTO lv_campo.
*
*          ASSIGN (lv_campo) TO <fs_campo>.
*          READ TABLE gt_msegsum INTO gs_msegsum WITH KEY budat_mkpf = lv_data_aux.
*
*          IF sy-subrc EQ 0.
*            <fs_campo> = gs_msegsum-menge.
*          ENDIF.
*
*          lv_mes = lv_data_aux+4(2).
*          lv_ano = lv_data_aux(4).
*          IF lv_mes EQ '12'.
*            lv_mes = 01.
*            lv_ano = lv_ano + 1.
*          ELSE.
*            lv_mes = lv_mes + 1.
*          ENDIF.
*          lv_data_aux+4(2) = lv_mes.
*          lv_data_aux(4)   = lv_ano.
*
*        ENDDO.
*
        LOOP AT gt_msegsum ASSIGNING FIELD-SYMBOL(<ls_sum_s>) WHERE budat_mkpf = lv_mes1
                                                                 OR budat_mkpf = lv_mes2
                                                                 OR budat_mkpf = lv_mes3
                                                                 OR budat_mkpf = lv_mes4
                                                                 OR budat_mkpf = lv_mes5.
          "OR budat_mkpf = lv_mes6.

          DATA(lv_mes_calc_mse_sem) = COND i( WHEN <ls_sum_s>-budat_mkpf = lv_mes1 THEN 1
                                              WHEN <ls_sum_s>-budat_mkpf = lv_mes2 THEN 2
                                              WHEN <ls_sum_s>-budat_mkpf = lv_mes3 THEN 3
                                              WHEN <ls_sum_s>-budat_mkpf = lv_mes4 THEN 4
                                              WHEN <ls_sum_s>-budat_mkpf = lv_mes5 THEN 5 ).
          "WHEN <ls_sum_s>-budat_mkpf = lv_mes6 THEN 6 ).

          DATA(lv_assign_mse_sem) =  |GS_MSEGMESSEM-VENDA_MES{ lv_mes_calc_mse_sem }|.

          ASSIGN (lv_assign_mse_sem) TO <fs_campo>.

          IF <fs_campo> IS ASSIGNED.

            <fs_campo> = <fs_campo> + <ls_sum>-menge.

          ENDIF.

        ENDLOOP.

        COLLECT gs_msegmessem INTO gt_msegmessem.

      ENDLOOP.
    ENDIF.

    "EBAN PARA MATERIAIS
*    SELECT matnr
*           werks
*           lifnr
*           banfn
*           bnfpo
*           erdat
*           bednr
*           menge
*           meins
*      INTO CORRESPONDING FIELDS OF TABLE gt_eban
*      FROM eban
*      FOR ALL ENTRIES IN gt_material
*      WHERE matnr EQ gt_material-matnr
*        AND werks IN r_werks
*        AND loekz EQ ''
*        AND bstyp EQ 'B'
*        AND statu EQ 'N'
*        AND flief eq gt_material-lifnr.
    SELECT matnr
           werks
           lifnr
           banfn
           bnfpo
           erdat
           bednr
           menge
           meins
      INTO CORRESPONDING FIELDS OF TABLE gt_eban
      FROM eban
      WHERE matnr IN lr_matnr_aux
        AND werks IN r_werks
        AND loekz EQ ''
        AND bstyp EQ 'B'
        AND statu EQ 'N'
        AND flief IN s_lifnr.

    SORT gt_eban BY matnr werks.

    CLEAR: gt_ebansum, gt_ebansum[], gs_ebansum.

*    LOOP AT gt_eban INTO gs_eban.
*
*      "Sumarizar registros por mes
*      gs_eban-werks      = ''.
*      gs_eban-lifnr      = ''.
*      gs_eban-banfn      = ''.
*      gs_eban-bnfpo      = ''.
*      gs_eban-bednr      = ''.
**      gs_eban-erdat+6(2) = '01'.
*      gs_eban-erdat      = ''.
*      gs_eban-meins      = ''.
*      gs_ebansum              = gs_eban.
*
*      COLLECT gs_ebansum INTO gt_ebansum.
*
*    ENDLOOP.
    IF gt_eban IS NOT INITIAL.
      DATA(lt_eban) = gt_eban.

      SORT lt_eban BY matnr ASCENDING.

      DELETE ADJACENT DUPLICATES FROM lt_eban COMPARING matnr.

      gt_ebansum  = VALUE #( FOR s_eb IN lt_eban
                             ( matnr = s_eb-matnr
                               menge = REDUCE #( INIT b_m TYPE menge_d FOR s_ebg IN gt_eban WHERE ( matnr = s_eb-matnr )
                                                 NEXT b_m = b_m + s_ebg-menge )
                             )
                           ).

      SORT gt_ebansum BY matnr meins.

      CLEAR: gt_matmeins, gt_matmeins[], gs_matmeins.
*    LOOP AT gt_ebansum INTO gs_ebansum.
*
*      gs_matmeins-matnr = gs_ebansum-matnr.
*      gs_matmeins-meins = gs_ebansum-meins.
*      COLLECT gs_matmeins INTO gt_matmeins.
*
*    ENDLOOP.

      gt_matmeins = VALUE #( FOR l_bs IN gt_ebansum
                              ( matnr = l_bs-matnr )
                           ).

      DELETE ADJACENT DUPLICATES FROM gt_matmeins COMPARING matnr.

      CLEAR: gt_ebanmesmat, gt_ebanmesmat[], gs_ebanmesmat.
*    LOOP AT gt_ebansum INTO gs_ebansum.
*
*      gs_ebanmesmat-matnr = gs_ebansum-matnr.
*      gs_ebanmesmat-meins = gs_ebansum-meins.
*      gs_ebanmesmat-menge = gs_ebansum-menge.
*
*      COLLECT gs_ebanmesmat INTO gt_ebanmesmat.
*
*    ENDLOOP.

      gt_ebanmesmat = VALUE #( FOR s_eb IN gt_ebansum
                              ( matnr = s_eb-matnr
                                menge = s_eb-menge )
                             ).

    ENDIF.

    IF gt_matsem IS NOT INITIAL.

      "EBAN PARA SEMELHANTES
*      SELECT matnr
*             werks
*             lifnr
*             banfn
*             bnfpo
*             erdat
*             bednr
*             menge
*             meins
*        INTO CORRESPONDING FIELDS OF TABLE gt_eban
*        FROM eban
*        FOR ALL ENTRIES IN gt_matsem
*        WHERE matnr EQ gt_matsem-matnr
*          AND werks IN r_werks
*          AND loekz EQ ''
*          AND bstyp EQ 'B'
*          AND statu EQ 'N'.
      SELECT matnr
             werks
             lifnr
             banfn
             bnfpo
             erdat
             bednr
             menge
             meins
        INTO CORRESPONDING FIELDS OF TABLE gt_eban
        FROM eban
        WHERE matnr IN lr_matnr
          AND werks IN r_werks
          AND loekz EQ ''
          AND bstyp EQ 'B'
          AND statu EQ 'N'.

      SORT gt_eban BY matnr werks.

      CLEAR: gt_ebansum, gt_ebansum[], gs_ebansum.
*    LOOP AT gt_eban INTO gs_eban.
*
*      "Sumarizar registros por mes
*      gs_eban-werks      = ''.
*      gs_eban-lifnr      = ''.
*      gs_eban-banfn      = ''.
*      gs_eban-bnfpo      = ''.
*      gs_eban-bednr      = ''.
*      gs_eban-erdat+6(2) = '01'.
*      gs_ebansum              = gs_eban.
*
*      COLLECT gs_ebansum INTO gt_ebansum.
*
*    ENDLOOP.

      DATA(lt_eban_s) = gt_eban.

      SORT lt_eban_s BY matnr erdat(6) meins ASCENDING.

      DELETE ADJACENT DUPLICATES FROM lt_eban_s COMPARING matnr erdat(6) meins.

      gt_ebansum  = VALUE #( FOR s_eb IN lt_eban_s
                             ( matnr = s_eb-matnr
                               meins = s_eb-meins
                               erdat = |{ s_eb-erdat(6) }01|
                               menge = REDUCE #( INIT b_m TYPE menge_d FOR s_ebg IN gt_eban WHERE (  matnr    = s_eb-matnr
                                                                                              AND    erdat(6) = s_eb-erdat(6)
                                                                                              AND    meins    = s_eb-meins )
                                                 NEXT b_m = b_m + s_ebg-menge )
                             )
                           ).

      SORT gt_ebansum BY matnr meins.

      CLEAR: gt_matmeins, gt_matmeins[], gs_matmeins.
*    LOOP AT gt_ebansum INTO gs_ebansum.
*
*      gs_matmeins-matnr = gs_ebansum-matnr.
*      gs_matmeins-meins = gs_ebansum-meins.
*      COLLECT gs_matmeins INTO gt_matmeins.
*
*    ENDLOOP.

      gt_matmeins = VALUE #( FOR l_bs IN gt_ebansum
                              ( matnr = l_bs-matnr
                                meins = l_bs-meins )
                           ).

      DELETE ADJACENT DUPLICATES FROM gt_matmeins COMPARING matnr meins.

      CLEAR: gt_ebanmessem, gt_ebanmessem[], gs_ebanmessem.
*    LOOP AT gt_matmeins INTO gs_matmeins.
*
*      gs_ebanmessem-matnr = gs_ebansum-matnr.
*      gs_ebanmessem-meins = gs_ebansum-meins.
*      gs_ebanmessem-menge = gs_ebansum-menge.
*
*      COLLECT gs_ebanmessem INTO gt_ebanmessem.
*
*    ENDLOOP.

      gt_ebanmessem = VALUE #( FOR s_eb IN gt_ebansum
                              ( matnr = s_eb-matnr
                                meins = s_eb-meins
                                menge = s_eb-menge )
                             ).

    ENDIF.

*   APPC - MM.577809 – Ericky Fernandes - 05/06/2024 - Início
    "Busca os dados do pedido da loja determinante
    SELECT ekpo~ebeln,
           ekpo~ebelp,
           ekpo~matnr,
           ekpo~werks,
           ekko~lifnr,
           ekko~aedat,
           ekko~knumv,
           ekpo~bednr,
           ekpo~menge,
           ekpo~netpr,
           ekpo~meins
      FROM ekko
      INNER JOIN ekpo
      ON ekpo~ebeln = ekko~ebeln
      WHERE ekpo~matnr  IN @lr_matnr_aux
      AND werks         IN @s_cd
      AND ekko~bstyp    EQ 'F'
      AND ekko~bsart    IN @lr_bsart
      AND ekko~lifnr    IN @s_lifnr
      AND ekko~memory   EQ @space
      AND ekko~procstat NE '03'
      AND ekpo~loekz    EQ @space
      INTO TABLE @DATA(lt_ekko_cedente).

    IF sy-subrc = 0.
      "Busca os dados de divisões do programa de remessas
      SELECT eket~ebeln,
             eket~ebelp,
             eket~etenr,
             eket~menge,
             eket~wemng,
             ekpo~matnr,
             ekpo~meins
        FROM eket
        INNER JOIN ekpo
        ON  ekpo~ebeln = eket~ebeln
        AND ekpo~ebelp = eket~ebelp
        FOR ALL ENTRIES IN @lt_ekko_cedente
        WHERE eket~ebeln EQ @lt_ekko_cedente-ebeln
        AND eket~ebelp   EQ @lt_ekko_cedente-ebelp
        AND ekpo~loekz   EQ @space
        AND ekpo~elikz   EQ @space
        INTO TABLE @DATA(lt_eket_cedente).

      IF sy-subrc = 0.

        DATA(lt_eket_aux)    = lt_eket_cedente.
        DATA(lt_eketsum_aux) = gt_eketsum.

        SORT lt_eket_aux BY matnr ASCENDING.

        DELETE ADJACENT DUPLICATES FROM lt_eket_aux COMPARING matnr.

        lt_eketsum_aux = VALUE #( FOR s_eket_aux IN lt_eket_aux
                                ( matnr = s_eket_aux-matnr
                                  menge = REDUCE #( INIT b_m TYPE menge_d FOR s_eksum_aux IN lt_eket_cedente
                                           WHERE  ( matnr = s_eket_aux-matnr )
                                                    NEXT b_m = b_m + s_eksum_aux-menge )
                                  wemng = REDUCE #( INIT b_w TYPE weemg FOR s_eksum_aux IN lt_eket_cedente
                                           WHERE  ( matnr = s_eket_aux-matnr )
                                                    NEXT b_w = b_w + s_eksum_aux-wemng )
                                meins = s_eket_aux-meins ) ).

      ENDIF.
    ENDIF.
*   APPC - MM.577809 – Ericky Fernandes - 05/06/2024 - Fim

    SELECT ekpo~ebeln
           ekpo~ebelp
           ekpo~matnr
           ekpo~werks
           ekko~lifnr
           ekko~aedat
           ekko~knumv
           ekpo~bednr
           ekpo~menge
           ekpo~netpr
           ekpo~meins
      INTO CORRESPONDING FIELDS OF TABLE gt_ekko
      FROM ekko
      INNER JOIN ekpo
             ON ( ekpo~ebeln = ekko~ebeln )
      WHERE ekpo~matnr    IN lr_matnr_aux
        AND werks         IN s_cddet
        AND ekko~bstyp    EQ 'F'
        AND ekko~bsart    IN lr_bsart
        AND ekko~lifnr    IN s_lifnr
        AND ekko~memory   EQ ' '
        AND ekko~procstat NE '03'
        AND ekpo~loekz    EQ ' '.

    SORT gt_ekko BY ebeln ebelp.

    CLEAR: gt_ekkosum, gt_ekkosum[], gs_ekkosum.
*    LOOP AT gt_ekko INTO gs_ekko WHERE elikz EQ ' '.
*
*      "Sumarizar registros por mes
*      gs_ekko-werks      = ''.
*      gs_ekko-lifnr      = ''.
*      gs_ekko-ebeln      = ''.
*      gs_ekko-ebelp      = ''.
*      gs_ekko-bednr      = ''.
**      gs_ekko-aedat+6(2) = '01'.
*      gs_ekko-aedat      = ''.
**      gs_ekko-meins      = ''.
*      gs_ekkosum              = gs_ekko.
*
*      COLLECT gs_ekkosum INTO gt_ekkosum.
*
*    ENDLOOP.

    IF gt_ekko IS NOT INITIAL.

      DATA(lt_ekko) = gt_ekko.

      SORT lt_ekko BY matnr knumv meins elikz ASCENDING.

      DELETE ADJACENT DUPLICATES FROM lt_ekko COMPARING matnr knumv meins elikz.

      gt_ekkosum = VALUE #( FOR s_ekko IN lt_ekko
                              ( matnr = s_ekko-matnr
                                knumv = s_ekko-knumv
                                meins = s_ekko-meins
                                elikz = s_ekko-elikz
                                menge = REDUCE #( INIT b_m TYPE menge_d FOR s_ekkog IN gt_ekko
                                                                                    WHERE ( matnr = s_ekko-matnr
                                                                                      AND   knumv = s_ekko-knumv
                                                                                      AND   meins = s_ekko-meins
                                                                                      AND   elikz = s_ekko-elikz )
                                                                                    NEXT b_m = b_m + s_ekkog-menge
                                                )
                                netpr = REDUCE #( INIT b_n TYPE bprei FOR s_ekkog IN gt_ekko
                                                                                  WHERE ( matnr = s_ekko-matnr
                                                                                    AND   knumv = s_ekko-knumv
                                                                                    AND   meins = s_ekko-meins
                                                                                    AND   elikz = s_ekko-elikz )
                                                                                  NEXT b_n = b_n + s_ekkog-netpr
                                                )

                              )

                         ).

      SORT gt_ekkosum BY matnr meins.

      CLEAR: gt_matmeins, gt_matmeins[], gs_matmeins.
*    LOOP AT gt_ekkosum INTO gs_ekkosum.
*
*      gs_matmeins-matnr = gs_ekkosum-matnr.
*      gs_matmeins-meins = gs_ekkosum-meins.
*      COLLECT gs_matmeins INTO gt_matmeins.
*
*    ENDLOOP.

      gt_matmeins = VALUE #( FOR s_sum IN gt_ekkosum
                                ( matnr = s_sum-matnr
                                  meins = s_sum-meins
                                 )
                           ).
      SORT gt_matmeins BY matnr meins ASCENDING.

      DELETE ADJACENT DUPLICATES FROM gt_matmeins COMPARING matnr meins.

      CLEAR: gt_ekkomesmat, gt_ekkomesmat[], gs_ekkomesmat.
*    LOOP AT gt_ekkosum INTO gs_ekkosum.
*
*      gs_ekkomesmat-matnr = gs_ekkosum-matnr.
*      gs_ekkomesmat-meins = gs_ekkosum-meins.
*      gs_ekkomesmat-menge = gs_ekkosum-menge.
*
*      COLLECT gs_ekkomesmat INTO gt_ekkomesmat.
*
*    ENDLOOP.

      gt_ekkomesmat = VALUE #( FOR s_sum IN gt_ekkosum
                              ( matnr = s_sum-matnr
                                meins = s_sum-meins
                                menge = s_sum-menge
                               )
       ).

      SORT gt_ekkomesmat BY matnr meins ASCENDING.

      DELETE ADJACENT DUPLICATES FROM gt_ekkomesmat COMPARING matnr meins.

      "EKET PARA MATERIAIS
      SELECT eket~ebeln
             eket~ebelp
             eket~etenr
             eket~menge
             eket~wemng
             ekpo~matnr
             ekpo~meins
        INTO CORRESPONDING FIELDS OF TABLE gt_eket
        FROM eket
        INNER JOIN ekpo
               ON ( ekpo~ebeln = eket~ebeln
               AND  ekpo~ebelp = eket~ebelp )
        FOR ALL ENTRIES IN gt_ekko
        WHERE eket~ebeln EQ gt_ekko-ebeln
          AND eket~ebelp EQ gt_ekko-ebelp
          AND ekpo~loekz EQ ''
          AND ekpo~elikz EQ ''.

      CLEAR: gt_eketsum, gt_eketsum[], gs_eketsum.
*      LOOP AT gt_eket INTO gs_eket.
*
*        "Sumarizar registros por mes
*        gs_eket-ebeln      = ''.
*        gs_eket-ebelp      = ''.
*        gs_eket-etenr      = ''.
*        gs_eketsum         = gs_eket.
*
*        COLLECT gs_eketsum INTO gt_eketsum.
*
*      ENDLOOP.

      IF gt_eket IS NOT INITIAL.

        DATA(lt_eket) = gt_eket.

        SORT lt_eket BY matnr ASCENDING.

        DELETE ADJACENT DUPLICATES FROM lt_eket COMPARING matnr.

        gt_eketsum = VALUE #( FOR s_eket IN lt_eket
                              ( matnr = s_eket-matnr
                                menge = REDUCE #( INIT b_m TYPE menge_d FOR s_eksum IN gt_eket
                                                                          WHERE ( matnr = s_eket-matnr )
                                                                          NEXT b_m = b_m + s_eksum-menge )
                                wemng = REDUCE #( INIT b_w TYPE weemg   FOR s_eksum IN gt_eket
                                                                          WHERE ( matnr = s_eket-matnr )
                                                                          NEXT b_w = b_w + s_eksum-wemng )
                                meins = s_eket-meins
                              )
                            ).

      ENDIF.

    ENDIF.

    CLEAR gt_prcd_elements.
    IF gt_ekko IS NOT INITIAL.
      SELECT knumv kposn kschl kbetr kwert kpein INTO TABLE gt_prcd_elements
        FROM prcd_elements
        FOR ALL ENTRIES IN gt_ekko
        WHERE knumv EQ gt_ekko-knumv
          AND kschl IN ('PBXX','PB00').

      SORT gt_prcd_elements BY knumv ASCENDING kposn ASCENDING kschl ASCENDING kbetr DESCENDING.
      DELETE gt_prcd_elements WHERE kbetr EQ 0.
    ENDIF.

    IF gt_matsem IS NOT INITIAL.

      "EKKO/EKPO PARA SEMELHANTES
*      SELECT
*             ekpo~ebeln
*             ekpo~ebelp
*             ekpo~matnr
*             ekpo~werks
*             ekko~lifnr
*             ekko~aedat
*             ekko~knumv
*             ekpo~bednr
*             ekpo~menge
*             ekpo~netpr
*             ekpo~meins
*        INTO CORRESPONDING FIELDS OF TABLE gt_ekkos
*        FROM ekko
*        INNER JOIN ekpo
*               ON ( ekpo~ebeln = ekko~ebeln )
*        FOR ALL ENTRIES IN gt_matsem
*        WHERE ekpo~matnr EQ gt_matsem-matnr
*          AND werks IN r_werks
*          AND ekko~bstyp EQ 'F'
**          AND ekpo~elikz EQ ' '
*          AND ekpo~loekz EQ ' '.
      SELECT
           ekpo~ebeln
           ekpo~ebelp
           ekpo~matnr
           ekpo~werks
           ekko~lifnr
           ekko~aedat
           ekko~knumv
           ekpo~bednr
           ekpo~menge
           ekpo~netpr
           ekpo~meins
      INTO CORRESPONDING FIELDS OF TABLE gt_ekkos
      FROM ekko
      INNER JOIN ekpo
             ON ( ekpo~ebeln = ekko~ebeln )
      WHERE ekpo~matnr IN lr_matnr
        AND werks      IN r_werks
        AND ekko~bstyp EQ 'F'
        AND ekpo~loekz EQ ' '.

      SORT gt_ekkos BY ebeln ebelp.

    ENDIF.

    CLEAR: gt_ekkosum, gt_ekkosum[], gs_ekkosum.
*    LOOP AT gt_ekkos INTO gs_ekkos WHERE elikz EQ ' '.
*
*      "Sumarizar registros por mes
*      gs_ekkos-werks      = ''.
*      gs_ekkos-lifnr      = ''.
*      gs_ekkos-ebeln      = ''.
*      gs_ekkos-ebelp      = ''.
*      gs_ekkos-bednr      = ''.
**      gs_ekkos-aedat+6(2) = '01'.
*      gs_ekkos-aedat      = ''.
**      gs_ekkos-meins      = ''.
*      gs_ekkosum              = gs_ekkos.
*
*      COLLECT gs_ekkosum INTO gt_ekkosum.
*
*    ENDLOOP.

    IF gt_ekkos IS NOT INITIAL.

      DATA(lt_ekkos) = gt_ekkos.

      SORT lt_ekkos BY matnr knumv meins elikz.

      DELETE ADJACENT DUPLICATES FROM lt_ekkos COMPARING matnr knumv meins elikz.

      gt_ekkosum = VALUE #( FOR s_ekkos IN lt_ekkos WHERE ( elikz = ' ' )
                            ( matnr = s_ekkos-matnr
                              knumv = s_ekkos-knumv
                              meins = s_ekkos-meins
                              elikz = s_ekkos-elikz
                              menge = REDUCE #( INIT b_m TYPE menge_d FOR s_ekssum IN gt_ekkos
                                                                          WHERE ( matnr = s_ekkos-matnr
                                                                            AND   knumv = s_ekkos-knumv
                                                                            AND   meins = s_ekkos-meins
                                                                            AND   elikz = ' ' )
                                                                          NEXT b_m = b_m + s_ekssum-menge )
                              netpr = REDUCE #( INIT b_n TYPE bprei   FOR s_ekssum IN gt_ekkos
                                                                          WHERE ( matnr = s_ekkos-matnr
                                                                            AND   knumv = s_ekkos-knumv
                                                                            AND   meins = s_ekkos-meins
                                                                            AND   elikz = ' ' )
                                                                          NEXT b_n = b_n + s_ekssum-netpr )
                            )
                          ).

      SORT gt_ekkosum BY matnr meins.

      CLEAR: gt_matmeins, gt_matmeins[], gs_matmeins.
*    LOOP AT gt_ekkosum INTO gs_ekkosum.
*
*      gs_matmeins-matnr = gs_ekkosum-matnr.
*      gs_matmeins-meins = gs_ekkosum-meins.
*      COLLECT gs_matmeins INTO gt_matmeins.
*
*    ENDLOOP.

      gt_matmeins = VALUE #( FOR s_ekksum IN gt_ekkosum
                          ( matnr = s_ekksum-matnr
                            meins = s_ekksum-meins )
                        ).

      SORT gt_matmeins BY matnr meins.

      DELETE ADJACENT DUPLICATES FROM gt_matmeins COMPARING matnr meins.

      CLEAR: gt_ekkomessem, gt_ekkomessem[], gs_ekkomessem.
*    LOOP AT gt_ekkosum INTO gs_ekkosum.
*
*      gs_ekkomessem-matnr = gs_ekkosum-matnr.
*      gs_ekkomessem-meins = gs_ekkosum-meins.
*      gs_ekkomessem-menge = gs_ekkosum-menge.
*
*      COLLECT gs_ekkomessem INTO gt_ekkomessem.
*
*    ENDLOOP.

      gt_ekkomessem = VALUE #( FOR s_ekksum IN gt_ekkosum
                               ( matnr = gs_ekkosum-matnr
                                 meins = gs_ekkosum-meins
                                 menge = gs_ekkosum-menge )
                             ).

      DELETE ADJACENT DUPLICATES FROM gt_ekkomessem COMPARING matnr meins.

      "EKET PARA SEMELHANTES
      SELECT eket~ebeln
             eket~ebelp
             eket~etenr
             eket~menge
             eket~wemng
             ekpo~matnr
             ekpo~meins
        INTO CORRESPONDING FIELDS OF TABLE gt_eket
        FROM eket
        INNER JOIN ekpo
               ON ( ekpo~ebeln = eket~ebeln
               AND  ekpo~ebelp = eket~ebelp )
        FOR ALL ENTRIES IN gt_ekkos
        WHERE eket~ebeln EQ gt_ekkos-ebeln
          AND eket~ebelp EQ gt_ekkos-ebelp
          AND ekpo~loekz EQ ''
          AND ekpo~elikz EQ ''.

      CLEAR: gt_eketsums, gt_eketsums[], gs_eketsums.
*      LOOP AT gt_eket INTO gs_eket.
*
*        "Sumarizar registros por mes
*        gs_eket-ebeln      = ''.
*        gs_eket-ebelp      = ''.
*        gs_eket-etenr      = ''.
*        gs_eketsums         = gs_eket.
*
*        COLLECT gs_eketsums INTO gt_eketsum.
*
*      ENDLOOP.

      IF gt_eket IS NOT INITIAL.

        DATA(lt_eket_s) = gt_eket.

        SORT lt_eket_s BY  matnr.

        DELETE ADJACENT DUPLICATES FROM lt_eket_s COMPARING matnr.

        gt_eketsum = VALUE #( FOR s_eket_s IN lt_eket_s
                        ( matnr = s_eket_s-matnr
                          menge = REDUCE #( INIT b_m TYPE menge_d FOR s_eksum_s IN gt_eket
                                                                    WHERE ( matnr = s_eket_s-matnr )
                                                                    NEXT b_m = b_m + s_eksum_s-menge )
                          wemng = REDUCE #( INIT b_w TYPE weemg   FOR s_eksum_s IN gt_eket
                                                                    WHERE ( matnr = s_eket_s-matnr )
                                                                    NEXT b_w = b_w + s_eksum_s-wemng )
                          meins = s_eket_s-meins
                        )
                      ).

      ENDIF.

    ENDIF.

    CLEAR gt_prcd_eless.
    IF gt_ekkos IS NOT INITIAL.
      SELECT knumv kposn kschl kbetr kwert kpein INTO TABLE gt_prcd_eless
        FROM prcd_elements
        FOR ALL ENTRIES IN gt_ekkos
        WHERE knumv EQ gt_ekkos-knumv
          AND kschl IN ('PBXX','PB00').

      SORT gt_prcd_eless BY knumv ASCENDING kposn ASCENDING kschl ASCENDING kbetr DESCENDING.
      DELETE gt_prcd_eless WHERE kbetr EQ 0.
    ENDIF.

    "MARD DOS CD'S PARA MATERIAIS
*    SELECT matnr
*           werks
*           lgort
*           labst
*      INTO CORRESPONDING FIELDS OF TABLE gt_mard
*      FROM mard
*      FOR ALL ENTRIES IN gt_material
*      WHERE matnr EQ gt_material-matnr
*        AND werks IN s_cd
*        AND lgort IN s_lgort
*        AND lvorm EQ ''
*        AND labst GT 0.
    SELECT matnr
           werks
           lgort
           labst
      INTO CORRESPONDING FIELDS OF TABLE gt_mard
      FROM mard
      WHERE matnr IN lr_matnr_aux
        AND werks IN s_cd
        AND lgort IN s_lgort
        AND lvorm EQ ''
        AND labst GT 0.

    CLEAR: gt_mardsum, gt_mardsum[], gs_mardsum.
*    LOOP AT gt_mard INTO gs_mard.
*
*      "Sumarizar registros
*      gs_mard-lgort = ''.
*      gs_mardsum    = gs_mard.
*
*      COLLECT gs_mardsum INTO gt_mardsum.
*
*    ENDLOOP.

    IF gt_mard IS NOT INITIAL.

      DATA(lt_mard) = gt_mard.

      SORT lt_mard BY matnr werks.

      DELETE ADJACENT DUPLICATES FROM lt_mard COMPARING matnr werks.

      gt_mardsum = VALUE #( FOR s_mr IN lt_mard
                            ( matnr = s_mr-matnr
                              werks = s_mr-werks
                              labst = REDUCE #( INIT b_l TYPE labst FOR s_mrd IN gt_mard
                                                                    WHERE ( matnr = s_mr-matnr
                                                                      AND   werks = s_mr-werks )
                                                                    NEXT b_l = b_l + s_mrd-labst )

                            )
                          ).

      SORT gt_mardsum BY matnr werks.

      gt_mardmat[] = gt_mardsum[].

    ENDIF.

    IF gt_matsem IS NOT INITIAL.

      "MARD DOS CD'S PARA SEMELHANTES
*      SELECT matnr
*             werks
*             lgort
*             labst
*        INTO CORRESPONDING FIELDS OF TABLE gt_mard
*        FROM mard
*        FOR ALL ENTRIES IN gt_matsem
*        WHERE matnr EQ gt_matsem-matnr
*          AND werks IN s_cd
*          AND lgort IN s_lgort
*          AND lvorm EQ ''
*          AND labst GT 0.
      SELECT matnr
             werks
             lgort
             labst
        INTO CORRESPONDING FIELDS OF TABLE gt_mard
        FROM mard
        WHERE matnr IN lr_matnr
          AND werks IN s_cd
          AND lgort IN s_lgort
          AND lvorm EQ ''
          AND labst GT 0.

      CLEAR: gt_mardsum, gt_mardsum[], gs_mardsum.
*     LOOP AT gt_mard INTO gs_mard.
*
*       "Sumarizar registros
*       gs_mard-lgort = ''.
*       gs_mardsum    = gs_mard.
*
*       COLLECT gs_mardsum INTO gt_mardsum.
*
*     ENDLOOP.
      IF gt_mard  IS NOT INITIAL.

        DATA(lt_mard_s) = gt_mard.

        SORT lt_mard_s BY matnr werks.

        DELETE ADJACENT DUPLICATES FROM lt_mard_s COMPARING matnr werks.

        gt_mardsum = VALUE #( FOR s_mr IN lt_mard_s
                              ( matnr = s_mr-matnr
                                werks = s_mr-werks
                                labst = REDUCE #( INIT b_l TYPE labst FOR s_mrd IN gt_mard
                                                                      WHERE ( matnr = s_mr-matnr
                                                                        AND   werks = s_mr-werks )
                                                                      NEXT b_l = b_l + s_mrd-labst )

                              )
                            ).

        SORT gt_mardsum BY matnr werks.

        gt_mardsem[] = gt_mardsum[].

      ENDIF.

    ENDIF.

    "MARD DOS CD'S PARA MATERIAIS CONF
*    SELECT matnr
*           werks
*           lgort
*           labst
*      INTO CORRESPONDING FIELDS OF TABLE gt_mard
*      FROM mard
*      FOR ALL ENTRIES IN gt_material
*      WHERE matnr EQ gt_material-matnr
*        AND werks IN s_cd
*        AND lgort IN r_lgort_conf
*        AND lvorm EQ ''
*        AND labst GT 0.
    SELECT matnr
           werks
           lgort
           labst
      INTO CORRESPONDING FIELDS OF TABLE gt_mard
      FROM mard
      WHERE matnr IN lr_matnr_aux
        AND werks IN s_cd
        AND lgort IN r_lgort_conf
        AND lvorm EQ ''
        AND labst GT 0.

    CLEAR: gt_mardsum, gt_mardsum[], gs_mardsum.
*    LOOP AT gt_mard INTO gs_mard.
*
*      "Sumarizar registros
*      gs_mard-lgort = ''.
*      gs_mardsum    = gs_mard.
*
*      COLLECT gs_mardsum INTO gt_mardsum.
*
*    ENDLOOP.
    IF gt_mard IS NOT INITIAL.

      DATA(lt_mard_cd) = gt_mard.

      SORT lt_mard_cd BY matnr werks.

      DELETE ADJACENT DUPLICATES FROM lt_mard_cd COMPARING matnr werks.

      gt_mardsum = VALUE #( FOR s_mr IN lt_mard_cd
                            ( matnr = s_mr-matnr
                              werks = s_mr-werks
                              labst = REDUCE #( INIT b_l TYPE labst FOR s_mrd IN gt_mard
                                                                    WHERE ( matnr = s_mr-matnr
                                                                      AND   werks = s_mr-werks )
                                                                    NEXT b_l = b_l + s_mrd-labst )

                            )
                          ).

      SORT gt_mardsum BY matnr werks.

      gt_mardcdmat[] = gt_mardsum[].

    ENDIF.

    IF gt_matsem IS NOT INITIAL.

      "MARD DOS CD'S PARA SEMELHANTES CONF
*      SELECT matnr
*             werks
*             lgort
*             labst
*        INTO CORRESPONDING FIELDS OF TABLE gt_mard
*        FROM mard
*        FOR ALL ENTRIES IN gt_matsem
*        WHERE matnr EQ gt_matsem-matnr
*          AND werks IN s_cd
*          AND lgort IN r_lgort_conf
*          AND lvorm EQ ''
*          AND labst GT 0.
      SELECT matnr
             werks
             lgort
             labst
        INTO CORRESPONDING FIELDS OF TABLE gt_mard
        FROM mard
        WHERE matnr IN lr_matnr
          AND werks IN s_cd
          AND lgort IN r_lgort_conf
          AND lvorm EQ ''
          AND labst GT 0.

      CLEAR: gt_mardsum, gt_mardsum[], gs_mardsum.
*    LOOP AT gt_mard INTO gs_mard.
*
*      "Sumarizar registros
*      gs_mard-lgort = ''.
*      gs_mardsum    = gs_mard.
*
*      COLLECT gs_mardsum INTO gt_mardsum.
*
*    ENDLOOP.

      IF gt_mard IS NOT INITIAL.

        DATA(lt_mard_csc) = gt_mard.

        SORT lt_mard_csc BY matnr werks.

        DELETE ADJACENT DUPLICATES FROM lt_mard_csc COMPARING matnr werks.

        gt_mardsum = VALUE #( FOR s_mr IN lt_mard_csc
                              ( matnr = s_mr-matnr
                                werks = s_mr-werks
                                labst = REDUCE #( INIT b_l TYPE labst FOR s_mrd IN gt_mard
                                                                      WHERE ( matnr = s_mr-matnr
                                                                        AND   werks = s_mr-werks )
                                                                      NEXT b_l = b_l + s_mrd-labst )

                              )
                            ).

        SORT gt_mardsum BY matnr werks.

        gt_mardcdsem[] = gt_mardsum[].

      ENDIF.

    ENDIF.

    "MARD DAS LOJAS PARA MATERIAIS
*    SELECT matnr
*           werks
*           lgort
*           labst
*      INTO CORRESPONDING FIELDS OF TABLE gt_mard
*      FROM mard
*      FOR ALL ENTRIES IN gt_material
*      WHERE matnr EQ gt_material-matnr
*        AND werks IN s_lojas
*        AND lgort IN s_lgort
*        AND lvorm EQ ''
*        AND labst GT 0.

    SELECT matnr
           werks
           lgort
           labst
      INTO CORRESPONDING FIELDS OF TABLE gt_mard
      FROM mard
      WHERE matnr IN lr_matnr_aux
        AND werks IN s_lojas
        AND lgort IN s_lgort
        AND lvorm EQ ''
        AND labst GT 0.

    CLEAR: gt_mardsum, gt_mardsum[], gs_mardsum.
*    LOOP AT gt_mard INTO gs_mard.
*
*      "Sumarizar registros
*      gs_mard-werks = ''.
*      gs_mard-lgort = ''.
*      gs_mardsum    = gs_mard.
*
*      COLLECT gs_mardsum INTO gt_mardsum.
*
*    ENDLOOP.

    IF gt_mard IS NOT INITIAL.

      DATA(lt_mard_lj) = gt_mard.

      SORT lt_mard_lj BY matnr.

      DELETE ADJACENT DUPLICATES FROM lt_mard_lj COMPARING matnr.

      gt_mardsum = VALUE #( FOR s_mr IN lt_mard_lj
                            ( matnr = s_mr-matnr
                              labst = REDUCE #( INIT b_l TYPE labst FOR s_mrd IN gt_mard
                                                                    WHERE ( matnr = s_mr-matnr )
                                                                    NEXT b_l = b_l + s_mrd-labst )
                            )
                          ).

      SORT gt_mardsum BY matnr werks.

      gt_mardljmat[] = gt_mardsum[].

    ENDIF.

    "MARD DOS CENTROS PARA MATERIAIS
*    SELECT matnr
*           werks
*           lgort
*           labst
*      INTO CORRESPONDING FIELDS OF TABLE gt_mard
*      FROM mard
*      FOR ALL ENTRIES IN gt_material
*      WHERE matnr EQ gt_material-matnr
*        AND werks IN r_werks
*        AND lgort IN r_lgort
*        AND lvorm EQ ''
*        AND labst GT 0.

    SELECT matnr
           werks
           lgort
           labst
      INTO CORRESPONDING FIELDS OF TABLE gt_mard
      FROM mard
      WHERE matnr IN lr_matnr_aux
        AND werks IN r_werks
        AND lgort IN r_lgort
        AND lvorm EQ ''
        AND labst GT 0.

    CLEAR: gt_mardsum, gt_mardsum[], gs_mardsum.
*    LOOP AT gt_mard INTO gs_mard.
*
*      "Sumarizar registros
*      gs_mard-werks = ''.
*      gs_mard-lgort = ''.
*      gs_mardsum    = gs_mard.
*
*      COLLECT gs_mardsum INTO gt_mardsum.
*
*    ENDLOOP.

    IF gt_mard IS NOT INITIAL.

      DATA(lt_mard_cn) = gt_mard.

      SORT lt_mard_cn BY matnr.

      DELETE ADJACENT DUPLICATES FROM lt_mard_cn COMPARING matnr.

      gt_mardsum = VALUE #( FOR s_mr IN lt_mard_cn
                            ( matnr = s_mr-matnr
                              labst = REDUCE #( INIT b_l TYPE labst FOR s_mrd IN gt_mard
                                                                    WHERE ( matnr = s_mr-matnr )
                                                                    NEXT b_l = b_l + s_mrd-labst )
                            )
                          ).


      SORT gt_mardsum BY matnr werks.

      gt_mardljmat_c[] = gt_mardsum[].

    ENDIF.

    IF gt_matsem IS NOT INITIAL.

      "MARD DAS LOJAS PARA SEMELHANTES
*      SELECT matnr
*             werks
*             lgort
*             labst
*        INTO CORRESPONDING FIELDS OF TABLE gt_mard
*        FROM mard
*        FOR ALL ENTRIES IN gt_matsem
*        WHERE matnr EQ gt_matsem-matnr
**          AND werks EQ gt_matsem-werks
*          AND werks IN s_lojas
*          AND lgort IN s_lgort
*          AND lvorm EQ ''
*          AND labst GT 0.

      SELECT matnr
             werks
             lgort
             labst
        INTO CORRESPONDING FIELDS OF TABLE gt_mard
        FROM mard
        WHERE matnr IN lr_matnr
          AND werks IN s_lojas
          AND lgort IN s_lgort
          AND lvorm EQ ''
          AND labst GT 0.


      CLEAR: gt_mardsum, gt_mardsum[], gs_mardsum.
*    LOOP AT gt_mard INTO gs_mard.
*
*      "Sumarizar registros
*      gs_mard-lgort = ''.
*      gs_mard-werks = ''.
*      gs_mardsum    = gs_mard.
*
*      COLLECT gs_mardsum INTO gt_mardsum.
*
*    ENDLOOP.
      IF gt_mard IS NOT INITIAL.

        DATA(lt_mard_js) = gt_mard.

        SORT lt_mard_js BY matnr.

        DELETE ADJACENT DUPLICATES FROM lt_mard_js COMPARING matnr.

        gt_mardsum = VALUE #( FOR s_mr IN lt_mard_js
                              ( matnr = s_mr-matnr
                                labst = REDUCE #( INIT b_l TYPE labst FOR s_mrd IN gt_mard
                                                                      WHERE ( matnr = s_mr-matnr )
                                                                      NEXT b_l = b_l + s_mrd-labst )
                              )
                            ).


        SORT gt_mardsum BY matnr werks.

        gt_mardljsem[] = gt_mardsum[].

      ENDIF.

    ENDIF.

    IF gt_matsem IS NOT INITIAL.

      "MARD DOS CENTROS PARA SEMELHANTES
*      SELECT matnr
*             werks
*             lgort
*             labst
*        INTO CORRESPONDING FIELDS OF TABLE gt_mard
*        FROM mard
*        FOR ALL ENTRIES IN gt_matsem
*        WHERE matnr EQ gt_matsem-matnr
*          AND werks IN r_werks
*          AND lgort IN r_lgort
*          AND lvorm EQ ''
*          AND labst GT 0.

      SELECT matnr
            werks
            lgort
            labst
       INTO CORRESPONDING FIELDS OF TABLE gt_mard
       FROM mard
       WHERE matnr IN lr_matnr
         AND werks IN r_werks
         AND lgort IN r_lgort
         AND lvorm EQ ''
         AND labst GT 0.

      CLEAR: gt_mardsum, gt_mardsum[], gs_mardsum.
*    LOOP AT gt_mard INTO gs_mard.
*
*      "Sumarizar registros
*      gs_mard-lgort = ''.
*      gs_mard-werks = ''.
*      gs_mardsum    = gs_mard.
*
*      COLLECT gs_mardsum INTO gt_mardsum.
*
*    ENDLOOP.
      IF gt_mard IS NOT INITIAL.

        DATA(lt_mard_cs) = gt_mard.

        SORT lt_mard_cs BY matnr.

        DELETE ADJACENT DUPLICATES FROM lt_mard_cs COMPARING matnr.

        gt_mardsum = VALUE #( FOR s_mr IN lt_mard_cs
                              ( matnr = s_mr-matnr
                                labst = REDUCE #( INIT b_l TYPE labst FOR s_mrd IN gt_mard
                                                                      WHERE ( matnr = s_mr-matnr )
                                                                      NEXT b_l = b_l + s_mrd-labst )
                              )
                            ).

        SORT gt_mardsum BY matnr werks.

        gt_mardljsem_c[] = gt_mardsum[].

      ENDIF.

    ENDIF.

*** GFX - DFLC - Início - 16/03/2020-  Busca dados bstme e umrez para  tabela de saída
    SELECT  a~matnr
            a~bstme
            b~umrez
      FROM mara AS a
      INNER JOIN marm AS b ON ( a~matnr = b~matnr
                           AND  a~bstme = b~meinh )
      INTO TABLE gt_bstumrez
      WHERE a~matnr IN lr_matnr_aux.

    "Caso haja semelhante
    IF gt_matsem IS NOT INITIAL.

      DATA(lt_bstm) = gt_bstumrez.

      CLEAR lt_bstm.

      SELECT  a~matnr
          a~bstme
          b~umrez
        FROM mara AS a
        INNER JOIN marm AS b ON ( a~matnr = b~matnr
                             AND  a~bstme = b~meinh )
        INTO TABLE lt_bstm
        WHERE a~matnr IN lr_matnr.

      IF lt_bstm IS NOT INITIAL.

        APPEND LINES OF lt_bstm TO gt_bstumrez.

      ENDIF.

    ENDIF.

    DELETE ADJACENT DUPLICATES FROM gt_bstumrez COMPARING matnr.

*** GFX - DFLC - Fim 16/03/2020

    SELECT * INTO TABLE gt_t141t
      FROM t141t
      WHERE spras EQ sy-langu.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 60
        text       = 'Realizando a montagem dos dados'.

*    LOOP AT gt_consumomat INTO gs_consumomat.
*
*      CLEAR gs_material.
*      READ TABLE gt_material INTO gs_material WITH KEY matnr = gs_consumomat-matnr.
*
*      gs_consumomat-maktx      = gs_material-maktx.
*
*      MODIFY gt_consumomat FROM gs_consumomat.
*
*    ENDLOOP.

    lt_consumomat = VALUE #( FOR s_con IN gt_consumomat
                            ( matnr      = s_con-matnr
                              venda_mes1 = s_con-venda_mes1
                              venda_mes2 = s_con-venda_mes2
                              venda_mes3 = s_con-venda_mes3
                              venda_mes4 = s_con-venda_mes4
                              venda_mes5 = s_con-venda_mes5
                              venda_mes6 = s_con-venda_mes6
                              maktx      = COND #( WHEN line_exists( gt_material[ matnr = s_con-matnr ] )
                                                    THEN gt_material[ matnr = s_con-matnr ]-maktx )
                            )
                           ).

    gt_consumomat = lt_consumomat.

    SORT gt_consumomat BY maktx.

*    LOOP AT gt_semelhante INTO gs_semelhante.
*
*      CLEAR gs_matsem.
*      READ TABLE gt_matsem INTO gs_matsem WITH KEY matnr = gs_semelhante-matnr.
*      IF sy-subrc NE 0.
*        READ TABLE gt_matsem_aux INTO gs_matsem WITH KEY matnr = gs_semelhante-matnr.
*      ENDIF.
*
*      gs_semelhante-maktx      = gs_matsem-maktx.
*
*      MODIFY gt_semelhante FROM gs_semelhante.
*
*    ENDLOOP.

    IF gt_semelhante IS NOT INITIAL.

      DATA(lt_semelhante) = gt_semelhante.

      lt_semelhante = VALUE #(  FOR s_sem IN gt_semelhante
                                ( matnr  = s_sem-matnr
                                  bismt  = s_sem-bismt
                                  maktx  = COND #( WHEN line_exists( gt_matsem[ matnr = s_sem-matnr ] )
                                                    THEN gt_matsem[ matnr = s_sem-matnr ]-maktx
                                                   WHEN line_exists( gt_matsem_aux[ matnr = s_sem-matnr ] )
                                                    THEN gt_matsem_aux[ matnr = s_sem-matnr ]-maktx )
                                )
                             ).

      gt_semelhante = lt_semelhante.

      SORT gt_semelhante BY bismt maktx.

    ENDIF.

    CLEAR: gs_zmmt007, gt_zmmt007, gt_zmmt007[].
    gs_zmmt007-refrel        = p_refrel.
    gs_zmmt007-cd1           = gv_cd1.
    gs_zmmt007-cd2           = gv_cd2.
    gs_zmmt007-cd3           = gv_cd3.
    gs_zmmt007-lojas         = gv_lojas.
    gs_zmmt007-cddet         = s_cddet-low.
    gs_zmmt007-mmedia        = gv_mm_media.
    gs_zmmt007-ncmedia       = gv_nc_media.
    gs_zmmt007-delecao       = ''.
    gs_zmmt007-obs           = p_obs.
    gs_zmmt007-lifnr         = gs_lfa1-lifnr.
    gs_zmmt007-name1         = gs_lfa1-name1.
    gs_zmmt007-lgort         = gv_lgort.
    gs_zmmt007-ekgrp         = gv_ekgrp.
    gs_zmmt007-matkl         = gv_matkl.
    gs_zmmt007-prdha         = gv_prdha.
    gs_zmmt007-matnr         = gv_matnr.
    gs_zmmt007-bismt         = gv_bismt.
    gs_zmmt007-range_vnd_ini = s_rgvnd-low.
    gs_zmmt007-range_vnd_fim = s_rgvnd-high.
    gs_zmmt007-nec_compra    = s_necom-low.
    gs_zmmt007-hist_bv       = p_hist.
    gs_zmmt007-real_bv       = p_real.
    gs_zmmt007-pc            = p_pc.
    gs_zmmt007-rc            = p_rc.
    gs_zmmt007-sasem         = p_sasem.
    gs_zmmt007-pcsem         = p_pcsem.
    gs_zmmt007-tr            = rb_tr.
    gs_zmmt007-ap            = rb_ap.
    gs_zmmt007-ss            = rb_ss.
    gs_zmmt007-cs            = rb_cs.
    gs_zmmt007-uname         = sy-uname.
    gs_zmmt007-udate         = sy-datum.
    gs_zmmt007-utime         = sy-uzeit.

    APPEND gs_zmmt007 TO gt_zmmt007.

    SORT gt_ekko  BY matnr aedat DESCENDING ebeln DESCENDING.
    SORT gt_ekkos BY matnr aedat DESCENDING ebeln DESCENDING.

******************************
*Deivison Ferreira
*Data: 21/10/2020
*Buscar o preço do pedido na lista de preço de acordo com o fornecedor/Hierarquia do material
*****************************
    IF gt_consumomat[] IS NOT INITIAL.
*Buscar dados de materiais/Hierarquia de produtos conforme seleção do programa
      SELECT matnr , prdha FROM mara INTO TABLE @DATA(ti_mara)
      FOR ALL ENTRIES IN @gt_consumomat
      WHERE matnr = @gt_consumomat-matnr.
      IF ti_mara[] IS NOT INITIAL.
        SELECT matnr, mtorg FROM mbew INTO TABLE @DATA(ti_mbew)
        FOR ALL ENTRIES IN @ti_mara
        WHERE matnr = @ti_mara-matnr.
        SORT ti_mbew BY matnr.
*Buscar todas as politicas de preço cadastradas para o fornecedor informado com suas respectivas hierarquias de produtos existentes na TI_MARA
        SORT ti_mara BY matnr.
        SELECT * FROM zmmt018 INTO TABLE @DATA(ti_zmmt018)
          FOR ALL ENTRIES IN @ti_mara
          WHERE burks = '1000' AND
                lifnr = @s_lifnr-low AND
                prdha = @ti_mara-prdha AND
                dt_inicio_vigencia LE @sy-datum AND
                dt_final_vigencia GE @sy-datum.

        IF ti_zmmt018[] IS NOT INITIAL.
          SORT ti_zmmt018 BY prdha.
*Buscar os registros da lista de preço para o fornecedor/Hierarquia encontrado
          SELECT * FROM zmmt022 INTO TABLE @DATA(ti_zmmt022)
            FOR ALL ENTRIES IN @gt_consumomat
            WHERE burks = '1000' AND
                  lifnr = @s_lifnr-low AND
                  matnr = @gt_consumomat-matnr.
          SORT ti_zmmt022 BY matnr.
        ENDIF.
      ENDIF.
    ENDIF.


    CLEAR: gt_zmmt008, gt_zmmt008[].
    CLEAR lv_cont.

*** GFX - JBRS - DEVK923960 - Início - 17/11/2021
    IF gt_consumomat IS NOT INITIAL OR
       gt_semelhante IS NOT INITIAL.

      "Ranger
      DATA: lr_werks_ekpo TYPE RANGE OF ekpo-werks.
*            lr_matnr_ekpo TYPE RANGE OF ekpo-matnr.

      "Objetos
      DATA: lo_stock_query TYPE REF TO zcl_stock_query.

      IF gt_consumomat IS NOT INITIAL.

        APPEND LINES OF s_cd    TO lr_werks_ekpo.
        APPEND LINES OF s_lojas TO lr_werks_ekpo.

        SORT lr_werks_ekpo BY low ASCENDING.
        DELETE ADJACENT DUPLICATES FROM lr_werks_ekpo COMPARING low.

*        lr_matnr = VALUE #( FOR <fs_consumomat> IN gt_consumomat ( sign   = 'I'
*                                                                   option = 'EQ'
*                                                                   low    = <fs_consumomat>-matnr ) ).
*        "Seleciona o estoque em trânsito
*        SELECT matnr
*              ,werks
*              ,SUM( menge ) AS menge
*              ,meins
*          FROM ekpo
*          INNER JOIN ekko
*            ON ( ekko~ebeln = ekpo~ebeln )
*          INTO TABLE @DATA(lt_ekpo_consumomat)
*          WHERE ekpo~matnr IN @lr_matnr
*            AND ekpo~werks IN @lr_werks_ekpo
*            AND ekpo~loekz = ''
*            AND ekpo~elikz = ''
*            AND ekko~bsart IN ( 'UB','UB2','ZUB','ZUB1','ZUB2' )
*          GROUP BY matnr, werks, meins.
*
*        FREE: lr_werks_ekpo, lr_matnr.
*
*        "Seleciona estoque pendente de venda
*        SELECT matnr
*              ,werks
*              ,SUM( omeng ) AS menge
*          FROM vbbe
*          INTO TABLE @DATA(lt_vbbe)
*          WHERE matnr IN @lr_matnr
*            AND werks IN @lr_werks_ekpo
*          GROUP BY matnr, werks.
        CREATE OBJECT lo_stock_query.

        lo_stock_query->get_stock_transfer(
          EXPORTING
            ir_matnr = VALUE #( FOR <fs_consumomat> IN gt_consumomat ( sign   = 'I'
                                                                       option = 'EQ'
                                                                       low    = <fs_consumomat>-matnr ) )
            ir_werks = lr_werks_ekpo
            ir_bsart = VALUE #( ( sign = 'I' option = 'EQ' low = 'UB'   )
                                ( sign = 'I' option = 'EQ' low = 'UB2'  )
                                ( sign = 'I' option = 'EQ' low = 'ZUB'  )
                                ( sign = 'I' option = 'EQ' low = 'ZUB1' )
                                ( sign = 'I' option = 'EQ' low = 'ZUB2' )  )
          IMPORTING
            et_ekpo  = DATA(lt_ekpo_consumomat)
            et_vbbe  = DATA(lt_vbbe) ).

      ENDIF.

    ENDIF.
*** GFX - JBRS - DEVK923960 - Fim - 17/11/2021

    LOOP AT gt_consumomat INTO gs_consumomat.

      ADD 1 TO lv_cont.
      CLEAR gs_material.

      TRY.
          gs_material = gt_material[ matnr = gs_consumomat-matnr ].

        CATCH cx_sy_itab_line_not_found.

      ENDTRY.

      TRY.
          gv_estcd1 = gt_mardcdmat[ matnr = gs_consumomat-matnr
                                         werks = gv_cd1 ]-labst.

        CATCH cx_sy_itab_line_not_found.

          CLEAR gv_estcd1.

      ENDTRY.

      TRY.

          gv_estcd2 = gt_mardcdmat[ matnr = gs_consumomat-matnr
                                       werks = gv_cd2 ]-labst.

        CATCH cx_sy_itab_line_not_found.

          CLEAR gv_estcd2.

      ENDTRY.

      TRY.

          gv_estcd3 = gt_mardcdmat[ matnr = gs_consumomat-matnr
                                       werks = gv_cd3 ]-labst.

        CATCH cx_sy_itab_line_not_found.

          CLEAR gv_estcd3.

      ENDTRY.

      CLEAR gs_mardljmat.
      TRY.
          gs_mardljmat = gt_mardljmat[ matnr = gs_consumomat-matnr ].

        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      CLEAR gs_mardljmat_c.
      TRY.
          gs_mardljmat_c = gt_mardljmat_c[ matnr = gs_consumomat-matnr ].

        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      CLEAR gs_ebanmesmat.
      TRY.
          gs_ebanmesmat = gt_ebanmesmat[ matnr = gs_consumomat-matnr ].

        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      CLEAR gs_eketsum.
      TRY.
          gs_eketsum = gt_eketsum[ matnr = gs_consumomat-matnr ].

        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      CLEAR gs_ekko.
      TRY.
          gs_ekko = gt_ekko[ matnr = gs_consumomat-matnr ].

        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      CLEAR gs_prcd_elements.
      TRY.
          gs_prcd_elements = gt_prcd_elements[ knumv = gs_ekko-knumv
                                               kposn = gs_ekko-ebelp
                                               kschl = 'PBXX' ].

        CATCH cx_sy_itab_line_not_found.

          TRY.
              gs_prcd_elements = gt_prcd_elements[ knumv = gs_ekko-knumv
                                                   kposn = gs_ekko-ebelp
                                                   kschl = 'PB00' ].

            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

      ENDTRY.

      CLEAR gs_t141t.
      TRY.
          gs_t141t = gt_t141t[ mmsta = gs_material-mstae ].

        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      CLEAR: gs_marm.
      LOOP AT gt_marm INTO gs_marm WHERE matnr EQ gs_material-matnr.
        IF gs_marm-meinh NE gs_material-meins.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF gs_marm-umrez IS INITIAL.
        gs_marm-umrez = 1.
      ENDIF.

      CLEAR: gs_zmmt008.

      DATA: lr_lojas        TYPE RANGE OF werks_d,
            lv_matnr        TYPE bapi_mrp_mat_param-material,
            lv_plant        TYPE bapi_mrp_mat_param-plant,
            ls_list         TYPE bapi_mrp_list,
            ls_stock_datail TYPE bapi_mrp_stock_detail,
            lv_bstme        TYPE bstme.

      APPEND LINES OF s_cd TO lr_lojas.
      APPEND LINES OF s_lojas TO lr_lojas.

      SORT lr_lojas BY low ASCENDING.
      DELETE ADJACENT DUPLICATES FROM lr_lojas COMPARING low.

      "Estoque em transferência
      LOOP AT lr_lojas ASSIGNING FIELD-SYMBOL(<fs_lojas>).
        lv_matnr = CONV #( gs_material-matnr ).
        lv_plant = CONV #( <fs_lojas>-low ).

        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
          EXPORTING
            input    = gs_material-bstme
            language = sy-langu
          IMPORTING
            output   = lv_bstme.
*** GFX - JBRS - DEVK923960 - Início - 17/11/2021
*          CALL FUNCTION 'BAPI_MATERIAL_STOCK_REQ_LIST'
*            EXPORTING
*              material         = lv_matnr
*              plant            = lv_plant
*            IMPORTING
*              mrp_list         = ls_list
*              mrp_stock_detail = ls_stock_datail.
        ASSIGN lt_ekpo_consumomat[ matnr = lv_matnr
                                   werks = lv_plant ] TO FIELD-SYMBOL(<fs_ekpo_consumomat>).
        IF sy-subrc IS INITIAL.
*            IF ls_list-base_uom = lv_bstme.
*              gs_zmmt008-esttransf = gs_zmmt008-esttransf + ls_stock_datail-stck_in_transit.
          IF <fs_ekpo_consumomat>-meins = lv_bstme.
            gs_zmmt008-esttransf = gs_zmmt008-esttransf + <fs_ekpo_consumomat>-menge.
          ELSEIF gs_marm-umrez IS NOT INITIAL.
*              gs_zmmt008-esttransf = gs_zmmt008-esttransf + ( ls_stock_datail-stck_in_transit / gs_marm-umrez ).
            gs_zmmt008-esttransf = gs_zmmt008-esttransf + ( <fs_ekpo_consumomat>-menge / gs_marm-umrez ).
          ENDIF.
          FREE: lv_bstme, lv_matnr, lv_plant, ls_stock_datail, ls_list.
        ENDIF.

        "Seleciona estoque pendente de venda
        ASSIGN lt_vbbe[ matnr = lv_matnr
                        werks = lv_plant ] TO FIELD-SYMBOL(<fs_vbbe>).
        IF sy-subrc = 0.

          gs_mardljmat-labst = gs_mardljmat-labst - <fs_vbbe>-menge.
          IF gs_mardljmat-labst < 0.
            gs_mardljmat-labst = 0.
          ENDIF.

        ENDIF.
*** GFX - JBRS - DEVK923960 - FIm - 17/11/2021

      ENDLOOP.
      FREE lr_lojas.

      gs_zmmt008-refrel     = p_refrel.
      gs_zmmt008-seq        = lv_cont.
      gs_zmmt008-subseq     = '0'.
      gs_zmmt008-maktx      = gs_material-maktx.
      gs_zmmt008-matnr      = gs_material-matnr.
      gs_zmmt008-bismt      = gs_material-bismt.
*      gs_zmmt008-meins      = gs_material-meins.
      gs_zmmt008-meins      = gs_marm-meinh.
      gs_zmmt008-umrez2     = gs_marm-umrez.
      gs_zmmt008-ekgrp      = gs_material-ekgrp.
      gs_zmmt008-ekorg      = gs_material-ekorg.
      gs_zmmt008-venda_mes1 = gs_consumomat-venda_mes1 / gs_marm-umrez.
      gs_zmmt008-venda_mes2 = gs_consumomat-venda_mes2 / gs_marm-umrez.
      gs_zmmt008-venda_mes3 = gs_consumomat-venda_mes3 / gs_marm-umrez.
      gs_zmmt008-venda_mes4 = gs_consumomat-venda_mes4 / gs_marm-umrez.
      gs_zmmt008-venda_mes5 = gs_consumomat-venda_mes5 / gs_marm-umrez.
      gs_zmmt008-venda_mes6 = gs_consumomat-venda_mes6 / gs_marm-umrez.
      gs_zmmt008-estcd1     = gv_estcd1 / gs_marm-umrez.
      gs_zmmt008-estcd2     = gv_estcd2 / gs_marm-umrez.
      gs_zmmt008-estcd3     = gv_estcd3 / gs_marm-umrez.
      gs_zmmt008-estconf    = gs_mardljmat_c-labst / gs_marm-umrez.
      gs_zmmt008-estlojas   = COND #( WHEN gs_mardljmat-labst > 0
                                        THEN gs_mardljmat-labst / gs_marm-umrez ).
      gs_zmmt008-estsem     = ''.
      gs_zmmt008-pcsem      = ''.
      gs_zmmt008-esttotal   = gs_zmmt008-estcd1 + gs_zmmt008-estcd2 + gs_zmmt008-estcd3 + gs_zmmt008-estlojas + gs_zmmt008-esttransf.
      gs_zmmt008-dtulcomp   = gs_ekko-aedat.
      gs_zmmt008-qtulcomp   = gs_ekko-menge.
      gs_zmmt008-vlulcomp   = gs_ekko-netpr.

***Buscar o preço da lista de preço menos o desconto oferecido na política comercial de preço
      CLEAR: gs_zmmt008-vlbrulcomp,gs_zmmt008-origemprecobruto.
      READ TABLE ti_mara WITH KEY matnr = gs_zmmt008-matnr ASSIGNING FIELD-SYMBOL(<fs_mara>) BINARY SEARCH.
      IF sy-subrc EQ 0.
        READ TABLE ti_mbew ASSIGNING FIELD-SYMBOL(<fs_mbew>) WITH KEY matnr = <fs_mara>-matnr BINARY SEARCH.
        READ TABLE ti_zmmt018 WITH KEY prdha = <fs_mara>-prdha ASSIGNING FIELD-SYMBOL(<fs_zmmt018>) BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE ti_zmmt022 WITH KEY matnr = <fs_mara>-matnr ASSIGNING FIELD-SYMBOL(<fs_zmmt022>) BINARY SEARCH.
          IF sy-subrc EQ 0.
            CLEAR v_desconto.
            IF <fs_zmmt018>-vlrdesctotal > 0.
              v_desconto = <fs_zmmt022>-brtwr_lp * ( <fs_zmmt018>-vlrdesctotal / 100 ).
            ENDIF.
            IF <fs_mbew> IS ASSIGNED.
              IF ( <fs_mbew>-mtorg > 0 AND  <fs_mbew>-mtorg < 3 ) OR ( <fs_mbew>-mtorg > 5 AND <fs_mbew>-mtorg < 8 ).
                "Material importado
                v_desconto = v_desconto + ( ( <fs_zmmt022>-brtwr_lp - v_desconto ) * ( <fs_zmmt018>-percdificmsimp / 100 ) ).
              ELSE.
                "Material nacional
                v_desconto = v_desconto + ( ( <fs_zmmt022>-brtwr_lp - v_desconto ) * ( <fs_zmmt018>-percdificmsnac / 100 ) ).
              ENDIF.
            ENDIF.
            gs_zmmt008-vlbrulcomp = <fs_zmmt022>-brtwr_lp - v_desconto.
            gs_zmmt008-origemprecobruto = 'LP'.
            gs_zmmt008-brtwr_perc = <fs_zmmt022>-brtwr_perc.
            gs_zmmt008-bstrf      = <fs_zmmt022>-bstrf.
          ENDIF.
        ENDIF.
      ENDIF.
      IF gs_zmmt008-origemprecobruto IS INITIAL.
        lv_kposn = gs_ekko-ebelp.
        READ TABLE gt_prcd_elements INTO gs_prcd_elements WITH KEY knumv = gs_ekko-knumv kposn = lv_kposn kschl = 'PBXX' BINARY SEARCH.
        IF sy-subrc NE 0.
          READ TABLE gt_prcd_elements INTO gs_prcd_elements WITH KEY knumv = gs_ekko-knumv kposn = lv_kposn kschl = 'PB00' BINARY SEARCH.
        ENDIF.
        IF gs_prcd_elements-kpein > 0.
          gs_zmmt008-vlbrulcomp = ( gs_prcd_elements-kbetr / gs_prcd_elements-kpein ).
        ELSE.
          gs_zmmt008-vlbrulcomp = gs_prcd_elements-kbetr .
        ENDIF.
        gs_zmmt008-origemprecobruto = 'UC'.
      ENDIF.

*     gs_zmmt008-vlbrulcomp = gs_prcd_elements-kbetr / gs_prcd_elements-kpein.
      gs_zmmt008-vlforn     = gs_zmmt008-vlbrulcomp.
      gs_zmmt008-rctotal    = gs_ebanmesmat-menge / gs_marm-umrez.
*      gs_zmmt008-pctotal    = gs_ekkomesmat-menge.

      IF gs_eketsum-meins = gs_material-bstme.
        gs_zmmt008-pctotal    = gs_eketsum-menge.
      ELSE.
        gs_zmmt008-pctotal    = ( gs_eketsum-menge - gs_eketsum-wemng ) / gs_marm-umrez.
      ENDIF.

*     APPC - MM.577809 – Ericky Fernandes - 05/06/2024 - Início
      gs_zmmt008-pctotal_fl_analise = VALUE #( lt_eketsum_aux[ matnr = gs_consumomat-matnr ]-menge OPTIONAL ).
      ASSIGN lt_ultima_compra[ matnr = gs_consumomat-matnr ] TO FIELD-SYMBOL(<lfs_ultima_compra>).
      IF sy-subrc = 0.
        gs_zmmt008-valor_bruto_geral  = COND #( WHEN <lfs_ultima_compra>-menge IS NOT INITIAL
                                                THEN <lfs_ultima_compra>-brtwr / <lfs_ultima_compra>-menge
                                                ELSE <lfs_ultima_compra>-brtwr ).
      ENDIF.
*     APPC - MM.577809 – Ericky Fernandes - 05/06/2024 - Fim

      gs_zmmt008-qtcd1      = ''. "OK
*      gs_zmmt008-qtcd2      = ''. "OK
      gs_zmmt008-banfncd1   = ''. "OK
      gs_zmmt008-banfncd2   = ''. "OK
      gs_zmmt008-lifnr_sem  = s_lifnr-low.
      gs_zmmt008-obsitem    = p_obs.
      gs_zmmt008-log        = ''. "OK
      gs_zmmt008-ersda      = gs_material-ersda.
      gs_zmmt008-matkl      = gs_material-matkl.
      gs_zmmt008-prdha      = gs_material-prdha.
      gs_zmmt008-mfrpn      = gs_material-mfrpn.
      gs_zmmt008-zz_lifnr_ref = gs_material-zz_lifnr_ref.
      gs_zmmt008-ekgrp      = gs_material-ekgrp.
      gs_zmmt008-ekorg      = gs_material-ekorg.
      gs_zmmt008-mstae      = gs_material-mstae.
      gs_zmmt008-mtstb      = gs_t141t-mtstb.
      gs_zmmt008-uname      = sy-uname.
      gs_zmmt008-udate      = sy-datum.
      gs_zmmt008-utime      = sy-uzeit.

      IF p_pc    EQ 'X'.
        ADD gs_zmmt008-pctotal TO gs_zmmt008-esttotal.
      ENDIF.
      IF p_rc    EQ 'X'.
        ADD gs_zmmt008-rctotal TO gs_zmmt008-esttotal.
      ENDIF.

      APPEND gs_zmmt008 TO gt_zmmt008.

      CLEAR: gv_estsem, gv_pcsem.
      CLEAR lv_cont2.
      LOOP AT gt_semelhante INTO gs_semelhante WHERE bismt EQ gs_material-bismt.

        ADD 1 TO lv_cont2.

        CLEAR gs_matsem.
        TRY.
*        READ TABLE gt_matsem INTO gs_matsem WITH KEY matnr = gs_semelhante-matnr.
            gs_matsem = gt_matsem[ matnr = gs_semelhante-matnr ].

          CATCH cx_sy_itab_line_not_found.

*            IF sy-subrc NE 0.
*              READ TABLE gt_matsem_aux INTO gs_matsem WITH KEY matnr = gs_semelhante-matnr.
*            ENDIF.
            TRY.
                gs_matsem = gt_matsem_aux[ matnr = gs_semelhante-matnr ].

              CATCH cx_sy_itab_line_not_found.
            ENDTRY.
        ENDTRY.

        CLEAR gs_consumosem.
        READ TABLE gt_consumosem INTO gs_consumosem WITH KEY matnr = gs_semelhante-matnr.

        CLEAR gs_mardcdsem.
        READ TABLE gt_mardcdsem INTO gs_mardcdsem WITH KEY matnr = gs_semelhante-matnr
                                                           werks = gv_cd1.
        gv_estcd1 = gs_mardcdsem-labst.

        CLEAR gs_mardcdsem.
        READ TABLE gt_mardcdsem INTO gs_mardcdsem WITH KEY matnr = gs_semelhante-matnr
                                                           werks = gv_cd2.
        gv_estcd2 = gs_mardcdsem-labst.

        CLEAR gs_mardcdsem.
        READ TABLE gt_mardcdsem INTO gs_mardcdsem WITH KEY matnr = gs_semelhante-matnr
                                                           werks = gv_cd3.
        gv_estcd3 = gs_mardcdsem-labst.

        CLEAR gs_mardljsem.
        READ TABLE gt_mardljsem INTO gs_mardljsem WITH KEY matnr = gs_semelhante-matnr.

        ADD gs_mardljsem-labst TO gv_estsem.

        CLEAR gs_mardljsem_c.
        READ TABLE gt_mardljsem_c INTO gs_mardljsem_c WITH KEY matnr = gs_semelhante-matnr.

        CLEAR gs_ebanmessem.
        READ TABLE gt_ebanmessem INTO gs_ebanmessem WITH KEY matnr = gs_semelhante-matnr.

        CLEAR gs_eketsums.
        READ TABLE gt_eketsums INTO gs_eketsums WITH KEY matnr = gs_semelhante-matnr.

        gv_pcsem = gv_pcsem + ( gs_eketsums-menge - gs_eketsums-wemng ).

        CLEAR gs_ekkos.
        READ TABLE gt_ekkos INTO gs_ekkos WITH KEY matnr = gs_semelhante-matnr.

        CLEAR gs_prcd_eless.
        READ TABLE gt_prcd_eless INTO gs_prcd_eless WITH KEY knumv = gs_ekkos-knumv
                                                             kposn = gs_ekkos-ebelp
                                                             kschl = 'PBXX'.
        IF sy-subrc NE 0.
          READ TABLE gt_prcd_eless INTO gs_prcd_eless WITH KEY knumv = gs_ekkos-knumv
                                                               kposn = gs_ekkos-ebelp
                                                               kschl = 'PB00'.
        ENDIF.

        CLEAR gs_t141t.
        READ TABLE gt_t141t INTO gs_t141t WITH KEY mmsta = gs_matsem-mstae.

        CLEAR: gs_marm.
        LOOP AT gt_marm INTO gs_marm WHERE matnr EQ gs_matsem-matnr.
          IF gs_marm-meinh NE gs_matsem-meins.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF gs_marm-umrez IS INITIAL.
          gs_marm-umrez = 1.
        ENDIF.

        CLEAR: gs_zmmt008.

        APPEND LINES OF s_cd TO lr_lojas.
        APPEND LINES OF s_lojas TO lr_lojas.

        SORT lr_lojas BY low ASCENDING.
        DELETE ADJACENT DUPLICATES FROM lr_lojas COMPARING low.

        "Estoque em transferência
        UNASSIGN <fs_lojas>.
        LOOP AT lr_lojas ASSIGNING <fs_lojas>.
          lv_matnr = CONV #( gs_material-matnr ).
          lv_plant = CONV #( <fs_lojas>-low ).

          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
            EXPORTING
              input    = gs_material-bstme
              language = sy-langu
            IMPORTING
              output   = lv_bstme.

*** GFX - JBRS - DEVK923960 - Início - 17/11/2021
*          CALL FUNCTION 'BAPI_MATERIAL_STOCK_REQ_LIST'
*            EXPORTING
*              material         = lv_matnr
*              plant            = lv_plant
*            IMPORTING
*              mrp_list         = ls_list
*              mrp_stock_detail = ls_stock_datail.
          ASSIGN lt_ekpo_consumomat[ matnr = lv_matnr
                                     werks = lv_plant ] TO FIELD-SYMBOL(<fs_ekpo_semelhante>).
          IF sy-subrc IS INITIAL.
*            IF ls_list-base_uom = lv_bstme.
*              gs_zmmt008-esttransf = gs_zmmt008-esttransf + ls_stock_datail-stck_in_transit.
            IF <fs_ekpo_semelhante>-meins = lv_bstme.
              gs_zmmt008-esttransf = gs_zmmt008-esttransf + <fs_ekpo_semelhante>-menge.
            ELSEIF gs_marm-umrez IS NOT INITIAL.
*              gs_zmmt008-esttransf = gs_zmmt008-esttransf + ( ls_stock_datail-stck_in_transit / gs_marm-umrez ).
              gs_zmmt008-esttransf = gs_zmmt008-esttransf + ( <fs_ekpo_semelhante>-menge / gs_marm-umrez ).
            ENDIF.
            FREE: lv_bstme, lv_matnr, lv_plant, ls_stock_datail, ls_list.
          ENDIF.

          "Seleciona estoque pendente de venda
          ASSIGN lt_vbbe[ matnr = lv_matnr
                          werks = lv_plant ] TO FIELD-SYMBOL(<fs_vbbe_semelhante>).
          IF sy-subrc = 0.

            gs_mardljmat-labst = gs_mardljmat-labst - <fs_vbbe>-menge.
            IF gs_mardljmat-labst < 0.
              gs_mardljmat-labst = 0.
            ENDIF.

          ENDIF.
*** GFX - JBRS - DEVK923960 - Fim - 17/11/2021

        ENDLOOP.
        FREE lr_lojas.

        gs_zmmt008-refrel     = p_refrel.
        gs_zmmt008-seq        = lv_cont.
        gs_zmmt008-subseq     = lv_cont2.
        gs_zmmt008-maktx      = gs_matsem-maktx.
        gs_zmmt008-matnr      = gs_semelhante-matnr.
        gs_zmmt008-bismt      = gs_matsem-bismt.
*        gs_zmmt008-meins      = gs_matsem-meins.
        gs_zmmt008-meins      = gs_marm-meinh.
        gs_zmmt008-umrez2     = gs_marm-umrez.
        gs_zmmt008-venda_mes1 = gs_consumosem-venda_mes1 / gs_marm-umrez.
        gs_zmmt008-venda_mes2 = gs_consumosem-venda_mes2 / gs_marm-umrez.
        gs_zmmt008-venda_mes3 = gs_consumosem-venda_mes3 / gs_marm-umrez.
        gs_zmmt008-venda_mes4 = gs_consumosem-venda_mes4 / gs_marm-umrez.
        gs_zmmt008-venda_mes5 = gs_consumosem-venda_mes5 / gs_marm-umrez.
        gs_zmmt008-venda_mes6 = gs_consumosem-venda_mes6 / gs_marm-umrez.
        gs_zmmt008-estcd1     = gv_estcd1 / gs_marm-umrez.
        gs_zmmt008-estcd2     = gv_estcd2 / gs_marm-umrez.
        gs_zmmt008-estcd3     = gv_estcd3 / gs_marm-umrez.
        gs_zmmt008-estconf    = gs_mardljsem_c-labst / gs_marm-umrez.
        ADD gs_zmmt008-estconf TO gs_zmmt008-estcd1.
        ADD gs_zmmt008-estconf TO gs_zmmt008-estcd2.
        ADD gs_zmmt008-estconf TO gs_zmmt008-estcd3.
        gs_zmmt008-estlojas   = COND #( WHEN gs_mardljsem-labst < 0
                                          THEN gs_mardljsem-labst / gs_marm-umrez ).
        gs_zmmt008-estsem     = ''.
        gs_zmmt008-pcsem      = ''.
        gs_zmmt008-esttotal   = gs_zmmt008-estcd1 + gs_zmmt008-estcd2 + gs_zmmt008-estcd3 + gs_zmmt008-estlojas + gs_zmmt008-esttransf.
        gs_zmmt008-dtulcomp   = gs_ekkos-aedat.
        gs_zmmt008-qtulcomp   = gs_ekkos-menge.
        gs_zmmt008-vlulcomp   = gs_ekkos-netpr.
        gs_zmmt008-vlbrulcomp = gs_prcd_eless-kbetr / gs_prcd_eless-kpein.
        gs_zmmt008-vlforn     = gs_zmmt008-vlbrulcomp.
        gs_zmmt008-rctotal    = gs_ebanmessem-menge / gs_marm-umrez.
*        gs_zmmt008-pctotal    = gs_ekkomessem-menge.

        IF gs_eketsum-meins = gs_material-bstme.
          gs_zmmt008-pctotal    = gs_eketsum-menge.
        ELSE.
          gs_zmmt008-pctotal    = ( gs_eketsum-menge - gs_eketsum-wemng ) / gs_marm-umrez.
        ENDIF.

        gs_zmmt008-qtcd1      = ''. "OK
*        gs_zmmt008-qtcd2      = ''. "OK
        gs_zmmt008-banfncd1   = ''. "OK
        gs_zmmt008-banfncd2   = ''. "OK
        gs_zmmt008-lifnr_sem  = gs_matsem-lifnr.
        gs_zmmt008-obsitem    = p_obs.
        gs_zmmt008-log        = ''. "OK
        gs_zmmt008-ersda      = gs_matsem-ersda.
        gs_zmmt008-matkl      = gs_matsem-matkl.
        gs_zmmt008-prdha      = gs_matsem-prdha.
        gs_zmmt008-mfrpn      = gs_matsem-mfrpn.
        gs_zmmt008-zz_lifnr_ref = gs_matsem-zz_lifnr_ref.
        gs_zmmt008-ekgrp      = gs_matsem-ekgrp.
        gs_zmmt008-ekorg      = gs_matsem-ekorg.
        gs_zmmt008-mstae      = gs_matsem-mstae.
        gs_zmmt008-mtstb      = gs_t141t-mtstb.
        gs_zmmt008-uname      = sy-uname.
        gs_zmmt008-udate      = sy-datum.
        gs_zmmt008-utime      = sy-uzeit.

        IF p_pc    EQ 'X'.
          ADD gs_zmmt008-pctotal TO gs_zmmt008-esttotal.
        ENDIF.
        IF p_rc    EQ 'X'.
          ADD gs_zmmt008-rctotal TO gs_zmmt008-esttotal.
        ENDIF.

        APPEND gs_zmmt008 TO gt_zmmt008.

      ENDLOOP.

      CLEAR: gs_zmmt008.
      READ TABLE gt_zmmt008 INTO gs_zmmt008 WITH KEY refrel = p_refrel
                                                     seq    = lv_cont
                                                     subseq = '0'.

      gs_zmmt008-estsem     = gv_estsem / gs_zmmt008-umrez2.
      gs_zmmt008-pcsem      = gv_pcsem / gs_zmmt008-umrez2.

      IF p_sasem EQ 'X'.
        ADD gs_zmmt008-estsem TO gs_zmmt008-esttotal.
      ENDIF.
      IF p_pcsem EQ 'X'.
        ADD gs_zmmt008-pcsem TO gs_zmmt008-esttotal.
      ENDIF.

      MODIFY TABLE gt_zmmt008 FROM gs_zmmt008.

    ENDLOOP.

    IF gv_tp_relat EQ 'S'.
      MODIFY zmmt007 FROM TABLE gt_zmmt007.
      MODIFY zmmt008 FROM TABLE gt_zmmt008.
    ENDIF.

*** GFX - DFLC - Fim 17/03/2020

  ENDMETHOD.

  METHOD monta_dados.

    PERFORM f_catalogo.

    DATA: lv_tot_venda TYPE zesttotal,
          lv_med_venda TYPE zesttotal,
          lv_mes_rgvnd TYPE month,
          lv_mes_atual TYPE month,
          lv_dia_atual TYPE fopc_day,
          lv_ult_dia   TYPE fopc_day,
          lv_dt_ult    TYPE sy-datum.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 85
        text       = 'Realizando a montagem dos dados'.

    lv_mes_rgvnd = s_rgvnd-high+4(2).
    lv_mes_atual = sy-datum+4(2).

    CALL FUNCTION 'MM_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = s_rgvnd-high
      IMPORTING
        last_day_of_month = lv_dt_ult
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.

    lv_dia_atual = sy-datum+6(2).
    lv_ult_dia   = lv_dt_ult+6(2).

    CLEAR: gt_saida, gt_saida[].
    LOOP AT gt_zmmt008 INTO gs_zmmt008.

*     APPC - MM.509373 – Ericky Fernandes - 21/02/2024 - Início
      CASE gs_zmmt008-mstae.
        WHEN '11'.
          gs_zmmt008-mfrpn = |{ gs_zmmt008-mfrpn } *|.
        WHEN '12'.
          gs_zmmt008-mfrpn = |{ gs_zmmt008-mfrpn } @|.
        WHEN '15'.
          gs_zmmt008-mfrpn = |{ gs_zmmt008-mfrpn } &|.
        WHEN '17'.
          gs_zmmt008-mfrpn = |{ gs_zmmt008-mfrpn } $|.
        WHEN '20'.
          gs_zmmt008-mfrpn = |{ gs_zmmt008-mfrpn } #|.
      ENDCASE.
*     APPC - MM.509373 – Ericky Fernandes - 21/02/2024 - Fim

      IF rb_cs EQ ''.
        IF NOT gs_zmmt008-subseq IS INITIAL.
          CONTINUE.
        ENDIF.
      ENDIF.

      CLEAR gs_saida.

      MOVE-CORRESPONDING gs_zmmt008 TO gs_saida.

      lv_tot_venda = gs_zmmt008-venda_mes1 + gs_zmmt008-venda_mes2 + gs_zmmt008-venda_mes3 +
                     gs_zmmt008-venda_mes4 + gs_zmmt008-venda_mes5 + gs_zmmt008-venda_mes6.

      lv_med_venda = ( lv_tot_venda / gv_mm_media ) * gv_nc_media.

*{   MODIFY         DEVK908437         GFX - JVRS - Ajuste ZMM011
      IF NOT gs_zmmt008-esttotal IS INITIAL AND lv_med_venda IS NOT INITIAL. """
        gs_saida-ruptura = gs_zmmt008-esttotal / lv_med_venda.

*        IF gs_saida-ruptura < 1.
*          gs_saida-ruptura = 0.
*        ENDIF.
      ELSE.
        gs_saida-ruptura = 0.
      ENDIF.

      gs_saida-nc = COND #( WHEN lv_med_venda < gs_zmmt008-esttotal THEN
                              0
                            ELSE
                              lv_med_venda - gs_zmmt008-esttotal ).

*}   MODIFY

      IF rb_ap EQ 'X'.
        IF gs_saida-nc EQ 0.
          CONTINUE.
        ENDIF.
      ENDIF.

**{   INSERT         DEVK908476         GFX - JVRS - Ajuste ZMM011 v2
*      DATA lt_line TYPE STANDARD TABLE OF tline.
*      DATA lv_name TYPE thead-tdname.
*
*      lv_name = gs_zmmt008-matnr.
*
*      CALL FUNCTION 'READ_TEXT'
*        EXPORTING
*          id                      = 'GRUN'
*          language                = sy-langu
*          name                    = lv_name
*          object                  = 'MATERIAL'
*        TABLES
*          lines                   = lt_line
*        EXCEPTIONS
*          id                      = 1
*          language                = 2
*          name                    = 3
*          not_found               = 4
*          object                  = 5
*          reference_check         = 6
*          wrong_access_to_archive = 7
*          OTHERS                  = 8.
*
*      IF sy-subrc IS INITIAL AND lt_line IS NOT INITIAL.
*        LOOP AT lt_line ASSIGNING FIELD-SYMBOL(<fs_s_line>).
*          gs_saida-maktx = COND #( WHEN sy-tabix = 1 THEN
*                                     <fs_s_line>-tdline
*                                   ELSE
*                                     |{ gs_saida-maktx } { <fs_s_line>-tdline }| ).
*        ENDLOOP.
*      ELSE.
*        gs_saida-maktx = gs_zmmt008-maktx.
*      ENDIF.
**}   INSERT

      gs_saida-seq        = gs_zmmt008-seq.
      gs_saida-subseq     = gs_zmmt008-subseq.
      gs_saida-matnr      = gs_zmmt008-matnr.

      "GFX - FAGP - Incluir unidades de medida no ALV
      SELECT SINGLE bstme
        INTO gs_saida-bstme
        FROM mara
       WHERE matnr = gs_saida-matnr.

      IF sy-subrc = 0.
        SELECT SINGLE umrez
          INTO gs_saida-umrez
          FROM marm
         WHERE matnr = gs_saida-matnr
           AND meinh = gs_saida-bstme.
      ENDIF.

      gs_saida-bismt      = gs_zmmt008-bismt.
      IF gs_zmmt008-mstae EQ '10'.
        gs_saida-icon_status = '@0V@'.
      ELSE.
        gs_saida-icon_status = '@0W@'.
      ENDIF.
      gs_saida-mstae      = gs_zmmt008-mstae.
      gs_saida-mtstb      = gs_zmmt008-mtstb.
      gs_saida-venda_mes1 = gs_zmmt008-venda_mes1.
      gs_saida-venda_mes2 = gs_zmmt008-venda_mes2.
      gs_saida-venda_mes3 = gs_zmmt008-venda_mes3.
      gs_saida-venda_mes4 = gs_zmmt008-venda_mes4.
      gs_saida-venda_mes5 = gs_zmmt008-venda_mes5.
      gs_saida-venda_mes6 = gs_zmmt008-venda_mes6.

      IF lv_mes_rgvnd EQ lv_mes_atual.
        gs_saida-vmedia     = ( gs_zmmt008-venda_mes1 + gs_zmmt008-venda_mes2 + gs_zmmt008-venda_mes3 +
                                gs_zmmt008-venda_mes4 + gs_zmmt008-venda_mes5 + ( ( lv_dia_atual / lv_ult_dia ) * gs_zmmt008-venda_mes6 ) ) /  gv_mm_media.
      ELSE.
        gs_saida-vmedia     = ( gs_zmmt008-venda_mes1 + gs_zmmt008-venda_mes2 + gs_zmmt008-venda_mes3 +
                                gs_zmmt008-venda_mes4 + gs_zmmt008-venda_mes5 + gs_zmmt008-venda_mes6 ) /  gv_mm_media.
      ENDIF.
      gs_saida-dtulcomp           = gs_zmmt008-dtulcomp.
      gs_saida-qtulcomp           = gs_zmmt008-qtulcomp.
      gs_saida-vlulcomp           = gs_zmmt008-vlulcomp.
      gs_saida-rctotal            = gs_zmmt008-rctotal.
      gs_saida-pctotal            = gs_zmmt008-pctotal.
      gs_saida-pctotal_fl_analise = gs_zmmt008-pctotal_fl_analise. " APPC - MM.577809 – Ericky Fernandes - 06/06/2024
      gs_saida-valor_bruto_geral  = gs_zmmt008-valor_bruto_geral.  " APPC - MM.577809 – Ericky Fernandes - 06/06/2024
      gs_saida-estcd1             = gs_zmmt008-estcd1.
      gs_saida-estcd2             = gs_zmmt008-estcd2.
      gs_saida-estcd3             = gs_zmmt008-estcd3.
      gs_saida-estlojas           = gs_zmmt008-estlojas.
      gs_saida-esttransf          = gs_zmmt008-esttransf.
      gs_saida-estconf            = gs_zmmt008-estconf.
      gs_saida-esttotal           = gs_zmmt008-esttotal.
      gs_saida-estsem             = gs_zmmt008-estsem.
      gs_saida-pcsem              = gs_zmmt008-pcsem.
      gs_saida-vlforn             = gs_zmmt008-vlforn.
      gs_saida-brtwr_perc         = gs_zmmt008-brtwr_perc.
      gs_saida-obsitem            = gs_zmmt008-obsitem.
      gs_saida-lifnr_sem          = gs_zmmt008-lifnr_sem.
      gs_saida-mfrpn              = gs_zmmt008-mfrpn.
      gs_saida-zz_lifnr_ref       = gs_zmmt008-zz_lifnr_ref.
      gs_saida-log                = gs_zmmt008-log.
      gs_saida-ersda              = gs_zmmt008-ersda.

      CLEAR: gt_linecolor, gt_linecolor[].

      IF NOT gs_zmmt008-subseq IS INITIAL. "Semelhante
        LOOP AT it_fieldcat INTO wa_fieldcat.

          gs_linecolor-color-col = '2'.
          gs_linecolor-color-int = '1'.
          gs_linecolor-color-inv = '1'.
          gs_linecolor-fieldname = wa_fieldcat-fieldname.
          APPEND gs_linecolor TO gt_linecolor.

          gs_saida-color = gt_linecolor[].

        ENDLOOP.
      ENDIF.

**      IF wa_saida-qtd_compras GT 0.
*      gs_linecolor-color-col = '6'.
**      ELSE.
*      gs_linecolor-color-col = '3'.
**      ENDIF.
*      gs_linecolor-color-int = '0'.
*      gs_linecolor-color-inv = '0'.
**      gs_linecolor-fieldname = 'QTCD1'.
*      gs_linecolor-fieldname = 'PCSEM'.
*      APPEND gs_linecolor TO gt_linecolor.

      gs_saida-color = gt_linecolor[].
      gs_saida-bstrf = gs_zmmt008-bstrf.
      APPEND gs_saida TO gt_saida.

      gs_zmmt008-vlforn     = gs_saida-vlforn.
      MODIFY gt_zmmt008 FROM gs_zmmt008.

    ENDLOOP.

    gt_saida2[] = gt_saida[].

  ENDMETHOD.

  METHOD imprimir_relatorio.

    IF sy-batch IS NOT INITIAL.

      PERFORM f_salv.

    ELSE.

      PERFORM f_cabecalho.
      PERFORM f_catalogo.
      PERFORM f_layout.
      PERFORM f_relatorio.

    ENDIF.

  ENDMETHOD.

  METHOD imprimir_smartform.

    PERFORM imprimir_smartform.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&      Form  F_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_cabecalho .

  DATA: st_header TYPE kkblo_listheader.
  DATA: vg_data(10)  TYPE c,
        vg_data1(10) TYPE c.

  CLEAR: gt_header, gt_header[].
* Título do relatório
*  CLEAR st_header.
*  st_header-typ  = 'H'.
*  st_header-info = 'Relatório de Necessidade de Compra - Novo Relatório'(009).
*  APPEND st_header TO gt_header.

  CLEAR st_header.
  st_header-typ  = 'S'.
  st_header-key  = 'Fornecedor:'.
  CONCATENATE gs_zmmt007-lifnr gs_zmmt007-name1 INTO st_header-info SEPARATED BY space.
  APPEND st_header TO gt_header.

  CLEAR st_header.
  st_header-typ  = 'S'.
*{   MODIFY         DEVK908437         GFX - JVRS - Ajuste ZMM011
  st_header-key  = 'Necessidade de Compra p/ (Fut):'.
*}   MODIFY
  CONCATENATE gs_zmmt007-nec_compra+6(2) '.'
              gs_zmmt007-nec_compra+4(2) '.'
              gs_zmmt007-nec_compra(4)
         INTO vg_data.
  st_header-info = vg_data.
  APPEND st_header TO gt_header.

  CLEAR st_header.
  st_header-typ  = 'S'.
  st_header-key  = 'CDs para Análise:'.
  IF NOT gv_cd3 IS INITIAL.
    CONCATENATE gv_cd1 ',' gv_cd2 'e' gv_cd3 INTO st_header-info SEPARATED BY space.
  ELSEIF NOT gv_cd2 IS INITIAL.
    CONCATENATE gv_cd1 'e' gv_cd2 INTO st_header-info SEPARATED BY space.
  ELSE.
    st_header-info = gv_cd1.
  ENDIF.
  APPEND st_header TO gt_header.

  CLEAR st_header.
  st_header-typ  = 'S'.
  st_header-key  = 'Lojas:'.
  st_header-info = gv_lojas.
  APPEND st_header TO gt_header.

  CLEAR st_header.
  st_header-typ  = 'S'.
  st_header-key  = 'CD Determinante:'.
  st_header-info = s_cddet-low.
  APPEND st_header TO gt_header.

  CLEAR st_header.
  st_header-typ  = 'S'.
  st_header-key  = 'Referência:'.
  st_header-info = p_refrel.
  APPEND st_header TO gt_header.

*{   INSERT         DEVK908437         GFX - JVRS - Ajuste ZMM011
  CLEAR st_header.
  st_header-typ  = 'S'.
  st_header-key  = 'Média de Vendas:'.
  st_header-info = gv_mm_media.
  APPEND st_header TO gt_header.

  CLEAR st_header.
  st_header-typ  = 'S'.
  st_header-key  = 'Média para Compra:'.
  st_header-info = gv_nc_media.
  APPEND st_header TO gt_header.
*}   INSERT

ENDFORM.                    " F_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  F_CATALOGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_catalogo .

  DATA: lv_index(1)      TYPE n,
        lv_campodata1(5) TYPE c,
        lv_campodata2(5) TYPE c,
        lv_campodata3(5) TYPE c,
        lv_campodata4(5) TYPE c,
        lv_campodata5(5) TYPE c,
        lv_campodata6(5) TYPE c,
        lv_campo(50)     TYPE c,
        lv_mes(2)        TYPE n,
        lv_ano(4)        TYPE n,
        lv_data_aux      TYPE sy-datum.

  FIELD-SYMBOLS: <fs_campo> TYPE any.

  lv_data_aux = gs_zmmt007-range_vnd_ini.
  lv_index = 0.
  DO 5 TIMES.
    ADD 1 TO lv_index.
    CONCATENATE 'LV_CAMPODATA'
                lv_index
                INTO lv_campo.

    ASSIGN (lv_campo) TO <fs_campo>.

    CONCATENATE lv_data_aux+4(2) '/' lv_data_aux+2(2) INTO <fs_campo>.

    lv_mes = lv_data_aux+4(2).
    lv_ano = lv_data_aux(4).
    IF lv_mes EQ '12'.
      lv_mes = 01.
      lv_ano = lv_ano + 1.
    ELSE.
      lv_mes = lv_mes + 1.
    ENDIF.
    lv_data_aux+4(2) = lv_mes.
    lv_data_aux(4)   = lv_ano.

  ENDDO.

  CLEAR: it_fieldcat, it_fieldcat[].
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
*     I_PROGRAM_NAME         =
*     I_INTERNAL_TABNAME     =
      i_structure_name       = 'ZMME003'
      i_client_never_display = 'X'
*     I_INCLNAME             =
      i_bypassing_buffer     = 'X'
*     I_BUFFER_ACTIVE        =
    CHANGING
      ct_fieldcat            = it_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  LOOP AT it_fieldcat INTO wa_fieldcat.
    IF wa_fieldcat-fieldname EQ 'QTCD1'  OR
       wa_fieldcat-fieldname EQ 'VLFORN' OR
*       wa_fieldcat-fieldname EQ 'QTCD2' OR
       wa_fieldcat-fieldname EQ 'OBSITEM'.
*      IF gv_tp_relat EQ 'S'.
      wa_fieldcat-edit        = 'X'.
      MODIFY it_fieldcat FROM wa_fieldcat.
*      ENDIF.
    ENDIF.
    IF wa_fieldcat-fieldname EQ 'ICON_STATUS'.
      wa_fieldcat-seltext_s   = 'Status'.
      wa_fieldcat-seltext_m   = 'Status'.
      wa_fieldcat-seltext_l   = 'Status'.
*{   INSERT         DEVK908476         GFX - JVRS - Ajuste ZMM011 v2
      wa_fieldcat-icon        = abap_true.
*}   INSERT
      MODIFY it_fieldcat FROM wa_fieldcat.
    ENDIF.
    IF wa_fieldcat-fieldname EQ 'MATNR'     OR
       wa_fieldcat-fieldname EQ 'LIFNR_SEM' OR
       wa_fieldcat-fieldname EQ 'EBELNCD1'.
*       wa_fieldcat-fieldname EQ 'BANFNCD1'  OR
*       wa_fieldcat-fieldname EQ 'BANFNCD2'.
      wa_fieldcat-hotspot = 'X'.
      MODIFY it_fieldcat FROM wa_fieldcat.
    ENDIF.
    IF wa_fieldcat-fieldname EQ 'VENDA_MES1'.
*{   INSERT         DEVK908476         GFX - JVRS - Ajuste ZMM011 v2
      wa_fieldcat-reptext_ddic = lv_campodata1.
*}   INSERT
      wa_fieldcat-seltext_s    = lv_campodata1.
      wa_fieldcat-seltext_m    = lv_campodata1.
      wa_fieldcat-seltext_l    = lv_campodata1.
      MODIFY it_fieldcat FROM wa_fieldcat.
    ENDIF.
    IF wa_fieldcat-fieldname EQ 'VENDA_MES2'.
*{   INSERT         DEVK908476         GFX - JVRS - Ajuste ZMM011 v2
      wa_fieldcat-reptext_ddic = lv_campodata2.
*}   INSERT
      wa_fieldcat-seltext_s    = lv_campodata2.
      wa_fieldcat-seltext_m    = lv_campodata2.
      wa_fieldcat-seltext_l    = lv_campodata2.
      MODIFY it_fieldcat FROM wa_fieldcat.
    ENDIF.
    IF wa_fieldcat-fieldname EQ 'VENDA_MES3'.
*{   INSERT         DEVK908476         GFX - JVRS - Ajuste ZMM011 v2
      wa_fieldcat-reptext_ddic = lv_campodata3.
*}   INSERT
      wa_fieldcat-seltext_s    = lv_campodata3.
      wa_fieldcat-seltext_m    = lv_campodata3.
      wa_fieldcat-seltext_l    = lv_campodata3.
      MODIFY it_fieldcat FROM wa_fieldcat.
    ENDIF.
    IF wa_fieldcat-fieldname EQ 'VENDA_MES4'.
*{   INSERT         DEVK908476         GFX - JVRS - Ajuste ZMM011 v2
      wa_fieldcat-reptext_ddic = lv_campodata4.
*}   INSERT
      wa_fieldcat-seltext_s    = lv_campodata4.
      wa_fieldcat-seltext_m    = lv_campodata4.
      wa_fieldcat-seltext_l    = lv_campodata4.
      MODIFY it_fieldcat FROM wa_fieldcat.
    ENDIF.
    IF wa_fieldcat-fieldname EQ 'VENDA_MES5'.
*{   INSERT         DEVK908476         GFX - JVRS - Ajuste ZMM011 v2
      wa_fieldcat-reptext_ddic = lv_campodata5.
*}   INSERT
      wa_fieldcat-seltext_s    = lv_campodata5.
      wa_fieldcat-seltext_m    = lv_campodata5.
      wa_fieldcat-seltext_l    = lv_campodata5.
      MODIFY it_fieldcat FROM wa_fieldcat.
    ENDIF.
*    IF wa_fieldcat-fieldname EQ 'VENDA_MES6'.
**{   INSERT         DEVK908476         GFX - JVRS - Ajuste ZMM011 v2
*      wa_fieldcat-reptext_ddic = lv_campodata6.
**}   INSERT
*      wa_fieldcat-seltext_s    = lv_campodata6.
*      wa_fieldcat-seltext_m    = lv_campodata6.
*      wa_fieldcat-seltext_l    = lv_campodata6.
*      wa_fieldcat-no_out       = 'X'.
*      MODIFY it_fieldcat FROM wa_fieldcat.
*    ENDIF.
    IF wa_fieldcat-fieldname EQ 'ESTCD1'.
      CONCATENATE 'Est.CD'  gv_cd1 INTO
        lv_campo SEPARATED BY space.
      wa_fieldcat-seltext_s    = lv_campo.
      wa_fieldcat-seltext_m    = lv_campo.
      wa_fieldcat-seltext_l    = lv_campo.
      wa_fieldcat-reptext_ddic = lv_campo.
      MODIFY it_fieldcat FROM wa_fieldcat.
    ENDIF.
    IF wa_fieldcat-fieldname EQ 'ESTCD2'.
      CONCATENATE 'Est.CD'  gv_cd2 INTO
        lv_campo SEPARATED BY space.
      wa_fieldcat-seltext_s    = lv_campo.
      wa_fieldcat-seltext_m    = lv_campo.
      wa_fieldcat-seltext_l    = lv_campo.
      wa_fieldcat-reptext_ddic = lv_campo.
      MODIFY it_fieldcat FROM wa_fieldcat.
    ENDIF.
    IF wa_fieldcat-fieldname EQ 'ESTCD3'.
      CONCATENATE 'Est.CD'  gv_cd3 INTO
        lv_campo SEPARATED BY space.
      wa_fieldcat-seltext_s    = lv_campo.
      wa_fieldcat-seltext_m    = lv_campo.
      wa_fieldcat-seltext_l    = lv_campo.
      wa_fieldcat-reptext_ddic = lv_campo.
      MODIFY it_fieldcat FROM wa_fieldcat.
    ENDIF.
    IF wa_fieldcat-fieldname EQ 'QTCD1'.
      CONCATENATE 'Qt.RC.CD'  gv_cd1 INTO
        lv_campo SEPARATED BY space.
      wa_fieldcat-seltext_s    = lv_campo.
      wa_fieldcat-seltext_m    = lv_campo.
      wa_fieldcat-seltext_l    = lv_campo.
      wa_fieldcat-reptext_ddic = lv_campo.
      MODIFY it_fieldcat FROM wa_fieldcat.
    ENDIF.
    IF wa_fieldcat-fieldname EQ 'BSTRF'.
      wa_fieldcat-seltext_s   = 'Multiplo De'.
      wa_fieldcat-seltext_m   = 'Multiplo De'.
      wa_fieldcat-seltext_l   = 'Multiplo De'.
      MODIFY it_fieldcat FROM wa_fieldcat.
    ENDIF.
*LD Consultoria - inicio
    IF wa_fieldcat-fieldname EQ 'ORIGEMPRECOBRUTO'.
      wa_fieldcat-seltext_s   = 'Origem Preço'.
      wa_fieldcat-seltext_m   = 'Origem Preço'.
      wa_fieldcat-seltext_l   = 'Origem Preço'.
      MODIFY it_fieldcat FROM wa_fieldcat.
    ENDIF.
*fim
*    IF wa_fieldcat-fieldname EQ 'QTCD2'.
*      CONCATENATE 'Qt.RC.CD'  gv_cd2 INTO
*        lv_campo SEPARATED BY space.
*      wa_fieldcat-seltext_s    = lv_campo.
*      wa_fieldcat-seltext_m    = lv_campo.
*      wa_fieldcat-seltext_l    = lv_campo.
*      wa_fieldcat-reptext_ddic = lv_campo.
*      MODIFY it_fieldcat FROM wa_fieldcat.
*    ENDIF.
*    IF wa_fieldcat-fieldname EQ 'BANFNCD1'.
*      CONCATENATE 'RC p/ CD'  gv_cd1 INTO
*        lv_campo SEPARATED BY space.
*      wa_fieldcat-seltext_s    = lv_campo.
*      wa_fieldcat-seltext_m    = lv_campo.
*      wa_fieldcat-seltext_l    = lv_campo.
*      wa_fieldcat-reptext_ddic = lv_campo.
*      MODIFY it_fieldcat FROM wa_fieldcat.
*    ENDIF.
*    IF wa_fieldcat-fieldname EQ 'BANFNCD2'.
*      CONCATENATE 'RC p/ CD'  gv_cd2 INTO
*        lv_campo SEPARATED BY space.
*      wa_fieldcat-seltext_s    = lv_campo.
*      wa_fieldcat-seltext_m    = lv_campo.
*      wa_fieldcat-seltext_l    = lv_campo.
*      wa_fieldcat-reptext_ddic = lv_campo.
*      MODIFY it_fieldcat FROM wa_fieldcat.
*    ENDIF.
    IF wa_fieldcat-fieldname EQ 'MTSTB'    OR
       wa_fieldcat-fieldname EQ 'MSTAE'    OR
       wa_fieldcat-fieldname EQ 'DATA'     OR
       wa_fieldcat-fieldname EQ 'HORA'     OR
       wa_fieldcat-fieldname EQ 'USUARIO'  OR
       wa_fieldcat-fieldname EQ 'STATUS'   OR
       wa_fieldcat-fieldname EQ 'QTCD2'    OR
       wa_fieldcat-fieldname EQ 'BANFNCD1' OR
       wa_fieldcat-fieldname EQ 'BANFNCD2' OR
       wa_fieldcat-fieldname EQ 'MSG'.
      wa_fieldcat-no_out      = 'X'.
      MODIFY it_fieldcat FROM wa_fieldcat.
    ENDIF.
*{   INSERT         DEVK908476         GFX - JVRS - Ajuste ZMM011 v2
    IF wa_fieldcat-fieldname EQ 'MAKTX'.
      wa_fieldcat-outputlen = 40.
      wa_fieldcat-ddic_outputlen = 40.
      MODIFY it_fieldcat FROM wa_fieldcat.
    ENDIF.
*}   INSERT
*   APPC - MM.577809 – Ericky Fernandes - 06/06/2024 - Início
    IF wa_fieldcat-fieldname EQ 'PCTOTAL_FL_ANALISE'.
      wa_fieldcat-seltext_s   = 'Pendência Fl análise'.
      wa_fieldcat-seltext_m   = 'Pendência Fl análise'.
      wa_fieldcat-seltext_l   = 'Pendência Fl análise'.
      MODIFY it_fieldcat FROM wa_fieldcat.
    ENDIF.
    IF wa_fieldcat-fieldname EQ 'VALOR_BRUTO_GERAL'.
      wa_fieldcat-seltext_s   = 'Valor bruto geral'.
      wa_fieldcat-seltext_m   = 'Valor bruto geral'.
      wa_fieldcat-seltext_l   = 'Valor bruto geral'.
      MODIFY it_fieldcat FROM wa_fieldcat.
    ENDIF.
*   APPC - MM.577809 – Ericky Fernandes - 06/06/2024 - Fim
  ENDLOOP.

ENDFORM.                    " F_CATALOGO
*&---------------------------------------------------------------------*
*&      Form  Preencher_field
*&---------------------------------------------------------------------*
FORM zf_preencher_field        USING  VALUE(p_texto)
                                      VALUE(p_campo)
                                      VALUE(p_tabela)
                                      VALUE(p_key)
                                      VALUE(p_hot)
                                      VALUE(p_edit)
                                      VALUE(p_icon)
                                      VALUE(p_out)
                                      VALUE(p_no_zero)
                                      VALUE(p_sum).

*  DATA: wl_afield     TYPE lvc_s_fcat.
  DATA wl_afield TYPE slis_fieldcat_alv.

  CLEAR wl_afield.
  wl_afield-outputlen = 50.
  wl_afield-fieldname        = p_campo.
  wl_afield-tabname          = p_tabela.
  wl_afield-seltext_l        = p_texto.
*  wl_afield-seltext_m        = p_texto.
*  wl_afield-seltext_s        = p_texto.
  wl_afield-key              = p_key.
  wl_afield-hotspot          = p_hot.
  wl_afield-edit             = p_edit.
  wl_afield-icon             = p_icon.
  wl_afield-no_out           = p_out.
  wl_afield-no_zero          = p_no_zero.
  wl_afield-do_sum           = p_sum.
  IF p_campo = 'VLMAN'.
    wl_afield-ref_tabname      = 'BSEG'.
    wl_afield-ref_fieldname    = 'WRBTR'.
  ENDIF.
  APPEND wl_afield TO it_fieldcat.

ENDFORM.                    "zf_preencher_field
*&---------------------------------------------------------------------*
*&      Form  F_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_layout .
*  t_layout-coltab_fieldname    = 'COLINFO'.
*  t_layout-zebra               = 'X'.
  gt_layout-group_buttons       = ' '.
  gt_layout-totals_only         = ' '.
  gt_layout-box_fieldname       = 'MARC'.
* st_layout-cell_merge          = 'X'.
* Otimizar colunas na tela
***  gt_layout-colwidth_optimize   = 'X'.
  gt_layout-coltab_fieldname    = 'COLOR'.

ENDFORM.                    " F_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  F_RELATORIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_relatorio .

  DATA: ls_variant             TYPE disvariant.

  MOVE sy-repid TO vg_repid.

  ls_variant-report   = sy-repid.
  ls_variant-username = sy-uname.
  ls_variant-variant  = p_vari.

* Não otimizar colunas no preview e nem na impressão
  gt_grid_settings-no_colwopt = 'X'.
  gt_grid_settings-edt_cll_cb = 'X'.
* Não imprimir o relatório estatístico antes do relatório
  gt_print-no_print_selinfos  = 'X'.
  gt_print-no_print_listinfos = 'X'.

* Event for Data Changed
  wa_eventos-name = 'DATA_CHANGED'.
  wa_eventos-form = 'GET_DATA_CHANGED'.
  APPEND wa_eventos TO it_eventos.

*** GFX - JBRS - INICIO - 19/09/2019
  SORT gt_saida BY matnr ASCENDING.
*** GFX - JBRS - FIM - 19/09/2019

*** APPC - MM - DAM - Ocultação de campos pelo perfil do usuário - Inicio 28/02/23.
  SELECT SINGLE nivel_user
    FROM zmmt044
    INTO @DATA(lv_tp_user)
    WHERE user_name = @sy-uname.

  IF lv_tp_user <> 'A' AND lv_tp_user <> 'C'.
    LOOP AT it_fieldcat INTO wa_fieldcat.
      IF wa_fieldcat-fieldname EQ 'VLULCOMP'   OR wa_fieldcat-fieldname EQ 'VLBRULCOMP' OR
         wa_fieldcat-fieldname EQ 'BRTWR_PERC' OR wa_fieldcat-fieldname EQ 'VLFORN'     OR
         wa_fieldcat-fieldname EQ 'NETPRICE'   OR wa_fieldcat-fieldname EQ 'QTCD1'      OR
         wa_fieldcat-fieldname EQ 'QTCD2'.

        wa_fieldcat-tech   = 'X'.
        wa_fieldcat-no_out = 'X'.
        MODIFY it_fieldcat FROM wa_fieldcat.

      ENDIF.
    ENDLOOP.
  ENDIF.
*** APPC - MM - DAM - Ocultação de campos pelo perfil do usuário - Fim 28/02/23.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = vg_repid
      is_layout                = gt_layout
      it_fieldcat              = it_fieldcat
*     it_special_groups        = t_special_groups
      i_callback_user_command  = 'AT_USER_COMMAND'
      i_callback_top_of_page   = 'TOP_OF_PAGE'
      i_background_id          = 'ALV_BACKGROUND'
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_grid_settings          = gt_grid_settings
*     it_sort                  = t_sort[]
*     i_save                   = vc_save
      i_save                   = 'A'
      i_default                = 'X'
      is_variant               = ls_variant
      it_events                = it_eventos[]
*     it_event_exit            = it_event_exit[]
      is_print                 = gt_print
    TABLES
      t_outtab                 = gt_saida
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    STOP.
  ENDIF.
*  PERFORM top_of_page.
*  perform set_pf_status.
*   perform at_user_command.
ENDFORM.                    " F_RELATORIO
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM top_of_page .

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_header
      i_logo             = 'LOGO_PADRE_CICERO'.
*      i_logo             = 'ENJOYSAP_LOGO'.

  vg_linno           = sy-linno.
  SKIP TO LINE vg_linno.

ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_pf_status USING rt_extab TYPE slis_t_extab.
  DATA: wa_rt TYPE slis_extab.

*  IF p_relat EQ 'X'.
*    wa_rt = 'RECALCULO'.
*    APPEND wa_rt TO rt_extab.
*    wa_rt = 'LANCAR'.
*    APPEND wa_rt TO rt_extab.
*    wa_rt = 'CALC_AUT'.
*    APPEND wa_rt TO rt_extab.
*    wa_rt = 'GRAVATAB'.
*    APPEND wa_rt TO rt_extab.
*  ENDIF.

  SET PF-STATUS 'PF_STATUS_ALV' EXCLUDING rt_extab.

ENDFORM.                    " SET_PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  AT_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM at_user_command USING r_ucomm LIKE sy-ucomm
                     rs_selfield TYPE slis_selfield.

  DATA: lv_ebeln TYPE ebeln,
        lv_matnr TYPE bapimatall-material.

  DATA: v_erro(1) TYPE c.

  DATA: _ref_grid      TYPE REF TO cl_gui_alv_grid.

  IF _ref_grid IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = _ref_grid.
  ENDIF.

  CALL METHOD _ref_grid->check_changed_data.

  CASE sy-ucomm.
    WHEN 'ENDE' OR 'EENB'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      IF gt_saida[] NE gt_saida2[].
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = TEXT-001 "NOVO RELATÓRIO
            text_question         = TEXT-002 "Quer salvar o NOVO RELATORIO ou apenas Exibir?
            text_button_1         = TEXT-005 "Sim
            icon_button_1         = 'ICON_OKAY'
            text_button_2         = TEXT-006 "Não
            icon_button_2         = 'ICON_CANCEL'
            default_button        = '1'
            display_cancel_button = 'X'
          IMPORTING
            answer                = gv_answer.

        CLEAR gv_tp_relat.
        IF gv_answer EQ '1'.
          PERFORM f_grava_tabela.
          LEAVE TO SCREEN 0.
        ELSEIF gv_answer EQ '2'.
          LEAVE TO SCREEN 0.
        ELSEIF gv_answer EQ 'A'.
          "Não faz nada
        ENDIF.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'PSNEW'.
      CALL METHOD _ref_grid->refresh_table_display.
      EXIT.
*    WHEN 'CRIAR_RC'.
*      PERFORM f_criar_rc.
*      CALL METHOD _ref_grid->refresh_table_display.
*      EXIT.
    WHEN 'CRIAR_PC'.
      CLEAR v_erro.
      PERFORM f_validar_multiplos CHANGING v_erro.
      IF v_erro IS NOT INITIAL.
        EXIT.
      ENDIF.
      PERFORM f_criar_pc.
      CALL METHOD _ref_grid->refresh_table_display.
      EXIT.
    WHEN 'ATUALIZ_CAMPOS'.
      CLEAR v_erro.
      PERFORM f_validar_multiplos CHANGING v_erro.
      IF v_erro IS NOT INITIAL.
        EXIT.
      ENDIF.
      PERFORM f_atualizar_campos.
      CALL METHOD _ref_grid->refresh_table_display.
      EXIT.
    WHEN 'PRINT_SF'.
      PERFORM imprimir_smartform.
      EXIT.
    WHEN '&DATA_SAVE'.
      PERFORM f_grava_tabela.
      CALL METHOD _ref_grid->refresh_table_display.
      EXIT.
  ENDCASE.

  rs_selfield = rs_selfield.

  IF rs_selfield-sel_tab_field = '1-EBELNCD1'.
    lv_ebeln = rs_selfield-value.
    IF NOT lv_ebeln IS INITIAL.
      CALL FUNCTION 'RV_CALL_DISPLAY_TRANSACTION'
        EXPORTING
          vbeln = lv_ebeln
          vbtyp = 'V'.
    ENDIF.
*  ELSIF rs_selfield-sel_tab_field = '1-BANFNCD1'.
*    lv_ebeln = rs_selfield-value.
*    IF NOT lv_ebeln IS INITIAL.
*      CALL FUNCTION 'RV_CALL_DISPLAY_TRANSACTION'
*        EXPORTING
*          vbeln = lv_ebeln
*          vbtyp = 'v'.
*    ENDIF.
*  ELSEIF rs_selfield-sel_tab_field = '1-BANFNCD2'.
*    lv_ebeln = rs_selfield-value.
*    IF NOT lv_ebeln IS INITIAL.
*      CALL FUNCTION 'RV_CALL_DISPLAY_TRANSACTION'
*        EXPORTING
*          vbeln = lv_ebeln
*          vbtyp = 'v'.
*    ENDIF.
  ELSEIF rs_selfield-sel_tab_field = '1-MATNR'.
    lv_matnr = rs_selfield-value.
    IF NOT lv_matnr IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = lv_matnr
        IMPORTING
          output = lv_matnr.
*      READ TABLE it_saida INTO wa_saida INDEX rs_selfield-tabindex.
      CALL FUNCTION 'BAPI_MATERIAL_DISPLAY'
        EXPORTING
          material = lv_matnr.
    ENDIF.
  ELSEIF rs_selfield-sel_tab_field = '1-LIFNR_SEM'.
    lv_ebeln = rs_selfield-value.
    IF NOT lv_ebeln IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_ebeln
        IMPORTING
          output = lv_ebeln.
      CALL FUNCTION 'BAPI_VENDOR_DISPLAY'
        EXPORTING
          vendorno = lv_ebeln.
    ENDIF.
  ENDIF.

ENDFORM.                    " AT_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  get_data_changed
*&---------------------------------------------------------------------*
*       Here we will get the current cell and than add 1 to it to move
*         to next row. Than we will call the method to set the new row
*----------------------------------------------------------------------*
FORM get_data_changed USING rr_data_changed
        TYPE REF TO cl_alv_changed_data_protocol.
*
  DATA: _ref_grid TYPE REF TO cl_gui_alv_grid.
*
  DATA: le_row     TYPE i,
        le_value   TYPE c,
        le_col     TYPE i,
        les_row_id TYPE lvc_s_row,
        les_col_id TYPE lvc_s_col,
        les_row_no TYPE lvc_s_roid.

*
* Get the ALV Object reference
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = _ref_grid.

*
* Get the Current Cell
  CALL METHOD _ref_grid->get_current_cell
    IMPORTING
      e_row     = le_row
      e_value   = le_value
      e_col     = le_col
      es_row_id = les_row_id
      es_col_id = les_col_id
      es_row_no = les_row_no.

*  READ TABLE it_saida INTO wa_saida INDEX les_row_id-index.
*  wa_saida-marc = 'X'.
*  MODIFY it_saida FROM wa_saida INDEX les_row_id-index TRANSPORTING marc.
*      CALL METHOD _ref_grid->refresh_table_display.

*
* Set to the next cell
*  DESCRIBE TABLE  it_saida LINES sy-index.
*  les_row_id-index  = les_row_id-index  + 1.
*  les_row_no-row_id = les_row_no-row_id + 1.
*  IF les_row_id-index LE sy-index.
*    CALL METHOD _ref_grid->set_current_cell_via_id
*      EXPORTING
*        is_row_id    = les_row_id
*        is_column_id = les_col_id
*        is_row_no    = les_row_no.
*  ENDIF.

ENDFORM.                    "get_data_changed*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form F_GRAVA_TABELA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_grava_tabela .

  DATA: lv_erro.

*  IF gv_tp_relat NE 'S'.
*    "Opção de gravação de não foi escolhida!
*    lv_erro = 'X'.
*    MESSAGE s018(zmm01)  WITH '2' DISPLAY LIKE 'S'.
*    EXIT.
*  ENDIF.

  CHECK lv_erro IS INITIAL.

  CLEAR: lv_erro.
  LOOP AT gt_saida INTO gs_saida.

    IF NOT gs_saida-subseq IS INITIAL. "Semelhante

      IF gs_saida-qtcd1 NE 0.
        "Qtd RC CD & não deve ser preenchido!
        lv_erro = 'X'.
        MESSAGE s017(zmm01)  WITH '1' DISPLAY LIKE 'S'.
        EXIT.
      ENDIF.

*      IF gs_saida-qtcd2 NE 0.
*        "Qtd RC CD & não deve ser preenchido!
*        lv_erro = 'X'.
*        MESSAGE s017(zmm01)  WITH '2' DISPLAY LIKE 'S'.
*        EXIT.
*      ENDIF.

    ENDIF.

  ENDLOOP.

  CHECK lv_erro IS INITIAL.

  LOOP AT gt_zmmt008 INTO gs_zmmt008 WHERE subseq EQ ''.

    READ TABLE gt_saida INTO gs_saida WITH KEY seq = gs_zmmt008-seq.

    gs_zmmt008-qtcd1    = gs_saida-qtcd1.
*    gs_zmmt008-qtcd2    = gs_saida-qtcd2.
    gs_zmmt008-vlforn   = gs_saida-vlforn.
    gs_zmmt008-netprice = gs_saida-netprice.
    gs_zmmt008-peinh    = gs_saida-peinh.
    gs_zmmt008-obsitem  = gs_saida-obsitem.
    gs_zmmt008-mstae    = gs_saida-mstae.
    gs_zmmt008-mtstb    = gs_saida-mtstb.

    MODIFY gt_zmmt008 FROM gs_zmmt008.

  ENDLOOP.

  CLEAR: gs_zmmt007, gt_zmmt007, gt_zmmt007[].
  gs_zmmt007-refrel  = p_refrel.
  gs_zmmt007-cd1     = gv_cd1.
  gs_zmmt007-cd2     = gv_cd2.
  gs_zmmt007-cd3     = gv_cd3.
  gs_zmmt007-lojas   = gv_lojas.
  gs_zmmt007-cddet   = s_cddet-low.
  gs_zmmt007-mmedia  = gv_mm_media.
  gs_zmmt007-ncmedia = gv_nc_media.
  gs_zmmt007-delecao = ''.
  gs_zmmt007-obs     = p_obs.
  gs_zmmt007-lifnr   = gs_lfa1-lifnr.
  gs_zmmt007-name1   = gs_lfa1-name1.
  gs_zmmt007-lgort   = gv_lgort.
  gs_zmmt007-ekgrp   = gv_ekgrp.
  gs_zmmt007-matkl   = gv_matkl.
  gs_zmmt007-prdha   = gv_prdha.
  gs_zmmt007-matnr   = gv_matnr.
  gs_zmmt007-bismt   = gv_bismt.
  gs_zmmt007-range_vnd_ini = s_rgvnd-low.
  gs_zmmt007-range_vnd_fim = s_rgvnd-high.
  gs_zmmt007-nec_compra    = s_necom-low.
  gs_zmmt007-hist_bv = p_hist.
  gs_zmmt007-real_bv = p_real.
  gs_zmmt007-pc      = p_pc.
  gs_zmmt007-rc      = p_rc.
  gs_zmmt007-sasem   = p_sasem.
  gs_zmmt007-pcsem   = p_pcsem.
  gs_zmmt007-tr      = rb_tr.
  gs_zmmt007-ap      = rb_ap.
  gs_zmmt007-ss      = rb_ss.
  gs_zmmt007-cs      = rb_cs.
  gs_zmmt007-uname   = sy-uname.
  gs_zmmt007-udate   = sy-datum.
  gs_zmmt007-utime   = sy-uzeit.

  APPEND gs_zmmt007 TO gt_zmmt007.

*  IF gv_tp_relat EQ 'S'.
  MODIFY zmmt007 FROM TABLE gt_zmmt007.
  MODIFY zmmt008 FROM TABLE gt_zmmt008.
  "Dados gravados com sucesso!
*  MESSAGE s019(zmm01)  WITH '2' DISPLAY LIKE 'S'.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CRIAR_RC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_criar_rc .

  DATA: lv_erro.

  CLEAR: lv_erro.
  LOOP AT gt_saida INTO gs_saida.

    IF NOT gs_saida-subseq IS INITIAL. "Semelhante

      IF gs_saida-qtcd1 NE 0.
        "Qtd RC CD & não deve ser preenchido para Semelhante!
        lv_erro = 'X'.
        MESSAGE s017(zmm01) WITH '1' DISPLAY LIKE 'S'.
        EXIT.
      ENDIF.

*      IF gs_saida-qtcd2 NE 0.
*        "Qtd RC CD & não deve ser preenchido para Semelhante!
*        lv_erro = 'X'.
*        MESSAGE s017(zmm01) WITH '2' DISPLAY LIKE 'S'.
*        EXIT.
*      ENDIF.

    ENDIF.

*    IF gv_cd2 IS INITIAL AND
*       gs_saida-qtcd2 NE 0.
*      "CD 2 não informado. Qtd RC CD 2 não deve ser preenchido!
*      lv_erro = 'X'.
*      MESSAGE s022(zmm01) DISPLAY LIKE 'S'.
*      EXIT.
*    ENDIF.

    IF gs_saida-qtcd1 NE 0.
*       gs_saida-qtcd2 NE 0.
      SELECT SINGLE mstae INTO gs_saida-mstae
        FROM mara
        WHERE matnr EQ gs_saida-matnr.

      IF gs_saida-mstae NE '10'.
        "Material & Não Liberado Compra/Venda
        lv_erro = 'X'.
        MESSAGE s025(zmm01) WITH gs_saida-matnr DISPLAY LIKE 'S'.
        EXIT.
      ENDIF.

      CLEAR gs_t141t.
      READ TABLE gt_t141t INTO gs_t141t WITH KEY mmsta = gs_saida-mstae.

      gs_saida-mtstb      = gs_t141t-mtstb.

      IF gs_saida-mstae EQ '10'.
        gs_saida-icon_status = '@0V@'.
      ELSE.
        gs_saida-icon_status = '@0W@'.
      ENDIF.

      MODIFY gt_saida FROM gs_saida.
    ENDIF.

  ENDLOOP.

  CHECK lv_erro IS INITIAL.

  LOOP AT gt_saida INTO gs_saida WHERE qtcd1 NE 0.
*                                    OR qtcd2 NE 0.
  ENDLOOP.

  IF sy-subrc NE 0.
    "Qtd RC CD's não preenchido. RC não poderá ser criada!
    lv_erro = 'X'.
    MESSAGE s021(zmm01) DISPLAY LIKE 'S'.
  ENDIF.

  CHECK lv_erro IS INITIAL.

  t_bsart = 'NB'.
  CALL SCREEN '9000' STARTING AT 12 02
                     ENDING AT 41 05.
  IF ok_code IS INITIAL.
*        CLEAR t_vlr_recebendo.
  ENDIF.
  CLEAR ok_code.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CRIAR_PC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_criar_pc .

  DATA: lv_erro.

  CLEAR: lv_erro.
  LOOP AT gt_saida INTO gs_saida.

    IF NOT gs_saida-subseq IS INITIAL. "Semelhante

      IF gs_saida-qtcd1 NE 0.
        "Qtd RC CD & não deve ser preenchido para Semelhante!
        lv_erro = 'X'.
        MESSAGE s017(zmm01) WITH '1' DISPLAY LIKE 'S'.
        EXIT.
      ENDIF.

    ENDIF.

    IF gs_saida-qtcd1 NE 0.
*       gs_saida-qtcd2 NE 0.
      SELECT SINGLE mstae INTO gs_saida-mstae
        FROM mara
        WHERE matnr EQ gs_saida-matnr.

*** GFX - JBRS - DEVK916087 - 31/02/2020 - INICIO

*      IF gs_saida-mstae NE '10'.
*        "Material & Não Liberado Compra/Venda
*        lv_erro = 'X'.
*        MESSAGE s025(zmm01) WITH gs_saida-matnr DISPLAY LIKE 'S'.
*        EXIT.
*      ENDIF.

      READ TABLE s_mstae TRANSPORTING NO FIELDS WITH KEY low = gs_saida-mstae.
      IF sy-subrc <> 0.
        "Material & Não Liberado Compra/Venda
        lv_erro = 'X'.
        MESSAGE s025(zmm01) WITH gs_saida-matnr DISPLAY LIKE 'S'.
      ENDIF.

*** GFX - JBRS - DEVK916087 - 31/02/2020 - FIM

      CLEAR gs_t141t.
      READ TABLE gt_t141t INTO gs_t141t WITH KEY mmsta = gs_saida-mstae.

      gs_saida-mtstb      = gs_t141t-mtstb.

      IF gs_saida-mstae EQ '10'.
        gs_saida-icon_status = '@0V@'.
      ELSE.
        gs_saida-icon_status = '@0W@'.
      ENDIF.

      MODIFY gt_saida FROM gs_saida.
    ENDIF.

    vg_frac  = frac( gs_saida-vlforn ).
    vg_int   = vg_frac * 100000.
    vg_num5  = vg_frac * 100000.
    vg_char5 = vg_num5.
    vg_char3 = vg_char5+2(3).
    IF vg_int EQ 0.
      vg_multiplicador = 1.
    ELSE.
      IF vg_char3 EQ '000'.
        vg_multiplicador = 1.
      ELSEIF vg_char3+1(2) EQ '00'.
        vg_multiplicador = 10.
      ELSEIF vg_char3+2(1) EQ '0'.
        vg_multiplicador = 100.
      ELSE.
        vg_multiplicador = 1000.
      ENDIF.
    ENDIF.

    gs_saida-peinh    = vg_multiplicador.
    gs_saida-netprice = gs_saida-vlforn * gs_saida-peinh.

    MODIFY gt_saida FROM gs_saida.

  ENDLOOP.

  CHECK lv_erro IS INITIAL.

  LOOP AT gt_saida INTO gs_saida WHERE qtcd1 NE 0.

  ENDLOOP.

  IF sy-subrc NE 0.
    "Qtd RC CD's não preenchido. RC não poderá ser criada!
    lv_erro = 'X'.
    MESSAGE s021(zmm01) DISPLAY LIKE 'S'.
  ENDIF.

  CHECK lv_erro IS INITIAL.

  t_bsart = 'ZATA'.
  t_ekgrp = '000'.
  CALL SCREEN '9001' STARTING AT 12 02
                     ENDING AT 41 07.
  IF ok_code IS INITIAL.
*        CLEAR t_vlr_recebendo.
  ENDIF.
  CLEAR ok_code.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS '9000'.
  SET TITLEBAR '9000'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_9001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS '9001'.
  SET TITLEBAR '9001'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CANCEL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cancel INPUT.

  CASE ok_code.

    WHEN 'BACK'.
      CLEAR ok_code.
      LEAVE TO SCREEN 0.

    WHEN 'CANC'.
      CLEAR ok_code.
      LEAVE TO SCREEN 0.

    WHEN 'EXIT'.
      CLEAR ok_code.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  CASE ok_code.

    WHEN 'BACK'.
      CLEAR ok_code.
      LEAVE TO SCREEN 0.

    WHEN 'CANC'.
      CLEAR ok_code.
      LEAVE TO SCREEN 0.

    WHEN 'EXIT'.
      CLEAR ok_code.
      LEAVE TO SCREEN 0.

    WHEN 'BSIM'. "Sim
      PERFORM criar_rc.
      LEAVE TO SCREEN 0.

    WHEN ' '.
      CLEAR ok_code.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.

  CASE ok_code.

    WHEN 'BACK'.
      CLEAR ok_code.
      LEAVE TO SCREEN 0.

    WHEN 'CANC'.
      CLEAR ok_code.
      LEAVE TO SCREEN 0.

    WHEN 'EXIT'.
      CLEAR ok_code.
      LEAVE TO SCREEN 0.

    WHEN 'BSIM'. "Sim
      PERFORM criar_pc.
      LEAVE TO SCREEN 0.

    WHEN ' '.
      CLEAR ok_code.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form CRIAR_RC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM criar_rc .

**----------------------------------------------------------------------*
** Tabelas/estruturas para BAPI
**----------------------------------------------------------------------*
*  DATA: lv_number   TYPE bapiebanc-preq_no,
*        lv_cont     TYPE i,
*        lv_item     TYPE bapiebanc-preq_item,
*        lv_vendor   TYPE  bapieina-vendor,
*        lv_material TYPE  bapieina-material.
*
*  DATA: lt_pritem                TYPE STANDARD TABLE OF bapimereqitemimp,
*        lt_pritemx               TYPE STANDARD TABLE OF bapimereqitemx,
*        lt_pritemtext            TYPE STANDARD TABLE OF bapimereqitemtext,
*        lt_requisition_items     TYPE STANDARD TABLE OF bapiebanc,
*        lt_requisition_item_text TYPE STANDARD TABLE OF bapiebantx,
*        lt_components            TYPE STANDARD TABLE OF bapi_alm_order_component_e,
*        lt_inforecord_general    TYPE STANDARD TABLE OF bapieina,
*        lt_inforecord_purchorg   TYPE STANDARD TABLE OF bapieine,
*        lt_return                TYPE STANDARD TABLE OF bapireturn,
*        lt_return2               TYPE STANDARD TABLE OF bapiret2.
*
*  DATA: ls_mbew                  TYPE mbew,
*        ls_prheader              TYPE bapimereqheader,
*        ls_prheaderx             TYPE bapimereqheaderx,
*        ls_prheaderexp           TYPE bapimereqheader,
*        ls_pritem                TYPE bapimereqitemimp,
*        ls_pritemx               TYPE bapimereqitemx,
*        ls_pritemtext            TYPE bapimereqitemtext,
*        ls_header                TYPE bapi_alm_order_header_e,
*        ls_requisition_items     TYPE bapiebanc,
*        ls_requisition_item_text TYPE bapiebantx,
*        ls_components            TYPE bapi_alm_order_component_e,
*        ls_inforecord_general    TYPE bapieina,
*        ls_inforecord_purchorg   TYPE bapieine,
*        ls_return                TYPE bapireturn,
*        ls_return2               TYPE bapiret2.
*
*
*  CLEAR: ls_prheader, ls_prheaderx.
*  ls_prheader-pr_type    = t_bsart.
*  ls_prheader-create_ind = 'B'.
*
*  ls_prheaderx-pr_type    = 'X'.
*  ls_prheaderx-create_ind = 'X'.
*
*  CLEAR: lt_pritem,  lt_pritem[],
*         lt_pritemx, lt_pritemx[].
*  CLEAR lv_cont.
**{   MODIFY         DEVK908476         GFX - JVRS - Ajuste ZMM011 v2
*  LOOP AT gt_saida INTO gs_saida WHERE qtcd1    NE 0
*                                   AND banfncd1 IS INITIAL.
**}   MODIFY
*    CLEAR: ls_pritem, ls_pritemx.
*
*    CLEAR gs_material.
*    READ TABLE gt_material INTO gs_material WITH KEY matnr = gs_saida-matnr.
*
*    lv_vendor   = s_lifnr-low.
*    lv_material = gs_saida-matnr.
*
*    CLEAR: lt_inforecord_general,  lt_inforecord_general[],
*           lt_inforecord_purchorg, lt_inforecord_purchorg[],
*           lt_return,              lt_return[].
*    CALL FUNCTION 'BAPI_INFORECORD_GETLIST'
*      EXPORTING
*        vendor              = lv_vendor
*        material            = lv_material
*      TABLES
*        inforecord_general  = lt_inforecord_general
*        inforecord_purchorg = lt_inforecord_purchorg
*        return              = lt_return.
*
*    CLEAR ls_return.
*    READ TABLE lt_return INTO ls_return WITH KEY code = 'W5063'.
*    "Não existem regs.info para compras para est
*
*    IF sy-subrc EQ 0.
*      CLEAR ls_mbew.
*      SELECT SINGLE * INTO ls_mbew
*        FROM mbew
*        WHERE matnr EQ gs_saida-matnr
*          AND bwkey EQ gv_cd1.
*
*      IF ls_mbew-verpr IS INITIAL.
*        ls_mbew-verpr = '0.01'.
*      ENDIF.
*
*      ls_pritem-pur_group  = gs_material-ekgrp.
*      ls_pritem-preq_price = ls_mbew-verpr.
*      ls_pritem-price_unit = '1'.
*      ls_pritem-purch_org  = gs_material-ekorg.
*      ls_pritem-info_rec   = ''.
*      ls_pritem-vend_mat   = ''.
*    ELSE.
*      SORT lt_inforecord_purchorg BY created_at DESCENDING.
*
*      CLEAR ls_inforecord_purchorg.
*      READ TABLE lt_inforecord_purchorg INTO ls_inforecord_purchorg INDEX 1.
*
*      IF ls_inforecord_purchorg-net_price IS INITIAL.
*        ls_inforecord_purchorg-net_price = '0.01'.
*      ENDIF.
*
*      ls_pritem-pur_group  = ls_inforecord_purchorg-pur_group.
*      ls_pritem-preq_price = ls_inforecord_purchorg-net_price.
*      ls_pritem-price_unit = ls_inforecord_purchorg-price_unit.
*      ls_pritem-purch_org  = ls_inforecord_purchorg-purch_org.
*      ls_pritem-info_rec   = ls_inforecord_general-info_rec.
*      ls_pritem-vend_mat   = ls_inforecord_general-vend_mat.
*    ENDIF.
*
*    ADD 1 TO lv_cont.
*
*    ls_pritem-preq_item  = lv_cont.
*    ls_pritem-preq_name  = sy-uname.
*    ls_pritem-preq_date  = sy-datum.
*    ls_pritem-material   = gs_saida-matnr.
*    ls_pritem-plant      = gv_cd1.
*    ls_pritem-store_loc  = '1000'.
*    ls_pritem-trackingno = p_refrel.
*    ls_pritem-quantity   = gs_saida-qtcd1.
*    ls_pritem-unit       = gs_material-meins.
*    ls_pritem-deliv_date = t_lfdat.
*    ls_pritem-fixed_vend = s_lifnr-low.
*
*    APPEND ls_pritem TO lt_pritem.
*
*    ls_pritemx-preq_item  = lv_cont.
*    ls_pritemx-pur_group  = 'X'.
*    ls_pritemx-preq_price = 'X'.
*    ls_pritemx-price_unit = 'X'.
*    ls_pritemx-purch_org  = 'X'.
*    ls_pritemx-info_rec   = 'X'.
*    ls_pritemx-vend_mat   = 'X'.
*    ls_pritemx-preq_name  = 'X'.
*    ls_pritemx-preq_date  = 'X'.
*    ls_pritemx-material   = 'X'.
*    ls_pritemx-plant      = 'X'.
*    ls_pritemx-store_loc  = 'X'.
*    ls_pritemx-trackingno = 'X'.
*    ls_pritemx-quantity   = 'X'.
*    ls_pritemx-unit       = 'X'.
*    ls_pritemx-deliv_date = 'X'.
*    ls_pritemx-fixed_vend = 'X'.
*
*    APPEND ls_pritemx TO lt_pritemx.
*
*  ENDLOOP.
*
**{   INSERT         DEVK908476         GFX - JVRS - Ajuste ZMM011 v2
*  IF sy-subrc IS INITIAL.
**}   INSERT
*
*    CLEAR: lt_return2, lt_return2[], ls_return2, lv_number, ls_prheaderexp.
*    CALL FUNCTION 'BAPI_PR_CREATE'
*      EXPORTING
*        prheader    = ls_prheader
*        prheaderx   = ls_prheaderx
*      IMPORTING
*        number      = lv_number
*        prheaderexp = ls_prheaderexp
*      TABLES
*        return      = lt_return2
*        pritem      = lt_pritem
*        pritemx     = lt_pritemx
*        pritemtext  = lt_pritemtext.
*
*    DELETE lt_return2 WHERE type NE 'E'.
*    DELETE lt_return2 WHERE id     EQ 'BAPI'
*                        AND number EQ '001'.
*    READ TABLE lt_return2 INTO ls_return2 INDEX 1.
*
*    IF NOT sy-subrc IS INITIAL.
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*
**{   MODIFY         DEVK908476         GFX - JVRS - Ajuste ZMM011 v2
*      LOOP AT gt_saida INTO gs_saida WHERE qtcd1    NE 0
*                                       AND banfncd1 IS INITIAL.
**}   MODIFY
*        gs_saida-banfncd1   = lv_number.
*        gs_saida-log        = ''.
*        MODIFY gt_saida FROM gs_saida.
*
*        LOOP AT gt_zmmt008 INTO gs_zmmt008 WHERE refrel = p_refrel
*                                             AND seq    = gs_saida-seq
*                                             AND subseq = gs_saida-subseq.
*          gs_zmmt008-banfncd1   = gs_saida-banfncd1.
*          gs_zmmt008-qtcd1      = gs_saida-qtcd1.
*          gs_zmmt008-log        = gs_saida-log.
*          MODIFY gt_zmmt008 FROM gs_zmmt008.
*        ENDLOOP.
*      ENDLOOP.
*
*    ELSE.
**{   MODIFY         DEVK908476         GFX - JVRS - Ajuste ZMM011 v2
*      LOOP AT gt_saida INTO gs_saida WHERE qtcd1    NE 0
*                                       AND banfncd1 IS INITIAL.
**}   MODIFY
*        gs_saida-log = ls_return2-message.
*        MODIFY gt_saida FROM gs_saida.
*
*        LOOP AT gt_zmmt008 INTO gs_zmmt008 WHERE refrel = p_refrel
*                                             AND seq    = gs_saida-seq
*                                             AND subseq = gs_saida-subseq.
*          gs_zmmt008-log        = gs_saida-log.
*          MODIFY gt_zmmt008 FROM gs_zmmt008.
*        ENDLOOP.
*      ENDLOOP.
*    ENDIF.
*
**{   INSERT         DEVK908476         GFX - JVRS - Ajuste ZMM011 v2
*  ENDIF.
**}   INSERT

*  CLEAR: ls_prheader, ls_prheaderx.
*  ls_prheader-pr_type    = t_bsart.
*  ls_prheader-create_ind = 'B'.
*
*  ls_prheaderx-pr_type    = 'X'.
*  ls_prheaderx-create_ind = 'X'.
*
*  CLEAR: lt_pritem,  lt_pritem[],
*         lt_pritemx, lt_pritemx[].
*  CLEAR lv_cont.
*
**{   MODIFY         DEVK908476         GFX - JVRS - Ajuste ZMM011 v2
*  LOOP AT gt_saida INTO gs_saida WHERE qtcd2    NE 0
*                                   AND banfncd2 IS INITIAL.
**}   MODIFY
*
*    CLEAR: ls_pritem, ls_pritemx.
*
*    CLEAR gs_material.
*    READ TABLE gt_material INTO gs_material WITH KEY matnr = gs_saida-matnr.
*
*    lv_vendor   = s_lifnr-low.
*    lv_material = gs_saida-matnr.
*
*    CLEAR: lt_inforecord_general,  lt_inforecord_general[],
*           lt_inforecord_purchorg, lt_inforecord_purchorg[],
*           lt_return,              lt_return[].
*    CALL FUNCTION 'BAPI_INFORECORD_GETLIST'
*      EXPORTING
*        vendor              = lv_vendor
*        material            = lv_material
*      TABLES
*        inforecord_general  = lt_inforecord_general
*        inforecord_purchorg = lt_inforecord_purchorg
*        return              = lt_return.
*
*    CLEAR ls_return.
*    READ TABLE lt_return INTO ls_return WITH KEY code = 'W5063'.
*    "Não existem regs.info para compras para est
*
*    IF sy-subrc EQ 0.
*      CLEAR ls_mbew.
*      SELECT SINGLE * INTO ls_mbew
*        FROM mbew
*        WHERE matnr EQ gs_saida-matnr
*          AND bwkey EQ gv_cd2.
*
*      IF ls_mbew-verpr IS INITIAL.
*        ls_mbew-verpr = '0.01'.
*      ENDIF.
*
*      ls_pritem-pur_group  = gs_material-ekgrp.
*      ls_pritem-preq_price = ls_mbew-verpr.
*      ls_pritem-price_unit = '1'.
*      ls_pritem-purch_org  = gs_material-ekorg.
*      ls_pritem-info_rec   = ''.
*      ls_pritem-vend_mat   = ''.
*    ELSE.
*      SORT lt_inforecord_purchorg BY created_at DESCENDING.
*
*      CLEAR ls_inforecord_purchorg.
*      READ TABLE lt_inforecord_purchorg INTO ls_inforecord_purchorg INDEX 1.
*
*      IF ls_inforecord_purchorg-net_price IS INITIAL.
*        ls_inforecord_purchorg-net_price = '0.01'.
*      ENDIF.
*
*      ls_pritem-pur_group  = ls_inforecord_purchorg-pur_group.
*      ls_pritem-preq_price = ls_inforecord_purchorg-net_price.
*      ls_pritem-price_unit = ls_inforecord_purchorg-price_unit.
*      ls_pritem-purch_org  = ls_inforecord_purchorg-purch_org.
*      ls_pritem-info_rec   = ls_inforecord_general-info_rec.
*      ls_pritem-vend_mat   = ls_inforecord_general-vend_mat.
*    ENDIF.
*
*    ADD 1 TO lv_cont.
*
*    ls_pritem-preq_item  = lv_cont.
*    ls_pritem-preq_name  = sy-uname.
*    ls_pritem-preq_date  = sy-datum.
*    ls_pritem-material   = gs_saida-matnr.
*    ls_pritem-plant      = gv_cd2.
*    ls_pritem-store_loc  = '1000'.
*    ls_pritem-trackingno = p_refrel.
*    ls_pritem-quantity   = gs_saida-qtcd2.
*    ls_pritem-unit       = gs_material-meins.
*    ls_pritem-deliv_date = t_lfdat.
*    ls_pritem-fixed_vend = s_lifnr-low.
*
*    APPEND ls_pritem TO lt_pritem.
*
*    ls_pritemx-preq_item  = lv_cont.
*    ls_pritemx-pur_group  = 'X'.
*    ls_pritemx-preq_price = 'X'.
*    ls_pritemx-price_unit = 'X'.
*    ls_pritemx-purch_org  = 'X'.
*    ls_pritemx-info_rec   = 'X'.
*    ls_pritemx-vend_mat   = 'X'.
*    ls_pritemx-preq_name  = 'X'.
*    ls_pritemx-preq_date  = 'X'.
*    ls_pritemx-material   = 'X'.
*    ls_pritemx-plant      = 'X'.
*    ls_pritemx-store_loc  = 'X'.
*    ls_pritemx-trackingno = 'X'.
*    ls_pritemx-quantity   = 'X'.
*    ls_pritemx-unit       = 'X'.
*    ls_pritemx-deliv_date = 'X'.
*    ls_pritemx-fixed_vend = 'X'.
*
*    APPEND ls_pritemx TO lt_pritemx.
*
*  ENDLOOP.
*
**{   INSERT         DEVK908476         GFX - JVRS - Ajuste ZMM011 v2
*  IF sy-subrc IS INITIAL.
**}   INSERT
*
*    CLEAR: lt_return2, lt_return2[], ls_return2, lv_number, ls_prheaderexp.
*    CALL FUNCTION 'BAPI_PR_CREATE'
*      EXPORTING
*        prheader    = ls_prheader
*        prheaderx   = ls_prheaderx
*      IMPORTING
*        number      = lv_number
*        prheaderexp = ls_prheaderexp
*      TABLES
*        return      = lt_return2
*        pritem      = lt_pritem
*        pritemx     = lt_pritemx
*        pritemtext  = lt_pritemtext.
*
*    DELETE lt_return2 WHERE type NE 'E'.
*    DELETE lt_return2 WHERE id     EQ 'BAPI'
*                        AND number EQ '001'.
*    READ TABLE lt_return2 INTO ls_return2 INDEX 1.
*
*    IF NOT sy-subrc IS INITIAL.
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*
**{   MODIFY         DEVK908476         GFX - JVRS - Ajuste ZMM011 v2
*      LOOP AT gt_saida INTO gs_saida WHERE qtcd2    NE 0
*                                       AND banfncd2 IS INITIAL.
**}   MODIFY
*        gs_saida-banfncd2   = lv_number.
*        gs_saida-log        = ''.
*        MODIFY gt_saida FROM gs_saida.
*
*        LOOP AT gt_zmmt008 INTO gs_zmmt008 WHERE refrel = p_refrel
*                                             AND seq    = gs_saida-seq
*                                             AND subseq = gs_saida-subseq.
*          gs_zmmt008-banfncd2   = gs_saida-banfncd2.
*          gs_zmmt008-qtcd2      = gs_saida-qtcd2.
*          gs_zmmt008-log        = gs_saida-log.
*          MODIFY gt_zmmt008 FROM gs_zmmt008.
*        ENDLOOP.
*      ENDLOOP.
*
*    ELSE.
**{   MODIFY         DEVK908476         GFX - JVRS - Ajuste ZMM011 v2
*      LOOP AT gt_saida INTO gs_saida WHERE qtcd2    NE 0
*                                       AND banfncd2 IS INITIAL.
**}   MODIFY
*        gs_saida-log = ls_return2-message.
*        MODIFY gt_saida FROM gs_saida.
*
*        LOOP AT gt_zmmt008 INTO gs_zmmt008 WHERE refrel = p_refrel
*                                             AND seq    = gs_saida-seq
*                                             AND subseq = gs_saida-subseq.
*          gs_zmmt008-log        = gs_saida-log.
*          MODIFY gt_zmmt008 FROM gs_zmmt008.
*        ENDLOOP.
*      ENDLOOP.
*    ENDIF.
*
**{   INSERT         DEVK908476         GFX - JVRS - Ajuste ZMM011 v2
*  ENDIF.
**}   INSERT

  PERFORM f_grava_tabela.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CRIAR_PC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM criar_pc .
  IF sy-uname = 'D.NELSON'.
    BREAK-POINT.
  ENDIF.
*----------------------------------------------------------------------*
* Tabelas/estruturas para BAPI
*----------------------------------------------------------------------*
  """ Tabelas locais """
  DATA: lt_item        TYPE TABLE OF bapimepoitem,
        lt_itemx       TYPE TABLE OF bapimepoitemx,
        lt_poschedule  TYPE TABLE OF bapimeposchedule,
        lt_poschedulex TYPE TABLE OF bapimeposchedulx,
        lt_pocond      TYPE TABLE OF bapimepocond,
        lt_pocondx     TYPE TABLE OF bapimepocondx,
        lt_popartner   TYPE TABLE OF bapiekkop,
        lt_return      TYPE bapiret2_t.

  """ Estruturas locais """
  DATA: ls_header      TYPE bapimepoheader,
        ls_headerx     TYPE bapimepoheaderx,
        ls_item        TYPE bapimepoitem,
        ls_itemx       TYPE bapimepoitemx,
        ls_poschedule  TYPE bapimeposchedule,
        ls_poschedulex TYPE bapimeposchedulx,
        ls_pocond      TYPE bapimepocond,
        ls_pocondx     TYPE bapimepocondx,
        ls_popartner   TYPE bapiekkop,
        ls_return      TYPE bapiret2.

  """ Variáveis locais """
  DATA: lv_item  TYPE bapimepoitem-po_item,
        lv_seq   TYPE zmme003-sequencial,
        lv_ebeln TYPE bapimepoheader-po_number,
        lv_tipo  TYPE char1.

  CLEAR: ls_header, ls_headerx, lt_return, lt_return[], ls_return, lv_ebeln,
         lt_item, lt_item[], ls_item, lt_itemx, lt_itemx[], ls_item,
         lt_poschedule, lt_poschedule[], ls_poschedule, lt_poschedulex, lt_poschedulex[], ls_poschedule,
         lt_pocond, lt_pocond[], ls_pocond, lt_pocondx, lt_pocondx[], ls_pocondx,
         lt_popartner, lt_popartner[], ls_popartner.

  CLEAR gs_wyt3.
  SELECT SINGLE * INTO gs_wyt3
    FROM wyt3
    WHERE lifnr EQ gs_saida-lifnr_sem
      AND parvw EQ 'RS'.

  "Se não achar registro na tabela, utilizar o mesmo lifnr
  IF sy-subrc NE 0.
    gs_wyt3-lifn2 = gs_saida-lifnr_sem.
  ENDIF.

  LOOP AT gt_saida INTO gs_saida WHERE qtcd1    NE 0
                                   AND ebelncd1 IS INITIAL.
    EXIT.
  ENDLOOP.

  " PREENCHE - ls_header
  ls_header-comp_code   = '1000'.
  ls_header-doc_type    = t_bsart.
  ls_header-item_intvl  = '1'.
  ls_header-vendor      = gs_saida-lifnr_sem.
  ls_header-purch_org   = '1000'.
  ls_header-pur_group   = t_ekgrp.
  ls_header-currency    = 'BRL'.
  ls_header-doc_date    = sy-datum.
  ls_header-incoterms1  = t_inco1.
  ls_header-incoterms2  = t_inco2_l.
  ls_header-incoterms2l = t_inco2_l.

  " PREENCHE - ls_headerx
  ls_headerx-comp_code   = 'X'.
  ls_headerx-doc_type    = 'X'.
  ls_headerx-item_intvl  = 'X'.
  ls_headerx-vendor      = 'X'.
  ls_headerx-purch_org   = 'X'.
  ls_headerx-pur_group   = 'X'.
  ls_headerx-currency    = 'X'.
  ls_headerx-doc_date    = 'X'.
  ls_headerx-incoterms1  = 'X'.
  ls_headerx-incoterms2  = 'X'.
  ls_headerx-incoterms2l = 'X'.

  ls_popartner-partnerdesc = 'EF'.
  ls_popartner-langu       = 'PT'.
  ls_popartner-buspartno   = gs_wyt3-lifn2.
  APPEND ls_popartner TO lt_popartner.

  ls_popartner-partnerdesc = 'FM'.
  ls_popartner-langu       = 'PT'.
  ls_popartner-buspartno   = gs_saida-lifnr_sem.
  APPEND ls_popartner TO lt_popartner.

  CLEAR lv_item.
  lv_seq = 1.
  LOOP AT gt_saida INTO gs_saida WHERE qtcd1    NE 0
                                   AND ebelncd1 IS INITIAL.
    ADD 1 TO lv_item.

    ls_item-po_item	    = lv_item.
    ls_item-material    = gs_saida-matnr.
    ls_item-plant       = s_cddet-low.
    ls_item-stge_loc    = '1500'.
    ls_item-quantity    = gs_saida-qtcd1.

    IF gs_saida-bstme IS NOT INITIAL.
      ls_item-po_unit	    = gs_saida-bstme.
      ls_item-orderpr_un  = gs_saida-bstme.
    ELSE.
      ls_item-po_unit	    = gs_saida-meins.
      ls_item-orderpr_un  = gs_saida-meins.
    ENDIF.

    ls_item-net_price   = gs_saida-netprice.
    ls_item-price_unit  = gs_saida-peinh.
    ls_item-info_upd      = 'X'.
    ls_item-prnt_price    = 'X'.
    ls_item-unlimited_dlv = 'X'.
    ls_item-tax_code    = 'R7'. "Exit determinação IVA automático
    ls_item-gr_ind      = 'X'.
    ls_item-incoterms1  = t_inco1.
    ls_item-incoterms2  = t_inco2_l.
    ls_item-incoterms2l = t_inco2_l.
*    ls_item-po_price    = '2'.
    APPEND ls_item TO lt_item.

    ls_itemx-po_item     = lv_item.
    ls_itemx-material    = 'X'.
    ls_itemx-plant       = 'X'.
    ls_itemx-stge_loc    = 'X'.
    ls_itemx-quantity    = 'X'.
    ls_itemx-po_unit     = 'X'.
    ls_itemx-orderpr_un  = 'X'.
    ls_itemx-net_price   = 'X'.
    ls_itemx-price_unit  = 'X'.
    ls_itemx-info_upd      = 'X'.
    ls_itemx-prnt_price    = 'X'.
    ls_itemx-unlimited_dlv = 'X'.
    ls_itemx-tax_code    = 'X'.
    ls_itemx-gr_ind      = 'X'.
    ls_itemx-incoterms1  = 'X'.
    ls_itemx-incoterms2  = 'X'.
    ls_itemx-incoterms2l = 'X'.
*    ls_itemx-po_price    = 'X'.
    APPEND ls_itemx TO lt_itemx.

    ls_poschedule-po_item	      = lv_item.
    ls_poschedule-delivery_date	= t_eindt.
    ls_poschedule-quantity      = gs_saida-qtcd1.
    APPEND ls_poschedule TO lt_poschedule.

    ls_poschedulex-po_item        = lv_item.
    ls_poschedulex-delivery_date  = 'X'.
    ls_poschedulex-quantity       = 'X'.
    APPEND ls_poschedulex TO lt_poschedulex.

    ls_pocond-itm_number = lv_item.
    ls_pocond-cond_st_no = '001'.
    ls_pocond-cond_count = '01'.
    ls_pocond-cond_type  = 'PB00'.
    ls_pocond-cond_value = gs_saida-netprice.
    ls_pocond-currency   = 'BRL'.
    ls_pocond-cond_p_unt = gs_saida-peinh.
    ls_pocond-change_id  = 'U'.
    APPEND ls_pocond TO lt_pocond.

    ls_pocondx-itm_number = lv_item.
    ls_pocondx-cond_st_no = 'X'.
    ls_pocondx-cond_count = 'X'.
    ls_pocondx-cond_type  = 'X'.
    ls_pocondx-cond_value = 'X'.
    ls_pocondx-currency   = 'X'.
    ls_pocondx-cond_p_unt = 'X'.
    ls_pocondx-change_id  = 'X'.
    APPEND ls_pocondx TO lt_pocondx.

    ls_pocond-itm_number = lv_item.
    ls_pocond-cond_st_no = '001'.
    ls_pocond-cond_count = '02'.
    ls_pocond-cond_type  = 'PBXX'.
    ls_pocond-cond_value = gs_saida-netprice.
    ls_pocond-currency   = 'BRL'.
    ls_pocond-cond_p_unt = gs_saida-peinh.
    ls_pocond-change_id  = 'U'.
    APPEND ls_pocond TO lt_pocond.

    ls_pocondx-itm_number = lv_item.
    ls_pocondx-cond_st_no = 'X'.
    ls_pocondx-cond_count = 'X'.
    ls_pocondx-cond_type  = 'X'.
    ls_pocondx-cond_value = 'X'.
    ls_pocondx-currency   = 'X'.
    ls_pocondx-cond_p_unt = 'X'.
    ls_pocondx-change_id  = 'X'.
    APPEND ls_pocondx TO lt_pocondx.

    gs_saida-sequencial = lv_seq.
    MODIFY gt_saida FROM gs_saida.

    IF lv_item EQ 100.

      CALL FUNCTION 'BAPI_PO_CREATE1'
        EXPORTING
          poheader         = ls_header
          poheaderx        = ls_headerx
          no_price_from_po = 'X'
        IMPORTING
          exppurchaseorder = lv_ebeln
        TABLES
          return           = lt_return
          poitem           = lt_item
          poitemx          = lt_itemx
          poschedule       = lt_poschedule
          poschedulex      = lt_poschedulex
          pocond           = lt_pocond
          pocondx          = lt_pocondx
          popartner        = lt_popartner.

      IF NOT lv_ebeln IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

        LOOP AT gt_saida INTO gs_saida WHERE sequencial EQ lv_seq.
          gs_saida-ebelncd1   = lv_ebeln.
          gs_saida-log        = ''.
          MODIFY gt_saida FROM gs_saida.

          LOOP AT gt_zmmt008 INTO gs_zmmt008 WHERE refrel = p_refrel
                                               AND seq    = gs_saida-seq
                                               AND subseq = gs_saida-subseq.
            gs_zmmt008-ebelncd1   = gs_saida-ebelncd1.
            gs_zmmt008-qtcd1      = gs_saida-qtcd1.
            gs_zmmt008-log        = gs_saida-log.
            MODIFY gt_zmmt008 FROM gs_zmmt008.
          ENDLOOP.
        ENDLOOP.

      ELSE.

*        CLEAR ls_return.
*        READ TABLE lt_return INTO ls_return INDEX 1.
*        LOOP AT lt_return INTO ls_return WHERE type EQ 'E'.
*        ENDLOOP.
*
*        LOOP AT gt_saida INTO gs_saida WHERE sequencial EQ lv_seq.
*          gs_saida-log = ls_return-message.
*          MODIFY gt_saida FROM gs_saida.
*
*          LOOP AT gt_zmmt008 INTO gs_zmmt008 WHERE refrel = p_refrel
*                                               AND seq    = gs_saida-seq
*                                               AND subseq = gs_saida-subseq.
*            gs_zmmt008-log        = gs_saida-log.
*            MODIFY gt_zmmt008 FROM gs_zmmt008.
*          ENDLOOP.
*        ENDLOOP.
        SORT lt_return BY row ASCENDING.
        LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_returnx>) WHERE type = 'E'.

          READ TABLE lt_item ASSIGNING FIELD-SYMBOL(<fs_itemx>) INDEX <fs_returnx>-row.
          CHECK sy-subrc = 0.

          ASSIGN gt_saida[ matnr = <fs_itemx>-material ] TO FIELD-SYMBOL(<fs_saidax>).
          CHECK sy-subrc = 0.

          <fs_saidax>-log = <fs_returnx>-message.

          ASSIGN gt_zmmt008[ matnr = <fs_itemx>-material ] TO FIELD-SYMBOL(<fs_zmmt008x>).
          CHECK sy-subrc = 0.

          <fs_zmmt008x>-log = <fs_returnx>-message.

        ENDLOOP.

        IF sy-subrc <> 0.
          CLEAR ls_return.
          READ TABLE lt_return INTO ls_return INDEX 1.
          LOOP AT lt_return INTO ls_return WHERE type EQ 'E'.
          ENDLOOP.

          LOOP AT gt_saida INTO gs_saida WHERE qtcd1    NE 0
                                           AND ebelncd1 IS INITIAL.
            gs_saida-log = ls_return-message.
            MODIFY gt_saida FROM gs_saida.

            LOOP AT gt_zmmt008 INTO gs_zmmt008 WHERE refrel = p_refrel
                                                 AND seq    = gs_saida-seq
                                                 AND subseq = gs_saida-subseq.
              gs_zmmt008-log        = gs_saida-log.
              MODIFY gt_zmmt008 FROM gs_zmmt008.
            ENDLOOP.
          ENDLOOP.
        ENDIF.

      ENDIF.

      CLEAR: lt_item, lt_item[], ls_item, lt_itemx, lt_itemx[], ls_item,
             lt_poschedule, lt_poschedule[], ls_poschedule, lt_poschedulex, lt_poschedulex[], ls_poschedule,
             lt_pocond, lt_pocond[], ls_pocond, lt_pocondx, lt_pocondx[], ls_pocondx.

      CLEAR lv_item.
      ADD 1 TO lv_seq.

    ENDIF.

  ENDLOOP.

  IF lt_item[] IS NOT INITIAL.
    CALL FUNCTION 'BAPI_PO_CREATE1'
      EXPORTING
        poheader         = ls_header
        poheaderx        = ls_headerx
        no_price_from_po = 'X'
      IMPORTING
        exppurchaseorder = lv_ebeln
      TABLES
        return           = lt_return
        poitem           = lt_item
        poitemx          = lt_itemx
        poschedule       = lt_poschedule
        poschedulex      = lt_poschedulex
        pocond           = lt_pocond
        pocondx          = lt_pocondx
        popartner        = lt_popartner.

    IF NOT lv_ebeln IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

      LOOP AT gt_saida INTO gs_saida WHERE sequencial = lv_seq.
        gs_saida-ebelncd1   = lv_ebeln.
        gs_saida-log        = ''.
        MODIFY gt_saida FROM gs_saida.

        LOOP AT gt_zmmt008 INTO gs_zmmt008 WHERE refrel = p_refrel
                                             AND seq    = gs_saida-seq
                                             AND subseq = gs_saida-subseq.
          gs_zmmt008-ebelncd1   = gs_saida-ebelncd1.
          gs_zmmt008-qtcd1      = gs_saida-qtcd1.
          gs_zmmt008-log        = gs_saida-log.
          MODIFY gt_zmmt008 FROM gs_zmmt008.
        ENDLOOP.
      ENDLOOP.

    ELSE.

*      CLEAR ls_return.
*      READ TABLE lt_return INTO ls_return INDEX 1.
*      LOOP AT lt_return INTO ls_return WHERE type EQ 'E'.
*      ENDLOOP.
*
*      LOOP AT gt_saida INTO gs_saida WHERE qtcd1    NE 0
*                                       AND ebelncd1 IS INITIAL.
*        gs_saida-log = ls_return-message.
*        MODIFY gt_saida FROM gs_saida.
*
*        LOOP AT gt_zmmt008 INTO gs_zmmt008 WHERE refrel = p_refrel
*                                             AND seq    = gs_saida-seq
*                                             AND subseq = gs_saida-subseq.
*          gs_zmmt008-log        = gs_saida-log.
*          MODIFY gt_zmmt008 FROM gs_zmmt008.
*        ENDLOOP.
*      ENDLOOP.

      SORT lt_return BY row ASCENDING.
      LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_return>) WHERE type = 'E'.

        READ TABLE lt_item ASSIGNING FIELD-SYMBOL(<fs_item>) INDEX <fs_return>-row.
        CHECK sy-subrc = 0.

        ASSIGN gt_saida[ matnr = <fs_item>-material ] TO FIELD-SYMBOL(<fs_saida>).
        CHECK sy-subrc = 0.

        <fs_saida>-log = <fs_return>-message.

        ASSIGN gt_zmmt008[ matnr = <fs_item>-material ] TO FIELD-SYMBOL(<fs_zmmt008>).
        CHECK sy-subrc = 0.

        <fs_zmmt008>-log = <fs_return>-message.

      ENDLOOP.

      IF sy-subrc <> 0.
        CLEAR ls_return.
        READ TABLE lt_return INTO ls_return INDEX 1.
        LOOP AT lt_return INTO ls_return WHERE type EQ 'E'.
        ENDLOOP.

        LOOP AT gt_saida INTO gs_saida WHERE qtcd1    NE 0
                                         AND ebelncd1 IS INITIAL.
          gs_saida-log = ls_return-message.
          MODIFY gt_saida FROM gs_saida.

          LOOP AT gt_zmmt008 INTO gs_zmmt008 WHERE refrel = p_refrel
                                               AND seq    = gs_saida-seq
                                               AND subseq = gs_saida-subseq.
            gs_zmmt008-log        = gs_saida-log.
            MODIFY gt_zmmt008 FROM gs_zmmt008.
          ENDLOOP.
        ENDLOOP.
      ENDIF.

    ENDIF.
  ENDIF.

  PERFORM f_grava_tabela.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form IMPRIMIR_SMARTFORM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM imprimir_smartform .

  DATA: formname              TYPE tdsfname,
        fm_name               TYPE rs38l_fnam,
        wa_control_parameters TYPE ssfctrlop,         "Estrutura de controle (smartforms)
        wa_output_options     TYPE ssfcompop.         "Estrutura de controle (smartforms)

  DATA: gc_ucomm     LIKE sy-ucomm,
        gc_tam39(39) TYPE c.

  DATA: l_visual.

  READ TABLE gt_zmmt007 INTO gs_zmmt007 INDEX 1.

  SORT gt_zmmt008 BY seq subseq.

  CLEAR: gt_zmmt003, gt_zmmt003[].
  LOOP AT gt_zmmt008 INTO gs_zmmt008.

    IF rb_cs EQ ''.
      IF NOT gs_zmmt008-subseq IS INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING gs_zmmt008 TO gs_zmmt003.

    gs_zmmt003-vmedia = gs_zmmt007-mmedia.
    gc_tam39          = gs_zmmt003-maktx.
    gs_zmmt003-maktx  = gc_tam39.

*** GFX - SFSS - DEVK917192 - Início - 09.04.2020
    READ TABLE gt_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) WITH KEY matnr = gs_zmmt003-matnr.
    IF sy-subrc IS INITIAL.
      IF <fs_saida>-bstme IS NOT INITIAL. "Unidade de medida do pedido
        gs_zmmt003-meins = gs_zmmt003-bstme = <fs_saida>-bstme.
      ELSEIF <fs_saida>-meins IS NOT INITIAL. "Unidade de medida básica
        gs_zmmt003-meins = gs_zmmt003-bstme = <fs_saida>-meins.
      ENDIF.
    ENDIF.
*** GFX - SFSS - DEVK917192 - Fim - 09.04.2020

    APPEND gs_zmmt003 TO gt_zmmt003.

  ENDLOOP.

  l_visual = 'X'.

  IF l_visual EQ 'X'.
    gc_ucomm = 'PREVOUTPUT'.
  ELSE.
    gc_ucomm = 'SAIDA_IMPR'.
  ENDIF.

  CASE gc_ucomm.
    WHEN 'PREVOUTPUT' OR '9ANZ'  OR 'VIEW'.
      wa_output_options-tdimmed       = ''. "nast-dimme.
      wa_output_options-tddelete      = space.
      wa_output_options-tdnewid       = 'X'.
*      wa_output_options-tddest        = nast-ldest.
      wa_output_options-tdnoprev      = ''.
      wa_control_parameters-langu     = 'PT'.
      wa_control_parameters-no_dialog = 'X'.
      wa_control_parameters-preview   = 'X'.
    WHEN OTHERS.
      wa_output_options-tdimmed       = ''. "nast-dimme.
      wa_output_options-tddelete      = space.
      wa_output_options-tdnewid       = 'X'.
      wa_output_options-tdnoprev      = 'X'.
      wa_control_parameters-langu     = 'PT'.
      wa_control_parameters-no_dialog = 'X'.
      wa_control_parameters-preview   = ''.
  ENDCASE.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZMM_ANALISE_COMPRA'
    IMPORTING
      fm_name            = fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  CALL FUNCTION fm_name
    EXPORTING
      control_parameters = wa_control_parameters
      output_options     = wa_output_options
      gs_zmmt007         = gs_zmmt007
    TABLES
      gt_zmmt003         = gt_zmmt003
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ATUALIZAR_CAMPOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_atualizar_campos .

  LOOP AT gt_saida INTO gs_saida.

    vg_frac  = frac( gs_saida-vlforn ).
    vg_int   = vg_frac * 100000.
    vg_num5  = vg_frac * 100000.
    vg_char5 = vg_num5.
    vg_char3 = vg_char5+2(3).
    IF vg_int EQ 0.
      vg_multiplicador = 1.
    ELSE.
      IF vg_char3 EQ '000'.
        vg_multiplicador = 1.
      ELSEIF vg_char3+1(2) EQ '00'.
        vg_multiplicador = 10.
      ELSEIF vg_char3+2(1) EQ '0'.
        vg_multiplicador = 100.
      ELSE.
        vg_multiplicador = 1000.
      ENDIF.
    ENDIF.

    gs_saida-peinh    = vg_multiplicador.
    gs_saida-netprice = gs_saida-vlforn * gs_saida-peinh.

    MODIFY gt_saida FROM gs_saida.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_salv .

  DATA lo_alv TYPE REF TO cl_salv_table.

  TRY .

      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = gt_saida.

      lo_alv->display( ).

      MODIFY zmmt007 FROM TABLE gt_zmmt007.
      MODIFY zmmt008 FROM TABLE gt_zmmt008.
      COMMIT WORK.

    CATCH cx_salv_msg.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDTRY.

ENDFORM.
