*----------------------------------------------------------------------*
***INCLUDE SAPMZFI001F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CHAMAR_VENDAS_BALCAO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM chamar_vendas_balcao .

  CALL SCREEN '9001'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form VERIFICA_CAMPOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM verifica_campos .

  DATA: l_cadena   TYPE string,
        l_lenght   TYPE i,
        l_caracter TYPE c.

  CLEAR: ge_zfit001.
  SELECT SINGLE * INTO ge_zfit001
    FROM zfit001
    WHERE bukrs EQ t_bukrs
      AND bupla EQ t_bupla
      AND caixa EQ t_caixa
      AND bname EQ sy-uname.

  IF sy-subrc NE 0.
    "Usuário não cadastrado para lançamento de caixa!
    CLEAR ok_code.
    MESSAGE e002(zfi001).
  ENDIF.

  CLEAR: gt_zfit006, gt_zfit006[].
  SELECT  * INTO TABLE gt_zfit006
    FROM zfit006
    WHERE bukrs    EQ t_bukrs
      AND bupla    EQ t_bupla
      AND caixa    EQ t_caixa
      AND bname    EQ sy-uname
      AND data     EQ sy-datum.

  CLEAR: ge_zfit006.
  READ TABLE gt_zfit006 INTO ge_zfit006 WITH KEY operacao = 'ABER'.

  IF sy-subrc NE 0.
    CLEAR ok_code.
    "Caixa & não apto para recebimento!
    MESSAGE e006(zfi001) WITH t_caixa.
  ENDIF.

  CLEAR: ge_zfit006.
  READ TABLE gt_zfit006 INTO ge_zfit006 WITH KEY operacao = 'FECH'.

  IF sy-subrc EQ 0.
    CLEAR ok_code.
    "Caixa & fechado para recebimento!
    MESSAGE e009(zfi001) WITH t_caixa.
  ENDIF.

  CLEAR: ge_zfit002b.
  SELECT SINGLE * INTO ge_zfit002b
    FROM zfit002b
    WHERE bukrs EQ t_bukrs
      AND bupla EQ t_bupla.

  IF sy-subrc NE 0.
    CLEAR ok_code.
    "Dados da Filial & não cadastrados. Utilizar ZFI022
    MESSAGE e026(zfi001) WITH t_bupla.
  ENDIF.

  CLEAR: gt_zfit012, gt_zfit012[].
  SELECT  * INTO TABLE gt_zfit012
    FROM zfit012.

  CLEAR: gt_j_1b_nftypereg, gt_j_1b_nftypereg[].
  SELECT  * INTO TABLE gt_j_1b_nftypereg
    FROM j_1b_nftypereg.

  CLEAR: ge_zfit005.
  SELECT SINGLE * INTO ge_zfit005
    FROM zfit005
    WHERE bukrs EQ t_bukrs
      AND bupla EQ t_bupla
      AND caixa EQ t_caixa.

  IF sy-subrc NE 0.
    CLEAR ok_code.
    "Dados de Servidores de Comunicação não Cadastrados. Utilizar ZFI005
    MESSAGE e033(zfi001) WITH t_bupla.
  ENDIF.

  "Inicializa variáveis
  g_file_req  = ge_zfit005-dir_entrada.
  g_file_resp = ge_zfit005-dir_saida.
  g_file_001  = 'IntPos.001'.
  g_file_sts  = 'IntPos.STS'.
  g_file_tmp  = 'IntPos.TMP'.

  "Verifica o último caracter do diretório de Entrada
  l_cadena = g_file_req.
  l_lenght = strlen( l_cadena ).
  l_lenght = l_lenght - 1.
  l_caracter = l_cadena+l_lenght(1).

  IF l_caracter NE '\'.
    CLEAR ok_code.
    "Último caracter do diretório de & é diferente de  "\"!
    MESSAGE e043(zfi001) WITH 'Entrada'.
  ENDIF.

  "Verifica o último caracter do diretório de Saída
  l_cadena = g_file_resp.
  l_lenght = strlen( l_cadena ).
  l_lenght = l_lenght - 1.
  l_caracter = l_cadena+l_lenght(1).

  IF l_caracter NE '\'.
    CLEAR ok_code.
    "Último caracter do diretório de & é diferente de  "\"!
    MESSAGE e043(zfi001) WITH 'Saída'.
  ENDIF.

  "Verifica a existência do diretório de Entrada
  CLEAR g_file_exists.
  CALL METHOD cl_gui_frontend_services=>directory_exist
    EXPORTING
      directory            = g_file_req
    RECEIVING
      result               = g_file_exists
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      wrong_parameter      = 3
      not_supported_by_gui = 4
      OTHERS               = 5.

  IF g_file_exists IS INITIAL.
    CLEAR ok_code.
    "Diretório de & não existe. &
    MESSAGE e044(zfi001) WITH 'Entrada' g_file_req.
  ENDIF.

  "Verifica a existência do diretório de Saída
  CLEAR g_file_exists.
  CALL METHOD cl_gui_frontend_services=>directory_exist
    EXPORTING
      directory            = g_file_resp
    RECEIVING
      result               = g_file_exists
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      wrong_parameter      = 3
      not_supported_by_gui = 4
      OTHERS               = 5.

  IF g_file_exists IS INITIAL.
    CLEAR ok_code.
    "Diretório de & não existe. &
    MESSAGE e044(zfi001) WITH 'Saída' g_file_resp.
  ENDIF.

  "Selecionar clientez balcão
  CLEAR: gt_zsdt003f, gt_zsdt003f[].
  SELECT * INTO TABLE gt_zsdt003f
    FROM zsdt003f.

  CLEAR g_werks.
  SELECT SINGLE werks INTO g_werks
    FROM t001w
    WHERE j_1bbranch EQ t_bupla.

  CLEAR: g_nodetype, g_atacado.
  SELECT SINGLE nodetype INTO g_nodetype
    FROM t001w
    WHERE j_1bbranch EQ t_bupla.

  IF g_nodetype EQ 'DC'.
    g_atacado = 'X'.
  ENDIF.

*** GFX - DFLC - F-PACI209 - Início 12/04/2021
  SELECT SINGLE tipo
    FROM zfit053
    INTO g_nodetype
    WHERE centro = t_bupla.

  IF sy-subrc IS INITIAL AND g_nodetype = 'AT'.

    g_atacado = abap_true.

  ELSEIF sy-subrc IS INITIAL AND g_nodetype <> 'AT'.

    g_atacado = abap_false.

  ENDIF.

  SELECT * INTO TABLE gt_zmmt005
    FROM zmmt005.

*  IF g_nodetype EQ 'DC'.
*    CALL FUNCTION 'POPUP_TO_CONFIRM'
*      EXPORTING
*        titlebar              = TEXT-001 "Aviso de Crédito
*        text_question         = TEXT-002 "Cliente possui crédito, deseja utilizar?
*        text_button_1         = TEXT-003 "Sim
*        icon_button_1         = 'ICON_OKAY'
*        text_button_2         = TEXT-004 "Não
*        icon_button_2         = 'ICON_CANCEL'
*        default_button        = '1'
*        display_cancel_button = ' '
*      IMPORTING
*        answer                = g_answer.
*
*    IF g_answer EQ '1'.
*    ENDIF.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form IMPRIMIR_RECIBO_RECEB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM imprimir_recibo_receb.

  DATA: formname              TYPE tdsfname,
        fm_name               TYPE rs38l_fnam,
        wa_control_parameters TYPE ssfctrlop,         "Estrutura de controle (smartforms)
        wa_output_options     TYPE ssfcompop.         "Estrutura de controle (smartforms)

  DATA: gc_ucomm LIKE sy-ucomm.

  DATA: l_visual,
        l_name_text       TYPE adrp-name_text,
        l_val2dec         TYPE j_1bnflin-netwr,
        l_lenght          TYPE i,
        l_prbruto         TYPE j_1bnflin-netwr,
        l_prunita         TYPE j_1bnflin-netwr,
        l_qtdabs          TYPE ladlg,
        l_campoaux1(132)  TYPE c,
        l_campoaux2(132)  TYPE c,
        l_campoval(25)    TYPE c,
        l_campocpf(14)    TYPE c,
        l_campocnpj(18)   TYPE c,
        l_campocnpj2(18)  TYPE c,
        l_campovlrit(10)  TYPE c,
        l_campoperde(6)   TYPE c,
        l_campomenge(50)  TYPE c,
        l_campomeng7(7)   TYPE c,
        l_campomeins(50)  TYPE c,
        l_camponetpr(50)  TYPE c,
        l_campobruto(10)  TYPE c,
        l_campounita(10)  TYPE c,
        l_campototal(10)  TYPE c,
        l_campovalor(10)  TYPE c,
        l_campotaxval(50) TYPE c,
        l_camponetwr(50)  TYPE c,
        l_campo1(50)      TYPE c,
        l_campo2(50)      TYPE c,
        l_ck              TYPE char100.

*** GFX - SFSS - SD - DEVK918925 - Início - 30/10/2020
  DATA: lv_tribest TYPE j_1bnfstx-taxval,
        lv_tribfed TYPE j_1bnfstx-taxval,
        lv_auxest  TYPE char10,
        lv_auxfed  TYPE char10.
*** GFX - SFSS - SD - DEVK918925 - Fim - 30/10/2020

  CLEAR ge_zfie007.

  CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
    EXPORTING
      branch            = t_bupla
      bukrs             = t_bukrs
    IMPORTING
      address           = ge_address
      branch_data       = ge_branch_data
      cgc_number        = g_cgc_number2
      address1          = ge_address1
    EXCEPTIONS
      branch_not_found  = 1
      address_not_found = 2
      company_not_found = 3
      OTHERS            = 4.

  CLEAR l_name_text.
*  SELECT SINGLE name_text INTO l_name_text
*    FROM usr21
*    INNER JOIN adrp ON
*    usr21~persnumber = adrp~persnumber
*    WHERE bname EQ sy-uname.
  l_name_text = sy-uname.

  SELECT * INTO TABLE gt_vbrp
    FROM vbrp
    WHERE vbeln = g_fatura.

  SELECT * INTO TABLE gt_prcd_elements
  FROM prcd_elements
  WHERE  knumv EQ ge_zfie004-knumv
    AND kschl IN ('ZPVE','ZDVE').

  SORT gt_prcd_elements BY kposn ASCENDING kschl ASCENDING kbetr DESCENDING.

  CLEAR ge_kna1.
  IF NOT ge_zfie004-bstkd_e IS INITIAL.
    l_lenght = strlen( ge_zfie004-bstkd_e ).

    IF l_lenght EQ 14. "CNPJ
      g_cgc_number = ge_zfie004-bstkd_e.
      WRITE g_cgc_number TO l_campocnpj.
      g_cnpj = ge_zfie004-bstkd_e.
      SELECT SINGLE * INTO ge_kna1
        FROM kna1
        WHERE stcd1 = g_cnpj.
    ELSE.              "CPF
      g_cpf_number = ge_zfie004-bstkd_e.
      WRITE g_cpf_number TO l_campocpf.
      l_campocnpj = l_campocpf.
      g_cpf  = ge_zfie004-bstkd_e.
      SELECT SINGLE * INTO ge_kna1
        FROM kna1
        WHERE stcd2 = g_cpf.
    ENDIF.
  ENDIF.

  g_id       = 'TX03'.
  g_language = sy-langu.
  g_name     = ge_zfie004-vgbel.
  g_object   = 'VBBK'.

  CLEAR: gt_line, gt_line[].
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = g_id
      language                = g_language
      name                    = g_name
      object                  = g_object
    TABLES
      lines                   = gt_line
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  CLEAR g_texto_ender.
  LOOP AT gt_line INTO ge_line.
    IF sy-tabix = 1.
      g_texto_ender = ge_line-tdline.
    ELSE.
      CONCATENATE g_texto_ender ge_line-tdline INTO g_texto_ender SEPARATED BY space.
    ENDIF.
  ENDLOOP.

  "Nome da Filial
  CONCATENATE ge_address-name1 '-' ge_address-sortl
    INTO g_linha01 SEPARATED BY space.

  "Endereço da Filial
  g_linha02 =  ge_address-stras.

  CONCATENATE ge_address-ort02 '-' ge_address-ort01 '-' ge_address-regio
    INTO g_linha03 SEPARATED BY space.

  ge_zfie007-filial           = g_linha01.
  ge_zfie007-end1_filial      = g_linha02.
  ge_zfie007-end2_filial      = g_linha03.

  CLEAR: gt_dados, gt_dados[].

*CPF/CNPJ consumidor: 902.561.236-91
  CONCATENATE 'CPF/CNPJ consumidor:' l_campocnpj
      INTO g_linha09 SEPARATED BY space.

*  CONCATENATE 'Destinatario:' l_campocnpj
*      INTO g_linha10 SEPARATED BY space.
*  WRITE g_linha10 TO ge_dados-tdline.
*  APPEND ge_dados TO gt_dados.

  WRITE '--------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE g_nfenum TO l_campo1.
  CONDENSE l_campo1.
  CONCATENATE '               Extrato N.' l_campo1
      INTO g_linha10 SEPARATED BY space.
  WRITE g_linha10 TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE 'CUPOM FISCAL ELETRÔNICO - SAT                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '--------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE g_linha09                                                  TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '--------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE 'IT  COD      DESC                                       ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '   QTD UN      VLUN R$ (VL TR R$)*             VLITEM R$' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '--------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.

  CLEAR: g_contn, g_subtotal, g_totdesc, g_tottaxval.

  LOOP AT gt_vbrp INTO ge_vbrp.

    ADD 1 TO g_contn.

    g_contc = g_contn.

    g_taxval = ge_vbrp-mwsbp.
    ADD g_taxval TO g_tottaxval.

    CLEAR ge_prcd_elements.
    READ TABLE gt_prcd_elements INTO ge_prcd_elements WITH KEY kposn = ge_vbrp-posnr
                                                               kschl = 'ZDVE'.

    g_perdesc  = ge_prcd_elements-kbetr.
    g_desconto = ge_prcd_elements-kwert * -1.
    ADD g_desconto TO g_totdesc.

    CLEAR ge_prcd_elements.
    READ TABLE gt_prcd_elements INTO ge_prcd_elements WITH KEY kposn = ge_vbrp-posnr
                                                               kschl = 'ZPVE'.

    g_vlrvenda = ge_prcd_elements-kwert.

    g_vlritem  = ge_vbrp-netwr + ge_vbrp-mwsbp.
    g_vlritem  = g_vlrvenda.
    ADD g_vlritem TO g_subtotal.

    WRITE g_vlritem TO l_campovlrit.

    g_vlrtotal = g_vlritem - g_desconto.

    WRITE g_vlrtotal TO l_campototal.

    l_qtdabs = ge_vbrp-fkimg.
    WRITE l_qtdabs TO l_campomeng7.

    WRITE ge_vbrp-meins TO l_campomeins.
    CONDENSE l_campomeins.

    l_val2dec = ge_vbrp-netwr / ge_vbrp-fkimg.
*    l_prbruto = g_vlritem / ge_vbrp-fkimg.
    l_prbruto = g_vlrvenda.
    l_prunita = g_vlrtotal / ge_vbrp-fkimg.

    WRITE l_prbruto TO l_campobruto.

    WRITE l_prunita TO l_campounita.

    WRITE l_val2dec TO l_camponetpr.
    CONDENSE l_camponetpr.

    WRITE g_taxval TO l_campotaxval.
    CONDENSE l_campotaxval.

    CONCATENATE '(' l_campotaxval ')' INTO l_campotaxval.
    CONDENSE l_campotaxval.

    WRITE g_vlritem TO l_camponetwr.
    CONDENSE l_camponetwr.

    WRITE g_perdesc TO l_campoperde.
    CONCATENATE l_campoperde '%' INTO l_campoperde.
    g_taxval = ge_vbrp-mwsbp.
    ADD g_taxval TO g_tottaxval.

    WRITE g_perdesc TO l_campoperde.
    CONCATENATE l_campoperde '%' INTO l_campoperde.

    WRITE ge_vbrp-matnr TO l_campo1.
    CONDENSE l_campo1.

    WRITE ge_vbrp-matnr TO l_campo1.
    CONDENSE l_campo1.
    g_linha04       = g_contc.
    g_linha04+4(8)  = l_campo1.
    g_linha04+13(50) = ge_vbrp-arktx.
    WRITE g_linha04 TO ge_dados-tdline.
    APPEND ge_dados TO gt_dados.

    CONCATENATE l_campomeng7 l_campomeins 'X' l_campobruto
                l_campotaxval
      INTO g_linha04 SEPARATED BY space.
    g_linha04+47(10) = l_campovlrit.
    WRITE g_linha04 TO ge_dados-tdline.
    APPEND ge_dados TO gt_dados.

    WRITE g_desconto TO l_camponetwr.
    CONDENSE l_camponetwr.

    g_linha06 = 'Rateio de desconto sobre subtotal         (-)'.
    g_linha06+52(50) = l_camponetwr.
    WRITE g_linha06 TO ge_dados-tdline.
    APPEND ge_dados TO gt_dados.

  ENDLOOP.

  g_perdesc = 100 * ( g_totdesc / g_subtotal ).

  WRITE g_subtotal TO l_campovlrit.
*  CONDENSE l_camponetwr.
  g_linha06 = 'Subtotal                                     '.
  g_linha06+47(10) = l_campovlrit.
  WRITE g_linha06 TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE g_perdesc TO l_camponetwr.
  CONDENSE l_camponetwr.
  CONCATENATE '(' l_camponetwr '%) (-)'
      INTO g_linha07.
  WRITE g_totdesc TO l_camponetwr.
  CONDENSE l_camponetwr.
  g_linha06 = 'Descontos'.
  g_linha06+34(15) = g_linha07.
  g_linha06+51(50) = l_camponetwr.
  WRITE g_linha06 TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  g_vlrtotal = g_subtotal - g_totdesc.
  WRITE g_vlrtotal TO l_campovlrit.
*  CONDENSE l_campovlrit.
  g_linha06 = 'TOTAL R$:                                    '.
  g_linha06+47(10) = l_campovlrit.
  WRITE g_linha06 TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  LOOP AT gt_formpag INTO ge_formpag.

    CASE ge_formpag-dcr_meio_pagamento.
      WHEN 'BOLETO CONVENCIONAL'.
        g_linha06 = 'Boleto Convencional                          '.
      WHEN 'CARTÃO DE CRÉDITO'.
        g_linha06 = 'Cartão de Crédito                            '.
      WHEN 'CARTÃO DE DÉBITO'.
        g_linha06 = 'Cartão de Débito                             '.
      WHEN 'CHEQUE'.
        g_linha06 = 'Cheques                                      '.
      WHEN 'DINHEIRO'.
        g_linha06 = 'Dinheiro                                     '.
      WHEN 'TROCO'.
        g_linha06 = 'Troco                                        '.
      WHEN 'CRÉDITO DO CLIENTE'.
        g_linha06 = 'Credito Loja                                 '.
      WHEN OTHERS.
    ENDCASE.
    WRITE ge_formpag-dmbtr TO l_campovlrit.
    g_linha06+47(10) = l_campovlrit.
    WRITE g_linha06 TO ge_dados-tdline.
    APPEND ge_dados TO gt_dados.

  ENDLOOP.

  IF t_vlr_troco GT 0.
    g_linha06 = 'Troco                                        '.
    WRITE t_vlr_troco TO l_campovlrit.
    g_linha06+47(10) = l_campovlrit.
    WRITE g_linha06 TO ge_dados-tdline.
    APPEND ge_dados TO gt_dados.
  ENDIF.

  WRITE '--------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE 'DADOS PARA ENTREGA                                      ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'Endereco:' g_texto_ender
      INTO g_linha10 SEPARATED BY space.
  WRITE g_linha10 TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'Destinatario:' ge_zfie004-bstkd
      INTO g_linha10 SEPARATED BY space.
  WRITE g_linha10 TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '--------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE 'OBSERVACOES DO CONTRIBUINTE                             ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.

*** GFX - SFSS - SD - DEVK918925 - Início - 30/10/2020
  READ TABLE gt_vbrp ASSIGNING FIELD-SYMBOL(<fs_vbrp>) INDEX 1.
  IF sy-subrc IS INITIAL AND <fs_vbrp> IS ASSIGNED.
    "Busca o estado da loja
    SELECT SINGLE regio
      FROM t001w
      WHERE werks = @<fs_vbrp>-werks
      INTO @DATA(lv_regio).

    "Busca estado para regra de RN
    SELECT SINGLE low
      FROM tvarvc
      WHERE name = 'ZSD_CONV_CFOP'
      INTO @DATA(lv_estado).

    IF ( lv_regio IS NOT INITIAL AND lv_estado IS NOT INITIAL ) AND
       ( lv_regio = lv_estado ).

      "Busca o docnum
      SELECT SINGLE docnum
        FROM j_1bnflin
        WHERE reftyp EQ 'BI'
          AND refkey EQ @<fs_vbrp>-vbeln
          AND refitm EQ @<fs_vbrp>-posnr
        INTO @DATA(lv_docnum).

      IF lv_docnum IS NOT INITIAL.
        "Busca os tributos
        SELECT taxval,
               taxgrp
          FROM j_1bnfstx
          WHERE docnum = @lv_docnum
          INTO TABLE @DATA(lt_trib).

        "Precorre os tributos para somar
        LOOP AT lt_trib ASSIGNING FIELD-SYMBOL(<fs_trib>).
          CASE <fs_trib>-taxgrp.
            WHEN 'ICMS' OR 'ICST'.  "Tributos estaduais: J_1BNFSTX-TAXGRP = “ICMS” ou “ICST”
              ADD <fs_trib>-taxval TO lv_tribest.
            WHEN 'IPI' OR 'PIS' OR 'COFI'.
              ADD <fs_trib>-taxval TO lv_tribfed.
          ENDCASE.
        ENDLOOP.
      ENDIF.

      "Trib. aprox. R$: 99,99 Fed e R$: 99,99 Est e R$: 0,0 Muni (conforme Lei Fed. 12.741/2012)
      lv_auxest = lv_tribest.
      lv_auxfed = lv_tribfed.
      CONDENSE: lv_auxest, lv_auxfed.

      CONCATENATE 'Trib. aprox. R$:' lv_auxfed
                  'Fed e R$:' lv_auxest
                  'Est e R$: 0,00 Muni.'
             INTO g_linha10 SEPARATED BY space.
      WRITE g_linha10 TO ge_dados-tdline.
      APPEND ge_dados TO gt_dados.
      WRITE '(conforme Lei Fed. 12.741/2012)                         ' TO ge_dados-tdline.
      APPEND ge_dados TO gt_dados.

    ENDIF.
  ENDIF.

  IF lv_regio <> lv_estado.
    WRITE 'Valor aproximado dos tributos totais deste CUPOM        ' TO ge_dados-tdline.
    APPEND ge_dados TO gt_dados.
    WRITE g_tottaxval TO l_campo1.
    CONDENSE l_campo1.
    CONCATENATE '(conforme Lei Fed. 12.741/2012) R$:' l_campo1
        INTO g_linha10 SEPARATED BY space.
    WRITE g_linha10 TO ge_dados-tdline.
    APPEND ge_dados TO gt_dados.
  ENDIF.
*** GFX - SFSS - SD - DEVK918925 - Fim - 30/10/2020

  CONCATENATE 'Fatura:' g_fatura
      INTO g_linha10 SEPARATED BY space.
  WRITE g_linha10 TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE 'Msg: DEVOL. APENAS C/ CPF                               ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'Vendedor:' ge_zfie004-name1
      INTO g_linha10 SEPARATED BY space.
  WRITE g_linha10 TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'Operador:' l_name_text
    INTO g_linha10 SEPARATED BY space.
  WRITE g_linha10 TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE 'Cond Pgto: A VISTA                                      ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '--------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                  SAT No. 230046773                     ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.

  WRITE sy-datum TO l_campoaux1.
  WRITE sy-uzeit TO l_campoaux2.
  CONCATENATE l_campoaux1 l_campoaux2
    INTO g_linha10 SEPARATED BY space.
  g_linha11+16(50) = g_linha10.
  WRITE g_linha11 TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.

  CLEAR: g_docnum,
         g_nfenum,
         g_regio,
         g_nfyear,
         g_nfmonth,
         g_stcd1,
         g_model,
         g_serie,
         g_nfnum9,
         g_docnum9,
         g_cdv.
  SELECT SINGLE
    j_1bnfdoc~docnum
    j_1bnfdoc~nfenum
    j_1bnfe_active~regio
    j_1bnfe_active~nfyear
    j_1bnfe_active~nfmonth
    j_1bnfe_active~stcd1
    j_1bnfe_active~model
    j_1bnfe_active~serie
    j_1bnfe_active~nfnum9
    j_1bnfe_active~docnum9
    j_1bnfe_active~cdv
    INTO (g_docnum,
          g_nfenum,
          g_regio,
          g_nfyear,
          g_nfmonth,
          g_stcd1,
          g_model,
          g_serie,
          g_nfnum9,
          g_docnum9,
          g_cdv)
    FROM j_1bnfdoc
    INNER JOIN j_1bnflin      ON j_1bnflin~docnum = j_1bnfdoc~docnum
    INNER JOIN j_1bnfe_active ON j_1bnfe_active~docnum = j_1bnfdoc~docnum
    WHERE j_1bnflin~reftyp EQ 'BI'
      AND j_1bnflin~refkey EQ ge_vbrp-vbeln
      AND j_1bnflin~refitm EQ ge_vbrp-posnr.
*    WHERE j_1bnflin~docnum EQ '0000000122'.

  IF sy-subrc EQ 0.
    g_acckey-regio   = g_regio.
    g_acckey-nfyear  = g_nfyear.
    g_acckey-nfmonth = g_nfmonth.
    g_acckey-stcd1   = g_stcd1.
    g_acckey-model   = g_model.
    g_acckey-serie   = g_serie.
    g_acckey-nfnum   = g_nfnum9.
    g_acckey-docnum  = g_docnum9.
    g_acckey-cdv     = g_cdv.
  ENDIF.

  l_ck = g_acckey.

  CALL FUNCTION 'RKD_WORD_WRAP'
    EXPORTING
      textline            = l_ck
      outputlen           = 4
    TABLES
      out_lines           = gt_lines
    EXCEPTIONS
      outputlen_too_large = 1
      OTHERS              = 2.

  CLEAR l_campoaux1.
  LOOP AT gt_lines INTO ge_lines.

    CONCATENATE l_campoaux1 ge_lines INTO
                l_campoaux1 SEPARATED BY space.

  ENDLOOP.

  CONDENSE l_campoaux1.
  g_linha06 = l_campoaux1.
  WRITE g_linha06 TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.

  ge_zfie007-qrcode           = l_campoaux1.
  ge_zfie007-barcode          = g_acckey.
  g_linha10 = 'QRCODE'.
  WRITE g_linha10 TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.

  WRITE '                                                        ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                        ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                        ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '   Consulte o QR Code pelo aplicativo "De olho na nota" ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '   disponível na AppStore (Apele) e PlayStore (Android) ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.

  l_visual = ' '.

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
      wa_output_options-tdimmed       = 'X'. "nast-dimme.
      wa_output_options-tddelete      = space.
      wa_output_options-tdnewid       = 'X'.
      wa_output_options-tdnoprev      = 'X'.
      wa_control_parameters-langu     = 'PT'.
      wa_control_parameters-no_dialog = 'X'.
      wa_control_parameters-preview   = ''.
  ENDCASE.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZFI_CUPOM_FISCAL_ELETRONICO'
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
      ge_zfie007         = ge_zfie007
    TABLES
      gt_dados           = gt_dados
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

ENDFORM.

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM user_ok_tc USING    p_tc_name TYPE dynfnam
                         p_table_name
                         p_mark_name
                CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA: l_ok     TYPE sy-ucomm,
        l_offset TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  SEARCH p_ok FOR p_tc_name.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  l_offset = strlen( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
  CASE l_ok.
    WHEN 'INSR'.                      "insert row
      PERFORM fcode_insert_row USING    p_tc_name
                                        p_table_name.
      CLEAR p_ok.

    WHEN 'DELE'.                      "delete row
      PERFORM fcode_delete_row USING    p_tc_name
                                        p_table_name
                                        p_mark_name.
      CLEAR p_ok.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM compute_scrolling_in_tc USING p_tc_name
                                            l_ok.
      CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM fcode_tc_mark_lines USING p_tc_name
                                        p_table_name
                                        p_mark_name   .
      CLEAR p_ok.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM fcode_tc_demark_lines USING p_tc_name
                                          p_table_name
                                          p_mark_name .
      CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_insert_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_lines_name       LIKE feld-name.
  DATA l_selline          LIKE sy-stepl.
  DATA l_lastline         TYPE i.
  DATA l_line             TYPE i.
  DATA l_table_name       LIKE feld-name.
  FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
  FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
  ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE l_selline.
  IF sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
    IF l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    ELSE.
      <tc>-top_line = 1.
    ENDIF.
  ELSE.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <table> INDEX l_selline.
  <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE l_line.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_delete_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name
                       p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <table> LINES <tc>-lines.

  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    IF <mark_field> = 'X'.
      DELETE <table> INDEX syst-tabix.
      IF sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc USING    p_tc_name
                                      p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_tc_new_top_line     TYPE i.
  DATA l_tc_name             LIKE feld-name.
  DATA l_tc_lines_name       LIKE feld-name.
  DATA l_tc_field_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
  ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
  IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
    l_tc_new_top_line = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        entry_act      = <tc>-top_line
        entry_from     = 1
        entry_to       = <tc>-lines
        last_page_full = 'X'
        loops          = <lines>
        ok_code        = p_ok
        overlapping    = 'X'
      IMPORTING
        entry_new      = l_tc_new_top_line
      EXCEPTIONS
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO    = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        OTHERS         = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD l_tc_field_name
             AREA  l_tc_name.

  IF syst-subrc = 0.
    IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD l_tc_field_name LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_mark_lines USING p_tc_name
                               p_table_name
                               p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_demark_lines USING p_tc_name
                                 p_table_name
                                 p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = space.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines
*&---------------------------------------------------------------------*
*& Form BUSCAR_DADOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM buscar_dados .

  SELECT DISTINCT
    lips~vgbel
    lips~vbeln
    vbak~auart
    vbak~netwr
    vbak~erdat
    vbak~erzet
    vbak~knumv
    vbkd~ihrez
    vbkd~bstkd
    vbkd~bstkd_e
    vbkd~zlsch
    vbkd~zterm
    vbpa~lifnr
    lfa1~name1
    zfit002a~tp_doc_fiscal
    tvzbt~vtext
    t001w~land1
    t001w~regio
    vbak~vbeln AS vbeln_vbak
    INTO CORRESPONDING FIELDS OF TABLE gt_zfie004
    FROM vbak
    INNER JOIN vbap     ON vbap~vbeln     = vbak~vbeln
    INNER JOIN lips     ON lips~vgbel     = vbap~vbeln
                       AND lips~vgpos     = vbap~posnr
    INNER JOIN likp     ON likp~vbeln     = lips~vbeln
    INNER JOIN t001w    ON t001w~werks    = vbap~werks
    INNER JOIN vbkd     ON vbkd~vbeln     = vbak~vbeln
    INNER JOIN vbpa     ON vbpa~vbeln     = vbak~vbeln
    INNER JOIN lfa1     ON lfa1~lifnr     = vbpa~lifnr
    INNER JOIN zfit002a ON zfit002a~auart = vbak~auart
    INNER JOIN tvzbt    ON tvzbt~zterm    = vbkd~zterm
    WHERE likp~fkstk       NE 'C' AND likp~fkstk NE ''
      AND likp~wbstk       EQ 'C'
      AND likp~lfart       EQ 'LF'
      AND t001w~j_1bbranch EQ t_bupla
      AND zfit002a~bupla   EQ t_bupla
      AND vbak~augru       EQ ''
      AND vbak~audat       EQ t_datum
      AND vbpa~parvw       EQ 'ZV'
      AND vbkd~posnr       EQ '000000'
      AND tvzbt~spras      EQ sy-langu.

  CHECK sy-subrc EQ 0.

*** GFX - JBRS - SD - DEVK925111 - Inicio - 08/02/2022
  SELECT ordem
    FROM zsdt039
    INTO TABLE @DATA(lt_zsdt039)
    FOR ALL ENTRIES IN @gt_zfie004
    WHERE ordem = @gt_zfie004-vbeln_vbak.

  IF sy-subrc = 0.

    LOOP AT lt_zsdt039 ASSIGNING FIELD-SYMBOL(<fs_zsdt039>).

      DELETE gt_zfie004 WHERE vbeln_vbak = <fs_zsdt039>-ordem.

    ENDLOOP.

  ENDIF.

  CHECK gt_zfie004[] IS NOT INITIAL.
*** GFX - JBRS - SD - DEVK925111 - Fim - 08/02/2022

  SORT gt_zfie004 BY vgbel.

  SELECT * INTO TABLE gt_t042z
    FROM t042z
    WHERE land1 EQ 'BR'.

  SELECT DISTINCT
    vbpa~vbeln
    vbpa~kunnr
    kna1~name1
    INTO TABLE gt_vbpa
    FROM vbpa
    INNER JOIN kna1  ON kna1~kunnr  = vbpa~kunnr
    FOR ALL ENTRIES IN gt_zfie004
    WHERE vbpa~vbeln EQ gt_zfie004-vgbel
      AND vbpa~parvw EQ 'AG'.

  SELECT vbap~vbeln vbap~posnr vbap~netwr vbap~mwsbp vbap~kwmeng lips~lfimg vbap~mfrgr "GFX - LUVA - F-PACI33
    INTO TABLE gt_vbap
    FROM vbap
    INNER JOIN lips  ON lips~vgbel EQ vbap~vbeln
                    AND lips~vgpos EQ vbap~posnr
    FOR ALL ENTRIES IN gt_zfie004
    WHERE vbap~vbeln EQ gt_zfie004-vgbel.
*      AND vbap~abgru EQ ''.

  SELECT *
    INTO TABLE gt_tmfgt
    FROM tmfgt
    FOR ALL ENTRIES IN gt_vbap
    WHERE spras = sy-langu AND
          mfrgr = gt_vbap-mfrgr. "GFX - MSSF - F-PACI33

  SORT gt_vbap BY vbeln posnr.

  SELECT * INTO TABLE gt_prcd_elements
    FROM prcd_elements
    FOR ALL ENTRIES IN gt_zfie004
    WHERE knumv EQ gt_zfie004-knumv
      AND kschl IN ('ZPVE','ZDVE').

  SORT gt_prcd_elements BY knumv kposn.
  DELETE gt_prcd_elements WHERE kbetr EQ 0.

  LOOP AT gt_zfie004 INTO ge_zfie004.

    CLEAR ge_vbpa.
    READ TABLE gt_vbpa INTO ge_vbpa WITH KEY vbeln = ge_zfie004-vgbel.

    ge_zfie004-kunnr = ge_vbpa-kunnr.

    CLEAR ge_zsdt003f.
    READ TABLE gt_zsdt003f INTO ge_zsdt003f WITH KEY werks = g_werks
                                                     kunnr = ge_zfie004-kunnr.

    IF sy-subrc IS INITIAL.
*   IF ge_zfie004-bstkd_e IS NOT INITIAL.
      ge_zfie004-name2 = ge_zfie004-bstkd.
    ELSE.
      ge_zfie004-name2 = ge_vbpa-name1.
    ENDIF.

    CLEAR: ge_t042z.
    READ TABLE gt_t042z INTO ge_t042z WITH KEY zlsch = ge_zfie004-zlsch.

    ge_zfie004-text1 = ge_t042z-text1.

    IF  ge_zfie004-name2 CS filtro_nome  AND
        ge_zfie004-text1 CS filtro_forma AND
        ge_zfie004-vtext CS filtro_condpag.
      "ok
    ELSE.
      DELETE gt_zfie004.
      CONTINUE.
    ENDIF.

    CLEAR: ge_zfie004-netwr, g_kwmeng, g_lfimg.
    LOOP AT gt_vbap INTO ge_vbap WHERE vbeln EQ ge_zfie004-vgbel.

      ADD ge_vbap-netwr TO ge_zfie004-netwr.
      ADD ge_vbap-mwsbp TO ge_zfie004-netwr.

      ADD ge_vbap-kwmeng TO g_kwmeng.
      ADD ge_vbap-lfimg  TO g_lfimg.

      LOOP AT gt_tmfgt INTO ge_tmfgt WHERE mfrgr = ge_vbap-mfrgr.
        MOVE ge_tmfgt-bezei TO ge_zfie004-bezei.
      ENDLOOP.  "GFX- MSSF - F-PACI33

    ENDLOOP.

    "Verifica se em alguma remessa existe algum item com qtd = 0
    IF g_lfimg IS INITIAL.
*      DELETE gt_zfie004.
      CONTINUE.
    ENDIF.

    "Se a quantidade da ordem for diferente da remessa, fazer uma simulação  de
    "faturamento para buscar o valor a ser faturado
    IF g_kwmeng NE g_lfimg.
      CLEAR: gt_billingdatain,     gt_billingdatain[],
             gt_return1,           gt_return1[],
             gt_success,           gt_success[],
             gt_errors,            gt_errors[].

      CLEAR: ge_billingdatain,
             ge_return1,
             ge_success,
             gt_errors.

      CLEAR: g_fatura, g_erro.

      ge_billingdatain-ref_doc    = ge_zfie004-vbeln.
      ge_billingdatain-ref_doc_ca = 'J'.
      APPEND ge_billingdatain TO gt_billingdatain.

      CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'
        EXPORTING
          testrun       = 'X'
        TABLES
          billingdatain = gt_billingdatain
          return        = gt_return1
          errors        = gt_errors
          success       = gt_success.

      CLEAR ge_success.
      READ TABLE gt_success INTO ge_success INDEX 1.

      CLEAR ge_zfie004-netwr..
      IF sy-subrc EQ 0.
        ADD ge_success-net_value TO ge_zfie004-netwr.
        ADD ge_success-tax_value TO ge_zfie004-netwr.
      ENDIF.
    ENDIF.

    IF ge_zfie004-tp_doc_fiscal EQ 'DV'.
      ge_zfie004-netwr = ge_zfie004-netwr * - 1.
    ENDIF.

    CLEAR: g_totdesc, g_vlrtotal, ge_zfie004-corte.
    LOOP AT gt_prcd_elements INTO ge_prcd_elements WHERE knumv EQ ge_zfie004-knumv.

      READ TABLE gt_vbap INTO ge_vbap WITH KEY vbeln = ge_zfie004-vgbel
                                               posnr = ge_prcd_elements-kposn.

      IF sy-subrc NE 0.
        ge_zfie004-corte = 'X'.
        CONTINUE.
      ENDIF.

      IF ge_vbap-kwmeng NE ge_vbap-lfimg.
        ge_zfie004-corte = 'X'.
*        CONTINUE.
      ENDIF.

      IF ge_prcd_elements-kschl = 'ZDVE'.
*        g_desconto = ge_prcd_elements-kwert * -1.
*        g_desconto = ( ge_prcd_elements-kbetr / 10 ) * ge_vbap-lfimg * -1.
        g_desconto = ge_prcd_elements-kbetr * ( ge_vbap-lfimg / ge_vbap-kwmeng ) * -1.
        ADD g_desconto TO g_totdesc.
      ENDIF.

      IF ge_prcd_elements-kschl = 'ZPVE'.
*        g_vlrvenda = ge_prcd_elements-kwert.
        g_vlrvenda = ge_prcd_elements-kbetr * ge_vbap-lfimg.
        ADD g_vlrvenda TO g_vlrtotal.
      ENDIF.

    ENDLOOP.

    ge_zfie004-valor_bruto   = g_vlrtotal.
    ge_zfie004-desconto      = g_totdesc.
    IF g_vlrtotal NE 0.
      ge_zfie004-perc_desconto = ( g_totdesc / g_vlrtotal ) * 100.
    ENDIF.

    MODIFY gt_zfie004 FROM ge_zfie004.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form RECEBER_PAGAMENTO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM receber_pagamento .

  CLEAR: g_ident_cartao, gt_zfie011, gt_zfie011[], gt_cartao, gt_cartao[], gt_imprcartao, gt_imprcartao[].
  g_operacao  = 'VEND'.

  CLEAR: gt_f4_meio_pagto, gt_f4_meio_pagto[].

  SELECT dcr_meio_pagamento
    FROM zfit008
    INTO CORRESPONDING FIELDS OF TABLE gt_f4_meio_pagto
    WHERE bukrs EQ t_bukrs
      AND bupla EQ t_bupla
      AND caixa EQ t_caixa.

  SORT gt_f4_meio_pagto BY dcr_meio_pagamento.

  READ TABLE gt_zfie004 INTO ge_zfie004 WITH KEY marc = 'X'.

  IF sy-subrc NE 0 .
    "Selecionar uma venda!
    MESSAGE e008(zfi001).
  ENDIF.

  CLEAR: gt_zfit006, gt_zfit006[].
  SELECT  * INTO TABLE gt_zfit006
    FROM zfit006
    WHERE bukrs    EQ t_bukrs
      AND bupla    EQ t_bupla
      AND caixa    EQ t_caixa
      AND bname    EQ sy-uname
      AND data     EQ sy-datum.

  CLEAR: ge_zfit006.
  READ TABLE gt_zfit006 INTO ge_zfit006 WITH KEY operacao = 'FECH'.

  IF sy-subrc EQ 0.
    CLEAR ok_code.
    "Caixa & fechado para recebimento!
    MESSAGE e009(zfi001) WITH t_caixa.
  ENDIF.

  CASE ge_zfie004-zlsch.
    WHEN 'A'.
      g_dcr_meio_pagamento = 'BOLETO'.
      PERFORM lancar_recebimento_direto.

*** GFX - SFSS - DEVK915251 - 18.11.2019 - Início
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 95
          text       = 'Finalizando recebimento...'.
*** GFX - SFSS - DEVK915251 - 18.11.2019 - Fim

      PERFORM buscar_dados.
    WHEN 'F' OR 'D' OR 'V' OR '1'. "APPC - DAM - Adição da forma de pagamento 1 - 05/05/2023
      g_dcr_meio_pagamento = 'DUPLICATA'.
      PERFORM lancar_recebimento_duplicata.

*** GFX - SFSS - DEVK915251 - 18.11.2019 - Início
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 95
          text       = 'Finalizando recebimento...'.
*** GFX - SFSS - DEVK915251 - 18.11.2019 - Fim

      PERFORM buscar_dados.
    WHEN OTHERS.
      PERFORM executar_recebimento.
      PERFORM buscar_dados.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GRAVAR_TABELAS_RECEB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM gravar_tabelas_receb.

  DATA: l_cont TYPE i.

*  g_valuecnf    = g_value.
*  g_valueqrcode = g_value.
*  g_valueid     = g_value.

  g_nfenum = g_valuecnf.

  CLEAR: gt_zfit006, gt_zfit006[], ge_zfit006,
         gt_cct006, gt_cct006[], l_cont.
  CASE ge_zfie004-zlsch.
    WHEN 'A' OR 'F' OR 'D' OR '1'. "APPC - DAM - Adição da forma de pagamento 1 - 05/05/2023
      ADD 1 TO l_cont.

      CLEAR ge_zfit006.
      ge_zfit006-bukrs              = t_bukrs.
      ge_zfit006-bupla              = t_bupla.
      ge_zfit006-caixa              = t_caixa.
      ge_zfit006-operacao           = g_operacao.
      ge_zfit006-data               = sy-datum.
      ge_zfit006-vbeln              = ge_zfie004-vgbel.
      ge_zfit006-dcr_meio_pagamento = g_dcr_meio_pagamento.
      ge_zfit006-item               = l_cont.
      ge_zfit006-belnr              = g_belnr.
      ge_zfit006-gjahr              = g_gjahr.
      ge_zfit006-bname              = sy-uname.
      ge_zfit006-vbeln_vf           = g_fatura.
      ge_zfit006-docnum             = g_docnum.
      ge_zfit006-nfenum             = g_nfenum.
      ge_zfit006-tp_doc_fiscal      = ge_zfie004-tp_doc_fiscal.
      ge_zfit006-zlsch              = ge_zfie004-zlsch.
      ge_zfit006-valor              = ge_zfie004-netwr.
      ge_zfit006-num_apr_cheque     = ge_formpag-num_apr_cheque.
      ge_zfit006-cmc7               = ge_formpag-cmc7.
      ge_zfit006-data_bom_para      = ge_formpag-data_bom_para.
      ge_zfit006-tp_pagto_cartao    = ge_formpag-tp_pagto_cartao.
      ge_zfit006-cartao             = ge_formpag-cartao.
      ge_zfit006-modalidade         = ge_formpag-modalidade.
      ge_zfit006-parcelas           = ge_formpag-parcelas.
      ge_zfit006-apr_pos            = ge_formpag-apr_pos.
      ge_zfit006-estabelecimento    = ge_formpag-estabelecimento.
      ge_zfit006-serial             = ge_formpag-serial.
      ge_zfit006-valor_bruto        = ge_zfie004-valor_bruto.
      ge_zfit006-desconto           = ge_zfie004-desconto.
      ge_zfit006-hora               = sy-uzeit.
      ge_zfit006-datum              = sy-datum.
      ge_zfit006-acckey             = g_valueid.
      ge_zfit006-qrcode             = g_valueqrcode.
      ge_zfit006-xml                = lv_xml.
      ge_zfit006-nsu_host           = ge_formpag-nsu_host. "APPC - DAM - Passagem do NSU - 05/05/2023

      IF ge_formpag-nsu_host IS NOT INITIAL.
        ge_zfit006-nsu_host = ge_formpag-nsu_host.
      ELSE.
        ge_zfit006-nsu_host = ge_zfit006-apr_pos.
      ENDIF.

**     Início - GFX - NIIS - F-PACI131 - Ajuste POS Debito  - caixa - 28/07/2020
*      IF ge_formpag-cod_adq         = 'REDE' AND
*         ge_zfit006-tp_pagto_cartao = 'POS'  AND
*         ge_zfit006-modalidade      = 'D'.
*
*        ge_zfit006-nsu_host = ge_zfit006-apr_pos.
*        ge_zfit006-apr_pos  = ''.
*
*      ENDIF.
**     Fim    - GFX - NIIS - F-PACI131 - Ajuste POS Debito  - caixa - 28/07/2020

*        Inicio - GFX - JECA - F-PACI171 - ajuste de NSU HOST no TEF - caixa - 20/11/2020
      SHIFT  ge_zfit006-nsu_host LEFT DELETING LEADING '0'.
      IF  strlen( ge_zfit006-cod_autorizacao ) < 6.
        CONCATENATE '0' ge_zfit006-cod_autorizacao INTO ge_zfit006-cod_autorizacao.
      ENDIF.
*        Fim - GFX - JECA - F-PACI171 - ajuste de NSU HOST no TEF - caixa - 20/11/2020

      APPEND ge_zfit006 TO gt_zfit006.

    WHEN OTHERS.
      LOOP AT gt_formpag INTO ge_formpag.
        ADD 1 TO l_cont.

        CLEAR ge_zfit006.
        ge_zfit006-bukrs              = t_bukrs.
        ge_zfit006-bupla              = t_bupla.
        ge_zfit006-caixa              = t_caixa.
        ge_zfit006-operacao           = g_operacao.
        ge_zfit006-data               = sy-datum.
        ge_zfit006-vbeln              = ge_zfie004-vgbel.
        ge_zfit006-dcr_meio_pagamento = ge_formpag-dcr_meio_pagamento.
        ge_zfit006-item               = l_cont.
        ge_zfit006-belnr              = g_belnr.
        ge_zfit006-gjahr              = g_gjahr.
        ge_zfit006-bname              = sy-uname.
        ge_zfit006-vbeln_vf           = g_fatura.
        ge_zfit006-docnum             = g_docnum.
        ge_zfit006-nfenum             = g_nfenum.
        ge_zfit006-tp_doc_fiscal      = ge_zfie004-tp_doc_fiscal.
        ge_zfit006-zlsch              = ge_zfie004-zlsch.
        IF ge_formpag-dcr_meio_pagamento EQ 'DINHEIRO'.
          ge_zfit006-valor              = ge_formpag-dmbtr - t_vlr_troco.
        ELSE.
          ge_zfit006-valor              = ge_formpag-dmbtr.
        ENDIF.
        ge_zfit006-num_apr_cheque     = ge_formpag-num_apr_cheque.
        ge_zfit006-cmc7               = ge_formpag-cmc7.
        ge_zfit006-data_bom_para      = ge_formpag-data_bom_para.
        ge_zfit006-tp_pagto_cartao    = ge_formpag-tp_pagto_cartao.
        ge_zfit006-modalidade         = ge_formpag-modalidade.
        ge_zfit006-cartao             = ge_formpag-cartao.
        ge_zfit006-cartao_tef         = ge_formpag-cartao_tef.
        ge_zfit006-cartao_c2d_ok      = ge_formpag-cartao_c2d_ok.
        ge_zfit006-parcelas           = ge_formpag-parcelas.
        ge_zfit006-apr_pos            = ge_formpag-apr_pos.
        ge_zfit006-cod_adq            = ge_formpag-cod_adq.
        ge_zfit006-estabelecimento    = ge_formpag-estabelecimento.
        ge_zfit006-serial             = ge_formpag-serial.
        ge_zfit006-num_transacao_nsu  = ge_formpag-num_transacao_nsu.
        ge_zfit006-cod_autorizacao    = ge_formpag-cod_autorizacao.
        ge_zfit006-cliente            = ge_formpag-cliente.
        ge_zfit006-valor_bruto        = ge_zfie004-valor_bruto.
        ge_zfit006-desconto           = ge_zfie004-desconto.
        ge_zfit006-hora               = sy-uzeit.
        ge_zfit006-datum              = sy-datum.
        ge_zfit006-acckey             = g_valueid.
        ge_zfit006-qrcode             = g_valueqrcode.
        ge_zfit006-xml                = lv_xml.
        ge_zfit006-nsu_host           = ge_formpag-nsu_host. "APPC - DAM - Passagem do NSU - 05/05/2023

        IF ge_formpag-nsu_host IS NOT INITIAL.
          ge_zfit006-nsu_host = ge_formpag-nsu_host.
        ELSE.
          ge_zfit006-nsu_host = ge_zfit006-apr_pos.
        ENDIF.

**     Início - GFX - NIIS - F-PACI131 - Ajuste POS Debito  - caixa - 28/07/2020
*        IF ge_formpag-cod_adq         = 'REDE' AND
*           ge_zfit006-tp_pagto_cartao = 'POS'  AND
*           ge_zfit006-modalidade      = 'D'.
*
*          ge_zfit006-nsu_host = ge_zfit006-apr_pos.
*          ge_zfit006-apr_pos  = ''.
*
*        ENDIF.
**     Fim    - GFX - NIIS - F-PACI131 - Ajuste POS Debito  - caixa - 28/07/2020

*        Inicio - GFX - JECA - F-PACI171 - ajuste de NSU HOST no TEF - caixa - 20/11/2020
        SHIFT  ge_zfit006-nsu_host LEFT DELETING LEADING '0'.
        IF  strlen( ge_zfit006-cod_autorizacao ) < 6.
          CONCATENATE '0' ge_zfit006-cod_autorizacao INTO ge_zfit006-cod_autorizacao.
        ENDIF.
*        Fim - GFX - JECA - F-PACI171 - ajuste de NSU HOST no TEF - caixa - 20/11/2020

        APPEND ge_zfit006 TO gt_zfit006.

      ENDLOOP.

      CLEAR ge_formpag.

      IF t_vlr_troco GT 0.
        ADD 1 TO l_cont.

        CLEAR ge_zfit006.
        ge_zfit006-bukrs              = t_bukrs.
        ge_zfit006-bupla              = t_bupla.
        ge_zfit006-caixa              = t_caixa.
        ge_zfit006-operacao           = g_operacao.
        ge_zfit006-data               = sy-datum.
        ge_zfit006-vbeln              = ge_zfie004-vgbel.
        ge_zfit006-dcr_meio_pagamento = 'TROCO'.
        ge_zfit006-item               = l_cont.
        ge_zfit006-belnr              = g_belnr.
        ge_zfit006-gjahr              = g_gjahr.
        ge_zfit006-bname              = sy-uname.
        ge_zfit006-vbeln_vf           = g_fatura.
        ge_zfit006-docnum             = g_docnum.
        ge_zfit006-nfenum             = g_nfenum.
        ge_zfit006-tp_doc_fiscal      = ge_zfie004-tp_doc_fiscal.
        ge_zfit006-zlsch              = ge_zfie004-zlsch.
        ge_zfit006-valor              = t_vlr_troco.
        ge_zfit006-num_apr_cheque     = ''.
        ge_zfit006-tp_pagto_cartao    = ''.
        ge_zfit006-data_bom_para      = ''.
        ge_zfit006-tp_pagto_cartao    = ''.
        ge_zfit006-modalidade         = ''.
        ge_zfit006-cartao             = ''.
        ge_zfit006-parcelas           = ''.
        ge_zfit006-apr_pos            = ''.
        ge_zfit006-hora               = sy-uzeit.
        ge_zfit006-datum              = sy-datum.
        ge_zfit006-acckey             = g_valueid.
        ge_zfit006-qrcode             = g_valueqrcode.
        ge_zfit006-xml                = lv_xml.

        APPEND ge_zfit006 TO gt_zfit006.
      ENDIF.

      "Grava a contabilização na tabela do C2D apenas quando não utilizar a nova conciliação
      IF g_nova_conc IS INITIAL.

        LOOP AT gt_zfie011 INTO ge_zfie011.

          CLEAR ge_cct006.
          ge_cct006-empresa         = t_bukrs.
          ge_cct006-filial          = t_bupla.
          ge_cct006-cliente         = ge_zfie011-cliente.
          ge_cct006-num_documento   = ge_zfie011-belnr.
          ge_cct006-item            = ge_zfie011-buzei.
          ge_cct006-ano             = ge_zfie011-gjahr.
          ge_cct006-num_parcela     = ge_zfie011-num_parcela.
          ge_cct006-tot_parcelas    = ge_zfie011-parcelas.
          ge_cct006-chave_bandeira  = ge_zfie011-chave_bandeira.
          ge_cct006-cod_aut         = ge_zfie011-apr_pos.
          ge_cct006-data_venda      = ge_zfie011-data_venda.
          ge_cct006-valor_venda     = ge_zfie011-valor_parcela.
          ge_cct006-nsu             = ge_zfie011-num_transacao_nsu.
          ge_cct006-nsu_host        = ge_zfie011-nsu_host.

*        Inicio - GFX - JECA - F-PACI171 - ajuste de NSU HOST no TEF - caixa - 20/11/2020
          SHIFT  ge_cct006-nsu_host LEFT DELETING LEADING '0'.

          IF  strlen( ge_cct006-cod_aut ) < 6.
            CONCATENATE '0' ge_cct006-cod_aut INTO ge_cct006-cod_aut.
          ENDIF.

*        Fim - GFX - JECA - F-PACI171 - ajuste de NSU HOST no TEF - caixa - 20/11/2020

*       Início - GFX - NIIS - F-PACI131 - Ajuste POS Debito  - caixa - 28/07/2020
          IF ge_zfie011-cod_adq         = 'REDE' AND
             ge_zfie011-tp_pagto_cartao = 'POS'  AND
             ge_zfie011-modalidade      = 'D'.

            ge_cct006-nsu_host = ge_cct006-cod_aut.
            ge_cct006-cod_aut  = ''.

          ENDIF.
*       Fim    - GFX - NIIS - F-PACI131 - Ajuste POS Debito  - caixa - 28/07/2020

          APPEND ge_cct006 TO gt_cct006.

        ENDLOOP.

      ENDIF.

  ENDCASE.

  IF gt_zfit006 IS NOT INITIAL.
    MODIFY zfit006 FROM TABLE gt_zfit006.
    COMMIT WORK.
  ENDIF.
  IF gt_cct006 IS NOT INITIAL.
    MODIFY /gfxdsi/cct006 FROM TABLE gt_cct006.
    COMMIT WORK.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GRAVAR_TABELAS_DEVO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM gravar_tabelas_devo.

  DATA: l_cont TYPE i.
*
* g_doc_contabil -> gerado pela FB05

  CLEAR: gt_zfit006, gt_zfit006[], l_cont.
  LOOP AT gt_devol INTO ge_devol WHERE marc = 'X'.
    CLEAR ge_zfit006.
    ADD 1 TO l_cont.

    ge_zfit006-bukrs              = t_bukrs.
    ge_zfit006-bupla              = t_bupla.
    ge_zfit006-caixa              = t_caixa.
    ge_zfit006-operacao           = g_operacao.
    ge_zfit006-data               = sy-datum.
    ge_zfit006-vbeln              = ''.
    ge_zfit006-dcr_meio_pagamento = 'DINHEIRO'.
    ge_zfit006-item               = l_cont.
    ge_zfit006-belnr              = g_belnr.
    ge_zfit006-gjahr              = g_gjahr.
    ge_zfit006-bname              = sy-uname.
    ge_zfit006-vbeln_vf           = ''.
    ge_zfit006-nfenum             = ''.
    ge_zfit006-tp_doc_fiscal      = ''.
    ge_zfit006-zlsch              = ''.
    IF g_operacao = 'DEVO'.
      ge_zfit006-valor              = t_vlr_tot_a_devolver.
    ELSE.
      ge_zfit006-valor              = ge_devol-dmbtr2.
    ENDIF.
    ge_zfit006-num_apr_cheque     = ''.
    ge_zfit006-tp_pagto_cartao    = ''.
    ge_zfit006-cartao             = ''.
    ge_zfit006-parcelas           = ''.
    ge_zfit006-apr_pos            = ''.
*Gravar Referencia
    ge_zfit006-vbeln_ori          = ge_devol-vbeln_ori.
    ge_zfit006-vbeln_vf_ori       = ge_devol-vbeln_vf_ori.

    ge_zfit006-valor_ori          = ge_devol-dmbtr3.
    ge_zfit006-hora               = sy-uzeit.
    ge_zfit006-datum              = sy-datum.

    APPEND ge_zfit006 TO gt_zfit006.
  ENDLOOP.

  IF gt_zfit006 IS NOT INITIAL.
    MODIFY zfit006 FROM TABLE gt_zfit006.
    COMMIT WORK.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GRAVAR_TABELAS_DEVO_TOT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM gravar_tabelas_devo_tot.

  DATA: l_cont TYPE i.

  gt_zfit006aux[] = gt_zfit006[].

  CLEAR: gt_zfit006, gt_zfit006[], l_cont.
  LOOP AT gt_devol INTO ge_devol WHERE belnr = g_belnr_dev
                                   AND icon  = '@0V@'.
    CLEAR ge_zfit006.
    READ TABLE gt_zfit006aux INTO ge_zfit006 WITH KEY vbeln = ge_devol-vbeln_ori
                                                      item  = ge_devol-item.

    IF sy-subrc EQ 0.
      ge_zfit006-estorno            = 'X'.

      APPEND ge_zfit006 TO gt_zfit006.
    ENDIF.

    ADD 1 TO l_cont.

    ge_zfit006-bukrs              = t_bukrs.
    ge_zfit006-bupla              = t_bupla.
    ge_zfit006-caixa              = t_caixa.
    ge_zfit006-operacao           = g_operacao.
    ge_zfit006-data               = sy-datum.
    ge_zfit006-vbeln              = ''.
    ge_zfit006-dcr_meio_pagamento = ge_devol-dcr_meio_pagamento.
    ge_zfit006-item               = l_cont.
    ge_zfit006-belnr              = g_belnr.
    ge_zfit006-gjahr              = g_gjahr.
    ge_zfit006-bname              = sy-uname.
    ge_zfit006-vbeln_vf           = ge_devol-vbeln_vf.
    ge_zfit006-valor              = ge_devol-dmbtr2.
    ge_zfit006-tp_pagto_cartao    = ge_devol-tp_pagto_cartao.
    ge_zfit006-cartao             = ge_devol-cartao.
    ge_zfit006-apr_pos            = ge_devol-apr_pos.
    ge_zfit006-vbeln_ori          = ge_devol-vbeln_ori.
    ge_zfit006-vbeln_vf_ori       = ge_devol-vbeln_vf_ori.
    ge_zfit006-valor_ori          = ge_devol-dmbtr3.
    ge_zfit006-hora               = sy-uzeit.
    ge_zfit006-datum              = sy-datum.
    ge_zfit006-estorno            = ''.

*        Inicio - GFX - JECA - F-PACI171 - ajuste de NSU HOST no TEF - caixa - 20/11/2020
    SHIFT  ge_zfit006-nsu_host LEFT DELETING LEADING '0'.

    IF  strlen( ge_zfit006-cod_autorizacao ) < 6.
      CONCATENATE '0' ge_zfit006-cod_autorizacao INTO ge_cct006-cod_aut.
    ENDIF.

*        Fim - GFX - JECA - F-PACI171 - ajuste de NSU HOST no TEF - caixa - 20/11/2020

    APPEND ge_zfit006 TO gt_zfit006.

    LOOP AT gt_cct006 INTO ge_cct006 WHERE cod_aut EQ ge_zfit006-apr_pos.

      ge_cct006-status =  'ES'.
      ge_cct006-num_estorno = g_belnr.
      ge_cct006-ano_estorno = g_gjahr.

      MODIFY gt_cct006 FROM ge_cct006.

    ENDLOOP.

  ENDLOOP.

  IF gt_zfit006 IS NOT INITIAL.
    MODIFY zfit006 FROM TABLE gt_zfit006.
    COMMIT WORK.
  ENDIF.
  IF gt_cct006 IS NOT INITIAL.
    MODIFY /gfxdsi/cct006 FROM TABLE gt_cct006.
    COMMIT WORK.
  ENDIF.

  UPDATE zmfetf001
    SET estornado = 'X'
    WHERE bukrs  = t_bukrs
      AND bupla  = t_bupla
      AND docnum = ge_devol-docnum.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GRAVAR_TABELAS_ADIA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM gravar_tabelas_adia .

  DATA: l_cont TYPE i.
*
* g_doc_contabil -> gerado pela FB05

  CLEAR: gt_zfit006, gt_zfit006[], ge_zfit006, l_cont.
  LOOP AT gt_formpag INTO ge_formpag.
    ADD 1 TO l_cont.

    ge_zfit006-bukrs              = t_bukrs.
    ge_zfit006-bupla              = t_bupla.
    ge_zfit006-caixa              = t_caixa.
    ge_zfit006-operacao           = g_operacao.
    ge_zfit006-data               = sy-datum.
    ge_zfit006-vbeln              = g_belnr.
    ge_zfit006-dcr_meio_pagamento = ge_formpag-dcr_meio_pagamento.
    ge_zfit006-item               = l_cont.
    ge_zfit006-belnr              = g_belnr.
    ge_zfit006-gjahr              = g_gjahr.
    ge_zfit006-bname              = sy-uname.
    ge_zfit006-vbeln_vf           = ''.
    ge_zfit006-nfenum             = ''.
    ge_zfit006-tp_doc_fiscal      = ''.
    ge_zfit006-zlsch              = ''.
    IF ge_formpag-dcr_meio_pagamento EQ 'DINHEIRO'.
      ge_zfit006-valor              = ge_formpag-dmbtr - t_vlr_troco.
    ELSE.
      ge_zfit006-valor              = ge_formpag-dmbtr.
    ENDIF.
    ge_zfit006-num_apr_cheque     = ge_formpag-num_apr_cheque.
    ge_zfit006-tp_pagto_cartao    = ge_formpag-tp_pagto_cartao.
    ge_zfit006-modalidade         = ge_formpag-modalidade.
    ge_zfit006-cartao             = ge_formpag-cartao.
    ge_zfit006-cartao_tef         = ge_formpag-cartao_tef.
    ge_zfit006-cartao_c2d_ok      = ge_formpag-cartao_c2d_ok.
    ge_zfit006-parcelas           = ge_formpag-parcelas.
    ge_zfit006-apr_pos            = ge_formpag-apr_pos.
    ge_zfit006-cod_adq            = ge_formpag-cod_adq.
    ge_zfit006-cliente            = ge_formpag-cliente.
    ge_zfit006-valor_bruto        = ge_zfit006-valor.
    ge_zfit006-desconto           = 0.
    ge_zfit006-hora               = sy-uzeit.
    ge_zfit006-datum              = sy-datum.
    ge_zfit006-nsu_host           = ge_formpag-nsu_host. "APPC - DAM - Passagem do NSU - 05/05/2023

    IF ge_formpag-nsu_host IS NOT INITIAL.
      ge_zfit006-nsu_host = ge_formpag-nsu_host.
    ELSE.
      ge_zfit006-nsu_host = ge_zfit006-apr_pos.
    ENDIF.

*        Inicio - GFX - JECA - F-PACI171 - ajuste de NSU HOST no TEF - caixa - 25/01/2020
    SHIFT ge_zfit006-nsu_host LEFT DELETING LEADING '0'.
    SHIFT ge_zfit006-apr_pos LEFT DELETING LEADING '0'.
*        Fim - GFX - JECA - F-PACI171 - ajuste de NSU HOST no TEF - caixa - 25/01/2020

**   Início - GFX - NIIS - F-PACI144 - Ajuste POS Debito  - caixa(adiantamento) - 06/11/2020
*    IF ge_formpag-cod_adq         = 'REDE' AND
*       ge_zfit006-tp_pagto_cartao = 'POS'  AND
*       ge_zfit006-modalidade      = 'D'.
*
*      ge_zfit006-nsu_host = ge_zfit006-apr_pos.
*      ge_zfit006-apr_pos  = ''.
*    ENDIF.
**   Fim    - GFX - NIIS - F-PACI144 - Ajuste POS Debito  - caixa(adiantamento) - 06/11/2020

    APPEND ge_zfit006 TO gt_zfit006.
  ENDLOOP.

  IF t_vlr_troco GT 0.
    ADD 1 TO l_cont.

    ge_zfit006-bukrs              = t_bukrs.
    ge_zfit006-bupla              = t_bupla.
    ge_zfit006-caixa              = t_caixa.
    ge_zfit006-operacao           = g_operacao.
    ge_zfit006-data               = sy-datum.
    ge_zfit006-vbeln              = ''.
    ge_zfit006-dcr_meio_pagamento = 'TROCO'.
    ge_zfit006-item               = l_cont.
    ge_zfit006-bname              = sy-uname.
    ge_zfit006-belnr              = g_belnr.
    ge_zfit006-gjahr              = g_gjahr.
    ge_zfit006-vbeln_vf           = ''.
    ge_zfit006-nfenum             = ''.
    ge_zfit006-tp_doc_fiscal      = ''.
    ge_zfit006-zlsch              = ''.
    ge_zfit006-valor              = t_vlr_troco.
    ge_zfit006-num_apr_cheque     = ge_formpag-num_apr_cheque.
    ge_zfit006-tp_pagto_cartao    = ge_formpag-tp_pagto_cartao.
    ge_zfit006-cartao             = ge_formpag-cartao.
    ge_zfit006-parcelas           = ge_formpag-parcelas.
    ge_zfit006-apr_pos            = ge_formpag-apr_pos.
    ge_zfit006-valor_bruto        = ge_zfit006-valor.
    ge_zfit006-desconto           = 0.
    ge_zfit006-hora               = sy-uzeit.
    ge_zfit006-datum              = sy-datum.
    ge_zfit006-nsu_host           = ge_formpag-nsu_host.

*        Inicio - GFX - JECA - F-PACI171 - ajuste de NSU HOST no TEF - caixa - 20/11/2020
    SHIFT ge_cct006-nsu_host LEFT DELETING LEADING '0'.
    SHIFT ge_zfit006-apr_pos LEFT DELETING LEADING '0'.
*        Fim - GFX - JECA - F-PACI171 - ajuste de NSU HOST no TEF - caixa - 20/11/2020

*   Início - GFX - NIIS - F-PACI144 - Ajuste POS Debito  - caixa(adiantamento) - 06/11/2020
    IF ge_formpag-cod_adq         = 'REDE' AND
       ge_zfit006-tp_pagto_cartao = 'POS'  AND
       ge_zfit006-modalidade      = 'D'.

      ge_zfit006-nsu_host = ge_zfit006-apr_pos.
      ge_zfit006-apr_pos  = ''.
    ENDIF.
*   Fim    - GFX - NIIS - F-PACI144 - Ajuste POS Debito  - caixa(adiantamento) - 06/11/2020

    APPEND ge_zfit006 TO gt_zfit006.
  ENDIF.

  "Grava a contabilização na tabela do C2D apenas quando não utilizar a nova conciliação
  IF g_nova_conc IS INITIAL.

    LOOP AT gt_zfie011 INTO ge_zfie011.

      CLEAR ge_cct006.
      ge_cct006-empresa         = t_bukrs.
      ge_cct006-filial          = t_bupla.
      ge_cct006-cliente         = ge_zfie011-cliente.
      ge_cct006-num_documento   = ge_zfie011-belnr.
      ge_cct006-item            = ge_zfie011-buzei.
      ge_cct006-ano             = ge_zfie011-gjahr.
      ge_cct006-num_parcela     = ge_zfie011-num_parcela.
      ge_cct006-tot_parcelas    = ge_zfie011-parcelas.
      ge_cct006-chave_bandeira  = ge_zfie011-chave_bandeira.
      ge_cct006-cod_aut         = ge_zfie011-apr_pos.
      ge_cct006-data_venda      = ge_zfie011-data_venda.
      ge_cct006-valor_venda     = ge_zfie011-valor_parcela.
      ge_cct006-nsu_host        = ge_zfie011-nsu_host.

*        Inicio - GFX - JECA - F-PACI171 - ajuste de NSU HOST no TEF - caixa - 20/11/2020
      SHIFT  ge_cct006-nsu_host LEFT DELETING LEADING '0'.
      IF  strlen( ge_cct006-cod_aut ) < 6.
        CONCATENATE '0' ge_cct006-cod_aut INTO ge_cct006-cod_aut.
      ENDIF.
*        Fim - GFX - JECA - F-PACI171 - ajuste de NSU HOST no TEF - caixa - 20/11/2020

*   Início - GFX - NIIS - F-PACI144 - Ajuste POS Debito  - caixa(adiantamento) - 06/11/2020
      IF ge_zfie011-cod_adq         = 'REDE' AND
         ge_zfie011-tp_pagto_cartao = 'POS'  AND
         ge_zfie011-modalidade      = 'D'.

        ge_cct006-nsu_host = ge_cct006-cod_aut.
        ge_cct006-cod_aut  = ''.

      ENDIF.
*   Fim    - GFX - NIIS - F-PACI144 - Ajuste POS Debito  - caixa(adiantamento) - 06/11/2020

      APPEND ge_cct006 TO gt_cct006.

    ENDLOOP.

  ENDIF.

  IF gt_zfit006 IS NOT INITIAL.
    MODIFY zfit006 FROM TABLE gt_zfit006.
    COMMIT WORK.
  ENDIF.
  IF gt_cct006 IS NOT INITIAL.
    MODIFY /gfxdsi/cct006 FROM TABLE gt_cct006.
    COMMIT WORK.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXECUTAR_RECEBIMENTO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM executar_recebimento .

  CLEAR: gt_formpag, gt_formpag[],
         gt_zfie011, gt_zfie011[].


  SELECT COUNT(*)
    FROM zsdt003f
    WHERE werks EQ g_werks
      AND kunnr EQ ge_zfie004-kunnr.

  "Verifica Cliente Balcão
  IF NOT sy-subrc IS INITIAL.

    "Verificar Créditos
    CLEAR: gt_creditos, gt_creditos[].
    SELECT belnr buzei gjahr xblnr bldat dmbtr
      INTO CORRESPONDING FIELDS OF TABLE gt_creditos
      FROM bsid
      WHERE bukrs EQ t_bukrs
        AND kunnr EQ ge_zfie004-kunnr
        AND bschl IN ('11', '12', '14', '15', '16', '17', '19')
        AND zlspr EQ ''.

    SORT gt_creditos BY bldat dmbtr.

    IF NOT gt_creditos[] IS INITIAL.

      "Verificar os créditos de cliente
      "Se está no varejo, não precisa de nenhuma rotina, fico como esta.
      IF g_atacado EQ 'X'.

        SELECT *
          INTO TABLE gt_acdoca
          FROM acdoca
          FOR ALL ENTRIES IN gt_creditos
          WHERE rldnr  EQ '0L'
            AND rbukrs EQ t_bukrs
            AND belnr  EQ gt_creditos-belnr
            AND gjahr  EQ gt_creditos-gjahr
            AND buzei  EQ gt_creditos-buzei.

        SORT gt_acdoca BY belnr buzei.

        CLEAR: t_vlr_tot_credito, t_vlr_credito_utilizado.
        LOOP AT gt_creditos INTO ge_creditos.

          CLEAR ge_acdoca.
          READ TABLE gt_acdoca INTO ge_acdoca WITH KEY belnr = ge_creditos-belnr
                                                       gjahr = ge_creditos-gjahr
                                                       buzei = ge_creditos-buzei.

          CLEAR ge_zmmt005.
          READ TABLE gt_zmmt005 INTO ge_zmmt005 WITH KEY prctr = ge_acdoca-prctr.

          ge_creditos-bupla = ge_zmmt005-branch.

          CLEAR: g_nodetype.
          SELECT SINGLE nodetype INTO g_nodetype
            FROM t001w
            WHERE j_1bbranch EQ ge_zmmt005-branch.

          IF g_nodetype NE 'DC'.
            DELETE gt_creditos.
            CONTINUE.
          ENDIF.

        ENDLOOP.

      ENDIF.

      "Os créditos gerados nas lojas do segmento VAREJO só poderão ser utilizados nas lojas
      "deste seguimento.
      "Os créditos gerados nas lojas do segmento ATACADO poderão ser utilizados nas lojas
      "dos seguimentos ATACADO e VAREJO.
      "Verificar os créditos de cliente

      IF NOT gt_creditos[] IS INITIAL.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = TEXT-001 "Aviso de Crédito
            text_question         = TEXT-002 "Cliente possui crédito, deseja utilizar?
            text_button_1         = TEXT-003 "Sim
            icon_button_1         = 'ICON_OKAY'
            text_button_2         = TEXT-004 "Não
            icon_button_2         = 'ICON_CANCEL'
            default_button        = '1'
            display_cancel_button = ' '
          IMPORTING
            answer                = g_answer.

        IF g_answer EQ '1'.

          CLEAR: t_vlr_tot_credito, t_vlr_credito_utilizado.
          LOOP AT gt_creditos INTO ge_creditos.
            ADD ge_creditos-dmbtr TO t_vlr_tot_credito.
          ENDLOOP.

          t_vlr_credito_utilizado = ge_zfie004-netwr.

          CLEAR: gt_formpag, gt_formpag[].
          ge_formpag-dcr_meio_pagamento = 'CRÉDITO DO CLIENTE'.

          CALL SCREEN '9006' STARTING AT 02 02
                             ENDING AT 40 4.
        ELSE.
          CLEAR: gt_formpag.
          t_dcr_meio_pagamento = 'DINHEIRO'.
          t_vlr_recebendo      = ge_zfie004-netwr.
          t_vlr_tot_a_receber  = ge_zfie004-netwr.
          CLEAR: t_vlr_tot_recebido, t_vlr_tot_restante, t_vlr_troco, ok_code, sy-ucomm.

          CALL SCREEN '9003' STARTING AT 02 02
                             ENDING AT 80 20.
        ENDIF.
      ELSE.
        CLEAR: gt_formpag.
        t_dcr_meio_pagamento = 'DINHEIRO'.
        t_vlr_recebendo      = ge_zfie004-netwr.
        t_vlr_tot_a_receber  = ge_zfie004-netwr.
        CLEAR: t_vlr_tot_recebido, t_vlr_tot_restante, t_vlr_troco, ok_code, sy-ucomm.

        CALL SCREEN '9003' STARTING AT 02 02
                           ENDING AT 80 20.
      ENDIF.
    ELSE.
      CLEAR: gt_formpag.
      t_dcr_meio_pagamento = 'DINHEIRO'.
      t_vlr_recebendo      = ge_zfie004-netwr.
      t_vlr_tot_a_receber  = ge_zfie004-netwr.
      CLEAR: t_vlr_tot_recebido, t_vlr_tot_restante, t_vlr_troco, ok_code, sy-ucomm.

      CALL SCREEN '9003' STARTING AT 02 02
                         ENDING AT 80 20.
    ENDIF.
  ELSE.
    CLEAR: gt_formpag.
    t_dcr_meio_pagamento = 'DINHEIRO'.
    t_vlr_recebendo      = ge_zfie004-netwr.
    t_vlr_tot_a_receber  = ge_zfie004-netwr.
    CLEAR: t_vlr_tot_recebido, t_vlr_tot_restante, t_vlr_troco, ok_code, sy-ucomm.

    CALL SCREEN '9003' STARTING AT 02 02
                       ENDING AT 80 20.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM user_ok_tc2 USING    p_tc_name TYPE dynfnam
                         p_table_name
                         p_mark_name
                CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA: l_ok     TYPE sy-ucomm,
        l_offset TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  SEARCH p_ok FOR p_tc_name.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  l_offset = strlen( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
  CASE l_ok.
    WHEN 'INSR'.                      "insert row
      PERFORM fcode_insert_row2 USING    p_tc_name
                                         p_table_name.
      CLEAR p_ok.

    WHEN 'DELE'.                      "delete row
      PERFORM fcode_delete_row2 USING    p_tc_name
                                         p_table_name
                                         p_mark_name.
      CLEAR p_ok.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM compute_scrolling_in_tc2 USING p_tc_name
                                             l_ok.
      CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM fcode_tc_mark_lines2 USING p_tc_name
                                         p_table_name
                                         p_mark_name   .
      CLEAR p_ok.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM fcode_tc_demark_lines2 USING p_tc_name
                                           p_table_name
                                           p_mark_name .
      CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC2

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW2                                        *
*&---------------------------------------------------------------------*
FORM fcode_insert_row2
              USING    p_tc_name           TYPE dynfnam
                       p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_lines_name       LIKE feld-name.
  DATA l_selline          LIKE sy-stepl.
  DATA l_lastline         TYPE i.
  DATA l_line             TYPE i.
  DATA l_table_name       LIKE feld-name.
  FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
  FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
  ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE l_selline.
  IF sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
    IF l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    ELSE.
      <tc>-top_line = 1.
    ENDIF.
  ELSE.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <table> INDEX l_selline.
  <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE l_line.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW2                                        *
*&---------------------------------------------------------------------*
FORM fcode_delete_row2
              USING    p_tc_name           TYPE dynfnam
                       p_table_name
                       p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <table> LINES <tc>-lines.

  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    IF <mark_field> = 'X'.
      DELETE <table> INDEX syst-tabix.
      IF sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc2 USING    p_tc_name
                                       p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_tc_new_top_line     TYPE i.
  DATA l_tc_name             LIKE feld-name.
  DATA l_tc_lines_name       LIKE feld-name.
  DATA l_tc_field_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
  ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
  IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
    l_tc_new_top_line = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        entry_act      = <tc>-top_line
        entry_from     = 1
        entry_to       = <tc>-lines
        last_page_full = 'X'
        loops          = <lines>
        ok_code        = p_ok
        overlapping    = 'X'
      IMPORTING
        entry_new      = l_tc_new_top_line
      EXCEPTIONS
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO    = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        OTHERS         = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD l_tc_field_name
             AREA  l_tc_name.

  IF syst-subrc = 0.
    IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD l_tc_field_name LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES2
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_mark_lines2 USING p_tc_name
                                p_table_name
                                p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES2
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_demark_lines2 USING p_tc_name
                                  p_table_name
                                  p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = space.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines
*&---------------------------------------------------------------------*
*& Form SOMAR_CREDITOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM somar_creditos .

  CLEAR g_totcred.
  LOOP AT gt_creditos INTO ge_creditos WHERE marc EQ 'X'.

    ADD ge_creditos-dmbtr TO g_totcred.

  ENDLOOP.

  CLEAR: gt_formpag, gt_formpag[].
  IF g_totcred GT 0.
    ge_formpag-dcr_meio_pagamento = 'CRÉDITO DO CLIENTE'.
    ge_formpag-dmbtr              = g_totcred.
    APPEND ge_formpag TO gt_formpag.
  ENDIF.

  CALL SCREEN '9003' STARTING AT 02 02
                     ENDING AT 80 20.

ENDFORM.

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM user_ok_tc3 USING    p_tc_name TYPE dynfnam
                          p_table_name
                          p_mark_name
                 CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA: l_ok     TYPE sy-ucomm,
        l_offset TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  SEARCH p_ok FOR p_tc_name.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  l_offset = strlen( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
  CASE l_ok.
    WHEN 'INSR'.                      "insert row
      PERFORM fcode_insert_row3 USING    p_tc_name
                                         p_table_name.
      CLEAR p_ok.

    WHEN 'DELE'.                      "delete row
      PERFORM fcode_delete_row3 USING    p_tc_name
                                         p_table_name
                                         p_mark_name.
      CLEAR p_ok.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM compute_scrolling_in_tc3 USING p_tc_name
                                             l_ok.
      CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM fcode_tc_mark_lines3 USING p_tc_name
                                         p_table_name
                                         p_mark_name   .
      CLEAR p_ok.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM fcode_tc_demark_lines3 USING p_tc_name
                                           p_table_name
                                           p_mark_name .
      CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_insert_row3
              USING    p_tc_name           TYPE dynfnam
                       p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_lines_name       LIKE feld-name.
  DATA l_selline          LIKE sy-stepl.
  DATA l_lastline         TYPE i.
  DATA l_line             TYPE i.
  DATA l_table_name       LIKE feld-name.
  FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
  FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
  ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE l_selline.
  IF sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
    IF l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    ELSE.
      <tc>-top_line = 1.
    ENDIF.
  ELSE.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <table> INDEX l_selline.
  <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE l_line.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_delete_row3
              USING    p_tc_name           TYPE dynfnam
                       p_table_name
                       p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <table> LINES <tc>-lines.

  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    IF <mark_field> = 'X'.
      DELETE <table> INDEX syst-tabix.
      IF sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc3 USING    p_tc_name
                                       p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_tc_new_top_line     TYPE i.
  DATA l_tc_name             LIKE feld-name.
  DATA l_tc_lines_name       LIKE feld-name.
  DATA l_tc_field_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
  ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
  IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
    l_tc_new_top_line = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        entry_act      = <tc>-top_line
        entry_from     = 1
        entry_to       = <tc>-lines
        last_page_full = 'X'
        loops          = <lines>
        ok_code        = p_ok
        overlapping    = 'X'
      IMPORTING
        entry_new      = l_tc_new_top_line
      EXCEPTIONS
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO    = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        OTHERS         = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD l_tc_field_name
             AREA  l_tc_name.

  IF syst-subrc = 0.
    IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD l_tc_field_name LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_mark_lines3 USING p_tc_name
                                p_table_name
                                p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_demark_lines3 USING p_tc_name
                                  p_table_name
                                  p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = space.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines
*&---------------------------------------------------------------------*
*& Form ACUMULA_VALORES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM acumula_valores .

  CLEAR g_adiantamento.

  IF NOT t_dcr_meio_pagamento IS INITIAL.
    READ TABLE gt_f4_meio_pagto INTO ge_f4_meio_pagto WITH KEY t_dcr_meio_pagamento.

    IF sy-subrc NE 0.
      CLEAR: ok_code, t_dcr_meio_pagamento.
      "Forma de Pagamento Inválida!
      MESSAGE e013(zfi001).
    ENDIF.

    IF t_dcr_meio_pagamento NE 'DINHEIRO'.
      IF t_vlr_recebendo GT t_vlr_tot_a_receber.
        CLEAR: ok_code, t_dcr_meio_pagamento.
        "Valor Recebendo & não pode ser maior que Total a Receber!
        MESSAGE e022(zfi001) WITH t_vlr_recebendo.
      ENDIF.
      CLEAR g_totcred.
      LOOP AT gt_formpag INTO ge_formpag WHERE dcr_meio_pagamento NE 'DINHEIRO'.
        ADD ge_formpag-dmbtr TO g_totcred.
      ENDLOOP.
      g_difer = t_vlr_tot_a_receber - g_totcred.
      IF t_vlr_recebendo GT g_difer.
        CLEAR: ok_code, t_dcr_meio_pagamento.
        "Valor Recebendo & não pode ser maior que Total a Receber!
        MESSAGE e022(zfi001) WITH t_vlr_recebendo.
      ENDIF.
    ENDIF.
  ENDIF.

  CLEAR ge_formpag.

  IF t_vlr_recebendo GT 0.
    IF t_dcr_meio_pagamento EQ 'CHEQUE'.

      CLEAR ge_zsdt003f.
      READ TABLE gt_zsdt003f INTO ge_zsdt003f WITH KEY werks = g_werks
                                                       kunnr = ge_zfie004-kunnr.

      IF sy-subrc EQ 0.
        CLEAR: ok_code, t_dcr_meio_pagamento.
        "Proibido venda em cheque p/cliente balcão,realizar cadastro e nova venda
        MESSAGE e052(zfi001) WITH t_vlr_recebendo.
      ENDIF.

      CALL SCREEN '9004' STARTING AT 02 02
                         ENDING AT 63 05.
      IF ok_code IS INITIAL.
        CLEAR t_vlr_recebendo.
      ENDIF.
      CLEAR ok_code.
    ELSEIF t_dcr_meio_pagamento  EQ 'CARTÃO DE CRÉDITO'.
      ADD 1 TO g_ident_cartao.
      ge_formpag-parcelas   = '1'.
      ge_formpag-cod_adq = 'CIELO'.  "GFX - FI - SFSS - DEVK915041 - 01/11/2019
      CALL SCREEN '9005' STARTING AT 02 02
                         ENDING AT 37 08.
      IF ok_code IS INITIAL.
        CLEAR t_vlr_recebendo.
      ENDIF.
      CLEAR ok_code.
    ELSEIF t_dcr_meio_pagamento  EQ 'CARTÃO DE DÉBITO'.
      ADD 1 TO g_ident_cartao.
      ge_formpag-cod_adq = 'CIELO'.  "GFX - FI - SFSS - DEVK915041 - 01/11/2019
      CALL SCREEN '9009' STARTING AT 02 02
                         ENDING AT 37 08.
      IF ok_code IS INITIAL.
        CLEAR t_vlr_recebendo.
      ENDIF.
      CLEAR ok_code.
    ENDIF.
  ENDIF.

  IF NOT t_dcr_meio_pagamento IS INITIAL.
    IF t_dcr_meio_pagamento  EQ 'CARTÃO DE CRÉDITO' OR
       t_dcr_meio_pagamento  EQ 'CARTÃO DE DÉBITO'.
      ge_formpag-ident_cartao       = g_ident_cartao.
    ENDIF.
    ge_formpag-dcr_meio_pagamento = t_dcr_meio_pagamento.
    ge_formpag-dmbtr              = t_vlr_recebendo.
    IF t_vlr_recebendo GT 0.
      IF t_dcr_meio_pagamento EQ 'DINHEIRO'.
        COLLECT ge_formpag INTO gt_formpag.
      ELSE.
        APPEND ge_formpag TO gt_formpag.
      ENDIF.
    ENDIF.
  ENDIF.

  CLEAR: t_vlr_tot_a_receber, t_vlr_tot_recebido, t_vlr_tot_restante, t_vlr_troco.
  LOOP AT gt_formpag INTO ge_formpag.
    ADD ge_formpag-dmbtr TO t_vlr_tot_recebido.
  ENDLOOP.

  t_vlr_tot_a_receber = ge_zfie004-netwr.
  t_vlr_tot_restante  = t_vlr_tot_a_receber - t_vlr_tot_recebido.
  IF t_vlr_tot_recebido GT t_vlr_tot_a_receber.
    t_vlr_troco        = t_vlr_tot_recebido - t_vlr_tot_a_receber.
    t_vlr_tot_restante = 0.
  ENDIF.

  DELETE gt_formpag WHERE dmbtr EQ 0.

  CLEAR: t_dcr_meio_pagamento, t_vlr_recebendo.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form LANCAR_RECEBIMENTO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM lancar_recebimento .

  IF t_vlr_tot_restante NE 0.
    CLEAR ok_code.
    "Total Recebido & não é suficiente para pagamento!
    MESSAGE e010(zfi001) WITH t_vlr_tot_recebido.
  ENDIF.

  IF t_vlr_tot_recebido EQ 0.
    CLEAR ok_code.
    "Total Recebido & não é suficiente para pagamento!
    MESSAGE e010(zfi001) WITH t_vlr_tot_recebido.
  ENDIF.

  CLEAR: g_value, g_valuecnf, g_valueqrcode, g_valueid,
         g_docnum, g_nfenum, lv_xml.

  CLEAR g_erro.

*** GFX - SFSS - DEVK915251 - 18.11.2019 - Início

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 25
      text       = 'Emitindo fatura...'.

*** GFX - SFSS - DEVK915251 - 18.11.2019 - Fim

  PERFORM faturar_remessa.

  CHECK g_erro IS INITIAL.

*** GFX - SFSS - DEVK915251 - 18.11.2019 - Início

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Lançando documento contábil...'.

*** GFX - SFSS - DEVK915251 - 18.11.2019 - Fim

  PERFORM lanc_contabil_fb05.

  CHECK g_erro IS INITIAL.

  PERFORM gravar_tabelas_receb.

  CHECK g_erro IS INITIAL.

  "Nota Fiscal Type Redetermination per Region for SD
  CLEAR ge_j_1b_nftypereg.
  READ TABLE gt_j_1b_nftypereg INTO ge_j_1b_nftypereg WITH KEY country = ge_zfie004-land1
                                                               regio   = ge_zfie004-regio.

  IF sy-subrc EQ 0.
    PERFORM busca_dados_nf.

*** GFX - SFSS - DEVK915251 - 18.11.2019 - Início

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 70
        text       = 'Emitindo Documento Fiscal...'.

    SUBMIT j_bnfecallrfc
            WITH so_docnm-low = g_docnum
            AND RETURN.

    "Aguarda até 40 segundos
    DO 40 TIMES.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 70
          text       = 'Emitindo Documento Fiscal...'.

      "Busca status da nota
      SELECT SINGLE docnum
                  , docsta
                  , code
                  , action_requ
        FROM j_1bnfe_active
        INTO @DATA(ls_active)
        WHERE docnum EQ @g_docnum
          AND ( action_requ NE '' AND action_requ NE 'A' AND action_requ NE '3' ).

      IF sy-subrc IS INITIAL.
        EXIT.
      ELSE.
        WAIT UP TO 1 SECONDS.
      ENDIF.
    ENDDO.

    IF ls_active IS INITIAL OR ls_active-docnum IS INITIAL.

********************************************************************** GFX - JBRS - INICIO - 14/01/2020 - Ajuste mensagem
*    MESSAGE 'Documento fiscal sem retorno da SEFAZ, por favor verifique o monitor de notas.' TYPE 'I' DISPLAY LIKE 'E'.
      MESSAGE 'Tempo limite de 40 segundos ultrapassado, por gentileza acompanhar a emissão através da transação J1BNFE' TYPE 'I' DISPLAY LIKE 'W'.
********************************************************************** GFX - JBRS - FIM - 14/01/2020 - Ajuste mensagem
      g_erro = 'X'.
      EXIT.

    ELSE.

      IF ls_active-action_requ = '8'.
        MESSAGE 'Documento fiscal com erro, envio não efetuado.' TYPE 'I' DISPLAY LIKE 'E'.
        g_erro = 'X'.
        EXIT.
      ELSE.

        "Busca o texto do status
        SELECT SINGLE code
                    , text
          FROM j_1bstscodet
          INTO @DATA(ls_code)
          WHERE code  EQ @ls_active-code
            AND spras EQ @sy-langu.

        IF ls_code-text IS NOT INITIAL.
          IF ls_active-action_requ EQ 'C'.
            MESSAGE ls_code-text TYPE 'I' DISPLAY LIKE 'S'.
          ELSE.
            MESSAGE ls_code-text TYPE 'I' DISPLAY LIKE 'E'.
            g_erro = 'X'.
            EXIT.
          ENDIF.
        ELSE.
          IF ls_active-action_requ EQ 'C'.
            MESSAGE 'Documento fiscal emitido com sucesso.' TYPE 'I' DISPLAY LIKE 'S'.
          ELSE.
            MESSAGE 'Documento fiscal não emitido. Por favor verifique o monitor de notas.' TYPE 'I' DISPLAY LIKE 'E'.
            g_erro = 'X'.
            EXIT.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDIF.

    FREE: ls_active, ls_code.

*** GFX - SFSS - DEVK915251 - 18.11.2019 - Fim

    "Só verifica se a nota for do modelo 65 (NFCe) - SFSS - 22/08/2019
    IF g_acckey-model EQ '65'.
      PERFORM verifica_nfce.
    ENDIF.
  ELSE.
    IF ge_zfie004-tp_doc_fiscal EQ 'CF'.
      PERFORM integracao_mfe.
      PERFORM busca_dados_nf.
    ELSE.
      PERFORM busca_dados_nf.

*** GFX - SFSS - DEVK915251 - 18.11.2019 - Início

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 70
          text       = 'Emitindo Documento Fiscal...'.

      SUBMIT j_bnfecallrfc
              WITH so_docnm-low = g_docnum
              AND RETURN.

      "Aguarda até 40 segundos
      DO 40 TIMES.

        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            percentage = 70
            text       = 'Emitindo Documento Fiscal...'.

        "Busca status da nota
        SELECT SINGLE docnum
                      docsta
                      code
                      action_requ
          FROM j_1bnfe_active
          INTO CORRESPONDING FIELDS OF ls_active
          WHERE docnum EQ g_docnum
            AND ( action_requ NE '' AND action_requ NE 'A' AND action_requ NE '3' ).

        IF sy-subrc IS INITIAL.
          EXIT.
        ELSE.
          WAIT UP TO 1 SECONDS.
        ENDIF.
      ENDDO.

      IF ls_active IS INITIAL OR ls_active-docnum IS INITIAL.

********************************************************************** GFX - JBRS - INICIO - 14/01/2020 - Ajuste mensagem
*    MESSAGE 'Documento fiscal sem retorno da SEFAZ, por favor verifique o monitor de notas.' TYPE 'I' DISPLAY LIKE 'E'.
        MESSAGE 'Tempo limite de 40 segundos ultrapassado, por gentileza acompanhar a emissão através da transação J1BNFE' TYPE 'I' DISPLAY LIKE 'W'.
********************************************************************** GFX - JBRS - FIM - 14/01/2020 - Ajuste mensagem
        g_erro = 'X'.
        EXIT.

      ELSE.

        IF ls_active-action_requ = '8'.
          MESSAGE 'Documento fiscal com erro, envio não efetuado.' TYPE 'I' DISPLAY LIKE 'E'.
          g_erro = 'X'.
          EXIT.
        ELSE.

          "Busca o texto do status
          SELECT SINGLE code
                        text
            FROM j_1bstscodet
            INTO CORRESPONDING FIELDS OF ls_code
            WHERE code  EQ ls_active-code
              AND spras EQ sy-langu.

          IF ls_code-text IS NOT INITIAL.
            IF ls_active-action_requ EQ 'C'.
              MESSAGE ls_code-text TYPE 'I' DISPLAY LIKE 'S'.
            ELSE.
              MESSAGE ls_code-text TYPE 'I' DISPLAY LIKE 'E'.
              g_erro = 'X'.
              EXIT.
            ENDIF.
          ELSE.
            IF ls_active-action_requ EQ 'C'.
              MESSAGE 'Documento fiscal emitido com sucesso.' TYPE 'I' DISPLAY LIKE 'S'.
            ELSE.
              MESSAGE 'Documento fiscal não emitido. Por favor verifique o monitor de notas.' TYPE 'I' DISPLAY LIKE 'E'.
              g_erro = 'X'.
              EXIT.
            ENDIF.
          ENDIF.

        ENDIF.

      ENDIF.

      FREE: ls_active, ls_code.

*** GFX - SFSS - DEVK915251 - 18.11.2019 - Fim

      "Só verifica se a nota for do modelo 65 (NFCe) - SFSS - 22/08/2019
      IF g_acckey-model EQ '65'.
        PERFORM verifica_nfce.
      ENDIF.
    ENDIF.
  ENDIF.

  CHECK g_erro IS INITIAL.

*** GFX - SFSS - DEVK915251 - 18.11.2019 - Início

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 95
      text       = 'Finalizando recebimento...'.

*** GFX - SFSS - DEVK915251 - 18.11.2019 - Fim

  LOOP AT gt_formpag INTO ge_formpag.
    IF ge_formpag-dcr_meio_pagamento EQ 'CRÉDITO DO CLIENTE'.
      PERFORM imprimir_recibo_cred_cli1.

      PERFORM imprimir_recibo_cred_cli2.
    ENDIF.
  ENDLOOP.

  PERFORM confirmar_tef.
  PERFORM imprimir_recibo_tef_novo.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form LANCAR_RECEBIMENTO_DIRETO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM lancar_recebimento_direto.

  CLEAR: gt_formpag, gt_formpag[], t_dcr_meio_pagamento, t_vlr_recebendo,
         t_vlr_tot_a_receber, t_vlr_tot_recebido, t_vlr_tot_restante,
         t_vlr_troco, g_erro.

*** GFX - SFSS - DEVK915251 - 18.11.2019 - Início

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 30
      text       = 'Lançando fatura...'.

*** GFX - SFSS - DEVK915251 - 18.11.2019 - Fim

  PERFORM faturar_remessa.

  CHECK g_erro IS INITIAL.


* retirar esta rotina e colocar dentro da impressao de danfe
  "Chamando o programa de impressão de boleto
*  SUBMIT zfir001 WITH s_bukrs EQ t_bukrs
*                 WITH s_belnr EQ g_fatura
*                 AND RETURN.

  CHECK g_erro IS INITIAL.

  PERFORM gravar_tabelas_receb.

  CHECK g_erro IS INITIAL.

  PERFORM busca_dados_nf.

*** GFX - SFSS - DEVK915251 - 18.11.2019 - Início

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 60
      text       = 'Emitindo Documento Fiscal...'.

  SUBMIT j_bnfecallrfc
          WITH so_docnm-low = g_docnum
          AND RETURN.

  "Aguarda até 40 segundos
  DO 40 TIMES.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 60
        text       = 'Emitindo Documento Fiscal...'.

    "Busca status da nota
    SELECT SINGLE docnum
                , docsta
                , code
                , action_requ
      FROM j_1bnfe_active
      INTO @DATA(ls_active)
      WHERE docnum EQ @g_docnum
        AND ( action_requ NE '' AND action_requ NE 'A' AND action_requ NE '3' ).

    IF sy-subrc IS INITIAL.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
    ENDIF.
  ENDDO.

  IF ls_active IS INITIAL OR ls_active-docnum IS INITIAL.

********************************************************************** GFX - JBRS - INICIO - 14/01/2020 - Ajuste mensagem
*    MESSAGE 'Documento fiscal sem retorno da SEFAZ, por favor verifique o monitor de notas.' TYPE 'I' DISPLAY LIKE 'E'.
    MESSAGE 'Tempo limite de 40 segundos ultrapassado, por gentileza acompanhar a emissão através da transação J1BNFE' TYPE 'I' DISPLAY LIKE 'W'.
********************************************************************** GFX - JBRS - FIM - 14/01/2020 - Ajuste mensagem
    g_erro = 'X'.
    EXIT.
  ELSE.

    IF ls_active-action_requ = '8'.
      MESSAGE 'Documento fiscal com erro, envio não efetuado.' TYPE 'I' DISPLAY LIKE 'E'.
      g_erro = 'X'.
      EXIT.
    ELSE.

      "Busca o texto do status
      SELECT SINGLE code
                  , text
        FROM j_1bstscodet
        INTO @DATA(ls_code)
        WHERE code  EQ @ls_active-code
          AND spras EQ @sy-langu.

      IF ls_code-text IS NOT INITIAL.
        IF ls_active-action_requ EQ 'C'.
          MESSAGE ls_code-text TYPE 'I' DISPLAY LIKE 'S'.
        ELSE.
          MESSAGE ls_code-text TYPE 'I' DISPLAY LIKE 'E'.
          g_erro = 'X'.
          EXIT.
        ENDIF.
      ELSE.
        IF ls_active-action_requ EQ 'C'.
          MESSAGE 'Documento fiscal emitido com sucesso.' TYPE 'I' DISPLAY LIKE 'S'.
        ELSE.
          MESSAGE 'Documento fiscal não emitido. Por favor verifique o monitor de notas.' TYPE 'I' DISPLAY LIKE 'E'.
          g_erro = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDIF.

  FREE: ls_active, ls_code.

*** GFX - SFSS - DEVK915251 - 18.11.2019 - Fim

ENDFORM.
*&---------------------------------------------------------------------*
*& Form LANCAR_RECEBIMENTO_DUPLICATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM lancar_recebimento_duplicata.

  CLEAR: gt_formpag, gt_formpag[], t_dcr_meio_pagamento, t_vlr_recebendo,
         t_vlr_tot_a_receber, t_vlr_tot_recebido, t_vlr_tot_restante,
         t_vlr_troco, g_erro.

*** GFX - SFSS - DEVK915251 - 18.11.2019 - Início

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 30
      text       = 'Lançando fatura...'.

*** GFX - SFSS - DEVK915251 - 18.11.2019 - Fim

  PERFORM faturar_remessa.

  CHECK g_erro IS INITIAL.

  PERFORM gravar_tabelas_receb.

  IF ge_zfie004-tp_doc_fiscal EQ 'CF' AND
     ge_zfie004-zlsch         EQ 'F'.
    PERFORM integracao_mfe_fi.
    PERFORM busca_dados_nf.
  ELSE.
    PERFORM busca_dados_nf.

*** GFX - SFSS - DEVK915251 - 18.11.2019 - Início

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 60
        text       = 'Emitindo Documento Fiscal...'.

    SUBMIT j_bnfecallrfc
            WITH so_docnm-low = g_docnum
            AND RETURN.

    "Aguarda até 40 segundos
    DO 40 TIMES.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 60
          text       = 'Emitindo Documento Fiscal...'.

      "Busca status da nota
      SELECT SINGLE docnum
                  , docsta
                  , code
                  , action_requ
        FROM j_1bnfe_active
        INTO @DATA(ls_active)
        WHERE docnum EQ @g_docnum
          AND ( action_requ NE '' AND action_requ NE 'A' AND action_requ NE '3' ).

      IF sy-subrc IS INITIAL.
        EXIT.
      ELSE.
        WAIT UP TO 1 SECONDS.
      ENDIF.
    ENDDO.

    IF ls_active IS INITIAL OR ls_active-docnum IS INITIAL.

********************************************************************** GFX - JBRS - INICIO - 14/01/2020 - Ajuste mensagem
*    MESSAGE 'Documento fiscal sem retorno da SEFAZ, por favor verifique o monitor de notas.' TYPE 'I' DISPLAY LIKE 'E'.
      MESSAGE 'Tempo limite de 40 segundos ultrapassado, por gentileza acompanhar a emissão através da transação J1BNFE' TYPE 'I' DISPLAY LIKE 'W'.
********************************************************************** GFX - JBRS - FIM - 14/01/2020 - Ajuste mensagem
      g_erro = 'X'.
      EXIT.

    ELSE.

      IF ls_active-action_requ = '8'.
        MESSAGE 'Documento fiscal com erro, envio não efetuado.' TYPE 'I' DISPLAY LIKE 'E'.
        g_erro = 'X'.
        EXIT.
      ELSE.

        "Busca o texto do status
        SELECT SINGLE code
                    , text
          FROM j_1bstscodet
          INTO @DATA(ls_code)
          WHERE code  EQ @ls_active-code
            AND spras EQ @sy-langu.

        IF ls_code-text IS NOT INITIAL.
          IF ls_active-action_requ EQ 'C'.
            MESSAGE ls_code-text TYPE 'I' DISPLAY LIKE 'S'.
          ELSE.
            MESSAGE ls_code-text TYPE 'I' DISPLAY LIKE 'E'.
            g_erro = 'X'.
            EXIT.
          ENDIF.
        ELSE.
          IF ls_active-action_requ EQ 'C'.
            MESSAGE 'Documento fiscal emitido com sucesso.' TYPE 'I' DISPLAY LIKE 'S'.
          ELSE.
            MESSAGE 'Documento fiscal não emitido. Por favor verifique o monitor de notas.' TYPE 'I' DISPLAY LIKE 'E'.
            g_erro = 'X'.
            EXIT.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDIF.

    FREE: ls_active, ls_code.

*** GFX - SFSS - DEVK915251 - 18.11.2019 - Fim

  ENDIF.

*  "Chamando o programa de impressão de comprovante
  SUBMIT zfir014 WITH p_bukrs  EQ t_bukrs
                 WITH p_bupla  EQ t_bupla
                 WITH p_caixa  EQ t_caixa
                 WITH p_vbeln  EQ g_fatura
                 AND RETURN.

*  PERFORM imprimir_recibo_receb.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form LANC_CONTABIL_FB05
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM lanc_contabil_fb05 .

  DATA: l_num_documento TYPE bseg-belnr,
        l_ano           TYPE bseg-gjahr,
        l_item          TYPE bseg-buzei,
        l_dif           TYPE bsid-dmbtr,
        l_liq           TYPE bsid-dmbtr,
        l_taxa          TYPE bsid-dmbtr,
        l_gname         TYPE seqg3-gname,
        l_garg          TYPE seqg3-garg,
        l_code          TYPE j_1bnfdoc-code,
        l_docstat       TYPE j_1bnfdoc-docstat,
        l_nftype        TYPE j_1bnfdoc-nftype,
        l_ok            TYPE c,
        l_cliente(10)   TYPE c,
        lv_taxa_vl      TYPE char11,
        lv_valor_d2     TYPE p DECIMALS 2.

  DATA: lt_enq TYPE TABLE OF seqg3,
        ls_enq TYPE seqg3.

  CLEAR: gt_blntab,
         gt_ftclear,
         gt_ftpost,
         gt_fttax,
         ge_blntab,
         ge_ftclear,
         ge_ftpost,
         ge_fttax,
         g_doc_contabil.

  "Verificar se o cliente está sendo utilizado em outro lugar
  l_gname = 'KNB1'.
  CONCATENATE sy-mandt ge_zfie004-kunnr t_bukrs INTO l_garg.

  FREE g_nova_conc.

  "Busca conta do razão para nova conciliação
  SELECT SINGLE operacao
              , bupla
              , hkont
    FROM zfit003
    WHERE operacao EQ 'CONC'
      AND bupla    EQ @t_bupla
    INTO @DATA(ls_conta_razao).

* APPC - ESF - Tratamento PIX - Início 16.02.2022

  "Busca as contas do razão do PIX
  SELECT SINGLE hkont,    " Conta Razão
                hkont_fds " Cobta Razão Fim de Semana
    FROM zfit003
    WHERE operacao EQ 'PIX'
      AND bupla    EQ @t_bupla
      AND hkont     <> ''
      AND hkont_fds <> ''
    INTO @DATA(ls_pix).

  "Busca centro de custo, conta e taxa PIX
  SELECT SINGLE kostl,
                hkont_taxa,
                taxa_pix
    FROM zfit003
    WHERE operacao   EQ 'PIX'
      AND bupla      EQ @t_bupla
      AND kostl      <> ''
      AND hkont_taxa <> ''
*      AND taxa_pix   <> ''
    INTO @DATA(ls_taxa_pix).

  "Verifica se é dia útil
  DATA v_dsemana TYPE t246-langt.
  DATA v_data    TYPE sy-datum.
  DATA v_idioma  TYPE sy-langu.

  v_data = sy-datum.
  v_idioma = 'PT'.

  CALL FUNCTION 'ISP_GET_WEEKDAY_NAME'
    EXPORTING
      date        = v_data
      language    = v_idioma
    IMPORTING
      longtext    = v_dsemana
    EXCEPTIONS
      calendar_id = 1
      date_error  = 2
      not_found   = 3
      wrong_input = 4
      OTHERS      = 5.

  IF v_dsemana NE 'Sábado' AND v_dsemana NE 'Domingo'.

    "Verifica se é feriado
    SELECT SINGLE zzdia_semana
      FROM a901
      INNER JOIN konp
      ON ( a901~knumh = konp~knumh )
        WHERE a901~werks    = 'F000'    AND
              a901~datbi    = @v_data   AND
              konp~loevm_ko = ''
      INTO @v_dsemana.

  ENDIF.

* APPC - ESF - Tratamento PIX - Fim 16.02.2022

  g_mode = 'N'.
  CALL FUNCTION 'POSTING_INTERFACE_START'
    EXPORTING
      i_function         = 'C'
      i_mode             = g_mode "Especialista
      i_user             = sy-uname
    EXCEPTIONS
      client_incorrect   = 1
      function_invalid   = 2
      group_name_missing = 3
      mode_invalid       = 4
      update_invalid     = 5
      OTHERS             = 6.

  IF sy-subrc NE 0.
    """ Tratar o erro """
  ENDIF.

  SELECT * INTO TABLE gt_zfit003
    FROM zfit003
    WHERE operacao EQ 'ABER'
      AND bupla    EQ t_bupla.

  g_texto            = 'Recebimento Caixa'.
  "Montando Cabeçalho
  ge_ftpost-stype = 'K'."Tipo Cabeçalho (K)
  ge_ftpost-count = 1.  "Número da Tela

  "Empresa
  ge_ftpost-fnam = 'BKPF-BUKRS'.
  ge_ftpost-fval = t_bukrs.
  APPEND ge_ftpost TO gt_ftpost.

  "Data do documento
  ge_ftpost-fnam = 'BKPF-BLDAT'.
  CONCATENATE sy-datum+6(2) "Dia
              sy-datum+4(2) "Mês
              sy-datum(4)   "Ano
         INTO ge_ftpost-fval.
  APPEND ge_ftpost TO gt_ftpost.

  "Data do lançamento
  ge_ftpost-fnam = 'BKPF-BUDAT'.
  CONCATENATE sy-datum+6(2) "Dia
              sy-datum+4(2) "Mês
              sy-datum(4)   "Ano
         INTO ge_ftpost-fval.
  APPEND ge_ftpost TO gt_ftpost.

  "Tipo do Documento
  ge_ftpost-fnam = 'BKPF-BLART'.
  ge_ftpost-fval = 'DZ'. "Same type as documents cleared via F-32
  APPEND ge_ftpost TO gt_ftpost.

  "Moeda
  ge_ftpost-fnam = 'BKPF-WAERS'.
  ge_ftpost-fval = 'BRL'.
  APPEND ge_ftpost TO gt_ftpost.

  "Texto do Documento
  ge_ftpost-fnam = 'BKPF-BKTXT'.
  ge_ftpost-fval = g_texto.
  APPEND ge_ftpost TO gt_ftpost.

  "Incluir o CRÉDITO DO CLIENTE na última linha do documento contábil
  CLEAR ge_formpag.
  READ TABLE gt_formpag INTO ge_formpag WITH KEY dcr_meio_pagamento = 'CRÉDITO DO CLIENTE'.
  IF sy-subrc EQ 0.
    DELETE gt_formpag WHERE dcr_meio_pagamento = 'CRÉDITO DO CLIENTE'.
    APPEND ge_formpag TO gt_formpag.
  ENDIF.

  CLEAR g_totcred.
  CLEAR g_cont.
  LOOP AT gt_formpag INTO ge_formpag.

    "Definindo Contador.
    ADD 1 TO g_cont.

    "Entrando Item 01
    ge_ftpost-stype = 'P'.
    ge_ftpost-count = g_cont.

    IF ge_formpag-dcr_meio_pagamento EQ 'CHEQUE'.
      "Campo Chave de Lançamento
      ge_ftpost-fnam = 'RF05A-NEWBS'.
      ge_ftpost-fval = '09'.
      APPEND ge_ftpost TO gt_ftpost.

      "Campo Conta
      ge_ftpost-fnam = 'BSEG-HKONT'.
      ge_ftpost-fval = ge_zfie004-kunnr.
      APPEND ge_ftpost TO gt_ftpost.

      "Centro de Lucro
      ge_ftpost-fnam = 'COBL-PRCTR'.
      ge_ftpost-fval = ge_zfit002b-prctr.
      APPEND ge_ftpost TO gt_ftpost.

      "Campo Conta
      ge_ftpost-fnam = 'RF05A-NEWUM'.
      ge_ftpost-fval = 'X'.
      APPEND ge_ftpost TO gt_ftpost.

      "Campo Data Efetiva
      ge_ftpost-fnam = 'BSEG-ZFBDT'.
      CONCATENATE ge_formpag-data_bom_para+6(2) "Dia
                  ge_formpag-data_bom_para+4(2) "Mês
                  ge_formpag-data_bom_para(4)   "Ano
             INTO ge_ftpost-fval.
      APPEND ge_ftpost TO gt_ftpost.

      "Campo Montante
      ge_ftpost-fnam = 'BSEG-WRBTR'.
      ge_ftpost-fval = ge_formpag-dmbtr.
      "Retira os espaços
      SHIFT ge_ftpost-fval LEFT DELETING LEADING space.
      "Troca o . por ,
      TRANSLATE ge_ftpost-fval USING '.,'.
      APPEND ge_ftpost TO gt_ftpost.

      "Campo Atribuição
      ge_ftpost-fnam = 'BSEG-ZUONR'.
      ge_ftpost-fval = 'CHEQUE'.
      APPEND ge_ftpost TO gt_ftpost.
    ELSEIF ge_formpag-dcr_meio_pagamento EQ 'CARTÃO DE CRÉDITO'
        OR ge_formpag-dcr_meio_pagamento EQ 'CARTÃO DE DÉBITO'.
      SUBTRACT 1 FROM g_cont.

      LOOP AT gt_zfie011 INTO ge_zfie011 WHERE ident_cartao EQ ge_formpag-ident_cartao.
        ADD 1 TO g_cont.
        "Entrando Item 01
        ge_ftpost-stype = 'P'.
        ge_ftpost-count = g_cont.

        ge_zfie011-buzei = g_cont.
        MODIFY gt_zfie011 FROM ge_zfie011.

*        l_taxa = ( ge_zfie011-valor_parcela * ge_zfie011-tx_comissao ) / 100.
*        l_liq  = ge_zfie011-valor_parcela - l_taxa.

        "Campo Chave de Lançamento
        ge_ftpost-fnam = 'RF05A-NEWBS'.
*** GFX - JBRS - Início - 19/07/2021
*        ge_ftpost-fval = '01'.
        ge_ftpost-fval = COND #( WHEN ls_conta_razao-hkont IS NOT INITIAL
                                  THEN '40'
                                  ELSE '01' ).
*** GFX - JBRS - Fim - 19/07/2021
        APPEND ge_ftpost TO gt_ftpost.

        "Campo Conta
        ge_ftpost-fnam = 'BSEG-HKONT'.
        IF ls_conta_razao IS NOT INITIAL AND ls_conta_razao-hkont IS NOT INITIAL.
          ge_ftpost-fval = ls_conta_razao-hkont.
          g_nova_conc    = 'X'.
        ELSE.
          ge_ftpost-fval = ge_zfie011-cliente.
        ENDIF.
        APPEND ge_ftpost TO gt_ftpost.

        "Centro de Lucro
        ge_ftpost-fnam = 'COBL-PRCTR'.
        ge_ftpost-fval = ge_zfit002b-prctr.
        APPEND ge_ftpost TO gt_ftpost.

*** GFX - JBRS - Início - 19/07/2021
        IF ls_conta_razao-hkont IS INITIAL.
          "Campo Data Efetiva
          ge_ftpost-fnam = 'BSEG-ZFBDT'.
          CONCATENATE ge_zfie011-data_parcela+6(2) "Dia
                      ge_zfie011-data_parcela+4(2) "Mês
                      ge_zfie011-data_parcela(4)   "Ano
                 INTO ge_ftpost-fval.
          APPEND ge_ftpost TO gt_ftpost.
        ENDIF.
*** GFX - JBRS - Fim - 19/07/2021

        "Campo Montante
        ge_ftpost-fnam = 'BSEG-WRBTR'.
        ge_ftpost-fval = ge_zfie011-valor_parcela.
        "Retira os espaços
        SHIFT ge_ftpost-fval LEFT DELETING LEADING space.
        "Troca o . por ,
        TRANSLATE ge_ftpost-fval USING '.,'.
        APPEND ge_ftpost TO gt_ftpost.

        "Campo Atribuição
        ge_ftpost-fnam = 'BSEG-ZUONR'.
        ge_ftpost-fval = 'CARTAO'.
        APPEND ge_ftpost TO gt_ftpost.

      ENDLOOP.
    ELSEIF ge_formpag-dcr_meio_pagamento EQ 'DINHEIRO'.
      "Campo Chave de Lançamento
      ge_ftpost-fnam = 'RF05A-NEWBS'.
      ge_ftpost-fval = '40'.
      APPEND ge_ftpost TO gt_ftpost.

      CLEAR ge_zfit003.
      READ TABLE gt_zfit003 INTO ge_zfit003 WITH KEY natureza_operacao = 'S'.

      "Campo Conta
      ge_ftpost-fnam = 'BSEG-HKONT'.
      ge_ftpost-fval = ge_zfit003-hkont.
      APPEND ge_ftpost TO gt_ftpost.

      "Centro de Lucro
      ge_ftpost-fnam = 'COBL-PRCTR'.
      ge_ftpost-fval = ge_zfit002b-prctr.
      APPEND ge_ftpost TO gt_ftpost.

      "Campo Data Efetiva
      ge_ftpost-fnam = 'BSEG-VALUT'.
      CONCATENATE sy-datum+6(2) "Dia
                  sy-datum+4(2) "Mês
                  sy-datum(4)   "Ano
             INTO ge_ftpost-fval.
      APPEND ge_ftpost TO gt_ftpost.

      "Campo Montante
      ge_ftpost-fnam = 'BSEG-WRBTR'.
      ge_ftpost-fval = ge_formpag-dmbtr - t_vlr_troco.
      "Retira os espaços
      SHIFT ge_ftpost-fval LEFT DELETING LEADING space.
      "Troca o . por ,
      TRANSLATE ge_ftpost-fval USING '.,'.
      APPEND ge_ftpost TO gt_ftpost.

      "Campo Atribuição
      ge_ftpost-fnam = 'BSEG-ZUONR'.
      ge_ftpost-fval = 'DINHEIRO'.
      APPEND ge_ftpost TO gt_ftpost.
* APPC - ESF - Incluindo a forma de pagamento PIX - Início 16.02.2022
    ELSEIF ge_formpag-dcr_meio_pagamento EQ 'PIX'.

      "Verifica se encontrou as configurações de contas do razão
      IF ls_pix IS INITIAL AND ls_taxa_pix IS INITIAL.
        MESSAGE |Contas do razão não definidas na ZFI003 para o operador { sy-uname }| TYPE 'E'.
      ENDIF.

      "Chave de Lançamento
      ge_ftpost-fnam = 'RF05A-NEWBS'.
      ge_ftpost-fval = '40'.
      APPEND ge_ftpost TO gt_ftpost.

      "Verifica qual o dia da semana
      IF v_dsemana = 'Sábado' OR v_dsemana = 'Domingo' OR v_dsemana = 'FERIADO'.
        "Conta do Razão
        ge_ftpost-fnam = 'BSEG-HKONT'.
        ge_ftpost-fval = ls_pix-hkont_fds.
        APPEND ge_ftpost TO gt_ftpost.

      ELSE.

        "Conta do Razão
        ge_ftpost-fnam = 'BSEG-HKONT'.
        ge_ftpost-fval = ls_pix-hkont.
        APPEND ge_ftpost TO gt_ftpost.
      ENDIF.

      "Centro de Lucro
      ge_ftpost-fnam = 'COBL-PRCTR'.
      ge_ftpost-fval = ge_zfit002b-prctr.
      APPEND ge_ftpost TO gt_ftpost.

      "Data Efetiva
      ge_ftpost-fnam = 'BSEG-VALUT'.
      CONCATENATE sy-datum+6(2) "Dia
                  sy-datum+4(2) "Mês
                  sy-datum(4)   "Ano
             INTO ge_ftpost-fval.
      APPEND ge_ftpost TO gt_ftpost.


      "Montante
      ge_ftpost-fnam = 'BSEG-WRBTR'.

      IF ls_taxa_pix-taxa_pix <> 0. "APPC - FI.486184 - Hemilly Maria - 11/03/2024
        lv_taxa_vl = ls_taxa_pix-taxa_pix / 100.
        lv_taxa_vl = 1 - lv_taxa_vl.
        CONDENSE lv_taxa_vl NO-GAPS.
        lv_valor_d2 = ge_formpag-dmbtr * lv_taxa_vl.
        ge_ftpost-fval = lv_valor_d2.
      ELSE.
        ge_ftpost-fval = ge_formpag-dmbtr. "APPC - FI.486184 - Hemilly Maria - 11/03/2024
      ENDIF.

      "Retira os espaços
      SHIFT ge_ftpost-fval LEFT DELETING LEADING space.
      "Troca o . por ,
      TRANSLATE ge_ftpost-fval USING '.,'.
      APPEND ge_ftpost TO gt_ftpost.

      "Atribuição
      ge_ftpost-fnam = 'BSEG-ZUONR'.
      ge_ftpost-fval = 'PIX'.
      APPEND ge_ftpost TO gt_ftpost.

      "Texto
      ge_ftpost-fnam = 'BSEG-SGTXT'.
      ge_ftpost-fval = |Pag. Pix { sy-uname }|.
      APPEND ge_ftpost TO gt_ftpost.
* APPC - ESF - Incluindo a forma de pagamento PIX - Fim 16.02.2022

** APPC - RMS - 26/10/2022 - INICIO
      "Montando o item da TAXA PIX
      ADD 1 TO g_cont.

      "Entrando Item
      ge_ftpost-stype = 'P'.
      ge_ftpost-count = g_cont.

      "Chave de Lançamento
      ge_ftpost-fnam = 'RF05A-NEWBS'.
      ge_ftpost-fval = '40'.
      APPEND ge_ftpost TO gt_ftpost.

      "Conta do Razão
      ge_ftpost-fnam = 'BSEG-HKONT'.
      ge_ftpost-fval = ls_taxa_pix-hkont_taxa.
      APPEND ge_ftpost TO gt_ftpost.

      "Centro de Lucro
      ge_ftpost-fnam = 'COBL-PRCTR'.
      ge_ftpost-fval = ge_zfit002b-prctr.
      APPEND ge_ftpost TO gt_ftpost.

      "Data Efetiva
*      ge_ftpost-fnam = 'BSEG-VALUT'.
*      CONCATENATE sy-datum+6(2) "Dia
*                  sy-datum+4(2) "Mês
*                  sy-datum(4)   "Ano
*             INTO ge_ftpost-fval.
*      APPEND ge_ftpost TO gt_ftpost.

      "Montante
      ge_ftpost-fnam = 'BSEG-WRBTR'.
      lv_valor_d2 = ( ls_taxa_pix-taxa_pix * ge_formpag-dmbtr ) / 100.
      ge_ftpost-fval = lv_valor_d2.
      "Retira os espaços
      SHIFT ge_ftpost-fval LEFT DELETING LEADING space.
      "Troca o . por ,
      TRANSLATE ge_ftpost-fval USING '.,'.
      APPEND ge_ftpost TO gt_ftpost.

      "Centro de custo
      ge_ftpost-fnam = 'COBL-KOSTL'.
      ge_ftpost-fval = ls_taxa_pix-kostl.
      APPEND ge_ftpost TO gt_ftpost.

      "Atribuição
      ge_ftpost-fnam = 'BSEG-ZUONR'.
      ge_ftpost-fval = 'PIX'.
      APPEND ge_ftpost TO gt_ftpost.

      "Texto
      ge_ftpost-fnam = 'BSEG-SGTXT'.
      ge_ftpost-fval = |Pag. Pix { sy-uname }|.
      APPEND ge_ftpost TO gt_ftpost.
** APPC - RMS - 26/10/2022 - FIM

      CLEAR: lv_valor_d2,
             lv_taxa_vl.

    ELSEIF ge_formpag-dcr_meio_pagamento EQ 'CRÉDITO DO CLIENTE'.
      CLEAR g_totcred.
      LOOP AT gt_creditos INTO ge_creditos WHERE marc = 'X'.
        ADD ge_creditos-dmbtr TO g_totcred.
      ENDLOOP.

      IF ge_formpag-dmbtr NE g_totcred.
        l_dif = g_totcred - ge_formpag-dmbtr.
      ENDIF.

      IF l_dif GT 0.
        "Campo Chave de Lançamento
        ge_ftpost-fnam = 'RF05A-NEWBS'.
        ge_ftpost-fval = '11'.
        APPEND ge_ftpost TO gt_ftpost.

        "Campo Conta
        ge_ftpost-fnam = 'BSEG-HKONT'.
        ge_ftpost-fval = ge_zfie004-kunnr.
        APPEND ge_ftpost TO gt_ftpost.

        "Centro de Lucro
        ge_ftpost-fnam = 'COBL-PRCTR'.
        ge_ftpost-fval = ge_zfit002b-prctr.
        APPEND ge_ftpost TO gt_ftpost.

        "Campo Data Efetiva
        ge_ftpost-fnam = 'BSEG-ZFBDT'.
        CONCATENATE sy-datum+6(2) "Dia
                    sy-datum+4(2) "Mês
                    sy-datum(4)   "Ano
               INTO ge_ftpost-fval.
        APPEND ge_ftpost TO gt_ftpost.

        "Campo Montante
        ge_ftpost-fnam = 'BSEG-WRBTR'.
        ge_ftpost-fval = l_dif.
        "Retira os espaços
        SHIFT ge_ftpost-fval LEFT DELETING LEADING space.
        "Troca o . por ,
        TRANSLATE ge_ftpost-fval USING '.,'.
        APPEND ge_ftpost TO gt_ftpost.

        "Campo Atribuição
        ge_ftpost-fnam = 'BSEG-ZUONR'.
        ge_ftpost-fval = 'CRÉDITO DO CLIENTE'.
        APPEND ge_ftpost TO gt_ftpost.
      ENDIF.
    ENDIF.

  ENDLOOP.

  "Processar PA.
  "Documents to be cleared
  ge_ftclear-agkoa = 'D'.              "Account Type
  ge_ftclear-xnops = 'X'.              "Indicator: Select only open items which are not special G/L?
  ge_ftclear-agbuk = t_bukrs.          "Example company code
  ge_ftclear-agkon = ge_zfie004-kunnr. "Example Customer
  ge_ftclear-selfd = 'BELNR'.          "Selection Field
  ge_ftclear-agums = 'AX'.             "Códigos de razão especial que vai ser selecionado

  l_num_documento    = g_fatura.
  l_ano              = sy-datum(4).
  l_item             = '001'.

  CONCATENATE l_num_documento l_ano l_item INTO ge_ftclear-selvon.
  CONCATENATE l_num_documento l_ano l_item INTO ge_ftclear-selbis.
  APPEND ge_ftclear TO gt_ftclear.

  IF g_totcred GT 0.
    LOOP AT gt_creditos INTO ge_creditos WHERE marc = 'X'.
      l_num_documento    = ge_creditos-belnr.
      l_ano              = ge_creditos-gjahr.
      l_item             = ge_creditos-buzei.

      CONCATENATE l_num_documento l_ano l_item INTO ge_ftclear-selvon.
      CONCATENATE l_num_documento l_ano l_item INTO ge_ftclear-selbis.
      APPEND ge_ftclear TO gt_ftclear.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
    EXPORTING
      i_auglv                    = g_auglv
      i_tcode                    = g_tcode
      i_sgfunct                  = g_sgfunct
      i_no_auth                  = 'X'
    IMPORTING
      e_msgid                    = g_msgid
      e_msgno                    = g_msgno
      e_msgty                    = g_msgty
      e_msgv1                    = g_msgv1
      e_msgv2                    = g_msgv2
      e_msgv3                    = g_msgv3
      e_msgv4                    = g_msgv4
    TABLES
      t_blntab                   = gt_blntab
      t_ftclear                  = gt_ftclear
      t_ftpost                   = gt_ftpost
      t_fttax                    = gt_fttax
    EXCEPTIONS
      clearing_procedure_invalid = 1
      clearing_procedure_missing = 2
      table_t041a_empty          = 3
      transaction_code_invalid   = 4
      amount_format_error        = 5
      too_many_line_items        = 6
      company_code_invalid       = 7
      screen_not_found           = 8
      no_authorization           = 9
      OTHERS                     = 10.

  IF g_msgid EQ 'F5' AND
     g_msgno EQ 287.
    CLEAR l_ok.
    DO 120 TIMES.

      CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
        EXPORTING
          i_auglv                    = g_auglv
          i_tcode                    = g_tcode
          i_sgfunct                  = g_sgfunct
          i_no_auth                  = 'X'
        IMPORTING
          e_msgid                    = g_msgid
          e_msgno                    = g_msgno
          e_msgty                    = g_msgty
          e_msgv1                    = g_msgv1
          e_msgv2                    = g_msgv2
          e_msgv3                    = g_msgv3
          e_msgv4                    = g_msgv4
        TABLES
          t_blntab                   = gt_blntab
          t_ftclear                  = gt_ftclear
          t_ftpost                   = gt_ftpost
          t_fttax                    = gt_fttax
        EXCEPTIONS
          clearing_procedure_invalid = 1
          clearing_procedure_missing = 2
          table_t041a_empty          = 3
          transaction_code_invalid   = 4
          amount_format_error        = 5
          too_many_line_items        = 6
          company_code_invalid       = 7
          screen_not_found           = 8
          no_authorization           = 9
          OTHERS                     = 10.


      IF g_msgid EQ 'F5' AND
         g_msgno EQ 287.
        "Faz novamente
      ELSE.
        l_ok = 'X'.
        EXIT.
      ENDIF.

      WAIT UP TO 5 SECONDS.

    ENDDO.
  ENDIF.

  "Checa se aconteceu algum erro
  IF g_msgty = 'E'.
    g_msg_no = g_msgno.

    CLEAR g_mensagem.
    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        msg_id                 = g_msgid
        msg_no                 = g_msg_no
        msg_var1               = g_msgv1
        msg_var2               = g_msgv2
        msg_var3               = g_msgv3
        msg_var4               = g_msgv4
      IMPORTING
        msg_text               = g_mensagem
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        OTHERS                 = 3.

    CLEAR: gt_mess, gt_mess[].
    ge_mess-message = g_mensagem.
    APPEND ge_mess TO gt_mess.

    g_erro = 'X'.
  ELSE.
    IF sy-subrc NE 0.

      CASE sy-subrc.
        WHEN 1.
          g_mensagem = 'O procedimento de compensação transferido é inválido'.
        WHEN 2.
          g_mensagem = 'Procedimento de compensação não transferido'.
        WHEN 3.
          g_mensagem = 'Tabela dos procedimentos de compensação (T041A) está vazia'.
        WHEN 4.
          g_mensagem = 'Código de transação transferido não suportado'.
        WHEN 5.
          g_mensagem = 'Erro de formatação com o valor especificado'.
        WHEN 6.
          g_mensagem = 'O cupom contém muitos itens'.
        WHEN 7.
          g_mensagem = 'Empresa Inválida'.
        WHEN 8.
          g_mensagem = 'Próxima tela não encontrada'.
        WHEN 9.
          g_mensagem = 'Sem Autorização'.
        WHEN OTHERS.
          g_mensagem = 'Outro erro'.
      ENDCASE.

      CLEAR: gt_mess, gt_mess[].
      ge_mess-message = g_mensagem.
      APPEND ge_mess TO gt_mess.

      g_erro = 'X'.
    ELSE.
      "OK
      IF g_msgid = 'F5'  AND
         g_msgno = '312' AND
         g_msgty = 'S'.
        g_belnr = g_msgv1.
        g_gjahr = sy-datum(4).
        LOOP AT gt_zfie011 INTO ge_zfie011.
          ge_zfie011-belnr              = g_belnr.
          ge_zfie011-gjahr              = g_gjahr.
          MODIFY gt_zfie011 FROM ge_zfie011.
        ENDLOOP.
      ELSE.
        g_msg_no = g_msgno.

        CLEAR g_mensagem.
        CALL FUNCTION 'MESSAGE_PREPARE'
          EXPORTING
            msg_id                 = g_msgid
            msg_no                 = g_msg_no
            msg_var1               = g_msgv1
            msg_var2               = g_msgv2
            msg_var3               = g_msgv3
            msg_var4               = g_msgv4
          IMPORTING
            msg_text               = g_mensagem
          EXCEPTIONS
            function_not_completed = 1
            message_not_found      = 2
            OTHERS                 = 3.

        CLEAR: gt_mess, gt_mess[].
        ge_mess-message = g_mensagem.
        APPEND ge_mess TO gt_mess.

        g_erro = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

  IF g_erro = 'X'.
    IF gt_mess[] IS INITIAL.
      ge_mess-message = 'Erro'.
      APPEND ge_mess TO gt_mess.
    ENDIF.

    CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
      EXPORTING
        endpos_col   = 80
        endpos_row   = 10
        startpos_col = 1
        startpos_row = 1
        titletext    = 'Erro'
      IMPORTING
        choise       = g_tabix
      TABLES
        valuetab     = gt_mess
      EXCEPTIONS
        break_off    = 1
        OTHERS       = 2.

    SELECT SINGLE code docstat nftype INTO (l_code, l_docstat, l_nftype)
      FROM j_1bnfdoc
      WHERE docnum EQ g_docnum.

    IF l_code   = '100' AND
       l_docstat = '1'.
      CLEAR: gt_mess, gt_mess[].
      ge_mess-message = 'Documento emitido, por gentileza solicitar devolução.'.
      APPEND ge_mess TO gt_mess.

      CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
        EXPORTING
          endpos_col   = 80
          endpos_row   = 10
          startpos_col = 1
          startpos_row = 1
          titletext    = 'Erro'
        IMPORTING
          choise       = g_tabix
        TABLES
          valuetab     = gt_mess
        EXCEPTIONS
          break_off    = 1
          OTHERS       = 2.
      g_erro = 'X'.
    ELSE.
      IF l_nftype EQ 'ZB'.
        CALL FUNCTION 'J_1B_NFE_CHECK_CANCEL_PR_AUTH'
          EXPORTING
            iv_docnum          = g_docnum
          IMPORTING
            es_acttab_mod      = ge_nfe_active
          EXCEPTIONS
            status_not_allowed = 1
            no_entry           = 2
            OTHERS             = 3.

        CALL FUNCTION 'J_1B_NFE_CANCEL_PRIOR_AUTH'
          EXPORTING
            is_acttab      = ge_nfe_active
          EXCEPTIONS
            process_errors = 1
            OTHERS         = 2.

        IF sy-subrc EQ 0.

          CALL FUNCTION 'BAPI_BILLINGDOC_CANCEL1'
            EXPORTING
              billingdocument = g_fatura
            TABLES
              return          = gt_return3
              success         = gt_success.

          CLEAR ge_return3.
          READ TABLE gt_return3 INTO ge_return3 WITH KEY type   = 'S'
                                                         id     = 'VF'
                                                         number = '311'.
          IF sy-subrc EQ 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

            UPDATE j_1bnfe_active
              SET cancel    = 'X'
              WHERE docnum EQ g_docnum.
          ELSE.
            CLEAR: gt_mess, gt_mess[].
            LOOP AT gt_return3 INTO ge_return3.
              ge_mess-message = ge_return3-message.
              APPEND ge_mess TO gt_mess.
            ENDLOOP.

            CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
              EXPORTING
                endpos_col   = 80
                endpos_row   = 10
                startpos_col = 1
                startpos_row = 1
                titletext    = 'Erro'
              IMPORTING
                choise       = g_tabix
              TABLES
                valuetab     = gt_mess
              EXCEPTIONS
                break_off    = 1
                OTHERS       = 2.
            g_erro = 'X'.
          ENDIF.
        ENDIF.
      ELSE.
        CLEAR: gt_mess, gt_mess[].
        ge_mess-message = 'Por gentileza realizar cancelamento da nota no monitor J1BNFE.'.
        APPEND ge_mess TO gt_mess.

        CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
          EXPORTING
            endpos_col   = 80
            endpos_row   = 10
            startpos_col = 1
            startpos_row = 1
            titletext    = 'Erro'
          IMPORTING
            choise       = g_tabix
          TABLES
            valuetab     = gt_mess
          EXCEPTIONS
            break_off    = 1
            OTHERS       = 2.
        g_erro = 'X'.
      ENDIF.
    ENDIF.

  ENDIF.

  CALL FUNCTION 'POSTING_INTERFACE_END'
    EXPORTING
      i_bdcimmed              = 'X'
    EXCEPTIONS
      session_not_processable = 1
      OTHERS                  = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_CARTAO_CRED
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_cartao_cred.

  ge_formpag-modalidade = 'C'.
*  ge_formpag-cod_adq    = g_cod_adq.

  IF g_adiantamento = 'X'.
    IF rb_tef EQ 'X'.
      CLEAR ok_code.
      "Opção TEF não é válida para adiantamento!
      MESSAGE e045(zfi001).
    ENDIF.
  ENDIF.

  IF rb_tef EQ 'X'.
    ge_formpag-tp_pagto_cartao = 'TEF'.
  ELSE.
    ge_formpag-tp_pagto_cartao = 'POS'.
    IF ge_formpag-apr_pos IS INITIAL.
      CLEAR ok_code.
      "Campo "Aprovação POS" obrigatório para esta opção!
      MESSAGE e011(zfi001).
    ENDIF.
    IF ge_formpag-serial IS INITIAL.
      CLEAR ok_code.
      "Campo "Serial POS" obrigatório para esta opção!
      MESSAGE e038(zfi001).
    ENDIF.
    SELECT *
      FROM zfit015
      INTO CORRESPONDING FIELDS OF TABLE gt_f4_serial_cartao_credito
      WHERE empresa    EQ t_bukrs
        AND filial     EQ t_bupla.
    CLEAR ge_f4_serial_cartao_credito.
    READ TABLE gt_f4_serial_cartao_credito INTO ge_f4_serial_cartao_credito WITH KEY serial = ge_formpag-serial.
    ge_formpag-estabelecimento = ge_f4_serial_cartao_credito-estabelecimento.
    ge_formpag-adquirente      = ge_f4_serial_cartao_credito-cod_adq.
  ENDIF.

  IF ge_formpag-tp_pagto_cartao = 'TEF'.
    IF NOT ge_formpag-apr_pos IS INITIAL.
      CLEAR ok_code.
      "Campo "Aprovação POS" não deve ser preenchido!
      MESSAGE e027(zfi001).
    ENDIF.
    IF NOT ge_formpag-serial IS INITIAL.
      CLEAR ok_code.
      "Campo "Serial POS" não deve ser preenchido!
      MESSAGE e039(zfi001).
    ENDIF.
  ENDIF.

*  IF g_adiantamento = 'X'.
*    IF ge_formpag-parcelas GT 1.
*      CLEAR ok_code.
*      "Parcelamento no Adiantamento não é Permitido!
*      MESSAGE e032(zfi001).
*    ENDIF.
*  ENDIF.

  SELECT *
    FROM /gfxdsi/cct003
    INTO TABLE gt_cct003
    WHERE empresa    EQ t_bukrs
      AND filial     EQ t_bupla.

  SELECT bandeira
    FROM /gfxdsi/cct002
    INTO CORRESPONDING FIELDS OF TABLE gt_f4_cartao_credito
    FOR ALL ENTRIES IN gt_cct003
    WHERE cod_adq    EQ ge_formpag-cod_adq
      AND modalidade EQ 'C'
      AND chave_band EQ gt_cct003-chave_band
      AND bandeira   EQ ge_formpag-cartao.

  IF sy-subrc NE 0.
    CLEAR: ok_code.
    "Bandeira de Cartão Inválida!
    MESSAGE e016(zfi001).
  ENDIF.

  CONCATENATE ge_formpag-cod_adq '_' ge_formpag-cartao '_' 'CREDITO' INTO
    g_chave_band.

  SELECT *
    FROM /gfxdsi/cct003
    INTO TABLE gt_cct003
    WHERE empresa    EQ t_bukrs
      AND filial     EQ t_bupla
      AND chave_band EQ g_chave_band.

  IF sy-subrc NE 0.
    CLEAR: ok_code.
    "Regras de Negócio não cadastradas para Chave Bandeira &
    MESSAGE e019(zfi001) WITH g_chave_band.
  ENDIF.

  SORT gt_cct003 BY empresa filial chave_band.

  IF rb_tef EQ 'X'.
    PERFORM enviar_tef.
    IF g_erro EQ 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  CLEAR ge_cct003.
  READ TABLE gt_cct003 INTO ge_cct003 INDEX 1.

  ge_formpag-cliente            = ge_cct003-cliente.

  CLEAR: g_parcelaprim, g_parcelaprox.
  IF ge_formpag-parcelas GT 1.
    g_parcelaprox = t_vlr_recebendo / ge_formpag-parcelas.
    g_parcelaprim = t_vlr_recebendo - ( ( ge_formpag-parcelas - 1 ) * g_parcelaprox ).
  ELSE.
    g_parcelaprim = t_vlr_recebendo.
  ENDIF.

  CLEAR g_tabix.
  DO ge_formpag-parcelas TIMES.
    CLEAR ge_zfie011.

    ADD 1 TO g_tabix.

    ge_zfie011-ident_cartao       = g_ident_cartao.
    ge_zfie011-dcr_meio_pagamento = t_dcr_meio_pagamento.
    ge_zfie011-valor_total        = t_vlr_recebendo.
    IF g_tabix EQ 1.
      ge_zfie011-valor_parcela      = g_parcelaprim.
      ge_zfie011-data_parcela       = sy-datum.
      g_dataparcaux                 = sy-datum.
    ELSE.
      ge_zfie011-valor_parcela      = g_parcelaprox.
      DATA:
        g_data   TYPE datum,
        g_days   TYPE t5a4a-dlydy,
        g_months TYPE t5a4a-dlymo,
        g_years  TYPE t5a4a-dlyyr.
      g_days   = '00'.
      g_months = g_tabix - 1.
      g_years  = '+'.
      g_years  = '00'.
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = sy-datum
          days      = g_days
          months    = g_months
          signum    = '+'
          years     = g_years
        IMPORTING
          calc_date = g_dataparcela.

      IF g_dataparcela+6(2) NE g_data+6(2).
        ge_zfie011-data_parcela       = g_dataparcela - 1.
      ELSE.
        ge_zfie011-data_parcela       = g_dataparcela.
      ENDIF.
    ENDIF.

    ge_zfie011-tp_pagto_cartao    = ge_formpag-tp_pagto_cartao.
    ge_zfie011-modalidade         = ge_formpag-modalidade.
    ge_zfie011-cartao             = ge_formpag-cartao.
    ge_zfie011-num_parcela        = g_tabix.
    ge_zfie011-parcelas           = ge_formpag-parcelas.
    IF rb_tef EQ 'X'.
      ge_zfie011-apr_pos            = g_msg013.
    ELSE.
      ge_zfie011-apr_pos            = ge_formpag-apr_pos.
    ENDIF.
    ge_zfie011-cod_adq            = ge_formpag-cod_adq.
    ge_zfie011-belnr              = ''.
    ge_zfie011-gjahr              = ''.
    ge_zfie011-buzei              = ''.
    ge_zfie011-chave_bandeira     = g_chave_band.
    ge_zfie011-data_venda         = sy-datum.
    ge_zfie011-cliente            = ge_cct003-cliente.
    ge_zfie011-conta_comissao     = ''.
    ge_zfie011-tx_comissao        = ''.
    ge_zfie011-num_transacao_nsu  = ge_formpag-num_transacao_nsu.
    ge_zfie011-nsu_host           = ge_formpag-nsu_host.

    APPEND ge_zfie011 TO gt_zfie011.
  ENDDO.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_CARTAO_DEBT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_cartao_debt.

  ge_formpag-modalidade = 'D'.
  ge_formpag-parcelas   = '1'.

  IF g_adiantamento = 'X'.
    IF rb_tef EQ 'X'.
      CLEAR ok_code.
      "Opção TEF não é válida para adiantamento!
      MESSAGE e045(zfi001).
    ENDIF.
  ENDIF.

  IF rb_tef EQ 'X'.
    ge_formpag-tp_pagto_cartao = 'TEF'.
  ELSE.
    ge_formpag-tp_pagto_cartao = 'POS'.
    IF ge_formpag-apr_pos IS INITIAL.
      CLEAR ok_code.
      "Campo "Aprovação POS" obrigatório para esta opção!
      MESSAGE e011(zfi001).
    ENDIF.
    IF ge_formpag-serial IS INITIAL.
      CLEAR ok_code.
      "Campo "Serial POS" obrigatório para esta opção!
      MESSAGE e038(zfi001).
    ENDIF.
    SELECT *
      FROM zfit015
      INTO CORRESPONDING FIELDS OF TABLE gt_f4_serial_cartao_credito
      WHERE empresa    EQ t_bukrs
        AND filial     EQ t_bupla.
    CLEAR ge_f4_serial_cartao_credito.
    READ TABLE gt_f4_serial_cartao_credito INTO ge_f4_serial_cartao_credito WITH KEY serial = ge_formpag-serial.
    ge_formpag-estabelecimento = ge_f4_serial_cartao_credito-estabelecimento.
    ge_formpag-adquirente      = ge_f4_serial_cartao_credito-cod_adq.
  ENDIF.

  IF ge_formpag-tp_pagto_cartao = 'TEF'.
    IF NOT ge_formpag-apr_pos IS INITIAL.
      CLEAR ok_code.
      "Campo "Aprovação POS" não deve ser preenchido!
      MESSAGE e027(zfi001).
    ENDIF.
  ENDIF.

  SELECT *
    FROM /gfxdsi/cct003
    INTO TABLE gt_cct003
    WHERE empresa    EQ t_bukrs
      AND filial     EQ t_bupla.

  SELECT bandeira
    FROM /gfxdsi/cct002
    INTO CORRESPONDING FIELDS OF TABLE gt_f4_cartao_credito
    FOR ALL ENTRIES IN gt_cct003
    WHERE cod_adq    EQ ge_formpag-cod_adq
      AND modalidade EQ 'D'
      AND chave_band EQ gt_cct003-chave_band
      AND bandeira   EQ ge_formpag-cartao.

  IF sy-subrc NE 0.
    CLEAR: ok_code.
    "Bandeira de Cartão Inválida!
    MESSAGE e016(zfi001).
  ENDIF.

  CONCATENATE ge_formpag-cod_adq '_' ge_formpag-cartao '_' 'DEBITO' INTO
    g_chave_band.

  SELECT *
    FROM /gfxdsi/cct003
    INTO TABLE gt_cct003
    WHERE empresa    EQ t_bukrs
      AND filial     EQ t_bupla
      AND chave_band EQ g_chave_band.

  IF sy-subrc NE 0.
    CLEAR: ok_code.
    "Regras de Negócio não cadastradas para Chave Bandeira &
    MESSAGE e019(zfi001) WITH g_chave_band.
  ENDIF.

  SORT gt_cct003 BY empresa filial chave_band.

  IF rb_tef EQ 'X'.
    PERFORM enviar_tef.
  ENDIF.

  CLEAR ge_cct003.
  READ TABLE gt_cct003 INTO ge_cct003 INDEX 1.

  ge_formpag-cliente            = ge_cct003-cliente.

  CLEAR ge_zfie011.

  ge_zfie011-ident_cartao       = g_ident_cartao.
  ge_zfie011-dcr_meio_pagamento = t_dcr_meio_pagamento.
  ge_zfie011-valor_total        = t_vlr_recebendo.
  ge_zfie011-valor_parcela      = t_vlr_recebendo.
  ge_zfie011-data_parcela       = sy-datum.
  ge_zfie011-tp_pagto_cartao    = ge_formpag-tp_pagto_cartao.
  ge_zfie011-modalidade         = ge_formpag-modalidade.
  ge_zfie011-cartao             = ge_formpag-cartao.
  ge_zfie011-num_parcela        = ge_formpag-parcelas.
  ge_zfie011-parcelas           = ge_formpag-parcelas.
  IF rb_tef EQ 'X'.
    ge_zfie011-apr_pos            = g_msg013.
  ELSE.
    ge_zfie011-apr_pos            = ge_formpag-apr_pos.
  ENDIF.
  ge_zfie011-belnr              = ''.
  ge_zfie011-gjahr              = ''.
  ge_zfie011-buzei              = ''.
  ge_zfie011-chave_bandeira     = g_chave_band.
  ge_zfie011-data_venda         = sy-datum.
  ge_zfie011-cliente            = ge_cct003-cliente.
  ge_zfie011-conta_comissao     = ''.
  ge_zfie011-tx_comissao        = ''.
  ge_zfie011-num_transacao_nsu  = ge_formpag-num_transacao_nsu.
  ge_zfie011-nsu_host           = ge_formpag-nsu_host.

  ge_zfie011-cod_adq           = ge_formpag-cod_adq. " GFX - NIIS - F-PACI131

  APPEND ge_zfie011 TO gt_zfie011.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_CARTAO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_cartao.

  CLEAR g_est_ok.
  IF ge_devol-tp_pagto_cartao EQ 'TEF'.
    PERFORM canc_tef.
  ELSE.
    g_est_ok = 'X'.
  ENDIF.

  IF g_est_ok IS NOT INITIAL. "Estorno ok
    ge_devol-icon               = '@0V@'.
    ge_devol-estornado          = 'X'.
    MODIFY gt_devol FROM ge_devol INDEX g_tabix_dev.
  ELSE.
    EXIT.
  ENDIF.

  CLEAR ge_cct003.
  READ TABLE gt_cct003 INTO ge_cct003 INDEX 1.

  CLEAR ge_zfie011.

  ge_zfie011-ident_cartao       = g_ident_cartao.
  ge_zfie011-dcr_meio_pagamento = t_dcr_meio_pagamento.
  ge_zfie011-valor_total        = t_vlr_recebendo.
  ge_zfie011-valor_parcela      = t_vlr_recebendo.
  ge_zfie011-data_parcela       = sy-datum.
  ge_zfie011-tp_pagto_cartao    = ge_formpag-tp_pagto_cartao.
  ge_zfie011-modalidade         = ge_formpag-modalidade.
  ge_zfie011-cartao             = ge_formpag-cartao.
  ge_zfie011-num_parcela        = ge_formpag-parcelas.
  ge_zfie011-parcelas           = ge_formpag-parcelas.
  IF rb_tef EQ 'X'.
    ge_zfie011-apr_pos            = g_msg013.
  ELSE.
    ge_zfie011-apr_pos            = ge_formpag-apr_pos.
  ENDIF.
  ge_zfie011-belnr              = ''.
  ge_zfie011-gjahr              = ''.
  ge_zfie011-buzei              = ''.
  ge_zfie011-chave_bandeira     = g_chave_band.
  ge_zfie011-data_venda         = sy-datum.
  ge_zfie011-cliente            = ge_cct003-cliente.
  ge_zfie011-conta_comissao     = ''.
  ge_zfie011-tx_comissao        = ''.
  ge_zfie011-num_transacao_nsu  = ge_formpag-num_transacao_nsu.
  ge_zfie011-nsu_host           = ge_formpag-nsu_host.

  APPEND ge_zfie011 TO gt_zfie011.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form INFORMAR_CREDITO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM informar_credito .

  IF t_vlr_credito_utilizado EQ 0.
    CLEAR ok_code.
    "Favor informar o valor do crédito!
    MESSAGE e012(zfi001).
  ENDIF.

  IF t_vlr_credito_utilizado GT ge_zfie004-netwr.
    CLEAR ok_code.
    "Valor a ser Utilizado & não pode ser maior que Valor da Venda!
    MESSAGE e023(zfi001).
  ENDIF.

  IF t_vlr_credito_utilizado GT t_vlr_tot_credito.
    CLEAR ok_code.
    "Valor a ser Utilizado & não pode ser maior que Valor Total de Crédito!
    MESSAGE e047(zfi001).
  ENDIF.

  CLEAR: gt_formpag, gt_formpag[].
*** GFX - DFLC - F-PACI244 - 02/09/2021
  CLEAR ge_formpag.
*** GFX - DFLC - F-PACI244 - Fim
  IF t_vlr_credito_utilizado GT 0.
    ge_formpag-dcr_meio_pagamento = 'CRÉDITO DO CLIENTE'.
    ge_formpag-dmbtr              = t_vlr_credito_utilizado.
    APPEND ge_formpag TO gt_formpag.
  ENDIF.

  CLEAR g_totcred.
  LOOP AT gt_creditos INTO ge_creditos.

    ge_creditos-marc = 'X'.
    MODIFY gt_creditos FROM ge_creditos.

    ADD ge_creditos-dmbtr TO g_totcred.

    IF g_totcred GE t_vlr_credito_utilizado.
      EXIT.
    ENDIF.

  ENDLOOP.

  t_vlr_tot_recebido  = t_vlr_credito_utilizado.
  t_vlr_tot_a_receber = ge_zfie004-netwr.
  t_vlr_tot_restante  = t_vlr_tot_a_receber - t_vlr_tot_recebido.
*  IF t_vlr_tot_recebido GT t_vlr_tot_a_receber.
*    t_vlr_troco        = t_vlr_tot_recebido - t_vlr_tot_a_receber.
*    t_vlr_tot_restante = 0.
*  ENDIF.

  CLEAR t_vlr_troco.
  CLEAR: ok_code, sy-ucomm.

  CALL SCREEN '9003' STARTING AT 02 02
                     ENDING AT 80 20.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FATURAR_REMESSA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM faturar_remessa .

  DATA: l_cont1 TYPE i,
        l_cont2 TYPE i,
        l_rfmng TYPE vbfa-rfmng.

  CLEAR g_fatura.
  SELECT SINGLE vbfa~vbeln INTO g_fatura
    FROM vbfa
    INNER JOIN vbrk  ON vbrk~vbeln EQ vbfa~vbeln
    WHERE vbfa~vbelv   EQ ge_zfie004-vgbel
      AND vbfa~vbtyp_n EQ 'M'
      AND vbfa~vbtyp_v EQ 'C'
      AND vbrk~fksto   EQ ''.

  IF NOT g_fatura IS INITIAL.
    CLEAR: gt_mess, gt_mess[].
    ge_mess-message = 'Documento já faturado'.
    APPEND ge_mess TO gt_mess.

    CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
      EXPORTING
        endpos_col   = 80
        endpos_row   = 10
        startpos_col = 1
        startpos_row = 1
        titletext    = 'Erro'
      IMPORTING
        choise       = g_tabix
      TABLES
        valuetab     = gt_mess
      EXCEPTIONS
        break_off    = 1
        OTHERS       = 2.

    g_erro = 'X'.
  ENDIF.

  CHECK g_erro IS INITIAL.

  CLEAR: gt_billingdatain,     gt_billingdatain[],
         gt_return1,           gt_return1[],
         gt_success,           gt_success[],
         gt_errors,            gt_errors[].

  CLEAR: ge_billingdatain,
         ge_return1,
         ge_success,
         gt_errors.

  CLEAR: g_fatura, g_erro.

  ge_billingdatain-ref_doc    = ge_zfie004-vbeln.
  ge_billingdatain-ref_doc_ca = 'J'.
  APPEND ge_billingdatain TO gt_billingdatain.

  CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'
    TABLES
      billingdatain = gt_billingdatain
      return        = gt_return1
      errors        = gt_errors
      success       = gt_success.

  READ TABLE gt_return1 INTO ge_return1 WITH KEY type   = 'E'.

  IF sy-subrc EQ 0.
    g_erro = 'X'.
  ELSE.
    READ TABLE gt_return1 INTO ge_return1 WITH KEY type   = 'I'
                                                   id     = 'VF'
                                                   number = '044'.
    IF sy-subrc EQ 0.
      g_erro = 'X'.
    ENDIF.
  ENDIF.

  IF g_erro = 'X'.
    CLEAR: gt_mess, gt_mess[].
    LOOP AT gt_return1 INTO ge_return1.
      ge_mess-message = ge_return1-message.
      APPEND ge_mess TO gt_mess.
    ENDLOOP.

    CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
      EXPORTING
        endpos_col   = 80
        endpos_row   = 10
        startpos_col = 1
        startpos_row = 1
        titletext    = 'Erro'
      IMPORTING
        choise       = g_tabix
      TABLES
        valuetab     = gt_mess
      EXCEPTIONS
        break_off    = 1
        OTHERS       = 2.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

    CLEAR ge_success.
    READ TABLE gt_success INTO ge_success INDEX 1.

    IF sy-subrc EQ 0.
      g_fatura = ge_success-bill_doc.
    ENDIF.

    DO 10 TIMES.

      WAIT UP TO 1 SECONDS.

      CLEAR ge_vbrp.
      SELECT SINGLE vbeln posnr INTO (ge_vbrp-vbeln, ge_vbrp-posnr)
        FROM vbrp
        WHERE vbeln = g_fatura.

      CHECK sy-subrc EQ 0.

      CLEAR: g_docnum,
             g_nfenum,
             g_regio,
             g_nfyear,
             g_nfmonth,
             g_stcd1,
             g_model,
             g_serie,
             g_nfnum9,
             g_docnum9,
             g_cdv.
      SELECT SINGLE
        j_1bnfdoc~docnum
        j_1bnfdoc~nfenum
        j_1bnfe_active~regio
        j_1bnfe_active~nfyear
        j_1bnfe_active~nfmonth
        j_1bnfe_active~stcd1
        j_1bnfe_active~model
        j_1bnfe_active~serie
        j_1bnfe_active~nfnum9
        j_1bnfe_active~docnum9
        j_1bnfe_active~cdv
        INTO (g_docnum,
              g_nfenum,
              g_regio,
              g_nfyear,
              g_nfmonth,
              g_stcd1,
              g_model,
              g_serie,
              g_nfnum9,
              g_docnum9,
              g_cdv)
        FROM j_1bnfdoc
        INNER JOIN j_1bnflin      ON j_1bnflin~docnum = j_1bnfdoc~docnum
        INNER JOIN j_1bnfe_active ON j_1bnfe_active~docnum = j_1bnfdoc~docnum
        WHERE j_1bnflin~reftyp EQ 'BI'
          AND j_1bnflin~refkey EQ ge_vbrp-vbeln.
*          AND j_1bnflin~refitm EQ ge_vbrp-posnr.

      IF sy-subrc EQ 0.
        g_acckey-regio   = g_regio.
        g_acckey-nfyear  = g_nfyear.
        g_acckey-nfmonth = g_nfmonth.
        g_acckey-stcd1   = g_stcd1.
        g_acckey-model   = g_model.
        g_acckey-serie   = g_serie.
        g_acckey-nfnum   = g_nfnum9.
        g_acckey-docnum  = g_docnum9.
        g_acckey-cdv     = g_cdv.
        EXIT.
      ENDIF.
    ENDDO.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADIANTAR_PAGAMENTO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM adiantar_pagamento .

  CLEAR: g_ident_cartao, gt_zfie011, gt_zfie011[], gt_cartao, gt_cartao[], gt_imprcartao, gt_imprcartao[].
  g_operacao  = 'ADIA'.

  CLEAR: gt_f4_meio_pagto5, gt_f4_meio_pagto5[].
  ge_f4_meio_pagto5-dcr_meio_pagamento = 'DINHEIRO'.
  APPEND ge_f4_meio_pagto5 TO gt_f4_meio_pagto5.
  ge_f4_meio_pagto5-dcr_meio_pagamento = 'CARTÃO DE CRÉDITO'.
  APPEND ge_f4_meio_pagto5 TO gt_f4_meio_pagto5.
  ge_f4_meio_pagto5-dcr_meio_pagamento = 'CARTÃO DE DÉBITO'.
  APPEND ge_f4_meio_pagto5 TO gt_f4_meio_pagto5.
  ge_f4_meio_pagto5-dcr_meio_pagamento = 'CHEQUE'.
  APPEND ge_f4_meio_pagto5 TO gt_f4_meio_pagto5.
  ge_f4_meio_pagto5-dcr_meio_pagamento = 'PIX'.
  APPEND ge_f4_meio_pagto5 TO gt_f4_meio_pagto5.

  SORT gt_f4_meio_pagto5 BY dcr_meio_pagamento.

  CLEAR: gt_zfit006, gt_zfit006[].
  SELECT  * INTO TABLE gt_zfit006
    FROM zfit006
    WHERE bukrs    EQ t_bukrs
      AND bupla    EQ t_bupla
      AND caixa    EQ t_caixa
      AND bname    EQ sy-uname
      AND data     EQ sy-datum.

  CLEAR: ge_zfit006.
  READ TABLE gt_zfit006 INTO ge_zfit006 WITH KEY operacao = 'FECH'.

  IF sy-subrc EQ 0.
    CLEAR ok_code.
    "Caixa & fechado para recebimento!
    MESSAGE e009(zfi001) WITH t_caixa.
  ENDIF.

  PERFORM executar_adiantamento.
  PERFORM buscar_dados.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXECUTAR_ADIANTAMENTO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM executar_adiantamento .

  CLEAR: gt_formpag, gt_formpag[],
         t_kunnr,
         t_name1,
         t_vlr_tot_a_receber,
         t_dcr_meio_pagamento,
         t_vlr_recebendo,
         t_vlr_tot_recebido,
         t_vlr_tot_restante,
         t_vlr_troco.

  CALL SCREEN '9007' STARTING AT 02 02
                     ENDING AT 80 20.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form LANCAR_ADIANTAMENTO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM lancar_adiantamento .

  IF t_vlr_tot_restante NE 0.
    CLEAR ok_code.
    "Total Recebido & não é suficiente para pagamento!
    MESSAGE e010(zfi001) WITH t_vlr_tot_recebido.
  ENDIF.

  IF t_vlr_tot_recebido EQ 0.
    CLEAR ok_code.
    "Total Recebido & não é suficiente para pagamento!
    MESSAGE e010(zfi001) WITH t_vlr_tot_recebido.
  ENDIF.

  CLEAR g_erro.

  PERFORM lanc_contabil_fb01.

  CHECK g_erro IS INITIAL.

  PERFORM gravar_tabelas_adia.

  CHECK g_erro IS INITIAL.

*  PERFORM imprimir_recibo_adia.

  "Chamando o programa de impressão de comprovante
  SUBMIT zfir007 WITH p_bukrs  EQ t_bukrs
                 WITH p_bupla  EQ t_bupla
                 WITH p_caixa  EQ t_caixa
                 WITH p_belnr  EQ g_belnr
                 WITH p_kunnr  EQ t_kunnr
                 AND RETURN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form LANC_CONTABIL_FB05
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM lanc_contabil_fb01.

  DATA: l_num_documento TYPE bseg-belnr,
        l_ano           TYPE bseg-gjahr,
        l_item          TYPE bseg-buzei,
        l_cartao        TYPE bsid-dmbtr,
        l_dinheiro      TYPE bsid-dmbtr,
        l_cheque        TYPE bsid-dmbtr,
        l_pix           TYPE bsid-dmbtr,
        l_tot           TYPE bsid-dmbtr,
        lv_taxa_vl      TYPE char11,
        lv_valor_d2     TYPE p DECIMALS 2.

  DATA: l_tabix TYPE sy-tabix.

  FREE g_nova_conc.

  "Busca conta do razão para nova conciliação
  SELECT SINGLE operacao
              , bupla
              , hkont
    FROM zfit003
    WHERE operacao EQ 'CONC'
      AND bupla    EQ @t_bupla
    INTO @DATA(ls_conta_razao).

  SELECT * INTO TABLE gt_zfit003
    FROM zfit003
    WHERE operacao EQ 'ABER'
      AND bupla    EQ t_bupla.

* APPC - ESF - Tratamento PIX - Início 16.02.2022

  "Busca as contas do razão do PIX
  SELECT SINGLE hkont,    " Conta Razão
                hkont_fds " Cobta Razão Fim de Semana
    FROM zfit003
    WHERE operacao EQ 'PIX'
      AND bupla    EQ @t_bupla
      AND hkont     <> ''
      AND hkont_fds <> ''
    INTO @DATA(ls_pix).

*APPC - RMS - 26/10/2022
  "Busca centro de custo, conta e taxa PIX
  SELECT SINGLE kostl,
                hkont_taxa,
                taxa_pix
    FROM zfit003
    WHERE operacao   EQ 'PIX'
      AND bupla      EQ @t_bupla
      AND kostl      <> ''
      AND hkont_taxa <> ''
      AND taxa_pix   <> ''
    INTO @DATA(ls_taxa_pix).

  "Verifica se é dia útil
  DATA v_dsemana TYPE t246-langt.
  DATA v_data    TYPE sy-datum.
  DATA v_idioma  TYPE sy-langu.

  v_data = sy-datum.
  v_idioma = 'PT'.

  CALL FUNCTION 'ISP_GET_WEEKDAY_NAME'
    EXPORTING
      date        = v_data
      language    = v_idioma
    IMPORTING
      longtext    = v_dsemana
    EXCEPTIONS
      calendar_id = 1
      date_error  = 2
      not_found   = 3
      wrong_input = 4
      OTHERS      = 5.

  IF v_dsemana NE 'Sábado' AND v_dsemana NE 'Domingo'.

    "Verifica se é feriado
    SELECT SINGLE zzdia_semana
      FROM a901
      INNER JOIN konp
      ON ( a901~knumh = konp~knumh )
        WHERE a901~werks    = 'F000'    AND
              a901~datbi    = @v_data   AND
              konp~loevm_ko = ''
      INTO @v_dsemana.

  ENDIF.

* APPC - ESF - Tratamento PIX - Fim 16.02.2022

  g_text_oper = 'Texto Adiantamento'.

  CLEAR: ge_documentheader,
         gt_accountgl,         gt_accountgl[],
         gt_accountreceivable, gt_accountreceivable[],
         gt_currencyamount,    gt_currencyamount[],
         gt_return,            gt_return[].
*"      ACCOUNTRECEIVABLE STRUCTURE  BAPIACAR09 OPTIONAL
  "inicio - LD - FI - DNF - DEVK917924 - 04/08/2020
  DATA : it_bapiparex TYPE TABLE OF bapiparex,
         wa_bapiparex TYPE bapiparex.
  "fim - LD - FI - DNF - DEVK917924 - 04/08/2020



  "DOCUMENTHEADER
  ge_documentheader-doc_date   = sy-datum.
  ge_documentheader-doc_type   = 'DZ'.
  ge_documentheader-comp_code  = t_bukrs.
  ge_documentheader-pstng_date = sy-datum.
  ge_documentheader-ref_doc_no = g_text_oper.
  ge_documentheader-header_txt = g_text_oper.
  ge_documentheader-username   = sy-uname.


  CLEAR: g_totcred, l_cartao, l_dinheiro, l_cheque.
  CLEAR g_cont.
  LOOP AT gt_formpag INTO ge_formpag.

    "Definindo Contador.
    ADD 1 TO g_cont.

    IF ge_formpag-dcr_meio_pagamento EQ 'CARTÃO DE CRÉDITO'
    OR ge_formpag-dcr_meio_pagamento EQ 'CARTÃO DE DÉBITO'.
      SUBTRACT 1 FROM g_cont.

      LOOP AT gt_zfie011 INTO ge_zfie011 WHERE ident_cartao EQ ge_formpag-ident_cartao.
        ADD 1 TO g_cont.

        ge_zfie011-buzei              = g_cont.
        MODIFY gt_zfie011 FROM ge_zfie011.

        "ACCOUNTRECEIVABLE
        CLEAR ge_accountreceivable.
        ge_accountreceivable-itemno_acc = g_cont.

        IF ls_conta_razao IS NOT INITIAL AND ls_conta_razao-hkont IS NOT INITIAL.
          IF ge_accountreceivable-gl_account IS INITIAL.
            ge_accountreceivable-gl_account = ls_conta_razao-hkont.
          ENDIF.
          ge_ftpost-fval = ls_conta_razao-hkont.
          g_nova_conc    = 'X'.
        ELSE.
          ge_accountreceivable-customer = ge_zfie011-cliente.
        ENDIF.

        ge_accountreceivable-item_text  = ge_formpag-dcr_meio_pagamento.
        ge_accountreceivable-comp_code  = t_bukrs.
        ge_accountreceivable-profit_ctr = ge_zfit002b-prctr.
        APPEND ge_accountreceivable TO gt_accountreceivable.

        IF ge_accountreceivable-gl_account IS NOT INITIAL AND
           ls_conta_razao-hkont IS NOT INITIAL.
          FREE: ge_accountreceivable-gl_account.
        ENDIF.

        "CURRENCYAMOUNT
        ge_currencyamount-itemno_acc = g_cont.
        ge_currencyamount-currency   = 'BRL'.
        ge_currencyamount-amt_doccur = ge_zfie011-valor_parcela.
        APPEND ge_currencyamount TO gt_currencyamount.

        ADD ge_zfie011-valor_parcela TO l_cartao.
      ENDLOOP.
    ELSEIF ge_formpag-dcr_meio_pagamento EQ 'PIX'.

      "Verifica se encontrou as configurações de contas do razão
      IF ls_pix IS INITIAL AND ls_taxa_pix IS INITIAL.
        MESSAGE |Contas do razão não definidas na ZFI003 para o operador { sy-uname }| TYPE 'E'.
      ENDIF.

      "ACCOUNTGL
      CLEAR ge_accountgl.
      ge_accountgl-itemno_acc = g_cont.

      "Verifica qual o dia da semana
      IF v_dsemana = 'Sábado' OR v_dsemana = 'Domingo' OR v_dsemana = 'FERIADO'.
        ge_accountgl-gl_account = ls_pix-hkont_fds.
      ELSE.
        ge_accountgl-gl_account = ls_pix-hkont.
      ENDIF.

      ge_accountgl-item_text  = |Adiantamento PIX - { sy-uname }|.
      ge_accountgl-pstng_date = sy-datum.
      ge_accountgl-comp_code  = t_bukrs.
      ge_accountgl-profit_ctr = ge_zfit002b-prctr.
      APPEND ge_accountgl TO gt_accountgl.

      "CURRENCYAMOUNT
      ge_currencyamount-itemno_acc = g_cont.
      ge_currencyamount-currency   = 'BRL'.
      lv_taxa_vl = ls_taxa_pix-taxa_pix / 100.
      lv_taxa_vl = 1 - lv_taxa_vl.
      CONDENSE lv_taxa_vl NO-GAPS.
      lv_valor_d2 = ge_formpag-dmbtr * lv_taxa_vl.
      ge_currencyamount-amt_doccur = lv_valor_d2.
      ADD ge_formpag-dmbtr TO l_pix.
      APPEND ge_currencyamount TO gt_currencyamount.

*APPC - RMS - 26/10/2022 - Adicionando o item da TAXA PIX - INICIO
      "ACCOUNTGL
      CLEAR ge_accountgl.
      ADD 1 TO g_cont.
      ge_accountgl-itemno_acc = g_cont.

      ge_accountgl-gl_account = ls_taxa_pix-hkont_taxa.
      ge_accountgl-item_text  = |Adiantamento PIX - { sy-uname }|.
      ge_accountgl-pstng_date = sy-datum.
      ge_accountgl-comp_code  = t_bukrs.
      ge_accountgl-costcenter = ls_taxa_pix-kostl.
      ge_accountgl-profit_ctr = ge_zfit002b-prctr.
      APPEND ge_accountgl TO gt_accountgl.

      "CURRENCYAMOUNT
      ge_currencyamount-itemno_acc = g_cont.
      ge_currencyamount-currency   = 'BRL'.
      lv_valor_d2 = ( ls_taxa_pix-taxa_pix * ge_formpag-dmbtr ) / 100.
      ge_currencyamount-amt_doccur = lv_valor_d2.
      APPEND ge_currencyamount TO gt_currencyamount.
*APPC - RMS - 26/10/2022 - Adicionando o item da TAXA PIX - FIM

    ELSEIF ge_formpag-dcr_meio_pagamento EQ 'DINHEIRO' OR
           ge_formpag-dcr_meio_pagamento EQ 'CHEQUE'.
      "ACCOUNTGL
      CLEAR ge_zfit003.
      READ TABLE gt_zfit003 INTO ge_zfit003 WITH KEY natureza_operacao = 'S'.

      CLEAR ge_accountgl.
      ge_accountgl-itemno_acc = g_cont.
      ge_accountgl-gl_account = ge_zfit003-hkont.
      ge_accountgl-item_text  = ge_formpag-dcr_meio_pagamento.
      ge_accountgl-pstng_date = sy-datum.
      ge_accountgl-comp_code  = t_bukrs.
      ge_accountgl-profit_ctr = ge_zfit002b-prctr.
      APPEND ge_accountgl TO gt_accountgl.

      "CURRENCYAMOUNT
      ge_currencyamount-itemno_acc = g_cont.
      ge_currencyamount-currency   = 'BRL'.
      IF ge_formpag-dcr_meio_pagamento EQ 'DINHEIRO'.
        ge_currencyamount-amt_doccur = ge_formpag-dmbtr - t_vlr_troco.
        l_dinheiro = ge_formpag-dmbtr - t_vlr_troco.
      ELSE.
        ge_currencyamount-amt_doccur = ge_formpag-dmbtr.
        ADD ge_formpag-dmbtr TO l_cheque.
      ENDIF.
      APPEND ge_currencyamount TO gt_currencyamount.
*    ELSEIF ge_formpag-dcr_meio_pagamento EQ 'CHEQUE'.
*      "ACCOUNTGL
*
*      CLEAR ge_accountreceivable.
*      ge_accountreceivable-itemno_acc = g_cont.
*      ge_accountreceivable-customer   = t_kunnr.
*      ge_accountreceivable-item_text  = g_text_oper.
*      ge_accountreceivable-comp_code  = t_bukrs.
*      ge_accountreceivable-profit_ctr = ge_zfit002b-prctr.
*      ge_accountreceivable-sp_gl_ind  = 'X'.
*      APPEND ge_accountreceivable TO gt_accountreceivable.
*
*      "CURRENCYAMOUNT
*      ge_currencyamount-itemno_acc = g_cont.
*      ge_currencyamount-currency   = 'BRL'.
*      ge_currencyamount-amt_doccur = ge_formpag-dmbtr.
*      APPEND ge_currencyamount TO gt_currencyamount.
    ENDIF.

  ENDLOOP.

  "ACCOUNTRECEIVABLE
  CLEAR ge_zfit003.
  READ TABLE gt_zfit003 INTO ge_zfit003 WITH KEY natureza_operacao = 'S'.

  IF l_cheque GT 0.
*GFX - LUVA - PACI82 - Melhorias para lançamento de cheque - Início

    IF ge_formpag-dcr_meio_pagamento EQ 'CHEQUE'.

      CLEAR: gt_accountgl, gt_currencyamount. " Limpa linha do razão/montantes

      LOOP AT gt_formpag INTO ge_formpag.

        "Adiciona crédito ao cliente
        g_cont = g_cont + 1.
        ge_accountreceivable-itemno_acc = g_cont.
        ge_accountreceivable-customer   = t_kunnr.
        ge_accountreceivable-item_text  = 'CHEQUE'.
        ge_accountreceivable-bline_date  = ge_formpag-data_bom_para.
        ge_accountreceivable-comp_code  = t_bukrs.
        ge_accountreceivable-sp_gl_ind  = 'X'.
        ge_accountreceivable-profit_ctr = ge_zfit002b-prctr.
        APPEND ge_accountreceivable TO gt_accountreceivable.

        ge_currencyamount-itemno_acc = g_cont.
        ge_currencyamount-currency   = 'BRL'.
*      ge_currencyamount-amt_doccur = l_cheque.
        ge_currencyamount-amt_doccur = ge_formpag-dmbtr.
        APPEND ge_currencyamount TO gt_currencyamount.

      ENDLOOP.
      "Adiciona débito
      g_cont = g_cont + 1.
      ge_accountreceivable-itemno_acc = g_cont.
      ge_accountreceivable-customer   = t_kunnr.
      ge_accountreceivable-item_text  = g_text_oper.
      ge_accountreceivable-bline_date  = ge_formpag-data_bom_para.
      ge_accountreceivable-comp_code  = t_bukrs.
      "Inicio - LD - FI - DNF - DEVK917924 - 04/08/2020
*      ge_accountreceivable-sp_gl_ind  = 'A'.
      "fim - LD - FI - DNF - DEVK917924 - 04/08/2020
      ge_accountreceivable-profit_ctr = ge_zfit002b-prctr.
      APPEND ge_accountreceivable TO gt_accountreceivable.

      ge_currencyamount-itemno_acc = g_cont.
      ge_currencyamount-currency   = 'BRL'.
      ge_currencyamount-amt_doccur = l_cheque * -1.
      APPEND ge_currencyamount TO gt_currencyamount.

*GFX - LUVA - PACI82 - Melhorias para lançamento de cheque - Fim
    ELSE.
      g_cont = g_cont + 1.
      ge_accountreceivable-itemno_acc = g_cont.
      ge_accountreceivable-customer   = t_kunnr.
      ge_accountreceivable-item_text  = g_text_oper.
      ge_accountreceivable-comp_code  = t_bukrs.
      ge_accountreceivable-sp_gl_ind  = 'X'.
      ge_accountreceivable-profit_ctr = ge_zfit002b-prctr.
      APPEND ge_accountreceivable TO gt_accountreceivable.

      ge_currencyamount-itemno_acc = g_cont.
      ge_currencyamount-currency   = 'BRL'.
      ge_currencyamount-amt_doccur = l_cheque * -1.
      APPEND ge_currencyamount TO gt_currencyamount.
    ENDIF.
  ENDIF.

  l_tot = l_cartao + l_dinheiro + l_pix.

  IF l_tot GT 0.
    g_cont = g_cont + 1.
    ge_accountreceivable-itemno_acc = g_cont.
    ge_accountreceivable-customer   = t_kunnr.
    ge_accountreceivable-item_text  = g_text_oper.
    ge_accountreceivable-comp_code  = t_bukrs.
    "Inicio - LD - FI - DNF - DEVK917924 - 04/08/2020
*    ge_accountreceivable-sp_gl_ind  = 'A'.
    "Fim - LD - FI - DNF - DEVK917924 - 04/08/2020
    ge_accountreceivable-profit_ctr = ge_zfit002b-prctr.
    APPEND ge_accountreceivable TO gt_accountreceivable.

    ge_currencyamount-itemno_acc = g_cont.
    ge_currencyamount-currency   = 'BRL'.
    ge_currencyamount-amt_doccur = l_tot * -1.
    APPEND ge_currencyamount TO gt_currencyamount.
  ENDIF.

  "Inicio - LD - FI - DNF - DEVK917924 - 04/08/2020
*     Populate the Extension table
  wa_bapiparex-structure  = 'POSTING_KEY'.
  wa_bapiparex-valuepart1 = '40'.            " Item number
  wa_bapiparex-valuepart2 = '11'.            " Posting Key
  APPEND wa_bapiparex TO it_bapiparex.
  "Fim - LD - FI - DNF - DEVK917924 - 04/08/2020

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader    = ge_documentheader
    IMPORTING
      obj_type          = g_obj_type
      obj_key           = g_obj_key
      obj_sys           = g_obj_sys
    TABLES
      accountgl         = gt_accountgl
      accountreceivable = gt_accountreceivable
      currencyamount    = gt_currencyamount
      return            = gt_return.

  READ TABLE gt_return INTO ge_return WITH KEY type   = 'S'
                                               id     = 'RW'
                                               number = '605'.
  CLEAR g_erro.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    g_belnr = g_obj_key+00(10).
    g_bukrs = g_obj_key+10(04).
    g_gjahr = g_obj_key+14(04).

    "Limpa os dados
    FREE: v_dsemana,
          v_data,
          v_idioma,
          ls_pix.

    LOOP AT gt_zfie011 INTO ge_zfie011.
      ge_zfie011-belnr              = g_belnr.
      ge_zfie011-gjahr              = g_gjahr.
      MODIFY gt_zfie011 FROM ge_zfie011.
    ENDLOOP.

    g_belnr = g_obj_key+00(10).
    g_bukrs = g_obj_key+10(04).
    g_gjahr = g_obj_key+14(04).

    DO 60 TIMES.

      WAIT UP TO 2 SECONDS.

      UPDATE bseg
        SET zlspr = ''
        WHERE belnr = g_belnr
          AND bukrs = g_bukrs
          AND gjahr = g_gjahr.

      IF sy-subrc EQ 0.
        EXIT.
      ENDIF.

    ENDDO.
  ELSE.
    CLEAR: gt_mess, gt_mess[].
    LOOP AT gt_return INTO ge_return.
      ge_mess-message = ge_return-message.
      APPEND ge_mess TO gt_mess.
    ENDLOOP.

    CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
      EXPORTING
        endpos_col   = 80
        endpos_row   = 10
        startpos_col = 1
        startpos_row = 1
        titletext    = 'Erro'
      IMPORTING
        choise       = l_tabix
      TABLES
        valuetab     = gt_mess
      EXCEPTIONS
        break_off    = 1
        OTHERS       = 2.

    g_erro = 'X'.
  ENDIF.

  CHECK g_erro IS INITIAL.

*  IF r_abrir EQ 'X'.
*    g_tipo_func = '1'.
*  ELSE.
*    g_tipo_func = '2'.
*  ENDIF.
*
*  CLEAR ge_zfit007.
*  ge_zfit007-bukrs             = t_bukrs.
*  ge_zfit007-bupla             = t_bupla.
*  ge_zfit007-caixa             = t_caixa.
*  ge_zfit007-bname             = sy-uname.
*  ge_zfit007-tp_funcionalidade = g_tipo_func.
*  ge_zfit007-data              = sy-datum.
*  ge_zfit007-hora              = sy-uzeit.
*
*  MODIFY zfit007 FROM ge_zfit007.
*
*  CLEAR ge_zfit007.
*  CLEAR ge_zfit007.
*  ge_zfit006-bukrs             = t_bukrs.
*  ge_zfit006-bupla             = t_bupla.
*  ge_zfit006-caixa             = t_caixa.
*  ge_zfit006-operacao          = g_operacao.
*  ge_zfit006-bname             = sy-uname.
*  ge_zfit006-nfenum            = ''.
*  ge_zfit006-tp_doc_fiscal     = ''.
*  ge_zfit006-zlsch             = ''.
*  ge_zfit006-valor             = t_valor.
*  ge_zfit006-belnr             = g_belnr.
*  ge_zfit006-gjahr             = g_gjahr.
*  ge_zfit006-data              = sy-datum.
*  ge_zfit006-hora              = sy-uzeit.
*
*  MODIFY zfit006 FROM ge_zfit006.
*
*  PERFORM imprimir_recibo.
*
*  IF r_abrir EQ 'X'.
*    "Caixa & &!
*    MESSAGE s005(zfi001) WITH t_caixa 'Aberto'.
*  ELSE.
*    "Caixa & &!
*    MESSAGE s005(zfi001) WITH t_caixa 'Fechado'.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ACUMULA_VALORES_ADIA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM acumula_valores_adia.

  g_adiantamento = 'X'.

  CHECK NOT t_kunnr IS INITIAL.

  SELECT SINGLE name1 INTO t_name1
    FROM kna1
    WHERE kunnr EQ t_kunnr.

  IF sy-subrc NE 0 .
    "Cliente & não cadastrado!
    CLEAR: t_kunnr, t_name1.
    MESSAGE e014(zfi001) WITH t_kunnr.
  ENDIF.

  IF NOT t_dcr_meio_pagamento IS INITIAL.
    READ TABLE gt_f4_meio_pagto5 INTO ge_f4_meio_pagto5 WITH KEY t_dcr_meio_pagamento.

    IF sy-subrc NE 0.
      CLEAR: ok_code, t_dcr_meio_pagamento.
      "Forma de Pagamento Inválida!
      MESSAGE e013(zfi001).
    ENDIF.
  ENDIF.

  CLEAR ge_formpag.

  IF t_vlr_recebendo GT 0.
    IF t_dcr_meio_pagamento EQ 'CHEQUE'.
      CALL SCREEN '9004' STARTING AT 02 02
                         ENDING AT 63 05.
      IF ok_code IS INITIAL.
        CLEAR t_vlr_recebendo.
      ENDIF.
    ELSEIF t_dcr_meio_pagamento  EQ 'CARTÃO DE CRÉDITO'.
      ADD 1 TO g_ident_cartao.
      ge_formpag-parcelas   = '1'.
      ge_formpag-cod_adq = 'CIELO'.
      CALL SCREEN '9005' STARTING AT 02 02
                         ENDING AT 37 08.
      IF ok_code IS INITIAL.
        CLEAR t_vlr_recebendo.
      ENDIF.
    ELSEIF t_dcr_meio_pagamento  EQ 'CARTÃO DE DÉBITO'.
      ADD 1 TO g_ident_cartao.
      ge_formpag-cod_adq = 'CIELO'.  "GFX - FI - SFSS - DEVK915041 - 01/11/2019
      CALL SCREEN '9009' STARTING AT 02 02
                         ENDING AT 37 08.
      IF ok_code IS INITIAL.
        CLEAR t_vlr_recebendo.
      ENDIF.
    ENDIF.
  ENDIF.

  IF NOT t_dcr_meio_pagamento IS INITIAL.
    IF t_dcr_meio_pagamento  EQ 'CARTÃO DE CRÉDITO' OR
       t_dcr_meio_pagamento  EQ 'CARTÃO DE DÉBITO'.
      ge_formpag-ident_cartao       = g_ident_cartao.
    ENDIF.
    ge_formpag-dcr_meio_pagamento = t_dcr_meio_pagamento.
    ge_formpag-dmbtr              = t_vlr_recebendo.
    IF t_vlr_recebendo GT 0.
      IF t_dcr_meio_pagamento EQ 'DINHEIRO'.
        COLLECT ge_formpag INTO gt_formpag.
      ELSE.
        APPEND ge_formpag TO gt_formpag.
      ENDIF.
    ENDIF.
  ENDIF.

  CLEAR: t_vlr_tot_recebido, t_vlr_tot_restante, t_vlr_troco.
  LOOP AT gt_formpag INTO ge_formpag.
    ADD ge_formpag-dmbtr TO t_vlr_tot_recebido.
  ENDLOOP.

  t_vlr_tot_restante = t_vlr_tot_a_receber - t_vlr_tot_recebido.
  IF t_vlr_tot_recebido GT t_vlr_tot_a_receber.
    t_vlr_troco        = t_vlr_tot_recebido - t_vlr_tot_a_receber.
    t_vlr_tot_restante = 0.
  ENDIF.

  DELETE gt_formpag WHERE dmbtr EQ 0.

  CLEAR: t_dcr_meio_pagamento, t_vlr_recebendo.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DEVOLVER_PAGAMENTO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM devolver_pagamento.

  g_operacao  = 'DEVO'.

  CLEAR: gt_f4_meio_pagto6, gt_f4_meio_pagto6[].
  ge_f4_meio_pagto5-dcr_meio_pagamento = 'DINHEIRO'.
  APPEND ge_f4_meio_pagto6 TO gt_f4_meio_pagto6.
  ge_f4_meio_pagto5-dcr_meio_pagamento = 'CARTÃO DE CRÉDITO'.
  APPEND ge_f4_meio_pagto6 TO gt_f4_meio_pagto6.
  ge_f4_meio_pagto5-dcr_meio_pagamento = 'CARTÃO DE DÉBITO'.
  APPEND ge_f4_meio_pagto6 TO gt_f4_meio_pagto6.

  SORT gt_f4_meio_pagto6 BY dcr_meio_pagamento.

  CLEAR: gt_zfit006, gt_zfit006[].
  SELECT  * INTO TABLE gt_zfit006
    FROM zfit006
    WHERE bukrs    EQ t_bukrs
      AND bupla    EQ t_bupla
      AND caixa    EQ t_caixa
      AND bname    EQ sy-uname
      AND data     EQ sy-datum.

  CLEAR: ge_zfit006.
  READ TABLE gt_zfit006 INTO ge_zfit006 WITH KEY operacao = 'FECH'.

  IF sy-subrc EQ 0.
    CLEAR ok_code.
    "Caixa & fechado para recebimento!
    MESSAGE e009(zfi001) WITH t_caixa.
  ENDIF.

  PERFORM executar_devolucao.
  PERFORM buscar_dados.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DEVOLVER_PAGAMENTO_TOTAL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM devolver_pagamento_total.

  g_operacao  = 'DEVO'.

  CLEAR: gt_f4_meio_pagto6, gt_f4_meio_pagto6[].
  ge_f4_meio_pagto5-dcr_meio_pagamento = 'DINHEIRO'.
  APPEND ge_f4_meio_pagto6 TO gt_f4_meio_pagto6.
  ge_f4_meio_pagto5-dcr_meio_pagamento = 'CARTÃO DE CRÉDITO'.
  APPEND ge_f4_meio_pagto6 TO gt_f4_meio_pagto6.
  ge_f4_meio_pagto5-dcr_meio_pagamento = 'CARTÃO DE DÉBITO'.
  APPEND ge_f4_meio_pagto6 TO gt_f4_meio_pagto6.

  SORT gt_f4_meio_pagto6 BY dcr_meio_pagamento.

  CLEAR: gt_zfit006, gt_zfit006[].
  SELECT  * INTO TABLE gt_zfit006
    FROM zfit006
    WHERE bukrs    EQ t_bukrs
      AND bupla    EQ t_bupla
      AND caixa    EQ t_caixa
      AND bname    EQ sy-uname
      AND data     EQ sy-datum.

  CLEAR: ge_zfit006.
  READ TABLE gt_zfit006 INTO ge_zfit006 WITH KEY operacao = 'FECH'.

  IF sy-subrc EQ 0.
    CLEAR ok_code.
    "Caixa & fechado para recebimento!
    MESSAGE e009(zfi001) WITH t_caixa.
  ENDIF.

  PERFORM executar_devolucao_total.
  PERFORM buscar_dados.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXECUTAR_DEVOLUCAO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM executar_devolucao.

  CLEAR: t_kunnr, t_name1, gt_devol, gt_devol[],
         t_vlr_tot_a_devolver, t_vlr_tot_cred_cliente, t_vlr_tot_restante_dev.

  CALL SCREEN '9010' STARTING AT 02 01
                     ENDING AT 106 26.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXECUTAR_DEVOLUCAO_TOTAL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM executar_devolucao_total.

  CLEAR: t_kunnr, t_name1, gt_devol, gt_devol[],
         t_vlr_tot_a_devolver, t_vlr_tot_cred_cliente, t_vlr_tot_restante_dev.

  CALL SCREEN '9011' STARTING AT 02 01
                     ENDING AT 106 25.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form LANCAR_DEVOLUCAO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM lancar_devolucao.

  IF t_vlr_tot_a_devolver EQ 0.
    "Favor informar o Valor a ser Devolvido!
    MESSAGE e025(zfi001).
  ENDIF.

  CLEAR: g_totdev, g_totcred.
  g_totdev2 = t_vlr_tot_a_devolver.  "Total de Crédito para ressarcimento
  SORT gt_devol BY marc.
  READ TABLE gt_devol INTO ge_devol WITH KEY marc = 'X' BINARY SEARCH.
  IF sy-subrc NE 0.
    MESSAGE e034(zfi001).
  ENDIF.

  LOOP AT gt_devol INTO ge_devol WHERE marc = 'X'.
    IF g_totdev2 > ge_devol-dmbtr2. "Se total de Credito ressarcimento for maior que o valor selecionado
      MESSAGE e055(zfi001) WITH g_totdev2.
    ENDIF.

    ADD ge_devol-dmbtr  TO g_totcred.
    ADD ge_devol-dmbtr2 TO g_totdev.

    ge_devol-marc = 'X'. "APPC - DAM - descomentando - 05/05/2023
    ge_devol-dmbtr3 = ge_devol-dmbtr2.
*** APPC - DAM - alterando a passagem do valor da devolução - inicio 05/05/2023
    IF g_totdev GE t_vlr_tot_a_devolver.
      ge_devol-dmbtr2 = g_totdev2.
      MODIFY gt_devol FROM ge_devol.
      EXIT.
    ELSE.
      MODIFY gt_devol FROM ge_devol.
    ENDIF.

    SUBTRACT ge_devol-dmbtr2 FROM g_totdev2.
*** APPC - DAM - alterando a passagem do valor da devolução - fim 05/05/2023
  ENDLOOP.

  t_vlr_tot_restante_dev = t_vlr_tot_cred_cliente - t_vlr_tot_a_devolver.

  CLEAR g_erro.

  PERFORM lanc_contabil_fb05_devo.

  CHECK g_erro IS INITIAL.

  PERFORM gravar_tabelas_devo.

  CHECK g_erro IS INITIAL.

*  PERFORM imprimir_recibo_devo.

  "Chamando o programa de impressão de comprovante
  SUBMIT zfir008 WITH p_bukrs  EQ t_bukrs
                 WITH p_bupla  EQ t_bupla
                 WITH p_caixa  EQ t_caixa
                 WITH p_valor  EQ t_vlr_tot_a_devolver
                 AND RETURN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form LANCAR_DEVOLUCAO_TOT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM lancar_devolucao_tot.

  IF ok_code EQ 'CANC'.
    CLEAR ge_devol.
*  READ TABLE gt_devol INTO ge_devol WITH KEY marc = 'X'.
    READ TABLE gt_devol INTO ge_devol WITH KEY estornado = 'X'.
  ELSE.
    CLEAR ge_devol.
    READ TABLE gt_devol INTO ge_devol WITH KEY marc = 'X'.
  ENDIF.

  IF sy-subrc NE 0 .
    "Selecionar uma linha!
    MESSAGE e034(zfi001).
  ENDIF.

  g_belnr_dev = ge_devol-belnr.
  g_vlrtotal  = ge_devol-dmbtr.

  CLEAR g_vlrdev.
  LOOP AT gt_devol INTO ge_devol WHERE belnr = g_belnr_dev
                                   AND icon  = '@0V@'.
    ADD ge_devol-dmbtr2 TO g_vlrdev.
  ENDLOOP.

  IF g_vlrdev EQ 0.
    "Nenhum valor a ser devolvido!
    MESSAGE e037(zfi001).
  ENDIF.

  CLEAR g_erro.

  PERFORM lanc_contabil_fb05_devo_tot.

  CHECK g_erro IS INITIAL.

  PERFORM gravar_tabelas_devo_tot.

  CHECK g_erro IS INITIAL.

*  PERFORM imprimir_recibo_devo_tot.

  READ TABLE gt_devol INTO ge_devol WITH KEY belnr          = g_belnr_dev
                                             dcr_meio_pagamento = 'DINHEIRO'.

  LOOP AT gt_devol INTO ge_devol WHERE belnr = g_belnr_dev.
    CASE ge_devol-dcr_meio_pagamento.
      WHEN 'DINHEIRO'.
        "Chamando o programa de impressão de comprovante de dinheiro
        SUBMIT zfir008 WITH p_bukrs  EQ t_bukrs
                       WITH p_bupla  EQ t_bupla
                       WITH p_caixa  EQ t_caixa
                       WITH p_valor  EQ ge_devol-dmbtr2
                       AND RETURN.
      WHEN 'CHEQUE'.
        "Chamando o programa de impressão de comprovante de devolução de cheque
        SUBMIT zfir010 WITH p_bukrs  EQ t_bukrs
                       WITH p_bupla  EQ t_bupla
                       WITH p_caixa  EQ t_caixa
                       WITH p_valor  EQ ge_devol-dmbtr2
                       WITH p_aprch  EQ ge_devol-num_apr_cheque
                       AND RETURN.
    ENDCASE.
  ENDLOOP.

*  "Chamando o programa de impressão de comprovante
  SUBMIT zfir009 WITH p_bukrs  EQ t_bukrs
                 WITH p_bupla  EQ t_bupla
                 WITH p_caixa  EQ t_caixa
                 WITH p_belnr  EQ g_belnr
                 WITH p_kunnr  EQ t_kunnr
                 AND RETURN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FATURAR_REMESSA_DEVO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM faturar_remessa_devo.

  DATA: l_cont1 TYPE i,
        l_cont2 TYPE i,
        l_rfmng TYPE vbfa-rfmng.

  CLEAR g_fatura.
  SELECT SINGLE vbeln INTO g_fatura
    FROM vbfa
    WHERE vbelv   EQ ge_zfie004-vgbel
      AND vbtyp_n EQ 'M'
      AND vbtyp_v EQ 'C'.

  IF NOT g_fatura IS INITIAL.
    CLEAR: gt_mess, gt_mess[].
    ge_mess-message = 'Documento já faturado'.
    APPEND ge_mess TO gt_mess.

    CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
      EXPORTING
        endpos_col   = 80
        endpos_row   = 10
        startpos_col = 1
        startpos_row = 1
        titletext    = 'Erro'
      IMPORTING
        choise       = g_tabix
      TABLES
        valuetab     = gt_mess
      EXCEPTIONS
        break_off    = 1
        OTHERS       = 2.

    g_erro = 'X'.
  ENDIF.

  CHECK g_erro IS INITIAL.

  CLEAR: gt_billingdatain,     gt_billingdatain[],
         gt_return1,           gt_return1[],
         gt_success,           gt_success[],
         gt_errors,            gt_errors[].

  CLEAR: ge_billingdatain,
         ge_return1,
         ge_success,
         gt_errors.

  CLEAR: g_fatura, g_erro.

  ge_billingdatain-ref_doc    = ge_zfie004-vbeln.
  ge_billingdatain-ref_doc_ca = 'J'.
  APPEND ge_billingdatain TO gt_billingdatain.

  CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'
    TABLES
      billingdatain = gt_billingdatain
      return        = gt_return1
      errors        = gt_errors
      success       = gt_success.

  READ TABLE gt_return1 INTO ge_return1 WITH KEY type   = 'E'.

  IF sy-subrc EQ 0.
    g_erro = 'X'.
  ELSE.
    READ TABLE gt_return1 INTO ge_return1 WITH KEY type   = 'I'
                                                   id     = 'VF'
                                                   number = '044'.
    IF sy-subrc EQ 0.
      g_erro = 'X'.
    ENDIF.
  ENDIF.

  IF g_erro = 'X'.
    CLEAR: gt_mess, gt_mess[].
    LOOP AT gt_return1 INTO ge_return1.
      ge_mess-message = ge_return1-message.
      APPEND ge_mess TO gt_mess.
    ENDLOOP.

    CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
      EXPORTING
        endpos_col   = 80
        endpos_row   = 10
        startpos_col = 1
        startpos_row = 1
        titletext    = 'Erro'
      IMPORTING
        choise       = g_tabix
      TABLES
        valuetab     = gt_mess
      EXCEPTIONS
        break_off    = 1
        OTHERS       = 2.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

    CLEAR ge_success.
    READ TABLE gt_success INTO ge_success INDEX 1.

    IF sy-subrc EQ 0.
      g_fatura = ge_success-bill_doc.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form LANC_CONTABIL_FB05_DEVO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM lanc_contabil_fb05_devo.

  DATA: l_num_documento TYPE bseg-belnr,
        l_ano           TYPE bseg-gjahr,
        l_item          TYPE bseg-buzei,
        l_dif           TYPE bsid-dmbtr,
        l_liq           TYPE bsid-dmbtr,
        l_taxa          TYPE bsid-dmbtr.

  CLEAR: gt_blntab,
         gt_ftclear,
         gt_ftpost,
         gt_fttax,
         ge_blntab,
         ge_ftclear,
         ge_ftpost,
         ge_fttax.

  CLEAR g_doc_contabil.

  g_mode = 'N'.
  CALL FUNCTION 'POSTING_INTERFACE_START'
    EXPORTING
      i_function         = 'C'
      i_mode             = g_mode "Especialista
      i_user             = sy-uname
    EXCEPTIONS
      client_incorrect   = 1
      function_invalid   = 2
      group_name_missing = 3
      mode_invalid       = 4
      update_invalid     = 5
      OTHERS             = 6.

  IF sy-subrc NE 0.
    """ Tratar o erro """
  ENDIF.

  SELECT * INTO TABLE gt_zfit003
    FROM zfit003
    WHERE operacao EQ 'ABER'
      AND bupla    EQ t_bupla.

  g_texto            = 'Devolução Caixa'.
  "Montando Cabeçalho
  ge_ftpost-stype = 'K'."Tipo Cabeçalho (K)
  ge_ftpost-count = 1.  "Número da Tela

  "Empresa
  ge_ftpost-fnam = 'BKPF-BUKRS'.
  ge_ftpost-fval = t_bukrs.
  APPEND ge_ftpost TO gt_ftpost.

  "Data do documento
  ge_ftpost-fnam = 'BKPF-BLDAT'.
  CONCATENATE sy-datum+6(2) "Dia
              sy-datum+4(2) "Mês
              sy-datum(4)   "Ano
         INTO ge_ftpost-fval.
  APPEND ge_ftpost TO gt_ftpost.

  "Data do lançamento
  ge_ftpost-fnam = 'BKPF-BUDAT'.
  CONCATENATE sy-datum+6(2) "Dia
              sy-datum+4(2) "Mês
              sy-datum(4)   "Ano
         INTO ge_ftpost-fval.
  APPEND ge_ftpost TO gt_ftpost.

  "Tipo do Documento
  ge_ftpost-fnam = 'BKPF-BLART'.
  ge_ftpost-fval = 'DZ'. "Same type as documents cleared via F-32
  APPEND ge_ftpost TO gt_ftpost.

  "Moeda
  ge_ftpost-fnam = 'BKPF-WAERS'.
  ge_ftpost-fval = 'BRL'.
  APPEND ge_ftpost TO gt_ftpost.

  "Texto do Documento
  ge_ftpost-fnam = 'BKPF-BKTXT'.
  ge_ftpost-fval = g_texto.
  APPEND ge_ftpost TO gt_ftpost.

  CLEAR g_cont.

  "Definindo Contador.
  ADD 1 TO g_cont.

  "Entrando Item 01
  ge_ftpost-stype = 'P'.
  ge_ftpost-count = g_cont.

  "Campo Chave de Lançamento
  ge_ftpost-fnam = 'RF05A-NEWBS'.
  ge_ftpost-fval = '50'.
  APPEND ge_ftpost TO gt_ftpost.

  CLEAR ge_zfit003.
  READ TABLE gt_zfit003 INTO ge_zfit003 WITH KEY natureza_operacao = 'S'.

  "Campo Conta
  ge_ftpost-fnam = 'BSEG-HKONT'.
  ge_ftpost-fval = ge_zfit003-hkont.
  APPEND ge_ftpost TO gt_ftpost.

  "Centro de Lucro
  ge_ftpost-fnam = 'COBL-PRCTR'.
  ge_ftpost-fval = ge_zfit002b-prctr.
  APPEND ge_ftpost TO gt_ftpost.

  "Campo Data Efetiva
  ge_ftpost-fnam = 'BSEG-VALUT'.
  CONCATENATE sy-datum+6(2) "Dia
              sy-datum+4(2) "Mês
              sy-datum(4)   "Ano
         INTO ge_ftpost-fval.
  APPEND ge_ftpost TO gt_ftpost.

  "Campo Montante
  ge_ftpost-fnam = 'BSEG-WRBTR'.
  ge_ftpost-fval = t_vlr_tot_a_devolver.
  "Retira os espaços
  SHIFT ge_ftpost-fval LEFT DELETING LEADING space.
  "Troca o . por ,
  TRANSLATE ge_ftpost-fval USING '.,'.
  APPEND ge_ftpost TO gt_ftpost.

  "Campo Atribuição
  ge_ftpost-fnam = 'BSEG-ZUONR'.
  ge_ftpost-fval = 'ATRIBUICAO'.
  APPEND ge_ftpost TO gt_ftpost.

* Gerar crédito para o cliente
  IF g_totcred GT t_vlr_tot_a_devolver.
    ADD 1 TO g_cont.

    "Entrando de partida de juros **
    ge_ftpost-stype = 'P'.
    ge_ftpost-count =  g_cont.

    "Campo Chave de Lançamento
    ge_ftpost-fnam = 'RF05A-NEWBS'.
    ge_ftpost-fval = '11'.
    APPEND ge_ftpost TO gt_ftpost.

    "Campo Conta
    ge_ftpost-fnam = 'BSEG-HKONT'.
    ge_ftpost-fval = t_kunnr.
    APPEND ge_ftpost TO gt_ftpost.

    "Centro de Lucro
    ge_ftpost-fnam = 'COBL-PRCTR'.
    ge_ftpost-fval = ge_zfit002b-prctr.
    APPEND ge_ftpost TO gt_ftpost.

    "Campo Data Efetiva
    ge_ftpost-fnam = 'BSEG-ZFBDT'.
    CONCATENATE sy-datum+6(2) "Dia
                sy-datum+4(2) "Mês
                sy-datum(4)   "Ano
           INTO ge_ftpost-fval.
    APPEND ge_ftpost TO gt_ftpost.

    "Campo Montante
    ge_ftpost-fnam = 'BSEG-WRBTR'.
    ge_ftpost-fval = g_totcred - t_vlr_tot_a_devolver.
    "Retira os espaços
    SHIFT ge_ftpost-fval LEFT DELETING LEADING space.
    "Troca o . por ,
    TRANSLATE ge_ftpost-fval USING '.,'.
    APPEND ge_ftpost TO gt_ftpost.

    "Campo Atribuição
    ge_ftpost-fnam = 'BSEG-ZUONR'.
    ge_ftpost-fval = 'ATRIBUICAO'.
    APPEND ge_ftpost TO gt_ftpost.
  ENDIF.

  "Processar PA.
  "Documents to be cleared
  ge_ftclear-agkoa = 'D'.              "Account Type
  ge_ftclear-xnops = 'X'.              "Indicator: Select only open items which are not special G/L?
  ge_ftclear-agbuk = t_bukrs.          "Example company code
  ge_ftclear-agkon = t_kunnr.          "Example Customer
  ge_ftclear-selfd = 'BELNR'.          "Selection Field
  ge_ftclear-agums = 'AX'.             "Códigos de razão especial que vai ser selecionado

  LOOP AT gt_devol INTO ge_devol WHERE marc = 'X'.
    l_num_documento    = ge_devol-belnr.
    l_ano              = ge_devol-gjahr.
    l_item             = ge_devol-buzei.

    CONCATENATE l_num_documento l_ano l_item INTO ge_ftclear-selvon.
    CONCATENATE l_num_documento l_ano l_item INTO ge_ftclear-selbis.
    APPEND ge_ftclear TO gt_ftclear.
  ENDLOOP.

  CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
    EXPORTING
      i_auglv                    = g_auglv
      i_tcode                    = g_tcode
      i_sgfunct                  = g_sgfunct
      i_no_auth                  = 'X'
    IMPORTING
      e_msgid                    = g_msgid
      e_msgno                    = g_msgno
      e_msgty                    = g_msgty
      e_msgv1                    = g_msgv1
      e_msgv2                    = g_msgv2
      e_msgv3                    = g_msgv3
      e_msgv4                    = g_msgv4
    TABLES
      t_blntab                   = gt_blntab
      t_ftclear                  = gt_ftclear
      t_ftpost                   = gt_ftpost
      t_fttax                    = gt_fttax
    EXCEPTIONS
      clearing_procedure_invalid = 1
      clearing_procedure_missing = 2
      table_t041a_empty          = 3
      transaction_code_invalid   = 4
      amount_format_error        = 5
      too_many_line_items        = 6
      company_code_invalid       = 7
      screen_not_found           = 8
      no_authorization           = 9
      OTHERS                     = 10.

  "Checa se aconteceu algum erro
  IF g_msgty = 'E'.
    g_msg_no = g_msgno.

    CLEAR g_mensagem.
    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        msg_id                 = g_msgid
        msg_no                 = g_msg_no
        msg_var1               = g_msgv1
        msg_var2               = g_msgv2
        msg_var3               = g_msgv3
        msg_var4               = g_msgv4
      IMPORTING
        msg_text               = g_mensagem
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        OTHERS                 = 3.

    CLEAR: gt_mess, gt_mess[].
    ge_mess-message = g_mensagem.
    APPEND ge_mess TO gt_mess.

    g_erro = 'X'.
  ELSE.
    IF sy-subrc NE 0.

      CASE sy-subrc.
        WHEN 1.
          g_mensagem = 'O procedimento de compensação transferido é inválido'.
        WHEN 2.
          g_mensagem = 'Procedimento de compensação não transferido'.
        WHEN 3.
          g_mensagem = 'Tabela dos procedimentos de compensação (T041A) está vazia'.
        WHEN 4.
          g_mensagem = 'Código de transação transferido não suportado'.
        WHEN 5.
          g_mensagem = 'Erro de formatação com o valor especificado'.
        WHEN 6.
          g_mensagem = 'O cupom contém muitos itens'.
        WHEN 7.
          g_mensagem = 'Empresa Inválida'.
        WHEN 8.
          g_mensagem = 'Próxima tela não encontrada'.
        WHEN 9.
          g_mensagem = 'Sem Autorização'.
        WHEN OTHERS.
          g_mensagem = 'Outro erro'.
      ENDCASE.

      CLEAR: gt_mess, gt_mess[].
      ge_mess-message = g_mensagem.
      APPEND ge_mess TO gt_mess.

      g_erro = 'X'.
    ELSE.
      "OK
      IF g_msgid = 'F5'  AND
         g_msgno = '312' AND
         g_msgty = 'S'.
        g_belnr = g_msgv1.
        g_gjahr = sy-datum(4).
      ELSE.
        g_erro = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

  IF g_erro = 'X'.
    CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
      EXPORTING
        endpos_col   = 80
        endpos_row   = 10
        startpos_col = 1
        startpos_row = 1
        titletext    = 'Erro'
      IMPORTING
        choise       = g_tabix
      TABLES
        valuetab     = gt_mess
      EXCEPTIONS
        break_off    = 1
        OTHERS       = 2.
  ENDIF.

  CALL FUNCTION 'POSTING_INTERFACE_END'
    EXPORTING
      i_bdcimmed              = 'X'
    EXCEPTIONS
      session_not_processable = 1
      OTHERS                  = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form LANC_CONTABIL_FB05_DEVO_TOT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM lanc_contabil_fb05_devo_tot.

  DATA: l_num_documento TYPE bseg-belnr,
        l_ano           TYPE bseg-gjahr,
        l_item          TYPE bseg-buzei,
        l_dif           TYPE bsid-dmbtr,
        l_liq           TYPE bsid-dmbtr,
        l_taxa          TYPE bsid-dmbtr,
        l_selvon        TYPE ftclear-selvon,
        l_selbis        TYPE ftclear-selbis.

  CLEAR: gt_blntab,
         gt_ftclear,
         gt_ftpost,
         gt_fttax,
         ge_blntab,
         ge_ftclear,
         ge_ftpost,
         ge_fttax.

  CLEAR g_doc_contabil.

  g_mode = 'N'.
  CALL FUNCTION 'POSTING_INTERFACE_START'
    EXPORTING
      i_function         = 'C'
      i_mode             = g_mode "Especialista
      i_user             = sy-uname
    EXCEPTIONS
      client_incorrect   = 1
      function_invalid   = 2
      group_name_missing = 3
      mode_invalid       = 4
      update_invalid     = 5
      OTHERS             = 6.

  IF sy-subrc NE 0.
    """ Tratar o erro """
  ENDIF.

  SELECT * INTO TABLE gt_zfit003
    FROM zfit003
    WHERE operacao EQ 'ABER'
      AND bupla    EQ t_bupla.

  "Busca conta do razão para nova conciliação
  SELECT SINGLE operacao
              , bupla
              , hkont
    FROM zfit003
    WHERE operacao EQ 'CONC'
      AND bupla    EQ @t_bupla
    INTO @DATA(ls_conta_razao).

  g_texto            = 'Devolução Total'.
  "Montando Cabeçalho
  ge_ftpost-stype = 'K'."Tipo Cabeçalho (K)
  ge_ftpost-count = 1.  "Número da Tela

  "Empresa
  ge_ftpost-fnam = 'BKPF-BUKRS'.
  ge_ftpost-fval = t_bukrs.
  APPEND ge_ftpost TO gt_ftpost.

  "Data do documento
  ge_ftpost-fnam = 'BKPF-BLDAT'.
  CONCATENATE sy-datum+6(2) "Dia
              sy-datum+4(2) "Mês
              sy-datum(4)   "Ano
         INTO ge_ftpost-fval.
  APPEND ge_ftpost TO gt_ftpost.

  "Data do lançamento
  ge_ftpost-fnam = 'BKPF-BUDAT'.
  CONCATENATE sy-datum+6(2) "Dia
              sy-datum+4(2) "Mês
              sy-datum(4)   "Ano
         INTO ge_ftpost-fval.
  APPEND ge_ftpost TO gt_ftpost.

  "Tipo do Documento
  ge_ftpost-fnam = 'BKPF-BLART'.
  ge_ftpost-fval = 'DZ'. "Same type as documents cleared via F-32
  APPEND ge_ftpost TO gt_ftpost.

  "Moeda
  ge_ftpost-fnam = 'BKPF-WAERS'.
  ge_ftpost-fval = 'BRL'.
  APPEND ge_ftpost TO gt_ftpost.

  "Texto do Documento
  ge_ftpost-fnam = 'BKPF-BKTXT'.
  ge_ftpost-fval = g_texto.
  APPEND ge_ftpost TO gt_ftpost.


  CLEAR g_cont.

  LOOP AT gt_devol INTO ge_devol WHERE belnr = g_belnr_dev
                                   AND icon  = '@0V@'.

    "Definindo Contador.
    ADD 1 TO g_cont.

    "Entrando Item 01
    ge_ftpost-stype = 'P'.
    ge_ftpost-count = g_cont.

    IF ge_devol-dcr_meio_pagamento EQ 'CHEQUE'.
      "Campo Conta
      ge_ftpost-fnam = 'BSEG-HKONT'.
      ge_ftpost-fval = t_kunnr.
      APPEND ge_ftpost TO gt_ftpost.

      "Campo Conta
      ge_ftpost-fnam = 'RF05A-NEWUM'.
      ge_ftpost-fval = 'X'.
      APPEND ge_ftpost TO gt_ftpost.

* UMSKZ

      "Campo Chave de Lançamento
      ge_ftpost-fnam = 'RF05A-NEWBS'.
      ge_ftpost-fval = '19'.
      APPEND ge_ftpost TO gt_ftpost.

      "Centro de Lucro
      ge_ftpost-fnam = 'COBL-PRCTR'.
      ge_ftpost-fval = ge_zfit002b-prctr.
      APPEND ge_ftpost TO gt_ftpost.

      "Campo Data Efetiva
      ge_ftpost-fnam = 'BSEG-ZFBDT'.
      CONCATENATE sy-datum+6(2) "Dia
                  sy-datum+4(2) "Mês
                  sy-datum(4)   "Ano
             INTO ge_ftpost-fval.
      APPEND ge_ftpost TO gt_ftpost.

      "Campo Montante
      ge_ftpost-fnam = 'BSEG-WRBTR'.
      ge_ftpost-fval = ge_devol-dmbtr2.
      "Retira os espaços
      SHIFT ge_ftpost-fval LEFT DELETING LEADING space.
      "Troca o . por ,
      TRANSLATE ge_ftpost-fval USING '.,'.
      APPEND ge_ftpost TO gt_ftpost.

      "Campo Atribuição
      ge_ftpost-fnam = 'BSEG-ZUONR'.
      ge_ftpost-fval = 'CHEQUE'.
      APPEND ge_ftpost TO gt_ftpost.
    ELSEIF ge_devol-dcr_meio_pagamento EQ 'CARTÃO DE CRÉDITO'
        OR ge_devol-dcr_meio_pagamento EQ 'CARTÃO DE DÉBITO'.

      "Campo Chave de Lançamento
      ge_ftpost-fnam = 'RF05A-NEWBS'.
*** GFX - JBRS - Início - 19/07/2021
*      ge_ftpost-fval = '11'.
      ge_ftpost-fval = COND #( WHEN ls_conta_razao-hkont IS NOT INITIAL
                                THEN '50'
                                ELSE '11' ).
*** GFX - JBRS - Fim - 19/07/2021
      APPEND ge_ftpost TO gt_ftpost.

      "Campo Conta
      ge_ftpost-fnam = 'BSEG-HKONT'.
*** GFX - JBRS - Início - 19/07/2021
*      ge_ftpost-fval = ge_devol-cliente.
      ge_ftpost-fval = COND #( WHEN ls_conta_razao-hkont IS NOT INITIAL
                                THEN ls_conta_razao-hkont
                                ELSE ge_devol-cliente ).

*** GFX - JBRS - Fim - 19/07/2021

      APPEND ge_ftpost TO gt_ftpost.

      "Centro de Lucro
      ge_ftpost-fnam = 'COBL-PRCTR'.
      ge_ftpost-fval = ge_zfit002b-prctr.
      APPEND ge_ftpost TO gt_ftpost.

*** GFX - JBRS - Inicio - 19/07/2021
      IF ls_conta_razao-hkont IS INITIAL.
        "Campo Data Efetiva
        ge_ftpost-fnam = 'BSEG-ZFBDT'.
        CONCATENATE ge_zfie011-data_parcela+6(2) "Dia
                    ge_zfie011-data_parcela+4(2) "Mês
                    ge_zfie011-data_parcela(4)   "Ano
               INTO ge_ftpost-fval.
        APPEND ge_ftpost TO gt_ftpost.
      ENDIF.
*** GFX - JBRS - Fim - 19/07/2021

      "Campo Montante
      ge_ftpost-fnam = 'BSEG-WRBTR'.
      ge_ftpost-fval = ge_devol-dmbtr2.
      "Retira os espaços
      SHIFT ge_ftpost-fval LEFT DELETING LEADING space.
      "Troca o . por ,
      TRANSLATE ge_ftpost-fval USING '.,'.
      APPEND ge_ftpost TO gt_ftpost.

      "Campo Atribuição
      ge_ftpost-fnam = 'BSEG-ZUONR'.
      ge_ftpost-fval = 'CARTAO'.
      APPEND ge_ftpost TO gt_ftpost.

*      "Campo Fatura Relacionada
*      ge_ftpost-fnam = 'BSEG-REBZG'. "REBZJ "REBZZ
*      ge_ftpost-fval = ge_devol-vbeln_vf_ori.
*      APPEND ge_ftpost TO gt_ftpost.
*
*      ge_ftpost-fnam = 'BSEG-REBZJ'. "REBZJ "REBZZ
*      ge_ftpost-fval = ge_devol-gjahr.
*      APPEND ge_ftpost TO gt_ftpost.
    ELSEIF ge_devol-dcr_meio_pagamento EQ 'DINHEIRO'.
      "Campo Chave de Lançamento
      ge_ftpost-fnam = 'RF05A-NEWBS'.
      ge_ftpost-fval = '50'.
      APPEND ge_ftpost TO gt_ftpost.

      CLEAR ge_zfit003.
      READ TABLE gt_zfit003 INTO ge_zfit003 WITH KEY natureza_operacao = 'S'.

      "Campo Conta
      ge_ftpost-fnam = 'BSEG-HKONT'.
      ge_ftpost-fval = ge_zfit003-hkont.
      APPEND ge_ftpost TO gt_ftpost.

      "Centro de Lucro
      ge_ftpost-fnam = 'COBL-PRCTR'.
      ge_ftpost-fval = ge_zfit002b-prctr.
      APPEND ge_ftpost TO gt_ftpost.

      "Campo Data Efetiva
      ge_ftpost-fnam = 'BSEG-VALUT'.
      CONCATENATE sy-datum+6(2) "Dia
                  sy-datum+4(2) "Mês
                  sy-datum(4)   "Ano
             INTO ge_ftpost-fval.
      APPEND ge_ftpost TO gt_ftpost.

      "Campo Montante
      ge_ftpost-fnam = 'BSEG-WRBTR'.
      ge_ftpost-fval = ge_devol-dmbtr2.
      "Retira os espaços
      SHIFT ge_ftpost-fval LEFT DELETING LEADING space.
      "Troca o . por ,
      TRANSLATE ge_ftpost-fval USING '.,'.
      APPEND ge_ftpost TO gt_ftpost.

      "Campo Atribuição
      ge_ftpost-fnam = 'BSEG-ZUONR'.
      ge_ftpost-fval = 'DINHEIRO'.
      APPEND ge_ftpost TO gt_ftpost.
    ENDIF.
  ENDLOOP.

* Gerar crédito para o cliente
  IF g_vlrtotal GT g_vlrdev.
    ADD 1 TO g_cont.

    "Entrando de partida de juros **
    ge_ftpost-stype = 'P'.
    ge_ftpost-count =  g_cont.

    "Campo Chave de Lançamento
    ge_ftpost-fnam = 'RF05A-NEWBS'.
    ge_ftpost-fval = '11'.
    APPEND ge_ftpost TO gt_ftpost.

    "Campo Conta
    ge_ftpost-fnam = 'BSEG-HKONT'.
    ge_ftpost-fval = t_kunnr.
    APPEND ge_ftpost TO gt_ftpost.

    "Centro de Lucro
    ge_ftpost-fnam = 'COBL-PRCTR'.
    ge_ftpost-fval = ge_zfit002b-prctr.
    APPEND ge_ftpost TO gt_ftpost.

    "Campo Data Efetiva
    ge_ftpost-fnam = 'BSEG-ZFBDT'.
    CONCATENATE sy-datum+6(2) "Dia
                sy-datum+4(2) "Mês
                sy-datum(4)   "Ano
           INTO ge_ftpost-fval.
    APPEND ge_ftpost TO gt_ftpost.

    "Campo Montante
    ge_ftpost-fnam = 'BSEG-WRBTR'.
    ge_ftpost-fval = g_vlrtotal - g_vlrdev.
    "Retira os espaços
    SHIFT ge_ftpost-fval LEFT DELETING LEADING space.
    "Troca o . por ,
    TRANSLATE ge_ftpost-fval USING '.,'.
    APPEND ge_ftpost TO gt_ftpost.

    "Campo Atribuição
    ge_ftpost-fnam = 'BSEG-ZUONR'.
    ge_ftpost-fval = 'ATRIBUICAO'.
    APPEND ge_ftpost TO gt_ftpost.
  ENDIF.

  "Processar PA.
  "Documents to be cleared
  ge_ftclear-agkoa = 'D'.              "Account Type
  ge_ftclear-xnops = 'X'.              "Indicator: Select only open items which are not special G/L?
  ge_ftclear-agbuk = t_bukrs.          "Example company code
  ge_ftclear-agkon = t_kunnr.          "Example Customer
  ge_ftclear-selfd = 'BELNR'.          "Selection Field
  ge_ftclear-agums = 'AX'.             "Códigos de razão especial que vai ser selecionado

  LOOP AT gt_devol INTO ge_devol WHERE belnr = g_belnr_dev
                                   AND icon  = '@0V@'.
    l_num_documento    = ge_devol-belnr.
    l_ano              = ge_devol-gjahr.
    l_item             = ge_devol-buzei.

    CONCATENATE l_num_documento l_ano l_item INTO l_selvon.
    CONCATENATE l_num_documento l_ano l_item INTO l_selbis.

    READ TABLE gt_ftclear INTO ge_ftclear WITH KEY selvon = l_selvon
                                                   selbis = l_selbis.

    IF sy-subrc NE 0.
      CONCATENATE l_num_documento l_ano l_item INTO ge_ftclear-selvon.
      CONCATENATE l_num_documento l_ano l_item INTO ge_ftclear-selbis.
      APPEND ge_ftclear TO gt_ftclear.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
    EXPORTING
      i_auglv                    = g_auglv
      i_tcode                    = g_tcode
      i_sgfunct                  = g_sgfunct
      i_no_auth                  = 'X'
    IMPORTING
      e_msgid                    = g_msgid
      e_msgno                    = g_msgno
      e_msgty                    = g_msgty
      e_msgv1                    = g_msgv1
      e_msgv2                    = g_msgv2
      e_msgv3                    = g_msgv3
      e_msgv4                    = g_msgv4
    TABLES
      t_blntab                   = gt_blntab
      t_ftclear                  = gt_ftclear
      t_ftpost                   = gt_ftpost
      t_fttax                    = gt_fttax
    EXCEPTIONS
      clearing_procedure_invalid = 1
      clearing_procedure_missing = 2
      table_t041a_empty          = 3
      transaction_code_invalid   = 4
      amount_format_error        = 5
      too_many_line_items        = 6
      company_code_invalid       = 7
      screen_not_found           = 8
      no_authorization           = 9
      OTHERS                     = 10.

  "Checa se aconteceu algum erro
  IF g_msgty = 'E'.
    g_msg_no = g_msgno.

    CLEAR g_mensagem.
    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        msg_id                 = g_msgid
        msg_no                 = g_msg_no
        msg_var1               = g_msgv1
        msg_var2               = g_msgv2
        msg_var3               = g_msgv3
        msg_var4               = g_msgv4
      IMPORTING
        msg_text               = g_mensagem
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        OTHERS                 = 3.

    CLEAR: gt_mess, gt_mess[].
    ge_mess-message = g_mensagem.
    APPEND ge_mess TO gt_mess.

    g_erro = 'X'.
  ELSE.
    IF sy-subrc NE 0.

      CASE sy-subrc.
        WHEN 1.
          g_mensagem = 'O procedimento de compensação transferido é inválido'.
        WHEN 2.
          g_mensagem = 'Procedimento de compensação não transferido'.
        WHEN 3.
          g_mensagem = 'Tabela dos procedimentos de compensação (T041A) está vazia'.
        WHEN 4.
          g_mensagem = 'Código de transação transferido não suportado'.
        WHEN 5.
          g_mensagem = 'Erro de formatação com o valor especificado'.
        WHEN 6.
          g_mensagem = 'O cupom contém muitos itens'.
        WHEN 7.
          g_mensagem = 'Empresa Inválida'.
        WHEN 8.
          g_mensagem = 'Próxima tela não encontrada'.
        WHEN 9.
          g_mensagem = 'Sem Autorização'.
        WHEN OTHERS.
          g_mensagem = 'Outro erro'.
      ENDCASE.

      CLEAR: gt_mess, gt_mess[].
      ge_mess-message = g_mensagem.
      APPEND ge_mess TO gt_mess.

      g_erro = 'X'.
    ELSE.
      "OK
      IF g_msgid = 'F5'  AND
         g_msgno = '312' AND
         g_msgty = 'S'.
        g_belnr = g_msgv1.
        g_gjahr = sy-datum(4).
      ELSE.
        g_erro = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

  IF g_erro = 'X'.
    CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
      EXPORTING
        endpos_col   = 80
        endpos_row   = 10
        startpos_col = 1
        startpos_row = 1
        titletext    = 'Erro'
      IMPORTING
        choise       = g_tabix
      TABLES
        valuetab     = gt_mess
      EXCEPTIONS
        break_off    = 1
        OTHERS       = 2.
  ENDIF.

  CALL FUNCTION 'POSTING_INTERFACE_END'
    EXPORTING
      i_bdcimmed              = 'X'
    EXCEPTIONS
      session_not_processable = 1
      OTHERS                  = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ACUMULA_VALORES_DEVO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM acumula_valores_devo.

  IF t_kunnr IS INITIAL.
    CLEAR: t_vlr_tot_a_devolver, t_vlr_tot_cred_cliente, t_vlr_tot_restante_dev,
           t_name1, gt_devol.
  ENDIF.

  IF t_vlr_tot_a_devolver GT t_vlr_tot_cred_cliente.
    CLEAR t_vlr_tot_a_devolver.
    "Valor a ser Recebido não pode ser maior do que o Valor Pago em Dinheiro!
    MESSAGE e024(zfi001).
  ENDIF.

  t_vlr_tot_restante_dev = t_vlr_tot_cred_cliente - t_vlr_tot_a_devolver.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form IMPRIMIR_RECIBO_ADIA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM imprimir_recibo_adia .

  DATA: formname              TYPE tdsfname,
        fm_name               TYPE rs38l_fnam,
        wa_control_parameters TYPE ssfctrlop,         "Estrutura de controle (smartforms)
        wa_output_options     TYPE ssfcompop.         "Estrutura de controle (smartforms)

  DATA: gc_ucomm LIKE sy-ucomm.

  DATA: l_visual,
        l_name_text      TYPE adrp-name_text,
        l_campoaux1(132) TYPE c,
        l_campoaux2(132) TYPE c,
        l_campoval(25)   TYPE c,
        l_campocnpj(18)  TYPE c,
        l_campocnpj2(18) TYPE c,
        l_campocpf(14)   TYPE c,
        l_campo(10)      TYPE c,
        l_tipo(15)       TYPE c.

  CLEAR ge_zfie007.

  CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
    EXPORTING
      branch            = t_bupla
      bukrs             = t_bukrs
    IMPORTING
      address           = ge_address
      branch_data       = ge_branch_data
      cgc_number        = g_cgc_number2
      address1          = ge_address1
    EXCEPTIONS
      branch_not_found  = 1
      address_not_found = 2
      company_not_found = 3
      OTHERS            = 4.

  SELECT SINGLE * INTO ge_kna1
    FROM kna1
    WHERE kunnr EQ t_kunnr.

  IF NOT ge_kna1-stcd1 IS INITIAL.
    l_campo = 'CNPJ :'.
    g_cgc_number = ge_kna1-stcd1.
    WRITE g_cgc_number TO l_campocnpj.
  ELSE.              "CPF
    l_campo = 'CPF  :'.
    g_cpf_number = ge_kna1-stcd2.
    WRITE g_cpf_number TO l_campocpf.
    l_campocnpj = l_campocpf.
  ENDIF.

  CLEAR l_name_text.
*  SELECT SINGLE name_text INTO l_name_text
*    FROM usr21
*    INNER JOIN adrp ON
*    usr21~persnumber = adrp~persnumber
*    WHERE bname EQ sy-uname.
  l_name_text = sy-uname.

  "Nome da Filial
  CONCATENATE ge_address-name1 '-' ge_address-sortl
    INTO g_linha01 SEPARATED BY space.

  "Endereço da Filial
  g_linha02 =  ge_address-stras.

  CONCATENATE ge_address-ort02 '-' ge_address-ort01 '-' ge_address-regio
    INTO g_linha03 SEPARATED BY space.

  ge_zfie007-filial           = g_linha01.
  ge_zfie007-end1_filial      = g_linha02.
  ge_zfie007-end2_filial      = g_linha03.

  CLEAR: gt_dados, gt_dados[].
  WRITE '                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '            RECIBO ADIANTAMENTO            ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE sy-datum TO l_campoaux1.
  WRITE sy-uzeit TO l_campoaux2.
  CONCATENATE l_campoaux1 l_campoaux2
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE l_campo l_campocnpj
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'NOME :' t_name1
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'OPER :' l_name_text
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  l_tipo = 'ADIANTAMENTO'.
  CONCATENATE 'TIPO :' l_tipo
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '              MEIOS PAGAMENTO              ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CLEAR: g_totcred.
  LOOP AT gt_formpag INTO ge_formpag.
    WRITE ge_formpag-dmbtr TO l_campoval.
    g_linha = ge_formpag-dcr_meio_pagamento.
    g_linha+19(25) = l_campoval.
    WRITE g_linha TO ge_dados-tdline.
    APPEND ge_dados TO gt_dados.
    ADD ge_formpag-dmbtr TO g_totcred.
  ENDLOOP.
  WRITE '                          -----------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE g_totcred TO l_campoval.
  g_linha = 'TOTAL:'.
  g_linha+19(25) = l_campoval.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
*  WRITE sy-datum TO l_campoaux1.
*  WRITE sy-uzeit TO l_campoaux2.
*  CONCATENATE l_campoaux1 l_campoaux2
*    INTO g_linha SEPARATED BY space.
*  WRITE g_linha TO ge_dados-tdline.
*  APPEND ge_dados TO gt_dados.
*  WRITE '-------------------------------------------' TO ge_dados-tdline.
*  APPEND ge_dados TO gt_dados.

  l_visual = ' '.

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
      wa_output_options-tdimmed       = 'X'. "nast-dimme.
      wa_output_options-tddelete      = space.
      wa_output_options-tdnewid       = 'X'.
      wa_output_options-tdnoprev      = 'X'.
      wa_control_parameters-langu     = 'PT'.
      wa_control_parameters-no_dialog = 'X'.
      wa_control_parameters-preview   = ''.
  ENDCASE.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZFI_RECIBO_COM_LOGO'
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
      ge_zfie007         = ge_zfie007
    TABLES
      gt_dados           = gt_dados
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form IMPRIMIR_RECIBO_DEVO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM imprimir_recibo_devo.

  DATA: formname              TYPE tdsfname,
        fm_name               TYPE rs38l_fnam,
        wa_control_parameters TYPE ssfctrlop,         "Estrutura de controle (smartforms)
        wa_output_options     TYPE ssfcompop.         "Estrutura de controle (smartforms)

  DATA: gc_ucomm LIKE sy-ucomm.

  DATA: l_visual,
        l_name_text       TYPE adrp-name_text,
        l_lenght          TYPE i,
        l_val2dec         TYPE j_1bnflin-netwr,
        l_prbruto         TYPE j_1bnflin-netwr,
        l_prunita         TYPE j_1bnflin-netwr,
        l_qtdabs          TYPE ladlg,
        l_campoaux1(132)  TYPE c,
        l_campoaux2(132)  TYPE c,
        l_campoval(25)    TYPE c,
        l_campocpf(14)    TYPE c,
        l_campocnpj(18)   TYPE c,
        l_campocnpj2(18)  TYPE c,
        l_campovlrit(50)  TYPE c,
        l_campoperde(6)   TYPE c,
        l_campomenge(50)  TYPE c,
        l_campomeng7(7)   TYPE c,
        l_campomeins(50)  TYPE c,
        l_camponetpr(50)  TYPE c,
        l_campobruto(10)  TYPE c,
        l_campounita(10)  TYPE c,
        l_campototal(10)  TYPE c,
        l_campovalor(10)  TYPE c,
        l_campotaxval(50) TYPE c,
        l_camponetwr(50)  TYPE c,
        l_campo1(50)      TYPE c,
        l_campo2(50)      TYPE c.

  CLEAR ge_zfie007.

  CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
    EXPORTING
      branch            = t_bupla
      bukrs             = t_bukrs
    IMPORTING
      address           = ge_address
      branch_data       = ge_branch_data
      cgc_number        = g_cgc_number2
      address1          = ge_address1
    EXCEPTIONS
      branch_not_found  = 1
      address_not_found = 2
      company_not_found = 3
      OTHERS            = 4.

  CLEAR l_name_text.
*  SELECT SINGLE name_text INTO l_name_text
*    FROM usr21
*    INNER JOIN adrp ON
*    usr21~persnumber = adrp~persnumber
*    WHERE bname EQ sy-uname.
  l_name_text = sy-uname.

  CLEAR ge_kna1.
  SELECT SINGLE * INTO ge_kna1
    FROM kna1
    WHERE kunnr EQ t_kunnr.

  IF NOT ge_kna1-stcd1 IS INITIAL. "CNPJ
    g_cpf_cnpj_txt = 'CNPJ:'.
    g_cgc_number = ge_kna1-stcd1.
    WRITE g_cgc_number TO l_campocnpj.
  ELSE.              "CPF
    g_cpf_cnpj_txt = 'CPF:'.
    g_cpf_number = ge_kna1-stcd2.
    WRITE g_cpf_number TO l_campocpf.
    l_campocnpj = l_campocpf.
  ENDIF.

  "Nome da Filial
  CONCATENATE ge_address-name1 '-' ge_address-sortl
    INTO g_linha01 SEPARATED BY space.

  "Endereço da Filial
  g_linha02 =  ge_address-stras.

  CONCATENATE ge_address-ort02 '-' ge_address-ort01 '-' ge_address-regio
    INTO g_linha03 SEPARATED BY space.

  ge_zfie007-filial           = g_linha01.
  ge_zfie007-end1_filial      = g_linha02.
  ge_zfie007-end2_filial      = g_linha03.

  CLEAR: gt_dados, gt_dados[].
  CONCATENATE ge_address-name1 '-' ge_address-sortl
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE ge_address-stras TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE ge_address-ort02 '-' ge_address-ort01 '-' ge_address-regio
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE g_cgc_number2 TO l_campocnpj.
  CONCATENATE 'CNPJ:' l_campocnpj
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'IE:' ge_branch_data-state_insc
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-----------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '               COMPROVANTE DE RESSARCIMENTO                ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE sy-datum TO l_campoaux1.
  WRITE sy-uzeit TO l_campoaux2.
  CONCATENATE l_campoaux1 l_campoaux2
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-----------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE sy-datum TO l_campoaux1.
  CONCATENATE 'DATA:' l_campoaux1
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'CAIXA:' t_caixa
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'OPERADOR:' l_name_text
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE g_cpf_cnpj_txt l_campocnpj
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'CLIENTE:' ge_kna1-name1
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE t_vlr_tot_a_devolver TO l_campovalor.
  CONCATENATE 'VALOR DO RESSARCIMENTO:' l_campovalor
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-----------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE 'Ass Cliente: ______________________________________________' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-----------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE sy-datum TO l_campoaux1.
  WRITE sy-uzeit TO l_campoaux2.
  CONCATENATE l_campoaux1 l_campoaux2
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-----------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.

  l_visual = ' '.

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
      wa_output_options-tdimmed       = 'X'. "nast-dimme.
      wa_output_options-tddelete      = space.
      wa_output_options-tdnewid       = 'X'.
      wa_output_options-tdnoprev      = 'X'.
      wa_control_parameters-langu     = 'PT'.
      wa_control_parameters-no_dialog = 'X'.
      wa_control_parameters-preview   = ' '.
  ENDCASE.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZFI_RECIBO_COM_LOGO'
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
      ge_zfie007         = ge_zfie007
    TABLES
      gt_dados           = gt_dados
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form IMPRIMIR_RECIBO_DEVO_TOT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM imprimir_recibo_devo_tot.

  DATA: formname              TYPE tdsfname,
        fm_name               TYPE rs38l_fnam,
        wa_control_parameters TYPE ssfctrlop,         "Estrutura de controle (smartforms)
        wa_output_options     TYPE ssfcompop.         "Estrutura de controle (smartforms)

  DATA: gc_ucomm LIKE sy-ucomm.

  DATA: l_visual,
        l_name_text       TYPE adrp-name_text,
        l_lenght          TYPE i,
        l_val2dec         TYPE j_1bnflin-netwr,
        l_prbruto         TYPE j_1bnflin-netwr,
        l_prunita         TYPE j_1bnflin-netwr,
        l_qtdabs          TYPE ladlg,
        l_campoaux1(132)  TYPE c,
        l_campoaux2(132)  TYPE c,
        l_campoval(25)    TYPE c,
        l_campocpf(14)    TYPE c,
        l_campocnpj(18)   TYPE c,
        l_campocnpj2(18)  TYPE c,
        l_campovlrit(50)  TYPE c,
        l_campoperde(6)   TYPE c,
        l_campomenge(50)  TYPE c,
        l_campomeng7(7)   TYPE c,
        l_campomeins(50)  TYPE c,
        l_camponetpr(50)  TYPE c,
        l_campobruto(10)  TYPE c,
        l_campounita(10)  TYPE c,
        l_campototal(10)  TYPE c,
        l_campovalor(10)  TYPE c,
        l_campotaxval(50) TYPE c,
        l_camponetwr(50)  TYPE c,
        l_campo1(50)      TYPE c,
        l_campo2(50)      TYPE c.

  CLEAR ge_zfie007.

  CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
    EXPORTING
      branch            = t_bupla
      bukrs             = t_bukrs
    IMPORTING
      address           = ge_address
      branch_data       = ge_branch_data
      cgc_number        = g_cgc_number2
      address1          = ge_address1
    EXCEPTIONS
      branch_not_found  = 1
      address_not_found = 2
      company_not_found = 3
      OTHERS            = 4.

  CLEAR l_name_text.
*  SELECT SINGLE name_text INTO l_name_text
*    FROM usr21
*    INNER JOIN adrp ON
*    usr21~persnumber = adrp~persnumber
*    WHERE bname EQ sy-uname.
  l_name_text = sy-uname.

  CLEAR ge_kna1.
  SELECT SINGLE * INTO ge_kna1
    FROM kna1
    WHERE kunnr EQ t_kunnr.

  IF NOT ge_kna1-stcd1 IS INITIAL. "CNPJ
    g_cpf_cnpj_txt = 'CNPJ:'.
    g_cgc_number = ge_kna1-stcd1.
    WRITE g_cgc_number TO l_campocnpj.
  ELSE.              "CPF
    g_cpf_cnpj_txt = 'CPF:'.
    g_cpf_number = ge_kna1-stcd2.
    WRITE g_cpf_number TO l_campocpf.
    l_campocnpj = l_campocpf.
  ENDIF.

  "Nome da Filial
  CONCATENATE ge_address-name1 '-' ge_address-sortl
    INTO g_linha01 SEPARATED BY space.

  "Endereço da Filial
  g_linha02 =  ge_address-stras.

  CONCATENATE ge_address-ort02 '-' ge_address-ort01 '-' ge_address-regio
    INTO g_linha03 SEPARATED BY space.

  ge_zfie007-filial           = g_linha01.
  ge_zfie007-end1_filial      = g_linha02.
  ge_zfie007-end2_filial      = g_linha03.

  CLEAR: gt_dados, gt_dados[].
  CONCATENATE ge_address-name1 '-' ge_address-sortl
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE ge_address-stras TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE ge_address-ort02 '-' ge_address-ort01 '-' ge_address-regio
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE g_cgc_number2 TO l_campocnpj.
  CONCATENATE 'CNPJ:' l_campocnpj
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'IE:' ge_branch_data-state_insc
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-----------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '               COMPROVANTE DE RESSARCIMENTO                ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE sy-datum TO l_campoaux1.
  WRITE sy-uzeit TO l_campoaux2.
  CONCATENATE l_campoaux1 l_campoaux2
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-----------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE sy-datum TO l_campoaux1.
  CONCATENATE 'DATA:' l_campoaux1
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'CAIXA:' t_caixa
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'OPERADOR:' l_name_text
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE g_cpf_cnpj_txt l_campocnpj
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'CLIENTE:' ge_kna1-name1
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE t_vlr_tot_a_devolver TO l_campovalor.
  CONCATENATE 'VALOR DO RESSARCIMENTO:' l_campovalor
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-----------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE 'Ass Cliente: ______________________________________________' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-----------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE sy-datum TO l_campoaux1.
  WRITE sy-uzeit TO l_campoaux2.
  CONCATENATE l_campoaux1 l_campoaux2
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-----------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.

  l_visual = ' '.

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
      wa_output_options-tdimmed       = 'X'. "nast-dimme.
      wa_output_options-tddelete      = space.
      wa_output_options-tdnewid       = 'X'.
      wa_output_options-tdnoprev      = 'X'.
      wa_control_parameters-langu     = 'PT'.
      wa_control_parameters-no_dialog = 'X'.
      wa_control_parameters-preview   = ' '.
  ENDCASE.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZFI_RECIBO_COM_LOGO'
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
      ge_zfie007         = ge_zfie007
    TABLES
      gt_dados           = gt_dados
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form IMPRIMIR_RECIBO_DEVO2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM imprimir_recibo_devo2.

  DATA: formname              TYPE tdsfname,
        fm_name               TYPE rs38l_fnam,
        wa_control_parameters TYPE ssfctrlop,         "Estrutura de controle (smartforms)
        wa_output_options     TYPE ssfcompop.         "Estrutura de controle (smartforms)

  DATA: gc_ucomm LIKE sy-ucomm.

  DATA: l_visual,
        l_name_text       TYPE adrp-name_text,
        l_lenght          TYPE i,
        l_val2dec         TYPE j_1bnflin-netwr,
        l_prbruto         TYPE j_1bnflin-netwr,
        l_prunita         TYPE j_1bnflin-netwr,
        l_qtdabs          TYPE ladlg,
        l_campoaux1(132)  TYPE c,
        l_campoaux2(132)  TYPE c,
        l_campoval(25)    TYPE c,
        l_campocpf(14)    TYPE c,
        l_campocnpj(18)   TYPE c,
        l_campocnpj2(18)  TYPE c,
        l_campovlrit(50)  TYPE c,
        l_campoperde(6)   TYPE c,
        l_campomenge(50)  TYPE c,
        l_campomeng7(7)   TYPE c,
        l_campomeins(50)  TYPE c,
        l_camponetpr(50)  TYPE c,
        l_campobruto(10)  TYPE c,
        l_campounita(10)  TYPE c,
        l_campototal(10)  TYPE c,
        l_campovalor(10)  TYPE c,
        l_campotaxval(50) TYPE c,
        l_camponetwr(50)  TYPE c,
        l_campo1(50)      TYPE c,
        l_campo2(50)      TYPE c.

  CLEAR ge_zfie007.

  CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
    EXPORTING
      branch            = t_bupla
      bukrs             = t_bukrs
    IMPORTING
      address           = ge_address
      branch_data       = ge_branch_data
      cgc_number        = g_cgc_number2
      address1          = ge_address1
    EXCEPTIONS
      branch_not_found  = 1
      address_not_found = 2
      company_not_found = 3
      OTHERS            = 4.

  CLEAR l_name_text.
*  SELECT SINGLE name_text INTO l_name_text
*    FROM usr21
*    INNER JOIN adrp ON
*    usr21~persnumber = adrp~persnumber
*    WHERE bname EQ sy-uname.
  l_name_text = sy-uname.

  SELECT * INTO TABLE gt_vbrp
    FROM vbrp
    WHERE vbeln = g_fatura.

  SELECT * INTO TABLE gt_prcd_elements
  FROM prcd_elements
  WHERE  knumv EQ ge_zfie004-knumv
    AND kschl EQ 'ZDVE'.

  CLEAR ge_kna1.
  IF NOT ge_zfie004-bstkd_e IS INITIAL.
    l_lenght = strlen( ge_zfie004-bstkd_e ).

    IF l_lenght EQ 14. "CNPJ
      g_cpf_cnpj_txt = 'CNPJ        :'.
      g_cgc_number = ge_zfie004-bstkd_e.
      WRITE g_cgc_number TO l_campocnpj.
      g_cnpj = ge_zfie004-bstkd_e.
      SELECT SINGLE * INTO ge_kna1
        FROM kna1
        WHERE stcd1 = g_cnpj.
    ELSE.              "CPF
      g_cpf_cnpj_txt = 'CPF         :'.
      g_cpf_number = ge_zfie004-bstkd_e.
      WRITE g_cpf_number TO l_campocpf.
      l_campocnpj = l_campocpf.
      g_cpf  = ge_zfie004-bstkd_e.
      SELECT SINGLE * INTO ge_kna1
        FROM kna1
        WHERE stcd2 = g_cpf.
    ENDIF.
  ENDIF.

  "Nome da Filial
  CONCATENATE ge_address-name1 '-' ge_address-sortl
    INTO g_linha01 SEPARATED BY space.

  "Endereço da Filial
  g_linha02 =  ge_address-stras.

  CONCATENATE ge_address-ort02 '-' ge_address-ort01 '-' ge_address-regio
    INTO g_linha03 SEPARATED BY space.

  ge_zfie007-filial           = g_linha01.
  ge_zfie007-end1_filial      = g_linha02.
  ge_zfie007-end2_filial      = g_linha03.

  CLEAR: gt_dados, gt_dados[].
  CONCATENATE ge_address-name1 '-' ge_address-sortl
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE ge_address-stras TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE ge_address-ort02 '-' ge_address-ort01 '-' ge_address-regio
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE g_cgc_number2 TO l_campocnpj2.
  CONCATENATE 'CNPJ:' l_campocnpj2
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'IE:' ge_branch_data-state_insc
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '---------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '           COMPROVANTE DE CRÉDITO DE DEVOLUÇÃO           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE sy-datum TO l_campoaux1.
  WRITE sy-uzeit TO l_campoaux2.
  CONCATENATE l_campoaux1 l_campoaux2
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '---------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE sy-datum TO l_campoaux1.
  CONCATENATE 'N. ROMANEIO :' l_campoaux1
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'OPERADOR    :' sy-uname '-' l_name_text
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'NOME        :' ge_zfie004-bstkd
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE g_cpf_cnpj_txt l_campocnpj
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'RG          :' t_caixa
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE 'CUPOM/SR/ECF: 001717/F/230046764                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.

  LOOP AT gt_formpag INTO ge_formpag.
    WRITE ge_formpag-dmbtr TO l_campovalor.

    CONCATENATE 'FORMA PAGTO :' ge_formpag-dcr_meio_pagamento
      INTO g_linha SEPARATED BY space.
    g_linha+35(10) = l_campovalor.
    WRITE g_linha TO ge_dados-tdline.
    APPEND ge_dados TO gt_dados.
  ENDLOOP.

  WRITE '---------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                     ESPECIFICACOES                      ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '---------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE 'CODIGO   DESCRICAO                                       ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE 'QTDE   PR. BRUTO  DESC%  PR. UNIT SUB TOTAL No Rom NoCred' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-------------------------------------------------------- ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.

  CLEAR: g_contn, g_subtotal, g_totdesc, g_tottaxval.
  LOOP AT gt_vbrp INTO ge_vbrp.

    g_taxval = ge_vbrp-mwsbp.
    ADD g_taxval TO g_tottaxval.

    CLEAR ge_prcd_elements.
    READ TABLE gt_prcd_elements INTO ge_prcd_elements WITH KEY kposn = ge_vbrp-posnr.

    g_perdesc  = ge_prcd_elements-kbetr.
    g_desconto = ge_prcd_elements-kwert.
    ADD g_desconto TO g_totdesc.

    g_vlritem  = ge_vbrp-netwr + ge_vbrp-mwsbp.
    ADD g_vlritem TO g_subtotal.

    WRITE g_vlritem TO l_campovlrit.
    CONDENSE l_campovlrit.

    g_vlrtotal = g_vlritem - g_desconto.

    WRITE g_vlrtotal TO l_campototal.
*    CONDENSE l_campototal.

    l_qtdabs = ge_vbrp-fkimg.
    WRITE l_qtdabs TO l_campomeng7.

*    WRITE ge_vbrp-fkimg TO l_campomenge.
*    CONDENSE l_campomenge.

    WRITE ge_vbrp-meins TO l_campomeins.
    CONDENSE l_campomeins.

    l_val2dec = ge_vbrp-netwr / ge_vbrp-fkimg.
    l_prbruto = g_vlritem / ge_vbrp-fkimg.
    l_prunita = g_vlrtotal / ge_vbrp-fkimg.

    WRITE l_prbruto TO l_campobruto.
*    CONDENSE l_campobruto.

    WRITE l_prunita TO l_campounita.
*    CONDENSE l_campounita.

    WRITE l_val2dec TO l_camponetpr.
    CONDENSE l_camponetpr.

    WRITE g_taxval TO l_campotaxval.
    CONDENSE l_campotaxval.

    CONCATENATE '(' l_campotaxval ')' INTO l_campotaxval.
    CONDENSE l_campotaxval.

    WRITE g_vlritem TO l_camponetwr.
    CONDENSE l_camponetwr.

    WRITE g_perdesc TO l_campoperde.
    CONCATENATE l_campoperde '%' INTO l_campoperde.

    WRITE ge_vbrp-matnr TO l_campo1.
    CONDENSE l_campo1.
    g_linha04 = l_campo1.
    g_linha04+9(50) = ge_vbrp-arktx.
    WRITE g_linha04 TO ge_dados-tdline.
    APPEND ge_dados TO gt_dados.

    CONCATENATE l_campomeng7 l_campobruto l_campoperde l_campounita l_campototal
      INTO g_linha04 SEPARATED BY space.
    WRITE g_linha04(50) TO ge_dados-tdline.
    APPEND ge_dados TO gt_dados.

  ENDLOOP.

  WRITE '---------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '         _______________________________________         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                    ASS. RESPONSÁVEL                     ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '---------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE sy-datum TO l_campoaux1.
  WRITE sy-uzeit TO l_campoaux2.
  CONCATENATE l_campoaux1 l_campoaux2
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '---------------------------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '           OBS.: O CREDITO SÓ TERÁ VALIDADE SE           ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '       APRESENTADO COM DOCUMENTO DE IDENTIFICAÇÃO        ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '         ********  DOCUMENTO NÃO FISCAL *******          ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.

  l_visual = ' '.

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
      wa_output_options-tdimmed       = 'X'. "nast-dimme.
      wa_output_options-tddelete      = space.
      wa_output_options-tdnewid       = 'X'.
      wa_output_options-tdnoprev      = 'X'.
      wa_control_parameters-langu     = 'PT'.
      wa_control_parameters-no_dialog = 'X'.
      wa_control_parameters-preview   = ' '.
  ENDCASE.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZFI_RECIBO_COM_LOGO'
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
      ge_zfie007         = ge_zfie007
    TABLES
      gt_dados           = gt_dados
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUSCAR_DADOS_DEVO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM buscar_dados_devo.
  DATA: gt_zfit006_aux        TYPE STANDARD TABLE OF zfit006.
  DATA: gt_zfit006_venda_din  TYPE STANDARD TABLE OF zfit006.
  DATA: gt_zfit006_devo_din   TYPE STANDARD TABLE OF zfit006.
  DATA: gt_zfit006_aux2 TYPE STANDARD TABLE OF zfit006.
  DATA: l_valor_tot TYPE zfit006-valor.
  DATA: v_operacao(2) TYPE c.
  DATA: v_valor_devolvido TYPE zfit006-valor.
  DATA: v_saldo_devolucao TYPE zfit006-valor.
*inicio - LD - FI - DNF - DEVK917939 - 14/08/2020
  DATA: gt_zfit006_devo               TYPE STANDARD TABLE OF zfit006.
  DATA: l_valor_tot_dev  TYPE zfit006-valor.
*fim - LD - FI - DNF - DEVK917939 - 14/08/2020
  CLEAR: t_vlr_tot_a_devolver, t_vlr_tot_cred_cliente, t_vlr_tot_restante_dev,
         t_name1, gt_devol, gt_vbfa, gt_zfit006.

  CHECK NOT t_kunnr IS INITIAL.

  SELECT SINGLE name1 INTO t_name1
    FROM kna1
    WHERE kunnr EQ t_kunnr.

  IF sy-subrc NE 0 .
    "Cliente & não cadastrado!
    CLEAR: t_kunnr, t_name1.
    MESSAGE e014(zfi001) WITH t_kunnr.
  ENDIF.

  CLEAR: gt_creditos, gt_creditos[].
  "Buscar documentos de créditos do cliente
  SELECT belnr buzei gjahr xblnr bldat dmbtr
    INTO CORRESPONDING FIELDS OF TABLE gt_devol
    FROM bsid
    WHERE bukrs EQ t_bukrs
      AND kunnr EQ t_kunnr
      AND bschl IN ('11', '12', '14', '15', '16', '17', '19')
      AND zlspr EQ ''.

  SORT gt_devol BY bldat dmbtr.

  CHECK NOT gt_devol[] IS INITIAL.
  LOOP AT gt_devol INTO ge_devol.
    REFRESH: gt_zfit006, gt_zfit006_venda_din, gt_zfit006_aux.
*Primeira devolução, com referencia a documento SD
    SELECT * FROM vbfa INTO TABLE @DATA(gt_vbfa)
      WHERE vbeln = @ge_devol-belnr
        AND vbtyp_n = 'O'
        AND vbtyp_v = 'C'.
    IF sy-subrc EQ 0.
*  Registros de Devoluções de clientes  com Dinheiro referentes ao primeiro nível(Documento SD)
      SELECT *
        APPENDING TABLE gt_zfit006_venda_din
        FROM zfit006
        FOR ALL ENTRIES IN gt_vbfa
        WHERE vbeln              EQ gt_vbfa-vbelv
          AND operacao           EQ 'VEND'
          AND dcr_meio_pagamento EQ 'DINHEIRO'.
      v_operacao = 'DS'. "Devolução referente documento SD
    ELSE.
      SELECT *
      APPENDING TABLE gt_zfit006_venda_din
      FROM zfit006
      WHERE vbeln = ge_devol-belnr
      AND operacao           EQ 'ADIA'
       AND dcr_meio_pagamento EQ 'DINHEIRO'.

      IF sy-subrc EQ 0.
        v_operacao = 'DA'. "Devolução referente a Documento de Adiantamento
      ELSE.
*  Registros de devoluções ao clientes, que já ocorreram algum ressarcimento, para fins de manter a referencia ao documento original de SD
        SELECT *
        APPENDING TABLE gt_zfit006_aux
        FROM zfit006
        WHERE belnr = ge_devol-belnr.
        IF sy-subrc EQ 0.
          SELECT *
          APPENDING TABLE gt_zfit006_venda_din
          FROM zfit006
          FOR ALL ENTRIES IN gt_zfit006_aux
          WHERE vbeln              EQ gt_zfit006_aux-vbeln_ori
            AND ( operacao           EQ 'VEND' OR
                  operacao           EQ 'ADIA' )
            AND dcr_meio_pagamento EQ 'DINHEIRO'.
          v_operacao = 'DP'. "Devolução Parcial sem referencia a documento SD
        ENDIF.
      ENDIF.
    ENDIF.
    SORT gt_zfit006 BY vbeln .
    SORT gt_zfit006_venda_din BY vbeln .
    SORT gt_vbfa BY vbeln.
    SORT gt_zfit006_aux BY belnr.

    CLEAR ge_zfit006.
    CASE v_operacao.
      WHEN 'DS'. "Devolução Documento SD
        READ TABLE gt_vbfa ASSIGNING FIELD-SYMBOL(<fs_vbfa>) WITH KEY vbeln = ge_devol-belnr BINARY SEARCH.
        IF <fs_vbfa> IS  ASSIGNED.
          CLEAR ge_zfit006_dev.
          READ TABLE gt_zfit006_venda_din INTO ge_zfit006 WITH KEY vbeln = <fs_vbfa>-vbelv.
          IF sy-subrc EQ 0.
            SELECT *
            INTO TABLE gt_zfit006_devo_din
            FROM zfit006
            WHERE vbeln_ori              EQ <fs_vbfa>-vbelv
              AND operacao           EQ 'DEVO'
              AND dcr_meio_pagamento EQ 'DINHEIRO'.
            IF gt_zfit006_devo_din[] IS NOT INITIAL.
              CLEAR: v_valor_devolvido, v_saldo_devolucao.
              LOOP AT gt_zfit006_devo_din ASSIGNING FIELD-SYMBOL(<fs_gt_zfit006_devo_din1>).
                v_valor_devolvido = v_valor_devolvido + <fs_gt_zfit006_devo_din1>-valor.
              ENDLOOP.
              v_saldo_devolucao = ge_zfit006-valor - v_valor_devolvido.

              IF v_saldo_devolucao <  0.
                v_saldo_devolucao = 0.
              ENDIF.
              "Se o valor da Devolução for maior ou igual ao valor pago em Dinheiro durante a compra pelo cliente que será ressarcido
              IF ge_devol-dmbtr >= v_saldo_devolucao.
                "Posso devolver no máximo o valor pago em dinheiro.Neste caso o Valor da Devolução foi superior ao valor pago em dinheiro
                ge_devol-dmbtr2 = v_saldo_devolucao .
              ELSE.
                "Posso devolver no máximo o valor da devolução. Neste caso o valor da Devolução foi inferior ao valor pago em dinheiro
                ge_devol-dmbtr2 = ge_devol-dmbtr.
              ENDIF.


            ELSE.
              "Posso devolver no máximo o valor pago em dinheiro menos as Devoluções já realizadas
              IF ge_devol-dmbtr >=  ge_zfit006-valor.
                ge_devol-dmbtr2 = ge_zfit006-valor.
              ELSE.
                ge_devol-dmbtr2 = ge_devol-dmbtr.
              ENDIF.
            ENDIF.

            ge_devol-vbeln_ori    = ge_zfit006-vbeln.
            ge_devol-vbeln_vf_ori = ge_zfit006-vbeln_vf.
            ADD ge_devol-dmbtr2 TO t_vlr_tot_cred_cliente.
            MODIFY gt_devol FROM ge_devol.
          ENDIF.
        ENDIF.
      WHEN 'DP'. "Devolução Parcial sem referencia a documento SD
        READ TABLE gt_zfit006_aux ASSIGNING FIELD-SYMBOL(<fs_gt_zfit006_aux>) WITH KEY belnr = ge_devol-belnr BINARY SEARCH.
        IF <fs_gt_zfit006_aux> IS  ASSIGNED.
          CLEAR ge_zfit006_dev.
          READ TABLE gt_zfit006_venda_din INTO ge_zfit006 WITH KEY vbeln = <fs_gt_zfit006_aux>-vbeln_ori BINARY SEARCH.
          IF sy-subrc EQ 0.
            SELECT *
            INTO TABLE gt_zfit006_devo_din
            FROM zfit006
            WHERE vbeln_ori              EQ <fs_gt_zfit006_aux>-vbeln_ori
              AND operacao           EQ 'DEVO'
              AND dcr_meio_pagamento EQ 'DINHEIRO'.
            "Garantir que o valor da Devolução seja maior ou igual ao valor pago em Dinheiro durante a compra pelo cliente que será ressarcido
            IF gt_zfit006_devo_din[] IS NOT INITIAL.
              CLEAR: v_valor_devolvido, v_saldo_devolucao.
              LOOP AT gt_zfit006_devo_din ASSIGNING FIELD-SYMBOL(<fs_gt_zfit006_devo_din>).
                v_valor_devolvido = v_valor_devolvido + <fs_gt_zfit006_devo_din>-valor.
              ENDLOOP.
              v_saldo_devolucao = ge_zfit006-valor - v_valor_devolvido.
              IF v_saldo_devolucao <  0.
                v_saldo_devolucao = 0.
              ENDIF.

              "Se o valor da Devolução for maior ou igual ao valor pago em Dinheiro durante a compra pelo cliente que será ressarcido
              IF ge_devol-dmbtr >= v_saldo_devolucao.
                "Posso devolver no máximo o valor pago em dinheiro.Neste caso o Valor da Devolução foi superior ao valor pago em dinheiro
                ge_devol-dmbtr2 = v_saldo_devolucao .
              ELSE.
                "Posso devolver no máximo o valor da devolução. Neste caso o valor da Devolução foi inferior ao valor pago em dinheiro
                ge_devol-dmbtr2 = ge_devol-dmbtr.
              ENDIF.

            ELSE.
              "Posso devolver no máximo o valor pago em dinheiro menos as Devoluções já realizadas
              IF ge_devol-dmbtr >=  ge_zfit006-valor.
                ge_devol-dmbtr2 = ge_zfit006-valor.
              ELSE.
                ge_devol-dmbtr2 = ge_devol-dmbtr.
              ENDIF.
            ENDIF.

            ge_devol-vbeln_ori    = ge_zfit006-vbeln.
            ge_devol-vbeln_vf_ori = ge_zfit006-vbeln_vf.
            ADD ge_devol-dmbtr2 TO t_vlr_tot_cred_cliente.
            MODIFY gt_devol FROM ge_devol.
          ENDIF.
        ENDIF.
      WHEN 'DA'.
        CLEAR ge_zfit006_dev.
        READ TABLE gt_zfit006_venda_din INTO ge_zfit006 WITH KEY vbeln = ge_devol-belnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          SELECT *
          INTO TABLE gt_zfit006_devo_din
          FROM zfit006
          WHERE vbeln_ori              EQ ge_zfit006-vbeln
            AND operacao           EQ 'DEVO'
            AND dcr_meio_pagamento EQ 'DINHEIRO'.
          IF gt_zfit006_devo_din[] IS NOT INITIAL.
            CLEAR: v_valor_devolvido, v_saldo_devolucao.
            LOOP AT gt_zfit006_devo_din ASSIGNING FIELD-SYMBOL(<fs_gt_zfit006_devo_din2>).
              v_valor_devolvido = v_valor_devolvido + <fs_gt_zfit006_devo_din2>-valor.
            ENDLOOP.
            v_saldo_devolucao = ge_zfit006-valor - v_valor_devolvido.
            IF v_saldo_devolucao <  0.
              v_saldo_devolucao = 0.
            ENDIF.
            "Se o valor da Devolução for maior ou igual ao valor pago em Dinheiro durante a compra pelo cliente que será ressarcido
            IF ge_devol-dmbtr >= v_saldo_devolucao.
              "Posso devolver no máximo o valor pago em dinheiro.Neste caso o Valor da Devolução foi superior ao valor pago em dinheiro
              ge_devol-dmbtr2 = v_saldo_devolucao .
            ELSE.
              "Posso devolver no máximo o valor da devolução. Neste caso o valor da Devolução foi inferior ao valor pago em dinheiro
              ge_devol-dmbtr2 = ge_devol-dmbtr.
            ENDIF.

          ELSE.
            "Posso devolver no máximo o valor pago em dinheiro menos as Devoluções já realizadas
            IF ge_devol-dmbtr >=  ge_zfit006-valor.
              ge_devol-dmbtr2 = ge_zfit006-valor.
            ELSE.
              ge_devol-dmbtr2 = ge_devol-dmbtr.
            ENDIF.
          ENDIF.

          ge_devol-vbeln_ori    = ge_zfit006-vbeln.
          ge_devol-vbeln_vf_ori = ge_zfit006-vbeln_vf.
          ADD ge_devol-dmbtr2 TO t_vlr_tot_cred_cliente.
          MODIFY gt_devol FROM ge_devol.
        ENDIF.
    ENDCASE.
  ENDLOOP.

  SORT gt_devol BY bldat dmbtr2.

  t_vlr_tot_a_devolver = t_vlr_tot_cred_cliente.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUSCAR_DADOS_DEVO_TOTAL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM buscar_dados_devo_total.

  DATA: l_netwr     TYPE vbrk-netwr,
        l_netwr_dev TYPE vbrk-netwr,
        l_datum     TYPE datum,
        l_valor_tot TYPE zfit006-valor.

  CLEAR: t_vlr_tot_a_devolver, t_vlr_tot_cred_cliente, t_vlr_tot_restante_dev,
         t_name1, gt_devol, gt_vbfa, gt_zfit006.

  CHECK NOT t_kunnr IS INITIAL.
**Deivison Ferreira
**Data:16/10/2020
**********
*  l_datum =  sy-datum - 0.

  SELECT SINGLE name1 INTO t_name1
    FROM kna1
    WHERE kunnr EQ t_kunnr.

  IF sy-subrc NE 0 .
    "Cliente & não cadastrado!
    CLEAR: t_kunnr, t_name1.
    MESSAGE e014(zfi001) WITH t_kunnr.
  ENDIF.

  CLEAR: gt_creditos, gt_creditos[].

  SELECT belnr buzei gjahr xblnr bldat dmbtr
    INTO CORRESPONDING FIELDS OF TABLE gt_devol
    FROM bsid
    WHERE bukrs EQ t_bukrs
      AND kunnr EQ t_kunnr
      AND bschl IN ('11', '12', '14', '15', '16', '17', '19')
      AND zlspr EQ ''.

  SORT gt_devol BY belnr buzei.

  CHECK NOT gt_devol[] IS INITIAL.

*  CLEAR: gt_devol2, gt_devol2[].
*  LOOP AT gt_devol INTO ge_devol.
*    ge_devol-buzei = ''.
*    COLLECT ge_devol INTO gt_devol2.
*  ENDLOOP.
*
*  gt_devol[] = gt_devol2[].
*  FREE gt_devol2.

  SELECT *
    INTO TABLE gt_vbfa
    FROM vbfa
    FOR ALL ENTRIES IN gt_devol
    WHERE vbeln EQ gt_devol-belnr
      AND vbtyp_n EQ 'O'
      AND vbtyp_v EQ 'M'.

  SORT gt_vbfa BY vbelv.

  IF NOT gt_vbfa[] IS INITIAL.
    SELECT *
      INTO TABLE gt_zfit006
      FROM zfit006
      FOR ALL ENTRIES IN gt_vbfa
      WHERE vbeln_vf           EQ gt_vbfa-vbelv
        AND operacao           EQ 'VEND'.
**Deivison Ferreira
**Data 16/10/2020
*        AND data               EQ l_datum.
***
    SELECT *
      INTO TABLE gt_vbrk
      FROM vbrk
      FOR ALL ENTRIES IN gt_vbfa
      WHERE vbeln EQ gt_vbfa-vbelv.

    SELECT *
      APPENDING TABLE gt_vbrk
      FROM vbrk
      FOR ALL ENTRIES IN gt_vbfa
      WHERE vbeln EQ gt_vbfa-vbeln.

    SORT gt_vbrk BY vbeln.
  ENDIF.

  SELECT *
    APPENDING TABLE gt_zfit006
    FROM zfit006
    FOR ALL ENTRIES IN gt_devol
    WHERE belnr              EQ gt_devol-belnr
*      AND operacao           EQ 'VEND'
      AND operacao           IN ('VEND','ADIA').
**Deivison Ferreira
**Data: 16/10/2020
*      AND data               EQ l_datum.
********
  SORT gt_zfit006 BY vbeln item.

  DELETE ADJACENT DUPLICATES FROM gt_zfit006 COMPARING ALL FIELDS.
  DELETE gt_zfit006 WHERE dcr_meio_pagamento EQ 'CRÉDITO DO CLIENTE'.
  DELETE gt_zfit006 WHERE dcr_meio_pagamento EQ 'TROCO'.

  IF gt_zfit006[] IS NOT INITIAL.
    SELECT *
      INTO TABLE gt_cct006
      FROM /gfxdsi/cct006
      FOR ALL ENTRIES IN gt_zfit006
      WHERE empresa       EQ t_bukrs
        AND filial        EQ t_bupla
        AND cliente       EQ gt_zfit006-cliente
        AND num_documento EQ gt_zfit006-belnr.
  ENDIF.

  CLEAR: gt_devol2, gt_devol2[].
  LOOP AT gt_devol INTO ge_devol.

    CLEAR ge_vbfa.
    READ TABLE gt_vbfa INTO ge_vbfa WITH KEY vbeln = ge_devol-belnr.

    IF NOT ge_vbfa-vbelv IS INITIAL.
      CLEAR ge_zfit006.
      READ TABLE gt_zfit006 INTO ge_zfit006 WITH KEY vbeln_vf = ge_vbfa-vbelv.

      IF sy-subrc NE 0.
        DELETE gt_devol.
        CONTINUE.
      ENDIF.

      CLEAR ge_vbrk.
      READ TABLE gt_vbrk INTO ge_vbrk WITH KEY vbeln = ge_vbfa-vbelv.
      l_netwr     = ge_vbrk-netwr.

      CLEAR ge_vbrk.
      READ TABLE gt_vbrk INTO ge_vbrk WITH KEY vbeln = ge_vbfa-vbeln.
      l_netwr_dev = ge_vbrk-netwr.

      "Se o valor total da fatura devolvida for diferente do valor total da fatura
      "ela será uma devvolução parcial e não poderá estar nesta tela
      IF l_netwr NE l_netwr_dev.
        DELETE gt_devol.
        CONTINUE.
      ENDIF.

      CLEAR ge_zfit006.
      READ TABLE gt_zfit006 INTO ge_zfit006 WITH KEY vbeln_vf           = ge_vbfa-vbelv
                                                     dcr_meio_pagamento = 'DINHEIRO'.

    ELSE.
      CLEAR ge_zfit006.
      READ TABLE gt_zfit006 INTO ge_zfit006 WITH KEY belnr = ge_devol-belnr.

      IF sy-subrc NE 0.
        DELETE gt_devol.
        CONTINUE.
      ENDIF.

      CLEAR ge_zfit006.
      READ TABLE gt_zfit006 INTO ge_zfit006 WITH KEY belnr              = ge_devol-belnr
                                                     dcr_meio_pagamento = 'DINHEIRO'.
    ENDIF.

    l_valor_tot = ge_zfit006-valor_bruto - ge_zfit006-desconto.
    "Se o valor pago foi somente em dinheiro e teve uma devolção parcial
    "Este registro deve aparecer para ressarcimento
    IF ge_zfit006-valor NE l_valor_tot.
      "OK
    ELSE.
      "Não excluir operações de crédito em dinheiro da devolução total
      IF ge_zfit006-valor EQ ge_devol-dmbtr AND ge_zfit006-dcr_meio_pagamento NE  'DINHEIRO'.
        DELETE gt_devol.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF ge_vbfa-vbelv IS NOT INITIAL.
      LOOP AT gt_zfit006 INTO ge_zfit006 WHERE vbeln_vf = ge_vbfa-vbelv.

        ge_devol-dcr_meio_pagamento = ge_zfit006-dcr_meio_pagamento.
        IF ge_zfit006-valor_ori GT 0.
          ge_devol-dmbtr2             = ge_zfit006-valor_ori - ge_zfit006-valor.
        ELSE.
          ge_devol-dmbtr2             = ge_zfit006-valor.
          IF ge_devol-dmbtr2 GT ge_devol-dmbtr.
            ge_devol-dmbtr2             = ge_devol-dmbtr.
          ENDIF.
        ENDIF.

        ge_devol-vbeln_vf           = ge_vbfa-vbeln.
        ge_devol-vbeln_ori          = ge_zfit006-vbeln.
        ge_devol-vbeln_vf_ori       = ge_zfit006-vbeln_vf.
        ge_devol-tp_pagto_cartao    = ge_zfit006-tp_pagto_cartao.
        ge_devol-modalidade         = ge_zfit006-modalidade.
        ge_devol-num_apr_cheque     = ge_zfit006-num_apr_cheque.
        ge_devol-apr_pos            = ge_zfit006-apr_pos.
        ge_devol-item               = ge_zfit006-item.
        ge_devol-gjahr              = ge_zfit006-gjahr.
        ge_devol-cartao             = ge_zfit006-cartao.
        ge_devol-cliente            = ge_zfit006-cliente.
        ge_devol-docnum             = ge_zfit006-docnum.

        ADD ge_devol-dmbtr2 TO t_vlr_tot_cred_cliente.

        IF ge_zfit006-dcr_meio_pagamento EQ 'CARTÃO DE CRÉDITO' OR
           ge_zfit006-dcr_meio_pagamento EQ 'CARTÃO DE DÉBITO' .
          ge_devol-icon               = '@0W@'.
        ELSE.
          ge_devol-icon               = '@0V@'.
        ENDIF.

        APPEND ge_devol TO gt_devol2.

      ENDLOOP.
    ELSE.
      LOOP AT gt_zfit006 INTO ge_zfit006 WHERE belnr    = ge_devol-belnr.

        ge_devol-dcr_meio_pagamento = ge_zfit006-dcr_meio_pagamento.
        IF ge_zfit006-valor_ori GT 0.
          ge_devol-dmbtr2             = ge_zfit006-valor_ori - ge_zfit006-valor.
        ELSE.
          ge_devol-dmbtr2             = ge_zfit006-valor.
          IF ge_devol-dmbtr2 GT ge_devol-dmbtr.
            ge_devol-dmbtr2             = ge_devol-dmbtr.
          ENDIF.
        ENDIF.

        ge_devol-vbeln_vf           = ge_vbfa-vbeln.
        ge_devol-vbeln_ori          = ge_zfit006-vbeln.
        ge_devol-vbeln_vf_ori       = ge_zfit006-vbeln_vf.
        ge_devol-tp_pagto_cartao    = ge_zfit006-tp_pagto_cartao.
        ge_devol-modalidade         = ge_zfit006-modalidade.
        ge_devol-num_apr_cheque     = ge_zfit006-num_apr_cheque.
        ge_devol-apr_pos            = ge_zfit006-apr_pos.
        ge_devol-item               = ge_zfit006-item.
        ge_devol-gjahr              = ge_zfit006-gjahr.
        ge_devol-cartao             = ge_zfit006-cartao.
        ge_devol-cliente            = ge_zfit006-cliente.
        ge_devol-docnum             = ge_zfit006-docnum.

        ADD ge_devol-dmbtr2 TO t_vlr_tot_cred_cliente.

        IF ge_zfit006-dcr_meio_pagamento EQ 'CARTÃO DE CRÉDITO' OR
           ge_zfit006-dcr_meio_pagamento EQ 'CARTÃO DE DÉBITO' .
          ge_devol-icon               = '@0W@'.
        ELSE.
          ge_devol-icon               = '@0V@'.
        ENDIF.

        APPEND ge_devol TO gt_devol2.

      ENDLOOP.
    ENDIF.

  ENDLOOP.

  gt_devol[] = gt_devol2[].
*  SORT gt_devol BY vbeln_ori item bldat dmbtr2.

  FREE gt_devol2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONSULTA_CREDITO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM consulta_credito .

  CLEAR: t_vlr_tot_credito, t_kunnr, t_name1, gt_creditos.

  CALL SCREEN '9002' STARTING AT 02 01
                     ENDING AT 85 20.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUSCAR_DADOS_CRED
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM buscar_dados_cred.

  CLEAR: t_vlr_tot_credito, t_name1, gt_creditos.

  CHECK NOT t_kunnr IS INITIAL.

  SELECT COUNT(*)
    FROM zsdt003f
    WHERE werks EQ g_werks
      AND kunnr EQ t_kunnr.

  "Verifica Cliente Balcão
  CHECK NOT sy-subrc IS INITIAL.

  SELECT SINGLE name1 INTO t_name1
    FROM kna1
    WHERE kunnr EQ t_kunnr.

  IF sy-subrc NE 0 .
    "Cliente & não cadastrado!
    CLEAR: t_kunnr, t_name1.
    MESSAGE e014(zfi001) WITH t_kunnr.
  ENDIF.

  SELECT belnr buzei gjahr xblnr bldat dmbtr
    INTO CORRESPONDING FIELDS OF TABLE gt_creditos
    FROM bsid
    WHERE bukrs EQ t_bukrs
      AND kunnr EQ t_kunnr
      AND bschl IN ('11', '12', '14', '15', '16', '17', '19')
      AND zlspr EQ ''.

  SORT gt_creditos BY bldat dmbtr.

  IF NOT gt_creditos[] IS INITIAL.
    SELECT *
      INTO TABLE gt_acdoca
      FROM acdoca
      FOR ALL ENTRIES IN gt_creditos
      WHERE rldnr  EQ '0L'
        AND rbukrs EQ t_bukrs
        AND belnr  EQ gt_creditos-belnr
        AND gjahr  EQ gt_creditos-gjahr
        AND buzei  EQ gt_creditos-buzei.

    SORT gt_acdoca BY belnr buzei.

    LOOP AT gt_creditos INTO ge_creditos.

      "Busca o centro de lucro em documentos compensados
      SELECT SINGLE zfit064~prctr "Centro de lucro
      FROM bsad
      INNER JOIN t001w
      ON ( t001w~j_1bbranch = bsad~bupla )
      INNER JOIN zfit064
      ON ( zfit064~werks = t001w~werks )
      WHERE bsad~augbl = @ge_creditos-belnr
      AND   bsad~bschl = '01'
      AND   zfit064~prctr <> ''
      INTO @ge_creditos-prctr.

      CLEAR ge_acdoca.
      READ TABLE gt_acdoca INTO ge_acdoca WITH KEY belnr = ge_creditos-belnr
                                                   gjahr = ge_creditos-gjahr
                                                   buzei = ge_creditos-buzei.

      IF ge_creditos-prctr IS INITIAL.
        ge_creditos-prctr = ge_acdoca-prctr.
      ENDIF.

      CLEAR ge_zmmt005.
      READ TABLE gt_zmmt005 INTO ge_zmmt005 WITH KEY prctr = ge_acdoca-prctr.
      IF sy-subrc = 0.
        ge_creditos-bupla = ge_zmmt005-branch.
      ELSE.
        READ TABLE gt_zmmt005 INTO ge_zmmt005 WITH KEY prctr = ge_creditos-prctr.
        IF sy-subrc = 0.
          ge_creditos-bupla = ge_zmmt005-branch.
        ENDIF.
      ENDIF.

      MODIFY gt_creditos FROM ge_creditos.

    ENDLOOP.

  ENDIF.

*  SELECT *
*    APPENDING TABLE gt_zfit006
*    FROM zfit006
*    FOR ALL ENTRIES IN gt_devol
*    WHERE belnr              EQ gt_devol-belnr
*      AND operacao           EQ 'VEND'
*      AND data               EQ l_datum.
*
*  SORT gt_zfit006 BY vbeln item.


  LOOP AT gt_creditos INTO ge_creditos.

    ADD ge_creditos-dmbtr TO t_vlr_tot_credito.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form IMPRIMIR_RECIBO_CRED_CLI1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM imprimir_recibo_cred_cli1.

  DATA: formname              TYPE tdsfname,
        fm_name               TYPE rs38l_fnam,
        wa_control_parameters TYPE ssfctrlop,         "Estrutura de controle (smartforms)
        wa_output_options     TYPE ssfcompop.         "Estrutura de controle (smartforms)

  DATA: gc_ucomm LIKE sy-ucomm.

  DATA: l_visual,
        l_name_text       TYPE adrp-name_text,
        l_lenght          TYPE i,
        l_val2dec         TYPE j_1bnflin-netwr,
        l_prbruto         TYPE j_1bnflin-netwr,
        l_prunita         TYPE j_1bnflin-netwr,
        l_qtdabs          TYPE ladlg,
        l_campoaux1(132)  TYPE c,
        l_campoaux2(132)  TYPE c,
        l_campoval(25)    TYPE c,
        l_campocpf(14)    TYPE c,
        l_campocnpj(18)   TYPE c,
        l_campocnpj2(18)  TYPE c,
        l_campovlrit(50)  TYPE c,
        l_campoperde(6)   TYPE c,
        l_campomenge(50)  TYPE c,
        l_campomeng7(7)   TYPE c,
        l_campomeins(50)  TYPE c,
        l_camponetpr(50)  TYPE c,
        l_campobruto(10)  TYPE c,
        l_campounita(10)  TYPE c,
        l_campototal(10)  TYPE c,
        l_campovalor(10)  TYPE c,
        l_campotaxval(50) TYPE c,
        l_camponetwr(50)  TYPE c,
        l_campo1(50)      TYPE c,
        l_campo2(50)      TYPE c.

  CLEAR ge_zfie007.

  CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
    EXPORTING
      branch            = t_bupla
      bukrs             = t_bukrs
    IMPORTING
      address           = ge_address
      branch_data       = ge_branch_data
      cgc_number        = g_cgc_number2
      address1          = ge_address1
    EXCEPTIONS
      branch_not_found  = 1
      address_not_found = 2
      company_not_found = 3
      OTHERS            = 4.

  CLEAR l_name_text.
*  SELECT SINGLE name_text INTO l_name_text
*    FROM usr21
*    INNER JOIN adrp ON
*    usr21~persnumber = adrp~persnumber
*    WHERE bname EQ sy-uname.
  l_name_text = sy-uname.

  CLEAR ge_kna1.
  SELECT SINGLE * INTO ge_kna1
    FROM kna1
    WHERE kunnr EQ ge_zfie004-kunnr.

  IF NOT ge_kna1-stcd1 IS INITIAL. "CNPJ
    g_cpf_cnpj_txt = 'CNPJ:'.
    g_cgc_number = ge_kna1-stcd1.
    WRITE g_cgc_number TO l_campocnpj.
  ELSE.              "CPF
    g_cpf_cnpj_txt = 'CPF:'.
    g_cpf_number = ge_kna1-stcd2.
    WRITE g_cpf_number TO l_campocpf.
    l_campocnpj = l_campocpf.
  ENDIF.

  CLEAR g_totcred.
  LOOP AT gt_creditos INTO ge_creditos.

    ADD ge_creditos-dmbtr TO g_totcred.

  ENDLOOP.

  g_saldocred = g_totcred - ge_formpag-dmbtr.

  "Nome da Filial
  CONCATENATE ge_address-name1 '-' ge_address-sortl
    INTO g_linha01 SEPARATED BY space.

  "Endereço da Filial
  g_linha02 =  ge_address-stras.

  CONCATENATE ge_address-ort02 '-' ge_address-ort01 '-' ge_address-regio
    INTO g_linha03 SEPARATED BY space.

  ge_zfie007-filial           = g_linha01.
  ge_zfie007-end1_filial      = g_linha02.
  ge_zfie007-end2_filial      = g_linha03.

  CLEAR: gt_dados, gt_dados[].
  CONCATENATE ge_address-name1 '-' ge_address-sortl
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE ge_address-stras TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE ge_address-ort02 '-' ge_address-ort01 '-' ge_address-regio
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE g_cgc_number2 TO l_campocnpj.
  CONCATENATE 'CNPJ:' l_campocnpj
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'IE:' ge_branch_data-state_insc
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-----------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '             1a VIA-CLIENTE              ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '  COMPROVANTE DE UTILIZAÇÃO DE CRÉDITO   ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE sy-datum TO l_campoaux1.
  WRITE sy-uzeit TO l_campoaux2.
  CONCATENATE l_campoaux1 l_campoaux2
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-----------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE sy-datum TO l_campoaux1.
  CONCATENATE 'DATA:' l_campoaux1
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'CAIXA:' t_caixa
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'OPERADOR:' l_name_text
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE g_cpf_cnpj_txt l_campocnpj
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'CLIENTE:' ge_kna1-name1
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE ge_formpag-dmbtr TO l_campovalor.
  CONCATENATE 'VALOR DO CRÉDITO:' l_campovalor
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE g_saldocred TO l_campovalor.
  CONCATENATE 'SALDO DO CLIENTE:' l_campovalor
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-----------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE 'Ass Cliente: ____________________________' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-----------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
*  WRITE sy-datum TO l_campoaux1.
*  WRITE sy-uzeit TO l_campoaux2.
*  CONCATENATE l_campoaux1 l_campoaux2
*    INTO g_linha SEPARATED BY space.
*  WRITE g_linha TO ge_dados-tdline.
*  APPEND ge_dados TO gt_dados.
*  WRITE '-----------------------------------------' TO ge_dados-tdline.
*  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '.                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.

  l_visual = ' '.

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
      wa_output_options-tdimmed       = 'X'. "nast-dimme.
      wa_output_options-tddelete      = space.
      wa_output_options-tdnewid       = 'X'.
      wa_output_options-tdnoprev      = 'X'.
      wa_control_parameters-langu     = 'PT'.
      wa_control_parameters-no_dialog = 'X'.
      wa_control_parameters-preview   = ' '.
  ENDCASE.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZFI_RECIBO_COM_LOGO'
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
      ge_zfie007         = ge_zfie007
    TABLES
      gt_dados           = gt_dados
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form IMPRIMIR_RECIBO_CRED_CLI2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM imprimir_recibo_cred_cli2.

  DATA: formname              TYPE tdsfname,
        fm_name               TYPE rs38l_fnam,
        wa_control_parameters TYPE ssfctrlop,         "Estrutura de controle (smartforms)
        wa_output_options     TYPE ssfcompop.         "Estrutura de controle (smartforms)

  DATA: gc_ucomm LIKE sy-ucomm.

  DATA: l_visual,
        l_name_text       TYPE adrp-name_text,
        l_lenght          TYPE i,
        l_val2dec         TYPE j_1bnflin-netwr,
        l_prbruto         TYPE j_1bnflin-netwr,
        l_prunita         TYPE j_1bnflin-netwr,
        l_qtdabs          TYPE ladlg,
        l_campoaux1(132)  TYPE c,
        l_campoaux2(132)  TYPE c,
        l_campoval(25)    TYPE c,
        l_campocpf(14)    TYPE c,
        l_campocnpj(18)   TYPE c,
        l_campocnpj2(18)  TYPE c,
        l_campovlrit(50)  TYPE c,
        l_campoperde(6)   TYPE c,
        l_campomenge(50)  TYPE c,
        l_campomeng7(7)   TYPE c,
        l_campomeins(50)  TYPE c,
        l_camponetpr(50)  TYPE c,
        l_campobruto(10)  TYPE c,
        l_campounita(10)  TYPE c,
        l_campototal(10)  TYPE c,
        l_campovalor(10)  TYPE c,
        l_campotaxval(50) TYPE c,
        l_camponetwr(50)  TYPE c,
        l_campo1(50)      TYPE c,
        l_campo2(50)      TYPE c.

  CLEAR ge_zfie007.

  CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
    EXPORTING
      branch            = t_bupla
      bukrs             = t_bukrs
    IMPORTING
      address           = ge_address
      branch_data       = ge_branch_data
      cgc_number        = g_cgc_number2
      address1          = ge_address1
    EXCEPTIONS
      branch_not_found  = 1
      address_not_found = 2
      company_not_found = 3
      OTHERS            = 4.

  CLEAR l_name_text.
*  SELECT SINGLE name_text INTO l_name_text
*    FROM usr21
*    INNER JOIN adrp ON
*    usr21~persnumber = adrp~persnumber
*    WHERE bname EQ sy-uname.
  l_name_text = sy-uname.

  CLEAR ge_kna1.
  SELECT SINGLE * INTO ge_kna1
    FROM kna1
    WHERE kunnr EQ ge_zfie004-kunnr.

  IF NOT ge_kna1-stcd1 IS INITIAL. "CNPJ
    g_cpf_cnpj_txt = 'CNPJ:'.
    g_cgc_number = ge_kna1-stcd1.
    WRITE g_cgc_number TO l_campocnpj.
  ELSE.              "CPF
    g_cpf_cnpj_txt = 'CPF:'.
    g_cpf_number = ge_kna1-stcd2.
    WRITE g_cpf_number TO l_campocpf.
    l_campocnpj = l_campocpf.
  ENDIF.

  CLEAR g_totcred.
  LOOP AT gt_creditos INTO ge_creditos.

    ADD ge_creditos-dmbtr TO g_totcred.

  ENDLOOP.

  g_saldocred = g_totcred - ge_formpag-dmbtr.

  "Nome da Filial
  CONCATENATE ge_address-name1 '-' ge_address-sortl
    INTO g_linha01 SEPARATED BY space.

  "Endereço da Filial
  g_linha02 =  ge_address-stras.

  CONCATENATE ge_address-ort02 '-' ge_address-ort01 '-' ge_address-regio
    INTO g_linha03 SEPARATED BY space.

  ge_zfie007-filial           = g_linha01.
  ge_zfie007-end1_filial      = g_linha02.
  ge_zfie007-end2_filial      = g_linha03.

  "Nome da Filial
  CONCATENATE ge_address-name1 '-' ge_address-sortl
    INTO g_linha01 SEPARATED BY space.

  "Endereço da Filial
  g_linha02 =  ge_address-stras.

  CONCATENATE ge_address-ort02 '-' ge_address-ort01 '-' ge_address-regio
    INTO g_linha03 SEPARATED BY space.

  ge_zfie007-filial           = g_linha01.
  ge_zfie007-end1_filial      = g_linha02.
  ge_zfie007-end2_filial      = g_linha03.

  CLEAR: gt_dados, gt_dados[].
  CONCATENATE ge_address-name1 '-' ge_address-sortl
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE ge_address-stras TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE ge_address-ort02 '-' ge_address-ort01 '-' ge_address-regio
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE g_cgc_number2 TO l_campocnpj.
  CONCATENATE 'CNPJ:' l_campocnpj
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'IE:' ge_branch_data-state_insc
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-----------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '         2a VIA-ESTABELECIMENTO          ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '  COMPROVANTE DE UTILIZAÇÃO DE CRÉDITO   ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE sy-datum TO l_campoaux1.
  WRITE sy-uzeit TO l_campoaux2.
  CONCATENATE l_campoaux1 l_campoaux2
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-----------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE sy-datum TO l_campoaux1.
  CONCATENATE 'DATA:' l_campoaux1
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'CAIXA:' t_caixa
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'OPERADOR:' l_name_text
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE g_cpf_cnpj_txt l_campocnpj
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  CONCATENATE 'CLIENTE:' ge_kna1-name1
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE ge_formpag-dmbtr TO l_campovalor.
  CONCATENATE 'VALOR DO CRÉDITO:' l_campovalor
    INTO g_linha SEPARATED BY space.
  WRITE g_linha TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-----------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE 'Ass Cliente: ____________________________' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-----------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
*  WRITE sy-datum TO l_campoaux1.
*  WRITE sy-uzeit TO l_campoaux2.
*  CONCATENATE l_campoaux1 l_campoaux2
*    INTO g_linha SEPARATED BY space.
*  WRITE g_linha TO ge_dados-tdline.
*  APPEND ge_dados TO gt_dados.
*  WRITE '-----------------------------------------' TO ge_dados-tdline.
*  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '.                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.

  l_visual = ' '.

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
      wa_output_options-tdimmed       = 'X'. "nast-dimme.
      wa_output_options-tddelete      = space.
      wa_output_options-tdnewid       = 'X'.
      wa_output_options-tdnoprev      = 'X'.
      wa_control_parameters-langu     = 'PT'.
      wa_control_parameters-no_dialog = 'X'.
      wa_control_parameters-preview   = ' '.
  ENDCASE.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZFI_RECIBO_COM_LOGO'
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
      ge_zfie007         = ge_zfie007
    TABLES
      gt_dados           = gt_dados
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form IMPRIMIR_RECIBO_CARTAO_DC1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM imprimir_recibo_cartao_dc1.

  DATA: formname              TYPE tdsfname,
        fm_name               TYPE rs38l_fnam,
        wa_control_parameters TYPE ssfctrlop,         "Estrutura de controle (smartforms)
        wa_output_options     TYPE ssfcompop.         "Estrutura de controle (smartforms)

  DATA: gc_ucomm LIKE sy-ucomm.

  DATA: l_visual,
        l_name_text       TYPE adrp-name_text,
        l_lenght          TYPE i,
        l_val2dec         TYPE j_1bnflin-netwr,
        l_prbruto         TYPE j_1bnflin-netwr,
        l_prunita         TYPE j_1bnflin-netwr,
        l_qtdabs          TYPE ladlg,
        l_campoaux1(132)  TYPE c,
        l_campoaux2(132)  TYPE c,
        l_campoval(25)    TYPE c,
        l_campocpf(14)    TYPE c,
        l_campocnpj(18)   TYPE c,
        l_campocnpj2(18)  TYPE c,
        l_campovlrit(50)  TYPE c,
        l_campoperde(6)   TYPE c,
        l_campomenge(50)  TYPE c,
        l_campomeng7(7)   TYPE c,
        l_campomeins(50)  TYPE c,
        l_camponetpr(50)  TYPE c,
        l_campobruto(10)  TYPE c,
        l_campounita(10)  TYPE c,
        l_campototal(10)  TYPE c,
        l_campovalor(10)  TYPE c,
        l_campotaxval(50) TYPE c,
        l_camponetwr(50)  TYPE c,
        l_campo1(50)      TYPE c,
        l_campo2(50)      TYPE c.

  CLEAR ge_zfie007.

  CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
    EXPORTING
      branch            = t_bupla
      bukrs             = t_bukrs
    IMPORTING
      address           = ge_address
      branch_data       = ge_branch_data
      cgc_number        = g_cgc_number2
      address1          = ge_address1
    EXCEPTIONS
      branch_not_found  = 1
      address_not_found = 2
      company_not_found = 3
      OTHERS            = 4.

  CLEAR l_name_text.
*  SELECT SINGLE name_text INTO l_name_text
*    FROM usr21
*    INNER JOIN adrp ON
*    usr21~persnumber = adrp~persnumber
*    WHERE bname EQ sy-uname.
  l_name_text = sy-uname.

  CLEAR ge_kna1.
  SELECT SINGLE * INTO ge_kna1
    FROM kna1
    WHERE kunnr EQ ge_zfie004-kunnr.

  IF NOT ge_kna1-stcd1 IS INITIAL. "CNPJ
    g_cpf_cnpj_txt = 'CNPJ:'.
    g_cgc_number = ge_kna1-stcd1.
    WRITE g_cgc_number TO l_campocnpj.
  ELSE.              "CPF
    g_cpf_cnpj_txt = 'CPF:'.
    g_cpf_number = ge_kna1-stcd2.
    WRITE g_cpf_number TO l_campocpf.
    l_campocnpj = l_campocpf.
  ENDIF.

  CLEAR g_totcred.
  LOOP AT gt_creditos INTO ge_creditos.

    ADD ge_creditos-dmbtr TO g_totcred.

  ENDLOOP.

  g_saldocred = g_totcred - ge_formpag-dmbtr.

  "Nome da Filial
  CONCATENATE ge_address-name1 '-' ge_address-sortl
    INTO g_linha01 SEPARATED BY space.

  "Endereço da Filial
  g_linha02 =  ge_address-stras.

  CONCATENATE ge_address-ort02 '-' ge_address-ort01 '-' ge_address-regio
    INTO g_linha03 SEPARATED BY space.

  ge_zfie007-filial           = g_linha01.
  ge_zfie007-end1_filial      = g_linha02.
  ge_zfie007-end2_filial      = g_linha03.

  CLEAR: gt_dados, gt_dados[].
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '-----------------------------------------' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '               MASTERCARD                ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                 CIELO                   ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '               MASTERCARD                ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '             NUMERO DO CARTAO            ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '2a VIA-ESTABELECIMENTO   AUT=XXXXXXXX    ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '9999999999999999/PDV=88888888            ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE 'DOC=666666  19/06/2018   07:30ONL-C      ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE 'VENDA A CRÉDITO                          ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE 'VALOR:     28,10                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '    TRANSAÇÃO AUTORIZADA COM SENHA       ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '           SILVA/DIEHGO R C              ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '   A0000000000000000000000000000000000   ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                Credit                   ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                              (Sitef)    ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.
  WRITE '                                         ' TO ge_dados-tdline.
  APPEND ge_dados TO gt_dados.

  l_visual = ' '.

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
      wa_output_options-tdimmed       = 'X'. "nast-dimme.
      wa_output_options-tddelete      = space.
      wa_output_options-tdnewid       = 'X'.
      wa_output_options-tdnoprev      = 'X'.
      wa_control_parameters-langu     = 'PT'.
      wa_control_parameters-no_dialog = 'X'.
      wa_control_parameters-preview   = ' '.
  ENDCASE.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZFI_RECIBO_COM_LOGO'
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
      ge_zfie007         = ge_zfie007
    TABLES
      gt_dados           = gt_dados
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ESTORNAR_CARTAO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM estornar_cartao .

  READ TABLE gt_devol INTO ge_devol WITH KEY marc = 'X'.

  IF sy-subrc NE 0 .
    "Selecionar uma linha!
    MESSAGE e034(zfi001).
  ENDIF.

  g_tabix_dev = sy-tabix.

  IF ge_devol-dcr_meio_pagamento  EQ 'CARTÃO DE CRÉDITO' OR
     ge_devol-dcr_meio_pagamento  EQ 'CARTÃO DE DÉBITO'.

    t_cartao_d_c               = ge_devol-dcr_meio_pagamento.
    ge_formpag-cartao          = ge_devol-cartao.
    ge_formpag-apr_pos         = ge_devol-apr_pos.
    ge_formpag-dmbtr           = ge_devol-dmbtr2.
    ge_formpag-data_venda      = ge_devol-bldat.
    ge_formpag-tp_pagto_cartao = ge_devol-tp_pagto_cartao.

    CALL SCREEN '9012' STARTING AT 02 02
                       ENDING AT 37 08.
    IF ok_code IS INITIAL.
      CLEAR t_vlr_recebendo.
    ENDIF.
    CLEAR ok_code.
  ELSE.
    "Não pode estornar um meio de pagamento diferente de Cartão!
    MESSAGE e035(zfi001).
  ENDIF.

  IF NOT t_dcr_meio_pagamento IS INITIAL.
    IF t_dcr_meio_pagamento  EQ 'CARTÃO DE CRÉDITO' OR
       t_dcr_meio_pagamento  EQ 'CARTÃO DE DÉBITO'.
      ge_formpag-ident_cartao       = g_ident_cartao.
    ENDIF.
    ge_formpag-dcr_meio_pagamento = t_dcr_meio_pagamento.
    ge_formpag-dmbtr              = t_vlr_recebendo.
    IF t_vlr_recebendo GT 0.
      IF t_dcr_meio_pagamento EQ 'DINHEIRO'.
        COLLECT ge_formpag INTO gt_formpag.
      ELSE.
        APPEND ge_formpag TO gt_formpag.
      ENDIF.
    ENDIF.
  ENDIF.

  CLEAR: t_vlr_tot_a_receber, t_vlr_tot_recebido, t_vlr_tot_restante, t_vlr_troco.
  LOOP AT gt_formpag INTO ge_formpag.
    ADD ge_formpag-dmbtr TO t_vlr_tot_recebido.
  ENDLOOP.

  t_vlr_tot_a_receber = ge_zfie004-netwr.
  t_vlr_tot_restante  = t_vlr_tot_a_receber - t_vlr_tot_recebido.
  IF t_vlr_tot_recebido GT t_vlr_tot_a_receber.
    t_vlr_troco        = t_vlr_tot_recebido - t_vlr_tot_a_receber.
    t_vlr_tot_restante = 0.
  ENDIF.

  DELETE gt_formpag WHERE dmbtr EQ 0.

  CLEAR: t_dcr_meio_pagamento, t_vlr_recebendo.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DOWNLOAD_ARQUIVO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_TAB[]
*&      --> P_ARQ
*&---------------------------------------------------------------------*
FORM download_arquivo TABLES gt_tab LIKE gt_result[]
                      USING  p_arq  LIKE g_file.

  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename = p_arq
      filetype = 'ASC'
    CHANGING
      data_tab = gt_tab[]
    EXCEPTIONS
      OTHERS   = 3.

  IF sy-subrc NE 0.
    CLEAR ok_code.
    "Erro no download do arquivo &
    MESSAGE e028(zfi001) WITH p_arq.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form COPIA_ARQUIVO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> G_FILE
*&      --> G_FILE_DEST
*&---------------------------------------------------------------------*
FORM copia_arquivo USING  p_arq1  LIKE g_file
                          p_arq2  LIKE g_file.

  CALL METHOD cl_gui_frontend_services=>file_copy
    EXPORTING
      source               = p_arq1
      destination          = p_arq2
      overwrite            = 'X'
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      wrong_parameter      = 3
      disk_full            = 4
      access_denied        = 5
      file_not_found       = 6
      destination_exists   = 7
      unknown_error        = 8
      path_not_found       = 9
      disk_write_protect   = 10
      drive_not_ready      = 11
      not_supported_by_gui = 12
      OTHERS               = 13.

  IF sy-subrc NE 0.
    CLEAR ok_code.
    "Erro na cópia do arquivo &
    MESSAGE e029(zfi001) WITH p_arq1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXCLUI_ARQUIVO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> G_FILE
*&      --> G_FILE_DEST
*&---------------------------------------------------------------------*
FORM exclui_arquivo  USING p_arq.

  CALL METHOD cl_gui_frontend_services=>file_delete
    EXPORTING
      filename           = p_arq
    CHANGING
      rc                 = g_rc
    EXCEPTIONS
      file_delete_failed = 1
      cntl_error         = 2
      OTHERS             = 3.

*  IF sy-subrc NE 0.
*    CLEAR ok_code.
*    "Erro na exclusão do arquivo &
*    MESSAGE e030(zfi001) WITH p_arq.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPLOAD_ARQUIVO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_TABB[]
*&      --> P_ARQ
*&---------------------------------------------------------------------*
FORM upload_arquivo TABLES gt_tab LIKE gt_retsitef[]
                    USING  p_arq  LIKE g_file.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = p_arq
      filetype                = 'ASC'
    CHANGING
      data_tab                = gt_tab[]
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.
  IF sy-subrc NE 0.
    CLEAR ok_code.
    "Erro no upload do arquivo &
    MESSAGE e031(zfi001) WITH p_arq.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXISTE_ARQUIVO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_ARQ
*&---------------------------------------------------------------------*
FORM existe_arquivo  USING p_arq.

  CLEAR g_file_exists.
  CALL METHOD cl_gui_frontend_services=>file_exist
    EXPORTING
      file                 = p_arq
    RECEIVING
      result               = g_file_exists
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      wrong_parameter      = 3
      not_supported_by_gui = 4
      OTHERS               = 5.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ENVIAR_TEF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM enviar_tef .

*** GFX - DFLC - Início 05/01/2021
  SELECT SINGLE a~stcd1
    FROM kna1 AS a
    INNER JOIN t001w AS b ON ( b~kunnr = a~kunnr )
    INTO @DATA(lv_cnpj)
    WHERE b~j_1bbranch = @ge_zfit001-bupla.

  lv_cnpj = COND #( WHEN lv_cnpj IS INITIAL
                      THEN |07965809000105|
                    ELSE lv_cnpj ).

*** GFX - DFLC - Fim 05/01/2021

  CLEAR g_cod_bandeira_sitef.

  CLEAR ge_zfit012.
  READ TABLE gt_zfit012 INTO ge_zfit012 WITH KEY bandeira = ge_formpag-cartao.

  IF sy-subrc EQ 0.
    g_cod_bandeira_sitef = ge_zfit012-cod_bandeira_sitef.
  ELSE.
    g_cod_bandeira_sitef = '00000'.
  ENDIF.

  CLEAR g_number.
  "Preencher agrupador
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = c_nr
      object                  = c_obj
    IMPORTING
      number                  = g_number
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  CLEAR: gt_result, gt_result[], ge_result.

  ge_result = '000-000 = CRT'.
  APPEND ge_result TO gt_result.
  WRITE g_number TO g_campoval.
  CONDENSE g_campoval.
  CONCATENATE '001-000 =' g_campoval INTO ge_result SEPARATED BY space.
  APPEND ge_result TO gt_result.
  WRITE t_vlr_recebendo TO g_campoval.
  CONDENSE g_campoval.
  CONCATENATE '003-000 =' g_campoval INTO ge_result SEPARATED BY space.
  APPEND ge_result TO gt_result.
  ge_result = '004-000 = 0'.
  APPEND ge_result TO gt_result.
*** GFX - DFLC - Início 05/01/2021
  ge_result = |565-008 = 1={ lv_cnpj };2=11952885000145|.
*** GFX - DFLC - Fim 05/01/2021
  APPEND ge_result TO gt_result.
  ge_result = '999-999 = 0'.
  APPEND ge_result TO gt_result.

  "Exclui arquivo C:\CLIENTE\RESP\IntPos.001
  CONCATENATE g_file_resp g_file_001 INTO g_file.
  PERFORM exclui_arquivo USING g_file.

  "Exclui arquivo C:\CLIENTE\RESP\IntPos.STS
  CONCATENATE g_file_resp g_file_sts INTO g_file.
  PERFORM exclui_arquivo USING g_file.

  "Download arquivo C:\CLIENTE\REQ\IntPos.TMP
  CONCATENATE g_file_req g_file_tmp INTO g_file.
  PERFORM download_arquivo TABLES gt_result[] USING g_file.

  "Copia arquivo C:\CLIENTE\REQ\IntPos.TMP para IntPos.001
  CONCATENATE g_file_req g_file_001 INTO g_file_dest.
  PERFORM copia_arquivo USING g_file g_file_dest.

  "Exclui arquivo C:\CLIENTE\REQ\IntPos.TMP
  PERFORM exclui_arquivo USING g_file.

  CLEAR g_cont.
  "Existe arquivo C:\CLIENTE\RESP\IntPos.001
  CONCATENATE g_file_resp g_file_001 INTO g_file.
  DO 60 TIMES.
    ADD 1 TO g_cont.
    PERFORM existe_arquivo USING g_file.
    IF g_file_exists = 'X'.
      EXIT.
    ENDIF.
    WAIT UP TO 2 SECONDS.
  ENDDO.

  CLEAR g_erro.
  IF g_file_exists IS INITIAL.
    g_erro = 'X'.
    CLEAR: gt_mess, gt_mess[].
    ge_mess-message = 'Aplicação cairá por timeout.'.
    APPEND ge_mess TO gt_mess.
  ENDIF.

  IF g_erro IS INITIAL.
    CLEAR: gt_retsitef, gt_retsitef[], ge_retsitef.

    "Upload arquivo C:\CLIENTE\RESP\IntPos.001
    CONCATENATE g_file_resp g_file_001 INTO g_file.
    PERFORM upload_arquivo TABLES gt_retsitef[] USING g_file.

    "STATUS DA TRANSAÇÃO
    CLEAR ge_retsitef.
    READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '009-000'.

    CLEAR g_erro.
    IF ge_retsitef-tipoinf EQ '0'.
      "NOME DO PRODUTO
      CLEAR ge_retsitef.
      READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '010-000'.
      g_msg010 = ge_retsitef-tipoinf.

      CLEAR ge_formpag-adquirente.
      LOOP AT gt_retsitef INTO ge_retsitef.

        IF ge_retsitef-tipoinf CS 'CIELO'.
          ge_formpag-adquirente = 'CIELO'.
          EXIT.
        ENDIF.

        IF ge_retsitef-tipoinf CS 'REDE'.
          ge_formpag-adquirente = 'REDE'.
          EXIT.
        ENDIF.

        IF ge_retsitef-tipoinf CS 'LIBERCARD'.
          ge_formpag-adquirente = 'LIBERCARD'.
          EXIT.
        ENDIF.

        IF ge_retsitef-tipoinf CS 'CREDISHOP'.
          ge_formpag-adquirente = 'CREDISHOP'.
          EXIT.
        ENDIF.

        IF ge_retsitef-tipoinf CS 'FORTBRASIL'.
          ge_formpag-adquirente = 'FORTBRASIL'.
          EXIT.
        ENDIF.

      ENDLOOP.

      IF ge_formpag-adquirente IS INITIAL.
        ge_formpag-adquirente = 'CIELO'. "GFX - FI - SFSS - DEVK915041 - 01/11/2019
      ENDIF.

      "BIN DO CARTÃO
      CLEAR ge_retsitef.
      READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '010-004'.
      g_msg010_4 = ge_retsitef-tipoinf.
      ge_formpag-bin        = g_msg010_4.

      "QUATRO  ULTIMOS DIGITOS
      CLEAR ge_retsitef.
      READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '010-005'.
      g_msg010_5 = ge_retsitef-tipoinf.
      ge_formpag-qtrodigitos = g_msg010_5.

      "CÓDIGO DA BANDEIRA
      CLEAR ge_retsitef.
      READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '010-003'.
      g_msg010_3 = ge_retsitef-tipoinf.

      "CÓDIGO DA TRANSAÇÃO
      CLEAR ge_retsitef.
      READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '011-000'.
      g_msg011 = ge_retsitef-tipoinf.

      "NSU HOST
*** GFX - SFSS - DEVK915591 - 10.12.2019 - Início
      IF ge_formpag-adquirente = 'REDE'.

        CLEAR ge_retsitef.
        READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '012-000'.

        g_msg012 = ge_retsitef-tipoinf.
        ge_formpag-nsu_host = g_msg012.
        ge_formpag-data_venda = sy-datum.

      ELSE.

        "NÚMERO DA TRANSAÇÃO - NSU
        CLEAR ge_retsitef.
        READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '043-000'.
        IF sy-subrc NE 0.
          CLEAR ge_retsitef.
          READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '013-000'.
        ENDIF.
        IF sy-subrc NE 0.
          CLEAR ge_retsitef.
          READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '012-000'.
        ENDIF.

        g_msg012 = ge_retsitef-tipoinf.
        ge_formpag-apr_pos    = g_msg012.
        ge_formpag-data_venda = sy-datum.
        ge_formpag-num_transacao_nsu  = g_msg012.

      ENDIF.
*** GFX - SFSS - DEVK915591 - 10.12.2019 - Fim

      "CÓDIGO DE AUTORIZAÇÃO DA TRANSAÇÃO
      CLEAR ge_retsitef.
      READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '013-000'.
      g_msg013 = ge_retsitef-tipoinf.
      ge_formpag-cod_autorizacao    = g_msg013.

      "TIPO DO PARCELAMENTO
      CLEAR ge_retsitef.
      READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '017-000'.
      g_msg017 = ge_retsitef-tipoinf.

      "QUANTIDADE DE PARCELAS DA TRANSAÇÃO
      CLEAR ge_retsitef.
      READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '018-000'.
      g_msg018 = ge_retsitef-tipoinf.

      IF g_msg018 IS INITIAL.
        ge_formpag-parcelas   = '1'.
      ELSE.
        ge_formpag-parcelas   = g_msg018.
      ENDIF.

      "FINALIZAÇÃO
      CLEAR ge_retsitef.
      READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '027-000'.
      g_msg027 = ge_retsitef-tipoinf.

      "MARCA DO RECORTE
      CLEAR ge_retsitef.
      READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '028-001'.
      g_msg028 = ge_retsitef-tipoinf.

      CLEAR: g_recorte1, g_recorte2, g_recorte3.
      SPLIT g_msg028
        AT ':'
        INTO g_recorte1
             g_recorte2
             g_recorte3.

      "DATA DE VALIDADE DO CARTÃO
      CLEAR ge_retsitef.
      READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '300-001'.
      g_msg300_1 = ge_retsitef-tipoinf.
      ge_formpag-dtexpiracao = g_msg300_1.

      "NOME DONO CARTAO
      CLEAR ge_retsitef.
      READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '300-002'.
      g_msg300_2 = ge_retsitef-tipoinf.
      ge_formpag-donocartao = g_msg300_2.

      "Verificar se o tipo de cartão é o mesmo que o usuário colocou no PINPAD
      CLEAR g_erro.
      IF ge_formpag-modalidade = 'C'.
        IF g_msg011 NE '56'.
          IF g_msg010_3 IS INITIAL.
            "erro - cartao não é de crédito
            g_erro = 'X'.
          ENDIF.
        ENDIF.
      ELSEIF ge_formpag-modalidade = 'D'.
        IF g_msg011 NE '32'.
          IF NOT g_msg010_3 IS INITIAL.
            "erro - cartao não é de débito
            g_erro = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

      IF g_erro = 'X'.
        CLEAR: gt_mess, gt_mess[].
        ge_mess-message = 'Modalidade do Cartão informado no Caixa diferente do PINPAD!'.
        APPEND ge_mess TO gt_mess.
      ENDIF.

      "Verificar se o tipo de cartão é o mesmo que o usuário colocou no PINPAD
      IF g_erro IS INITIAL.
        IF NOT g_msg010_3 IS INITIAL.
          CLEAR ge_zfit012.
          READ TABLE gt_zfit012 INTO ge_zfit012 WITH KEY cod_bandeira_sitef = g_msg010_3.
          IF sy-subrc EQ 0.
            IF ge_formpag-cartao NE ge_zfit012-bandeira.
              g_erro = 'X'.
              CLEAR: gt_mess, gt_mess[].
              ge_mess-message = 'Cartão informado no Caixa diferente do PINPAD!'.
              APPEND ge_mess TO gt_mess.
            ELSE.
              ge_formpag-cartao_c2d_ok = 'X'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      CLEAR: gt_result, gt_result[], ge_result.
      IF g_erro IS INITIAL.
        WRITE g_number TO g_campoval.
        CONDENSE g_campoval.
        ge_cartao-ident_cartao = g_ident_cartao.
        ge_cartao-number       = g_campoval.
        ge_cartao-msg010       = g_msg010.
        ge_cartao-msg012       = g_msg012.
        ge_cartao-msg027       = g_msg027.
        APPEND ge_cartao TO gt_cartao.
*        ge_result = '000-000 = CNF'.
        ge_formpag-cartao_tef = g_msg010.

        "Inclui os dados da primeira via do cartão na tabela interna
        DELETE gt_retsitef WHERE codcampo(3) NE '029'.

        g_recnum = g_recorte1.
        CLEAR g_tabix.
        LOOP AT gt_retsitef INTO ge_retsitef.

          ADD 1 TO g_tabix.
          IF g_tabix GT g_recnum.
            EXIT.
          ENDIF.

          TRANSLATE ge_retsitef-tipoinf USING '" '.

          ge_imprcartao-ident_cartao = g_ident_cartao.
          ge_imprcartao-via          = '1'.
          ge_imprcartao-linha        = ge_retsitef-tipoinf.

          APPEND ge_imprcartao TO gt_imprcartao.

          DELETE gt_retsitef.

        ENDLOOP.

        "Inclui os dados da segunda via do cartão na tabela interna

        IF g_recorte2 IS INITIAL.
          g_recnum = 999.
        ELSE.
          g_recnum = g_recorte2.
        ENDIF.

        CLEAR g_tabix.
        LOOP AT gt_retsitef INTO ge_retsitef.

          ADD 1 TO g_tabix.
          IF g_tabix GT g_recnum.
            EXIT.
          ENDIF.

          TRANSLATE ge_retsitef-tipoinf USING '" '.

          ge_imprcartao-ident_cartao = g_ident_cartao.
          ge_imprcartao-via          = '2'.
          ge_imprcartao-linha        = ge_retsitef-tipoinf.

          APPEND ge_imprcartao TO gt_imprcartao.

          DELETE gt_retsitef.

        ENDLOOP.

      ELSE.
        ge_result = '000-000 = NCN'.
        APPEND ge_result TO gt_result.
        WRITE g_number TO g_campoval.
        CONDENSE g_campoval.
        CONCATENATE '001-000 =' g_campoval INTO ge_result SEPARATED BY space.
        APPEND ge_result TO gt_result.
        CONCATENATE '010-000 =' g_msg010 INTO ge_result SEPARATED BY space.
        APPEND ge_result TO gt_result.
        CONCATENATE '012-000 =' g_msg012 INTO ge_result SEPARATED BY space.
        APPEND ge_result TO gt_result.
        CONCATENATE '027-000 =' g_msg027 INTO ge_result SEPARATED BY space.
        APPEND ge_result TO gt_result.
        ge_result = '999-999 = 0'.
        APPEND ge_result TO gt_result.

        "Exclui arquivo C:\CLIENTE\RESP\IntPos.001
        CONCATENATE g_file_resp g_file_001 INTO g_file.
        PERFORM exclui_arquivo USING g_file.

        "Exclui arquivo C:\CLIENTE\RESP\IntPos.STS
        CONCATENATE g_file_resp g_file_sts INTO g_file.
        PERFORM exclui_arquivo USING g_file.

        "Download arquivo C:\CLIENTE\REQ\IntPos.TMP
        CONCATENATE g_file_req g_file_tmp INTO g_file.
        PERFORM download_arquivo TABLES gt_result[] USING g_file.

        "Copia arquivo C:\CLIENTE\REQ\IntPos.TMP para IntPos.001
        CONCATENATE g_file_req g_file_001 INTO g_file_dest.
        PERFORM copia_arquivo USING g_file g_file_dest.

        "Exclui arquivo C:\CLIENTE\RESP\IntPos.TMP
        PERFORM exclui_arquivo USING g_file.

        CLEAR g_cont.
        "Existe arquivo C:\CLIENTE\RESP\IntPos.STS
        CONCATENATE g_file_resp g_file_sts INTO g_file.
        DO 10 TIMES.
          ADD 1 TO g_cont.
          PERFORM existe_arquivo USING g_file.
          IF g_file_exists = 'X'.
            EXIT.
          ENDIF.
          WAIT UP TO 1 SECONDS.
        ENDDO.

        IF g_file_exists = 'X'.
          IF g_erro IS INITIAL .
*            PERFORM imprimir_recibo_tef.
*            ge_formpag-cartao_tef = g_msg010.
          ELSE.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      CLEAR ge_retsitef.
      READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '030-000'.

      CLEAR: gt_mess, gt_mess[].
      ge_mess-message = ge_retsitef-tipoinf.
      APPEND ge_mess TO gt_mess.

      CLEAR ok_code.
      g_erro = 'X'.
    ENDIF.
  ENDIF.
*  ELSE.
*    CLEAR ok_code.
*  ENDIF.

  IF g_erro = 'X'.
    CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
      EXPORTING
        endpos_col   = 80
        endpos_row   = 10
        startpos_col = 1
        startpos_row = 1
        titletext    = 'Erro'
      IMPORTING
        choise       = g_tabix
      TABLES
        valuetab     = gt_mess
      EXCEPTIONS
        break_off    = 1
        OTHERS       = 2.

    CLEAR ok_code.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form IMPRIMIR_RECIBO_TEF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM imprimir_recibo_tef .

  DATA: formname              TYPE tdsfname,
        fm_name               TYPE rs38l_fnam,
        wa_control_parameters TYPE ssfctrlop,         "Estrutura de controle (smartforms)
        wa_output_options     TYPE ssfcompop.         "Estrutura de controle (smartforms)

  DATA: gc_ucomm LIKE sy-ucomm.

  DATA: l_visual,
        l_name_text      TYPE adrp-name_text,
        l_campoaux1(132) TYPE c,
        l_campoaux2(132) TYPE c,
        l_campoval(25)   TYPE c,
        l_campocnpj(18)  TYPE c,
        l_tipo(15)       TYPE c.

  CLEAR ge_zfie007.

  CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
    EXPORTING
      branch            = t_bupla
      bukrs             = t_bukrs
    IMPORTING
      address           = ge_address
      branch_data       = ge_branch_data
      cgc_number        = g_cgc_number2
      address1          = ge_address1
    EXCEPTIONS
      branch_not_found  = 1
      address_not_found = 2
      company_not_found = 3
      OTHERS            = 4.

  CLEAR l_name_text.
*  SELECT SINGLE name_text INTO l_name_text
*    FROM usr21
*    INNER JOIN adrp ON
*    usr21~persnumber = adrp~persnumber
*    WHERE bname EQ sy-uname.
  l_name_text = sy-uname.

  "Nome da Filial
  CONCATENATE ge_address-name1 '-' ge_address-sortl
    INTO g_linha01 SEPARATED BY space.

  "Endereço da Filial
  g_linha02 =  ge_address-stras.

  CONCATENATE ge_address-ort02 '-' ge_address-ort01 '-' ge_address-regio
    INTO g_linha03 SEPARATED BY space.

  ge_zfie007-filial           = g_linha01.
  ge_zfie007-end1_filial      = g_linha02.
  ge_zfie007-end2_filial      = g_linha03.

  DELETE gt_retsitef WHERE codcampo(3) NE '029'.

  g_recnum = g_recorte1.
  CLEAR g_tabix.
  CLEAR: gt_dados, gt_dados[].
  LOOP AT gt_retsitef INTO ge_retsitef.

    ADD 1 TO g_tabix.
    IF g_tabix GT g_recnum.
      EXIT.
    ENDIF.

    TRANSLATE ge_retsitef-tipoinf USING '" '.
    ge_dados-tdline = ge_retsitef-tipoinf.

    APPEND ge_dados TO gt_dados.

    DELETE gt_retsitef.

  ENDLOOP.

  l_visual = ' '.

  IF l_visual EQ 'X'.
    gc_ucomm = 'PREVOUTPUT'.
  ELSE.
    gc_ucomm = 'SAIDA_IMPR'.
  ENDIF.

  CLEAR: wa_control_parameters, wa_output_options.

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
      wa_output_options-tdimmed       = 'X'. "nast-dimme.
      wa_output_options-tddelete      = space.
      wa_output_options-tdnewid       = 'X'.
      wa_output_options-tdnoprev      = 'X'.
      wa_control_parameters-langu     = 'PT'.
      wa_control_parameters-no_dialog = 'X'.
      wa_control_parameters-preview   = ''.
  ENDCASE.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZFI_RECIBO_COM_LOGO'
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
      ge_zfie007         = ge_zfie007
    TABLES
      gt_dados           = gt_dados
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF g_recorte2 IS INITIAL.
    g_recnum = 999.
  ELSE.
    g_recnum = g_recorte2.
  ENDIF.

  CLEAR g_tabix.
  CLEAR: gt_dados, gt_dados[].
  LOOP AT gt_retsitef INTO ge_retsitef.

    ADD 1 TO g_tabix.
    IF g_tabix GT g_recnum.
      EXIT.
    ENDIF.

    TRANSLATE ge_retsitef-tipoinf USING '" '.
    ge_dados-tdline = ge_retsitef-tipoinf.

    APPEND ge_dados TO gt_dados.

    DELETE gt_retsitef.

  ENDLOOP.

  CALL FUNCTION fm_name
    EXPORTING
      control_parameters = wa_control_parameters
      output_options     = wa_output_options
      ge_zfie007         = ge_zfie007
    TABLES
      gt_dados           = gt_dados
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form IMPRIMIR_RECIBO_TEF_NOVO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM imprimir_recibo_tef_novo .

  DATA: formname              TYPE tdsfname,
        fm_name               TYPE rs38l_fnam,
        wa_control_parameters TYPE ssfctrlop,         "Estrutura de controle (smartforms)
        wa_output_options     TYPE ssfcompop.         "Estrutura de controle (smartforms)

  DATA: gc_ucomm LIKE sy-ucomm.

  DATA: l_visual,
        l_name_text      TYPE adrp-name_text,
        l_campoaux1(132) TYPE c,
        l_campoaux2(132) TYPE c,
        l_campoval(25)   TYPE c,
        l_campocnpj(18)  TYPE c,
        l_tipo(15)       TYPE c.

  CLEAR ge_zfie007.

  CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
    EXPORTING
      branch            = t_bupla
      bukrs             = t_bukrs
    IMPORTING
      address           = ge_address
      branch_data       = ge_branch_data
      cgc_number        = g_cgc_number2
      address1          = ge_address1
    EXCEPTIONS
      branch_not_found  = 1
      address_not_found = 2
      company_not_found = 3
      OTHERS            = 4.

  CLEAR l_name_text.
*  SELECT SINGLE name_text INTO l_name_text
*    FROM usr21
*    INNER JOIN adrp ON
*    usr21~persnumber = adrp~persnumber
*    WHERE bname EQ sy-uname.
  l_name_text = sy-uname.

  "Nome da Filial
  CONCATENATE ge_address-name1 '-' ge_address-sortl
    INTO g_linha01 SEPARATED BY space.

  "Endereço da Filial
  g_linha02 =  ge_address-stras.

  CONCATENATE ge_address-ort02 '-' ge_address-ort01 '-' ge_address-regio
    INTO g_linha03 SEPARATED BY space.

  ge_zfie007-filial           = g_linha01.
  ge_zfie007-end1_filial      = g_linha02.
  ge_zfie007-end2_filial      = g_linha03.

  LOOP AT gt_cartao INTO ge_cartao.

    CLEAR: gt_dados, gt_dados[].
    LOOP AT gt_imprcartao INTO ge_imprcartao WHERE ident_cartao EQ ge_cartao-ident_cartao
                                  AND via          EQ '1'.

      ge_dados-tdline = ge_imprcartao-linha.
      APPEND ge_dados TO gt_dados.

    ENDLOOP.

    l_visual = ' '.

    IF l_visual EQ 'X'.
      gc_ucomm = 'PREVOUTPUT'.
    ELSE.
      gc_ucomm = 'SAIDA_IMPR'.
    ENDIF.

    CLEAR: wa_control_parameters, wa_output_options.

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
        wa_output_options-tdimmed       = 'X'. "nast-dimme.
        wa_output_options-tddelete      = space.
        wa_output_options-tdnewid       = 'X'.
        wa_output_options-tdnoprev      = 'X'.
        wa_control_parameters-langu     = 'PT'.
        wa_control_parameters-no_dialog = 'X'.
        wa_control_parameters-preview   = ''.
    ENDCASE.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = 'ZFI_RECIBO_COM_LOGO'
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
        ge_zfie007         = ge_zfie007
      TABLES
        gt_dados           = gt_dados
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.

    CLEAR: gt_dados, gt_dados[].
    LOOP AT gt_imprcartao INTO ge_imprcartao WHERE ident_cartao EQ ge_cartao-ident_cartao
                                  AND via          EQ '2'.

      ge_dados-tdline = ge_imprcartao-linha.
      APPEND ge_dados TO gt_dados.

    ENDLOOP.

    CALL FUNCTION fm_name
      EXPORTING
        control_parameters = wa_control_parameters
        output_options     = wa_output_options
        ge_zfie007         = ge_zfie007
      TABLES
        gt_dados           = gt_dados
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CANC_CARTAO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM canc_cartao .

  LOOP AT gt_formpag INTO ge_formpag WHERE tp_pagto_cartao EQ 'TEF'.
    IF ge_formpag-dcr_meio_pagamento  EQ 'CARTÃO DE CRÉDITO'.
      PERFORM canc_tef.
    ELSEIF ge_formpag-dcr_meio_pagamento  EQ 'CARTÃO DE DÉBITO'.
      PERFORM canc_tef.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CANC_TEF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM canc_tef .

  CLEAR: g_tef_ok, g_est_ok.
  CLEAR g_number.
  "Preencher agrupador
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = c_nr
      object                  = c_obj
    IMPORTING
      number                  = g_number
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  CLEAR: gt_result, gt_result[], ge_result.

  ge_result = '000-000 = CNC'.
  APPEND ge_result TO gt_result.
  WRITE g_number TO g_campoval.
  CONDENSE g_campoval.
  CONCATENATE '001-000 =' g_campoval INTO ge_result SEPARATED BY space.
  APPEND ge_result TO gt_result.
  WRITE ge_formpag-dmbtr TO g_campoval.
  CONDENSE g_campoval.
  CONCATENATE '003-000 =' g_campoval INTO ge_result SEPARATED BY space.
  APPEND ge_result TO gt_result.
  WRITE ge_formpag-apr_pos TO g_campoval.
  CONDENSE g_campoval.
  CONCATENATE '012-000 =' g_campoval INTO ge_result SEPARATED BY space.
  APPEND ge_result TO gt_result.
  CONCATENATE ge_formpag-data_venda+6(2) ge_formpag-data_venda+4(2) ge_formpag-data_venda(4)
    INTO g_campoval.
  CONCATENATE '022-000 =' g_campoval INTO ge_result SEPARATED BY space.
  APPEND ge_result TO gt_result.
  ge_result = '999-999 = 0'.
  APPEND ge_result TO gt_result.

  "Exclui arquivo C:\CLIENTE\RESP\IntPos.001
  CONCATENATE g_file_resp g_file_001 INTO g_file.
  PERFORM exclui_arquivo USING g_file.

  "Exclui arquivo C:\CLIENTE\RESP\IntPos.STS
  CONCATENATE g_file_resp g_file_sts INTO g_file.
  PERFORM exclui_arquivo USING g_file.

  "Download arquivo C:\CLIENTE\REQ\IntPos.TMP
  CONCATENATE g_file_req g_file_tmp INTO g_file.
  PERFORM download_arquivo TABLES gt_result[] USING g_file.

  "Copia arquivo C:\CLIENTE\REQ\IntPos.TMP para IntPos.001
  CONCATENATE g_file_req g_file_001 INTO g_file_dest.
  PERFORM copia_arquivo USING g_file g_file_dest.

  "Exclui arquivo C:\CLIENTE\REQ\IntPos.TMP
  PERFORM exclui_arquivo USING g_file.

  "Download arquivo C:\CLIENTE\RESP\IntPosCRT.TXT
  CONCATENATE g_file_resp 'IntPosCRT.TXT' INTO g_file.
  PERFORM download_arquivo TABLES gt_result[] USING g_file.

  CLEAR: g_cont.
  "Existe arquivo C:\CLIENTE\RESP\IntPos.001
  CONCATENATE g_file_resp g_file_001 INTO g_file.
  DO 60 TIMES.
    ADD 1 TO g_cont.
    PERFORM existe_arquivo USING g_file.
    IF g_file_exists = 'X'.
      EXIT.
    ENDIF.
    WAIT UP TO 2 SECONDS.
  ENDDO.

  CLEAR: gt_retsitef, gt_retsitef[], ge_retsitef.

  "Upload arquivo C:\CLIENTE\RESP\IntPos.001
  CONCATENATE g_file_resp g_file_001 INTO g_file.
  PERFORM upload_arquivo TABLES gt_retsitef[] USING g_file.

  CLEAR ge_retsitef.
  READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '009-000'.

  IF ge_retsitef-tipoinf EQ '0'.
    CLEAR ge_retsitef.
    READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '010-000'.
    g_msg010 = ge_retsitef-tipoinf.

    CLEAR ge_retsitef.
    READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '012-000'.
    g_msg012 = ge_retsitef-tipoinf.

    CLEAR ge_retsitef.
    READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '013-000'.
    g_msg013 = ge_retsitef-tipoinf.

    CLEAR ge_retsitef.
    READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '027-000'.
    g_msg027 = ge_retsitef-tipoinf.

    CLEAR: gt_result, gt_result[], ge_result.

    ge_result = '000-000 = CNF'.
    APPEND ge_result TO gt_result.
    WRITE g_number TO g_campoval.
    CONDENSE g_campoval.
    CONCATENATE '001-000 =' g_campoval INTO ge_result SEPARATED BY space.
    APPEND ge_result TO gt_result.
    CONCATENATE '010-000 =' g_msg010 INTO ge_result SEPARATED BY space.
    APPEND ge_result TO gt_result.
    CONCATENATE '012-000 =' g_msg012 INTO ge_result SEPARATED BY space.
    APPEND ge_result TO gt_result.
    CONCATENATE '027-000 =' g_msg027 INTO ge_result SEPARATED BY space.
    APPEND ge_result TO gt_result.
    ge_result = '999-999 = 0'.
    APPEND ge_result TO gt_result.

    "Exclui arquivo C:\CLIENTE\RESP\IntPos.001
    CONCATENATE g_file_resp g_file_001 INTO g_file.
    PERFORM exclui_arquivo USING g_file.

    "Exclui arquivo C:\CLIENTE\RESP\IntPos.STS
    CONCATENATE g_file_resp g_file_sts INTO g_file.
    PERFORM exclui_arquivo USING g_file.

    "Download arquivo C:\CLIENTE\REQ\IntPos.TMP
    CONCATENATE g_file_req g_file_tmp INTO g_file.
    PERFORM download_arquivo TABLES gt_result[] USING g_file.

    "Copia arquivo C:\CLIENTE\REQ\IntPos.TMP para IntPos.001
    CONCATENATE g_file_req g_file_001 INTO g_file_dest.
    PERFORM copia_arquivo USING g_file g_file_dest.

    "Exclui arquivo C:\CLIENTE\RESP\IntPos.TMP
    PERFORM exclui_arquivo USING g_file.

    DATA: l_cont TYPE sy-tabix.
    CLEAR l_cont.
    "Existe arquivo C:\CLIENTE\RESP\IntPos.STS
    CONCATENATE g_file_resp g_file_sts INTO g_file.
    DO 10 TIMES.
      ADD 1 TO l_cont.
      PERFORM existe_arquivo USING g_file.
      IF g_file_exists = 'X'.
        EXIT.
      ENDIF.
      WAIT UP TO 1 SECONDS.
    ENDDO.

    IF g_file_exists = 'X'.
      g_tef_ok = 'X'.
      g_est_ok = 'X'.
      PERFORM imprimir_recibo_tef.
    ENDIF.
  ELSE.
    CLEAR ge_retsitef.
    READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '030-000'.

    CLEAR: gt_mess, gt_mess[].
    ge_mess-message = ge_retsitef-tipoinf.
    APPEND ge_mess TO gt_mess.

    CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
      EXPORTING
        endpos_col   = 80
        endpos_row   = 10
        startpos_col = 1
        startpos_row = 1
        titletext    = 'Erro'
      IMPORTING
        choise       = g_tabix
      TABLES
        valuetab     = gt_mess
      EXCEPTIONS
        break_off    = 1
        OTHERS       = 2.
    CLEAR ok_code.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form INTEGRACAO_MFE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM integracao_mfe.

  DATA lo_mfe TYPE REF TO zcl_mfex.

  DATA: l_cfe      TYPE string,
        l_mensagem TYPE string,
        l_cont     TYPE sy-tabix,
        l_contitem TYPE zmfetf001-item.

  CLEAR l_cont.
  LOOP AT gt_formpag INTO ge_formpag WHERE dcr_meio_pagamento NE 'TROCO'.
    ADD 1 TO l_cont.
  ENDLOOP.

  CLEAR: gt_param, gt_param[], l_contitem.
  LOOP AT gt_formpag INTO ge_formpag.

    ADD 1 TO l_contitem.

    CLEAR ge_param.
    ge_param-docnum       = g_docnum.
    ge_param-codaut       = ge_formpag-apr_pos.
    ge_param-bin          = ge_formpag-bin.
    ge_param-donocartao   = ge_formpag-donocartao.
    ge_param-dtexpiracao  = ge_formpag-dtexpiracao.
    ge_param-adquirente   = ge_formpag-adquirente.
    ge_param-parcelas     = ge_formpag-parcelas.
    ge_param-codpgto      = ge_formpag-apr_pos.
    ge_param-valor_total  = ge_formpag-dmbtr.
    ge_param-donocartao   = ge_zfie004-name2.

    CASE ge_formpag-dcr_meio_pagamento.
      WHEN 'CARTÃO DE CRÉDITO'.
        ge_param-codmp        = '03'.
        ge_param-forma        = ge_formpag-tp_pagto_cartao.
      WHEN 'CARTÃO DE DÉBITO'.
        ge_param-codmp        = '04'.
        ge_param-forma        = ge_formpag-tp_pagto_cartao.
      WHEN 'CHEQUE'.
        ge_param-codmp        = '02'.
        ge_param-forma        = 'OUT'.
      WHEN 'DINHEIRO'.
        ge_param-codmp        = '01'.
        ge_param-forma        = 'OUT'.
      WHEN 'CRÉDITO DO CLIENTE'.
        ge_param-codmp        = '05'.
        ge_param-forma        = 'OUT'.
      WHEN OTHERS.
        ge_param-codmp        = '99'.
        ge_param-forma        = 'OUT'.
    ENDCASE.

    ge_param-qtrodigitos  = ge_formpag-qtrodigitos.
    ge_param-caixa        = t_caixa.
    ge_param-bukrs        = t_bukrs.
    ge_param-bupla        = t_bupla.
    ge_param-codestab     = ge_formpag-estabelecimento.
    ge_param-serial       = ge_formpag-serial.
    ge_param-bandeira     = ge_formpag-cartao.
    ge_param-nsu          = ge_formpag-num_transacao_nsu.
    ge_param-item         = l_contitem.
*    ge_param-item         = ge_formpag-ident_cartao.

    IF l_cont GT 1.
      ge_param-multi_pgto   = 'X'.
    ELSE.
      ge_param-multi_pgto   = ''.
    ENDIF.
    ge_param-data_cri     = sy-datum.
    ge_param-hora_cri     = sy-uzeit.

    APPEND ge_param TO gt_param.

  ENDLOOP.

  TRY .
      IF lo_mfe IS NOT BOUND.
        CREATE OBJECT lo_mfe.
      ENDIF.

      CALL METHOD lo_mfe->emite_cf
        EXPORTING
          it_f001     = gt_param    " Cupom fiscal - Ctg. de tabela Dados para Registro do CF
        IMPORTING
          ev_cfe      = l_cfe       " Cupom fiscal autênticado
          ev_mensagem = l_mensagem. " Mensagem de retorno

      IF l_cfe IS INITIAL. "Erro
        MESSAGE l_mensagem TYPE 'I'.
      ELSE.
        MESSAGE l_mensagem TYPE 'S'.
      ENDIF.

    CATCH cx_root.

  ENDTRY.

*** GFX - JBRS - DEVK922833 - Início - 06/09/2021
*  IF ge_tmfgt-mfrgr = 'Z03'.

  SELECT SINGLE acckey
    FROM zmfetf000
    INTO @DATA(lv_chave)
    WHERE docnum = @g_docnum
      AND procmt = '4'.

  IF sy-subrc = 0.

    DATA(lo_sistem_entregas) = NEW zcl_intr_sistema_entregas( iv_docnum = g_docnum
                                                              iv_werks  = g_werks
                                                              iv_chave  = CONV #( lv_chave ) ).

    lo_sistem_entregas->process_xml_download(
      EXPORTING
        iv_emitido_modelo_59_sap = abap_true " Indicação emitido pela SAP modelos de cupons fiscais 59
    ).

  ENDIF.

*  ENDIF.
*** GFX - JBRS - DEVK922833 - Fim - 06/09/2021

  lv_xml = l_cfe.
  IF lv_xml IS NOT INITIAL.
    PERFORM verifica_xml.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form INTEGRACAO_MFE_FI
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM integracao_mfe_fi.

  DATA lo_mfe TYPE REF TO zcl_mfex.

  DATA: l_cfe      TYPE string,
        l_mensagem TYPE string.

  CLEAR: gt_param, gt_param[].

  CLEAR ge_param.
  ge_param-docnum       = g_docnum.
  ge_param-codaut       = ''.
  ge_param-bin          = ''.
  ge_param-donocartao   = ''.
  ge_param-dtexpiracao  = ''.
  ge_param-adquirente   = ''.
  ge_param-parcelas     = ''.
  ge_param-codpgto      = ''.
  ge_param-valor_total  = ge_zfie004-netwr.
  ge_param-codmp        = '99'.
  ge_param-forma        = 'OUT'.
  ge_param-qtrodigitos  = ''.
  ge_param-caixa        = t_caixa.
  ge_param-bukrs        = t_bukrs.
  ge_param-bupla        = t_bupla.
  ge_param-codestab     = ''.
  ge_param-serial       = ''.
  ge_param-bandeira     = ''.
  ge_param-nsu          = ''.
  ge_param-multi_pgto   = ''.
  ge_param-data_cri     = sy-datum.
  ge_param-hora_cri     = sy-uzeit.

  APPEND ge_param TO gt_param.

  TRY .
      IF lo_mfe IS NOT BOUND.
        CREATE OBJECT lo_mfe.
      ENDIF.

      CALL METHOD lo_mfe->emite_cf
        EXPORTING
          it_f001     = gt_param                 " Cupom fiscal - Ctg. de tabela Dados para Registro do CF
        IMPORTING
          ev_cfe      = l_cfe                      " Cupom fiscal autênticado
          ev_mensagem = l_mensagem.                 " Mensagem de retorno

      MESSAGE l_mensagem TYPE 'I'.

    CATCH cx_root.

  ENDTRY.

  lv_xml = l_cfe.
  IF lv_xml IS NOT INITIAL.
    PERFORM verifica_xml.
  ENDIF.

ENDFORM.

FORM verifica_xml .

  DATA: lv_objkey  TYPE na_objkey.

  CLEAR g_value.
  CALL FUNCTION 'ZMFEFC_GET_XPATH_VALUE'
    EXPORTING
      iv_xml        = lv_xml
      iv_expression = '//ide/cNF'
    IMPORTING
      ev_value      = g_value.
  g_valuecnf    = g_value.

  CLEAR g_value.
  CALL FUNCTION 'ZMFEFC_GET_XPATH_VALUE'
    EXPORTING
      iv_xml        = lv_xml
      iv_expression = '//ide/assinaturaQRCODE'
    IMPORTING
      ev_value      = g_value.
  g_valueqrcode = g_value.

  CLEAR g_value.
  CALL FUNCTION 'ZMFEFC_GET_XPATH_VALUE'
    EXPORTING
      iv_xml        = lv_xml
      iv_expression = '//infCFe[@Id]'
    IMPORTING
      ev_value      = g_value.
  g_valueid     = g_value.

  CLEAR g_value.
  CALL FUNCTION 'ZMFEFC_GET_XPATH_VALUE'
    EXPORTING
      iv_xml        = lv_xml
      iv_expression = '//ide/nserieSAT'
    IMPORTING
      ev_value      = g_value.
  g_nseriesat     = g_value.

  UPDATE zfit006
    SET docnum    = g_docnum
        nfenum    = g_nfenum
        acckey    = g_valueid
        qrcode    = g_valueqrcode
        nseriesat = g_nseriesat
        xml       = lv_xml
    WHERE vbeln = ge_zfie004-vgbel.

  lv_objkey = g_docnum.

  SUBMIT rsnast00
    WITH s_kappl EQ 'NF'
    WITH s_objky EQ lv_objkey SIGN 'I'
     AND RETURN.


ENDFORM.

FORM print_xpath USING lv_expression TYPE string
                       lo_xslt       TYPE REF TO cl_xslt_processor
                       lv_value      TYPE string.

  DATA: lo_nodes        TYPE REF TO if_ixml_node_collection,
        lo_node         TYPE REF TO if_ixml_node,
        lo_iterator     TYPE REF TO if_ixml_node_iterator,
        lv_nodes_length TYPE i.

  lo_xslt->set_expression( expression = lv_expression ).
  lo_xslt->run( '' ).

* Os próximos comandos são usados para fazer um loop no
* resultado e mostrar em tela seus valores.
  lo_nodes = lo_xslt->get_nodes( ).
  lo_iterator = lo_nodes->create_iterator( ).
  lv_nodes_length = lo_nodes->get_length( ).

  DO lv_nodes_length TIMES.
    lo_node = lo_iterator->get_next( ).
    lv_value = lo_node->get_value( ).
  ENDDO.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form BUSCA_DADOS_NF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM busca_dados_nf .

  CLEAR ge_vbrp.
  SELECT SINGLE * INTO ge_vbrp
    FROM vbrp
    WHERE vbeln EQ g_fatura.

  CLEAR: g_docnum,
         g_nfenum,
         g_regio,
         g_nfyear,
         g_nfmonth,
         g_stcd1,
         g_model,
         g_serie,
         g_nfnum9,
         g_docnum9,
         g_cdv.
  SELECT SINGLE
    j_1bnfdoc~docnum
    j_1bnfdoc~nfenum
    j_1bnfe_active~regio
    j_1bnfe_active~nfyear
    j_1bnfe_active~nfmonth
    j_1bnfe_active~stcd1
    j_1bnfe_active~model
    j_1bnfe_active~serie
    j_1bnfe_active~nfnum9
    j_1bnfe_active~docnum9
    j_1bnfe_active~cdv
    INTO (g_docnum,
          g_nfenum,
          g_regio,
          g_nfyear,
          g_nfmonth,
          g_stcd1,
          g_model,
          g_serie,
          g_nfnum9,
          g_docnum9,
          g_cdv)
    FROM j_1bnfdoc
    INNER JOIN j_1bnflin      ON j_1bnflin~docnum = j_1bnfdoc~docnum
    INNER JOIN j_1bnfe_active ON j_1bnfe_active~docnum = j_1bnfdoc~docnum
    WHERE j_1bnflin~reftyp EQ 'BI'
      AND j_1bnflin~refkey EQ ge_vbrp-vbeln.
*      AND j_1bnflin~refitm EQ ge_vbrp-posnr.

  CHECK sy-subrc IS INITIAL.    "SFSS - 22/04/19

  UPDATE zfit006
    SET docnum    = g_docnum
        nfenum    = g_nfenum
        acckey    = g_valueid
        qrcode    = g_valueqrcode
        nseriesat = g_nseriesat
        xml       = lv_xml
    WHERE vbeln = ge_zfie004-vgbel.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADM_PINPAD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM adm_pinpad .

  CLEAR g_number.
  "Preencher agrupador
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = c_nr
      object                  = c_obj
    IMPORTING
      number                  = g_number
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  CLEAR: gt_result, gt_result[], ge_result.

  ge_result = '000-000 = ADM'.
  APPEND ge_result TO gt_result.
  WRITE g_number TO g_campoval.
  CONDENSE g_campoval.
  CONCATENATE '001-000 =' g_campoval INTO ge_result SEPARATED BY space.
  APPEND ge_result TO gt_result.
  ge_result = '999-999 = 0'.
  APPEND ge_result TO gt_result.

  "Exclui arquivo C:\CLIENTE\RESP\IntPos.001
  CONCATENATE g_file_resp g_file_001 INTO g_file.
  PERFORM exclui_arquivo USING g_file.

  "Exclui arquivo C:\CLIENTE\RESP\IntPos.STS
  CONCATENATE g_file_resp g_file_sts INTO g_file.
  PERFORM exclui_arquivo USING g_file.

  "Download arquivo C:\CLIENTE\REQ\IntPos.TMP
  CONCATENATE g_file_req g_file_tmp INTO g_file.
  PERFORM download_arquivo TABLES gt_result[] USING g_file.

  "Copia arquivo C:\CLIENTE\REQ\IntPos.TMP para IntPos.001
  CONCATENATE g_file_req g_file_001 INTO g_file_dest.
  PERFORM copia_arquivo USING g_file g_file_dest.

  "Exclui arquivo C:\CLIENTE\REQ\IntPos.TMP
  PERFORM exclui_arquivo USING g_file.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = TEXT-012 "Funções Administrativas
      text_question         = TEXT-013 "Deseja Reimprimir o Comprovante?
      text_button_1         = TEXT-003 "Sim
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = TEXT-004 "Não
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '1'
      display_cancel_button = ' '
    IMPORTING
      answer                = g_answer.

  IF g_answer EQ '1'.
    CLEAR g_erro.
    IF g_file_exists IS INITIAL.
      g_erro = 'X'.
      CLEAR: gt_mess, gt_mess[].
      ge_mess-message = 'Aplicação cairá por timeout.'.
      APPEND ge_mess TO gt_mess.
    ENDIF.

    IF g_erro IS INITIAL.
      CLEAR: gt_retsitef, gt_retsitef[], ge_retsitef.

      "Upload arquivo C:\CLIENTE\RESP\IntPos.001
      CONCATENATE g_file_resp g_file_001 INTO g_file.
      PERFORM upload_arquivo TABLES gt_retsitef[] USING g_file.

      "STATUS DA TRANSAÇÃO
      CLEAR ge_retsitef.
      READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '009-000'.

      CLEAR g_erro.
      IF ge_retsitef-tipoinf EQ '0'.
        "MARCA DO RECORTE
        CLEAR ge_retsitef.
        READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '028-001'.
        g_msg028 = ge_retsitef-tipoinf.

        CLEAR: g_recorte1, g_recorte2, g_recorte3.
        SPLIT g_msg028
          AT ':'
          INTO g_recorte1
               g_recorte2
               g_recorte3.

        "Exclui arquivo C:\CLIENTE\RESP\IntPos.001
        CONCATENATE g_file_resp g_file_001 INTO g_file.
        PERFORM exclui_arquivo USING g_file.

        "Exclui arquivo C:\CLIENTE\RESP\IntPos.STS
        CONCATENATE g_file_resp g_file_sts INTO g_file.
        PERFORM exclui_arquivo USING g_file.

        PERFORM imprimir_recibo_tef.

      ELSE.
        CLEAR ge_retsitef.
        READ TABLE gt_retsitef INTO ge_retsitef WITH KEY codcampo = '030-000'.

        CLEAR: gt_mess, gt_mess[].
        ge_mess-message = ge_retsitef-tipoinf.
        APPEND ge_mess TO gt_mess.

        CLEAR ok_code.
        g_erro = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

  IF g_erro = 'X'.
    CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
      EXPORTING
        endpos_col   = 80
        endpos_row   = 10
        startpos_col = 1
        startpos_row = 1
        titletext    = 'Erro'
      IMPORTING
        choise       = g_tabix
      TABLES
        valuetab     = gt_mess
      EXCEPTIONS
        break_off    = 1
        OTHERS       = 2.

    CLEAR ok_code.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form LIMPAR_FILTROS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM limpar_filtros .

  CLEAR: filtro_nome, filtro_forma, filtro_condpag.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CANCELAR_VENDA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM cancelar_venda .


  "Tabelas locais
  DATA: lt_vbeln TYPE RANGE OF vbeln,
        lt_rager TYPE RANGE OF char10.

  "Estrutura local
  DATA: ls_ranger      LIKE LINE OF lt_rager.

  DATA: lv_vbeln TYPE vbfa-vbeln.

  READ TABLE gt_zfie004 INTO ge_zfie004 WITH KEY marc = 'X'.

  IF sy-subrc NE 0 .
    "Selecionar uma venda!
    MESSAGE e008(zfi001).
  ENDIF.

  CLEAR lv_vbeln.
  SELECT SINGLE vbeln INTO lv_vbeln
    FROM vbfa
    WHERE vbelv   EQ ge_zfie004-vbeln
      AND vbtyp_n EQ '8'
      AND vbtyp_v EQ 'J'.

  "Verifica se tem transporte, em caso afirmativo, não excluir.
  IF sy-subrc EQ 0.
    "Remessa não pode ser excluída & Existe transporte &!
    MESSAGE e046(zfi001) WITH ge_zfie004-vbeln lv_vbeln.
  ENDIF.

  CLEAR: lt_vbeln, lt_vbeln[], ls_ranger.
  ls_ranger-sign    = 'I'.
  ls_ranger-option  = 'EQ'.
  ls_ranger-low     = ge_zfie004-vgbel.
  APPEND ls_ranger TO lt_vbeln.

  SUBMIT zsdr012a
          WITH s_vbeln IN lt_vbeln
          EXPORTING LIST TO MEMORY
          AND RETURN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONFIRMAR_TEF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM confirmar_tef .

  LOOP AT gt_cartao INTO ge_cartao.

    CLEAR: gt_result, gt_result[], ge_result.

*        WRITE g_number TO g_campoval.
*        CONDENSE g_campoval.
*        ge_cartao-ident_cartao = g_ident_cartao.
*        ge_cartao-number       = g_campoval.
*        ge_cartao-msg010       = g_msg010.
*        ge_cartao-msg012       = g_msg012.
*        ge_cartao-msg027       = g_msg027.
*        APPEND ge_cartao TO gt_cartao.
**        ge_result = '000-000 = CNF'.
*        ge_formpag-cartao_tef = g_msg010.

    ge_result = '000-000 = CNF'.
    APPEND ge_result TO gt_result.
    WRITE ge_cartao-number TO g_campoval.
    CONDENSE g_campoval.
    CONCATENATE '001-000 =' g_campoval INTO ge_result SEPARATED BY space.
    APPEND ge_result TO gt_result.
    CONCATENATE '010-000 =' ge_cartao-msg010 INTO ge_result SEPARATED BY space.
    APPEND ge_result TO gt_result.
    CONCATENATE '012-000 =' ge_cartao-msg012 INTO ge_result SEPARATED BY space.
    APPEND ge_result TO gt_result.
    CONCATENATE '027-000 =' ge_cartao-msg027 INTO ge_result SEPARATED BY space.
    APPEND ge_result TO gt_result.
    ge_result = '999-999 = 0'.
    APPEND ge_result TO gt_result.

    "Exclui arquivo C:\CLIENTE\RESP\IntPos.001
    CONCATENATE g_file_resp g_file_001 INTO g_file.
    PERFORM exclui_arquivo USING g_file.

    "Exclui arquivo C:\CLIENTE\RESP\IntPos.STS
    CONCATENATE g_file_resp g_file_sts INTO g_file.
    PERFORM exclui_arquivo USING g_file.

    "Download arquivo C:\CLIENTE\REQ\IntPos.TMP
    CONCATENATE g_file_req g_file_tmp INTO g_file.
    PERFORM download_arquivo TABLES gt_result[] USING g_file.

    "Copia arquivo C:\CLIENTE\REQ\IntPos.TMP para IntPos.001
    CONCATENATE g_file_req g_file_001 INTO g_file_dest.
    PERFORM copia_arquivo USING g_file g_file_dest.

    "Exclui arquivo C:\CLIENTE\RESP\IntPos.TMP
    PERFORM exclui_arquivo USING g_file.

    CLEAR g_cont.
    "Existe arquivo C:\CLIENTE\RESP\IntPos.STS
    CONCATENATE g_file_resp g_file_sts INTO g_file.
    DO 10 TIMES.
      ADD 1 TO g_cont.
      PERFORM existe_arquivo USING g_file.
      IF g_file_exists = 'X'.
        EXIT.
      ENDIF.
      WAIT UP TO 1 SECONDS.
    ENDDO.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form VERIFICA_NFCE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM verifica_nfce .

  DATA: l_flag.

  WHILE l_flag IS INITIAL.
    SELECT SINGLE conting docsta
      INTO (g_conting,
            g_docsta)
      FROM j_1bnfe_active
      WHERE docnum EQ g_docnum
        AND model  EQ '65'.

    IF g_conting IS NOT INITIAL.
      "OK
      l_flag = 'X'.
    ELSE.
      IF g_docsta EQ '1'.
        "OK
        l_flag = 'X'.
      ELSE.
        IF g_docsta IS INITIAL.
          "ler novamente
        ELSE.
          g_erro = 'X'.
          l_flag = 'X'.
          PERFORM rollback_nf.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDWHILE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ROLLBACK_NF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM rollback_nf .

  CLEAR: g_code, g_docstat, g_nftype.
  SELECT SINGLE code docstat nftype INTO (g_code, g_docstat, g_nftype)
    FROM j_1bnfdoc
    WHERE docnum EQ g_docnum.

  IF g_code   = '100' AND
     g_docstat = '1'.
    CLEAR: gt_mess, gt_mess[].
    ge_mess-message = 'Documento emitido, por gentileza solicitar devolução.'.
    APPEND ge_mess TO gt_mess.

    CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
      EXPORTING
        endpos_col   = 80
        endpos_row   = 10
        startpos_col = 1
        startpos_row = 1
        titletext    = 'Erro'
      IMPORTING
        choise       = g_tabix
      TABLES
        valuetab     = gt_mess
      EXCEPTIONS
        break_off    = 1
        OTHERS       = 2.
    g_erro = 'X'.
  ELSE.
    IF g_nftype EQ 'ZB'.
      CALL FUNCTION 'J_1B_NFE_CHECK_CANCEL_PR_AUTH'
        EXPORTING
          iv_docnum          = g_docnum
        IMPORTING
          es_acttab_mod      = ge_nfe_active
        EXCEPTIONS
          status_not_allowed = 1
          no_entry           = 2
          OTHERS             = 3.

      CALL FUNCTION 'J_1B_NFE_CANCEL_PRIOR_AUTH'
        EXPORTING
          is_acttab      = ge_nfe_active
        EXCEPTIONS
          process_errors = 1
          OTHERS         = 2.

      IF sy-subrc EQ 0.

        CALL FUNCTION 'BAPI_BILLINGDOC_CANCEL1'
          EXPORTING
            billingdocument = g_fatura
          TABLES
            return          = gt_return3
            success         = gt_success.

        CLEAR ge_return3.
        READ TABLE gt_return3 INTO ge_return3 WITH KEY type   = 'S'
                                                       id     = 'VF'
                                                       number = '311'.
        IF sy-subrc EQ 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

          UPDATE j_1bnfe_active
            SET cancel    = 'X'
            WHERE docnum EQ g_docnum.
        ELSE.
          CLEAR: gt_mess, gt_mess[].
          LOOP AT gt_return3 INTO ge_return3.
            ge_mess-message = ge_return3-message.
            APPEND ge_mess TO gt_mess.
          ENDLOOP.

          CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
            EXPORTING
              endpos_col   = 80
              endpos_row   = 10
              startpos_col = 1
              startpos_row = 1
              titletext    = 'Erro'
            IMPORTING
              choise       = g_tabix
            TABLES
              valuetab     = gt_mess
            EXCEPTIONS
              break_off    = 1
              OTHERS       = 2.
          g_erro = 'X'.
        ENDIF.
      ENDIF.
    ELSE.
      CLEAR: gt_mess, gt_mess[].
      ge_mess-message = 'Por gentileza realizar cancelamento da nota no monitor J1BNFE.'.
      APPEND ge_mess TO gt_mess.

      CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
        EXPORTING
          endpos_col   = 80
          endpos_row   = 10
          startpos_col = 1
          startpos_row = 1
          titletext    = 'Erro'
        IMPORTING
          choise       = g_tabix
        TABLES
          valuetab     = gt_mess
        EXCEPTIONS
          break_off    = 1
          OTHERS       = 2.
      g_erro = 'X'.
    ENDIF.
  ENDIF.

ENDFORM.

*****************************************************************************
*COPIA
*****************
*FORM buscar_dados_devo.
*
*  DATA: l_valor_tot TYPE zfit006-valor.
**inicio - LD - FI - DNF - DEVK917939 - 14/08/2020
*  data: gt_zfit006_devo               TYPE STANDARD TABLE OF zfit006.
*  DATA: l_valor_tot_dev  TYPE zfit006-valor.
**fim - LD - FI - DNF - DEVK917939 - 14/08/2020
*  CLEAR: t_vlr_tot_a_devolver, t_vlr_tot_cred_cliente, t_vlr_tot_restante_dev,
*         t_name1, gt_devol, gt_vbfa, gt_zfit006.
*
*  CHECK NOT t_kunnr IS INITIAL.
*
*  SELECT SINGLE name1 INTO t_name1
*    FROM kna1
*    WHERE kunnr EQ t_kunnr.
*
*  IF sy-subrc NE 0 .
*    "Cliente & não cadastrado!
*    CLEAR: t_kunnr, t_name1.
*    MESSAGE e014(zfi001) WITH t_kunnr.
*  ENDIF.
*
*  CLEAR: gt_creditos, gt_creditos[].
*
*  SELECT belnr buzei gjahr xblnr bldat dmbtr
*    INTO CORRESPONDING FIELDS OF TABLE gt_devol
*    FROM bsid
*    WHERE bukrs EQ t_bukrs
*      AND kunnr EQ t_kunnr
*      AND bschl IN ('11', '12', '14', '15', '16', '17', '19')
*      AND zlspr EQ ''.
*
*  SORT gt_devol BY bldat dmbtr.
*
*  CHECK NOT gt_devol[] IS INITIAL.
*
*  SELECT *
*    INTO TABLE gt_vbfa
*    FROM vbfa
*    FOR ALL ENTRIES IN gt_devol
*    WHERE vbeln   EQ gt_devol-belnr
*      AND vbtyp_n EQ 'O'
*      AND vbtyp_v EQ 'M'.
*
*  SORT gt_vbfa BY vbelv.
*
*  IF NOT gt_vbfa[] IS INITIAL.
*    SELECT *
*      INTO TABLE gt_zfit006
*      FROM zfit006
*      FOR ALL ENTRIES IN gt_vbfa
*      WHERE vbeln_vf           EQ gt_vbfa-vbelv
*        AND operacao           EQ 'VEND'
*        AND dcr_meio_pagamento EQ 'DINHEIRO'.
*  ENDIF.
*
*  SELECT *
*    APPENDING TABLE gt_zfit006
*    FROM zfit006
*    FOR ALL ENTRIES IN gt_devol
*    WHERE belnr              EQ gt_devol-belnr
**      AND operacao           EQ 'VEND'
*      AND operacao           IN ('VEND','ADIA','DEVO')
*      AND dcr_meio_pagamento EQ 'DINHEIRO'.
*
*    if gt_zfit006[] is not INITIAL.
*      SELECT *
*        INTO TABLE gt_zfit006_devo
*        FROM zfit006
*        FOR ALL ENTRIES IN gt_zfit006
*        WHERE VBELN_ORI          EQ gt_zfit006-VBELN
*          and VBELN_VF_ORI       eq gt_zfit006-VBELN_VF
*          AND operacao           EQ 'DEVO'
*          AND dcr_meio_pagamento EQ 'DINHEIRO'.
*    endif.
*  clear l_valor_tot_dev.
*  loop at gt_zfit006_devo ASSIGNING FIELD-SYMBOL(<fs_zfit006_devo>).
*    l_valor_tot_dev = l_valor_tot_dev + <fs_zfit006_devo>-valor.
*  ENDLOOP.
*  SORT gt_zfit006 BY vbeln item.
*
*  CLEAR t_vlr_tot_cred_cliente.
*
*  LOOP AT gt_devol INTO ge_devol.
*
*    CLEAR ge_vbfa.
*    READ TABLE gt_vbfa INTO ge_vbfa WITH KEY vbeln = ge_devol-belnr.
*
*    IF NOT ge_vbfa-vbelv IS INITIAL.
*      CLEAR ge_zfit006.
*      READ TABLE gt_zfit006 INTO ge_zfit006 WITH KEY vbeln_vf           = ge_vbfa-vbelv.
**                                                     dcr_meio_pagamento = 'DINHEIRO'.
*
*      IF sy-subrc NE 0.
*        DELETE gt_devol.
*        CONTINUE.
*      ENDIF.
*    ELSE.
*      CLEAR ge_zfit006.
*      READ TABLE gt_zfit006 INTO ge_zfit006 WITH KEY belnr = ge_devol-belnr.
*
*      IF sy-subrc NE 0.
*        DELETE gt_devol.
*        CONTINUE.
*      ENDIF.
*    ENDIF.
*
**    l_valor_tot = ge_zfit006-valor_bruto - ge_zfit006-desconto.
**    "Se o valor pago foi somente em dinheiro e teve uma devolção parcial
**    "Este registro deve aparecer para ressarcimento
**    IF ge_zfit006-valor EQ l_valor_tot.
**      "OK
**    ELSE.
**      IF ge_zfit006-valor NE ge_devol-dmbtr.
**        DELETE gt_devol.
**        CONTINUE.
**      ENDIF.
**    ENDIF.
*
**inicio - LD - FI - DNF - DEVK917939 - 10/08/2020
*    "l_valor_tot = ge_zfit006-valor_bruto - ge_zfit006-desconto.
*    "Se o valor pago foi somente em dinheiro e teve uma devolção parcial
*    "Este registro deve aparecer para ressarcimento
*    IF ge_zfit006-valor EQ l_valor_tot_dev.
*      DELETE gt_devol.
*      CONTINUE.
*    ENDIF.
**Fim - LD - FI - DNF - DEVK917939 - 14/08/2020
*
*
*    ge_devol-dcr_meio_pagamento = ge_zfit006-dcr_meio_pagamento.
*
**inicio - LD - FI - DNF - DEVK917939 - 14/08/2020
**    IF ge_zfit006-valor_ori GT 0.
**      ge_devol-dmbtr2             = ge_zfit006-valor_ori - ge_zfit006-valor.
**    ELSE.
**      ge_devol-dmbtr2             = ge_zfit006-valor.
**      IF ge_devol-dmbtr2 GT ge_devol-dmbtr.
**        ge_devol-dmbtr2             = ge_devol-dmbtr.
**      ENDIF.
**    ENDIF.
*    if ge_devol-DMBTR LE ( ge_zfit006-valor - l_valor_tot_dev ).
*       ge_devol-dmbtr2 = ge_devol-DMBTR.
*       l_valor_tot_dev = l_valor_tot_dev + ge_devol-dmbtr2.
*    else.
*       ge_devol-dmbtr2             = ge_zfit006-valor - l_valor_tot_dev.
*       l_valor_tot_dev = l_valor_tot_dev + ge_devol-dmbtr2.
*    endif.
**Fim - LD - FI - DNF - DEVK917939 - 14/08/2020
*    ge_devol-vbeln_vf           = ge_vbfa-vbeln.
*    ge_devol-vbeln_ori          = ge_zfit006-vbeln.
*    ge_devol-vbeln_vf_ori       = ge_zfit006-vbeln_vf.
*    ADD ge_devol-dmbtr2 TO t_vlr_tot_cred_cliente.
*
*    MODIFY gt_devol FROM ge_devol.
*
*  ENDLOOP.
*
*  SORT gt_devol BY bldat dmbtr2.
*
*  t_vlr_tot_a_devolver = t_vlr_tot_cred_cliente.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_BUSCAR_DOC_ORIGEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GE_DEVOL_BELNR
*&      <-- GE_ZFIT006_VBELN_ORI
*&      <-- GE_ZFIT006_VBELN_VF_ORI
*&      <-- GE_ZFIT006_BELNR_ORI
*&---------------------------------------------------------------------*
FORM f_buscar_doc_origem  USING    p_belnr
                          CHANGING p_vbeln_ori
                                   p_vbeln_vf_ori
                                   p_belnr_ori.
  p_belnr_ori = p_belnr.

  SELECT * FROM vbfa  INTO TABLE @DATA(gt_vbfa_o)
  WHERE vbeln = @p_belnr "Documento Referencia
  AND vbtyp_n = 'O'.    "Ctg.doc.subsequente
  IF sy-subrc EQ 0.
    READ TABLE gt_vbfa_o ASSIGNING FIELD-SYMBOL(<fs_vbfa_o>) INDEX 1.
    IF <fs_vbfa_o> IS ASSIGNED.
      p_vbeln_ori = <fs_vbfa_o>-vbelv.
    ENDIF.
    SELECT * FROM vbfa  INTO TABLE @DATA(gt_vbfa_m)
    WHERE vbelv = @p_vbeln_ori "Documento Referencia
      AND vbtyp_n = 'M'.    "Ctg.doc.subsequente
    IF sy-subrc EQ 0.
      READ TABLE gt_vbfa_m ASSIGNING FIELD-SYMBOL(<fs_vbfa_m>) INDEX 1.
      IF <fs_vbfa_m> IS ASSIGNED.
        p_vbeln_vf_ori = <fs_vbfa_m>-vbeln.
      ENDIF.
    ENDIF.
  ELSE.
    SELECT * FROM zfit006 INTO TABLE @DATA(gt_zfit006)
      WHERE belnr = @p_belnr.
    IF sy-subrc EQ 0.
      READ TABLE gt_zfit006 ASSIGNING FIELD-SYMBOL(<fs_zfit006>) INDEX 1.
      IF <fs_zfit006> IS ASSIGNED.
        p_vbeln_ori = <fs_zfit006>-vbeln_ori.
      ENDIF.
      p_vbeln_vf_ori = <fs_zfit006>-vbeln_vf_ori.
    ENDIF.
  ENDIF.
ENDFORM.
