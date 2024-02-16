*&---------------------------------------------------------------------*
*& Include          ZSDR071_C01
*&---------------------------------------------------------------------*
" Implementação de classe
CLASS zcl_main_report IMPLEMENTATION.
  METHOD: init.
    lv_minutos = p_min.

    CALL FUNCTION 'IGN_TIMESTAMP_PLUSMINUS'
      EXPORTING
        start_date      = sy-datum
        start_time      = sy-uzeit
        forward         = ' '
        backward        = 'X'
        days            = '0'
        hours           = '0'
        minutes         = lv_minutos
        seconds         = '0'
      IMPORTING
        result_date     = lv_data
        result_time     = lv_hora
      EXCEPTIONS
        parameter_error = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    select_data( ).
  ENDMETHOD.

  METHOD: select_data.

    "Buscando as MIGOs criadas
    SELECT DISTINCT pf~mblnr,
                    pf~xblnr,
                    seg~lifnr,
                    seg~ebeln,
                    seg~mjahr, " APPC - MM.459885 - Ericky Fernandes - 08/02/2024
      concat( pf~mblnr, pf~mjahr ) AS obj
      FROM mseg AS seg
      INNER JOIN ekko AS ko
      ON ( ko~ebeln = seg~ebeln )
      INNER JOIN mkpf AS pf
      ON ( pf~mblnr = seg~mblnr )
      INNER JOIN zwmst004 AS wms
      ON ( wms~bwart = seg~bwart )
      INNER JOIN cdhdr
      ON ( cdhdr~objectid = ko~ebeln ) "APPC - MM.449001 - Hemilly Maria - 08/11/2023
      WHERE wms~interface = '11'
      AND pf~budat  = @sy-datum
      AND ko~bsart  <> 'ZUB1'
      AND pf~cputm BETWEEN @lv_hora AND @sy-uzeit
      AND cdhdr~tcode <> 'ME21N' "APPC - MM.449001 - Hemilly Maria - 08/11/2023
      INTO TABLE @lt_migo.

    IF lt_migo IS NOT INITIAL.
      "Buscando as MIROs criadas
      SELECT DISTINCT kp~belnr,
                      kp~xblnr,
                      kp~lifnr,
                      bs~ebeln,
                      bs~gjahr AS mgjahr, " APPC - MM.459885 - Ericky Fernandes - 08/02/2024
                      bs~awkey
        FROM rbkp AS kp
        INNER JOIN bseg AS bs
        ON ( bs~belnr = kp~belnr )
        FOR ALL ENTRIES IN @lt_migo
        WHERE kp~xblnr = @lt_migo-xblnr
        AND   bs~ebeln = @lt_migo-ebeln
        AND   kp~lifnr = @lt_migo-lifnr
        INTO TABLE @lt_miro.
    ENDIF.

*** APPC - MM.430750 - Davi Alves - 15/09/2023 - Inicio
    SELECT DISTINCT a~ebeln
      FROM ekko AS a
      INNER JOIN lips AS b
      ON ( b~vgbel = a~ebeln )
      INNER JOIN cdhdr
      ON ( cdhdr~objectid = a~ebeln )
      WHERE a~aedat = @sy-datum
      AND a~bsart IN ( 'UB', 'ZUB', 'ZUB1' )
      AND b~erzet BETWEEN @lv_hora AND @sy-uzeit
      AND cdhdr~tcode <> 'ME21N' "APPC - MM.506069 - Ericky Fernandes - 15/02/2024
      ORDER BY a~ebeln
      INTO TABLE @lt_ebeln.

    DELETE ADJACENT DUPLICATES FROM lt_ebeln.
*** APPC - MM.430750 - Davi Alves - 15/09/2023 - Fim

    IF lt_migo IS NOT INITIAL OR lt_miro IS NOT INITIAL OR lt_ebeln IS NOT INITIAL. "APPC - MM.430750 - Davi Alves - 15/09/2023
      process_data( ).
    ELSE.
      MESSAGE |Nenhum documento encontrado para envio| TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

  ENDMETHOD.

  METHOD: process_data.

    LOOP AT lt_migo ASSIGNING FIELD-SYMBOL(<fs_migo>).
      "Verificando se é uma nota de compra
      SELECT SINGLE mr~mblnr,
                    mr~ref
        FROM @lt_miro AS mr
        WHERE mr~lifnr = @<fs_migo>-lifnr
        AND   mr~ebeln = @<fs_migo>-ebeln
        INTO CORRESPONDING FIELDS OF @ls_migo.

      IF sy-subrc = 0.
        <fs_migo>-mblnr = ls_migo-mblnr.
        <fs_migo>-ref   = ls_migo-ref.
      ENDIF.

      ls_mblnr-sign   = 'I'.
      ls_mblnr-option = 'EQ'.
      ls_mblnr-low    = <fs_migo>-mblnr.
      APPEND ls_mblnr TO lr_mblnr.

*     APPC - MM.459885 - Ericky Fernandes - 08/02/2024 - Início
      IF lv_mjahr IS INITIAL AND <fs_migo>-mjahr IS NOT INITIAL.
        lv_mjahr = <fs_migo>-mjahr.
      ENDIF.
*     APPC - MM.459885 - Ericky Fernandes - 08/02/2024 - Fim
    ENDLOOP.

*** APPC - MM.430750 - Davi Alves - 15/09/2023 - Inicio
    "Verificando os pedidos de compras possuem envio para o wms
    LOOP AT lt_ebeln ASSIGNING FIELD-SYMBOL(<fs_ebeln>).

      APPEND INITIAL LINE TO lr_ebeln ASSIGNING FIELD-SYMBOL(<fs_rebeln>).
      <fs_rebeln>-sign   = 'I'.
      <fs_rebeln>-option = 'EQ'.
      <fs_rebeln>-low    = <fs_ebeln>-ebeln.

    ENDLOOP.
*** APPC - MM.430750 - Davi Alves - 15/09/2023 - Fim

    IF lr_mblnr IS NOT INITIAL OR lr_ebeln IS NOT INITIAL.
      send_document( ).
    ELSE.
      MESSAGE 'Nenhum documento atendeu os requisitos para o reenvio' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

  ENDMETHOD.

  METHOD: send_document.
*** APPC - MM.430750 - Davi Alves - 15/09/2023 - Inicio
    IF lr_mblnr IS NOT INITIAL.

      SUBMIT zwmsr002 AND RETURN
                            WITH s_mblnr  IN lr_mblnr
                            WITH p_mjahr  =  lv_mjahr " APPC - MM.459885 - Ericky Fernandes - 08/02/2024
                            WITH p_entdiv =  abap_true.
    ENDIF.

    IF lr_ebeln IS NOT INITIAL.
      SUBMIT zwmsr002 AND RETURN
                          WITH s_ebeln  IN lr_ebeln
                          WITH p_mjahr  = sy-datum(4)
                          WITH p_pedtra = abap_true.
    ENDIF.
*** APPC - MM.430750 - Davi Alves - 15/09/2023 - Fim

    MESSAGE |Documentos enviados com sucesso| TYPE 'I' DISPLAY LIKE 'S'.
  ENDMETHOD.
ENDCLASS.
