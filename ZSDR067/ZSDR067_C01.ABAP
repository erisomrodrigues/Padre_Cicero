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
    IF rb_sinte IS NOT INITIAL.
*** APPC - HML - Busca as notas se estão canceladas ou não - 24/03/2023
      SELECT DISTINCT lin~docnum,
                      lin~itmnum,
                      SUM( tax~taxval ) AS som
        FROM j_1bnfstx AS tax
        INNER JOIN j_1bnflin AS lin
        ON ( lin~docnum = tax~docnum
        AND  lin~itmnum = tax~itmnum )
        INNER JOIN j_1bnfdoc AS doc
        ON ( doc~docnum = tax~docnum )
        WHERE doc~direct IN @s_direct "Direção do movimento de mercadorias
        AND   doc~pstdat IN @s_pstdat "Data documento
        AND   doc~branch IN @s_branch "Local de negócios
        AND   doc~cancel  = @cb_canc
        AND   tax~taxgrp  = 'ICST'
        AND   doc~doctyp  IN ('1','6') "APPC - SD. 494365 - Erisom Almeida - 07/02/2024
        AND   doc~docstat = '1'
        OR    tax~taxgrp  = 'IPI'
        AND   doc~direct IN @s_direct "Direção do movimento de mercadorias
        AND   doc~pstdat IN @s_pstdat "Data documento
        AND   doc~branch IN @s_branch "Local de negócios
        AND   doc~cancel  = @cb_canc
        AND   doc~doctyp  = '1'
        AND   doc~docstat = '1'
        GROUP BY lin~docnum, lin~itmnum
        INTO TABLE @lt_stax.

      SELECT DISTINCT j_1bnfdoc~branch,              " Local de negócios
                      j_1bnflin~cfop,                " Código e extensão CFOP
                      SUM( coalesce( j_1bnflin~nfnett, 0 ) + coalesce( tax~nfnett, 0 ) ) AS som " Valor total incluindo todos os impostos
        FROM j_1bnfdoc
        INNER JOIN j_1bnflin
        ON ( j_1bnflin~docnum = j_1bnfdoc~docnum )
        LEFT JOIN @lt_stax AS tax
        ON ( tax~docnum = j_1bnfdoc~docnum )
        WHERE j_1bnfdoc~direct IN @s_direct "Direção do movimento de mercadorias
        AND   j_1bnfdoc~pstdat IN @s_pstdat "Data documento
        AND   j_1bnfdoc~branch IN @s_branch "Local de negócios
        AND   j_1bnfdoc~cancel  = @cb_canc
        AND   j_1bnfdoc~doctyp IN ('1','6') "APPC - SD. 494365 - Erisom Almeida - 07/02/2024
        AND   j_1bnfdoc~docstat = '1'
        GROUP BY j_1bnfdoc~branch, j_1bnflin~cfop
        HAVING ( j_1bnflin~cfop > 0 )
        ORDER BY j_1bnfdoc~branch, j_1bnflin~cfop
        INTO TABLE @lt_sinte.

    ELSE.

      SELECT DISTINCT lin~docnum,
                      lin~itmnum,
                      SUM( tax~taxval ) AS som
        FROM j_1bnfstx AS tax
        INNER JOIN j_1bnflin AS lin
        ON ( lin~docnum = tax~docnum
        AND  lin~itmnum = tax~itmnum )
        INNER JOIN j_1bnfdoc AS doc
        ON ( doc~docnum = tax~docnum )
        WHERE doc~direct IN @s_direct "Direção do movimento de mercadorias
        AND   doc~pstdat IN @s_pstdat "Data documento
        AND   doc~branch IN @s_branch "Local de negócios
        AND   doc~cancel  = @cb_canc
        AND   tax~taxgrp  = 'ICST'
        AND   doc~doctyp  = '1'
        AND   doc~docstat = '1'
        OR    doc~direct IN @s_direct "Direção do movimento de mercadorias
        AND   doc~pstdat IN @s_pstdat "Data documento
        AND   doc~branch IN @s_branch "Local de negócios
        AND   doc~cancel  = @cb_canc
        AND   tax~taxgrp  = 'IPI'
        AND   doc~doctyp  IN ('1','6') "APPC - SD. 494365 - Erisom Almeida - 07/02/2024
        AND   doc~docstat = '1'
        GROUP BY lin~docnum, lin~itmnum
        INTO TABLE @lt_stax.

      SELECT DISTINCT j_1bnfdoc~docnum,
                      j_1bnflin~cfop,
                      SUM( coalesce( j_1bnflin~nfnett, 0 ) + coalesce( tax~nfnett, 0 ) ) AS som
        FROM j_1bnfdoc
        INNER JOIN j_1bnflin
        ON ( j_1bnfdoc~docnum = j_1bnflin~docnum )
        LEFT JOIN @lt_stax AS tax
        ON ( j_1bnflin~docnum = tax~docnum
        AND  j_1bnflin~itmnum = tax~itmnum )
        WHERE j_1bnfdoc~direct IN @s_direct "Direção do movimento de mercadorias
        AND   j_1bnfdoc~pstdat IN @s_pstdat "Data documento
        AND   j_1bnfdoc~branch IN @s_branch "Local de negócios
        AND   j_1bnfdoc~cancel  = @cb_canc
        AND   j_1bnfdoc~doctyp  IN ('1','6') "APPC - SD. 494365 - Erisom Almeida - 07/02/2024
        AND   j_1bnfdoc~docstat = '1'
        GROUP BY j_1bnfdoc~docnum, j_1bnflin~cfop
        ORDER BY j_1bnfdoc~docnum ASCENDING
        INTO TABLE @lt_sum.

      SELECT DISTINCT som~docnum,       " Nº documento
                      j_1bnfdoc~nftype, " Ctg.de nota fiscal
                      j_1bnfdoc~direct, " Direção do movimento de mercadorias
                      j_1bnfdoc~docdat, " Data do documento
                      j_1bnfdoc~pstdat, " Data de lançamento
                      som~nfnett,       " Valor total incluindo todos os impostos
                      j_1bnfdoc~model,  " Modelo da nota fiscal
                      j_1bnfdoc~series, " Séries
                      j_1bnfdoc~branch, " Local de negócios
                      j_1bnfdoc~parid,  " Identificação do parceiro (cliente, fornecedor, loc.negócio)
                      j_1bnfdoc~nfenum, " Número de nota fiscal eletrônica
                      j_1bnfdoc~natop,  " Natureza da Operação
                      j_1bnfdoc~inco1,  " Incoterms
                      som~cfop,         " Código e extensão CFOP
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
        INNER JOIN @lt_sum AS som
        ON ( som~docnum = j_1bnfdoc~docnum )
        INNER JOIN j_1bnflin
        ON ( j_1bnfdoc~docnum = j_1bnflin~docnum )
        INNER JOIN j_1bnfstx AS tax
        ON ( tax~docnum = j_1bnfdoc~docnum )
        INNER JOIN j_1bnfe_active
        ON ( j_1bnfe_active~docnum = j_1bnfdoc~docnum )
        WHERE j_1bnfdoc~direct IN @s_direct "Direção do movimento de mercadorias
        AND   j_1bnfdoc~pstdat IN @s_pstdat "Data documento
        AND   j_1bnfdoc~branch IN @s_branch "Local de negócios
        AND   j_1bnfdoc~cancel  = @cb_canc
        AND   j_1bnfdoc~doctyp  IN ('1','6') "APPC - SD. 494365 - Erisom Almeida - 07/02/2024
        AND   j_1bnfdoc~docstat = '1'
        ORDER BY som~docnum ASCENDING
        INTO TABLE @lt_excel.
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
          data_display( ).
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
        ELSE.
          cl_salv_table=>factory(
          EXPORTING
            list_display   = if_salv_c_bool_sap=>false
          IMPORTING
            r_salv_table   = mo_salv_table
          CHANGING
            t_table        = lt_sinte
        ).
        ENDIF.
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
        ELSE.
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table = mo_salv_table
            CHANGING
              t_table      = lt_excel ).
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
ENDCLASS.
