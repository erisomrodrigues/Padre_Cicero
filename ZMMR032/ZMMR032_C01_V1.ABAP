*&---------------------------------------------------------------------*
*& Include          ZMMR032_C01
*&---------------------------------------------------------------------*

CLASS lcl_main_report IMPLEMENTATION.

  METHOD: init.
    lv_lifnr = p_lifnr.
    IF lines( p_emdes ) <= 20.
      select_data( ).
    ELSE.
      MESSAGE |Quantidade máxima de destinátarios inseridos| TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD: select_data.

    SELECT *
     FROM ekko AS a
     WHERE a~lifnr = @p_lifnr
     AND   a~ekgrp IN @so_ekgrp
     AND   a~bsart IN @so_bsart
     AND   a~bedat IN @so_bedat
     AND   a~frgke <> 'B'
     AND   a~ebeln IN ( SELECT appl_object_id
                         FROM  apoc_d_or_item AS b
                         WHERE appl_object_id      IN @so_ebeln
                         AND   b~appl_object_type  = 'PURCHASE_ORDER'
                         AND   b~channel           = 'EMAIL'
                         AND   b~dispatch_time     = '2' )
     INTO TABLE @gt_ekko.

    IF gt_ekko IS NOT INITIAL.
      processing( ).
    ELSE.
      MESSAGE: 'Pedido não liberado para a impressão do PDF' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.


  ENDMETHOD.

  METHOD: processing.

**  APPC - MM.568837 - Erisom Almeida - 08/07/2024 - Inicio
    DATA: lt_content TYPE soli_tab.

    DATA: lo_sender    TYPE REF TO cl_sapuser_bcs,
          lo_recipient TYPE REF TO if_recipient_bcs.

    CONSTANTS: gc_raw TYPE char3 VALUE 'RAW'.

    "Consultando usuários logados
    SELECT adr6~smtp_addr UP TO 1 ROWS
                          FROM usr21
                          INNER JOIN adr6
                          ON adr6~addrnumber = usr21~addrnumber
                          AND adr6~persnumber = usr21~persnumber
                          INTO TABLE @DATA(lt_loggeduser)
                          WHERE usr21~bname = @sy-uname
                          AND   adr6~smtp_srch IS NOT NULL
                          AND   adr6~smtp_srch <> ''.
** APPC - MM.568837 - Erisom Almeida - 08/07/2024 - Fim

    TRY.
        "Criar solicitação de envio
        go_bcs = cl_bcs=>create_persistent( ).

        "Definindo o remetente
        lo_sender = cl_sapuser_bcs=>create( sy-uname ). "APPC - AERA - MM.568837 - 10/07/2024

        "Adicionando o remetente
        go_bcs->set_sender( lo_sender )."APPC - AERA - MM.568837 - 10/07/2024

        "Email Para ...
        LOOP AT p_emdes ASSIGNING FIELD-SYMBOL(<fs_dest>).
          FREE: recipient.

          IF sy-tabix = 1.
            "Definindo o destinatário
            recipient = cl_cam_address_bcs=>create_internet_address( <fs_dest>-low ).

            "Adicionando o destinatário no email
            CALL METHOD go_bcs->add_recipient
              EXPORTING
                i_recipient = recipient
                i_express   = 'X'.

          ELSE.
            "Definindo o destinatário
            recipient = cl_cam_address_bcs=>create_internet_address( <fs_dest>-low ).

            "Adicionando o destinatário
            CALL METHOD go_bcs->add_recipient
              EXPORTING
                i_recipient = recipient
                i_copy      = 'X'.
          ENDIF.
        ENDLOOP.

        "Definindo o Remetente
        READ TABLE lt_loggeduser ASSIGNING FIELD-SYMBOL(<fs_user>) INDEX 1.
        FREE lo_recipient.
        lo_recipient = cl_cam_address_bcs=>create_internet_address( <fs_user>-smtp_addr ).

        "Adicionando o Remetente
        go_bcs->add_recipient(
          EXPORTING
            i_recipient  = lo_recipient
            i_express    = 'X' ).

        "Pegando o texto adicionado no campo de texto
        go_textedit->get_text_as_r3table(
          IMPORTING
            table                  = gt_tab
          EXCEPTIONS
            error_dp               = 1
            error_cntl_call_method = 2
            error_dp_create        = 3
            potential_data_loss    = 4
            OTHERS                 = 5
        ).

        LOOP AT gt_tab ASSIGNING FIELD-SYMBOL(<fs_tab>).
          APPEND <fs_tab> TO lt_content.
        ENDLOOP.

        "Criando o email com o corpo e titulo
        go_doc_bcs = cl_document_bcs=>create_document(
                       i_type         = gc_raw
                       i_subject      = p_assu
                       i_text         = lt_content ).

        "Adicionando o email ao objeto de envio
        go_bcs->set_document( go_doc_bcs ).

*        sender = cl_cam_address_bcs=>create_internet_address( i_address_string = 'nfe@pecicero.com.br'
*                                                              i_address_name   = p_reme ).

*        sender = cl_cam_address_bcs=>create_internet_address( p_reme ).



*        go_bcs->set_sender( sender ).

        "Setando para o envio imediato
        go_bcs->set_send_immediately( abap_true ).

        "Parâmetros de controle
        gs_outopt-tddest   = 'LP01'.
        gs_outopt-tdnewid  = abap_true.
        gs_outopt-tdimmed  = abap_true.
        gs_outopt-tddelete = ''.

        gs_ctrlop-device    = 'PRINTER'.
        gs_ctrlop-no_dialog = abap_true.
        gs_ctrlop-getotf    = abap_true.

        "Gerando o arquivo para anexo
        LOOP AT gt_ekko ASSIGNING FIELD-SYMBOL(<fs_ekko>).

          FREE: gs_po.
          "Pegando a condição de pagamento
          SELECT SINGLE vtext
            FROM tvzbt AS tv
            INNER JOIN ekko AS ko
            ON ( ko~zterm = tv~zterm
            AND  tv~spras = 'P' )
            WHERE ko~ebeln = @<fs_ekko>-ebeln
            INTO @gs_po-cond_pgto.

          "pegando a observação
          cabe_txt-tdname   = <fs_ekko>-ebeln.
          cabe_txt-tdid     = 'F01'.
          cabe_txt-tdspras  = 'P'.
          cabe_txt-tdobject = 'EKKO'.

          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = cabe_txt-tdid
              language                = cabe_txt-tdspras
              name                    = cabe_txt-tdname
              object                  = cabe_txt-tdobject
            TABLES
              lines                   = lt_lines
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.

          LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<fs_lines>).
            CONCATENATE gs_po-obs_cabe <fs_lines> INTO gs_po-obs_cabe SEPARATED BY space.
          ENDLOOP.

          "Pegando os dados para a seção de fornecedor
          SELECT SINGLE a~lifnr,
                        a~name1,
                        a~stras,
                        b~house_num1,
                        a~mcod3,
                        a~regio,
                        b~post_code1,
                        c~tel_number,
                        d~fax_number,
                        a~stcd1,
                        a~stcd3,
                        e~remark,
                        f~smtp_addr
            FROM lfa1 AS a
            INNER JOIN adrc AS b
            ON ( b~addrnumber = a~adrnr )
            LEFT JOIN adr2 AS c
            ON ( c~addrnumber = a~adrnr )
            LEFT JOIN adr3 AS d
            ON ( d~addrnumber = a~adrnr )
            LEFT JOIN adrct AS e
            ON ( e~addrnumber = a~adrnr )
            LEFT JOIN adr6 AS f
            ON ( f~addrnumber = a~adrnr )
            WHERE a~lifnr = @<fs_ekko>-lifnr
            INTO @DATA(ls_forn).

          "Pegando os dados do item
          SELECT DISTINCT po~ebelp   AS item,                          "Item
                          po~matnr   AS codigo,                        "Material
                          po~mfrpn   AS referencia,                    "Referencia
                          po~txz01   AS descricao,                     "Descrição
                          po~j_1bnbm AS ncm,                           "NCM
                          po~menge   AS quantidade,                    "Quantidade
                          po~meins   AS unidade,                       "Unidade
                          po~netpr   AS preco_uni,                     "Preço unitário
                          po~effwr   AS total                          "Total
            FROM ekpo AS po
            WHERE po~ebeln = @<fs_ekko>-ebeln
            ORDER BY po~ebelp
            INTO CORRESPONDING FIELDS OF TABLE @gt_itens.

          "Calculando o IPI, ICMS, SUBTOTAL e TOTAL do pedido
          LOOP AT gt_itens ASSIGNING FIELD-SYMBOL(<fs_itens>).

            CALL FUNCTION 'ZMM_CALCULA_TAXAS'
              EXPORTING
                p_ebeln = <fs_ekko>-ebeln
                p_ebelp = <fs_itens>-item
                p_ekko  = <fs_ekko>
              IMPORTING
                p_wmwst = lv_imp
                t_komv  = lt_imp.

*** APPC - MM.26364 - Davi Alves - 17.08.2023 - Ínicio
            "Pegando o valor do IPI do item
            SELECT SINGLE kawrt
              FROM @lt_imp AS ipi
              WHERE kschl IN ( 'IPI0', 'IPI1', 'IPI2' )
              AND kwert > 0
              INTO @DATA(lv_ipi).

            "Pegando o valor o ICMS do item
            SELECT SINGLE kawrt
              FROM @lt_imp AS icms
              WHERE kschl IN ( 'ICM0', 'ICM1', 'ICM2' )
              AND kwert > 0
              INTO @DATA(lv_icms).
*** APPC - MM.26364 - Davi Alves - 17.08.2023 - Fim

            "Pegando o valor unitário do produto
            SELECT SINGLE pr~kbetr
              FROM prcd_elements AS pr
              INNER JOIN ekko AS ko
              ON ( ko~knumv = pr~knumv )
              WHERE ko~ebeln = @<fs_ekko>-ebeln
              AND   pr~kposn = @<fs_itens>-item
              AND   pr~kschl IN ( 'PBXX', 'PB00' )
              AND   pr~kbetr > 0
              INTO @lv_uni.

            "Valor do Preço unitário
            <fs_itens>-preco_uni = lv_uni.

            "Valor total
            <fs_itens>-total = <fs_itens>-preco_uni * <fs_itens>-quantidade.

            lv_ttipi = ( ( lv_ipi * 100 ) / <fs_itens>-total ).

            "Valor do IPI do item no smartforms
            <fs_itens>-ipi = lv_ttipi.

            lv_tticm = ( ( lv_icms * 100 ) / <fs_itens>-total ).

            "Valor do ICMS do item no smartforms
            <fs_itens>-icms = lv_tticm.

            "Valor do IPI por material * quantidade
            lv_ipip  = lv_ipi.

            "Valor do ICMS por material * quantidade
            lv_icmsp = lv_icms.

            "Valor total do IPI
            gs_po-tot_ipi = gs_po-tot_ipi + lv_ipip.

            "Valor total do ICMS
            gs_po-tot_icms = gs_po-tot_icms + lv_icmsp.

            "Valor Subtotal
            gs_po-sub_tot = gs_po-sub_tot + <fs_itens>-total.

            lv_st = lv_imp - lv_ipi - lv_icms.

            "Valor total com imposto
            gs_po-tot_imp = gs_po-tot_imp + ( <fs_itens>-total + lv_st + lv_ipi ).

          ENDLOOP.

          gs_po-pedido     = <fs_ekko>-ebeln.
          gs_po-tp_ped     = <fs_ekko>-bsart.
          gs_po-item       = gt_itens.
          gs_po-fornecedor-cod_forn     = ls_forn-lifnr.
          gs_po-fornecedor-nome_forn    = ls_forn-name1.
          gs_po-fornecedor-end_forn     = ls_forn-stras.
          gs_po-fornecedor-num_forn     = ls_forn-house_num1.
          gs_po-fornecedor-cidade_forn  = ls_forn-mcod3.
          gs_po-fornecedor-uf_forn      = ls_forn-regio.
          gs_po-fornecedor-cep_forn     = ls_forn-post_code1.
          gs_po-fornecedor-tel_forn     = ls_forn-tel_number.
          gs_po-fornecedor-fax_forn     = ls_forn-fax_number.
          gs_po-fornecedor-cnpj_forn    = ls_forn-stcd1.
          gs_po-fornecedor-ie_forn      = ls_forn-stcd3.
          gs_po-fornecedor-contat_forn  = ls_forn-remark.
          gs_po-fornecedor-email_forn   = ls_forn-smtp_addr.

          FREE: gt_itens, lv_ipi, lv_icms, lv_ipip, lv_icmsp, lv_imp.

          "Pegando a função do smartforms
          CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
            EXPORTING
              formname           = gc_sfname
            IMPORTING
              fm_name            = gv_fname
            EXCEPTIONS
              no_form            = 1
              no_function_module = 2
              OTHERS             = 3.

          "Chamando a função do smartforms para pegar o arquivo no tipo otf
          CALL FUNCTION gv_fname
            EXPORTING
              control_parameters = gs_ctrlop
              output_options     = gs_outopt
              is_ped             = gs_po
            IMPORTING
              job_output_info    = gt_otfdata
            EXCEPTIONS
              formatting_error   = 1
              internal_error     = 2
              send_error         = 3
              user_canceled      = 4
              OTHERS             = 5.

          gt_otf[] = gt_otfdata-otfdata[].

          "Convertendo o OTF para PDF
          CALL FUNCTION 'CONVERT_OTF'
            EXPORTING
              format                = 'PDF'
              max_linewidth         = 132
            IMPORTING
              bin_file              = lv_file
            TABLES
              otf                   = gt_otf
              lines                 = gt_tab1
            EXCEPTIONS
              err_max_linewidth     = 1
              err_format            = 2
              err_conv_not_possible = 3
              err_bad_otf           = 4
              OTHERS                = 5.

          "Convertendo o PDF para binário
          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
            EXPORTING
              buffer     = lv_file
            TABLES
              binary_tab = gt_binary_content.

          lv_arq_sub = <fs_ekko>-ebeln.

          IF cb_file IS NOT INITIAL.
            generate_file( i_ebeln = <fs_ekko>-ebeln ).
          ENDIF.

          "Adicionando o PDF no email
          CALL METHOD go_doc_bcs->add_attachment
            EXPORTING
              i_attachment_type    = 'PDF'
              i_attachment_subject = lv_arq_sub
              i_att_content_hex    = gt_binary_content.

        ENDLOOP.

        IF cb_file IS INITIAL.
          send_email( ).
        ENDIF.

      CATCH cx_root INTO DATA(e_text).
        DATA(lv_tx) = e_text->get_text( ).
        MESSAGE: lv_tx TYPE 'E' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD: send_email.

    TRY.
        "Enviando E-mail
        CALL METHOD go_bcs->send
          EXPORTING
            i_with_error_screen = 'X'
          RECEIVING
            result              = gv_sent.

        COMMIT WORK AND WAIT.

        IF gv_sent IS NOT INITIAL.
          MESSAGE 'Email enviado com sucesso' TYPE 'S' DISPLAY LIKE 'S'.
        ENDIF.

      CATCH cx_send_req_bcs INTO DATA(lcx_email).
        DATA(lv_text) = lcx_email->get_text( ).
        MESSAGE lv_text TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD generate_file.

    IF p_dire IS NOT INITIAL.

      TRY.
          CONCATENATE p_dire i_ebeln '.PDF' INTO lv_dire.

          CALL FUNCTION 'GUI_DOWNLOAD'
            EXPORTING
              filename                = lv_dire
              filetype                = 'BIN'
              confirm_overwrite       = 'X'
            TABLES
              data_tab                = gt_binary_content
            EXCEPTIONS
              file_write_error        = 1
              no_batch                = 2
              gui_refuse_filetransfer = 3
              invalid_type            = 4
              no_authority            = 5
              unknown_error           = 6
              header_not_allowed      = 7
              separator_not_allowed   = 8
              filesize_not_allowed    = 9
              header_too_long         = 10
              dp_error_create         = 11
              dp_error_send           = 12
              dp_error_write          = 13
              unknown_dp_error        = 14
              access_denied           = 15
              dp_out_of_memory        = 16
              disk_full               = 17
              dp_timeout              = 18
              file_not_found          = 19
              dataprovider_exception  = 20
              control_flush_error     = 21
              OTHERS                  = 22.
        CATCH cx_root INTO DATA(lx_root).
          DATA(lv_err) = lx_root->get_text( ).
          MESSAGE 'Não foi possível baixar o arquivo' TYPE 'I' DISPLAY LIKE 'E'.
      ENDTRY.
    ELSE.
      MESSAGE 'Favor escolher um diretório' TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD search_folder.
    "Chama método para selecionar diretório
    CALL METHOD cl_gui_frontend_services=>directory_browse
      EXPORTING
        window_title         = 'Selecione a pasta desejada para baixar o arquivo'
        initial_folder       = 'C:/'
      CHANGING
        selected_folder      = lv_folder
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.

    IF sy-subrc = 0.
      CONCATENATE lv_folder '\' INTO p_dire.
    ELSE.
      MESSAGE e836(sd) WITH TEXT-005.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
