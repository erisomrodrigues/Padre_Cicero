*&---------------------------------------------------------------------*
*& Include          ZMMR032_SCR
*&---------------------------------------------------------------------*

TABLES: ekko, adr6.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-100.
PARAMETERS: p_lifnr TYPE ekko-lifnr OBLIGATORY.
SELECT-OPTIONS: so_ebeln FOR ekko-ebeln OBLIGATORY,
                so_ekgrp FOR ekko-ekgrp,
                so_bsart FOR ekko-bsart,
                so_bedat FOR ekko-bedat.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-101.
PARAMETERS: p_assu TYPE so_obj_des,
            p_reme TYPE ad_smtpadr OBLIGATORY.
SELECT-OPTIONS: p_emdes FOR adr6-smtp_addr OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-103.
PARAMETERS: cb_file AS CHECKBOX USER-COMMAND chang_screen DEFAULT abap_false,
            p_dire  TYPE rlgrap-filename.
SELECTION-SCREEN END OF BLOCK b4.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dire.
  IF lo_main_report IS NOT BOUND.
    CREATE OBJECT lo_main_report.
  ENDIF.

  lo_main_report->search_folder( ).

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CASE cb_file.
      WHEN 'X'.
        IF screen-name = 'P_DIRE'.
          screen-input     = 1.
          MODIFY SCREEN.
          CONTINUE.
        ENDIF.
      WHEN ' '.
        IF screen-name = 'P_DIRE'.
          screen-input     = 0.
          FREE p_dire.
          MODIFY SCREEN.
          CONTINUE.
        ENDIF.

    ENDCASE.
  ENDLOOP.

INITIALIZATION.

  TRY.
      IF go_docking_container IS NOT INITIAL.
        FREE: go_docking_container,
              go_textedit.
      ENDIF.

      CREATE OBJECT go_docking_container
        EXPORTING
          repid = sy-cprog
          dynnr = sy-dynnr
          ratio = 30
          side  = cl_gui_docking_container=>dock_at_bottom
          name  = 'CONTENT'.

      CREATE OBJECT go_textedit
        EXPORTING
          parent                     = go_docking_container
          wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
          wordwrap_to_linebreak_mode = cl_gui_textedit=>true
        EXCEPTIONS
          error_cntl_create          = 1
          error_cntl_init            = 2
          error_cntl_link            = 3
          error_dp_create            = 4
          gui_type_not_supported     = 5
          OTHERS                     = 6.
    CATCH cx_root INTO DATA(cx_err).
      DATA(err_txt) = cx_err->get_text( ).
      MESSAGE err_txt TYPE 'E' DISPLAY LIKE 'E'.
  ENDTRY.
