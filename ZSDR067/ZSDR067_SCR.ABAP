*&---------------------------------------------------------------------*
*& Include          ZSDR067_SCR
*&---------------------------------------------------------------------*
TABLES: j_1bnfdoc.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: s_direct FOR j_1bnfdoc-direct MODIF ID t1,            "Direção do movimento de mercadorias
                s_pstdat FOR j_1bnfdoc-pstdat MODIF ID t1,            "Data documento
                s_branch FOR j_1bnfdoc-branch MODIF ID t1.            "Local de negócios
PARAMETERS:     cb_canc AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.

PARAMETERS: cb_excel AS CHECKBOX,
            p_file   TYPE rlgrap-filename. "MODIF ID t2. "Arquivo

SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
PARAMETERS: rb_anali RADIOBUTTON GROUP rb01 USER-COMMAND chang_screen DEFAULT 'X',
            rb_sinte RADIOBUTTON GROUP rb01,
            rb_a_PRD RADIOBUTTON GROUP rb01. " APPC - MM. - Erisom Almeida - 28/08/2024

SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
PARAMETERS p_layout TYPE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b4.

*AT SELECTION-SCREEN OUTPUT.
*  LOOP AT SCREEN.
*    CASE 'X'.
*      WHEN rb_alv.
*        IF screen-group1 = 'T2'.
*          screen-invisible = 1.
*          screen-input     = 0.
*          screen-active    = 0.
*          MODIFY SCREEN.
*          CONTINUE.
*        ENDIF.
*
*        IF screen-group1 = 'T1'.
*          screen-invisible = 0.
*          screen-input     = 1.
*          screen-active    = 1.
*          MODIFY SCREEN.
*          CONTINUE.
*        ENDIF.
*
*        FREE p_file.
*
*      WHEN rb_excel.
*        IF screen-group1 = 'T1'.
*          screen-invisible = 0.
*          screen-input     = 1.
*          screen-active    = 1.
*          MODIFY SCREEN.
*          CONTINUE.
*
*          IF screen-group1 = 'T2'.
*            screen-invisible = 0.
*            screen-input     = 0.
*            screen-active    = 0.
*            MODIFY SCREEN.
*            CONTINUE.
*          ENDIF.
*        ENDIF.
*    ENDCASE.
*  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  IF go_main_report IS NOT BOUND.
    CREATE OBJECT go_main_report.
  ENDIF.

  go_main_report->busca_pasta( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.

  lcl_main_report=>f4_layout(
  EXPORTING
    iv_restrict = cl_salv_layout=>restrict_none
  CHANGING
    cv_layout   = p_layout ).
