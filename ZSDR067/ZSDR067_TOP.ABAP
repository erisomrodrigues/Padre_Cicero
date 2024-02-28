*&---------------------------------------------------------------------*
*& Include          ZSDR067_TOP
*&---------------------------------------------------------------------*
CLASS lcl_main_report DEFINITION.
  PUBLIC SECTION.

    "Estrutura (Cabeçalho do Excel)
    TYPES: BEGIN OF mty_s_head,
             nome_cabecalho(20) TYPE c,
*             docnum TYPE char20,
*             nftype TYPE char20,
*             direct TYPE char20,
*             docdat TYPE char20,
*             pstdat TYPE char20,
*             nftot  TYPE char20,
*             model  TYPE char20,
*             series TYPE char20,
*             nfnum  TYPE char20,
*             branch TYPE char20,
*             parid  TYPE char20,
*             nfenum TYPE char20,
*             natop  TYPE char20,
*             regio  TYPE char20,
*             name1  TYPE char20,
**             matnr  TYPE char20,
**             maktx  TYPE char20,
**             cfop   TYPE char20,
**             matorg TYPE char20,
**             taxsit TYPE char20,
**             taxsi2 TYPE char20,
**             cest   TYPE char20,
**             menge  TYPE char20,
**             meins  TYPE char20,
**             taxsi4 TYPE char20,
**             taxsi5 TYPE char20,
**             itmnum TYPE char20,
**             nfnet  TYPE char20,
*             chave  TYPE char20,
           END OF mty_s_head.

    "Estrutura (Dados do excel)
    TYPES: BEGIN OF mty_s_excel,
             docnum TYPE j_1bnfdoc-docnum,
             nftype TYPE j_1bnfdoc-nftype,
             direct TYPE j_1bnfdoc-direct,
             docdat TYPE j_1bnfdoc-docdat,
             pstdat TYPE j_1bnfdoc-pstdat,
             nfnett TYPE j_1bnflin-nfnett,
             model  TYPE j_1bnfdoc-model,
             series TYPE j_1bnfdoc-series,
*             nfnum  TYPE j_1bnfdoc-nfnum,
             branch TYPE j_1bnfdoc-branch,
             parid  TYPE j_1bnfdoc-parid,
             nfenum TYPE j_1bnfdoc-nfenum,
             natop  TYPE j_1bnfdoc-natop,
             inco1  TYPE j_1bnfdoc-inco1,
             cfop   TYPE j_1bnflin-cfop,
             regio  TYPE j_1bnfdoc-regio,
             name1  TYPE j_1bnfdoc-name1,
*             matnr  TYPE j_1bnflin-matnr,
*             maktx  TYPE j_1bnflin-maktx,
*             matorg TYPE j_1bnflin-matorg,
*             taxsit TYPE j_1bnflin-taxsit,
*             taxsi2 TYPE j_1bnflin-taxsi2,
*             cest   TYPE j_1bnflin-cest,
*             menge  TYPE j_1bnflin-menge,
*             meins  TYPE j_1bnflin-meins,
*             taxsi4 TYPE j_1bnflin-taxsi4,
*             taxsi5 TYPE j_1bnflin-taxsi5,
*             itmnum TYPE j_1bnflin-itmnum,
*             nfnet  TYPE j_1bnflin-nfnet,
             chave  TYPE j_1b_nfe_access_key_dtel44,
           END OF mty_s_excel.

    "Estrutura (Dados relatório sintético)
    TYPES: BEGIN OF mty_s_sint,
             branch TYPE j_1bnfdoc-branch,
             cfop   TYPE j_1bnflin-cfop,
             nfnett TYPE j_1bnflin-nfnett,
           END OF mty_s_sint.

    TYPES: BEGIN OF mty_sum,
             docnum  TYPE j_1bnfdoc-docnum,
             cfop    TYPE j_1bnflin-cfop,
             nfnett  TYPE j_1bnflin-nfnett,
             nfnett1 TYPE j_1bnflin-nfnett,
           END OF mty_sum.

    TYPES: BEGIN OF mty_stax,
             docnum TYPE j_1bnfdoc-docnum,
             itmnum TYPE j_1bnflin-itmnum,
             taxgrp TYPE j_1bnfstx-taxgrp,
             nfnett TYPE j_1bnflin-nfnett,
           END OF mty_stax.

    TYPES: BEGIN OF mty_gstx,
             docnum TYPE j_1bnfdoc-docnum,
             nfnett TYPE j_1bnflin-nfnett,
             taxval TYPE j_1bnfstx-taxval,
             total  TYPE j_1bnflin-nfnett,
           END OF mty_gstx.



    "APPC - SD. 494365 - Erisom Almeida - 22/02/2024 - Inicio
    TYPES: BEGIN OF mty_stx,
             docnum TYPE j_1bnfdoc-docnum,
             itmnum TYPE j_1bnflin-itmnum,
             nfnett TYPE j_1bnflin-nfnett,
             TAXGRP TYPE J_1BNFSTX-TAXGRP,
             taxval TYPE j_1bnfstx-taxval,
             total  TYPE j_1bnflin-nfnett,
           END OF mty_stx.
    "APPC - SD. 494365 - Erisom Almeida - 16/02/2024 - Fim

    "Estruturas
    DATA: ls_key    TYPE salv_s_layout_key,
          ls_layout TYPE salv_s_layout_info,
          ls_sum    TYPE mty_sum,
          ls_gstx   TYPE mty_stx.

    "Tabela
    DATA: lt_head          TYPE TABLE OF mty_s_head,
          lt_excel         TYPE TABLE OF mty_s_excel,
          lt_sum           TYPE TABLE OF mty_sum  WITH KEY docnum,
          lt_stax          TYPE TABLE OF mty_stax WITH KEY docnum,
          lt_gstx          TYPE TABLE OF mty_gstx  WITH KEY docnum,
          lt_gjtx          TYPE TABLE OF mty_stx  WITH KEY docnum,
          lt_stx           TYPE TABLE OF mty_stx  WITH KEY docnum,
          lt_sinte         TYPE TABLE OF mty_s_sint,
          lt_columns_table TYPE salv_t_column_ref,
          lt_rows          TYPE salv_t_row.

    "Variável
    DATA: lv_title    TYPE string,
          lv_folder   TYPE string,
          lv_filename TYPE string,
          lv_data     TYPE sy-datum,
          lv_item_aux TYPE j_1bnflin-itmnum.

    "Objetos locais
    DATA: mo_salv_table TYPE REF TO cl_salv_table,
          lo_selections TYPE REF TO cl_salv_selections,
          lo_column     TYPE REF TO cl_salv_column_table,
          lo_functions  TYPE REF TO cl_salv_functions_list,
          lo_display    TYPE REF TO cl_salv_display_settings,
          lo_selects    TYPE REF TO cl_salv_selections,
          lo_columns    TYPE REF TO cl_salv_columns_table,
          lo_layout     TYPE REF TO cl_salv_layout,
          lo_events     TYPE REF TO cl_salv_events_table,
          lo_agg        TYPE REF TO cl_salv_aggregations, "APPC - DAM - classe para adicionar o excel - 23/03/2023
          lo_sort       TYPE REF TO cl_salv_sorts,        "APPC - DAM - classe para adicionar o excel - 23/03/2023
          lo_sort_col   TYPE REF TO cl_salv_sort,         "APPC - DAM - classe para adicionar o excel - 23/03/2023
          lo_resu_data  TYPE REF TO cl_salv_ex_result_data_table. "APPC - DAM - classe para adicionar o excel - 28/03/2023

    "Metodos
    METHODS:init,
      data_select,
      gerar_excel,
      busca_pasta,
      data_display,
      change_columns
        IMPORTING
          io_columns TYPE REF TO cl_salv_columns_table.

    CLASS-METHODS: f4_layout
      IMPORTING
        iv_restrict TYPE salv_de_layout_restriction
      CHANGING
        cv_layout   TYPE disvariant-variant.

ENDCLASS.

DATA go_main_report TYPE REF TO lcl_main_report.
