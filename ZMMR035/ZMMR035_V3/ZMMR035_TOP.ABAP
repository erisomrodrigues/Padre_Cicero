*&---------------------------------------------------------------------*
*& Include          ZMMR035_TOP
*&---------------------------------------------------------------------*
CLASS zcl_main_report DEFINITION.
  PUBLIC SECTION.
    "Type Poool
    TYPE-POOLS: slis.

    "Tipos
    TYPES:
      BEGIN OF ty_final,
        matnr    TYPE mara-matnr,       " Código do Material
        mfrpn    TYPE mara-mfrpn,       " Referência
        maktx    TYPE makt-maktx,       " Aplicação do material
        lifnr    TYPE lfa1-lifnr,       " Código do fornecedor
        name1    TYPE lfa1-name1,       " Fornecedor
        bismt    TYPE mara-bismt,       " Agrupador
        matkl    TYPE mara-matkl,       " Grupo de mercadoria
        dtucp    TYPE ekko-aedat,       " Data da compra
        vrucp    TYPE ekpo-brtwr,       " Preço Bruto
        qtdtl    TYPE ekpo-menge,       " Quantidade
        ganhador TYPE lfa1-name1,       " Ganhador da cortação
      END OF ty_final,

      BEGIN OF ty_checkbox,
        check TYPE xfeld,               " CheckBox
        lifnr TYPE ekko-lifnr,          " Código do Fornecedor
        name1 TYPE lfa1-name1,          " Nome do Fornecedor
      END OF ty_checkbox,

      BEGIN OF ty_pedido,
        matnr TYPE mara-matnr,
        mfrpn TYPE mara-mfrpn,
        maktx TYPE makt-maktx,
        lifnr TYPE ekko-lifnr,
        name1 TYPE lfa1-name1,
        bismt TYPE mara-bismt,
        matkl TYPE mara-matkl,
      END OF ty_pedido.

    "Catalog de campos
    DATA: lt_fcat TYPE lvc_t_fcat,
          ls_fcat TYPE lvc_s_fcat.

    "Catalogo de campos do PopUp
    DATA: ls_lcat TYPE slis_t_fieldcat_alv,
          ls_dcat TYPE slis_fieldcat_alv.

    DATA: lv_dtatu TYPE sy-datum,
          lv_dtfin TYPE sy-datum,
          lv_dtam  TYPE sy-datum.


    "Tabelas e estrututas
    DATA:
      lt_final    TYPE TABLE OF ty_final,
      ls_final    TYPE          ty_final,
      lt_pedido   TYPE TABLE OF ty_pedido,
      ls_pedido   TYPE          ty_pedido,
      lt_checkbox TYPE TABLE OF ty_checkbox,
      ls_checkbox TYPE          ty_checkbox,
      ls_ganhador TYPE ty_final.

    "Botão
    DATA:
      ok_code       TYPE sy-ucomm.

    DATA: r_sel_checkbox TYPE RANGE OF lfa1-lifnr.

    "Objetos
    DATA:
      lo_salv        TYPE REF TO cl_salv_table,
      lo_column      TYPE REF TO cl_salv_column,
      lo_columns     TYPE REF TO cl_salv_columns_table,
      lo_funct       TYPE REF TO cl_salv_functions_list,
      lo_agg         TYPE REF TO cl_salv_aggregations,
      lo_result_data TYPE REF TO cl_salv_ex_result_data_table,
      lo_container   TYPE REF TO cl_gui_custom_container,
      go_alv         TYPE REF TO cl_gui_alv_grid,
      lo_data        TYPE REF TO data.

    "Metodos
    METHODS:
      init,
      select_dados,
*      send_dados,
      process_data,
      show_data.

ENDCLASS.

DATA: go_main_report TYPE REF TO zcl_main_report.
