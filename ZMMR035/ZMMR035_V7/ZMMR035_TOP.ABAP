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
        desc_mt TYPE  makt-maktx, "Descrição Material
        agrupad TYPE  mara-bismt, "Agrupador
        nomfor1 TYPE  lfa1-name1, "Razação Social do Fornecedor
        codmat1 TYPE  mara-matnr, "Código do Material
        vrulcp1 TYPE  ekpo-brtwr, "Valor da Ultima Compra
        dtulcp1 TYPE  ekko-aedat, "Data da Ultima Compra
        mdvend1 TYPE  ekpo-menge, "Media de Vendas Periodo 5 Meses
        nomfor2 TYPE  lfa1-name1,
        codmat2 TYPE  mara-matnr,
        vrulcp2 TYPE  ekpo-brtwr,
        dtulcp2 TYPE  ekko-aedat,
        mdvend2 TYPE  ekpo-menge,
        nomfor3 TYPE  lfa1-name1,
        codmat3 TYPE  mara-matnr,
        vrulcp3 TYPE  ekpo-brtwr,
        dtulcp3 TYPE  ekko-aedat,
        mdvend3 TYPE  ekpo-menge,
        nomfor4 TYPE  lfa1-name1,
        codmat4 TYPE  mara-matnr,
        vrulcp4 TYPE  ekpo-brtwr,
        dtulcp4 TYPE  ekko-aedat,
        mdvend4 TYPE  ekpo-menge,
        ganhado TYPE  lfa1-name1, "Ganhador da Cotação
      END OF ty_final,

      BEGIN OF ty_ganhado,
        matnr    TYPE mara-matnr, "Código do Material
        mfrpn    TYPE mara-mfrpn, "Referência
        maktx    TYPE makt-maktx, "Aplicação do material
        lifnr    TYPE lfa1-lifnr, "Código do fornecedor
        name1    TYPE lfa1-name1, "Fornecedor
        bismt    TYPE mara-bismt, "Agrupador
        matkl    TYPE mara-matkl, "Grupo de mercadoria
        dtucp    TYPE ekko-aedat, "Data da compra
        vrucp    TYPE ekpo-brtwr, "Preço Bruto
        qtdtl    TYPE ekpo-menge, "Quantidade
        ganhador TYPE lfa1-name1, "Ganhador da cortação
      END OF ty_ganhado,

    begin of ty_checkbox,
    CHECK type xfeld,      "CheckBox
    lifnr TYPE lfa1-lifnr, "Código do Fornecedor
    name1 TYPE lfa1-name1, "Nome do Fornecedor
    END OF ty_checkbox,

    BEGIN OF ty_pedido,
    matnr TYPE mara-matnr,
    mfrpn TYPE mara-mfrpn,
    maktx TYPE makt-maktx,
    lifnr TYPE ekko-lifnr,
    name1 TYPE lfa1-name1,
    bismt TYPE mara-bismt,
    matkl TYPE mara-matkl,
    END OF ty_pedido,

    BEGIN OF TY_DADOSCP,
      MATNR TYPE MARA-MATNR,
      DATCP TYPE EKKO-aedat,
      VALCP TYPE EKPO-brtwr,
   END OF TY_DADOSCP.


    "Catalog de campos
    DATA: gs_tcat TYPE lvc_t_fcat,  "Definição dos colunas do POPUP
          gs_scat TYPE lvc_s_fcat.  "Configuração do POPUP

    "Catalogo de campos do PopUp
    DATA: gs_lcat TYPE slis_t_fieldcat_alv,
          gs_dcat TYPE slis_fieldcat_alv.

    "Tabelas e estrututas
    DATA:
      gt_final    TYPE TABLE OF ty_final,     "Tabela global para exibir ultimo ALV
      wa_final    TYPE          ty_final,     "Work Área p/ armazenar temporariamente dados do ultimo ALV
      gt_pedido   TYPE TABLE OF ty_pedido,
      wa_pedido   TYPE          ty_pedido,
      gt_checkbox TYPE TABLE OF ty_checkbox,  "Tabela Global p/ armazenar a lista do POPUP
      wa_checkbox TYPE          ty_checkbox,  "Work Área p/ armazenar temporariamente dados do POPUP
      wa_ganhado  TYPE          ty_ganhado,
      gt_compras  TYPE TABLE OF ty_pedido,
      wa_compras  TYPE          ty_pedido,
      GT_DADOSCP  TYPE TABLE OF ty_dadoscp,
      WA_DADOSCP  TYPE          ty_dadoscp.

    "Range
    DATA: r_checkbox TYPE RANGE OF lfa1-lifnr. "Range p/ Armazenar os valores do POPUP

    "Botão
    DATA:
      ok_code       TYPE sy-ucomm.

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
      process_data,
      show_data.

ENDCLASS.

DATA: go_main_report TYPE REF TO zcl_main_report.
