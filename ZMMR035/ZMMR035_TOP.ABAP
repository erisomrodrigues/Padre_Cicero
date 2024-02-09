*&---------------------------------------------------------------------*
*& Include          ZMMR035_TOP
*&---------------------------------------------------------------------*

* Declarações dos tipos de dados
TYPES: BEGIN OF ty_final,
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
       END OF ty_final.
* Tabelas
DATA: lt_final    TYPE TABLE OF ty_final,
      ls_final    TYPE          ty_final,
      ls_ganhador TYPE ty_final.

* Declaração de Objetos
DATA: lo_salv        TYPE REF TO cl_salv_table,
      lo_column      TYPE REF TO cl_salv_column,
      lo_columns     TYPE REF TO cl_salv_columns_table,
      lo_funct       TYPE REF TO cl_salv_functions_list,
      lo_agg         TYPE REF TO cl_salv_aggregations,
      lo_result_data TYPE REF TO cl_salv_ex_result_data_table,
      lo_data        TYPE REF TO data.
