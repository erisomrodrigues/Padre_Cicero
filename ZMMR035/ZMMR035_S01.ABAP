*&---------------------------------------------------------------------*
*& Include          ZMMR035_S01
*&---------------------------------------------------------------------*

TABLES: mara, makt, mard, mseg, lfa1, ekko, ekpo.

* Determinando a tela de seleção
SELECTION-SCREEN: BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: so_lifnr   FOR lfa1-lifnr,         "Fornecedor
                so_bismt   FOR mara-bismt,         "Agrupador
                so_matnr   FOR mara-matnr,         "Material
                so_mfrpn   FOR mara-mfrpn,         "Referência
                so_matkl   FOR mara-matkl,         "Grupo de Mercadiria
                so_categ   FOR mara-zzcategoria,   "Categoria
                so_subct   FOR mara-zzsubcategoria,"Subcategoria
                so_mstae   FOR mara-mstae,         "Status
                so_prdha   FOR mara-prdha,         "Hierarquia
                so_werks   FOR mard-werks.         "Centros
SELECTION-SCREEN END OF BLOCK bl1.
