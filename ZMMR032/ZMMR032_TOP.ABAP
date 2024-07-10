*&---------------------------------------------------------------------*
*& Include          ZMMR032_TOP
*&---------------------------------------------------------------------*
CLASS lcl_main_report DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_docdata,
             obj_descr     TYPE so_obj_des,
             obj_langu     TYPE so_obj_la,
             sensitivity   TYPE so_sensit,
             sent_by       TYPE so_snd_nam,
             recipient     TYPE so_rec,
             subject       TYPE   so_obj_des,
             use_same_type TYPE so_username,
             object_type   TYPE so_obj_tp,
             filename      TYPE string,
             file_type     TYPE string,
             content       TYPE solix_tab,
           END OF ty_docdata.

    "Objetos
    DATA: go_bcs     TYPE REF TO cl_bcs,
          go_doc_bcs TYPE REF TO cl_document_bcs,
          sender     TYPE REF TO if_sender_bcs,
          recipient  TYPE REF TO if_recipient_bcs.

    "Tabelas
    DATA: gt_ekko           TYPE TABLE OF ekko,
          gt_otf            TYPE TABLE OF itcoo,
          gt_otfdata        TYPE ssfcrescl,
          gt_binary_content TYPE TABLE OF solix,
*          gt_content        TYPE soli_tab,
          gt_tab1           TYPE TABLE OF tline,
          gt_itens          TYPE TABLE OF zmmepo_itens,
          lt_lines          TYPE TABLE OF tline.

    "Estruturas
    DATA: gs_ctrlop TYPE ssfctrlop,
          gs_outopt TYPE ssfcompop,
          gs_po     TYPE zmmepo. "APPC - MM.26364 - Davi Alves - 17/07/2023

    "Constantes
    CONSTANTS: gc_sfname TYPE fpname VALUE 'ZMMSF_PEDIDO_COMPRA'.

    "Variaveis
    DATA: "gv_bin_xstr TYPE xstring,
      gv_sent     TYPE os_boolean,
      gv_fname    TYPE rs38l_fnam,
      gv_filesize TYPE so_obj_len,
      lv_file     TYPE xstring,
      lv_arq_sub  TYPE so_obj_des,
      lv_lifnr    TYPE string,
      lv_ipip     TYPE zipi,  "APPC - MM.26364 - Davi Alves - 17/07/2023
      lv_icmsp    TYPE zipi,  "APPC - MM.26364 - Davi Alves - 17/07/2023
      lv_imp      TYPE wmwst, "APPC - MM.26364 - Davi Alves - 18/07/2023
      lt_imp      TYPE komv_tab, "APPC - MM.26364 - Davi Alves - 18/07/2023
      lv_folder   TYPE string, "APPC - MM.26364 - Davi Alves - 18/07/2023
      lv_dire     TYPE string, "APPC - MM.26364 - Davi Alves - 18/07/2023
      lv_st       TYPE prcd_elements-kbetr, "APPC - MM.26364 - Davi Alves - 20/07/2023
      lv_ttipi    TYPE p LENGTH 13 DECIMALS 2,
      lv_tticm    TYPE p LENGTH 13 DECIMALS 2,
      cabe_txt    TYPE thead,
      itab        TYPE TABLE OF string,
      var1(254),
      var2(254),
      l_len1      TYPE i,
      l_len2      TYPE i,
      l_len3      TYPE i,
      l_tot1      TYPE i,
      l_tot2      TYPE i,
      lv_uni      TYPE bbwert.

    METHODS send_email
      IMPORTING
        is_kna1   TYPE zfie014      "Cliente
        iv_xblnr  TYPE bkpf-xblnr
      CHANGING
        ct_return TYPE bapiret2_t
      RAISING
        lcx_report_exception.

    METHODS: init,
      select_data,
      processing,
      generate_file IMPORTING i_ebeln TYPE ekko-ebeln,
      search_folder,
      send_email.
ENDCLASS.

DATA(lo_main_report) = NEW lcl_main_report( ).

"Objetos globais
DATA: go_docking_container TYPE REF TO cl_gui_docking_container,
      go_textedit          TYPE REF TO cl_gui_textedit,
      go_alv               TYPE REF TO cl_gui_alv_grid,
      go_container         TYPE REF TO cl_gui_custom_container.

"Tabelas
DATA: gt_tab  TYPE STANDARD TABLE OF tdline.
