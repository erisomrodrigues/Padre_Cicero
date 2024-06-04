*& ----------------------Auto Peças Padre Cicero-----------------------*
*& Programa : ZMMR035                                                  *
*& Descrição: Relatório para cotação de compras                        *
*& Autor    : Erisom Almeida                                           *
*& Data     : 31/10/2023                                               *
*& Request  : DEVK930587                                               *
*& ID Asana : MM.445780                                                *
*&---------------------------------------------------------------------*
*& Histórico das modificações                                          *
*& Data       Nome             Request    Descrição                    *
*&---------------------------------------------------------------------*
REPORT zmmr035.

INCLUDE:
  zmmr035_top, " Top Declarações
  zmmr035_s01, " Select Screen
  zmmr035_c01. " Class Implementation

START-OF-SELECTION.
  CREATE OBJECT go_main_report.
    go_main_report->init( ).
