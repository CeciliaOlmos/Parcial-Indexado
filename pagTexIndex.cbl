      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAG ASSIGN TO "..\pagos.txt"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT PAGOS ASSIGN TO "..\pagos.dat"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS pag-recibo
                   ALTERNATE RECORD KEY IS pag-socio WITH DUPLICATES.
       DATA DIVISION.
       FILE SECTION.
       FD  PAGOS.
       01  pag-reg.
           03 pag-recibo pic 9(10).
           03 pag-socio pic 9(5).
           03 pag-fecha pic 9(8).
           03 pag-importe pic 9(8).

       FD  PAG.
       01  pag-tex-reg.
           03 pag-tex-recibo pic 9(10).
           03 pag-tex-socio pic 9(5).
           03 pag-tex-fecha pic 9(8).
           03 pag-tex-importe pic 9(8).

       WORKING-STORAGE SECTION.
       77  w-flag-cli pic 9.
           88 fin-archivo value 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO.
           PERFORM 200-LEER-ARCH-FACT.
           PERFORM UNTIL fin-archivo
            PERFORM 300-PROCESO
            PERFORM 200-LEER-ARCH-FACT
           END-PERFORM.
           PERFORM 400-FIN.
            STOP RUN.

       100-INICIO.
           OPEN INPUT PAG.
           OPEN OUTPUT PAGOS.
       200-LEER-ARCH-FACT.
           READ PAG AT END MOVE 1 TO w-flag-cli.
       300-PROCESO.
           move pag-tex-recibo to pag-recibo.
           move pag-tex-socio to pag-socio.
           move pag-tex-fecha to pag-fecha.
           move pag-tex-importe to pag-importe.
           write pag-reg.

       400-FIN.
           CLOSE PAG.
           CLOSE PAGOS.
       END PROGRAM YOUR-PROGRAM-NAME.
