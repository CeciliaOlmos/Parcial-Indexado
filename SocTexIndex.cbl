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
           SELECT SOC ASSIGN TO "..\socios.txt"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT SOCIOS ASSIGN TO "..\socios.dat"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS SEQUENTIAL
                   RECORD KEY IS soc-llave.


       DATA DIVISION.
       FILE SECTION.
        FD  SOCIOS.
       01  soc-reg.
           03 soc-llave.
               05 soc-socio pic 9(5).
               05 soc-mes pic 9(2).
           03 soc-estado pic x.
           03 soc-cuota pic 9(6).
           03 soc-pagado pic 9(6).
           03 soc-debe pic 9(6).
       FD  SOC.
       01  soc-tex-reg.
           03 soc-tex-socio pic 9(5).
           03 soc-tex-mes pic 9(2).
           03 soc-tex-estado pic x.
           03 soc-tex-cuota pic 9(6).
           03 soc-tex-pagado pic 9(6).
           03 soc-tex-debe pic 9(6).
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
           OPEN INPUT SOC.
           OPEN OUTPUT SOCIOS.
       200-LEER-ARCH-FACT.
           READ SOC AT END MOVE 1 TO w-flag-cli.
       300-PROCESO.
           MOVE soc-tex-socio to soc-socio.
           MOVE soc-tex-mes to soc-mes.
           move soc-tex-estado to soc-estado.
           move soc-tex-cuota to soc-cuota.
           move soc-tex-pagado to soc-pagado.
           move soc-tex-debe to soc-debe.
           write soc-reg.

       400-FIN.
           CLOSE SOC.
           CLOSE SOCIOS.
       END PROGRAM YOUR-PROGRAM-NAME.
