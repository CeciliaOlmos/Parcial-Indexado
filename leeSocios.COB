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
           SELECT SOCIOS ASSIGN TO "..\socios.dat"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS SEQUENTIAL
                   RECORD KEY IS soc-llave
                    ALTERNATE RECORD KEY IS soc-estado WITH DUPLICATES.
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
       WORKING-STORAGE SECTION.
       77  sen pic 9 value 0.
       01  lin-cabecera.
           03 filler pic x(7) value "CODIGO:".
           03 filler pic x(2) value spaces.
           03 filler pic x(4) value "MES:".
           03 filler pic x(7) value "ESTADO:".
           03 filler pic x(1) value spaces.
           03 filler pic x(6) value "CUOTA:".
           03 filler pic x(4) value spaces.
           03 filler pic x(7) value "PAGADO:".
           03 filler pic x(4) value spaces.
           03 filler pic x(5) value "DEBE:".
           03 filler pic x(4) value spaces.
       01  lin-guarda.
           03 filler pic x(80) value all "-".
       01  lin-detalle.
           03 l-soc pic zzzzz value spaces.
           03 filler pic x(4) value spaces.
           03 l-mes pic z9 value spaces.
           03 filler pic x(4) value spaces.
           03 l-estado pic x value spaces.
           03 filler pic x(5) value spaces.
           03 l-cuota pic zzz.zz9.
           03 filler pic x(4) value spaces.
           03 l-pagado pic zzz.zz9.
           03 filler pic x(4) value spaces.
           03 l-debe pic zzz.zz9.
           03 filler pic x(5) value spaces.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO-LECTURA.
           PERFORM 200-LEE-ARCH-SOCIOS.
           PERFORM UNTIL sen is equal 1
               PERFORM 300-PROCESO-LECTURA
               PERFORM 200-LEE-ARCH-SOCIOS
           END-PERFORM.
           PERFORM 400-FIN-LECTURA.
            STOP RUN.
        100-INICIO-LECTURA.
           PERFORM 130-ABRIR-ARCHIVOS.
           PERFORM 150-LISTAR-ENCABEZADO.

       130-ABRIR-ARCHIVOS.
           OPEN INPUT SOCIOS.

       150-LISTAR-ENCABEZADO.
           DISPLAY lin-guarda.
           DISPLAY lin-cabecera.
           DISPLAY lin-guarda.

       200-LEE-ARCH-SOCIOS.
           READ SOCIOS at end move 1 to sen.

       300-PROCESO-LECTURA.
           MOVE soc-socio to l-soc.
           MOVE soc-mes to l-mes.
           MOVE soc-estado to l-estado.
           MOVE soc-cuota to l-cuota.
           move soc-pagado to l-pagado.
           move soc-debe to l-debe.
           DISPLAY lin-detalle.

       400-FIN-LECTURA.
           CLOSE SOCIOS.
       END PROGRAM YOUR-PROGRAM-NAME.
