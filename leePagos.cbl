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

       WORKING-STORAGE SECTION.
       77  sen pic 9 value zero.
       01  lin-cabecera.
           03 filler pic x(7) value "RECIBO:".
           03 filler pic x(2) value spaces.
           03 filler pic x(7) value "SOCIO:".
           03 filler pic x(1) value spaces.
           03 filler pic x(6) value "FECHA:".
           03 filler pic x(4) value spaces.
           03 filler pic x(8) value "IMPORTE:".

       01  lin-guarda.
           03 filler pic x(80) value all "-".
       01  lin-detalle.
           03 l-recibo pic zzzzzzzzzz value spaces.
           03 filler pic x(4) value spaces.
           03 l-socio pic zzzzz value spaces.
           03 filler pic x(4) value spaces.
           03 l-fecha pic zzzzzzzz value spaces.
           03 filler pic x(5) value spaces.
           03 l-importe pic zz.zzz.zz9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO-LECTURA.
           PERFORM 200-LEE-ARCH-PAGOS.
           PERFORM UNTIL sen is equal 1
               PERFORM 300-PROCESO-LECTURA
               PERFORM 200-LEE-ARCH-PAGOS
           END-PERFORM.
           PERFORM 400-FIN-LECTURA.

            STOP RUN.
        100-INICIO-LECTURA.
           PERFORM 130-ABRIR-ARCHIVOS.
           PERFORM 150-LISTAR-ENCABEZADO.

       130-ABRIR-ARCHIVOS.
           OPEN INPUT PAGOS.

       150-LISTAR-ENCABEZADO.
           DISPLAY lin-guarda.
           DISPLAY lin-cabecera.
           DISPLAY lin-guarda.

       200-LEE-ARCH-PAGOS.
           READ PAGOS NEXT at end move 1 to sen.

       300-PROCESO-LECTURA.
           MOVE pag-recibo TO l-recibo.

           MOVE pag-socio TO l-socio.
           MOVE pag-fecha TO l-fecha.
           MOVE pag-importe TO l-importe.
           DISPLAY lin-detalle.

       400-FIN-LECTURA.
           CLOSE PAGOS.
       END PROGRAM YOUR-PROGRAM-NAME.
