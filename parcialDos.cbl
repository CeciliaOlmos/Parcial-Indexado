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
            SELECT SOCIOS ASSIGN TO "..\socios.dat"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS soc-llave.
       DATA DIVISION.
       FILE SECTION.
        FD  PAGOS.
       01  pag-reg.
           03 pag-recibo pic 9(10).
           03 pag-socio pic 9(5).
           03 pag-fecha pic 9(8).
           03 pag-importe pic 9(8).

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
         77  w-flag-pagos pic 9.
           88 fin-arch-pago value 1.
       77  w-flag-socios pic 9.
           88 fin-arch-socios value 1.
       77  w-pag-ant pic 9(5).
       77  w-soc-ant pic 9(5).
       01  w-importe-pagado pic 9(8).
       01  w-cuotas-pag pic 99.
       01  w-i pic 99.
       01  w-importe-vector pic 9(8).
       01  w-mes-pagado pic 9(8).
       01  w-mes-nuevo pic 99.
       01  w-total-pagado pic 9(8).
       01  w-total-anterior pic 9(8).
       01  w-cuotas.
           03 filler pic 9(6) value 2000.
           03 filler pic 9(6) value 2000.
           03 filler pic 9(6) value 2000.
           03 filler pic 9(6) value 2500.
           03 filler pic 9(6) value 2500.
           03 filler pic 9(6) value 2500.
           03 filler pic 9(6) value 3000.
           03 filler pic 9(6) value 3000.
           03 filler pic 9(6) value 3500.
           03 filler pic 9(6) value 3500.
           03 filler pic 9(6) value 3800.
           03 filler pic 9(6) value 3800.
       01  tabla-cuotas REDEFINES w-cuotas.
           03 vec-mes pic 9(6) OCCURS 12 TIMES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO-ACTUALIZAR.
           PERFORM 200-POSICIONAR-CLAVE.
           PERFORM UNTIL fin-arch-pago
               PERFORM 400-INICIO-PAGO
               PERFORM UNTIL fin-arch-pago or w-pag-ant NOT = pag-socio
                       PERFORM 500-PROCESO-PAGO
                       PERFORM 300-LEER-ARCH-PAGO
               END-PERFORM
               PERFORM 600-FIN-PAGO
           END-PERFORM.
           PERFORM 1000-FIN-ACTUALIZAR.
           STOP RUN.

       100-INICIO-ACTUALIZAR.
           PERFORM 120-ABRIR-ARCHIVO.
           PERFORM 130-OBTENER-TOTAL-ANUAL.
       120-ABRIR-ARCHIVO.
           OPEN INPUT PAGOS.
           OPEN I-O SOCIOS.
       130-OBTENER-TOTAL-ANUAL.
           PERFORM VARYING w-i FROM 1 BY 1 UNTIL w-i>12
               ADD vec-mes(w-i) TO w-importe-vector
           END-PERFORM.
       200-POSICIONAR-CLAVE.
           MOVE ZERO TO pag-socio.
           START PAGOS KEY IS > pag-socio
               INVALID KEY
                   DISPLAY "NO ENCONTRE AL SOCIO"
               NOT INVALID KEY
                   PERFORM 300-LEER-ARCH-PAGO.
       300-LEER-ARCH-PAGO.
           READ PAGOS NEXT AT END MOVE 1 TO w-flag-pagos.
       400-INICIO-PAGO.
           MOVE pag-socio to w-pag-ant.
           MOVE ZERO TO w-importe-pagado.
       500-PROCESO-PAGO.
           add pag-importe to w-importe-pagado.
           PERFORM 700-BUSCO-SOCIO.

       600-FIN-PAGO.

       700-BUSCO-SOCIO.
           PERFORM 800-ARMO-CLAVE.
           PERFORM 810-POSICIONO-SOCIO.
       800-ARMO-CLAVE.
           MOVE w-pag-ant to soc-socio.
           MOVE ZERO to soc-mes.
       810-POSICIONO-SOCIO.
           START SOCIOS KEY IS = soc-llave
                   INVALID KEY
                   DISPLAY "NO ENCONTRE EL SOCIO"
                   NOT INVALID KEY
                   PERFORM 850-ACTUALIZO-SOCIO.
       850-ACTUALIZO-SOCIO.
           PERFORM 860-LEER-ARCH-SOCIO.
      *     PERFORM 870-MODIFICO-CUOTA-ZERO.
           PERFORM 880-INICIO-SOCIO.
           PERFORM 900-BUSCO-CUOTA.
      *     PERFORM 910-AGREGO-CUOTA.

       860-LEER-ARCH-SOCIO.
           READ SOCIOS NEXT AT END MOVE 1 TO w-flag-socios.
       870-MODIFICO-CUOTA-ZERO.
           MOVE soc-pagado to w-total-anterior.
           add w-importe-pagado to soc-pagado.
           IF soc-pagado=w-importe-vector
               MOVE "C" to soc-estado.
           REWRITE soc-reg.
       880-INICIO-SOCIO.
           MOVE soc-pagado to w-total-anterior.
           ADD w-mes-pagado to w-total-anterior.
           MOVE 1 to w-cuotas-pag.
           MOVE ZERO to w-total-pagado.
       900-BUSCO-CUOTA.
           PERFORM VARYING w-i FROM 1 BY 1 UNTIL w-i>12
           OR w-total-pagado = w-total-anterior
                ADD vec-mes(w-i) to w-total-pagado
                ADD 1 to w-cuotas-pag
           END-PERFORM
           COMPUTE w-mes-pagado= vec-mes(w-cuotas-pag)
           COMPUTE w-mes-nuevo=w-cuotas-pag
            COMPUTE w-importe-pagado=w-importe-pagado - w-mes-pagado
            DISPLAY w-mes-pagado
            DISPLAY w-mes-nuevo
            DISPLAY w-importe-pagado.



       910-AGREGO-CUOTA.

               MOVE w-pag-ant to soc-socio
               MOVE w-mes-nuevo to soc-mes
               MOVE "C" TO soc-estado
               MOVE w-mes-pagado to soc-cuota
               MOVE w-mes-pagado to soc-pagado
               MOVE ZERO to soc-debe
               WRITE soc-reg.

       1000-FIN-ACTUALIZAR.
           CLOSE PAGOS SOCIOS.
       END PROGRAM YOUR-PROGRAM-NAME.
