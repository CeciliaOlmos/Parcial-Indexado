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
       01  w-pag-ant pic 9(5).
       01  w-soc-ant pic 9(5).
       01  w-imp-pag pic s9(8).
       01  w-imp-pagado pic s9(8).
       01  w-cuotas-pag pic 99.
       01  w-i pic 99.
       01  w-importe-anual pic 9(8).
       01  w-mes-pagado pic 9(6).
       01  w-mes-debe pic 9(6).
       01  w-estado pic x value "C".
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
               ADD vec-mes(w-i) TO w-importe-anual
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
           MOVE ZERO TO w-imp-pag.
       500-PROCESO-PAGO.
           add pag-importe to w-imp-pag.
       600-FIN-PAGO.
           PERFORM 700-BUSCO-SOCIO.
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
           IF soc-estado is = "C"
               DISPLAY "El socio ", soc-socio, " canceló todo"
           ELSE
               PERFORM 870-INICIO-SOCIO
               PERFORM UNTIL fin-arch-socios
               or soc-socio is not = w-soc-ant
                   ADD 1 TO w-cuotas-pag
                   PERFORM 860-LEER-ARCH-SOCIO
               END-PERFORM
               PERFORM 900-FIN-SOCIO
           END-IF.
       860-LEER-ARCH-SOCIO.
            READ SOCIOS NEXT AT END MOVE 1 TO w-flag-socios.
       870-INICIO-SOCIO.
           MOVE soc-socio to w-soc-ant.
           MOVE ZERO TO w-cuotas-pag.
           MOVE zero to w-mes-debe.
       900-FIN-SOCIO.
           MOVE w-imp-pag TO w-total-anterior.
           move w-soc-ant to soc-socio.
           COMPUTE soc-mes= w-cuotas-pag - 1.

           START SOCIOS key is = soc-llave
                   INVALID KEY
                   DISPLAY "no esta el mes"
                   not INVALID KEY
                   PERFORM 910-DIF-MES-ANTERIOR.


       915-CALCULAR-NUEVO-MES.
      *      MOVE w-imp-pag TO w-total-anterior.
           PERFORM VARYING w-i from 1 by 1 UNTIL
           w-cuotas-pag >12 or vec-mes(w-cuotas-pag) IS > w-imp-pag
               PERFORM 930-CALCULAR-IMPORTE
           END-PERFORM.
           PERFORM 935-RESTO-IMPORTE-PAGADO.
           PERFORM 950-BUSCAR-MES-CERO.
       930-CALCULAR-IMPORTE.
               MOVE vec-mes(w-cuotas-pag)to w-mes-pagado.
               COMPUTE w-imp-pag=w-imp-pag - w-mes-pagado.
               MOVE w-mes-pagado TO w-total-pagado.
               PERFORM 920-ACTUALIZAR-SOCIO.
           IF w-cuotas-pag is not =12
                add 1 TO w-cuotas-pag
            END-IF.
       910-DIF-MES-ANTERIOR.
           PERFORM 860-LEER-ARCH-SOCIO.
           IF soc-mes > 0
                   IF soc-debe > 0
                       move w-imp-pag to w-imp-pagado
                       COMPUTE w-imp-pag= w-imp-pag - soc-debe
                       MOVE vec-mes(soc-mes) TO soc-cuota
                       IF w-imp-pag >= 0
                           MOVE "C" TO soc-estado
                           MOVE vec-mes(soc-mes) TO soc-pagado
                           MOVE ZERO TO soc-debe
                           REWRITE soc-reg
                           PERFORM 915-CALCULAR-NUEVO-MES
                       ELSE
                         COMPUTE soc-pagado= soc-pagado + w-imp-pagado
                         COMPUTE soc-debe= soc-debe - w-imp-pagado
                         REWRITE soc-reg
                       END-IF
                   ELSE
                       PERFORM 915-CALCULAR-NUEVO-MES
                  END-IF
           ELSE
           PERFORM 915-CALCULAR-NUEVO-MES.
       935-RESTO-IMPORTE-PAGADO.
            IF vec-mes(w-cuotas-pag) IS > w-imp-pag
                   and w-imp-pag is not=0
                   PERFORM 940-PREPARAR-DATOS
             END-IF.
       940-PREPARAR-DATOS.
              MOVE w-imp-pag to w-total-pagado.
              MOVE vec-mes(w-cuotas-pag) TO w-mes-pagado
              COMPUTE w-mes-debe= w-mes-pagado - w-imp-pag.
               PERFORM 920-ACTUALIZAR-SOCIO.
       920-ACTUALIZAR-SOCIO.
             MOVE w-soc-ant to soc-socio.
             MOVE w-cuotas-pag to soc-mes.
             IF w-mes-debe=0
                MOVE "C" to soc-estado
              ELSE
                  MOVE "A" TO soc-estado
              END-IF.
             MOVE w-mes-pagado to soc-cuota.
             MOVE w-total-pagado  TO soc-pagado.
             MOVE w-mes-debe TO soc-debe.
             WRITE soc-reg.
       950-BUSCAR-MES-CERO.
           MOVE w-soc-ant to soc-socio.
           MOVE zero to soc-mes.
           READ SOCIOS INVALID KEY
                       DISPLAY "no encontre socio"
                       not INVALID KEY
                       PERFORM 960-ACTUALIZAR-MES-CERO.
       960-ACTUALIZAR-MES-CERO.

           ADD w-total-anterior TO soc-pagado.
           IF soc-pagado=w-importe-anual
               MOVE "C" TO soc-estado
           ELSE
               MOVE "A" TO soc-estado.
           MOVE ZERO TO soc-cuota.
           MOVE ZERO TO soc-debe.
           reWRITE soc-reg.

       1000-FIN-ACTUALIZAR.
           CLOSE PAGOS SOCIOS.
       END PROGRAM YOUR-PROGRAM-NAME.
