      ************************************************************
      *                      PROGRAM SHELL                       *
      * -------------------------------------------------------- *
      *  THE PROGRAM SHELL CONTAINS THE PARTS OF A PROGRAM THAT  *
      *  ARE COMMON TO ALL PROGRAMS. THESE COMMON PARTS INCLUDE  *
      *  THE FOUR DIVISION HEADINGS, THE HEADINGS FOR THE MAIN   *
      *  COMPONENTS OF EACH DIVISION, THE STANDARD PARAGRAPHS    *
      *  WITHIN THE PROCEDURE DIVISION AND COMMENTS FOR ALL      *
      *  MAJOR AREAS.                                            *
      ************************************************************
      *                      IDENTIFICATION DIVISION                   *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    330CURSO.
       AUTHOR.        CORITEL(FRANCISCO GIL S-C).
       DATE-WRITTEN.  OCT, 2016.
       DATE-COMPILED.
      ************************************************************
      *                   ENVIRONMENT DIVISION                   *
      ************************************************************
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER.      IBM-PC.
       OBJECT-COMPUTER.      IBM-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      ************************************************************
      *                   INPUT  FILE                            *
      *----------------------------------------------------------*
      *          FICHERO QUE CONTIENE LOS DATOS DE ENTRADA       *
      ************************************************************
       SELECT INPROPIET
               ASSIGN TO '340-PROP.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

       SELECT INACTUAL
               ASSIGN TO '340-ACTU.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

      ************************************************************
      *                   OUTPUT  FILE                           *
      *----------------------------------------------------------*
      * FICHERO DE SALIDA, EN ESTE CASO ES UNA COPIA DEL FICHERO *
      * DE ENTRADA 'INCURSO'                                     *
      ************************************************************
       SELECT INFORME
               ASSIGN TO '340INFORME.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.


      ************************************************************
      *                    DATA DIVISION                         *
      ************************************************************
       DATA DIVISION.
       FILE SECTION.

      ************************************************************
      *                    INPUT FILE                            *
      ************************************************************
       FD  INPROPIET
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 33 CHARACTERS
           DATA RECORD IS IN-PROPIET-REC.

       01  IN-PROPIET-REC                     PIC X(33).

       FD  INACTUAL
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 34 CHARACTERS
           DATA RECORD IS IN-ACTUAL-REC.

       01  IN-ACTUAL-REC                     PIC X(34).

      ************************************************************
      *                    OUTPUT FILE                           *
      ************************************************************

       FD  INFORME
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS OUT-INFORME-REC.

       01  OUT-INFORME-REC                  PIC X(80).
      ************************************************************
      *               WORKING-STORAGE SECTION                    *
      ************************************************************
       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
      ************************************************************
      *                     RECORD LAYOUTS                       *
      ************************************************************
           05  WS-INFORME-REG.
               10 WS-INF-NLOTE PIC 9(9).
               10 FILLER PIC X(2) VALUE SPACES.
               10 WS-INF-NOMBREPROP PIC X(16).
               10 FILLER PIC X VALUE SPACE.
               10 WS-INF-VALCAT PIC $ZZZZZ9,99.
               10 FILLER PIC X(2) VALUE SPACES.
               10 WS-INF-NUEVPROP PIC X(16).
               10 FILLER PIC X(2) VALUE SPACES.
               10 WS-INF-NUEVTAS PIC 9(9),99.
               10 FILLER PIC X(2) VALUE SPACES.
               10 WS-INF-CANCEL PIC X(3) VALUE ALL '*'.

           05  WS-PROPIET-REG.
               10 WS-PROP-NLOTE PIC 9(9).
               10 WS-PROP-NOMBREPROP PIC X(16).
               10 WS-PROP-TAS PIC S9(6)V99.

           05  WS-ACTUAL-REG.
               10 WS-ACT-NLOTE PIC 9(9).
               10 WS-ACT-CODIGO PIC 9.
               10 WS-ACT-NOMBREPROP PIC X(16).
               10 WS-ACT-NUEVTAS PIC S9(6)V99.

      ************************************************************
      *               REPORT LINES AND HEADINGS                  *
      ************************************************************
º          05 CABECERA1.
               10 FILLER PIC X(9) VALUE 'NUMERO'.
               10 FILLER PIC X(2) VALUE SPACES.
               10 FILLER PIC X(16) VALUE 'PROPIETARIO'.
               10 FILLER PIC X(2) VALUE SPACES.
               10 FILLER PIC X(9) VALUE 'VALOR'.
               10 FILLER PIC X(2) VALUE SPACES.
               10 FILLER PIC X(16) VALUE 'NUEVO'.
               10 FILLER PIC X(2) VALUE SPACES.
               10 FILLER PIC X(10) VALUE 'NUEVA'.
               10 FILLER PIC X(2) VALUE SPACES.
               10 FILLER PIC X(10) VALUE 'CANCELADO'.

           05 CABECERA2.
               10 FILLER PIC X(9) VALUE 'DE LOTE'.
               10 FILLER PIC X(2) VALUE SPACES.
               10 FILLER PIC X(16) VALUE SPACES.
               10 FILLER PIC X(2) VALUE SPACES.
               10 FILLER PIC X(9) VALUE 'CATASTRAL'.
               10 FILLER PIC X(2) VALUE SPACES.
               10 FILLER PIC X(16) VALUE 'PROPIETARIO'.
               10 FILLER PIC X(2) VALUE SPACES.
               10 FILLER PIC X(10) VALUE 'TASACIÓN'.
               10 FILLER PIC X(2) VALUE SPACES.
               10 FILLER PIC X(10) VALUE SPACES.

           05 CABECERA3.
               10 FILLER PIC X(9) VALUE '========'.
               10 FILLER PIC X(2) VALUE SPACES.
               10 FILLER PIC X(16) VALUE '=============='.
               10 FILLER PIC X(2) VALUE SPACES.
               10 FILLER PIC X(9) VALUE '========='.
               10 FILLER PIC X(2) VALUE SPACES.
               10 FILLER PIC X(16) VALUE '============='.
               10 FILLER PIC X(2) VALUE SPACES.
               10 FILLER PIC X(10) VALUE '========'.
               10 FILLER PIC X(2) VALUE SPACES.
               10 FILLER PIC X(10) VALUE '========='.



      ************************************************************
      *                     FLAGS/SWITCHES                       *
      ************************************************************
           05 SW-FIN-INPROPIET PIC X VALUE 'N'.
               88 FIN-INPROPIET VALUE 'Y'.

           05 SW-FIN-INACTUAL PIC X VALUE 'N'.
               88 FIN-INACTUAL VALUE 'Y'.
      ************************************************************
      *               COUNTERS AND ACCUMULATORS                  *
      ************************************************************

      ************************************************************
      *                CONSTANTS AND LITERALS                    *
      ************************************************************

      ************************************************************
      *                      SCRATCH AREA                        *
      ************************************************************

      ************************************************************
      *                    PROCEDURE DIVISION                    *
      ************************************************************
       PROCEDURE DIVISION.

      ************************************************************
      *                 1000 MAINLINE PARAGRAPH                  *
      * -------------------------------------------------------- *
      *  THE MAINLINE PARAGRAPH CONTROLS THE ORDER IN WHICH THE  *
      *  OTHER THREE MAJOR PARAGRAPHS ARE PROCESSED.             *
      ************************************************************
       1000-MAINLINE.
           PERFORM 2000-HOUSEKEEPING
              THRU 2000-HOUSEKEEPING-EXIT.

           PERFORM 3000-PROCESS
              THRU 3000-PROCESS-EXIT.

           PERFORM 8000-WRAP-UP
              THRU 8000-WRAP-UP-EXIT.

           STOP RUN.



      ************************************************************
      *              2000 HOUSEKEEPING PARAGRAPH                 *
      * -------------------------------------------------------- *
      *  THE HOUSEKEEPING PARAGRAPH HANDLES ALL THE NECESSARY    *
      *  STEPS TO PREPARE THE PROGRAM FOR PROCESSING. THESE      *
      *  STEPS MAY INCLUDE OPENING FILES, INITIALIZING COUNTERS  *
      *  AND ACCUMULATORS, READING THE FIRST RECORD, AND SO      *
      *  FORTH.                                                  *
      ************************************************************
       2000-HOUSEKEEPING.
           OPEN INPUT INPROPIET
                      INACTUAL
                OUTPUT INFORME.

           INITIALIZE WS-PROPIET-REG
                      WS-ACTUAL-REG
                      WS-INFORME-REG.

           READ INPROPIET INTO WS-PROPIET-REG
               AT END
                   SET FIN-INPROPIET TO TRUE.

           READ INACTUAL INTO WS-ACTUAL-REG
               AT END
                   SET FIN-INACTUAL TO TRUE.

       2000-HOUSEKEEPING-EXIT.
           EXIT.


      ************************************************************
      *                   3000 PROCESS PARAGRAPH                 *
      * -------------------------------------------------------- *
      *  THE PROCESS PARAGRAPH PROCESSES THE INPUT DATA TO       *
      *  PRODUCE THE REQUIRED OUTPUT.                            *
      ************************************************************
       3000-PROCESS.
           PERFORM 3100-MOSTRAR-CABECERA
               THRU 3100-MOSTRAR-CABECERA-EXIT

           PERFORM 3200-TRATAR-REGISTRO
               THRU 3200-TRATAR-REGISTRO-EXIT
             UNTIL FIN-INPROPIET.

       3000-PROCESS-EXIT.
           EXIT.
      ************************************************************
      *                 3100-MOSTRAR-CABEBCERA                   *
      * -------------------------------------------------------- *
      * EN ESTE PARRAFO IMPRIME LAS 3 CABECERAS DEL FICHERO      *
      * DE SALIDAS.                                              *
      *                                                          *
      ************************************************************
       3100-MOSTRAR-CABECERA.
           WRITE OUT-INFORME-REC FROM CABECERA1.
           WRITE OUT-INFORME-REC FROM CABECERA2 AFTER ADVANCING 1 LINE.
           WRITE OUT-INFORME-REC FROM CABECERA3 AFTER ADVANCING 1 LINE.

       3100-MOSTRAR-CABECERA-EXIT.
           EXIT.
      ************************************************************
      *                 3200-TRATAR-REGISTRO                     *
      * -------------------------------------------------------- *
      * EN ESTE PARRAFO SE CARGANO LOS RESGISTROS UNO A UNO      *
      * COPIANDO LAS VARIABLES DE ENTRADA AL ARCHIVO DE          *
      *  DE SALIDA HASTA FIN-FICHERO                             *
      *                                                          *
      ************************************************************
       3200-TRATAR-REGISTRO.

           INITIALIZE  WS-INFORME-REG.

           IF WS-PROP-NLOTE = WS-ACT-NLOTE
              PERFORM 3300-PROP-CON-ACTUAL
              THRU 3300-PROP-CON-ACTUAL-EXIT
           ELSE
               PERFORM 3210-PROP-SIN-ACTUAL
               THRU 3210-PROP-SIN-ACTUAL-EXIT
           END-IF

           WRITE OUT-INFORME-REC FROM WS-INFORME-REG
           AFTER ADVANCING 1 LINE

           READ INPROPIET INTO WS-PROPIET-REG
               AT END
                   SET FIN-INPROPIET TO TRUE.

       3200-TRATAR-REGISTRO-EXIT.
           EXIT.
      ************************************************************
      *                 3210-PROP-SIN-ACTUAL                     *
      ************************************************************
       3210-PROP-SIN-ACTUAL.

           INITIALIZE  WS-INFORME-REG

           MOVE WS-PROP-NLOTE TO WS-INF-NLOTE.
           MOVE WS-PROP-NOMBREPROP TO WS-INF-NOMBREPROP.
           MOVE WS-PROP-TAS TO WS-INF-VALCAT.

       3210-PROP-SIN-ACTUAL-EXIT.
           EXIT.
      ************************************************************
      *                 3300-TRATAR-PROP-CON-ACTUAL              *
      ************************************************************
       3300-PROP-CON-ACTUAL.

           PERFORM 3210-PROP-SIN-ACTUAL
           THRU 3210-PROP-SIN-ACTUAL-EXIT
           PERFORM 3400-EVALUAR-CODIGO
           THRU 3400-EVALUAR-CODIGO-EXIT.

           READ INACTUAL INTO WS-ACTUAL-REG
               AT END SET FIN-INACTUAL TO TRUE.


       3300-PROP-CON-ACTUAL-EXIT.
           EXIT.
      ************************************************************
      *                 3400-EVALUAR-CODIGO                      *
      ************************************************************
       3400-EVALUAR-CODIGO.
           EVALUATE WS-ACT-CODIGO
               WHEN 1
                   MOVE WS-ACT-NOMBREPROP TO WS-INF-NUEVPROP
               WHEN 2
                   MOVE WS-ACT-NUEVTAS TO WS-INF-NUEVTAS
               WHEN 3
                   MOVE WS-ACT-NUEVTAS TO WS-INF-NUEVTAS
                   MOVE WS-ACT-NOMBREPROP TO WS-INF-NUEVPROP
               WHEN 4
      *             DISPLAY 'lA VARIABLE ES: ' WS-ACT-NUEVTAS
                   ADD WS-ACT-NUEVTAS TO WS-PROP-TAS GIVING
                   WS-INF-NUEVTAS
               WHEN 5
                   MOVE '***' TO WS-INF-CANCEL.


       3400-EVALUAR-CODIGO-EXIT.
           EXIT.
      ************************************************************
      ************************************************************
      *                    8000 WRAP-UP PARAGRAPH                *
      * -------------------------------------------------------- *
      *  THE WRAP-UP PARAGRAPH HANDLES THE CLEANING UP FUNCTIONS *
      *  SUCH AS CLOSING THE FILES USED BY THE PROGRAM.          *
      ************************************************************
       8000-WRAP-UP.


       8000-WRAP-UP-EXIT.
           EXIT.
