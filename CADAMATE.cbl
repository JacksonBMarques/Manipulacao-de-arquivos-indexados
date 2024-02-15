      ******************************************************************
      * Author: Breno Marques
      * Date: 25/01/2024
      * Purpose: cadastrar alunos e salvar num arquivo
      * Tectonics: cobc Linguagem: COBOL
      * Complexidade: C
      * UPDATE: 28/01/2024 - TRNASFORMADO DE PROGRAMA PARA MODULO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADAMATE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                DECIMAL-POINT IS COMMA.
                INPUT-OUTPUT SECTION.
            FILE-CONTROL.
                SELECT MATERIA ASSIGN TO
                'D:\Curso 1 COBOL\Desafio modulo 3\CFP001S2.DAT'
                ORGANISATION IS INDEXED
                ACCESS  MODE IS RANDOM
                RECORD KEY IS ID-MATERIA
                FILE STATUS IS WS-FILES.

       DATA DIVISION.
       FILE SECTION.
       FD MATERIA.
          COPY CFPK0002.
       WORKING-STORAGE SECTION.
       01 WS-MATERIA                      PIC X(27) VALUE SPACES.
       01 FILLER REDEFINES WS-MATERIA.
          03 WS-ID-MATERIA                PIC 9(03).
          03 WS-NM-MATERIA                PIC X(20).
          03 WS-NT-APROVACAO              PIC 9(02)V9(02).
       77 WS-FILES                        PIC 99.
          88 FILES-OK                     VALUE 0.
       77 WS-EXIT                         PIC X.
          88 EXIT-OK                      VALUE 'F' FALSE 'N'.

       LINKAGE SECTION.
       01 LK-COM-AREA.
         03 LK-MENSAGEM                   PIC X(40).

       PROCEDURE DIVISION USING LK-COM-AREA.
       MAIN-PROCEDURE.
            DISPLAY LK-MENSAGEM
            SET EXIT-OK                   TO FALSE
            PERFORM P1-CADASTRA           THRU P1-FIM  UNTIL EXIT-OK
            PERFORM P0-FIM

            .

       P1-CADASTRA.
            SET FILES-OK                  TO TRUE

            DISPLAY 'PARA REGISTRAR UMA MATERIA, INFORME: '
            DISPLAY 'Um numero para identificacao: '
            ACCEPT WS-ID-MATERIA
            DISPLAY 'Um nome para a materia: '
            ACCEPT WS-NM-MATERIA
            DISPLAY 'A nota de aprovavao na materia: '
            ACCEPT WS-NT-APROVACAO

            OPEN I-O MATERIA

            IF WS-FILES EQUAL 35 THEN
                OPEN OUTPUT MATERIA
            END-IF

            IF FILES-OK THEN
                     MOVE WS-ID-MATERIA           TO ID-MATERIA
                     MOVE WS-NM-MATERIA           TO NM-MATERIA
                     MOVE WS-NT-APROVACAO         TO NT-APROVACAO

                              WRITE REG-MATERIA
                           INVALID KEY
                              DISPLAY 'MATERIA JÁ CADASTRADA!'
                           NOT INVALID KEY
                              DISPLAY 'Materia cadastrada com sucesso!'
                     END-WRITE
            ELSE
                DISPLAY 'ERRO AO ABRIR O ARQUIVO DE MATERIAS'
                DISPLAY 'FILE STATUS: ' WS-FILES
            END-IF

            CLOSE MATERIA

            DISPLAY
              'TECLE: '
              '<QUALQUER TECLA> para continuar ou <F> para finalizar.'
              ACCEPT WS-EXIT
              IF WS-EXIT = 'f'
                       MOVE 'F'       TO WS-EXIT
              END-IF
            .
       P1-FIM.

       P0-FIM.
            GOBACK.
       END PROGRAM CADAMATE.
