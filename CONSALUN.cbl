      ******************************************************************
      * Author: Breno Marques
      * Date: 25/01/2024
      * Purpose: Consultar alunos salvos num arquivo
      * Tectonics: cobc Linguagem: COBOL
      * Complexidade: C
      * UPDATE: 28/01/2024 - TRNASFORMADO DE PROGRAMA PARA MODULO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSALUN.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
            INPUT-OUTPUT SECTION.
            FILE-CONTROL.
                SELECT ALUNO ASSIGN TO
                'D:\Curso 1 COBOL\Desafio modulo 3\CFP001S1.DAT'
                ORGANISATION IS INDEXED
                ACCESS  MODE IS RANDOM
                RECORD KEY IS ID-ALUNO
                FILE STATUS IS WS-FILES.

       DATA DIVISION.
       FILE SECTION.
       FD ALUNO.
          COPY CFPK0001.

       WORKING-STORAGE SECTION.
       01 WS-ALUNO                    PIC X(32) VALUE SPACES.
       01 FILLER REDEFINES WS-ALUNO.
          03 WS-ID-ALUNO                 PIC 9(03).
          03 WS-NM-ALUNO                 PIC X(20).
          03 WS-TL-ALUNO.
             05 WS-PREFIXO               PIC 9(05).
             05 WS-SUFIXO                PIC 9(04).
       77 WS-FILES                       PIC 99.
          88 FILES-OK                    VALUE 0.
       77 WS-EXIT                        PIC X.
          88 EXIT-OK                     VALUE 'F' FALSE 'N'.

       LINKAGE SECTION.
       01 LK-COM-AREA.
         03 LK-MENSAGEM                  PIC X(40).

       PROCEDURE DIVISION USING LK-COM-AREA.
       MAIN-PROCEDURE.
            DISPLAY LK-MENSAGEM
            SET EXIT-OK              TO FALSE
            PERFORM P1-CONSULTA    THRU P1-FIM UNTIL EXIT-OK
            PERFORM P0-FIM


            .
       P1-CONSULTA.
            SET FILES-OK                 TO TRUE

            OPEN INPUT ALUNO

            IF FILES-OK THEN
                DISPLAY 'Informe o numero de identificacao do aluno'
                ACCEPT ID-ALUNO


                READ ALUNO INTO WS-ALUNO
                  KEY IS ID-ALUNO
                  INVALID KEY
                    DISPLAY 'ALUNO NAO EXISTE!'
                  NOT INVALID KEY
                    DISPLAY WS-ID-ALUNO ' - ' FUNCTION TRIM(WS-NM-ALUNO)
                             ' - Tel: ' WS-PREFIXO '-' WS-SUFIXO
                END-READ


            ELSE
                DISPLAY 'ERRO AO ABRIR O ARQUIVO DE ALUNOS'
                DISPLAY 'FILE STATUS: ' WS-FILES
            END-IF

            CLOSE ALUNO

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
       END PROGRAM CONSALUN.
