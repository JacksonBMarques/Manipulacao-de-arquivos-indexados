      ******************************************************************
      * Author: Breno Marques
      * Date: 25/01/2024
      * Purpose: listar alunos salvos num arquivo
      * Tectonics: cobc Linguagem: COBOL
      * Complexidade: C
      * UPDATE: 28/01/2024 - TRNASFORMADO DE PROGRAMA PARA MODULO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DELEALUN.

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
       01 WS-ALUNO                     PIC X(32) VALUE SPACES.
       01 FILLER REDEFINES WS-ALUNO.
          03 WS-ID-ALUNO               PIC 9(03).
          03 WS-NM-ALUNO               PIC X(20).
          03 WS-TL-ALUNO               PIC 9(03).
       77 WS-FILES                     PIC 99.
          88 FILES-OK                  VALUE 0.
       77 WS-EXIT                      PIC X.
          88 EXIT-OK                   VALUE 'F' FALSE 'N'.
       77 WS-CONFIRM                   PIC X VALUE SPACES.

       LINKAGE SECTION.
       01 LK-COM-AREA.
         03 LK-MENSAGEM                PIC X(40).

       PROCEDURE DIVISION USING LK-COM-AREA.
       MAIN-PROCEDURE.
            DISPLAY LK-MENSAGEM
            SET EXIT-OK              TO FALSE
            PERFORM P1-EXCLUIR    THRU P1-FIM UNTIL EXIT-OK
            PERFORM P0-FIM


            .
       P1-EXCLUIR.

            SET FILES-OK                 TO TRUE
            MOVE SPACES               TO WS-CONFIRM

            OPEN I-O ALUNO

            IF FILES-OK THEN
                DISPLAY 'Informe o numero de identificacao do aluno'
                ACCEPT ID-ALUNO

                READ ALUNO INTO WS-ALUNO
                    KEY IS ID-ALUNO
                    INVALID KEY
                        DISPLAY 'ALUNO NAO EXISTE!'
                    NOT INVALID KEY
                        DISPLAY 'Nome atual: ' WS-NM-ALUNO
                        DISPLAY 'TECLE'
                                '<S> Para confirmar ou <QUALQUER TECLA>'
                                ' para abortar.'
                        ACCEPT WS-CONFIRM
                        EVALUATE WS-CONFIRM
                        WHEN 'S'
                           DELETE ALUNO RECORD
                           DISPLAY 'Aluno excluido com sucesso!'
                        WHEN 's'
                           DELETE ALUNO RECORD
                           DISPLAY 'Aluno excluido com sucesso!'
                        WHEN OTHER
                           DISPLAY 'Aluno não excluido!'
                        END-EVALUATE
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
              IF WS-EXIT EQUAL 'f'
                       MOVE 'F'          TO WS-EXIT
              END-IF


            .

       P1-FIM.


       P0-FIM.
            GOBACK.
       END PROGRAM DELEALUN.
