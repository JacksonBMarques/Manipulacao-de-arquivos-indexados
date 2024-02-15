      ******************************************************************
      * Author: Breno Marques
      * Date: 25/01/2024
      * Purpose: Alaterar alunos salvos num arquivo
      * Tectonics: cobc Linguagem: COBOL
      * Complexidade: C
      * UPDATE: 28/01/2024 - TRNASFORMADO DE PROGRAMA PARA MODULO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTEMATE.

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
       01 WS-MATERIA                       PIC X(27) VALUE SPACES.
       01 FILLER REDEFINES WS-MATERIA.
          03 WS-ID-MATERIA                 PIC 9(03).
          03 WS-NM-MATERIA                 PIC X(20).
          03 WS-NT-APROVACAO               PIC 9(02)V9(02).
       77 WS-FILES                         PIC 99.
          88 FILES-OK                      VALUE 0.
       77 WS-EXIT                          PIC X.
          88 EXIT-OK                       VALUE 'F' FALSE 'N'.
       77 WS-CONFIRM                       PIC X VALUE SPACES.

       LINKAGE SECTION.
       01 LK-COM-AREA.
         03 LK-MENSAGEM                  PIC X(40).

       PROCEDURE DIVISION USING LK-COM-AREA.
       MAIN-PROCEDURE.
            DISPLAY LK-MENSAGEM
            SET EXIT-OK              TO FALSE
            PERFORM P1-ALTERAR    THRU P1-FIM UNTIL EXIT-OK
            PERFORM P0-FIM


            .
       P1-ALTERAR.
            SET FILES-OK              TO TRUE
            MOVE SPACES               TO WS-CONFIRM

            OPEN I-O MATERIA

            IF FILES-OK THEN
                DISPLAY 'Informe o numero de identificacao da materia'
                ACCEPT ID-MATERIA

                READ MATERIA INTO WS-MATERIA
                    KEY IS ID-MATERIA
                    INVALID KEY
                        DISPLAY 'MATERIA NAO EXISTE!'
                    NOT INVALID KEY
                        DISPLAY 'Nome atual: ' WS-NM-MATERIA
                        DISPLAY 'Informe novo nome'
                        ACCEPT NM-MATERIA
                        DISPLAY 'Nota de aprovacao: ' WS-NT-APROVACAO
                        DISPLAY 'Informe novo nota de aprovacao:'
                        ACCEPT NT-APROVACAO
                        DISPLAY 'TECLE '
                                '<S> Para confirmar ou <QUALQUER TECLA>'
                                ' para continuar com o atual.'
                        ACCEPT WS-CONFIRM
                        EVALUATE WS-MATERIA
                        WHEN 'S'
                           REWRITE REG-MATERIA
                           DISPLAY 'Materia atualizada com sucesso!'
                        WHEN 's'
                           REWRITE REG-MATERIA
                           DISPLAY 'Materia atualizada com sucesso!'
                        WHEN OTHER
                           DISPLAY 'Alteracao nao realizada!'
                        END-EVALUATE

                END-READ
            ELSE
                DISPLAY 'ERRO AO ABRIR O ARQUIVO DE MATERIAS'
                DISPLAY 'FILE STATUS: ' WS-FILES
            END-IF

            CLOSE MATERIA

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
       END PROGRAM ALTEMATE.
