      ******************************************************************
      * Author: Breno Marques
      * Date: 25/01/2024
      * Purpose: listar alunos salvos num arquivo
      * Tectonics: cobc Linguagem: COBOL
      * Complexidade: C
      * UPDATE: 28/01/2024 - TRNASFORMADO DE PROGRAMA PARA MODULO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LISTMATE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
            INPUT-OUTPUT SECTION.
            FILE-CONTROL.
                SELECT MATERIA ASSIGN TO
                'D:\Curso 1 COBOL\Desafio modulo 3\CFP001S2.DAT'
                ORGANISATION IS INDEXED
                ACCESS  MODE IS SEQUENTIAL
                RECORD KEY IS ID-MATERIA
                FILE STATUS IS WS-FILES.

       DATA DIVISION.
       FILE SECTION.
       FD MATERIA.
          COPY CFPK0002.

       WORKING-STORAGE SECTION.
       01 WS-MATERIA                     PIC X(27) VALUE SPACES.
       01 FILLER REDEFINES WS-MATERIA.
          03 WS-ID-MATERIA                 PIC 9(03).
          03 WS-NM-MATERIA                 PIC X(20).
          03 WS-NT-APROVACAO               PIC 9(02)V9(02).
       77 WS-FILES                         PIC 99.
          88 FILES-OK                      VALUE 0.
       77 WS-EOF                           PIC X.
          88 EOF-OK                        VALUE 'S' FALSE 'N'.
       77 WS-EXIT                          PIC X.
          88 EXIT-OK                       VALUE 'F' FALSE 'N'.
       77 WS-CONT                          PIC 9(003) VALUE ZEROS.

       LINKAGE SECTION.
       01 LK-COM-AREA.
         03 LK-MENSAGEM                  PIC X(40).

       PROCEDURE DIVISION USING LK-COM-AREA.
       MAIN-PROCEDURE.
            DISPLAY LK-MENSAGEM
            SET EXIT-OK              TO FALSE
            PERFORM P1-LISTAR    THRU P1-FIM UNTIL EXIT-OK
            PERFORM P0-FIM


            .
       P1-LISTAR.
            SET EOF-OK                TO FALSE
            SET FILES-OK              TO TRUE
            SET WS-CONT               TO 0.

            OPEN INPUT MATERIA
            IF FILES-OK THEN
            PERFORM UNTIL EOF-OK
                IF FILES-OK THEN
                    READ MATERIA INTO WS-MATERIA

                         AT END
                            SET EOF-OK TO TRUE
                         NOT AT END
                             ADD 1     TO WS-CONT
                             DISPLAY'REGISTRO '
                                     WS-CONT
                                     ': '
                                     WS-ID-MATERIA
                                     ' - '
                             FUNCTION TRIM(WS-NM-MATERIA)
                                     ' - Nota de aprovacao: '
                                     WS-NT-APROVACAO


                    END-READ
                  END-PERFORM
            ELSE
                DISPLAY 'ERRO AO ABRIR ARQUIVO DE MATERIAS'
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
       END PROGRAM LISTMATE.
