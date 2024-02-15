      ******************************************************************

      * Author: Breno Marques
      * Date: 25/01/2024
      * Purpose: listar alunos salvos num arquivo
      * Tectonics: cobc Linguagem: COBOL
      * Complexidade: C
      * UPDATE: 28/01/2024 - TRNASFORMADO DE PROGRAMA PARA MODULO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LISTAPRO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
            INPUT-OUTPUT SECTION.
            FILE-CONTROL.
                SELECT APROVACAO1 ASSIGN TO
                'D:\Curso 1 COBOL\Desafio modulo 3\CFP001S4.DAT'
                ORGANISATION IS INDEXED
                ACCESS MODE IS SEQUENTIAL
                RECORD KEY IS ID-REGISTRO1
                FILE STATUS IS WS-FILES.

       DATA DIVISION.
       FILE SECTION.
       FD APROVACAO1.
          COPY CFPK0004.

       WORKING-STORAGE SECTION.
       01 WS-ALUNO                    PIC X(62) VALUE SPACES.
       01 FILLER REDEFINES WS-ALUNO.
          03 WS-ID-ALUNO2              PIC 9(03).
          03 WS-NM-ALUNO2              PIC X(20).
          03 WS-ID-MATERIA2            PIC 9(03).
          03 WS-NM-MATERIA2            PIC X(20).
          03 WS-ST-APROVACAO1          PIC X(09).
          03 WS-REGISTRO1              PIC 9(03).
          03 WS-MD-ALUNO1              PIC 9(02)V9(02).

       77 WS-FILES                           PIC 99.
          88 FILES-OK                        VALUE 0.
       77 WS-EOF                             PIC X.
          88 EOF-OK                          VALUE 'S' FALSE 'N'.
       77 WS-EXIT                            PIC X.
          88 EXIT-OK                         VALUE 'F' FALSE 'N'.
       77 WS-CONT                            PIC 9(003) VALUE ZEROS.


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

            OPEN INPUT APROVACAO1
            IF FILES-OK THEN
            PERFORM UNTIL EOF-OK
                IF FILES-OK THEN
                    READ APROVACAO1 INTO WS-ALUNO

                     AT END
                        SET EOF-OK TO TRUE
                     NOT AT END
                        ADD 1     TO WS-CONT
                        DISPLAY '*** SITUACAO DO ALUNO  ***'
                        DISPLAY '********** 'WS-CONT' *************'
                        DISPLAY 'ALUNO  : ' ID-ALUNO2 ' - '
                                       FUNCTION TRIM(NM-ALUNO2)
                        DISPLAY 'MATERIA: ' ID-MATERIA2 ' - '
                                       FUNCTION TRIM(NM-MATERIA2)
                        DISPLAY 'MEDIA  : ' MD-ALUNO1
                        DISPLAY 'STATUS : ' ST-APROVACAO1
                        DISPLAY '**********************************'
                        DISPLAY '                                  '

                    END-READ
                  END-PERFORM
            ELSE
                DISPLAY 'ERRO AO ABRIR ARQUIVO DE ALUNOS'
                DISPLAY 'FILE STATUS: ' WS-FILES
            END-IF

            CLOSE APROVACAO1

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
           END PROGRAM LISTAPRO.
