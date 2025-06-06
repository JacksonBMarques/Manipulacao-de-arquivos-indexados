      ******************************************************************

      * Author: Breno Marques
      * Date: 25/01/2024
      * Purpose: listar alunos salvos num arquivo
      * Tectonics: cobc Linguagem: COBOL
      * Complexidade: C
      * UPDATE: 28/01/2024 - TRNASFORMADO DE PROGRAMA PARA MODULO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LISTNTMT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
            INPUT-OUTPUT SECTION.
            FILE-CONTROL.
                SELECT APROVACAO ASSIGN TO
                'D:\Curso 1 COBOL\Desafio modulo 3\CFP001S3.DAT'
                ORGANISATION IS INDEXED
                ACCESS MODE IS RANDOM
                RECORD KEY IS ID-MATERIA1
                FILE STATUS IS WS-FILES.

       DATA DIVISION.
       FILE SECTION.
       FD APROVACAO.
          COPY CFPK0003.

       WORKING-STORAGE SECTION.
       01 WS-ALUNO                    PIC X(62) VALUE SPACES.
       01 FILLER REDEFINES WS-ALUNO.
          03 WS-ID-ALUNO1              PIC 9(03).
          03 WS-NM-ALUNO1              PIC X(20).
          03 WS-ID-MATERIA1            PIC 9(03).
          03 WS-NM-MATERIA1            PIC X(20).
          03 WS-ST-APROVACAO           PIC X(09).
          03 WS-REGISTRO               PIC 9(03).
          03 WS-MD-ALUNO               PIC 9(02)V9(02).

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

            OPEN INPUT APROVACAO
            IF FILES-OK THEN

                IF FILES-OK THEN
                DISPLAY 'Informe o numero de identificacao da materia'
                ACCEPT ID-MATERIA1
                    READ APROVACAO INTO WS-ALUNO
                    KEY IS WS-ID-MATERIA1
                     INVALID KEY
                        DISPLAY 'ERRO'

                        NOT INVALID KEY
                        DISPLAY '*** SITUACAO DO ALUNO  ***'
                        DISPLAY 'ALUNO  : ' ID-ALUNO1 ' - '
                                       FUNCTION TRIM(NM-ALUNO1)
                        DISPLAY 'MATERIA: ' ID-MATERIA1 ' - '
                                       FUNCTION TRIM(NM-MATERIA1)
                        DISPLAY 'MEDIA  : ' MD-ALUNO
                        DISPLAY 'STATUS : ' ST-APROVACAO
                        DISPLAY '**********************************'
                        DISPLAY '                                  '

                    END-READ
            ELSE
                DISPLAY 'ERRO AO ABRIR ARQUIVO DE ALUNOS'
                DISPLAY 'FILE STATUS: ' WS-FILES
            END-IF

            CLOSE APROVACAO

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
           END PROGRAM LISTNTMT.
