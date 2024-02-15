      ******************************************************************
      * Author: Breno Marques
      * Date: 25/01/2024
      * Purpose: cadastrar alunos e salvar num arquivo
      * Tectonics: cobc Linguagem: COBOL
      * Complexidade: C
      * UPDATE: 28/01/2024
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NOTAALUN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.

            INPUT-OUTPUT SECTION.
            FILE-CONTROL.
      * ARQUIVO PARA SALVAR A SITUAÇÃO DE APROVAÇÃO
                SELECT APROVACAO ASSIGN TO
                'D:\Curso 1 COBOL\Desafio modulo 3\CFP001S3.DAT'
                ORGANISATION IS INDEXED
                ACCESS MODE IS SEQUENTIAL
                RECORD KEY IS ID-REGISTRO
                FILE STATUS IS WS-FS.

                SELECT APROVACAO1 ASSIGN TO
                'D:\Curso 1 COBOL\Desafio modulo 3\CFP001S4.DAT'
                ORGANISATION IS INDEXED
                ACCESS MODE IS SEQUENTIAL
                RECORD KEY IS ID-REGISTRO1
                FILE STATUS IS WS-FILS.


      * ARQUIVO DE CONSULTA DOS NOMES DOS ALUNOS PELA CHAVE
                SELECT ALUNO ASSIGN TO
                'D:\Curso 1 COBOL\Desafio modulo 3\CFP001S1.DAT'
                ORGANISATION IS INDEXED
                ACCESS MODE IS RANDOM
                RECORD KEY IS ID-ALUNO
                FILE STATUS IS WS-FLS.
      * ARQUIVO DE CONSULTA DOS NOMES DAS MATERIAS PELA CHAVE
                SELECT MATERIA ASSIGN TO
                'D:\Curso 1 COBOL\Desafio modulo 3\CFP001S2.DAT'
                ORGANISATION IS INDEXED
                ACCESS MODE IS RANDOM
                RECORD KEY IS ID-MATERIA
                FILE STATUS IS WS-FILES.

       DATA DIVISION.
       FILE SECTION.
       FD APROVACAO1.
          COPY CFPK0004.
       FD APROVACAO.
          COPY CFPK0003.
       FD ALUNO.
          COPY CFPK0001.
       FD MATERIA.
          COPY CFPK0002.



       WORKING-STORAGE SECTION.
      *VARIAVEIS DO CFPK0001
       01 WS-ALUNO                      PIC X(32) VALUE SPACES.
       01 FILLER REDEFINES WS-ALUNO.
          03 WS-ID-ALUNO                PIC 9(03).
          03 WS-NM-ALUNO                PIC X(20).
          03 TL-ALUNO.
             05 PREFIXO                 PIC 9(05).
             05 SUFIXO                  PIC 9(04).
      *VARIAVEIS DO CFPK0002
       01 WS-MATERIA                    PIC X(27) VALUE SPACES.
       01 FILLER REDEFINES WS-MATERIA.
          03 WS-ID-MATERIA              PIC 9(03).
          03 WS-NM-MATERIA              PIC X(20).
          03 WS-NT-APROVACAO            PIC 9(02)V9(02).
      *VARIAVEIS DO CFPK0003
       01 WS-ALUNO1                    PIC X(66) VALUE SPACES.
       01 FILLER REDEFINES WS-ALUNO1.
          03 WS-ID-ALUNO1              PIC 9(03).
          03 WS-NM-ALUNO1              PIC X(20).
          03 WS-ID-MATERIA1            PIC 9(03).
          03 WS-NM-MATERIA1            PIC X(20).
          03 WS-NT-APROVACAO1          PIC 9(02)V9(02).
          03 WS-ST-APROVACAO           PIC X(09).
          03 WS-ID-REGISTRO            PIC 9(03).
          03 WS-MD-ALUNO               PIC 9(02)V9(02).

       01 WS-ALUNO2                    PIC X(66) VALUE SPACES.
       01 FILLER REDEFINES WS-ALUNO2.
          03 WS-ID-ALUNO2              PIC 9(03).
          03 WS-NM-ALUNO2              PIC X(20).
          03 WS-ID-MATERIA2            PIC 9(03).
          03 WS-NM-MATERIA2            PIC X(20).
          03 WS-NT-APROVACAO2          PIC 9(02)V9(02).
          03 WS-ST-APROVACAO2          PIC X(09).
          03 WS-ID-REGISTRO2           PIC 9(03).
          03 WS-MD-ALUNO1              PIC 9(02)V9(02).
      *MEDIA DOS ALUNOS A SER CALCULADA PARA DEFINIR A ST-APROVAÇÃO


      *FILE STATUS DOS ARQUIVOS
       77 WS-FILS                      PIC 99.
          88 FILS-OK                   VALUE 0.
       77 WS-FS                        PIC 99.
          88 FS-OK                     VALUE 0.
       77 WS-FIS                       PIC 99.
          88 FIS-OK                    VALUE 0.
       77 WS-FILES                     PIC 99.
          88 FILES-OK                  VALUE 0.
       77 WS-FLS                       PIC 99.
          88 FLS-OK                    VALUE 0.
      *NOTAS A SEREM INPUTADAS PARA CALCULAR MEDIA
       77 WS-NOTA1                      PIC S9(02)V9(02).
       77 WS-NOTA2                      PIC S9(02)V9(02).
       77 WS-NOTA3                      PIC S9(02)V9(02).
       77 WS-NOTA4                      PIC S9(02)V9(02).
      *CONDICAO DE SAIDA DE LOOPINGS
       77 WS-EXIT                       PIC X.
          88 EXIT-OK                    VALUE 'F' FALSE 'N'.

       77 WS-EX                         PIC X.
          88 EX-OK                      VALUE 'F' FALSE 'N'.
       77 WS-EXI                        PIC X.
          88 EXI-OK                     VALUE 'F' FALSE 'N'.

       77 WS-EOF                         PIC X.
          88 EOF-OK                      VALUE 'S' FALSE 'N'.
      *CONTADOR PARA GERAR O REGISTRO QUE SERA A CHAVE INDEXADA NO ARQUIVO DE APROVACAO
       77 WS-CONT                        PIC 9(03) VALUE ZEROS.


       LINKAGE SECTION.
       01 LK-COM-AREA.
         03 LK-MENSAGEM                  PIC X(40).

       PROCEDURE DIVISION USING LK-COM-AREA.
       MAIN-PROCEDURE.
            DISPLAY LK-MENSAGEM
            SET FIS-OK                    TO TRUE
            SET FLS-OK                    TO TRUE
            SET FILES-OK                  TO TRUE
            SET FS-OK                     TO TRUE
            SET EXIT-OK                   TO FALSE
            SET EX-OK                     TO FALSE
            SET EOF-OK                    TO FALSE
            SET WS-CONT                   TO 0

            PERFORM P1-CONTROLE     THRU P-CONTROLE-FIM UNTIL EXIT-OK
            PERFORM P0-FIM

      *ORDEM DE EXECUCAO DOS PARAGRAFOS
             .
       P1-CONTROLE.

            PERFORM P4-GERA-CHAVE         THRU P4-GERA-CHAVE-FIM
            PERFORM P1-ALUNO              THRU P1-ALUNOFIM
            PERFORM P2-MATERIA            THRU P2-MATERIAFIM
            PERFORM P3-NOTAS              THRU P3-NOTAS-FIM
            PERFORM P5-REGISTRO           THRU P5-REGISTRO-FIM
            PERFORM P6-EVAPR              THRU P6-EVAPR-FIM
            PERFORM P8-LOOP               THRU P8-LOOP-FIM

            .
       P-CONTROLE-FIM.
      *CONSULTA DO ALUNO

       P4-GERA-CHAVE.

            SET EOF-OK                    TO FALSE
            SET WS-CONT                   TO 0.
            SET FS-OK                     TO TRUE

            OPEN INPUT APROVACAO
            IF WS-FS EQUAL 35
                 OPEN OUTPUT APROVACAO
            END-IF

            IF FIS-OK THEN
              PERFORM UNTIL EOF-OK
                    READ APROVACAO INTO WS-ALUNO1
                         AT END
                            ADD 1         TO WS-CONT
                            SET EOF-OK    TO TRUE
                            EXIT PERFORM
                         NOT AT END
                            ADD 1         TO WS-CONT
                            END-ADD
                    END-READ
              END-PERFORM
            ELSE
              DISPLAY 'ERRO AO ABRIR ARQUIVO DE ALUNOS'
              DISPLAY 'FILE STATUS: ' WS-FIS
            END-IF

             DISPLAY ' REGISTRO: ' WS-CONT

            CLOSE APROVACAO


            .
       P4-GERA-CHAVE-FIM.
       P1-ALUNO.
            OPEN INPUT ALUNO

            IF WS-FLS EQUAL 35 THEN
               OPEN OUTPUT ALUNO
            END-IF

            IF FLS-OK
                DISPLAY 'Informe o codigo de identificacao do aluno:'
                ACCEPT ID-ALUNO

            PERFORM UNTIL EX-OK
               READ ALUNO INTO WS-ALUNO
                   KEY IS ID-ALUNO
                   INVALID KEY
                       DISPLAY 'ALUNO NÃO EXISTE'
                   DISPLAY 'Informe o código de identificacao do aluno:'
                       ACCEPT ID-ALUNO
                   NOT INVALID KEY
                       DISPLAY 'Nome:' WS-ID-ALUNO ' - ' WS-NM-ALUNO
                       MOVE WS-ID-ALUNO   TO WS-ID-ALUNO1
                       MOVE WS-NM-ALUNO   TO WS-NM-ALUNO1
                       MOVE 'F'           TO WS-EX
               END-READ

            END-PERFORM
            END-IF
            CLOSE ALUNO
            .

       P1-ALUNOFIM.
      *CONSULTA DA MATERIA
       P2-MATERIA.
            OPEN INPUT MATERIA

            IF WS-FILES EQUAL 35 THEN
               OPEN OUTPUT MATERIA
            END-IF

            IF FILES-OK
                DISPLAY 'Informe o codigo de identificacao da materia:'
                ACCEPT ID-MATERIA

            PERFORM UNTIL EXI-OK
               READ MATERIA INTO WS-MATERIA
                   KEY IS ID-MATERIA
                   INVALID KEY
                       DISPLAY 'MATERIA NÃO EXISTE'
                       DISPLAY 'Informe o código de identificacao da'
                   ' materia novamente:'
                       ACCEPT ID-MATERIA
                   NOT INVALID KEY
                       DISPLAY WS-ID-MATERIA
                       ' - 'FUNCTION TRIM(WS-NM-MATERIA)
                       ' - Nota de aprovacao: ' WS-NT-APROVACAO

                       MOVE WS-ID-MATERIA   TO WS-ID-MATERIA1
                       MOVE WS-NM-MATERIA   TO WS-NM-MATERIA1
                       MOVE WS-NT-APROVACAO TO WS-NT-APROVACAO1
                       MOVE 'F' TO WS-EXI
                       END-READ

            END-PERFORM

            END-IF
            CLOSE MATERIA.

       P2-MATERIAFIM.
      *INPUT DE NOTAS E CALCULO DE MEDIA  E APROVACAO
       P3-NOTAS.

       P1-NOTA.

            INITIALISE WS-NOTA1.
            DISPLAY 'DIGITE A NOTA DO PRIMEIRO BIMESTRE: '
            ACCEPT WS-NOTA1
            IF NOT(WS-NOTA1 > 0 AND < 100) OR NOT WS-NOTA1 IS NUMERIC
               DISPLAY '***********************************************'
               DISPLAY '*     NOTAS INVALIDAS – TENTE NOVAMENTE       *'
               DISPLAY '***********************************************'
               PERFORM P1-NOTA
            END-IF
            .

       P2-NOTA.
            INITIALISE WS-NOTA2.
            DISPLAY 'DIGITE A NOTA DO SEGUNDO BIMESTRE: '
            ACCEPT WS-NOTA2
            IF NOT(WS-NOTA2 > 0 AND < 100) OR NOT WS-NOTA2 IS NUMERIC
               DISPLAY '***********************************************'
               DISPLAY '*     NOTAS INVALIDAS – TENTE NOVAMENTE       *'
               DISPLAY '***********************************************'
                PERFORM P2-NOTA
            END-IF
            .

       P3-NOTA.
            INITIALISE WS-NOTA3.
            DISPLAY 'DIGITE A NOTA DO TERCEIRO BIMESTRE: '
            ACCEPT WS-NOTA3
            IF NOT(WS-NOTA3 > 0 AND < 100) OR NOT WS-NOTA3 IS NUMERIC
               DISPLAY '***********************************************'
               DISPLAY '*     NOTAS INVALIDAS – TENTE NOVAMENTE       *'
               DISPLAY '***********************************************'
                PERFORM P3-NOTA
            END-IF
            .

       P4-NOTA.
            INITIALISE WS-NOTA4.
            DISPLAY 'DIGITE A NOTA DO QUARTO BIMESTRE: '
            ACCEPT WS-NOTA4
            IF NOT(WS-NOTA4 > 0 AND < 100) OR NOT WS-NOTA4 IS NUMERIC
               DISPLAY '***********************************************'
               DISPLAY '*     NOTAS INVALIDAS - TENTE NOVAMENTE       *'
               DISPLAY '***********************************************'
                PERFORM P4-NOTA
            END-IF
            .

       P5-CALCULO.

            INITIALISE WS-MD-ALUNO
            INITIALISE WS-ST-APROVACAO

            COMPUTE WS-MD-ALUNO = ((WS-NOTA1+WS-NOTA2+WS-NOTA3+WS-NOTA4)
                                                                   / 4)
            END-COMPUTE

            IF WS-MD-ALUNO GREATER THAN OR EQUAL WS-NT-APROVACAO
                     MOVE 'APROVADO'         TO WS-ST-APROVACAO
            DISPLAY WS-ST-APROVACAO

            ELSE
                     MOVE 'REPROVADO'         TO WS-ST-APROVACAO
             END-IF


            .
       P3-NOTAS-FIM.




       P5-REGISTRO.

            OPEN EXTEND APROVACAO.
            IF WS-FS EQUAL 35
                 OPEN OUTPUT APROVACAO
            END-IF.

            IF FS-OK
                 MOVE WS-ID-ALUNO1      TO ID-ALUNO1
                 MOVE WS-NM-ALUNO1      TO NM-ALUNO1
                 MOVE WS-ID-MATERIA1    TO ID-MATERIA1
                 MOVE WS-NM-MATERIA1    TO NM-MATERIA1
                 MOVE WS-ST-APROVACAO   TO ST-APROVACAO
                 MOVE WS-CONT           TO ID-REGISTRO
                 MOVE WS-MD-ALUNO       TO MD-ALUNO

                          WRITE REGALUNO
                     INVALID KEY
                         DISPLAY 'Erro ao escrever no arquivo APROVACAO'
                         DISPLAY 'Código de erro: ' WS-FS
                     NOT INVALID KEY
                         DISPLAY 'NOTAS CADASTRADAS COM SUCESSO!'
                 END-WRITE

                          CLOSE APROVACAO
            ELSE
                 DISPLAY 'Erro ao abrir o arquivo APROVACAO'
                 DISPLAY 'Código de erro: ' WS-FS
             END-IF.





       P5-REGISTRO-FIM.

       P6-EVAPR.
            EVALUATE WS-ST-APROVACAO
            WHEN 'APROVADO'
            PERFORM P7-REGAPR   THRU  P7-REGAPR-FIM
            END-EVALUATE

            .
       P6-EVAPR-FIM.

       P7-REGAPR.

             OPEN EXTEND APROVACAO1.
              IF WS-FILS EQUAL 35
                   OPEN OUTPUT APROVACAO1
              END-IF

                IF FILS-OK
                   MOVE WS-ID-ALUNO1      TO ID-ALUNO2
                   MOVE WS-NM-ALUNO1      TO NM-ALUNO2
                   MOVE WS-ID-MATERIA1    TO ID-MATERIA2
                   MOVE WS-NM-MATERIA1    TO NM-MATERIA2
                   MOVE WS-ST-APROVACAO   TO ST-APROVACAO1
                   MOVE WS-CONT           TO ID-REGISTRO1
                   MOVE WS-MD-ALUNO       TO MD-ALUNO1

                            WRITE APRALUNO
                       INVALID KEY
                           DISPLAY 'Erro ao escrever no arquivo'
                           DISPLAY 'Código de erro: ' WS-FILS
                       NOT INVALID KEY
                           DISPLAY 'APROVADOS SALVOS COM SUCESSO'
                   END-WRITE

                            CLOSE APROVACAO1
               ELSE
                 DISPLAY 'Erro ao abrir o arquivo APROVACAO'
                 DISPLAY 'Código de erro: ' WS-FILS
               END-IF
            .
       P7-REGAPR-FIM.

       P8-LOOP.
            DISPLAY '*** RESULTADO DO PROCESSAMENTO ***'
            DISPLAY 'ALUNO:    ' WS-NM-ALUNO
            DISPLAY 'MATERIA:  ' NM-MATERIA1
            DISPLAY 'MEDIA:    ' WS-MD-ALUNO
            DISPLAY 'STATUS:   ' WS-ST-APROVACAO
            DISPLAY '**********************************'

           DISPLAY
              'TECLE: '
              '<QUALQUER TECLA> para continuar ou <F> para finalizar.'
              ACCEPT WS-EXIT
              IF WS-EXIT = 'f'
                       MOVE 'F'       TO WS-EXIT
              END-IF
           .
       P8-LOOP-FIM.
       P0-FIM.

            GOBACK.
       END PROGRAM NOTAALUN.
