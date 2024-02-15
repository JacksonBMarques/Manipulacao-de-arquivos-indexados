      ******************************************************************
      * Author: Breno Marques
      * Date: 25/01/2024
      * Purpose: MENU CHAMADOR PARA O BOLETIM
      * Tectonics: cobc Linguagem: COBOL
      * Complexidade: C
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENUBOLETIM.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-COM-AREA.
          03 WS-MENSAGEM                       PIC X(40).
       77 WS-OPCAO                             PIC X(02) VALUE SPACES.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            PERFORM P1-PROCESSA     THRU P1-FIM UNTIL WS-OPCAO = 'S'
            PERFORM P0-FIM
           .
       P1-PROCESSA.

            MOVE SPACES                        TO WS-OPCAO
            DISPLAY '--------------------------------------------------'
            DISPLAY '|               SISTEMA ESCOLAR                  |'
            DISPLAY '--------------------------------------------------'
            DISPLAY '**************************************************'
            DISPLAY '***************ESCOLHA UMA OPÇAO:*****************'
            DISPLAY '* 1- CADASTRAR ALUNOS |  6- CADASTRAR MATERIAS   *'
            DISPLAY '* 2- LISTAR ALUNOS    |  7- LISTAR MATERIAS      *'
            DISPLAY '* 3- CONSULTAR ALUNO  |  8- CONSULTAR MATERIAS   *'
            DISPLAY '* 4- ALTERAR ALUNO    |  9- ALTERAR MATERIAS     *'
            DISPLAY '* 5- EXCLUIR ALUNO    | 10- EXCLUIR MATERIAS     *'
            DISPLAY '*------------------------------------------------*'
            DISPLAY '* 11- CALCULAR MEDIA DO ALUNO                    *'
            DISPLAY '* 12- EXIBIR LISTA DE SITUACOES                  *'
            DISPLAY '* 13- EXIBIR LISTA DE APROVADOS                  *'
            DISPLAY '*           OU TECLE <S> PARA SAIR               *'
            DISPLAY '**************************************************'
            ACCEPT WS-OPCAO

            EVALUATE WS-OPCAO
                WHEN '1'
                  MOVE '*** CADASTRO DE ALUNOS ***' TO WS-MENSAGEM
                  CALL 'D:\Curso 1 COBOL\Desafio modulo 3\CADALUN'
                                                     USING WS-COM-AREA
                WHEN '2'
                  MOVE '*** LISTAGEM DE ALUNOS ***' TO WS-MENSAGEM
                  CALL 'D:\Curso 1 COBOL\Desafio modulo 3\LISTALUN'
                                                     USING WS-COM-AREA
                WHEN '3'
                  MOVE '*** CONSULTA DE ALUNOS ***' TO WS-MENSAGEM
                  CALL 'D:\Curso 1 COBOL\Desafio modulo 3\CONSALUN'
                                                     USING WS-COM-AREA
                WHEN '4'
                  MOVE '*** ALTERACAO DE ALUNOS ***' TO WS-MENSAGEM
                  CALL 'D:\Curso 1 COBOL\Desafio modulo 3\ALTALUN'
                                                     USING WS-COM-AREA
                WHEN '5'
                  MOVE '*** EXCLUSAO DE ALUNOS ***' TO WS-MENSAGEM
                  CALL 'D:\Curso 1 COBOL\Desafio modulo 3\DELEALUN'
                                                     USING WS-COM-AREA
                WHEN '6'
                  MOVE '*** CADASTRO DE MATERIAS ***' TO WS-MENSAGEM
                  CALL 'D:\Curso 1 COBOL\Desafio modulo 3\CADAMATE'
                                                     USING WS-COM-AREA
                WHEN '7'
                  MOVE '*** LISTAGEM DE MATERIAS ***' TO WS-MENSAGEM
                  CALL 'D:\Curso 1 COBOL\Desafio modulo 3\LISTMATE'
                                                     USING WS-COM-AREA
                WHEN '8'
                  MOVE '*** CONSULTA DE MATERIAS ***' TO WS-MENSAGEM
                  CALL 'D:\Curso 1 COBOL\Desafio modulo 3\CONSMATE'
                                                     USING WS-COM-AREA
                WHEN '9'
                  MOVE '*** ALTERACAO DE MATERIAS ***' TO WS-MENSAGEM
                  CALL 'D:\Curso 1 COBOL\Desafio modulo 3\ALTEMATE'
                                                     USING WS-COM-AREA
                WHEN '10'
                  MOVE '*** EXCLUSÃO DE MATERIAS ***' TO WS-MENSAGEM
                  CALL 'D:\Curso 1 COBOL\Desafio modulo 3\DELEMATE'
                                                     USING WS-COM-AREA
                WHEN '11'
                 MOVE '*** CALCULAR NOTAS ***'        TO WS-MENSAGEM
                 CALL 'D:\Curso 1 COBOL\Desafio modulo 3\NOTAALUN'
                                                     USING WS-COM-AREA
                WHEN '12'
                 MOVE '*** LISTA DE STUACOES ***'    TO WS-MENSAGEM
                 CALL 'D:\Curso 1 COBOL\Desafio modulo 3\LISTNOTA'
                                                     USING WS-COM-AREA
                WHEN '12'
                 MOVE '*** LISTA DE STUACOES ***'    TO WS-MENSAGEM
                 CALL 'D:\Curso 1 COBOL\Desafio modulo 3\LISTAPRO'
                                                     USING WS-COM-AREA
                WHEN 'S'
                  DISPLAY 'Obrigado e volte sempre!'

                WHEN 's'
                  MOVE 'S'                        TO WS-OPCAO
                  DISPLAY 'Obrigado e volte sempre!'

                WHEN OTHER
                  DISPLAY 'OPCAO INVALIDA!'




            END-EVALUATE
           .
       P1-FIM.

       P0-FIM.
            STOP RUN.
       END PROGRAM MENUBOLETIM.
