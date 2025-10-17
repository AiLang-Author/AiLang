       * ----------------------------------------------------------
       * CBDEM1 IS A SIMPLE EXAMPLE PROGRAM WHICH ADDS NEW EMPLOYEE
       * ROWS TO THE PERSONNEL DATA BASE. CHECKING IS DONE TO
       * INSURE THE INTEGRITY OF THE DATA BASE. EMPLOYEE NUMBERS
       * ARE AUTOMATICALLY SELECTED USING THE CURRENT MAXIMUM
       * EMPLOYEE NUMBER AS THE START. DUPLICATE NUMBERS ARE SKIPPED.
       * THE PROGRAM QUERIES THE USER FOR DATA AS FOLLOWS:
       *
       *		 Enter employee name  :
       *		 Enter employee job   :
       *		 Enter employee salary:
       *		 Enter employee dept  :
       *
       * TO EXIT THE PROGRAM, ENTER A CARRIAGE RETURN AT THE
       * PROMPT FOR EMPLOYEE NAME. IF THE ROW IS SUCCESSFULLY 
       * INSERTED, THE FOLLOWING IS PRINTED:
       *
       * ENAME added to DNAME department as employee # NNNNN
       *
       * THE MAXIMUM LENGTHS OF THE 'ENAME', 'JOB', AND 'DNAME'
       * COLUMNS WILL BE DETERMINED BY THE ODESCR CALL.
       *-----------------------------------------------------------
       
        IDENTIFICATION DIVISION.
        PROGRAM-ID.  CBDEM1.
        ENVIRONMENT DIVISION.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
       
        01  LDA.
            02   LDA-V2RC       PIC S9(4) COMP.
            02   FILLER         PIC X(10).
            02   LDA-RC         PIC S9(4) COMP.
            02   FILLER         PIC X(50).
        01  HDA                 PIC X(512).
       
        01  CURSOR-1.
            02   C-V2RC         PIC S9(4) COMP.
            02   C-TYPE         PIC S9(4) COMP.
            02   C-ROWS         PIC S9(9) COMP.
            02   C-OFFS         PIC S9(4) COMP.
            02   C-FNC          PIC S9(4) COMP.      
            02   C-RC           PIC S9(4) COMP.
            02   FILLER         PIC X(50).
        01  CURSOR-2.
            02   C-V2RC         PIC S9(4) COMP.
            02   C-TYPE         PIC S9(4) COMP.
            02   C-ROWS         PIC S9(9) COMP.
            02   C-OFFS         PIC S9(4) COMP.
            02   C-FNC          PIC S9(4) COMP.
            02   C-RC           PIC S9(4) COMP.
            02   FILLER         PIC X(50).
       
        77   USER-ID            PIC X(5)  VALUE "SCOTT".
        77   USER-ID-L          PIC S9(9) VALUE 5 COMP.
        77   PSW                PIC X(5)  VALUE "tiger".
        77   PSW-L              PIC S9(9) VALUE 5 COMP.
        77   CONN               PIC S9(9) VALUE 0 COMP.
        77   CONN-L             PIC S9(9) VALUE 0 COMP.
        77   CONN-MODE          PIC S9(9) VALUE 0 COMP.
        
        77   SQL-SEL            PIC X(38) VALUE
                "SELECT DNAME FROM DEPT WHERE DEPTNO=:1".
        77   SQL-SEL-L          PIC S9(9) VALUE 38 COMP.
       
        77   SQL-INS            PIC X(150) VALUE
                "INSERT INTO EMP (EMPNO,ENAME,JOB,SAL,DEPTNO)
       -        " VALUES (:EMPNO,:ENAME,:JOB,:SAL,:DEPTNO)".
        77   SQL-INS-L          PIC S9(9) VALUE 150 COMP.
       
        77   SQL-SELMAX         PIC X(33) VALUE
                "SELECT NVL(MAX(EMPNO),0) FROM EMP".
        77   SQL-SELMAX-L       PIC S9(9) VALUE 33 COMP.
       
        77   SQL-SELEMP         PIC X(26) VALUE
                "SELECT ENAME,JOB FROM EMP".
        77   SQL-SELEMP-L       PIC S9(9) VALUE 26 COMP.
        
        77   EMPNO              PIC S9(9) COMP.
        77   EMPNO-D            PIC ZZZZ9.
        77   ENAME              PIC X(12).
        77   JOB                PIC X(12).
        77   SAL                PIC X(10).
        77   DEPTNO             PIC X(10).
        77   FMT                PIC X(6).
        77   CBUF               PIC X(10).
        77   DNAME              PIC X(15).
       
        77   ENAME-L            PIC S9(9) VALUE 12 COMP.
        77   ENAME-SIZE         PIC S9(4) COMP.
        77   JOB-L              PIC S9(9) VALUE 12 COMP.
        77   JOB-SIZE           PIC S9(4) COMP.
        77   SAL-L              PIC S9(9) VALUE 10 COMP.
        77   DEPTNO-L           PIC S9(9) VALUE 10 COMP.
        77   DNAME-L            PIC S9(9) VALUE 15 COMP.
        77   DNAME-SIZE         PIC S9(4) COMP.
        77   EMPNO-N            PIC X(6) VALUE ":EMPNO".
        77   ENAME-N            PIC X(6) VALUE ":ENAME".
        77   JOB-N              PIC X(4) VALUE ":JOB".
        77   SAL-N              PIC X(4) VALUE ":SAL".
        77   DEPTNO-N           PIC X(7) VALUE ":DEPTNO".
        77   EMPNO-N-L          PIC S9(9) VALUE 6 COMP.
        77   ENAME-N-L          PIC S9(9) VALUE 6 COMP.
        77   JOB-N-L            PIC S9(9) VALUE 4 COMP.
        77   SAL-N-L            PIC S9(9) VALUE 4 COMP.
        77   DEPTNO-N-L         PIC S9(9) VALUE 7 COMP.
        
        77   INTEGER            PIC S9(9) COMP VALUE 3.
        77   ASC                PIC S9(9) COMP VALUE 1.
        77   ZERO-A             PIC S9(9) COMP VALUE 0.
        77   ZERO-B             PIC S9(4) COMP VALUE 0.
        77   ONE                PIC S9(9) COMP VALUE 1.
        77   TWO                PIC S9(9) COMP VALUE 2.
        77   FOUR               PIC S9(9) COMP VALUE 4.
        77   SIX                PIC S9(9) COMP VALUE 6.
        77   EIGHT              PIC S9(9) COMP VALUE 8.
        77   ERR-RC             PIC S9(4) COMP.
        77   ERR-FNC            PIC S9(4) COMP.
        77   ERR-RC-D           PIC ZZZ9.
        77   ERR-FNC-D          PIC ZZ9.
        77   MSGBUF             PIC X(160).
        77   MSGBUF-L           PIC S9(9) COMP VALUE 160. 
       
        77   ASK-EMP            PIC X(25) VALUE
                                  "Enter employee name: ".
        77   ASK-JOB            PIC X(25) VALUE
                                  "Enter employee job: ".
        77   ASK-SAL            PIC X(25) VALUE
                                  "Enter employee salary: ".
        77   ASK-DEPTNO         PIC X(25) VALUE
                                  "Enter employee dept: ".
       
        PROCEDURE DIVISION.
        BEGIN.
       
       *----------------------------------------------------------
       * CONNECT TO ORACLE IN NON-BLOCKING MODE.
       * HDA MUST BE INITIALIZED TO ALL ZEROS BEFORE CALL TO OLOG.
       *----------------------------------------------------------
       
            MOVE LOW-VALUES TO HDA.
        
            CALL "OLOG" USING LDA, HDA, USER-ID, USER-ID-L,
                  PSW, PSW-L, CONN, CONN-L, CONN-MODE.
            IF LDA-RC NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-STOP.
            DISPLAY "Connected to ORACLE as user ", USER-ID.
        
       *----------------------------------------------------------
       * OPEN THE CURSORS.
       *----------------------------------------------------------
        
            CALL "OOPEN" USING CURSOR-1, LDA.
            IF C-RC IN CURSOR-1 NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-LOGOF.
        
            CALL "OOPEN" USING CURSOR-2, LDA.
            IF C-RC IN CURSOR-2 NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-LOGOF.
       
       *----------------------------------------------------------
       * DISABLE AUTO-COMMIT.
       * NOTE: THE DEFAULT IS OFF, SO THIS COULD BE OMITTED.
       *----------------------------------------------------------
       
            CALL "OCOF" USING LDA.
            IF LDA-RC NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-CLOSE.
        
       *----------------------------------------------------------
       * RETRIEVE THE CURRENT MAXIMUM EMPLOYEE NUMBER.
       *----------------------------------------------------------
        
            CALL "OPARSE" USING CURSOR-1, SQL-SELMAX, SQL-SELMAX-L,
                  ZERO-A, TWO.
            IF C-RC IN CURSOR-1 NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-CLOSE.
           
            CALL "ODEFIN" USING CURSOR-1, ONE, EMPNO, FOUR,
                  INTEGER, ZERO-A, ZERO-B, FMT, ZERO-A, ZERO-A,
                  ZERO-B, ZERO-B.
            IF C-RC IN CURSOR-1 NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-CLOSE.
            CALL "OEXEC" USING CURSOR-1.   
            IF C-RC IN CURSOR-1 NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-CLOSE.
            CALL "OFETCH" USING CURSOR-1.
            IF C-RC IN CURSOR-1 NOT = 0
               IF C-RC IN CURSOR-1 NOT = 1403
                  PERFORM ORA-ERROR
                  GO TO EXIT-CLOSE
               ELSE 
                  MOVE 10 TO EMPNO.
       
       *----------------------------------------------------------
       * DETERMINE THE MAX LENGTH OF THE EMPLOYEE NAME AND
       * JOB TITLE.  PARSE THE SQL STATEMENT -
       * IT WILL NOT BE EXECUTED.
       * DESCRIBE THE TWO FIELDS SPECIFIED IN THE SQL STATEMENT.
       *----------------------------------------------------------
        
            CALL "OPARSE" USING CURSOR-1, SQL-SELEMP, SQL-SELEMP-L,
                  ZERO-A, TWO.
            IF C-RC IN CURSOR-1 NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-CLOSE.
        
            CALL "ODESCR" USING CURSOR-1, ONE, ENAME-SIZE, ZERO-B,
                  CBUF, ZERO-A, ZERO-A, ZERO-B, ZERO-B, ZERO-B.
            IF C-RC IN CURSOR-1 NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-CLOSE.
        
            CALL "ODESCR" USING CURSOR-1, TWO, JOB-SIZE, ZERO-B,
                  CBUF, ZERO-A, ZERO-A, ZERO-B, ZERO-B, ZERO-B.
            IF C-RC IN CURSOR-1 NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-CLOSE.
        
            IF ENAME-SIZE > ENAME-L
               DISPLAY "ENAME too large for buffer."
               GO TO EXIT-CLOSE.
            IF JOB-SIZE > JOB-L
               DISPLAY "JOB too large for buffer."
               GO TO EXIT-CLOSE.
        
        *----------------------------------------------------------
       * PARSE THE INSERT AND SELECT STATEMENTS.
       *----------------------------------------------------------
       
            CALL "OPARSE" USING CURSOR-1, SQL-INS, SQL-INS-L,
                  ZERO-A, TWO.
            IF C-RC IN CURSOR-1 NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-CLOSE.
            CALL "OPARSE" USING CURSOR-2, SQL-SEL, SQL-SEL-L,
                 ZERO-A, TWO.
            IF C-RC IN CURSOR-2 NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-CLOSE.
        
       *----------------------------------------------------------
       * BIND ALL SQL SUBSTITUTION VARIABLES.
       *----------------------------------------------------------
        
            CALL "OBNDRV" USING CURSOR-1, EMPNO-N, EMPNO-N-L,
                  EMPNO, FOUR, INTEGER, ZERO-A.
            IF C-RC IN CURSOR-1 NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-CLOSE.
        
            CALL "OBNDRV" USING CURSOR-1, ENAME-N, ENAME-N-L,
                 ENAME, ENAME-L, ASC.
            IF C-RC IN CURSOR-1 NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-CLOSE.
        
            CALL "OBNDRV" USING CURSOR-1, JOB-N, JOB-N-L,
                  JOB, JOB-L, ASC.
            IF C-RC IN CURSOR-1 NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-CLOSE.
        
            CALL "OBNDRV" USING CURSOR-1, SAL-N, SAL-N-L, SAL, 
                  SAL-L, ASC.
            IF C-RC IN CURSOR-1 NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-CLOSE.
        
            CALL "OBNDRV" USING CURSOR-1, DEPTNO-N, DEPTNO-N-L,
                  DEPTNO, DEPTNO-L, ASC.
            IF C-RC IN CURSOR-1 NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-CLOSE.
       
       *----------------------------------------------------------
       * BIND THE DEPTNO SUBSTITUTION VAR IN THE SELECT STATEMENT.
       *----------------------------------------------------------
       
            CALL "OBNDRN" USING CURSOR-2, ONE, DEPTNO,
                  DEPTNO-L, ASC.
            IF C-RC IN CURSOR-2 NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-CLOSE.
       *----------------------------------------------------------
       * DESCRIBE THE 'DNAME' COLUMN - ONLY THE LENGTH.
       *----------------------------------------------------------
       
            CALL "ODSC" USING CURSOR-2, ONE, DNAME-SIZE.
            IF C-RC IN CURSOR-2 NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-CLOSE.
        
            IF DNAME-SIZE > DNAME-L
               DISPLAY "DNAME is to large for buffer."
               GO TO EXIT-CLOSE.
        
       *----------------------------------------------------------
       * DEFINE THE BUFFER TO RECEIVE 'DNAME'.
       *----------------------------------------------------------
        
            CALL "ODEFIN" USING CURSOR-2, ONE, DNAME,
                  DNAME-L, ASC.
            IF C-RC IN CURSOR-2 NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-CLOSE.
        
       *----------------------------------------------------------
       * ASK THE USER FOR EMPLOYEE NAME, JOB, SAL, AND DEPTNO.
       *----------------------------------------------------------
        
        NEXT-EMP.
        
            DISPLAY ASK-EMP WITH NO ADVANCING.
            ACCEPT ENAME.
            IF ENAME = " "
               GO TO EXIT-CLOSE.
        
            DISPLAY ASK-JOB WITH NO ADVANCING.
            ACCEPT JOB.
        
            DISPLAY ASK-SAL WITH NO ADVANCING.
            ACCEPT SAL.
        
        ASK-DPT.
            DISPLAY ASK-DEPTNO WITH NO ADVANCING.
            ACCEPT DEPTNO.
       
       *----------------------------------------------------------
       * CHECK FOR A VALID DEPARTMENT NUMBER BY EXECUTING.
       * THE SELECT STATEMENT.
       *----------------------------------------------------------
       
        
            CALL "OEXEC" USING CURSOR-2.
            IF C-RC IN CURSOR-2 NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-CLOSE.
        
       *----------------------------------------------------------
       * FETCH THE ROWS - DEPTNO IS A PRIMARY KEY SO A MAX.
       * OF 1 ROW WILL BE FETCHED. IF CURSOR RETURN CODE IS 1403
       * THEN NO SUCH DEPARTMENT EXISTS.
       *----------------------------------------------------------
       
            MOVE SPACES TO DNAME.
            CALL "OFETCH" USING CURSOR-2.
            IF C-RC IN CURSOR-2 = 0 THEN GO TO ADD-ROW.
            IF C-RC IN CURSOR-2 = 1403
               DISPLAY "No such department."
               GO TO ASK-DPT.
       
       *----------------------------------------------------------
       * INCREMENT EMPNO BY 10.
       * EXECUTE THE INSERT STATEMENT.
       *----------------------------------------------------------
        
        ADD-ROW.
            ADD 10 TO EMPNO.
            IF EMPNO > 9999
               MOVE EMPNO TO EMPNO-D
               DISPLAY "Employee number " EMPNO-D " too large."
               GO TO EXIT-CLOSE.
            CALL "OEXEC" USING CURSOR-1.
            IF C-RC IN CURSOR-1 = 0 THEN GO TO PRINT-RESULT.
       
       *----------------------------------------------------------
       * IF THE RETURN CODE IS 1 (DUPLICATE VALUE IN INDEX),
       * THEN GENERATE THE NEXT POSSIBLE EMPLOYEE NUMBER.
       *----------------------------------------------------------
       
            IF C-RC IN CURSOR-1 = 1
               ADD 10 TO EMPNO
               GO TO ADD-ROW
            ELSE
               PERFORM ORA-ERROR
               GO TO EXIT-CLOSE.
          PRINT-RESULT.
            MOVE EMPNO TO EMPNO-D.
            DISPLAY ENAME " added to the " DNAME
              " department as employee number " EMPNO-D.
       
       *----------------------------------------------------------
       * THE ROW HAS BEEN ADDED - COMMIT THIS TRANSACTION.
       *----------------------------------------------------------
            CALL "OCOM" USING LDA.
            IF LDA-RC NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-CLOSE.
            GO TO NEXT-EMP.
        
       *----------------------------------------------------------
       * CLOSE CURSORS AND LOG OFF.
       *----------------------------------------------------------
        
        EXIT-CLOSE.
       
            CALL "OCLOSE" USING CURSOR-1.
            IF C-RC IN CURSOR-1 NOT = 0
               PERFORM ORA-ERROR.
        
            CALL "OCLOSE" USING CURSOR-2.
            IF C-RC IN CURSOR-2 NOT = 0
               PERFORM ORA-ERROR.
        
        EXIT-LOGOF.
        
            CALL "OLOGOF" USING LDA.
            IF LDA-RC NOT = 0
               PERFORM ORA-ERROR.
        
        EXIT-STOP.
        
            DISPLAY "End of the OCIDEMO1 program."
            STOP RUN.
       
       *----------------------------------------------------------
       * DISPLAY ORACLE ERROR NOTICE.
       *----------------------------------------------------------
        
        ORA-ERROR.
        
            IF LDA-RC NOT = 0
               DISPLAY "OLOGON error"
               MOVE LDA-RC TO ERR-RC
               MOVE "0" TO ERR-FNC
            ELSE IF C-RC IN CURSOR-1 NOT = 0
               MOVE C-RC IN CURSOR-1 TO ERR-RC
               MOVE C-FNC IN CURSOR-1 TO ERR-FNC
            ELSE
               MOVE C-RC IN CURSOR-2 TO ERR-RC
               MOVE C-FNC IN CURSOR-2 TO ERR-FNC.
            DISPLAY "ORACLE error" WITH NO ADVANCING.
            IF ERR-FNC NOT = 0
               MOVE ERR-FNC TO ERR-FNC-D
               DISPLAY " processing OCI function"
                    ERR-FNC-D "."
            ELSE
               DISPLAY ".".
            MOVE " " TO MSGBUF.
            CALL "OERHMS" USING LDA, ERR-RC, MSGBUF, MSGBUF-L.
            DISPLAY MSGBUF.
       
       CBDEM2.COB
       * CBDEM2.COB
       *
       * The program CBDEM2 accepts SQL statements from the
       * user at run time and processes them.
       * If the statement was a Data Definition Language (DDL),
       * Data Control Language (DCL), or Data Manipulation
       * Language (DML) statement, it is parsed and executed,
       * and the next statement is retrieved.  (Note that
       * performing the execute step for a DDL or DCL statement
       * is not necessary, but it does no harm, and simplifies
       * the program logic.)
       * If the statement was a query, the program describes
       * the select list, and defines output variables of the
       * appropriate type and size, depending on the internal
       * datatype of the select-list item.
       * Then, each row of the query is fetched, and the results
       * are displayed.
       * To keep the size of this example program to a
       * reasonable limit for this book, the following
       * restrictions are present:
       * (1) The SQL statement can contain only 25 elements (words
       *   and punctuation), and must be entered on a single line.
       *   There is no terminating ';'.
       * (2) A maximum of 8 bind (input) variables is permitted.
       *   Additional input variables are not bound, which will
       *   cause an error at execute time.  Input values must be
       *   enterable as character strings
       *   (numeric or alphanumeric).
       *   Placeholders for bind variables are :bv,
       *   as for OBNDRV.
       * (3) A maximum of 8 select-list items per table are
       *   described and defined.  Additional columns are
       *   not defined, which will cause unpredictable behavior
       *   at fetch time.
       * (4) Not all internal datatypes are handled for queries.
       *   Selecting a RAW or LONG column could cause problems.
       
        IDENTIFICATION DIVISION.
        PROGRAM-ID.  CBDEM2.
        ENVIRONMENT DIVISION.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
       
       * Logon, cursor, and host data areas.
        01  LDA.
            02      LDA-V2RC    PIC S9(4) COMP.
            02      FILLER      PIC X(10).
            02      LDA-RC      PIC S9(4) COMP.
            02      FILLER      PIC X(50).
        01  CDA.
            02      C-V2RC      PIC S9(4) COMP.
            02      C-TYPE      PIC S9(4) COMP.
            02      C-ROWS      PIC S9(9) COMP.
            02      C-OFFS      PIC S9(4) COMP.
            02      C-FNC       PIC S9(4) COMP.
            02      C-RC        PIC S9(4) COMP.
            02      FILLER      PIC X(50).
        01  HDA                 PIC X(512).
       
       * Error message variables for the OERHMS routine.
        01  MSGBUF              PIC X(256).
        01  MSGBUF-L            PIC S9(9) VALUE 256 COMP.
        01  ERR-FNC-D           PIC ZZZ.
       
       * Connect info.  Link single-task, or modify to use
       * SQL*Net connect string appropriate to your site.
        01  USER-ID             PIC X(5)  VALUE "SCOTT".
        01  USER-ID-L           PIC S9(9) VALUE 5 COMP.
        01  PSW                 PIC X(5)  VALUE "TIGER".
        01  PSW-L               PIC S9(9) VALUE 5 COMP.
        01  CONN                PIC S9(9) VALUE 0 COMP.
        01  CONN-L              PIC S9(9) VALUE 0 COMP.
        01  CONN-MODE           PIC S9(9) VALUE 0 COMP.
       
       
       
       * Parameters for OPARSE. 
        01  SQL-STMT            PIC X(132).
        01  SQLL                PIC S9(9) COMP.
        01  DEF-MODE            PIC S9(9) VALUE 1 COMP.
        01  NO-DEF-MODE         PIC S9(9) VALUE 0 COMP.
        01  V7-FLG              PIC S9(9) VALUE 2 COMP.
       
       * Parameters for OBNDRV.
        01  BVNX.
            03  BV-NAME         OCCURS 25 TIMES.
                05 BV-NAMEX     OCCURS 10 TIMES PIC X.
        01  BVVX.
            03  BV-VAL          OCCURS 10 TIMES PIC X(10).
        01  BV-VAL-L            PIC S9(9) VALUE 10 COMP.
        01  N-BV                PIC S9(9) COMP.
       
       * Parameters for ODESCR.  Note: some are two bytes (S9(4))
       * some are four bytes (S9(9)).
        01  DBSIZEX.
            03  DBSIZE          OCCURS 8 TIMES PIC S9(9) COMP.
        01  DBTYPEX.
            03  DBTYPE          OCCURS 8 TIMES PIC S9(4) COMP.
        01  NAMEX.
            03  DBNAME            OCCURS 8 TIMES PIC X(10).
        01  NAME-LX.
            03  NAME-L          OCCURS 8 TIMES PIC S9(9) COMP.
        01  DSIZEX.
            03  DSIZE           OCCURS 8 TIMES PIC S9(9) COMP.
        01  PRECX.
            03  PREC            OCCURS 8 TIMES PIC S9(4) COMP.
        01  SCALEX.
            03  SCALE           OCCURS 8 TIMES PIC S9(4) COMP.
        01  NULL-OKX.
            03  NULL-OK         OCCURS 8 TIMES PIC S9(4) COMP.
       
       * Parameters for ODEFIN.
        01  OV-CHARX.
            03  OV-CHAR         OCCURS 8 TIMES PIC X(10).
        01  OV-NUMX.
            03  OV-NUM          OCCURS 8 TIMES
                                   PIC S99999V99 COMP-3.
        01  INDPX.
            03  INDP            OCCURS 8 TIMES PIC S9(4) COMP.
        01  N-OV                PIC S9(9) COMP.
        01  N-ROWS              PIC S9(9) COMP.
        01  N-ROWS-D            PIC ZZZ9 DISPLAY.
        01  OV-CHAR-L           PIC S9(9) VALUE 10 COMP.
        01  SEVEN               PIC S9(9) VALUE 7 COMP.
        01  PACKED-DEC-L        PIC S9(9) VALUE 4 COMP.
        01  PACKED-DEC-T        PIC S9(9) VALUE 7 COMP.
        01  NUM-DISP            PIC ZZZZZ.ZZ.
        01  FMT                 PIC X(6) VALUE "08.+02".
        01  FMT-L               PIC S9(9) VALUE 6 COMP.
        01  FMT-NONE            PIC X(6).
       
       * Miscellaneous parameters.
        01  ZERO-A              PIC S9(9) VALUE 0 COMP.
        01  ZERO-B              PIC S9(9) VALUE 0 COMP.
        01  ZERO-C              PIC S9(4) VALUE 0 COMP.
        01  ONE                 PIC S9(9) VALUE 1 COMP.
        01  TWO                 PIC S9(9) VALUE 2 COMP.
        01  FOUR                PIC S9(9) VALUE 4 COMP.
        01  INDX                PIC S9(9) COMP.
        01  NAME-D8             PIC X(8).
        01  NAME-D10            PIC X(10).
        01  VARCHAR2-T          PIC S9(9) VALUE 1 COMP.
        01  NUMBER-T            PIC S9(9) VALUE 2 COMP.
        01  INTEGER-T           PIC S9(9) VALUE 3 COMP.
        01  DATE-T              PIC S9(9) VALUE 12 COMP.
        01  CHAR-T              PIC S9(9) VALUE 96 COMP.
        
        PROCEDURE DIVISION.
        BEGIN.
        
       * Connect to ORACLE in non-blocking mode.
       * HDA must be initialized to all zeros before call to OLOG.
            MOVE LOW-VALUES TO HDA.
              
            CALL "OLOG" USING LDA, HDA, USER-ID, USER-ID-L,
                  PSW, PSW-L, CONN, CONN-L, CONN-MODE.
       
       * Check for error, perform error routine if required.
            IF LDA-RC NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-STOP.
            DISPLAY "Logged on to ORACLE as user " USER-ID ".".
            DISPLAY "Type EXIT at SQL prompt to quit."
        
       * Open a cursor.  Only the first two parameters are
       * used, the remainder (for V2 compatibility) are ignored.
            CALL "OOPEN" USING CDA, LDA, USER-ID, ZERO-A,
                  ZERO-A, USER-ID, ZERO-A.
            IF C-RC IN CDA NOT = 0
               PERFORM ORA-ERROR
               GO TO EXIT-LOGOFF.
       
       * Process each SQL statement.
        STMT-LOOP.
            PERFORM DO-SQL-STMT.
            GO TO STMT-LOOP.
        EXIT-CLOSE.
            CALL "OCLOSE" USING CDA.
        EXIT-LOGOFF.
            CALL "OLOGOF" USING LDA.
        EXIT-STOP.
            STOP RUN.
       
       * Perform paragraphs.
        DO-SQL-STMT.
            MOVE " " TO SQL-STMT.
            DISPLAY " ".
            DISPLAY "SQL > " NO ADVANCING.
            ACCEPT SQL-STMT.
       
       * Get first word of statement.
            UNSTRING SQL-STMT DELIMITED BY ALL " "
                     INTO BV-NAME(1).
            IF (BV-NAME(1) = "exit" OR BV-NAME(1) = "EXIT")
               GO TO EXIT-CLOSE.
            MOVE 132 TO SQLL.
       
       * Use non-deferred parse, to catch syntax errors
       * right after the parse.
            CALL "OPARSE" USING CDA, SQL-STMT, SQLL,
                  NO-DEF-MODE, V7-FLG.
            IF C-RC IN CDA NOT = 0
               PERFORM ORA-ERROR
               GO TO DO-SQL-STMT.
            PERFORM BIND-VARS.
            DISPLAY " ".
            MOVE N-BV TO ERR-FNC-D.
            DISPLAY "There were" ERR-FNC-D 
                    " bind variables.".
       
       * Execute the statement.
            CALL "OEXN" USING CDA, ONE, ZERO-B.
            IF C-RC IN CDA NOT = 0
               PERFORM ORA-ERROR
               GO TO DO-SQL-STMT.
       
       * Describe the SQL statement, and define output
       * variables if it is a query.  Limit output variables
       * to eight.
            PERFORM DESCRIBE-DEFINE THRU DESCRIBE-DEFINE-EXIT.
            SUBTRACT 1 FROM N-OV.
            IF (N-OV > 0)
                MOVE N-OV TO ERR-FNC-D
                DISPLAY "There were" ERR-FNC-D
                        " define variables."
                DISPLAY " "
                PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > N-OV
                   IF (DBTYPE(INDX) NOT = 2)
                      MOVE DBNAME(INDX) TO NAME-D10
                      DISPLAY NAME-D10 NO ADVANCING
                   ELSE
                      MOVE DBNAME(INDX) TO NAME-D8
                      DISPLAY NAME-D8 NO ADVANCING
                   END-IF
                   DISPLAY " " NO ADVANCING
                END-PERFORM
                DISPLAY " "
                PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > N-OV
                   DISPLAY "--------" NO ADVANCING
                   IF DBTYPE(INDX) NOT = 2
                      DISPLAY "--" NO ADVANCING
                   END-IF
                   DISPLAY " " NO ADVANCING
                END-PERFORM
                DISPLAY " "
            END-IF.
       
       * If the statement was a query, fetch the rows and
       * display them.
            IF (C-TYPE IN CDA = 4)
               PERFORM FETCHN THRU FETCHN-EXIT
               MOVE N-ROWS TO N-ROWS-D
               DISPLAY " "
               DISPLAY N-ROWS-D " rows returned.".
       * End of DO-SQL-STMT.
       
        BIND-VARS.
            MOVE 0 TO N-BV.
            PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > 25
              MOVE " " TO BV-NAME(INDX)
            END-PERFORM.
            UNSTRING SQL-STMT
              DELIMITED BY "(" OR "," OR ";" OR "="
                        OR ")" OR ALL " "
                INTO BV-NAME(1)
                     BV-NAME(2)
                     BV-NAME(3)
                     BV-NAME(4)
                     BV-NAME(5)
                     BV-NAME(6)
                     BV-NAME(7)
                     BV-NAME(8)
                     BV-NAME(9)
                     BV-NAME(10)
                     BV-NAME(11)
                     BV-NAME(12)
                     BV-NAME(13)
                     BV-NAME(14)
                     BV-NAME(15)
                     BV-NAME(16)
                     BV-NAME(17)
                     BV-NAME(18)
                     BV-NAME(19)
                     BV-NAME(20)
                     BV-NAME(21)
                     BV-NAME(22)
                     BV-NAME(23)
                     BV-NAME(24)
                     BV-NAME(25).
       
       * Scan the words in the SQL statement.  If the
       * word begins with ':', it is a placeholder for
       * a bind variable.  Get a value for it (as a string)
       * and bind using the OBNDRV routine, datatype 1.
       
            MOVE 0 TO INDP(1).
            PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > 25
               IF BV-NAMEX(INDX,1) = ':'
                  ADD 1 TO N-BV
                  MOVE 0 TO SQLL
                  INSPECT BV-NAME(INDX) TALLYING SQLL
                     FOR CHARACTERS BEFORE INITIAL ' '
                  DISPLAY "Enter value for " BV-NAME(INDX) " --> "
                     NO ADVANCING
                  ACCEPT BV-VAL(N-BV)
                  CALL "OBNDRV" USING CDA, BV-NAME(INDX), SQLL,
                       BV-VAL(N-BV), BV-VAL-L, VARCHAR2-T,
                       ZERO-A, INDP(1), FMT-NONE, ZERO-A, ZERO-A
                  IF C-RC IN CDA NOT = 0
                     PERFORM ORA-ERROR
                     GO TO EXIT-CLOSE
                  ELSE
                     DISPLAY "Bound " BV-VAL(N-BV)
                  END-IF
               END-IF
            END-PERFORM.
        DESCRIBE-DEFINE.
            MOVE 0 TO N-OV.
            PERFORM 9 TIMES
               ADD 1 TO N-OV
               IF (N-OV > 8)
                  GO TO DESCRIBE-DEFINE-EXIT
               END-IF
               MOVE 10 TO NAME-L(N-OV)
               MOVE " " TO DBNAME(N-OV)
                     
               CALL "ODESCR" USING CDA, N-OV, DBSIZE(N-OV),
                     DBTYPE(N-OV),
                     DBNAME(N-OV), NAME-L(N-OV), DSIZE(N-OV),
                     PREC(N-OV), SCALE(N-OV), NULL-OK(N-OV)
       
       * Check for end of select list.
               IF (C-RC IN CDA = 1007)
                  GO TO DESCRIBE-DEFINE-EXIT
               END-IF
       
       * Check for error.
               IF (C-RC IN CDA NOT = 0)
                  PERFORM ORA-ERROR
                  GO TO DESCRIBE-DEFINE-EXIT
               END-IF
       
       * Define an output variable for the select-list item.
       * If it is a number, define a packed decimal variable,
       * and create a format string for it.
               IF (DBTYPE(N-OV) = 2)
                  CALL "ODEFIN" USING CDA, N-OV, OV-NUM(N-OV),
                       PACKED-DEC-L, PACKED-DEC-T, TWO,
                       INDP(N-OV), FMT, FMT-L, PACKED-DEC-T,
                       ZERO-C, ZERO-C
               ELSE
       
       * For all other types, convert to a VARCHAR2 of length 10.
                  CALL "ODEFIN" USING CDA, N-OV, OV-CHAR(N-OV),
                       OV-CHAR-L, VARCHAR2-T, ZERO-A, INDP(N-OV),
                       FMT, ZERO-A, ZERO-A, ZERO-C, ZERO-C
               END-IF
               IF (C-RC IN CDA NOT = 0)
                  PERFORM ORA-ERROR
                  GO TO DESCRIBE-DEFINE-EXIT
               END-IF
            END-PERFORM.
        DESCRIBE-DEFINE-EXIT.
        FETCHN.
        
            MOVE 0 TO N-ROWS.
            PERFORM 10000 TIMES
       
       * Clear any existing values from storage buffers
               MOVE SPACES TO OV-CHARX
               MOVE LOW-VALUES TO OV-NUMX
       
               CALL "OFETCH" USING CDA
       
       * Check for end of fetch ("no data found")
               IF C-RC IN CDA = 1403
                  GO TO FETCHN-EXIT
                END-IF
               IF C-RC IN CDA NOT = 0
                  PERFORM ORA-ERROR
                  GO TO FETCHN-EXIT
               END-IF
               ADD 1 TO N-ROWS
               PERFORM VARYING INDX FROM 1
                       BY 1 UNTIL INDX > N-OV
                  IF (DBTYPE(INDX) = 2)
                     MOVE OV-NUM(INDX) TO NUM-DISP
                     INSPECT NUM-DISP REPLACING ALL ".00" BY "   "
                     DISPLAY NUM-DISP NO ADVANCING
                  ELSE
                     DISPLAY OV-CHAR(INDX) NO ADVANCING
                  END-IF
                  DISPLAY " " NO ADVANCING
               END-PERFORM
               DISPLAY " "
            END-PERFORM.
            DISPLAY "LEAVING FETCHN...".
        FETCHN-EXIT.
       
       * Report an error.  Obtain the error message
       * text using the OERHMS routine.
        ORA-ERROR.
            IF LDA-RC IN LDA NOT = 0
               DISPLAY "OLOGON error"
               MOVE 0 TO C-FNC IN CDA
               MOVE LDA-RC IN LDA TO C-RC IN CDA.
            DISPLAY "ORACLE error " NO ADVANCING.
            IF C-FNC NOT = 0
               DISPLAY "processing OCI function" NO ADVANCING
               MOVE C-FNC IN CDA TO ERR-FNC-D
               DISPLAY ERR-FNC-D
            ELSE
               DISPLAY ":".
       
            MOVE " " TO MSGBUF.
            CALL "OERHMS" USING LDA, C-RC IN CDA, MSGBUF,MSGBUF-L.
            DISPLAY MSGBUF.
       
       CBDEM3.COB
        *  The program CBDEM3 creates a table called
        *  "VOICE_MAIL" that contains three fields:
        *  a message ID, and message length, and a LONG RAW
        *  column that contains a digitized voice
        *  message.  The program fills one row of the table with a
        *  (simulated) message, then plays the message by
        *  extracting 64 kB chunks of it using the OFLNG routine,
        *  and sending them to a (simulated) digital-to-analog
        *  (DAC) converter routine.
       
         IDENTIFICATION DIVISION.
         PROGRAM-ID.  CBDEM3.
         ENVIRONMENT DIVISION.
         DATA DIVISION.
         WORKING-STORAGE SECTION.
         01  LDA.
             02      LDA-V2RC    PIC S9(4) COMP.
             02      FILLER      PIC X(10).
             02      LDA-RC      PIC S9(4) COMP.
             02      FILLER      PIC X(50).
         01  CDA.
             02      C-V2RC      PIC S9(4) COMP.
             02      C-TYPE      PIC S9(4) COMP.
             02      C-ROWS      PIC S9(9) COMP.
             02      C-OFFS      PIC S9(4) COMP.
             02      C-FNC       PIC S9(4) COMP.
             02      C-RC        PIC S9(4) COMP.
             02      FILLER      PIC X(50).
         01  HDA                 PIC X(512).
       
         01  ERRMSG              PIC X(256).
         01  ERRMSG-L            PIC S9(9) VALUE 256 COMP.
         01  ERR-RC              PIC S9(9) COMP.
         01  ERR-FNC-D           PIC ZZ9.
         01  USER-ID             PIC X(5)  VALUE "SCOTT".
         01  USER-ID-L           PIC S9(9) VALUE 5 COMP.
         01  PSW                 PIC X(5)  VALUE "tiger".
         01  PSW-L               PIC S9(9) VALUE 5 COMP.
         01  CONN                PIC S9(9) VALUE 0 COMP.
         01  CONN-L              PIC S9(9) VALUE 0 COMP.
         01  CONN-MODE           PIC S9(9) VALUE 0 COMP.
         01  SQL-STMT            PIC X(132).
         01  SQLL                PIC S9(9) COMP.
         01  ZERO-A              PIC S9(9) VALUE 0 COMP.
         01  ZERO-B              PIC S9(9) VALUE 0 COMP.
         01  FMT                 PIC X(6).
       
        *  Establish a 200000 byte buffer.  (On most systems,
        *  including the VAX, a PIC 99 reserves one byte.)
         01  MSGX.
             02 MSG              OCCURS 200000 TIMES PIC 99.
         01  MSGX-L              PIC S9(9) VALUE 200000 COMP.
         01  MSG-L               PIC S9(9) COMP.
         01  MSG-L-D             PIC ZZZZZZ.
         01  MSG-ID              PIC S9(9) COMP.
         01  MSG-ID-L            PIC S9(9) VALUE 4 COMP.
         01  MSG-ID-D            PIC ZZZZ.
         01  LEN                 PIC 9(9) COMP.
         01  LEN-D               PIC ZZZZ9.
         01  INDX                PIC S9(9) COMP.
         01  INTEGER-T           PIC S9(9) VALUE 3 COMP.
         01  DEF-MODE            PIC S9(9) VALUE 1 COMP.
         01  LONG-RAW            PIC S9(9) VALUE 24 COMP.
         01  ONE                 PIC S9(9) VALUE 1 COMP.
         01  TWO                 PIC S9(9) VALUE 2 COMP.
         01  THREE               PIC S9(9) VALUE 3 COMP.
         01  ANSX.
             02      ANSWER      OCCURS 6 TIMES PIC X.
         01  VERSION-7           PIC S9(9) VALUE 2 COMP.
         01  INDP                PIC S9(4) COMP.
         01  RCODE               PIC S9(4) COMP.
         01  RLEN                PIC S9(4) COMP.
         01  RETL                PIC S9(9) COMP.
         01  OFF1                PIC S9(9) COMP.
        
         PROCEDURE DIVISION.
         BEGIN.
        
        *  Connect to ORACLE in non-blocking mode.
        *  HDA must be initialized to all zeros before call to OLOG.
             MOVE LOW-VALUES TO HDA.
         
             CALL "OLOG" USING LDA, HDA, USER-ID, USER-ID-L,
                   PSW, PSW-L, CONN, CONN-L, CONN-MODE.
             IF LDA-RC NOT = 0
                PERFORM ORA-ERROR
                GO TO EXIT-STOP.
             DISPLAY "Logged on to ORACLE as user ", USER-ID.
       
        *  Open a cursor.
             CALL "OOPEN" USING CDA, LDA, USER-ID, ZERO-A,
                   ZERO-A, USER-ID, ZERO-A.
             IF C-RC IN CDA NOT = 0
                PERFORM ORA-ERROR
                GO TO EXIT-LOGOFF.
       
        *  Drop the VOICE_MAIL table.
             DISPLAY "OK to drop VOICE_MAIL table (Y or N)? : "
        -  WITH NO ADVANCING.
             ACCEPT ANSX.
             IF (ANSWER(1) NOT = 'y' AND ANSWER(1) NOT = 'Y')
                DISPLAY "Exiting program now."
                GO TO EXIT-CLOSE.
             MOVE "DROP TABLE VOICE_MAIL" TO SQL-STMT.
             MOVE 132 TO SQLL.
       
        *  Call OPARSE with no deferred parse to execute the DDL
        *  statement immediately.
             CALL "OPARSE" USING CDA, SQL-STMT, SQLL,
                  ZERO-A, VERSION-7.
             IF C-RC IN CDA NOT = 0
                IF (C-RC IN CDA = 942)
                   DISPLAY "Table did not exist."
                ELSE
                   PERFORM ORA-ERROR
                   GO TO EXIT-LOGOFF
                END-IF
             ELSE
                DISPLAY "Table dropped."
             END-IF
       
        *  Create the VOICE_MAIL table anew.
             MOVE "CREATE TABLE VOICE_MAIL (MSG_ID NUMBER(6),
        -    "MSG_LEN NUMBER(12), MSG LONG RAW)" TO SQL-STMT.
             MOVE 132 TO SQLL.
       
        *  Non-deferred parse to execute the DDL SQL statement.
             DISPLAY "Table VOICE_MAIL " NO ADVANCING.
             CALL "OPARSE" USING CDA, SQL-STMT, SQLL,
                  ZERO-A, VERSION-7.
             IF C-RC IN CDA NOT = 0
                PERFORM ORA-ERROR
                GO TO EXIT-LOGOFF.
             DISPLAY "created.".
       
        *  Insert some data into the table.
             MOVE "INSERT INTO VOICE_MAIL VALUES (:1, :2, :3)"
                  TO SQL-STMT.
             MOVE 132 TO SQLL.
             CALL "OPARSE" USING CDA, SQL-STMT, SQLL,
                  ZERO-A, VERSION-7.
             IF C-RC IN CDA NOT = 0
                PERFORM ORA-ERROR
                GO TO EXIT-LOGOFF.
       
        *  Bind the inputs.
             MOVE 0 TO INDP.
             CALL "OBNDRN" USING CDA, ONE, MSG-ID, MSG-ID-L,
             INTEGER-T, ZERO-A, INDP, FMT, ZERO-A, ZERO-A.
             IF C-RC IN CDA NOT = 0
                PERFORM ORA-ERROR
                GO TO EXIT-LOGOFF.
             CALL "OBNDRN" USING CDA, TWO, MSG-L, MSG-ID-L,
                  INTEGER-T, ZERO-A, INDP, FMT, ZERO-A, ZERO-A.
             IF C-RC IN CDA NOT = 0
                PERFORM ORA-ERROR
                GO TO EXIT-LOGOFF.
             CALL "OBNDRN" USING CDA, THREE, MSGX, MSGX-L,
                  LONG-RAW, ZERO-A, INDP, FMT, ZERO-A, ZERO-A.
             IF C-RC IN CDA NOT = 0
                PERFORM ORA-ERROR
                GO TO EXIT-LOGOFF.
       
        *  Set input variables, then execute the INSERT statement.
             MOVE 100 TO MSG-ID.
             MOVE 200000 TO MSG-L.
             PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MSG-L
                MOVE 42 TO MSG(INDX)
             END-PERFORM.
             CALL "OEXN" USING CDA, ONE, ZERO-B.
             IF C-RC IN CDA NOT = 0
                PERFORM ORA-ERROR
                GO TO EXIT-LOGOFF.
             MOVE "SELECT MSG_ID, MSG_LEN, MSG FROM VOICE_MAIL
        -    " WHERE MSG_ID = 100" TO SQL-STMT.
       
        *  Call OPARSE in deferred mode to select a message.
             CALL "OPARSE" USING CDA, SQL-STMT, SQLL,
                  DEF-MODE, VERSION-7.
             IF C-RC IN CDA NOT = 0
                PERFORM ORA-ERROR
                GO TO EXIT-LOGOFF.
       
        *  Define the output variables.
             CALL "ODEFIN" USING CDA, ONE, MSG-ID,
                  MSG-ID-L, INTEGER-T, ZERO-A, ZERO-A, ZERO-A,
                  ZERO-A, ZERO-A, ZERO-A, ZERO-A.
             IF C-RC IN CDA NOT = 0
                PERFORM ORA-ERROR
                GO TO EXIT-LOGOFF.
             CALL "ODEFIN" USING CDA, TWO, MSG-L,
                  MSG-ID-L, INTEGER-T, ZERO-A, ZERO-A, ZERO-A, 
                  ZERO-A, ZERO-A, ZERO-A, ZERO-A.
             IF C-RC IN CDA NOT = 0
                PERFORM ORA-ERROR
                GO TO EXIT-LOGOFF.
             MOVE 100 TO MSG-ID-L.
       
       
             CALL "ODEFIN" USING CDA, THREE, MSGX,
                  MSG-ID-L, LONG-RAW, ZERO-A, INDP, ANSX, ZERO-A, ZERO-A,
                  RLEN, RCODE.
             IF C-RC IN CDA NOT = 0
                PERFORM ORA-ERROR
                GO TO EXIT-LOGOFF.
       
        *  Do the query, getting the message ID and just the first
        *  100 bytes of the message.  This query basically just sets
        *  the cursor to the right row.  The message contents are 
        *  fetched by the OFLNG routine.
             CALL "OEXFET" USING CDA, ONE, ZERO-A, ZERO-A.
             IF C-RC IN CDA NOT = 0
                PERFORM ORA-ERROR
                GO TO EXIT-LOGOFF.
             MOVE MSG-ID TO MSG-ID-D.
             DISPLAY " ".
             DISPLAY "Message " MSG-ID-D " is available.".
             MOVE MSG-L TO MSG-L-D.
             DISPLAY "The length is " MSG-L-D " bytes.".
             PERFORM VARYING OFF1 FROM 0 BY 65536
                   UNTIL MSG-L <= 0
                IF (MSG-L < 65536)
                   MOVE MSG-L TO LEN
                ELSE
                   MOVE 65536 TO LEN
                END-IF
                PERFORM PLAY-MSG THRU PLAY-MSG-EXIT
                SUBTRACT LEN FROM MSG-L
        *        IF (MSG-L < 0 OR MSG-L = 0)
        *           GO TO END-LOOP
        *        END-IF
             END-PERFORM.
         END-LOOP.
             DISPLAY " ".
             DISPLAY "End of message.".
         EXIT-CLOSE.
             CALL "OCLOSE" USING CDA.
         EXIT-LOGOFF.
             CALL "OLOGOF" USING LDA.
         EXIT-STOP.
             STOP RUN.
         PLAY-MSG.
             MOVE LEN TO LEN-D.
             DISPLAY "Playing " LEN-D " bytes.".
         PLAY-MSG-EXIT.
       
       
       
        * Report an error.  Obtain the error message
        * text using the OERHMS routine.
         ORA-ERROR.
             IF LDA-RC IN LDA NOT = 0
                DISPLAY "OLOGON error"
                MOVE 0 TO C-FNC IN CDA
                MOVE LDA-RC IN LDA TO C-RC IN CDA.
             DISPLAY "ORACLE error" NO ADVANCING.
             IF C-FNC NOT = 0
                DISPLAY " processing OCI function " NO ADVANCING
                MOVE C-FNC IN CDA TO ERR-FNC-D
                DISPLAY ERR-FNC-D
             ELSE
                DISPLAY ".".
       
             MOVE " " TO ERRMSG.
             CALL "OERHMS" USING LDA, C-RC IN CDA, ERRMSG, ERRMSG-L.
             DISPLAY ERRMSG.
       
       
       