*HEADER,COBOL,NC231A                                                            
000100 IDENTIFICATION DIVISION.                                         NC2314.2
000200 PROGRAM-ID.                                                      NC2314.2
000300     NC231A.                                                      NC2314.2
000400****************************************************************  NC2314.2
000500*                                                              *  NC2314.2
000600*    VALIDATION FOR:-                                          *  NC2314.2
000700*                                                              *  NC2314.2
000800*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2314.2
000900*                                                              *  NC2314.2
001000*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC2314.2
001100*                                                              *  NC2314.2
001200****************************************************************  NC2314.2
001300*                                                              *  NC2314.2
001400*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  NC2314.2
001500*                                                              *  NC2314.2
001600*        X-55  - SYSTEM PRINTER NAME.                          *  NC2314.2
001700*        X-82  - SOURCE COMPUTER NAME.                         *  NC2314.2
001800*        X-83  - OBJECT COMPUTER NAME.                         *  NC2314.2
001900*                                                              *  NC2314.2
002000****************************************************************  NC2314.2
002100*                                                              *  NC2314.2
002200*    PROGRAM NC231A USES FORMAT 1 OF THE "SEARCH" STATEMENT TO *  NC2314.2
002300*    ACCESS THREE AND SEVEN DIMENSIONAL TABLES.                *  NC2314.2
002400*    THE OPTIONAL "VARYING" PHRASE IS USED WITH AN IDENTIFIER. *  NC2314.2
002500*    THE SCOPE TERMINATOR "END-SEARCH" IS ALSO TESTED.         *  NC2314.2
002600*                                                              *  NC2314.2
002700****************************************************************  NC2314.2
002800 ENVIRONMENT DIVISION.                                            NC2314.2
002900 CONFIGURATION SECTION.                                           NC2314.2
003000 SOURCE-COMPUTER.                                                 NC2314.2
003100     XXXXX082.                                                    NC2314.2
003200 OBJECT-COMPUTER.                                                 NC2314.2
003300     XXXXX083.                                                    NC2314.2
003400 INPUT-OUTPUT SECTION.                                            NC2314.2
003500 FILE-CONTROL.                                                    NC2314.2
003600     SELECT PRINT-FILE ASSIGN TO                                  NC2314.2
003700     XXXXX055.                                                    NC2314.2
003800 DATA DIVISION.                                                   NC2314.2
003900 FILE SECTION.                                                    NC2314.2
004000 FD  PRINT-FILE.                                                  NC2314.2
004100 01  PRINT-REC PICTURE X(120).                                    NC2314.2
004200 01  DUMMY-RECORD PICTURE X(120).                                 NC2314.2
004300 WORKING-STORAGE SECTION.                                         NC2314.2
004400 77  SUB-1              PICTURE S99  VALUE ZERO.                  NC2314.2
004500 77  SUB-2              PICTURE 99   VALUE ZERO.                  NC2314.2
004600 77  SUB-3              PICTURE 99   VALUE ZERO.                  NC2314.2
004700 77  CON-7              PICTURE 99   VALUE 07.                    NC2314.2
004800 77  CON-10             PICTURE 99   VALUE 10.                    NC2314.2
004900 77  GRP-HOLD-AREA                PICTURE X(5) VALUE SPACES.      NC2314.2
005000 77  CON-5              PICTURE 99  VALUE 05.                     NC2314.2
005100 77  SEC-HOLD-AREA                PICTURE X(11) VALUE SPACES.     NC2314.2
005200 77  CON-6              PICTURE 99  VALUE 06.                     NC2314.2
005300 77  ELEM-HOLD-AREA               PICTURE X(15)  VALUE SPACES.    NC2314.2
005400 77  L1-HOLD                     PIC XX.                          NC2314.2
005500 77  L2-HOLD                     PIC XX.                          NC2314.2
005600 77  L3-HOLD                     PIC XX.                          NC2314.2
005700 77  L4-HOLD                     PIC XX.                          NC2314.2
005800 77  L5-HOLD                     PIC XX.                          NC2314.2
005900 77  L6-HOLD                     PIC XX.                          NC2314.2
006000 77  L7-HOLD                     PIC XX.                          NC2314.2
006100 77  N1                         PIC 99.                           NC2314.2
006200 77  N2                         PIC 99.                           NC2314.2
006300 77  N3                         PIC 99.                           NC2314.2
006400 77  N4                         PIC 99.                           NC2314.2
006500 77  N5                         PIC 99.                           NC2314.2
006600 77  N6                         PIC 99.                           NC2314.2
006700 77  N7                         PIC 99.                           NC2314.2
006800 01  GRP-NAME.                                                    NC2314.2
006900     02  FILLER              PICTURE XXX    VALUE "GRP".          NC2314.2
007000     02  ADD-GRP             PICTURE 99     VALUE 01.             NC2314.2
007100                                                                  NC2314.2
007200 01  SEC-NAME.                                                    NC2314.2
007300     02  FILLER              PICTURE X(5)   VALUE "SEC (".        NC2314.2
007400     02  SEC-GRP             PICTURE 99     VALUE 00.             NC2314.2
007500     02  FILLER              PICTURE X      VALUE ",".            NC2314.2
007600     02  ADD-SEC             PICTURE 99     VALUE 01.             NC2314.2
007700     02  FILLER              PICTURE X      VALUE ")".            NC2314.2
007800                                                                  NC2314.2
007900 01  ELEM-NAME.                                                   NC2314.2
008000     02  FILLER              PICTURE X(6)   VALUE "ELEM (".       NC2314.2
008100     02  ELEM-GRP            PICTURE 99     VALUE 00.             NC2314.2
008200     02  FILLER              PICTURE X      VALUE ",".            NC2314.2
008300     02  ELEM-SEC            PICTURE 99     VALUE 00.             NC2314.2
008400     02  FILLER              PICTURE X      VALUE ",".            NC2314.2
008500     02  ADD-ELEM            PICTURE 99     VALUE 01.             NC2314.2
008600     02  FILLER              PICTURE X      VALUE ")".            NC2314.2
008700                                                                  NC2314.2
008800                                                                  NC2314.2
008900 01  3-DIMENSION-TBL.                                             NC2314.2
009000     02  GRP-ENTRY OCCURS 10 TIMES INDEXED BY IDX-1.              NC2314.2
009100         03  ENTRY-1         PICTURE X(5).                        NC2314.2
009200         03  GRP2-ENTRY OCCURS 10 TIMES INDEXED BY IDX-2.         NC2314.2
009300             04  ENTRY-2     PICTURE X(11).                       NC2314.2
009400             04  GRP3-ENTRY OCCURS 10 TIMES INDEXED BY IDX-3.     NC2314.2
009500                 05  ENTRY-3 PICTURE X(15).                       NC2314.2
009600                                                                  NC2314.2
009700 01  7-DIMENSION-TBL.                                             NC2314.2
009800   02  GRP-7-1-ENTRY             OCCURS 2 INDEXED BY IX-1.        NC2314.2
009900     03  ENTRY-7-1               PIC XX.                          NC2314.2
010000     03  GRP-7-2-ENTRY           OCCURS 2 INDEXED BY IX-2.        NC2314.2
010100       04  ENTRY-7-2             PIC XX.                          NC2314.2
010200       04  GRP-7-3-ENTRY         OCCURS 2 INDEXED BY IX-3.        NC2314.2
010300         05  ENTRY-7-3           PIC XX.                          NC2314.2
010400         05  GRP-7-4-ENTRY       OCCURS 2 INDEXED BY IX-4.        NC2314.2
010500           06  ENTRY-7-4         PIC XX.                          NC2314.2
010600           06  GRP-7-5-ENTRY     OCCURS 2 INDEXED BY IX-5.        NC2314.2
010700             07  ENTRY-7-5       PIC XX.                          NC2314.2
010800             07  GRP-7-6-ENTRY   OCCURS 2 INDEXED BY IX-6.        NC2314.2
010900               08  ENTRY-7-6     PIC XX.                          NC2314.2
011000               08  GRP-7-7-ENTRY OCCURS 2 INDEXED BY IX-7.        NC2314.2
011100                 09  ENTRY-7-7   PIC XX.                          NC2314.2
011200                                                                  NC2314.2
011300 01  END-STMT.                                                    NC2314.2
011400     02  FILLER                   PICTURE X(7) VALUE "AT END ".   NC2314.2
011500     02  END-IDX                  PICTURE X(5) VALUE SPACES.      NC2314.2
011600     02  FILLER                   PICTURE XXX  VALUE " = ".       NC2314.2
011700     02  IDX-VALU                 PICTURE 99  VALUE 00.           NC2314.2
011800     02  FILLER                   PICTURE XXX  VALUE SPACES.      NC2314.2
011900                                                                  NC2314.2
012000 01  NOTE-1.                                                      NC2314.2
012100     02  FILLER                   PICTURE X(74)  VALUE            NC2314.2
012200     "NOTE 1 - CORRECT AND COMPUTED DATA ARE EQUAL BUT THE AT END NC2314.2
012300-     "PATH WAS TAKEN".                                           NC2314.2
012400     02  FILLER                   PICTURE X(46)  VALUE SPACE.     NC2314.2
012500                                                                  NC2314.2
012600 01  NOTE-2.                                                      NC2314.2
012700     02  FILLER                   PICTURE X(112)  VALUE           NC2314.2
012800     "NOTE 2 - CORRECT AND COMPUTED DATA ARE NOT EQUAL. THE COMPUTNC2314.2
012900-    "ED ENTRY WAS EXTRACTED FROM THE TABLE BY SUBSCRIPTS.".      NC2314.2
013000     02  FILLER                   PICTURE X(8)  VALUE SPACE.      NC2314.2
013100                                                                  NC2314.2
013200 01  TEST-RESULTS.                                                NC2314.2
013300     02 FILLER                   PIC X      VALUE SPACE.          NC2314.2
013400     02 FEATURE                  PIC X(20)  VALUE SPACE.          NC2314.2
013500     02 FILLER                   PIC X      VALUE SPACE.          NC2314.2
013600     02 P-OR-F                   PIC X(5)   VALUE SPACE.          NC2314.2
013700     02 FILLER                   PIC X      VALUE SPACE.          NC2314.2
013800     02  PAR-NAME.                                                NC2314.2
013900       03 FILLER                 PIC X(19)  VALUE SPACE.          NC2314.2
014000       03  PARDOT-X              PIC X      VALUE SPACE.          NC2314.2
014100       03 DOTVALUE               PIC 99     VALUE ZERO.           NC2314.2
014200     02 FILLER                   PIC X(8)   VALUE SPACE.          NC2314.2
014300     02 RE-MARK                  PIC X(61).                       NC2314.2
014400 01  TEST-COMPUTED.                                               NC2314.2
014500     02 FILLER                   PIC X(30)  VALUE SPACE.          NC2314.2
014600     02 FILLER                   PIC X(17)  VALUE                 NC2314.2
014700            "       COMPUTED=".                                   NC2314.2
014800     02 COMPUTED-X.                                               NC2314.2
014900     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          NC2314.2
015000     03 COMPUTED-N               REDEFINES COMPUTED-A             NC2314.2
015100                                 PIC -9(9).9(9).                  NC2314.2
015200     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         NC2314.2
015300     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     NC2314.2
015400     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     NC2314.2
015500     03       CM-18V0 REDEFINES COMPUTED-A.                       NC2314.2
015600         04 COMPUTED-18V0                    PIC -9(18).          NC2314.2
015700         04 FILLER                           PIC X.               NC2314.2
015800     03 FILLER PIC X(50) VALUE SPACE.                             NC2314.2
015900 01  TEST-CORRECT.                                                NC2314.2
016000     02 FILLER PIC X(30) VALUE SPACE.                             NC2314.2
016100     02 FILLER PIC X(17) VALUE "       CORRECT =".                NC2314.2
016200     02 CORRECT-X.                                                NC2314.2
016300     03 CORRECT-A                  PIC X(20) VALUE SPACE.         NC2314.2
016400     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      NC2314.2
016500     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         NC2314.2
016600     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     NC2314.2
016700     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     NC2314.2
016800     03      CR-18V0 REDEFINES CORRECT-A.                         NC2314.2
016900         04 CORRECT-18V0                     PIC -9(18).          NC2314.2
017000         04 FILLER                           PIC X.               NC2314.2
017100     03 FILLER PIC X(2) VALUE SPACE.                              NC2314.2
017200     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     NC2314.2
017300 01  CCVS-C-1.                                                    NC2314.2
017400     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PANC2314.2
017500-    "SS  PARAGRAPH-NAME                                          NC2314.2
017600-    "       REMARKS".                                            NC2314.2
017700     02 FILLER                     PIC X(20)    VALUE SPACE.      NC2314.2
017800 01  CCVS-C-2.                                                    NC2314.2
017900     02 FILLER                     PIC X        VALUE SPACE.      NC2314.2
018000     02 FILLER                     PIC X(6)     VALUE "TESTED".   NC2314.2
018100     02 FILLER                     PIC X(15)    VALUE SPACE.      NC2314.2
018200     02 FILLER                     PIC X(4)     VALUE "FAIL".     NC2314.2
018300     02 FILLER                     PIC X(94)    VALUE SPACE.      NC2314.2
018400 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       NC2314.2
018500 01  REC-CT                        PIC 99       VALUE ZERO.       NC2314.2
018600 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       NC2314.2
018700 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       NC2314.2
018800 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       NC2314.2
018900 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       NC2314.2
019000 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       NC2314.2
019100 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       NC2314.2
019200 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      NC2314.2
019300 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       NC2314.2
019400 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     NC2314.2
019500 01  CCVS-H-1.                                                    NC2314.2
019600     02  FILLER                    PIC X(39)    VALUE SPACES.     NC2314.2
019700     02  FILLER                    PIC X(42)    VALUE             NC2314.2
019800     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 NC2314.2
019900     02  FILLER                    PIC X(39)    VALUE SPACES.     NC2314.2
020000 01  CCVS-H-2A.                                                   NC2314.2
020100   02  FILLER                        PIC X(40)  VALUE SPACE.      NC2314.2
020200   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  NC2314.2
020300   02  FILLER                        PIC XXXX   VALUE             NC2314.2
020400     "4.2 ".                                                      NC2314.2
020500   02  FILLER                        PIC X(28)  VALUE             NC2314.2
020600            " COPY - NOT FOR DISTRIBUTION".                       NC2314.2
020700   02  FILLER                        PIC X(41)  VALUE SPACE.      NC2314.2
020800                                                                  NC2314.2
020900 01  CCVS-H-2B.                                                   NC2314.2
021000   02  FILLER                        PIC X(15)  VALUE             NC2314.2
021100            "TEST RESULT OF ".                                    NC2314.2
021200   02  TEST-ID                       PIC X(9).                    NC2314.2
021300   02  FILLER                        PIC X(4)   VALUE             NC2314.2
021400            " IN ".                                               NC2314.2
021500   02  FILLER                        PIC X(12)  VALUE             NC2314.2
021600     " HIGH       ".                                              NC2314.2
021700   02  FILLER                        PIC X(22)  VALUE             NC2314.2
021800            " LEVEL VALIDATION FOR ".                             NC2314.2
021900   02  FILLER                        PIC X(58)  VALUE             NC2314.2
022000     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2314.2
022100 01  CCVS-H-3.                                                    NC2314.2
022200     02  FILLER                      PIC X(34)  VALUE             NC2314.2
022300            " FOR OFFICIAL USE ONLY    ".                         NC2314.2
022400     02  FILLER                      PIC X(58)  VALUE             NC2314.2
022500     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC2314.2
022600     02  FILLER                      PIC X(28)  VALUE             NC2314.2
022700            "  COPYRIGHT   1985 ".                                NC2314.2
022800 01  CCVS-E-1.                                                    NC2314.2
022900     02 FILLER                       PIC X(52)  VALUE SPACE.      NC2314.2
023000     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              NC2314.2
023100     02 ID-AGAIN                     PIC X(9).                    NC2314.2
023200     02 FILLER                       PIC X(45)  VALUE SPACES.     NC2314.2
023300 01  CCVS-E-2.                                                    NC2314.2
023400     02  FILLER                      PIC X(31)  VALUE SPACE.      NC2314.2
023500     02  FILLER                      PIC X(21)  VALUE SPACE.      NC2314.2
023600     02 CCVS-E-2-2.                                               NC2314.2
023700         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      NC2314.2
023800         03 FILLER                   PIC X      VALUE SPACE.      NC2314.2
023900         03 ENDER-DESC               PIC X(44)  VALUE             NC2314.2
024000            "ERRORS ENCOUNTERED".                                 NC2314.2
024100 01  CCVS-E-3.                                                    NC2314.2
024200     02  FILLER                      PIC X(22)  VALUE             NC2314.2
024300            " FOR OFFICIAL USE ONLY".                             NC2314.2
024400     02  FILLER                      PIC X(12)  VALUE SPACE.      NC2314.2
024500     02  FILLER                      PIC X(58)  VALUE             NC2314.2
024600     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2314.2
024700     02  FILLER                      PIC X(13)  VALUE SPACE.      NC2314.2
024800     02 FILLER                       PIC X(15)  VALUE             NC2314.2
024900             " COPYRIGHT 1985".                                   NC2314.2
025000 01  CCVS-E-4.                                                    NC2314.2
025100     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      NC2314.2
025200     02 FILLER                       PIC X(4)   VALUE " OF ".     NC2314.2
025300     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      NC2314.2
025400     02 FILLER                       PIC X(40)  VALUE             NC2314.2
025500      "  TESTS WERE EXECUTED SUCCESSFULLY".                       NC2314.2
025600 01  XXINFO.                                                      NC2314.2
025700     02 FILLER                       PIC X(19)  VALUE             NC2314.2
025800            "*** INFORMATION ***".                                NC2314.2
025900     02 INFO-TEXT.                                                NC2314.2
026000       04 FILLER                     PIC X(8)   VALUE SPACE.      NC2314.2
026100       04 XXCOMPUTED                 PIC X(20).                   NC2314.2
026200       04 FILLER                     PIC X(5)   VALUE SPACE.      NC2314.2
026300       04 XXCORRECT                  PIC X(20).                   NC2314.2
026400     02 INF-ANSI-REFERENCE           PIC X(48).                   NC2314.2
026500 01  HYPHEN-LINE.                                                 NC2314.2
026600     02 FILLER  PIC IS X VALUE IS SPACE.                          NC2314.2
026700     02 FILLER  PIC IS X(65)    VALUE IS "************************NC2314.2
026800-    "*****************************************".                 NC2314.2
026900     02 FILLER  PIC IS X(54)    VALUE IS "************************NC2314.2
027000-    "******************************".                            NC2314.2
027100 01  CCVS-PGM-ID                     PIC X(9)   VALUE             NC2314.2
027200     "NC231A".                                                    NC2314.2
027300 PROCEDURE DIVISION.                                              NC2314.2
027400 CCVS1 SECTION.                                                   NC2314.2
027500 OPEN-FILES.                                                      NC2314.2
027600     OPEN     OUTPUT PRINT-FILE.                                  NC2314.2
027700     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   NC2314.2
027800     MOVE    SPACE TO TEST-RESULTS.                               NC2314.2
027900     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             NC2314.2
028000     GO TO CCVS1-EXIT.                                            NC2314.2
028100 CLOSE-FILES.                                                     NC2314.2
028200     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   NC2314.2
028300 TERMINATE-CCVS.                                                  NC2314.2
028400S    EXIT PROGRAM.                                                NC2314.2
028500STERMINATE-CALL.                                                  NC2314.2
028600     STOP     RUN.                                                NC2314.2
028700 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         NC2314.2
028800 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           NC2314.2
028900 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          NC2314.2
029000 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      NC2314.2
029100     MOVE "****TEST DELETED****" TO RE-MARK.                      NC2314.2
029200 PRINT-DETAIL.                                                    NC2314.2
029300     IF REC-CT NOT EQUAL TO ZERO                                  NC2314.2
029400             MOVE "." TO PARDOT-X                                 NC2314.2
029500             MOVE REC-CT TO DOTVALUE.                             NC2314.2
029600     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      NC2314.2
029700     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               NC2314.2
029800        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 NC2314.2
029900          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 NC2314.2
030000     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              NC2314.2
030100     MOVE SPACE TO CORRECT-X.                                     NC2314.2
030200     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         NC2314.2
030300     MOVE     SPACE TO RE-MARK.                                   NC2314.2
030400 HEAD-ROUTINE.                                                    NC2314.2
030500     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC2314.2
030600     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC2314.2
030700     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC2314.2
030800     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC2314.2
030900 COLUMN-NAMES-ROUTINE.                                            NC2314.2
031000     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2314.2
031100     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2314.2
031200     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        NC2314.2
031300 END-ROUTINE.                                                     NC2314.2
031400     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.NC2314.2
031500 END-RTN-EXIT.                                                    NC2314.2
031600     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2314.2
031700 END-ROUTINE-1.                                                   NC2314.2
031800      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      NC2314.2
031900      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               NC2314.2
032000      ADD PASS-COUNTER TO ERROR-HOLD.                             NC2314.2
032100*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   NC2314.2
032200      MOVE PASS-COUNTER TO CCVS-E-4-1.                            NC2314.2
032300      MOVE ERROR-HOLD TO CCVS-E-4-2.                              NC2314.2
032400      MOVE CCVS-E-4 TO CCVS-E-2-2.                                NC2314.2
032500      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           NC2314.2
032600  END-ROUTINE-12.                                                 NC2314.2
032700      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        NC2314.2
032800     IF       ERROR-COUNTER IS EQUAL TO ZERO                      NC2314.2
032900         MOVE "NO " TO ERROR-TOTAL                                NC2314.2
033000         ELSE                                                     NC2314.2
033100         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       NC2314.2
033200     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           NC2314.2
033300     PERFORM WRITE-LINE.                                          NC2314.2
033400 END-ROUTINE-13.                                                  NC2314.2
033500     IF DELETE-COUNTER IS EQUAL TO ZERO                           NC2314.2
033600         MOVE "NO " TO ERROR-TOTAL  ELSE                          NC2314.2
033700         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      NC2314.2
033800     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   NC2314.2
033900     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2314.2
034000      IF   INSPECT-COUNTER EQUAL TO ZERO                          NC2314.2
034100          MOVE "NO " TO ERROR-TOTAL                               NC2314.2
034200      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   NC2314.2
034300      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            NC2314.2
034400      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          NC2314.2
034500     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2314.2
034600 WRITE-LINE.                                                      NC2314.2
034700     ADD 1 TO RECORD-COUNT.                                       NC2314.2
034800Y    IF RECORD-COUNT GREATER 50                                   NC2314.2
034900Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          NC2314.2
035000Y        MOVE SPACE TO DUMMY-RECORD                               NC2314.2
035100Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  NC2314.2
035200Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             NC2314.2
035300Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     NC2314.2
035400Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          NC2314.2
035500Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          NC2314.2
035600Y        MOVE ZERO TO RECORD-COUNT.                               NC2314.2
035700     PERFORM WRT-LN.                                              NC2314.2
035800 WRT-LN.                                                          NC2314.2
035900     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               NC2314.2
036000     MOVE SPACE TO DUMMY-RECORD.                                  NC2314.2
036100 BLANK-LINE-PRINT.                                                NC2314.2
036200     PERFORM WRT-LN.                                              NC2314.2
036300 FAIL-ROUTINE.                                                    NC2314.2
036400     IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. NC2314.2
036500     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.NC2314.2
036600     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC2314.2
036700     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   NC2314.2
036800     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2314.2
036900     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC2314.2
037000     GO TO  FAIL-ROUTINE-EX.                                      NC2314.2
037100 FAIL-ROUTINE-WRITE.                                              NC2314.2
037200     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         NC2314.2
037300     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 NC2314.2
037400     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. NC2314.2
037500     MOVE   SPACES TO COR-ANSI-REFERENCE.                         NC2314.2
037600 FAIL-ROUTINE-EX. EXIT.                                           NC2314.2
037700 BAIL-OUT.                                                        NC2314.2
037800     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   NC2314.2
037900     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           NC2314.2
038000 BAIL-OUT-WRITE.                                                  NC2314.2
038100     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  NC2314.2
038200     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC2314.2
038300     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2314.2
038400     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC2314.2
038500 BAIL-OUT-EX. EXIT.                                               NC2314.2
038600 CCVS1-EXIT.                                                      NC2314.2
038700     EXIT.                                                        NC2314.2
038800 SECT-NC231A-001 SECTION.                                         NC2314.2
038900 TH-01-001.                                                       NC2314.2
039000     MOVE   "VI-2 1.3.4" TO ANSI-REFERENCE.                       NC2314.2
039100     PERFORM PARA-1 VARYING SUB-1 FROM 1 BY 1                     NC2314.2
039200         UNTIL SUB-1 EQUAL TO 11                                  NC2314.2
039300         AFTER SUB-2 FROM 1 BY 1 UNTIL SUB-2 EQUAL TO 11          NC2314.2
039400         AFTER SUB-3 FROM 1 BY 1 UNTIL SUB-3 EQUAL TO 11          NC2314.2
039500     GO TO CHECK-ENTRIES.                                         NC2314.2
039600                                                                  NC2314.2
039700 PARA-1.                                                          NC2314.2
039800     SET IDX-1 TO SUB-1.                                          NC2314.2
039900     SET IDX-2 TO SUB-2.                                          NC2314.2
040000     SET IDX-3 TO SUB-3.                                          NC2314.2
040100     SET ADD-GRP, SEC-GRP, ELEM-GRP TO IDX-1.                     NC2314.2
040200     MOVE GRP-NAME TO ENTRY-1 (IDX-1).                            NC2314.2
040300     SET ADD-SEC, ELEM-SEC TO IDX-2.                              NC2314.2
040400     MOVE SEC-NAME TO ENTRY-2 (IDX-1, IDX-2).                     NC2314.2
040500     SET ADD-ELEM TO IDX-3.                                       NC2314.2
040600     MOVE ELEM-NAME TO ENTRY-3 (IDX-1, IDX-2, IDX-3).             NC2314.2
040700                                                                  NC2314.2
040800 CHECK-ENTRIES.                                                   NC2314.2
040900     MOVE "SEARCH VARYING LEV 1" TO FEATURE.                      NC2314.2
041000     MOVE "CHECK-ENTRIES       " TO PAR-NAME.                     NC2314.2
041100     MOVE "GRP02" TO GRP-HOLD-AREA.                               NC2314.2
041200     MOVE 02 TO SUB-2.                                            NC2314.2
041300     MOVE 01 TO CON-5.                                            NC2314.2
041400     SET IDX-1 TO 01.                                             NC2314.2
041500     SEARCH GRP-ENTRY VARYING CON-5 AT END                        NC2314.2
041600         PERFORM GRP-FAIL-PARGRAPH                                NC2314.2
041700         GO TO LEVEL-1-TEST-2                                     NC2314.2
041800         WHEN ENTRY-1 (IDX-1) = GRP-HOLD-AREA NEXT SENTENCE.      NC2314.2
041900                                                                  NC2314.2
042000     PERFORM PASS-TH.                                             NC2314.2
042100     GO TO LEVEL-1-TEST-2.                                        NC2314.2
042200                                                                  NC2314.2
042300 GRP-FAIL-PARGRAPH.                                               NC2314.2
042400     MOVE GRP-HOLD-AREA TO CORRECT-A.                             NC2314.2
042500     IF ENTRY-1 (SUB-2) NOT EQUAL TO GRP-HOLD-AREA                NC2314.2
042600         MOVE ENTRY-1 (SUB-2) TO COMPUTED-A                       NC2314.2
042700         MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK  ELSE      NC2314.2
042800     MOVE "IDX-1" TO END-IDX                                      NC2314.2
042900     SET IDX-VALU TO IDX-1                                        NC2314.2
043000     MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK                NC2314.2
043100     MOVE END-STMT TO COMPUTED-A.                                 NC2314.2
043200                                                                  NC2314.2
043300     PERFORM FAIL-TH.                                             NC2314.2
043400 LEVEL-1-TEST-2.                                                  NC2314.2
043500     MOVE "LEVEL-1-TEST-2      " TO PAR-NAME.                     NC2314.2
043600     MOVE "GRP01" TO GRP-HOLD-AREA.                               NC2314.2
043700     MOVE 01 TO SUB-2.                                            NC2314.2
043800     MOVE 01 TO CON-5.                                            NC2314.2
043900     SET IDX-1 TO 01.                                             NC2314.2
044000     SEARCH GRP-ENTRY VARYING CON-5 AT END                        NC2314.2
044100         PERFORM GRP-FAIL-PARGRAPH                                NC2314.2
044200         GO TO LEVEL-1-TEST-3                                     NC2314.2
044300         WHEN ENTRY-1 (IDX-1) = GRP-HOLD-AREA NEXT SENTENCE.      NC2314.2
044400                                                                  NC2314.2
044500     PERFORM PASS-TH.                                             NC2314.2
044600 LEVEL-1-TEST-3.                                                  NC2314.2
044700     MOVE "LEVEL-1-TEST-3      " TO PAR-NAME.                     NC2314.2
044800     MOVE "GRP10" TO GRP-HOLD-AREA.                               NC2314.2
044900     MOVE 10 TO SUB-2.                                            NC2314.2
045000     MOVE 01 TO CON-5.                                            NC2314.2
045100     SET IDX-1 TO 01.                                             NC2314.2
045200     SEARCH GRP-ENTRY VARYING CON-5 AT END                        NC2314.2
045300         PERFORM GRP-FAIL-PARGRAPH                                NC2314.2
045400         GO TO LEVEL-1-TEST-4                                     NC2314.2
045500         WHEN ENTRY-1 (IDX-1) = GRP-HOLD-AREA NEXT SENTENCE.      NC2314.2
045600                                                                  NC2314.2
045700     PERFORM PASS-TH.                                             NC2314.2
045800 LEVEL-1-TEST-4.                                                  NC2314.2
045900     MOVE "LEVEL-1-TEST-4      " TO PAR-NAME.                     NC2314.2
046000     MOVE "GRP05" TO GRP-HOLD-AREA.                               NC2314.2
046100     MOVE 05 TO SUB-2.                                            NC2314.2
046200     MOVE 05 TO CON-5.                                            NC2314.2
046300     SET IDX-1 TO 05.                                             NC2314.2
046400     SEARCH GRP-ENTRY VARYING CON-5 WHEN ENTRY-1 (CON-5)          NC2314.2
046500         EQUAL TO GRP-HOLD-AREA GO TO PASS-TH-TEST-4.             NC2314.2
046600     PERFORM GRP-FAIL-PARGRAPH.                                   NC2314.2
046700     GO TO LEVEL-2-TEST-1.                                        NC2314.2
046800 PASS-TH-TEST-4.                                                  NC2314.2
046900                                                                  NC2314.2
047000     PERFORM PASS-TH.                                             NC2314.2
047100                                                                  NC2314.2
047200 LEVEL-2-TEST-1.                                                  NC2314.2
047300     MOVE "SEARCH VARYING LEV 2" TO FEATURE.                      NC2314.2
047400     MOVE "LEVEL-2-TEST-1      " TO PAR-NAME.                     NC2314.2
047500     MOVE "SEC (01,01)" TO SEC-HOLD-AREA.                         NC2314.2
047600     MOVE  1 TO SUB-1  SUB-2.                                     NC2314.2
047700     SET IDX-1 IDX-2 TO 01.                                       NC2314.2
047800     MOVE 01 TO CON-6.                                            NC2314.2
047900     SEARCH GRP2-ENTRY VARYING CON-6 AT END                       NC2314.2
048000         PERFORM SEC-FAIL-PARGRAF                                 NC2314.2
048100         GO TO LEVEL-2-TEST-2                                     NC2314.2
048200         WHEN ENTRY-2 (IDX-1, IDX-2) = SEC-HOLD-AREA              NC2314.2
048300             NEXT SENTENCE.                                       NC2314.2
048400                                                                  NC2314.2
048500     PERFORM PASS-TH.                                             NC2314.2
048600                                                                  NC2314.2
048700 LEVEL-2-TEST-2.                                                  NC2314.2
048800     MOVE "LEVEL-2-TEST-2      " TO PAR-NAME.                     NC2314.2
048900     MOVE "SEC (05,10)" TO SEC-HOLD-AREA.                         NC2314.2
049000     MOVE 05 TO SUB-1.                                            NC2314.2
049100     MOVE 10 TO SUB-2.                                            NC2314.2
049200     SET IDX-1 TO 5.                                              NC2314.2
049300     MOVE 01 TO CON-6.                                            NC2314.2
049400     SET IDX-2 TO 01.                                             NC2314.2
049500     SEARCH GRP2-ENTRY VARYING CON-6 AT END                       NC2314.2
049600         PERFORM SEC-FAIL-PARGRAF                                 NC2314.2
049700         GO TO LEVEL-2-TEST-3                                     NC2314.2
049800         WHEN ENTRY-2 (IDX-1, IDX-2) = SEC-HOLD-AREA              NC2314.2
049900             NEXT SENTENCE.                                       NC2314.2
050000                                                                  NC2314.2
050100     PERFORM PASS-TH.                                             NC2314.2
050200                                                                  NC2314.2
050300 LEVEL-2-TEST-3.                                                  NC2314.2
050400     MOVE "LEVEL-2-TEST-3      " TO PAR-NAME.                     NC2314.2
050500     MOVE "SEC (10,10)" TO SEC-HOLD-AREA.                         NC2314.2
050600     SET IDX-1 TO 10.                                             NC2314.2
050700     MOVE 01 TO CON-6.                                            NC2314.2
050800     SET IDX-2 TO 01.                                             NC2314.2
050900     MOVE 10 TO SUB-1  SUB-2.                                     NC2314.2
051000     SEARCH GRP2-ENTRY VARYING CON-6 AT END                       NC2314.2
051100         PERFORM SEC-FAIL-PARGRAF                                 NC2314.2
051200         GO TO LEVEL-2-TEST-4                                     NC2314.2
051300         WHEN ENTRY-2 (IDX-1, IDX-2) = SEC-HOLD-AREA              NC2314.2
051400             NEXT SENTENCE.                                       NC2314.2
051500                                                                  NC2314.2
051600     PERFORM PASS-TH.                                             NC2314.2
051700 LEVEL-2-TEST-4.                                                  NC2314.2
051800     MOVE "LEVEL-2-TEST-4      " TO PAR-NAME.                     NC2314.2
051900     MOVE "SEC (08,02)" TO SEC-HOLD-AREA.                         NC2314.2
052000     MOVE 08 TO SUB-1.                                            NC2314.2
052100     MOVE 02 TO SUB-2.                                            NC2314.2
052200     SET IDX-1 TO 08.                                             NC2314.2
052300     MOVE 01 TO CON-6.                                            NC2314.2
052400     SET IDX-2 TO 01.                                             NC2314.2
052500     SEARCH GRP2-ENTRY VARYING CON-6                              NC2314.2
052600         WHEN ENTRY-2 (IDX-1, IDX-2) = SEC-HOLD-AREA              NC2314.2
052700             GO TO PASS-TH-2-4.                                   NC2314.2
052800     PERFORM SEC-FAIL-PARGRAF.                                    NC2314.2
052900     GO TO LEVEL-3-TEST-1.                                        NC2314.2
053000 PASS-TH-2-4.                                                     NC2314.2
053100                                                                  NC2314.2
053200     PERFORM PASS-TH.                                             NC2314.2
053300     GO TO LEVEL-3-TEST-1.                                        NC2314.2
053400                                                                  NC2314.2
053500 SEC-FAIL-PARGRAF.                                                NC2314.2
053600     MOVE SEC-HOLD-AREA TO CORRECT-A.                             NC2314.2
053700     IF ENTRY-2 (SUB-1, SUB-2) = SEC-HOLD-AREA                    NC2314.2
053800         MOVE "IDX-2" TO END-IDX                                  NC2314.2
053900         SET IDX-VALU TO IDX-2                                    NC2314.2
054000         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2314.2
054100         MOVE END-STMT TO COMPUTED-A  ELSE                        NC2314.2
054200     MOVE ENTRY-2 (SUB-1, SUB-2) TO COMPUTED-A                    NC2314.2
054300     MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.               NC2314.2
054400                                                                  NC2314.2
054500     PERFORM FAIL-TH.                                             NC2314.2
054600                                                                  NC2314.2
054700 LEVEL-3-TEST-1.                                                  NC2314.2
054800     MOVE "LEVEL-3-TEST-1      " TO PAR-NAME.                     NC2314.2
054900     MOVE "SEARCH VARYING LEV 3" TO FEATURE.                      NC2314.2
055000     MOVE 1 TO SUB-1  SUB-2  SUB-3.                               NC2314.2
055100     MOVE "ELEM (01,01,01)" TO ELEM-HOLD-AREA.                    NC2314.2
055200     SET IDX-1 IDX-2 IDX-3 TO 01.                                 NC2314.2
055300     MOVE 01 TO CON-7.                                            NC2314.2
055400     SEARCH GRP3-ENTRY VARYING CON-7                              NC2314.2
055500         WHEN ENTRY-3 (IDX-1, IDX-2, IDX-3) = ELEM-HOLD-AREA      NC2314.2
055600             GO TO PASS-TH-3-1.                                   NC2314.2
055700     PERFORM ELEM-FAIL-PARA.                                      NC2314.2
055800     GO TO LEVEL-3-TEST-2.                                        NC2314.2
055900 PASS-TH-3-1.                                                     NC2314.2
056000                                                                  NC2314.2
056100     PERFORM PASS-TH.                                             NC2314.2
056200                                                                  NC2314.2
056300 LEVEL-3-TEST-2.                                                  NC2314.2
056400     MOVE "LEVEL-3-TEST-2      " TO PAR-NAME.                     NC2314.2
056500     MOVE 05 TO SUB-1.                                            NC2314.2
056600     MOVE 06 TO SUB-2.                                            NC2314.2
056700     MOVE 07 TO SUB-3.                                            NC2314.2
056800     SET IDX-1 TO 05.                                             NC2314.2
056900     SET IDX-2 TO 06.                                             NC2314.2
057000     MOVE 01 TO CON-7.                                            NC2314.2
057100     SET IDX-3 TO 01.                                             NC2314.2
057200     MOVE "ELEM (05,06,07)" TO ELEM-HOLD-AREA.                    NC2314.2
057300     SEARCH GRP3-ENTRY VARYING CON-7 AT END                       NC2314.2
057400         PERFORM ELEM-FAIL-PARA                                   NC2314.2
057500         GO TO LEVEL-3-TEST-3                                     NC2314.2
057600         WHEN ENTRY-3 (IDX-1, IDX-2, IDX-3) = ELEM-HOLD-AREA      NC2314.2
057700             NEXT SENTENCE.                                       NC2314.2
057800                                                                  NC2314.2
057900     PERFORM PASS-TH.                                             NC2314.2
058000                                                                  NC2314.2
058100 LEVEL-3-TEST-3.                                                  NC2314.2
058200     MOVE "LEVEL-3-TEST-3      " TO PAR-NAME.                     NC2314.2
058300     MOVE 10 TO SUB-1 SUB-2 SUB-3.                                NC2314.2
058400     SET IDX-1  IDX-2 TO 10.                                      NC2314.2
058500     SET IDX-3 TO 01.                                             NC2314.2
058600     MOVE 01 TO CON-7.                                            NC2314.2
058700     MOVE "ELEM (10,10,10)" TO ELEM-HOLD-AREA.                    NC2314.2
058800     SEARCH GRP3-ENTRY VARYING CON-7 AT END                       NC2314.2
058900         PERFORM ELEM-FAIL-PARA                                   NC2314.2
059000         GO TO LEVEL-3-TEST-4                                     NC2314.2
059100         WHEN ENTRY-3 (IDX-1, IDX-2, IDX-3) = ELEM-HOLD-AREA      NC2314.2
059200             NEXT SENTENCE.                                       NC2314.2
059300                                                                  NC2314.2
059400     PERFORM PASS-TH.                                             NC2314.2
059500 LEVEL-3-TEST-4.                                                  NC2314.2
059600     MOVE "LEVEL-3-TEST-4      " TO PAR-NAME.                     NC2314.2
059700     MOVE "ELEM (07,06,05)" TO ELEM-HOLD-AREA.                    NC2314.2
059800     MOVE 07 TO SUB-1.                                            NC2314.2
059900     MOVE 06 TO SUB-2.                                            NC2314.2
060000     MOVE 05 TO SUB-3.                                            NC2314.2
060100     SET IDX-1 TO 07.                                             NC2314.2
060200     SET IDX-2 TO 06.                                             NC2314.2
060300     SET IDX-3 TO 03.                                             NC2314.2
060400     MOVE 03 TO CON-7.                                            NC2314.2
060500     SEARCH GRP3-ENTRY VARYING CON-7 AT END                       NC2314.2
060600         PERFORM ELEM-FAIL-PARA                                   NC2314.2
060700         GO TO MULT-SEARCH-TEST-1                                 NC2314.2
060800         WHEN ENTRY-3 (IDX-1, IDX-2, IDX-3) = ELEM-HOLD-AREA      NC2314.2
060900         NEXT SENTENCE.                                           NC2314.2
061000                                                                  NC2314.2
061100     PERFORM PASS-TH.                                             NC2314.2
061200     GO TO MULT-SEARCH-TEST-1.                                    NC2314.2
061300 ELEM-FAIL-PARA.                                                  NC2314.2
061400     MOVE ELEM-HOLD-AREA TO CORRECT-A.                            NC2314.2
061500     IF ENTRY-3 (SUB-1, SUB-2, SUB-3) = ELEM-HOLD-AREA            NC2314.2
061600         MOVE "IDX-3" TO END-IDX                                  NC2314.2
061700         SET IDX-VALU TO IDX-3                                    NC2314.2
061800         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2314.2
061900         MOVE END-STMT TO COMPUTED-A  ELSE                        NC2314.2
062000     MOVE ENTRY-3 (SUB-1, SUB-2, SUB-3) TO COMPUTED-A             NC2314.2
062100     MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.               NC2314.2
062200                                                                  NC2314.2
062300     PERFORM FAIL-TH.                                             NC2314.2
062400                                                                  NC2314.2
062500 MULT-SEARCH-TEST-1.                                              NC2314.2
062600     MOVE "MULT-SEARCH-TEST-1  " TO PAR-NAME.                     NC2314.2
062700     MOVE "MULTIPLE SEARCH STMT" TO FEATURE.                      NC2314.2
062800     MOVE "GRP08" TO GRP-HOLD-AREA.                               NC2314.2
062900     MOVE "SEC (08,07)" TO SEC-HOLD-AREA.                         NC2314.2
063000     MOVE 01 TO CON-5 CON-6.                                      NC2314.2
063100     SET IDX-1 IDX-2 TO 01.                                       NC2314.2
063200     SEARCH GRP-ENTRY VARYING CON-5 AT END GO TO MULT-SEARCH-FAIL1NC2314.2
063300         WHEN ENTRY-1 (CON-5) = "GRP08" NEXT SENTENCE.            NC2314.2
063400     SEARCH GRP2-ENTRY VARYING CON-6 AT END GO TO MULT-SEARCH-FAILNC2314.2
063500         WHEN ENTRY-2 (CON-5, CON-6) = SEC-HOLD-AREA              NC2314.2
063600             NEXT SENTENCE.                                       NC2314.2
063700                                                                  NC2314.2
063800     PERFORM PASS-TH.                                             NC2314.2
063900     GO TO MULT-SEARCH-TEST-2.                                    NC2314.2
064000 MULT-SEARCH-FAIL1.                                               NC2314.2
064100     MOVE GRP-HOLD-AREA TO CORRECT-A.                             NC2314.2
064200     IF ENTRY-1 (08) = GRP-HOLD-AREA                              NC2314.2
064300         MOVE "IDX-1" TO END-IDX                                  NC2314.2
064400         SET IDX-VALU TO IDX-1                                    NC2314.2
064500         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2314.2
064600         MOVE END-STMT TO COMPUTED-A  ELSE                        NC2314.2
064700     MOVE ENTRY-1 (08) TO COMPUTED-A                              NC2314.2
064800     MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.               NC2314.2
064900                                                                  NC2314.2
065000     PERFORM FAIL-TH.                                             NC2314.2
065100     GO TO MULT-SEARCH-TEST-2.                                    NC2314.2
065200 MULT-SEARCH-FAIL.                                                NC2314.2
065300     MOVE SEC-HOLD-AREA TO CORRECT-A.                             NC2314.2
065400     IF ENTRY-2 (08, 07) = SEC-HOLD-AREA                          NC2314.2
065500         MOVE "IDX-2" TO END-IDX                                  NC2314.2
065600         SET IDX-VALU TO IDX-2                                    NC2314.2
065700         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2314.2
065800         MOVE END-STMT TO COMPUTED-A  ELSE                        NC2314.2
065900     MOVE ENTRY-2 (08, 07) TO COMPUTED-A                          NC2314.2
066000     MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.               NC2314.2
066100                                                                  NC2314.2
066200     PERFORM FAIL-TH.                                             NC2314.2
066300                                                                  NC2314.2
066400 MULT-SEARCH-TEST-2.                                              NC2314.2
066500     MOVE "MULT-SEARCH-TEST-2  " TO PAR-NAME.                     NC2314.2
066600     MOVE "GRP04" TO GRP-HOLD-AREA.                               NC2314.2
066700     MOVE "SEC (04,04)" TO SEC-HOLD-AREA.                         NC2314.2
066800     MOVE "ELEM (04,04,04)" TO ELEM-HOLD-AREA.                    NC2314.2
066900     MOVE 01 TO CON-5 CON-6 CON-7.                                NC2314.2
067000     SET IDX-1  IDX-2  IDX-3 TO 01.                               NC2314.2
067100     SEARCH GRP-ENTRY VARYING CON-5 AT END                        NC2314.2
067200         GO TO MULT-SEARCH-2-FAIL WHEN ENTRY-1 (CON-5) =          NC2314.2
067300         GRP-HOLD-AREA  NEXT SENTENCE.                            NC2314.2
067400     SEARCH GRP2-ENTRY VARYING CON-6 AT END                       NC2314.2
067500         GO TO MULT-SEARCH-3-FAIL WHEN ENTRY-2 (CON-5, CON-6) =   NC2314.2
067600         SEC-HOLD-AREA  NEXT SENTENCE.                            NC2314.2
067700     SEARCH GRP3-ENTRY VARYING CON-7 AT END                       NC2314.2
067800         GO TO MULT-SEARCH-4-FAIL WHEN ENTRY-3                    NC2314.2
067900             (CON-5, CON-6, CON-7) = ELEM-HOLD-AREA NEXT SENTENCE.NC2314.2
068000                                                                  NC2314.2
068100     PERFORM PASS-TH.                                             NC2314.2
068200     GO TO   MULT-SEARCH-7-INIT-3.                                NC2314.2
068300                                                                  NC2314.2
068400 MULT-SEARCH-2-FAIL.                                              NC2314.2
068500     MOVE GRP-HOLD-AREA TO CORRECT-A.                             NC2314.2
068600     IF ENTRY-1 (04) = GRP-HOLD-AREA                              NC2314.2
068700         MOVE "IDX-1" TO END-IDX                                  NC2314.2
068800         SET IDX-VALU TO IDX-1                                    NC2314.2
068900         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2314.2
069000         MOVE END-STMT TO COMPUTED-A  ELSE                        NC2314.2
069100     MOVE ENTRY-1 (04) TO COMPUTED-A                              NC2314.2
069200     MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.               NC2314.2
069300                                                                  NC2314.2
069400     PERFORM FAIL-TH.                                             NC2314.2
069500     GO TO   MULT-SEARCH-7-INIT-3.                                NC2314.2
069600                                                                  NC2314.2
069700 MULT-SEARCH-3-FAIL.                                              NC2314.2
069800     MOVE  SEC-HOLD-AREA TO CORRECT-A.                            NC2314.2
069900     IF ENTRY-2 (04, 04) = SEC-HOLD-AREA                          NC2314.2
070000         MOVE "IDX-2" TO END-IDX                                  NC2314.2
070100         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2314.2
070200         SET IDX-VALU TO IDX-2                                    NC2314.2
070300         MOVE END-STMT TO COMPUTED-A  ELSE                        NC2314.2
070400     MOVE ENTRY-2 (04, 04) TO COMPUTED-A                          NC2314.2
070500     MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.               NC2314.2
070600                                                                  NC2314.2
070700     PERFORM FAIL-TH.                                             NC2314.2
070800     GO TO   MULT-SEARCH-7-INIT-3.                                NC2314.2
070900                                                                  NC2314.2
071000 MULT-SEARCH-4-FAIL.                                              NC2314.2
071100     MOVE ELEM-HOLD-AREA TO CORRECT-A.                            NC2314.2
071200     IF ENTRY-3 (04, 04, 04) = ELEM-HOLD-AREA                     NC2314.2
071300         MOVE "IDX-3" TO END-IDX                                  NC2314.2
071400         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2314.2
071500         SET IDX-VALU TO IDX-3                                    NC2314.2
071600         MOVE END-STMT TO COMPUTED-A  ELSE                        NC2314.2
071700     MOVE ENTRY-3 (04, 04, 04) TO COMPUTED-A                      NC2314.2
071800     MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.               NC2314.2
071900                                                                  NC2314.2
072000     PERFORM FAIL-TH.                                             NC2314.2
072100                                                                  NC2314.2
072200 MULT-SEARCH-7-INIT-3.                                            NC2314.2
072300     MOVE   "MULT-SEARCH-7-TEST-3" TO PAR-NAME.                   NC2314.2
072400     MOVE   "VI-122 6.21"          TO ANSI-REFERENCE.             NC2314.2
072500     MOVE    ALL "ABCDEFGHIJKLMNOPQRSTUVWXYZ" TO 7-DIMENSION-TBL. NC2314.2
072600     MOVE   "UV" TO L1-HOLD.                                      NC2314.2
072700     MOVE   "WX" TO L2-HOLD.                                      NC2314.2
072800     MOVE   "IJ" TO L3-HOLD.                                      NC2314.2
072900     MOVE   "KL" TO L4-HOLD.                                      NC2314.2
073000     MOVE   "AB" TO L5-HOLD.                                      NC2314.2
073100     MOVE   "CD" TO L6-HOLD.                                      NC2314.2
073200     MOVE   "GH" TO L7-HOLD.                                      NC2314.2
073300     SET     IX-1 IX-2 IX-3 IX-4 IX-5 IX-6 IX-7 TO 1.             NC2314.2
073400     MOVE    1 TO N1 N2 N3 N4 N5 N6 N7.                           NC2314.2
073500     GO TO   MULT-SEARCH-7-TEST-3.                                NC2314.2
073600 MULT-SEARCH-7-DELETE-3.                                          NC2314.2
073700     PERFORM DE-LETE.                                             NC2314.2
073800     PERFORM PRINT-DETAIL.                                        NC2314.2
073900     GO TO   SPECIAL-TEST-1.                                      NC2314.2
074000 MULT-SEARCH-7-TEST-3.                                            NC2314.2
074100     SEARCH  GRP-7-1-ENTRY VARYING N1                             NC2314.2
074200             AT END  GO TO MULT-SEARCH-7-FAIL-1                   NC2314.2
074300             WHEN    ENTRY-7-1 (N1) =  L1-HOLD                    NC2314.2
074400                     NEXT SENTENCE.                               NC2314.2
074500     SEARCH  GRP-7-2-ENTRY VARYING N2                             NC2314.2
074600             AT END  GO TO MULT-SEARCH-7-FAIL-2                   NC2314.2
074700             WHEN    ENTRY-7-2 (N1 N2) = L2-HOLD                  NC2314.2
074800                     NEXT SENTENCE.                               NC2314.2
074900     SEARCH  GRP-7-3-ENTRY VARYING N3                             NC2314.2
075000             AT END  GO TO MULT-SEARCH-7-FAIL-3                   NC2314.2
075100             WHEN    ENTRY-7-3 (N1 N2 N3) = L3-HOLD               NC2314.2
075200                     NEXT SENTENCE.                               NC2314.2
075300     SEARCH  GRP-7-4-ENTRY VARYING N4                             NC2314.2
075400             AT END  GO TO MULT-SEARCH-7-FAIL-4                   NC2314.2
075500             WHEN    ENTRY-7-4 (N1 N2 N3 N4) =  L4-HOLD           NC2314.2
075600                     NEXT SENTENCE.                               NC2314.2
075700     SEARCH  GRP-7-5-ENTRY VARYING N5                             NC2314.2
075800             AT END  GO TO MULT-SEARCH-7-FAIL-5                   NC2314.2
075900             WHEN    ENTRY-7-5 (N1 N2 N3 N4 N5) = L5-HOLD         NC2314.2
076000                     NEXT SENTENCE.                               NC2314.2
076100     SEARCH  GRP-7-6-ENTRY VARYING N6                             NC2314.2
076200             AT END  GO TO MULT-SEARCH-7-FAIL-6                   NC2314.2
076300             WHEN    ENTRY-7-6 (N1 N2 N3 N4 N5 N6) = L6-HOLD      NC2314.2
076400                     NEXT SENTENCE.                               NC2314.2
076500     SEARCH  GRP-7-7-ENTRY VARYING N7                             NC2314.2
076600             AT END  GO TO MULT-SEARCH-7-FAIL-7                   NC2314.2
076700             WHEN    ENTRY-7-7 (N1 N2 N3 N4 N5 N6 N7) = L7-HOLD   NC2314.2
076800                     NEXT SENTENCE.                               NC2314.2
076900                                                                  NC2314.2
077000     PERFORM PASS-TH.                                             NC2314.2
077100     GO TO   SPECIAL-TEST-1.                                      NC2314.2
077200                                                                  NC2314.2
077300 MULT-SEARCH-7-FAIL-1.                                            NC2314.2
077400     MOVE    L1-HOLD TO CORRECT-A.                                NC2314.2
077500     IF      ENTRY-7-1 (2) = L1-HOLD                              NC2314.2
077600             MOVE   "IX-1" TO END-IDX                             NC2314.2
077700             SET     IDX-VALU TO IX-1                             NC2314.2
077800             MOVE   "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK      NC2314.2
077900             MOVE    END-STMT TO COMPUTED-A                       NC2314.2
078000     ELSE                                                         NC2314.2
078100             MOVE    ENTRY-7-1 (2) TO COMPUTED-A                  NC2314.2
078200             MOVE   "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.     NC2314.2
078300                                                                  NC2314.2
078400     PERFORM FAIL-TH.                                             NC2314.2
078500     GO TO   SPECIAL-TEST-1.                                      NC2314.2
078600                                                                  NC2314.2
078700 MULT-SEARCH-7-FAIL-2.                                            NC2314.2
078800     MOVE    L2-HOLD TO CORRECT-A.                                NC2314.2
078900     IF      ENTRY-7-2 (2 1) = L1-HOLD                            NC2314.2
079000             MOVE   "IX-2" TO END-IDX                             NC2314.2
079100             SET     IDX-VALU TO IX-2                             NC2314.2
079200             MOVE   "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK      NC2314.2
079300             MOVE    END-STMT TO COMPUTED-A                       NC2314.2
079400     ELSE                                                         NC2314.2
079500             MOVE    ENTRY-7-2 (2 1) TO COMPUTED-A                NC2314.2
079600             MOVE   "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.     NC2314.2
079700                                                                  NC2314.2
079800     PERFORM FAIL-TH.                                             NC2314.2
079900     GO TO   SPECIAL-TEST-1.                                      NC2314.2
080000                                                                  NC2314.2
080100 MULT-SEARCH-7-FAIL-3.                                            NC2314.2
080200     MOVE    L3-HOLD TO CORRECT-A.                                NC2314.2
080300     IF      ENTRY-7-3 (2 1 2) = L3-HOLD                          NC2314.2
080400             MOVE   "IX-3" TO END-IDX                             NC2314.2
080500             SET     IDX-VALU TO IX-3                             NC2314.2
080600             MOVE   "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK      NC2314.2
080700             MOVE    END-STMT TO COMPUTED-A                       NC2314.2
080800     ELSE                                                         NC2314.2
080900             MOVE    ENTRY-7-3 (2 1 2) TO COMPUTED-A              NC2314.2
081000             MOVE   "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.     NC2314.2
081100                                                                  NC2314.2
081200     PERFORM FAIL-TH.                                             NC2314.2
081300     GO TO   SPECIAL-TEST-1.                                      NC2314.2
081400                                                                  NC2314.2
081500 MULT-SEARCH-7-FAIL-4.                                            NC2314.2
081600     MOVE    L4-HOLD TO CORRECT-A.                                NC2314.2
081700     IF      ENTRY-7-4 (2 1 2 1) = L4-HOLD                        NC2314.2
081800             MOVE   "IX-4" TO END-IDX                             NC2314.2
081900             SET     IDX-VALU TO IX-4                             NC2314.2
082000             MOVE   "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK      NC2314.2
082100             MOVE    END-STMT TO COMPUTED-A                       NC2314.2
082200     ELSE                                                         NC2314.2
082300             MOVE    ENTRY-7-4 (2 1 2 1) TO COMPUTED-A            NC2314.2
082400             MOVE   "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.     NC2314.2
082500                                                                  NC2314.2
082600     PERFORM FAIL-TH.                                             NC2314.2
082700     GO TO   SPECIAL-TEST-1.                                      NC2314.2
082800                                                                  NC2314.2
082900 MULT-SEARCH-7-FAIL-5.                                            NC2314.2
083000     MOVE    L5-HOLD TO CORRECT-A.                                NC2314.2
083100     IF      ENTRY-7-5 (2 1 2 1 2) = L5-HOLD                      NC2314.2
083200             MOVE   "IX-5" TO END-IDX                             NC2314.2
083300             SET     IDX-VALU TO IX-5                             NC2314.2
083400             MOVE   "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK      NC2314.2
083500             MOVE    END-STMT TO COMPUTED-A                       NC2314.2
083600     ELSE                                                         NC2314.2
083700             MOVE    ENTRY-7-5 (2 1 2 1 2) TO COMPUTED-A          NC2314.2
083800             MOVE   "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.     NC2314.2
083900                                                                  NC2314.2
084000     PERFORM FAIL-TH.                                             NC2314.2
084100     GO TO   SPECIAL-TEST-1.                                      NC2314.2
084200                                                                  NC2314.2
084300 MULT-SEARCH-7-FAIL-6.                                            NC2314.2
084400     MOVE    L6-HOLD TO CORRECT-A.                                NC2314.2
084500     IF      ENTRY-7-6 (2 1 2 1 2 1) = L6-HOLD                    NC2314.2
084600             MOVE   "IX-6" TO END-IDX                             NC2314.2
084700             SET     IDX-VALU TO IX-6                             NC2314.2
084800             MOVE   "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK      NC2314.2
084900             MOVE    END-STMT TO COMPUTED-A                       NC2314.2
085000     ELSE                                                         NC2314.2
085100             MOVE    ENTRY-7-6 (2 1 2 1 2 1) TO COMPUTED-A        NC2314.2
085200             MOVE   "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.     NC2314.2
085300                                                                  NC2314.2
085400     PERFORM FAIL-TH.                                             NC2314.2
085500     GO TO   SPECIAL-TEST-1.                                      NC2314.2
085600                                                                  NC2314.2
085700 MULT-SEARCH-7-FAIL-7.                                            NC2314.2
085800     MOVE    L7-HOLD TO CORRECT-A.                                NC2314.2
085900     IF      ENTRY-7-7 (2 1 2 1 2 1 2) = L7-HOLD                  NC2314.2
086000             MOVE   "IX-7" TO END-IDX                             NC2314.2
086100             SET     IDX-VALU TO IX-7                             NC2314.2
086200             MOVE   "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK      NC2314.2
086300             MOVE    END-STMT TO COMPUTED-A                       NC2314.2
086400     ELSE                                                         NC2314.2
086500             MOVE    ENTRY-7-7 (2 1 2 1 2 1 2) TO COMPUTED-A      NC2314.2
086600             MOVE   "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.     NC2314.2
086700                                                                  NC2314.2
086800     PERFORM FAIL-TH.                                             NC2314.2
086900                                                                  NC2314.2
087000 SPECIAL-TEST-1.                                                  NC2314.2
087100     MOVE "SPECIAL-TEST-1      " TO PAR-NAME.                     NC2314.2
087200     MOVE "IDX SET HI TO ENTRY " TO FEATURE.                      NC2314.2
087300     MOVE 04 TO CON-5.                                            NC2314.2
087400     SET IDX-1 TO 04.                                             NC2314.2
087500     SEARCH GRP-ENTRY VARYING CON-5 AT END                        NC2314.2
087600     GO TO SPEC-PASS-PARAGRAPH-1 WHEN ENTRY-1 (CON-5) = "GRP03"   NC2314.2
087700         GO TO SPEC-FAIL-PARAGRAPH-1.                             NC2314.2
087800 SPECIAL-2-LEVEL-SEARCH.                                          NC2314.2
087900     MOVE "SPECIAL-2-LEVEL-SEAR" TO PAR-NAME.                     NC2314.2
088000     MOVE 04 TO CON-5.                                            NC2314.2
088100     MOVE 05 TO CON-6.                                            NC2314.2
088200     SET IDX-1 TO 04.                                             NC2314.2
088300     SET IDX-2 TO 05.                                             NC2314.2
088400     SEARCH GRP-ENTRY VARYING IDX-1 AT END                        NC2314.2
088500         GO TO SPEC-FAIL-PARAGRAPH-2                              NC2314.2
088600         WHEN ENTRY-1 (CON-5) = "GRP04" NEXT SENTENCE.            NC2314.2
088700     SEARCH GRP2-ENTRY VARYING CON-6 AT END                       NC2314.2
088800         GO TO SPEC-PASS-PARAGRAPH-2                              NC2314.2
088900         WHEN ENTRY-2 (CON-5, CON-6) = "SEC (04,04)"              NC2314.2
089000         GO TO SPEC-FAIL-PARAGRAPH-3.                             NC2314.2
089100 SPEC-PASS-PARAGRAPH-1.                                           NC2314.2
089200                                                                  NC2314.2
089300     PERFORM PASS-TH.                                             NC2314.2
089400     GO TO SPECIAL-2-LEVEL-SEARCH.                                NC2314.2
089500                                                                  NC2314.2
089600 SPEC-FAIL-PARAGRAPH-1.                                           NC2314.2
089700     MOVE "ENTRY SHOULD NOT BE FOUND  " TO RE-MARK.               NC2314.2
089800     MOVE "GRP03" TO COMPUTED-A.                                  NC2314.2
089900                                                                  NC2314.2
090000     MOVE SPACES TO CORRECT-A.                                    NC2314.2
090100     PERFORM FAIL-TH.                                             NC2314.2
090200     GO TO SPECIAL-2-LEVEL-SEARCH.                                NC2314.2
090300                                                                  NC2314.2
090400 SPEC-FAIL-PARAGRAPH-2.                                           NC2314.2
090500     MOVE "GRP04" TO CORRECT-A.                                   NC2314.2
090600     MOVE ENTRY-1 (04) TO COMPUTED-A.                             NC2314.2
090700                                                                  NC2314.2
090800     MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK.               NC2314.2
090900     PERFORM FAIL-TH.                                             NC2314.2
091000     GO TO SPECIAL-3-LEVEL-SEARCH.                                NC2314.2
091100                                                                  NC2314.2
091200 SPEC-FAIL-PARAGRAPH-3.                                           NC2314.2
091300     MOVE ENTRY-2 (04, 04) TO COMPUTED-A.                         NC2314.2
091400     MOVE SPACE TO CORRECT-A.                                     NC2314.2
091500     MOVE "ENTRY SHOULD NOT BE FOUND  " TO RE-MARK.               NC2314.2
091600                                                                  NC2314.2
091700     PERFORM FAIL-TH.                                             NC2314.2
091800     GO TO SPECIAL-3-LEVEL-SEARCH.                                NC2314.2
091900                                                                  NC2314.2
092000 SPEC-PASS-PARAGRAPH-2.                                           NC2314.2
092100                                                                  NC2314.2
092200     PERFORM PASS-TH.                                             NC2314.2
092300     GO TO SPECIAL-3-LEVEL-SEARCH.                                NC2314.2
092400                                                                  NC2314.2
092500 SPECIAL-3-LEVEL-SEARCH.                                          NC2314.2
092600     MOVE "SPECIAL-3-LEVEL-SEAR" TO PAR-NAME.                     NC2314.2
092700     SET IDX-1 TO 02.                                             NC2314.2
092800     MOVE 02 TO CON-5.                                            NC2314.2
092900     SEARCH GRP-ENTRY VARYING CON-5 AT END                        NC2314.2
093000         GO TO SPEC-FAIL-PARAGRAPH-4 WHEN ENTRY-1 (CON-5)         NC2314.2
093100         EQUAL TO "GRP02" NEXT SENTENCE.                          NC2314.2
093200     MOVE 01 TO CON-6.                                            NC2314.2
093300     SET IDX-2 TO 01.                                             NC2314.2
093400     SEARCH GRP2-ENTRY VARYING CON-6 AT END                       NC2314.2
093500         GO TO SPEC-FAIL-PARAGRAPH-5                              NC2314.2
093600     WHEN ENTRY-2 (CON-5, CON-6) = "SEC (02,03)" NEXT SENTENCE.   NC2314.2
093700     MOVE 05 TO CON-7.                                            NC2314.2
093800     SET IDX-3 TO 05.                                             NC2314.2
093900     SEARCH GRP3-ENTRY VARYING CON-7 AT END                       NC2314.2
094000         GO TO SPEC-PASS-PARAGRAPH-3                              NC2314.2
094100     WHEN ENTRY-3 (CON-5, CON-6, CON-7) = "ELEM (02,03,04)"       NC2314.2
094200                                                                  NC2314.2
094300     MOVE "INDEX SET HIGHER THAN ENTRY" TO RE-MARK                NC2314.2
094400         MOVE SPACES TO CORRECT-A                                 NC2314.2
094500         MOVE "ELEM (02,03,04)" TO COMPUTED-A                     NC2314.2
094600         PERFORM FAIL-TH                                          NC2314.2
094700         GO TO SEARCH-INIT-1.                                     NC2314.2
094800 SPEC-PASS-PARAGRAPH-3.                                           NC2314.2
094900                                                                  NC2314.2
095000     PERFORM PASS-TH.                                             NC2314.2
095100     GO TO SEARCH-INIT-1.                                         NC2314.2
095200                                                                  NC2314.2
095300 SPEC-FAIL-PARAGRAPH-4.                                           NC2314.2
095400     IF ENTRY-1 (02) = "GRP02"                                    NC2314.2
095500         MOVE "IDX-1" TO END-IDX                                  NC2314.2
095600         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2314.2
095700         SET IDX-VALU TO IDX-1                                    NC2314.2
095800         MOVE END-STMT TO COMPUTED-A  ELSE                        NC2314.2
095900     MOVE ENTRY-1 (02) TO COMPUTED-A                              NC2314.2
096000     MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.               NC2314.2
096100                                                                  NC2314.2
096200     MOVE "GRP02" TO CORRECT-A.                                   NC2314.2
096300     PERFORM FAIL-TH.                                             NC2314.2
096400     GO TO SEARCH-INIT-1.                                         NC2314.2
096500 SPEC-FAIL-PARAGRAPH-5.                                           NC2314.2
096600     IF ENTRY-2 (02, 03) = "SEC (02,03)"                          NC2314.2
096700         MOVE "IDX-2" TO END-IDX                                  NC2314.2
096800         SET IDX-VALU TO IDX-2                                    NC2314.2
096900         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2314.2
097000         MOVE END-STMT TO COMPUTED-A  ELSE                        NC2314.2
097100     MOVE ENTRY-2 (02, 03) TO COMPUTED-A                          NC2314.2
097200     MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.               NC2314.2
097300                                                                  NC2314.2
097400     MOVE "SEC (02, 03)" TO CORRECT-A.                            NC2314.2
097500     PERFORM FAIL-TH.                                             NC2314.2
097600                                                                  NC2314.2
097700                                                                  NC2314.2
097800 SEARCH-INIT-1.                                                   NC2314.2
097900     MOVE   "SEARCH-TEST-1" TO PAR-NAME.                          NC2314.2
098000     MOVE   "IV-41 6.4.3"   TO ANSI-REFERENCE.                    NC2314.2
098100     MOVE   "EXP.SCOPE TERMINATOR" TO FEATURE.                    NC2314.2
098200     MOVE   "CD"   TO L1-HOLD.                                    NC2314.2
098300     MOVE   "CD"   TO ENTRY-7-1 (2).                              NC2314.2
098400     MOVE    SPACE TO L2-HOLD.                                    NC2314.2
098500     MOVE    SPACE TO L3-HOLD.                                    NC2314.2
098600     MOVE    SPACE TO L4-HOLD.                                    NC2314.2
098700     MOVE    1 TO REC-CT.                                         NC2314.2
098800     MOVE    1 TO N1.                                             NC2314.2
098900     SET     IX-1 IX-2 IX-3 IX-4 IX-5 IX-6 IX-7 TO 1.             NC2314.2
099000     GO TO   SEARCH-TEST-1-0.                                     NC2314.2
099100 SEARCH-DELETE-1.                                                 NC2314.2
099200     PERFORM DE-LETE.                                             NC2314.2
099300     PERFORM PRINT-DETAIL.                                        NC2314.2
099400     GO TO   SEARCH-INIT-2.                                       NC2314.2
099500 SEARCH-TEST-1-0.                                                 NC2314.2
099600     SEARCH  GRP-7-1-ENTRY VARYING N1                             NC2314.2
099700             WHEN    ENTRY-7-1 (N1) =  L1-HOLD                    NC2314.2
099800                     MOVE   "AA" TO L2-HOLD                       NC2314.2
099900                     MOVE   "BB" TO L3-HOLD                       NC2314.2
100000     END-SEARCH                                                   NC2314.2
100100     MOVE   "CC" TO L4-HOLD.                                      NC2314.2
100200 SEARCH-TEST-1-1.                                                 NC2314.2
100300     MOVE   "SEARCH-TEST-1-1" TO PAR-NAME.                        NC2314.2
100400     IF      L2-HOLD = "AA"                                       NC2314.2
100500             PERFORM PASS                                         NC2314.2
100600             PERFORM PRINT-DETAIL                                 NC2314.2
100700     ELSE                                                         NC2314.2
100800             MOVE   "'WHEN' PHRASE SHOULD BE TRUE" TO RE-MARK     NC2314.2
100900             MOVE   "AA"  TO CORRECT-X                            NC2314.2
101000             MOVE    L2-HOLD TO COMPUTED-X                        NC2314.2
101100             PERFORM FAIL                                         NC2314.2
101200             PERFORM PRINT-DETAIL.                                NC2314.2
101300     ADD     1 TO REC-CT.                                         NC2314.2
101400 SEARCH-TEST-1-2.                                                 NC2314.2
101500     MOVE   "SEARCH-TEST-1-2" TO PAR-NAME.                        NC2314.2
101600     IF      L3-HOLD = "BB"                                       NC2314.2
101700             PERFORM PASS                                         NC2314.2
101800             PERFORM PRINT-DETAIL                                 NC2314.2
101900     ELSE                                                         NC2314.2
102000             MOVE   "'WHEN' PHRASE SHOULD BE TRUE" TO RE-MARK     NC2314.2
102100             MOVE   "BB"  TO CORRECT-X                            NC2314.2
102200             MOVE    L3-HOLD TO COMPUTED-X                        NC2314.2
102300             PERFORM FAIL                                         NC2314.2
102400             PERFORM PRINT-DETAIL.                                NC2314.2
102500     ADD     1 TO REC-CT.                                         NC2314.2
102600 SEARCH-TEST-1-3.                                                 NC2314.2
102700     MOVE   "SEARCH-TEST-1-3" TO PAR-NAME.                        NC2314.2
102800     IF      L4-HOLD = "CC"                                       NC2314.2
102900             PERFORM PASS                                         NC2314.2
103000             PERFORM PRINT-DETAIL                                 NC2314.2
103100     ELSE                                                         NC2314.2
103200             MOVE   "SCOPE TERMINATOR IGNORED" TO RE-MARK         NC2314.2
103300             MOVE   "CC"  TO CORRECT-X                            NC2314.2
103400             MOVE    L4-HOLD TO COMPUTED-X                        NC2314.2
103500             PERFORM FAIL                                         NC2314.2
103600             PERFORM PRINT-DETAIL.                                NC2314.2
103700                                                                  NC2314.2
103800                                                                  NC2314.2
103900 SEARCH-INIT-2.                                                   NC2314.2
104000     MOVE   "SEARCH-TEST-2" TO PAR-NAME.                          NC2314.2
104100     MOVE   "IV-41 6.4.3"   TO ANSI-REFERENCE.                    NC2314.2
104200     MOVE   "CD"   TO L1-HOLD.                                    NC2314.2
104300     MOVE   "ZZ"   TO ENTRY-7-1 (2).                              NC2314.2
104400     MOVE    SPACE TO L2-HOLD.                                    NC2314.2
104500     MOVE    SPACE TO L3-HOLD.                                    NC2314.2
104600     MOVE    SPACE TO L4-HOLD.                                    NC2314.2
104700     MOVE    1 TO REC-CT.                                         NC2314.2
104800     MOVE    1 TO N1.                                             NC2314.2
104900     SET     IX-1 IX-2 IX-3 IX-4 IX-5 IX-6 IX-7 TO 1.             NC2314.2
105000     GO TO   SEARCH-TEST-2-0.                                     NC2314.2
105100 SEARCH-DELETE-2.                                                 NC2314.2
105200     PERFORM DE-LETE.                                             NC2314.2
105300     PERFORM PRINT-DETAIL.                                        NC2314.2
105400     GO TO   END-SEARCH-TEST.                                     NC2314.2
105500 SEARCH-TEST-2-0.                                                 NC2314.2
105600     SEARCH  GRP-7-1-ENTRY VARYING N1                             NC2314.2
105700             WHEN    ENTRY-7-1 (N1) =  L1-HOLD                    NC2314.2
105800                     MOVE   "AA" TO L2-HOLD                       NC2314.2
105900                     MOVE   "BB" TO L3-HOLD                       NC2314.2
106000     END-SEARCH                                                   NC2314.2
106100     MOVE   "CC" TO L4-HOLD.                                      NC2314.2
106200 SEARCH-TEST-2-1.                                                 NC2314.2
106300     MOVE   "SEARCH-TEST-2-1" TO PAR-NAME.                        NC2314.2
106400     IF      L2-HOLD = SPACE                                      NC2314.2
106500             PERFORM PASS                                         NC2314.2
106600             PERFORM PRINT-DETAIL                                 NC2314.2
106700     ELSE                                                         NC2314.2
106800             MOVE   "'WHEN' PHRASE SHOULD BE FALSE" TO RE-MARK    NC2314.2
106900             MOVE    SPACE TO CORRECT-X                           NC2314.2
107000             MOVE    L2-HOLD TO COMPUTED-X                        NC2314.2
107100             PERFORM FAIL                                         NC2314.2
107200             PERFORM PRINT-DETAIL.                                NC2314.2
107300     ADD     1 TO REC-CT.                                         NC2314.2
107400 SEARCH-TEST-2-2.                                                 NC2314.2
107500     MOVE   "SEARCH-TEST-2-2" TO PAR-NAME.                        NC2314.2
107600     IF      L3-HOLD = SPACE                                      NC2314.2
107700             PERFORM PASS                                         NC2314.2
107800             PERFORM PRINT-DETAIL                                 NC2314.2
107900     ELSE                                                         NC2314.2
108000             MOVE   "'WHEN' PHRASE SHOULD BE FALSE" TO RE-MARK    NC2314.2
108100             MOVE    SPACE TO CORRECT-X                           NC2314.2
108200             MOVE    L3-HOLD TO COMPUTED-X                        NC2314.2
108300             PERFORM FAIL                                         NC2314.2
108400             PERFORM PRINT-DETAIL.                                NC2314.2
108500     ADD     1 TO REC-CT.                                         NC2314.2
108600 SEARCH-TEST-2-3.                                                 NC2314.2
108700     MOVE   "SEARCH-TEST-2-3" TO PAR-NAME.                        NC2314.2
108800     IF      L4-HOLD = "CC"                                       NC2314.2
108900             PERFORM PASS                                         NC2314.2
109000             PERFORM PRINT-DETAIL                                 NC2314.2
109100     ELSE                                                         NC2314.2
109200             MOVE   "SCOPE TERMINATOR IGNORED" TO RE-MARK         NC2314.2
109300             MOVE   "CC"  TO CORRECT-X                            NC2314.2
109400             MOVE    L4-HOLD TO COMPUTED-X                        NC2314.2
109500             PERFORM FAIL                                         NC2314.2
109600             PERFORM PRINT-DETAIL.                                NC2314.2
109700                                                                  NC2314.2
109800     GO TO END-SEARCH-TEST.                                       NC2314.2
109900                                                                  NC2314.2
110000 PASS-TH.                                                         NC2314.2
110100     PERFORM PASS.                                                NC2314.2
110200     PERFORM PRINT-DETAIL.                                        NC2314.2
110300 FAIL-TH.                                                         NC2314.2
110400     PERFORM FAIL.                                                NC2314.2
110500     PERFORM  PRINT-DETAIL.                                       NC2314.2
110600 END-SEARCH-TEST.                                                 NC2314.2
110700     EXIT.                                                        NC2314.2
110800 CCVS-EXIT SECTION.                                               NC2314.2
110900 CCVS-999999.                                                     NC2314.2
111000     GO TO CLOSE-FILES.                                           NC2314.2
*END-OF,NC231A                                                                  
*HEADER,COBOL,NC232A                                                            
000100 IDENTIFICATION DIVISION.                                         NC2324.2
000200 PROGRAM-ID.                                                      NC2324.2
000300     NC232A.                                                      NC2324.2
000400****************************************************************  NC2324.2
000500*                                                              *  NC2324.2
000600*    VALIDATION FOR:-                                          *  NC2324.2
000700*                                                              *  NC2324.2
000800*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2324.2
000900*                                                              *  NC2324.2
001000*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC2324.2
001100*                                                              *  NC2324.2
001200****************************************************************  NC2324.2
001300*                                                              *  NC2324.2
001400*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  NC2324.2
001500*                                                              *  NC2324.2
001600*        X-55  - SYSTEM PRINTER NAME.                          *  NC2324.2
001700*        X-82  - SOURCE COMPUTER NAME.                         *  NC2324.2
001800*        X-83  - OBJECT COMPUTER NAME.                         *  NC2324.2
001900*                                                              *  NC2324.2
002000****************************************************************  NC2324.2
002100                                                                  NC2324.2
002200*                                                                 NC2324.2
002300*    PROGRAM NC232A USES FORMAT 1 OF THE "SEARCH" STATEMENT TO *  NC2324.2
002400*    ACCESS A THREE DIMENSIONAL TABLE.  THE OPTIONAL "VARYING" *  NC2324.2
002500*    PHRASE IS USED WITH AN INDEX-NAME.                        *  NC2324.2
002600*                                                              *  NC2324.2
002700****************************************************************  NC2324.2
002800 ENVIRONMENT DIVISION.                                            NC2324.2
002900 CONFIGURATION SECTION.                                           NC2324.2
003000 SOURCE-COMPUTER.                                                 NC2324.2
003100     XXXXX082.                                                    NC2324.2
003200 OBJECT-COMPUTER.                                                 NC2324.2
003300     XXXXX083.                                                    NC2324.2
003400 INPUT-OUTPUT SECTION.                                            NC2324.2
003500 FILE-CONTROL.                                                    NC2324.2
003600     SELECT PRINT-FILE ASSIGN TO                                  NC2324.2
003700     XXXXX055.                                                    NC2324.2
003800 DATA DIVISION.                                                   NC2324.2
003900 FILE SECTION.                                                    NC2324.2
004000 FD  PRINT-FILE.                                                  NC2324.2
004100 01  PRINT-REC PICTURE X(120).                                    NC2324.2
004200 01  DUMMY-RECORD PICTURE X(120).                                 NC2324.2
004300 WORKING-STORAGE SECTION.                                         NC2324.2
004400 77  SUB-1              PICTURE S99  VALUE ZERO.                  NC2324.2
004500 77  SUB-2              PICTURE 99   VALUE ZERO.                  NC2324.2
004600 77  SUB-3              PICTURE 99   VALUE ZERO.                  NC2324.2
004700 77  CON-7              PICTURE 99  VALUE 07.                     NC2324.2
004800 77  CON-10             PICTURE 99  VALUE 10.                     NC2324.2
004900 77  ELEM-HOLD-AREA               PICTURE X(15)  VALUE SPACES.    NC2324.2
005000 77  CON-5              PICTURE 99  VALUE 05.                     NC2324.2
005100 77  SEC-HOLD-AREA                PICTURE X(11)  VALUE SPACES.    NC2324.2
005200 77  CON-6              PICTURE 99  VALUE 06.                     NC2324.2
005300 77  GRP-HOLD-AREA                PICTURE X(5)  VALUE SPACES.     NC2324.2
005400 01  GRP-NAME.                                                    NC2324.2
005500     02  FILLER              PICTURE XXX    VALUE "GRP".          NC2324.2
005600     02  ADD-GRP             PICTURE 99     VALUE 01.             NC2324.2
005700                                                                  NC2324.2
005800 01  SEC-NAME.                                                    NC2324.2
005900     02  FILLER              PICTURE X(5)   VALUE "SEC (".        NC2324.2
006000     02  SEC-GRP             PICTURE 99     VALUE 00.             NC2324.2
006100     02  FILLER              PICTURE X      VALUE ",".            NC2324.2
006200     02  ADD-SEC             PICTURE 99     VALUE 01.             NC2324.2
006300     02  FILLER              PICTURE X      VALUE ")".            NC2324.2
006400                                                                  NC2324.2
006500 01  ELEM-NAME.                                                   NC2324.2
006600     02  FILLER              PICTURE X(6)   VALUE "ELEM (".       NC2324.2
006700     02  ELEM-GRP            PICTURE 99     VALUE 00.             NC2324.2
006800     02  FILLER              PICTURE X      VALUE ",".            NC2324.2
006900     02  ELEM-SEC            PICTURE 99     VALUE 00.             NC2324.2
007000     02  FILLER              PICTURE X      VALUE ",".            NC2324.2
007100     02  ADD-ELEM            PICTURE 99     VALUE 01.             NC2324.2
007200     02  FILLER              PICTURE X      VALUE ")".            NC2324.2
007300                                                                  NC2324.2
007400 01  3-DIMENSION-TBL.                                             NC2324.2
007500     02  GRP-ENTRY OCCURS 10 TIMES INDEXED BY IDX-1.              NC2324.2
007600         03  ENTRY-1         PICTURE X(5).                        NC2324.2
007700         03  GRP2-ENTRY OCCURS 10 TIMES INDEXED BY IDX-2.         NC2324.2
007800             04  ENTRY-2     PICTURE X(11).                       NC2324.2
007900             04  GRP3-ENTRY OCCURS 10 TIMES INDEXED BY IDX-3.     NC2324.2
008000                 05  ENTRY-3 PICTURE X(15).                       NC2324.2
008100                                                                  NC2324.2
008200 01  END-STMT.                                                    NC2324.2
008300     02  FILLER              PICTURE X(7)  VALUE "AT END ".       NC2324.2
008400     02  END-IDX             PICTURE X(5)  VALUE SPACES.          NC2324.2
008500     02  FILLER              PICTURE XXX   VALUE " = ".           NC2324.2
008600     02  IDX-VALU            PICTURE 99    VALUE 00.              NC2324.2
008700     02  FILLER              PICTURE XXX   VALUE SPACES.          NC2324.2
008800 01  NOTE-1.                                                      NC2324.2
008900     02  FILLER                   PICTURE X(74)  VALUE            NC2324.2
009000     "NOTE 1 - CORRECT AND COMPUTED DATA ARE EQUAL BUT THE AT END NC2324.2
009100-    "PATH WAS TAKEN".                                            NC2324.2
009200     02  FILLER                   PICTURE X(46)  VALUE SPACES.    NC2324.2
009300 01  NOTE-2.                                                      NC2324.2
009400     02  FILLER                   PICTURE X(112)  VALUE           NC2324.2
009500     "NOTE 2 - CORRECT AND COMPUTED DATA ARE NOT EQUAL. THE COMPUTNC2324.2
009600-    "ED ENTRY WAS EXTRACTED FROM THE TABLE BY SUBSCRIPTS.".      NC2324.2
009700     02  FILLER                   PICTURE X(8)  VALUE SPACES.     NC2324.2
009800 01  TEST-RESULTS.                                                NC2324.2
009900     02 FILLER                   PIC X      VALUE SPACE.          NC2324.2
010000     02 FEATURE                  PIC X(20)  VALUE SPACE.          NC2324.2
010100     02 FILLER                   PIC X      VALUE SPACE.          NC2324.2
010200     02 P-OR-F                   PIC X(5)   VALUE SPACE.          NC2324.2
010300     02 FILLER                   PIC X      VALUE SPACE.          NC2324.2
010400     02  PAR-NAME.                                                NC2324.2
010500       03 FILLER                 PIC X(19)  VALUE SPACE.          NC2324.2
010600       03  PARDOT-X              PIC X      VALUE SPACE.          NC2324.2
010700       03 DOTVALUE               PIC 99     VALUE ZERO.           NC2324.2
010800     02 FILLER                   PIC X(8)   VALUE SPACE.          NC2324.2
010900     02 RE-MARK                  PIC X(61).                       NC2324.2
011000 01  TEST-COMPUTED.                                               NC2324.2
011100     02 FILLER                   PIC X(30)  VALUE SPACE.          NC2324.2
011200     02 FILLER                   PIC X(17)  VALUE                 NC2324.2
011300            "       COMPUTED=".                                   NC2324.2
011400     02 COMPUTED-X.                                               NC2324.2
011500     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          NC2324.2
011600     03 COMPUTED-N               REDEFINES COMPUTED-A             NC2324.2
011700                                 PIC -9(9).9(9).                  NC2324.2
011800     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         NC2324.2
011900     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     NC2324.2
012000     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     NC2324.2
012100     03       CM-18V0 REDEFINES COMPUTED-A.                       NC2324.2
012200         04 COMPUTED-18V0                    PIC -9(18).          NC2324.2
012300         04 FILLER                           PIC X.               NC2324.2
012400     03 FILLER PIC X(50) VALUE SPACE.                             NC2324.2
012500 01  TEST-CORRECT.                                                NC2324.2
012600     02 FILLER PIC X(30) VALUE SPACE.                             NC2324.2
012700     02 FILLER PIC X(17) VALUE "       CORRECT =".                NC2324.2
012800     02 CORRECT-X.                                                NC2324.2
012900     03 CORRECT-A                  PIC X(20) VALUE SPACE.         NC2324.2
013000     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      NC2324.2
013100     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         NC2324.2
013200     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     NC2324.2
013300     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     NC2324.2
013400     03      CR-18V0 REDEFINES CORRECT-A.                         NC2324.2
013500         04 CORRECT-18V0                     PIC -9(18).          NC2324.2
013600         04 FILLER                           PIC X.               NC2324.2
013700     03 FILLER PIC X(2) VALUE SPACE.                              NC2324.2
013800     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     NC2324.2
013900 01  CCVS-C-1.                                                    NC2324.2
014000     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PANC2324.2
014100-    "SS  PARAGRAPH-NAME                                          NC2324.2
014200-    "       REMARKS".                                            NC2324.2
014300     02 FILLER                     PIC X(20)    VALUE SPACE.      NC2324.2
014400 01  CCVS-C-2.                                                    NC2324.2
014500     02 FILLER                     PIC X        VALUE SPACE.      NC2324.2
014600     02 FILLER                     PIC X(6)     VALUE "TESTED".   NC2324.2
014700     02 FILLER                     PIC X(15)    VALUE SPACE.      NC2324.2
014800     02 FILLER                     PIC X(4)     VALUE "FAIL".     NC2324.2
014900     02 FILLER                     PIC X(94)    VALUE SPACE.      NC2324.2
015000 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       NC2324.2
015100 01  REC-CT                        PIC 99       VALUE ZERO.       NC2324.2
015200 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       NC2324.2
015300 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       NC2324.2
015400 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       NC2324.2
015500 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       NC2324.2
015600 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       NC2324.2
015700 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       NC2324.2
015800 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      NC2324.2
015900 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       NC2324.2
016000 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     NC2324.2
016100 01  CCVS-H-1.                                                    NC2324.2
016200     02  FILLER                    PIC X(39)    VALUE SPACES.     NC2324.2
016300     02  FILLER                    PIC X(42)    VALUE             NC2324.2
016400     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 NC2324.2
016500     02  FILLER                    PIC X(39)    VALUE SPACES.     NC2324.2
016600 01  CCVS-H-2A.                                                   NC2324.2
016700   02  FILLER                        PIC X(40)  VALUE SPACE.      NC2324.2
016800   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  NC2324.2
016900   02  FILLER                        PIC XXXX   VALUE             NC2324.2
017000     "4.2 ".                                                      NC2324.2
017100   02  FILLER                        PIC X(28)  VALUE             NC2324.2
017200            " COPY - NOT FOR DISTRIBUTION".                       NC2324.2
017300   02  FILLER                        PIC X(41)  VALUE SPACE.      NC2324.2
017400                                                                  NC2324.2
017500 01  CCVS-H-2B.                                                   NC2324.2
017600   02  FILLER                        PIC X(15)  VALUE             NC2324.2
017700            "TEST RESULT OF ".                                    NC2324.2
017800   02  TEST-ID                       PIC X(9).                    NC2324.2
017900   02  FILLER                        PIC X(4)   VALUE             NC2324.2
018000            " IN ".                                               NC2324.2
018100   02  FILLER                        PIC X(12)  VALUE             NC2324.2
018200     " HIGH       ".                                              NC2324.2
018300   02  FILLER                        PIC X(22)  VALUE             NC2324.2
018400            " LEVEL VALIDATION FOR ".                             NC2324.2
018500   02  FILLER                        PIC X(58)  VALUE             NC2324.2
018600     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2324.2
018700 01  CCVS-H-3.                                                    NC2324.2
018800     02  FILLER                      PIC X(34)  VALUE             NC2324.2
018900            " FOR OFFICIAL USE ONLY    ".                         NC2324.2
019000     02  FILLER                      PIC X(58)  VALUE             NC2324.2
019100     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC2324.2
019200     02  FILLER                      PIC X(28)  VALUE             NC2324.2
019300            "  COPYRIGHT   1985 ".                                NC2324.2
019400 01  CCVS-E-1.                                                    NC2324.2
019500     02 FILLER                       PIC X(52)  VALUE SPACE.      NC2324.2
019600     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              NC2324.2
019700     02 ID-AGAIN                     PIC X(9).                    NC2324.2
019800     02 FILLER                       PIC X(45)  VALUE SPACES.     NC2324.2
019900 01  CCVS-E-2.                                                    NC2324.2
020000     02  FILLER                      PIC X(31)  VALUE SPACE.      NC2324.2
020100     02  FILLER                      PIC X(21)  VALUE SPACE.      NC2324.2
020200     02 CCVS-E-2-2.                                               NC2324.2
020300         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      NC2324.2
020400         03 FILLER                   PIC X      VALUE SPACE.      NC2324.2
020500         03 ENDER-DESC               PIC X(44)  VALUE             NC2324.2
020600            "ERRORS ENCOUNTERED".                                 NC2324.2
020700 01  CCVS-E-3.                                                    NC2324.2
020800     02  FILLER                      PIC X(22)  VALUE             NC2324.2
020900            " FOR OFFICIAL USE ONLY".                             NC2324.2
021000     02  FILLER                      PIC X(12)  VALUE SPACE.      NC2324.2
021100     02  FILLER                      PIC X(58)  VALUE             NC2324.2
021200     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2324.2
021300     02  FILLER                      PIC X(13)  VALUE SPACE.      NC2324.2
021400     02 FILLER                       PIC X(15)  VALUE             NC2324.2
021500             " COPYRIGHT 1985".                                   NC2324.2
021600 01  CCVS-E-4.                                                    NC2324.2
021700     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      NC2324.2
021800     02 FILLER                       PIC X(4)   VALUE " OF ".     NC2324.2
021900     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      NC2324.2
022000     02 FILLER                       PIC X(40)  VALUE             NC2324.2
022100      "  TESTS WERE EXECUTED SUCCESSFULLY".                       NC2324.2
022200 01  XXINFO.                                                      NC2324.2
022300     02 FILLER                       PIC X(19)  VALUE             NC2324.2
022400            "*** INFORMATION ***".                                NC2324.2
022500     02 INFO-TEXT.                                                NC2324.2
022600       04 FILLER                     PIC X(8)   VALUE SPACE.      NC2324.2
022700       04 XXCOMPUTED                 PIC X(20).                   NC2324.2
022800       04 FILLER                     PIC X(5)   VALUE SPACE.      NC2324.2
022900       04 XXCORRECT                  PIC X(20).                   NC2324.2
023000     02 INF-ANSI-REFERENCE           PIC X(48).                   NC2324.2
023100 01  HYPHEN-LINE.                                                 NC2324.2
023200     02 FILLER  PIC IS X VALUE IS SPACE.                          NC2324.2
023300     02 FILLER  PIC IS X(65)    VALUE IS "************************NC2324.2
023400-    "*****************************************".                 NC2324.2
023500     02 FILLER  PIC IS X(54)    VALUE IS "************************NC2324.2
023600-    "******************************".                            NC2324.2
023700 01  CCVS-PGM-ID                     PIC X(9)   VALUE             NC2324.2
023800     "NC232A".                                                    NC2324.2
023900 PROCEDURE DIVISION.                                              NC2324.2
024000 CCVS1 SECTION.                                                   NC2324.2
024100 OPEN-FILES.                                                      NC2324.2
024200     OPEN     OUTPUT PRINT-FILE.                                  NC2324.2
024300     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   NC2324.2
024400     MOVE    SPACE TO TEST-RESULTS.                               NC2324.2
024500     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             NC2324.2
024600     GO TO CCVS1-EXIT.                                            NC2324.2
024700 CLOSE-FILES.                                                     NC2324.2
024800     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   NC2324.2
024900 TERMINATE-CCVS.                                                  NC2324.2
025000S    EXIT PROGRAM.                                                NC2324.2
025100STERMINATE-CALL.                                                  NC2324.2
025200     STOP     RUN.                                                NC2324.2
025300 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         NC2324.2
025400 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           NC2324.2
025500 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          NC2324.2
025600 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      NC2324.2
025700     MOVE "****TEST DELETED****" TO RE-MARK.                      NC2324.2
025800 PRINT-DETAIL.                                                    NC2324.2
025900     IF REC-CT NOT EQUAL TO ZERO                                  NC2324.2
026000             MOVE "." TO PARDOT-X                                 NC2324.2
026100             MOVE REC-CT TO DOTVALUE.                             NC2324.2
026200     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      NC2324.2
026300     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               NC2324.2
026400        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 NC2324.2
026500          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 NC2324.2
026600     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              NC2324.2
026700     MOVE SPACE TO CORRECT-X.                                     NC2324.2
026800     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         NC2324.2
026900     MOVE     SPACE TO RE-MARK.                                   NC2324.2
027000 HEAD-ROUTINE.                                                    NC2324.2
027100     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC2324.2
027200     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC2324.2
027300     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC2324.2
027400     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC2324.2
027500 COLUMN-NAMES-ROUTINE.                                            NC2324.2
027600     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2324.2
027700     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2324.2
027800     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        NC2324.2
027900 END-ROUTINE.                                                     NC2324.2
028000     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.NC2324.2
028100 END-RTN-EXIT.                                                    NC2324.2
028200     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2324.2
028300 END-ROUTINE-1.                                                   NC2324.2
028400      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      NC2324.2
028500      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               NC2324.2
028600      ADD PASS-COUNTER TO ERROR-HOLD.                             NC2324.2
028700*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   NC2324.2
028800      MOVE PASS-COUNTER TO CCVS-E-4-1.                            NC2324.2
028900      MOVE ERROR-HOLD TO CCVS-E-4-2.                              NC2324.2
029000      MOVE CCVS-E-4 TO CCVS-E-2-2.                                NC2324.2
029100      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           NC2324.2
029200  END-ROUTINE-12.                                                 NC2324.2
029300      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        NC2324.2
029400     IF       ERROR-COUNTER IS EQUAL TO ZERO                      NC2324.2
029500         MOVE "NO " TO ERROR-TOTAL                                NC2324.2
029600         ELSE                                                     NC2324.2
029700         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       NC2324.2
029800     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           NC2324.2
029900     PERFORM WRITE-LINE.                                          NC2324.2
030000 END-ROUTINE-13.                                                  NC2324.2
030100     IF DELETE-COUNTER IS EQUAL TO ZERO                           NC2324.2
030200         MOVE "NO " TO ERROR-TOTAL  ELSE                          NC2324.2
030300         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      NC2324.2
030400     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   NC2324.2
030500     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2324.2
030600      IF   INSPECT-COUNTER EQUAL TO ZERO                          NC2324.2
030700          MOVE "NO " TO ERROR-TOTAL                               NC2324.2
030800      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   NC2324.2
030900      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            NC2324.2
031000      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          NC2324.2
031100     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2324.2
031200 WRITE-LINE.                                                      NC2324.2
031300     ADD 1 TO RECORD-COUNT.                                       NC2324.2
031400Y    IF RECORD-COUNT GREATER 50                                   NC2324.2
031500Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          NC2324.2
031600Y        MOVE SPACE TO DUMMY-RECORD                               NC2324.2
031700Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  NC2324.2
031800Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             NC2324.2
031900Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     NC2324.2
032000Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          NC2324.2
032100Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          NC2324.2
032200Y        MOVE ZERO TO RECORD-COUNT.                               NC2324.2
032300     PERFORM WRT-LN.                                              NC2324.2
032400 WRT-LN.                                                          NC2324.2
032500     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               NC2324.2
032600     MOVE SPACE TO DUMMY-RECORD.                                  NC2324.2
032700 BLANK-LINE-PRINT.                                                NC2324.2
032800     PERFORM WRT-LN.                                              NC2324.2
032900 FAIL-ROUTINE.                                                    NC2324.2
033000     IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. NC2324.2
033100     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.NC2324.2
033200     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC2324.2
033300     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   NC2324.2
033400     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2324.2
033500     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC2324.2
033600     GO TO  FAIL-ROUTINE-EX.                                      NC2324.2
033700 FAIL-ROUTINE-WRITE.                                              NC2324.2
033800     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         NC2324.2
033900     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 NC2324.2
034000     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. NC2324.2
034100     MOVE   SPACES TO COR-ANSI-REFERENCE.                         NC2324.2
034200 FAIL-ROUTINE-EX. EXIT.                                           NC2324.2
034300 BAIL-OUT.                                                        NC2324.2
034400     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   NC2324.2
034500     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           NC2324.2
034600 BAIL-OUT-WRITE.                                                  NC2324.2
034700     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  NC2324.2
034800     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC2324.2
034900     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2324.2
035000     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC2324.2
035100 BAIL-OUT-EX. EXIT.                                               NC2324.2
035200 CCVS1-EXIT.                                                      NC2324.2
035300     EXIT.                                                        NC2324.2
035400 SECT-NC232A-001 SECTION.                                         NC2324.2
035500 TH-03-001.                                                       NC2324.2
035600*                                                                 NC2324.2
035700 BUILD-LEVEL-1.                                                   NC2324.2
035800     ADD 1 TO SUB-1.                                              NC2324.2
035900     IF SUB-1 = 11 GO TO CHECK-ENTRIES.                           NC2324.2
036000     MOVE GRP-NAME TO ENTRY-1 (SUB-1).                            NC2324.2
036100     ADD 1 TO ADD-GRP.                                            NC2324.2
036200                                                                  NC2324.2
036300 BUILD-LEVEL-2.                                                   NC2324.2
036400     ADD 1 TO SUB-2.                                              NC2324.2
036500     IF SUB-2 = 11                                                NC2324.2
036600         MOVE ZERO TO SUB-2                                       NC2324.2
036700         MOVE 01 TO ADD-SEC                                       NC2324.2
036800         GO TO BUILD-LEVEL-1.                                     NC2324.2
036900     MOVE SUB-1 TO SEC-GRP.                                       NC2324.2
037000     MOVE SEC-NAME TO ENTRY-2 (SUB-1, SUB-2).                     NC2324.2
037100     ADD 1 TO ADD-SEC.                                            NC2324.2
037200                                                                  NC2324.2
037300 BUILD-LEVEL-3.                                                   NC2324.2
037400     ADD 1 TO SUB-3.                                              NC2324.2
037500     IF SUB-3 = 11                                                NC2324.2
037600         MOVE ZERO TO SUB-3                                       NC2324.2
037700              MOVE 01 TO ADD-ELEM                                 NC2324.2
037800              GO TO BUILD-LEVEL-2.                                NC2324.2
037900     MOVE SUB-1 TO ELEM-GRP.                                      NC2324.2
038000     MOVE SUB-2 TO ELEM-SEC.                                      NC2324.2
038100     MOVE ELEM-NAME TO ENTRY-3 (SUB-1, SUB-2, SUB-3).             NC2324.2
038200     ADD 1 TO ADD-ELEM.                                           NC2324.2
038300     GO TO BUILD-LEVEL-3.                                         NC2324.2
038400                                                                  NC2324.2
038500 CHECK-ENTRIES.                                                   NC2324.2
038600     MOVE "SEARCH VARYING LEV 1" TO FEATURE.                      NC2324.2
038700     MOVE "CHECK-ENTRIES       " TO PAR-NAME.                     NC2324.2
038800     MOVE "GRP02" TO GRP-HOLD-AREA.                               NC2324.2
038900     MOVE 02 TO SUB-2.                                            NC2324.2
039000     SET IDX-1 TO 1.                                              NC2324.2
039100     SEARCH GRP-ENTRY VARYING IDX-1 AT END                        NC2324.2
039200         PERFORM GRP-FAIL-PARGRAPH                                NC2324.2
039300         GO TO TH1-TEST-F1-2                                      NC2324.2
039400         WHEN ENTRY-1 (IDX-1) = GRP-HOLD-AREA NEXT SENTENCE.      NC2324.2
039500     PERFORM PASS                                                 NC2324.2
039600     PERFORM PRINT-DETAIL.                                        NC2324.2
039700     GO TO TH1-TEST-F1-2.                                         NC2324.2
039800                                                                  NC2324.2
039900 GRP-FAIL-PARGRAPH.                                               NC2324.2
040000     MOVE GRP-HOLD-AREA TO CORRECT-A.                             NC2324.2
040100     IF ENTRY-1 (SUB-2) EQUAL TO GRP-HOLD-AREA                    NC2324.2
040200         MOVE "IDX-1" TO END-IDX                                  NC2324.2
040300         SET IDX-VALU TO IDX-1                                    NC2324.2
040400         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2324.2
040500         MOVE END-STMT TO COMPUTED-A  ELSE                        NC2324.2
040600     MOVE ENTRY-1 (SUB-2) TO COMPUTED-A                           NC2324.2
040700     MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.               NC2324.2
040800                                                                  NC2324.2
040900     PERFORM FAIL                                                 NC2324.2
041000     PERFORM PRINT-DETAIL.                                        NC2324.2
041100*                                                                 NC2324.2
041200 TH1-INIT-F1-2.                                                   NC2324.2
041300     MOVE "TH1-TEST-F1-2      " TO PAR-NAME.                      NC2324.2
041400     MOVE "VI-122 6.21"   TO ANSI-REFERENCE.                      NC2324.2
041500     MOVE "GRP01" TO GRP-HOLD-AREA.                               NC2324.2
041600     MOVE 01 TO SUB-2.                                            NC2324.2
041700     SET IDX-1 TO 1.                                              NC2324.2
041800 TH1-TEST-F1-2.                                                   NC2324.2
041900     SEARCH GRP-ENTRY VARYING IDX-1 AT END                        NC2324.2
042000         GO TO TH1-FAIL-F1-2                                      NC2324.2
042100         WHEN ENTRY-1 (IDX-1) = GRP-HOLD-AREA NEXT SENTENCE.      NC2324.2
042200                                                                  NC2324.2
042300     PERFORM PASS                                                 NC2324.2
042400     GO TO TH1-WRITE-F1-2.                                        NC2324.2
042500 TH1-DELETE-F1-2.                                                 NC2324.2
042600     PERFORM DE-LETE.                                             NC2324.2
042700     GO TO TH1-WRITE-F1-2.                                        NC2324.2
042800 TH1-FAIL-F1-2.                                                   NC2324.2
042900     PERFORM FAIL.                                                NC2324.2
043000 TH1-WRITE-F1-2.                                                  NC2324.2
043100     PERFORM PRINT-DETAIL.                                        NC2324.2
043200*                                                                 NC2324.2
043300 TH1-INIT-F1-3.                                                   NC2324.2
043400     MOVE "TH1-TEST-F1-3      " TO PAR-NAME.                      NC2324.2
043500     MOVE "VI-122 6.21"   TO ANSI-REFERENCE.                      NC2324.2
043600     MOVE "GRP10" TO GRP-HOLD-AREA.                               NC2324.2
043700     MOVE 10 TO SUB-2.                                            NC2324.2
043800     SET IDX-1 TO 1.                                              NC2324.2
043900 TH1-TEST-F1-3.                                                   NC2324.2
044000     SEARCH GRP-ENTRY VARYING IDX-1 AT END                        NC2324.2
044100         GO TO TH1-FAIL-F1-3                                      NC2324.2
044200         WHEN ENTRY-1 (IDX-1) = GRP-HOLD-AREA NEXT SENTENCE.      NC2324.2
044300                                                                  NC2324.2
044400     PERFORM PASS                                                 NC2324.2
044500     GO TO TH1-WRITE-F1-3.                                        NC2324.2
044600 TH1-DELETE-F1-3.                                                 NC2324.2
044700     PERFORM DE-LETE.                                             NC2324.2
044800     GO TO TH1-WRITE-F1-3.                                        NC2324.2
044900 TH1-FAIL-F1-3.                                                   NC2324.2
045000     PERFORM FAIL.                                                NC2324.2
045100 TH1-WRITE-F1-3.                                                  NC2324.2
045200     PERFORM PRINT-DETAIL.                                        NC2324.2
045300*                                                                 NC2324.2
045400 TH1-INIT-F1-4.                                                   NC2324.2
045500     MOVE "TH1-TEST-F1-4      " TO PAR-NAME.                      NC2324.2
045600     MOVE "VI-122 6.21"   TO ANSI-REFERENCE.                      NC2324.2
045700     MOVE "GRP05" TO GRP-HOLD-AREA.                               NC2324.2
045800     MOVE 05 TO SUB-2.                                            NC2324.2
045900     SET IDX-1 TO 05.                                             NC2324.2
046000 TH1-TEST-F1-4.                                                   NC2324.2
046100     SEARCH GRP-ENTRY VARYING IDX-1                               NC2324.2
046200         WHEN ENTRY-1 (IDX-1) = GRP-HOLD-AREA                     NC2324.2
046300             GO TO PASS-TH1-F1-4.                                 NC2324.2
046400     GO TO TH1-FAIL-F1-4.                                         NC2324.2
046500 PASS-TH1-F1-4.                                                   NC2324.2
046600                                                                  NC2324.2
046700     PERFORM PASS                                                 NC2324.2
046800     GO TO TH1-WRITE-F1-4.                                        NC2324.2
046900 TH1-DELETE-F1-4.                                                 NC2324.2
047000     PERFORM DE-LETE.                                             NC2324.2
047100     GO TO TH1-WRITE-F1-4.                                        NC2324.2
047200 TH1-FAIL-F1-4.                                                   NC2324.2
047300     PERFORM GRP-FAIL-PARGRAPH.                                   NC2324.2
047400 TH1-WRITE-F1-4.                                                  NC2324.2
047500     PERFORM PRINT-DETAIL.                                        NC2324.2
047600*                                                                 NC2324.2
047700 TH2-INIT-F1-1.                                                   NC2324.2
047800     MOVE "TH2-TEST-F1-1      " TO PAR-NAME.                      NC2324.2
047900     MOVE "VI-122 6.21"   TO ANSI-REFERENCE.                      NC2324.2
048000     MOVE "SEARCH VARYING LEV 2" TO FEATURE.                      NC2324.2
048100     MOVE "SEC (01,01)" TO SEC-HOLD-AREA.                         NC2324.2
048200     MOVE 1 TO SUB-1 SUB-2.                                       NC2324.2
048300     SET IDX-1 IDX-2 TO 1.                                        NC2324.2
048400 TH2-TEST-F1-1.                                                   NC2324.2
048500     SEARCH GRP2-ENTRY VARYING IDX-2 AT END                       NC2324.2
048600         GO TO TH2-FAIL-F1-1                                      NC2324.2
048700         WHEN ENTRY-2 (IDX-1, IDX-2) = SEC-HOLD-AREA              NC2324.2
048800             NEXT SENTENCE.                                       NC2324.2
048900                                                                  NC2324.2
049000     PERFORM PASS.                                                NC2324.2
049100     GO TO TH2-WRITE-F1-1.                                        NC2324.2
049200 TH2-DELETE-F1-1.                                                 NC2324.2
049300     PERFORM DE-LETE.                                             NC2324.2
049400     GO TO TH2-WRITE-F1-1.                                        NC2324.2
049500 TH2-FAIL-F1-1.                                                   NC2324.2
049600     PERFORM SEC-FAIL-PARGRAF.                                    NC2324.2
049700 TH2-WRITE-F1-1.                                                  NC2324.2
049800     PERFORM PRINT-DETAIL.                                        NC2324.2
049900                                                                  NC2324.2
050000 TH2-INIT-F1-2.                                                   NC2324.2
050100     MOVE "TH2-TEST-F1-2      " TO PAR-NAME.                      NC2324.2
050200     MOVE "VI-122 6.21"   TO ANSI-REFERENCE.                      NC2324.2
050300     MOVE "SEARCH VARYING LEV 2" TO FEATURE.                      NC2324.2
050400     MOVE "SEC (05,10)" TO SEC-HOLD-AREA.                         NC2324.2
050500     MOVE 05 TO SUB-1.                                            NC2324.2
050600     MOVE 10 TO SUB-2.                                            NC2324.2
050700     SET IDX-1 TO 5.                                              NC2324.2
050800     SET IDX-2 TO 1.                                              NC2324.2
050900 TH2-TEST-F1-2.                                                   NC2324.2
051000     SEARCH GRP2-ENTRY VARYING IDX-2 AT END                       NC2324.2
051100         GO TO TH2-FAIL-F1-2                                      NC2324.2
051200         WHEN ENTRY-2 (IDX-1, IDX-2) = SEC-HOLD-AREA              NC2324.2
051300             NEXT SENTENCE.                                       NC2324.2
051400                                                                  NC2324.2
051500     PERFORM PASS                                                 NC2324.2
051600     GO TO TH2-WRITE-F1-2.                                        NC2324.2
051700 TH2-DELETE-F1-2.                                                 NC2324.2
051800     PERFORM DE-LETE.                                             NC2324.2
051900     GO TO TH2-WRITE-F1-2.                                        NC2324.2
052000 TH2-FAIL-F1-2.                                                   NC2324.2
052100     PERFORM SEC-FAIL-PARGRAF.                                    NC2324.2
052200 TH2-WRITE-F1-2.                                                  NC2324.2
052300     PERFORM PRINT-DETAIL.                                        NC2324.2
052400*                                                                 NC2324.2
052500 TH2-INIT-F1-3.                                                   NC2324.2
052600     MOVE "TH2-TEST-F1-3      " TO PAR-NAME.                      NC2324.2
052700     MOVE "VI-122 6.21"   TO ANSI-REFERENCE.                      NC2324.2
052800     MOVE "SEARCH VARYING LEV 2" TO FEATURE.                      NC2324.2
052900     MOVE "SEC (10,10)" TO SEC-HOLD-AREA.                         NC2324.2
053000     SET IDX-1 TO 10.                                             NC2324.2
053100     SET IDX-2 TO 1.                                              NC2324.2
053200     MOVE 10 TO SUB-1  SUB-2.                                     NC2324.2
053300 TH2-TEST-F1-3.                                                   NC2324.2
053400     SEARCH GRP2-ENTRY VARYING IDX-2 AT END                       NC2324.2
053500         GO TO TH2-FAIL-F1-3                                      NC2324.2
053600         WHEN ENTRY-2 (IDX-1, IDX-2) = SEC-HOLD-AREA              NC2324.2
053700             NEXT SENTENCE.                                       NC2324.2
053800                                                                  NC2324.2
053900     PERFORM PASS                                                 NC2324.2
054000     GO TO TH2-WRITE-F1-3.                                        NC2324.2
054100 TH2-DELETE-F1-3.                                                 NC2324.2
054200     PERFORM DE-LETE.                                             NC2324.2
054300     GO TO TH2-WRITE-F1-3.                                        NC2324.2
054400 TH2-FAIL-F1-3.                                                   NC2324.2
054500     PERFORM SEC-FAIL-PARGRAF.                                    NC2324.2
054600 TH2-WRITE-F1-3.                                                  NC2324.2
054700     PERFORM PRINT-DETAIL.                                        NC2324.2
054800*                                                                 NC2324.2
054900 TH2-INIT-F1-4.                                                   NC2324.2
055000     MOVE "TH2-TEST-F1-4      " TO PAR-NAME.                      NC2324.2
055100     MOVE "VI-122 6.21"   TO ANSI-REFERENCE.                      NC2324.2
055200     MOVE "SEARCH VARYING LEV 2" TO FEATURE.                      NC2324.2
055300     MOVE "SEC (08,02)" TO SEC-HOLD-AREA.                         NC2324.2
055400     MOVE 08 TO SUB-1.                                            NC2324.2
055500     MOVE 02 TO SUB-2.                                            NC2324.2
055600     SET IDX-1 TO 08.                                             NC2324.2
055700     SET IDX-2 TO 01.                                             NC2324.2
055800 TH2-TEST-F1-4.                                                   NC2324.2
055900     SEARCH GRP2-ENTRY VARYING IDX-2                              NC2324.2
056000         WHEN ENTRY-2 (IDX-1, IDX-2) = SEC-HOLD-AREA              NC2324.2
056100             PERFORM PASS                                         NC2324.2
056200             GO TO TH2-WRITE-F1-4.                                NC2324.2
056300     GO TO TH2-FAIL-F1-4.                                         NC2324.2
056400 TH2-DELETE-F1-4.                                                 NC2324.2
056500     PERFORM DE-LETE.                                             NC2324.2
056600     GO TO TH2-WRITE-F1-4.                                        NC2324.2
056700 TH2-FAIL-F1-4.                                                   NC2324.2
056800     PERFORM SEC-FAIL-PARGRAF.                                    NC2324.2
056900 TH2-WRITE-F1-4.                                                  NC2324.2
057000     PERFORM PRINT-DETAIL.                                        NC2324.2
057100     GO TO TH3-INIT-F1-1.                                         NC2324.2
057200*                                                                 NC2324.2
057300 SEC-FAIL-PARGRAF.                                                NC2324.2
057400     MOVE SEC-HOLD-AREA TO CORRECT-A.                             NC2324.2
057500     IF ENTRY-2 (SUB-1, SUB-2) EQUAL TO SEC-HOLD-AREA             NC2324.2
057600         MOVE "IDX-2" TO END-IDX                                  NC2324.2
057700         SET IDX-VALU TO IDX-2                                    NC2324.2
057800         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2324.2
057900         MOVE END-STMT TO COMPUTED-A ELSE                         NC2324.2
058000     MOVE ENTRY-2 (SUB-1, SUB-2) TO COMPUTED-A                    NC2324.2
058100     MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.               NC2324.2
058200                                                                  NC2324.2
058300     PERFORM FAIL.                                                NC2324.2
058400                                                                  NC2324.2
058500 TH3-INIT-F1-1.                                                   NC2324.2
058600     MOVE "TH3-TEST-F1-1      " TO PAR-NAME.                      NC2324.2
058700     MOVE "VI-122 6.21"   TO ANSI-REFERENCE.                      NC2324.2
058800     MOVE "SEARCH VARYING LEV 3" TO FEATURE.                      NC2324.2
058900     MOVE 1 TO SUB-1  SUB-2  SUB-3.                               NC2324.2
059000     MOVE "ELEM (01,01,01)" TO ELEM-HOLD-AREA.                    NC2324.2
059100     SET IDX-1 IDX-2 IDX-3 TO 1.                                  NC2324.2
059200 TH3-TEST-F1-1.                                                   NC2324.2
059300     SEARCH GRP3-ENTRY VARYING IDX-3                              NC2324.2
059400         WHEN ENTRY-3 (IDX-1, IDX-2, IDX-3) = ELEM-HOLD-AREA      NC2324.2
059500             GO TO PASS-TH3-1.                                    NC2324.2
059600     GO TO TH3-FAIL-F1-1.                                         NC2324.2
059700 PASS-TH3-1.                                                      NC2324.2
059800                                                                  NC2324.2
059900     PERFORM PASS.                                                NC2324.2
060000     GO TO TH3-WRITE-F1-1.                                        NC2324.2
060100 TH3-DELETE-F1-1.                                                 NC2324.2
060200     PERFORM DE-LETE.                                             NC2324.2
060300     GO TO TH3-WRITE-F1-1.                                        NC2324.2
060400 TH3-FAIL-F1-1.                                                   NC2324.2
060500     PERFORM ELEM-FAIL-PARA.                                      NC2324.2
060600 TH3-WRITE-F1-1.                                                  NC2324.2
060700     PERFORM PRINT-DETAIL.                                        NC2324.2
060800*                                                                 NC2324.2
060900 TH3-INIT-F1-2.                                                   NC2324.2
061000     MOVE "TH3-TEST-F1-2      " TO PAR-NAME.                      NC2324.2
061100     MOVE "VI-122 6.21"   TO ANSI-REFERENCE.                      NC2324.2
061200     MOVE "SEARCH VARYING LEV 3" TO FEATURE.                      NC2324.2
061300     MOVE 05 TO SUB-1.                                            NC2324.2
061400     MOVE 06 TO SUB-2.                                            NC2324.2
061500     MOVE 07 TO SUB-3.                                            NC2324.2
061600     SET IDX-1 TO 05.                                             NC2324.2
061700     SET IDX-2 TO 06.                                             NC2324.2
061800     SET IDX-3 TO 1.                                              NC2324.2
061900     MOVE "ELEM (05,06,07)" TO ELEM-HOLD-AREA.                    NC2324.2
062000 TH3-TEST-F1-2.                                                   NC2324.2
062100     SEARCH GRP3-ENTRY VARYING IDX-3 AT END                       NC2324.2
062200         GO TO TH3-FAIL-F1-2                                      NC2324.2
062300         WHEN ENTRY-3 (IDX-1, IDX-2, IDX-3) = ELEM-HOLD-AREA      NC2324.2
062400             NEXT SENTENCE.                                       NC2324.2
062500                                                                  NC2324.2
062600     PERFORM PASS                                                 NC2324.2
062700     GO TO TH3-WRITE-F1-2.                                        NC2324.2
062800 TH3-DELETE-F1-2.                                                 NC2324.2
062900     PERFORM DE-LETE.                                             NC2324.2
063000     GO TO TH3-WRITE-F1-2.                                        NC2324.2
063100 TH3-FAIL-F1-2.                                                   NC2324.2
063200     PERFORM ELEM-FAIL-PARA.                                      NC2324.2
063300 TH3-WRITE-F1-2.                                                  NC2324.2
063400     PERFORM PRINT-DETAIL.                                        NC2324.2
063500*                                                                 NC2324.2
063600 TH3-INIT-F1-3.                                                   NC2324.2
063700     MOVE "TH3-TEST-F1-3      " TO PAR-NAME.                      NC2324.2
063800     MOVE "VI-122 6.21"   TO ANSI-REFERENCE.                      NC2324.2
063900     MOVE "SEARCH VARYING LEV 3" TO FEATURE.                      NC2324.2
064000     MOVE 10 TO SUB-1 SUB-2 SUB-3.                                NC2324.2
064100     SET IDX-1 IDX-2 TO 10.                                       NC2324.2
064200     SET IDX-3 TO 1.                                              NC2324.2
064300 TH3-TEST-F1-3.                                                   NC2324.2
064400     MOVE "ELEM (10,10,10)" TO ELEM-HOLD-AREA.                    NC2324.2
064500     SEARCH GRP3-ENTRY VARYING IDX-3 AT END                       NC2324.2
064600         GO TO TH3-FAIL-F1-3                                      NC2324.2
064700         WHEN ENTRY-3 (IDX-1, IDX-2, IDX-3) = ELEM-HOLD-AREA      NC2324.2
064800             NEXT SENTENCE.                                       NC2324.2
064900                                                                  NC2324.2
065000     PERFORM PASS                                                 NC2324.2
065100     GO TO TH3-WRITE-F1-3.                                        NC2324.2
065200 TH3-DELETE-F1-3.                                                 NC2324.2
065300     PERFORM DE-LETE.                                             NC2324.2
065400     GO TO TH3-WRITE-F1-3.                                        NC2324.2
065500 TH3-FAIL-F1-3.                                                   NC2324.2
065600     PERFORM ELEM-FAIL-PARA.                                      NC2324.2
065700 TH3-WRITE-F1-3.                                                  NC2324.2
065800     PERFORM PRINT-DETAIL.                                        NC2324.2
065900*                                                                 NC2324.2
066000 TH3-INIT-F1-4.                                                   NC2324.2
066100     MOVE "TH3-TEST-F1-4      " TO PAR-NAME.                      NC2324.2
066200     MOVE "VI-122 6.21"   TO ANSI-REFERENCE.                      NC2324.2
066300     MOVE "SEARCH VARYING LEV 3" TO FEATURE.                      NC2324.2
066400     MOVE "ELEM (07,06,05)" TO ELEM-HOLD-AREA.                    NC2324.2
066500     MOVE 07 TO SUB-1.                                            NC2324.2
066600     MOVE 06 TO SUB-2.                                            NC2324.2
066700     MOVE 05 TO SUB-3.                                            NC2324.2
066800     SET IDX-1 TO 07.                                             NC2324.2
066900     SET IDX-2 TO 06.                                             NC2324.2
067000     SET IDX-3 TO 03.                                             NC2324.2
067100 TH3-TEST-F1-4.                                                   NC2324.2
067200     SEARCH GRP3-ENTRY VARYING IDX-3 AT END                       NC2324.2
067300         GO TO TH3-TEST-F1-4                                      NC2324.2
067400         WHEN ENTRY-3 (IDX-1, IDX-2, IDX-3) = ELEM-HOLD-AREA      NC2324.2
067500         NEXT SENTENCE.                                           NC2324.2
067600                                                                  NC2324.2
067700     PERFORM PASS                                                 NC2324.2
067800     GO TO TH3-WRITE-F1-4.                                        NC2324.2
067900 TH3-DELETE-F1-4.                                                 NC2324.2
068000     PERFORM DE-LETE.                                             NC2324.2
068100     GO TO TH3-WRITE-F1-4.                                        NC2324.2
068200 TH3-FAIL-F1-4.                                                   NC2324.2
068300     PERFORM ELEM-FAIL-PARA.                                      NC2324.2
068400 TH3-WRITE-F1-4.                                                  NC2324.2
068500     PERFORM PRINT-DETAIL.                                        NC2324.2
068600     GO TO SCH-INIT-F1-1.                                         NC2324.2
068700*                                                                 NC2324.2
068800 ELEM-FAIL-PARA.                                                  NC2324.2
068900     MOVE ELEM-HOLD-AREA TO CORRECT-A.                            NC2324.2
069000     IF ENTRY-3 (SUB-1, SUB-2, SUB-3) EQUAL TO ELEM-HOLD-AREA     NC2324.2
069100         MOVE "IDX-3" TO END-IDX                                  NC2324.2
069200         SET IDX-VALU TO IDX-3                                    NC2324.2
069300         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2324.2
069400         MOVE END-STMT TO COMPUTED-A  ELSE                        NC2324.2
069500     MOVE ENTRY-3 (SUB-1, SUB-2, SUB-3) TO COMPUTED-A             NC2324.2
069600     MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.               NC2324.2
069700     PERFORM FAIL.                                                NC2324.2
069800*                                                                 NC2324.2
069900 SCH-INIT-F1-1.                                                   NC2324.2
070000     MOVE "SCH-TEST-F1-1  " TO PAR-NAME.                          NC2324.2
070100     MOVE "VI-122 6.21"   TO ANSI-REFERENCE.                      NC2324.2
070200     MOVE "MULTIPLE SEARCH STMT" TO FEATURE.                      NC2324.2
070300     MOVE "GRP08" TO GRP-HOLD-AREA.                               NC2324.2
070400     MOVE "SEC (08,07)" TO SEC-HOLD-AREA.                         NC2324.2
070500     SET IDX-1 IDX-2 TO 1.                                        NC2324.2
070600 SCH-TEST-F1-1.                                                   NC2324.2
070700     SEARCH GRP-ENTRY VARYING IDX-1 AT END GO TO SCH-FAIL-F1-1-A  NC2324.2
070800         WHEN ENTRY-1 (IDX-1) = "GRP08" NEXT SENTENCE.            NC2324.2
070900     SEARCH GRP2-ENTRY VARYING IDX-2 AT END GO TO SCH-FAIL-F1-1-B NC2324.2
071000         WHEN ENTRY-2 (IDX-1, IDX-2) = SEC-HOLD-AREA              NC2324.2
071100             NEXT SENTENCE.                                       NC2324.2
071200     PERFORM PASS                                                 NC2324.2
071300     GO TO SCH-WRITE-F1-1.                                        NC2324.2
071400 SCH-DELETE-F1-1.                                                 NC2324.2
071500     PERFORM DE-LETE.                                             NC2324.2
071600     GO TO SCH-WRITE-F1-1.                                        NC2324.2
071700 SCH-FAIL-F1-1-A.                                                 NC2324.2
071800     MOVE GRP-HOLD-AREA TO CORRECT-A.                             NC2324.2
071900     IF ENTRY-1 (08) EQUAL TO GRP-HOLD-AREA                       NC2324.2
072000         MOVE "IDX-1" TO END-IDX                                  NC2324.2
072100         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2324.2
072200         SET IDX-VALU TO IDX-1                                    NC2324.2
072300         MOVE END-STMT TO COMPUTED-A  ELSE                        NC2324.2
072400     MOVE ENTRY-1 (08) TO COMPUTED-A                              NC2324.2
072500     MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.               NC2324.2
072600                                                                  NC2324.2
072700     PERFORM FAIL                                                 NC2324.2
072800     GO TO SCH-WRITE-F1-1.                                        NC2324.2
072900 SCH-FAIL-F1-1-B.                                                 NC2324.2
073000     MOVE SEC-HOLD-AREA TO CORRECT-A.                             NC2324.2
073100     IF ENTRY-2 (08, 07) EQUAL TO SEC-HOLD-AREA                   NC2324.2
073200         MOVE "IDX-2" TO END-IDX                                  NC2324.2
073300         SET IDX-VALU TO IDX-2                                    NC2324.2
073400         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2324.2
073500         MOVE END-STMT TO COMPUTED-A  ELSE                        NC2324.2
073600     MOVE ENTRY-2 (08, 07) TO COMPUTED-A                          NC2324.2
073700     MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.               NC2324.2
073800                                                                  NC2324.2
073900     PERFORM FAIL.                                                NC2324.2
074000 SCH-WRITE-F1-1.                                                  NC2324.2
074100     PERFORM PRINT-DETAIL.                                        NC2324.2
074200*                                                                 NC2324.2
074300 SCH-INIT-F1-2.                                                   NC2324.2
074400     MOVE "SCH-TEST-F1-2  " TO PAR-NAME.                          NC2324.2
074500     MOVE "VI-122 6.21"   TO ANSI-REFERENCE.                      NC2324.2
074600     MOVE "MULTIPLE SEARCH STMT" TO FEATURE.                      NC2324.2
074700     MOVE "GRP04" TO GRP-HOLD-AREA.                               NC2324.2
074800     MOVE "SEC (04,04)" TO SEC-HOLD-AREA.                         NC2324.2
074900     MOVE "ELEM (04,04,04)" TO ELEM-HOLD-AREA.                    NC2324.2
075000     SET IDX-1 IDX-2 IDX-3 TO 1.                                  NC2324.2
075100 SCH-TEST-F1-2.                                                   NC2324.2
075200     SEARCH GRP-ENTRY VARYING IDX-1                               NC2324.2
075300         AT END                                                   NC2324.2
075400              GO TO SCH-FAIL-F1-2-A                               NC2324.2
075500         WHEN ENTRY-1 (IDX-1) = GRP-HOLD-AREA                     NC2324.2
075600              NEXT SENTENCE.                                      NC2324.2
075700*                                                                 NC2324.2
075800     SEARCH GRP2-ENTRY VARYING IDX-2                              NC2324.2
075900         AT END                                                   NC2324.2
076000              GO TO SCH-FAIL-F1-2-B                               NC2324.2
076100         WHEN ENTRY-2 (IDX-1, IDX-2) = SEC-HOLD-AREA              NC2324.2
076200              NEXT SENTENCE.                                      NC2324.2
076300*                                                                 NC2324.2
076400     SEARCH GRP3-ENTRY VARYING IDX-3                              NC2324.2
076500         AT END                                                   NC2324.2
076600              GO TO SCH-FAIL-F1-2-C                               NC2324.2
076700         WHEN ENTRY-3 (IDX-1, IDX-2, IDX-3) = ELEM-HOLD-AREA      NC2324.2
076800              NEXT SENTENCE.                                      NC2324.2
076900                                                                  NC2324.2
077000     PERFORM PASS                                                 NC2324.2
077100     GO TO SCH-WRITE-F1-2.                                        NC2324.2
077200 SCH-FAIL-F1-2-A.                                                 NC2324.2
077300     MOVE GRP-HOLD-AREA TO CORRECT-A.                             NC2324.2
077400     IF ENTRY-1 (04) EQUAL TO GRP-HOLD-AREA                       NC2324.2
077500         MOVE "IDX-1" TO END-IDX                                  NC2324.2
077600         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2324.2
077700         SET IDX-VALU TO IDX-1                                    NC2324.2
077800         MOVE END-STMT TO COMPUTED-A                              NC2324.2
077900     ELSE                                                         NC2324.2
078000         MOVE ENTRY-1 (04) TO COMPUTED-A                          NC2324.2
078100         MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.           NC2324.2
078200     PERFORM FAIL.                                                NC2324.2
078300     GO TO SCH-WRITE-F1-2.                                        NC2324.2
078400 SCH-FAIL-F1-2-B.                                                 NC2324.2
078500     MOVE  SEC-HOLD-AREA TO CORRECT-A.                            NC2324.2
078600     IF ENTRY-2 (04, 04) EQUAL TO SEC-HOLD-AREA                   NC2324.2
078700         MOVE "IDX-2" TO END-IDX                                  NC2324.2
078800         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2324.2
078900         SET IDX-VALU TO IDX-2                                    NC2324.2
079000         MOVE END-STMT TO COMPUTED-A                              NC2324.2
079100     ELSE                                                         NC2324.2
079200         MOVE ENTRY-2 (04, 04) TO COMPUTED-A                      NC2324.2
079300         MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.           NC2324.2
079400     PERFORM FAIL                                                 NC2324.2
079500     GO TO SCH-WRITE-F1-2.                                        NC2324.2
079600 SCH-FAIL-F1-2-C.                                                 NC2324.2
079700     MOVE ELEM-HOLD-AREA TO CORRECT-A.                            NC2324.2
079800     IF ENTRY-3 (04, 04, 04) EQUAL TO ELEM-HOLD-AREA              NC2324.2
079900         MOVE "IDX-3" TO END-IDX                                  NC2324.2
080000         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2324.2
080100         SET IDX-VALU TO IDX-3                                    NC2324.2
080200         MOVE END-STMT TO COMPUTED-A                              NC2324.2
080300     ELSE                                                         NC2324.2
080400         MOVE ENTRY-3 (04, 04, 04) TO COMPUTED-A                  NC2324.2
080500         MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.           NC2324.2
080600     PERFORM FAIL.                                                NC2324.2
080700 SCH-WRITE-F1-2.                                                  NC2324.2
080800     PERFORM PRINT-DETAIL.                                        NC2324.2
080900*                                                                 NC2324.2
081000 SPC-INIT-F1-1.                                                   NC2324.2
081100     MOVE "SPC-TEST-F1-1      " TO PAR-NAME.                      NC2324.2
081200     MOVE "VI-122 6.21"   TO ANSI-REFERENCE.                      NC2324.2
081300     MOVE "SEARCH WITH HI INDEX" TO FEATURE.                      NC2324.2
081400     SET IDX-1 TO 4.                                              NC2324.2
081500 SPC-TEST-F1-1.                                                   NC2324.2
081600     SEARCH GRP-ENTRY VARYING IDX-1                               NC2324.2
081700         AT END                                                   NC2324.2
081800              PERFORM PASS                                        NC2324.2
081900              GO TO SPC-WRITE-F1-1                                NC2324.2
082000         WHEN ENTRY-1 (IDX-1) = "GRP03"                           NC2324.2
082100              GO TO SPC-FAIL-F1-1.                                NC2324.2
082200 SPC-DELETE-F1-1.                                                 NC2324.2
082300     PERFORM DE-LETE.                                             NC2324.2
082400     GO TO SPC-WRITE-F1-1.                                        NC2324.2
082500 SPC-FAIL-F1-1.                                                   NC2324.2
082600     MOVE SPACES TO CORRECT-A.                                    NC2324.2
082700     MOVE ENTRY-1 (03) TO COMPUTED-A.                             NC2324.2
082800     MOVE SPACES TO RE-MARK.                                      NC2324.2
082900     PERFORM FAIL.                                                NC2324.2
083000 SPC-WRITE-F1-1.                                                  NC2324.2
083100     PERFORM PRINT-DETAIL.                                        NC2324.2
083200*                                                                 NC2324.2
083300 SPC-INIT-F1-2.                                                   NC2324.2
083400     MOVE "SPC-TEST-F1-2" TO PAR-NAME.                            NC2324.2
083500     MOVE "VI-122 6.21"   TO ANSI-REFERENCE.                      NC2324.2
083600     MOVE "SEARCH WITH HI INDEX" TO FEATURE.                      NC2324.2
083700     SET IDX-1 TO 4.                                              NC2324.2
083800     SET IDX-2 TO 5.                                              NC2324.2
083900 SPC-TEST-F1-2.                                                   NC2324.2
084000     SEARCH GRP-ENTRY VARYING IDX-1                               NC2324.2
084100         AT END GO TO SPC-FAIL-F1-2-A                             NC2324.2
084200         WHEN ENTRY-1 (IDX-1) = "GRP04" NEXT SENTENCE.            NC2324.2
084300     SEARCH GRP2-ENTRY VARYING IDX-2                              NC2324.2
084400         AT END PERFORM PASS                                      NC2324.2
084500                GO TO SPC-WRITE-F1-2                              NC2324.2
084600         WHEN ENTRY-2 (IDX-1, IDX-2) = "SEC (04,04)"              NC2324.2
084700                GO TO SPC-FAIL-F1-2-B.                            NC2324.2
084800 SPC-DELETE-F1-2.                                                 NC2324.2
084900     PERFORM DE-LETE.                                             NC2324.2
085000     GO TO SPC-WRITE-F1-2.                                        NC2324.2
085100 SPC-FAIL-F1-2-A.                                                 NC2324.2
085200     MOVE "GRP04" TO CORRECT-A.                                   NC2324.2
085300     IF ENTRY-1 (04) EQUAL TO "GRP04"                             NC2324.2
085400         MOVE "IDX-2" TO END-IDX                                  NC2324.2
085500         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2324.2
085600         SET IDX-VALU TO IDX-2                                    NC2324.2
085700         MOVE END-STMT TO COMPUTED-A  ELSE                        NC2324.2
085800     MOVE ENTRY-1 (04) TO COMPUTED-A                              NC2324.2
085900     MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.               NC2324.2
086000     PERFORM FAIL                                                 NC2324.2
086100     GO TO SPC-WRITE-F1-2.                                        NC2324.2
086200 SPC-FAIL-F1-2-B.                                                 NC2324.2
086300     MOVE ENTRY-2 (04, 04) TO COMPUTED-A                          NC2324.2
086400     MOVE SPACES TO CORRECT-A.                                    NC2324.2
086500     PERFORM FAIL.                                                NC2324.2
086600 SPC-WRITE-F1-2.                                                  NC2324.2
086700     PERFORM PRINT-DETAIL.                                        NC2324.2
086800*                                                                 NC2324.2
086900 SPC-INIT-F1-3.                                                   NC2324.2
087000     MOVE "SPC-TEST-F1-3" TO PAR-NAME.                            NC2324.2
087100     MOVE "VI-122 6.21"   TO ANSI-REFERENCE.                      NC2324.2
087200     MOVE "SEARCH WITH HI INDEX" TO FEATURE.                      NC2324.2
087300     SET IDX-1 TO 02.                                             NC2324.2
087400 SPC-TEST-F1-3.                                                   NC2324.2
087500     SEARCH GRP-ENTRY VARYING IDX-1                               NC2324.2
087600         AT END                                                   NC2324.2
087700                GO TO SPC-FAIL-F1-3-A                             NC2324.2
087800         WHEN ENTRY-1 (IDX-1) EQUAL TO "GRP02"                    NC2324.2
087900                NEXT SENTENCE.                                    NC2324.2
088000     SET IDX-2 TO 01.                                             NC2324.2
088100     SEARCH GRP2-ENTRY VARYING IDX-2                              NC2324.2
088200         AT END                                                   NC2324.2
088300                GO TO SPC-FAIL-F1-3-B                             NC2324.2
088400         WHEN ENTRY-2 (IDX-1, IDX-2) = "SEC (02,03)"              NC2324.2
088500                NEXT SENTENCE.                                    NC2324.2
088600     SET IDX-3 TO 05.                                             NC2324.2
088700     SEARCH GRP3-ENTRY VARYING IDX-3                              NC2324.2
088800         AT END PERFORM PASS                                      NC2324.2
088900                GO TO SPC-WRITE-F1-3                              NC2324.2
089000         WHEN ENTRY-3 (IDX-1, IDX-2, IDX-3) = "ELEM (02,03,04)"   NC2324.2
089100                GO TO SPC-FAIL-F1-3-C.                            NC2324.2
089200 SPC-FAIL-F1-3-A.                                                 NC2324.2
089300     MOVE "GRP02" TO CORRECT-A.                                   NC2324.2
089400     IF ENTRY-1 (02) EQUAL TO "GRP02"                             NC2324.2
089500         MOVE "IDX-1" TO END-IDX                                  NC2324.2
089600         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2324.2
089700         SET IDX-VALU TO IDX-1                                    NC2324.2
089800         MOVE END-STMT TO COMPUTED-A  ELSE                        NC2324.2
089900     MOVE ENTRY-1 (02) TO COMPUTED-A                              NC2324.2
090000     MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.               NC2324.2
090100     PERFORM FAIL                                                 NC2324.2
090200     GO TO SPC-WRITE-F1-3.                                        NC2324.2
090300 SPC-FAIL-F1-3-B.                                                 NC2324.2
090400     MOVE "SEC (02,03)" TO CORRECT-A.                             NC2324.2
090500     IF ENTRY-2 (02, 03) EQUAL TO "SEC (02,03)"                   NC2324.2
090600         MOVE "IDX-2" TO END-IDX                                  NC2324.2
090700         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2324.2
090800         SET IDX-VALU TO IDX-2                                    NC2324.2
090900         MOVE END-STMT TO COMPUTED-A  ELSE                        NC2324.2
091000     MOVE ENTRY-2 (02, 03) TO COMPUTED-A                          NC2324.2
091100     MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.               NC2324.2
091200     PERFORM FAIL                                                 NC2324.2
091300     GO TO SPC-WRITE-F1-3.                                        NC2324.2
091400 SPC-FAIL-F1-3-C.                                                 NC2324.2
091500     MOVE "INDEX SET HIGHER THAN ENTRY" TO RE-MARK                NC2324.2
091600     MOVE SPACES TO CORRECT-A                                     NC2324.2
091700     MOVE "ELEM (02,03,04)" TO COMPUTED-A                         NC2324.2
091800     PERFORM FAIL.                                                NC2324.2
091900 SPC-WRITE-F1-3.                                                  NC2324.2
092000     PERFORM PRINT-DETAIL.                                        NC2324.2
092100 CCVS-EXIT SECTION.                                               NC2324.2
092200 CCVS-999999.                                                     NC2324.2
092300     GO TO CLOSE-FILES.                                           NC2324.2
*END-OF,NC232A                                                                  
*HEADER,COBOL,NC233A                                                            
000100 IDENTIFICATION DIVISION.                                         NC2334.2
000200 PROGRAM-ID.                                                      NC2334.2
000300     NC233A.                                                      NC2334.2
000400****************************************************************  NC2334.2
000500*                                                              *  NC2334.2
000600*    VALIDATION FOR:-                                          *  NC2334.2
000700*                                                              *  NC2334.2
000800*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2334.2
000900*                                                              *  NC2334.2
001000*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC2334.2
001100*                                                              *  NC2334.2
001200****************************************************************  NC2334.2
001300*                                                              *  NC2334.2
001400*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  NC2334.2
001500*                                                              *  NC2334.2
001600*        X-55  - SYSTEM PRINTER NAME.                          *  NC2334.2
001700*        X-82  - SOURCE COMPUTER NAME.                         *  NC2334.2
001800*        X-83  - OBJECT COMPUTER NAME.                         *  NC2334.2
001900*                                                              *  NC2334.2
002000****************************************************************  NC2334.2
002100                                                                  NC2334.2
002200*                                                              *  NC2334.2
002300*    PROGRAM NC233A USES FORMAT 2 OF THE "SEARCH" STATEMENT    *  NC2334.2
002400*    TO ACCESS THRE AND SEVEN-DIMENSIONAL TABLES.              *  NC2334.2
002500*    THE SCOPE TERMINATOR "END-SEARCH" IS ALSO TESTED.         *  NC2334.2
002600*                                                              *  NC2334.2
002700****************************************************************  NC2334.2
002800 ENVIRONMENT DIVISION.                                            NC2334.2
002900 CONFIGURATION SECTION.                                           NC2334.2
003000 SOURCE-COMPUTER.                                                 NC2334.2
003100     XXXXX082.                                                    NC2334.2
003200 OBJECT-COMPUTER.                                                 NC2334.2
003300     XXXXX083.                                                    NC2334.2
003400 INPUT-OUTPUT SECTION.                                            NC2334.2
003500 FILE-CONTROL.                                                    NC2334.2
003600     SELECT PRINT-FILE ASSIGN TO                                  NC2334.2
003700     XXXXX055.                                                    NC2334.2
003800 DATA DIVISION.                                                   NC2334.2
003900 FILE SECTION.                                                    NC2334.2
004000 FD  PRINT-FILE.                                                  NC2334.2
004100 01  PRINT-REC PICTURE X(120).                                    NC2334.2
004200 01  DUMMY-RECORD PICTURE X(120).                                 NC2334.2
004300 WORKING-STORAGE SECTION.                                         NC2334.2
004400 77  SUB-1              PICTURE S99  VALUE ZERO.                  NC2334.2
004500 77  SUB-2              PICTURE 99   VALUE ZERO.                  NC2334.2
004600 77  SUB-3              PICTURE 99   VALUE ZERO.                  NC2334.2
004700 77  CON-7              PICTURE 99  VALUE 07.                     NC2334.2
004800 77  CON-10             PICTURE 99  VALUE 10.                     NC2334.2
004900 77  GRP-HOLD-AREA                PICTURE X(5)  VALUE SPACES.     NC2334.2
005000 77  CON-5              PICTURE 99  VALUE 05.                     NC2334.2
005100 77  SEC-HOLD-AREA                PICTURE X(11)  VALUE SPACES.    NC2334.2
005200 77  CON-6              PICTURE 99  VALUE 06.                     NC2334.2
005300 77  ELEM-HOLD-AREA               PICTURE X(15)  VALUE SPACES.    NC2334.2
005400 77  L1-HOLD                     PIC XX.                          NC2334.2
005500 77  L2-HOLD                     PIC XX.                          NC2334.2
005600 77  L3-HOLD                     PIC XX.                          NC2334.2
005700 77  L4-HOLD                     PIC XX.                          NC2334.2
005800 77  L5-HOLD                     PIC XX.                          NC2334.2
005900 77  L6-HOLD                     PIC XX.                          NC2334.2
006000 77  L7-HOLD                     PIC XX.                          NC2334.2
006100 77  N1                         PIC 99.                           NC2334.2
006200 77  N2                         PIC 99.                           NC2334.2
006300 77  N3                         PIC 99.                           NC2334.2
006400 77  N4                         PIC 99.                           NC2334.2
006500 77  N5                         PIC 99.                           NC2334.2
006600 77  N6                         PIC 99.                           NC2334.2
006700 77  N7                         PIC 99.                           NC2334.2
006800 01  GRP-NAME.                                                    NC2334.2
006900     02  FILLER              PICTURE XXX    VALUE "GRP".          NC2334.2
007000     02  ADD-GRP             PICTURE 99     VALUE 01.             NC2334.2
007100                                                                  NC2334.2
007200 01  SEC-NAME.                                                    NC2334.2
007300     02  FILLER              PICTURE X(5)   VALUE "SEC (".        NC2334.2
007400     02  SEC-GRP             PICTURE 99     VALUE 00.             NC2334.2
007500     02  FILLER              PICTURE X      VALUE ",".            NC2334.2
007600     02  ADD-SEC             PICTURE 99     VALUE 01.             NC2334.2
007700     02  FILLER              PICTURE X      VALUE ")".            NC2334.2
007800                                                                  NC2334.2
007900 01  ELEM-NAME.                                                   NC2334.2
008000     02  FILLER              PICTURE X(6)   VALUE "ELEM (".       NC2334.2
008100     02  ELEM-GRP            PICTURE 99     VALUE 00.             NC2334.2
008200     02  FILLER              PICTURE X      VALUE ",".            NC2334.2
008300     02  ELEM-SEC            PICTURE 99     VALUE 00.             NC2334.2
008400     02  FILLER              PICTURE X      VALUE ",".            NC2334.2
008500     02  ADD-ELEM            PICTURE 99     VALUE 01.             NC2334.2
008600     02  FILLER              PICTURE X      VALUE ")".            NC2334.2
008700                                                                  NC2334.2
008800 01  3-DIMENSION-TBL.                                             NC2334.2
008900     02  GRP-ENTRY OCCURS 10 TIMES ASCENDING KEY IS GRP           NC2334.2
009000         INDEXED BY IDX-1.                                        NC2334.2
009100         03  ENTRY-1.                                             NC2334.2
009200             05  GRP         PICTURE X(5).                        NC2334.2
009300         03  GRP2-ENTRY OCCURS 10 TIMES ASCENDING KEY IS SEC      NC2334.2
009400             INDEXED BY IDX-2.                                    NC2334.2
009500             04  ENTRY-2.                                         NC2334.2
009600                 05  FILLER  PICTURE X(4).                        NC2334.2
009700                 05  SEC     PICTURE X(7).                        NC2334.2
009800             04  GRP3-ENTRY OCCURS 10 TIMES ASCENDING KEY IS ELEM NC2334.2
009900                 INDEXED BY IDX-3.                                NC2334.2
010000                 05  ENTRY-3.                                     NC2334.2
010100                 07  FILLER  PICTURE X(8).                        NC2334.2
010200                 07  ELEM    PICTURE X(7).                        NC2334.2
010300                                                                  NC2334.2
010400                                                                  NC2334.2
010500 01  7-DIMENSION-TBL.                                             NC2334.2
010600   02  GRP-7-1-ENTRY             OCCURS 2                         NC2334.2
010700                                 ASCENDING KEY IS ENTRY-7-1G      NC2334.2
010800                                 INDEXED BY X1.                   NC2334.2
010900     03  ENTRY-7-1G.                                              NC2334.2
011000       04  CHARS-7-1               PIC X.                         NC2334.2
011100       04  ENTRY-7-1               PIC 9.                         NC2334.2
011200     03  GRP-7-2-ENTRY           OCCURS 2                         NC2334.2
011300                                 ASCENDING KEY IS ENTRY-7-2G      NC2334.2
011400                                 INDEXED BY X2.                   NC2334.2
011500       04  ENTRY-7-2G.                                            NC2334.2
011600         05  CHARS-7-2             PIC X.                         NC2334.2
011700         05  ENTRY-7-2             PIC 9.                         NC2334.2
011800       04  GRP-7-3-ENTRY         OCCURS 2                         NC2334.2
011900                                 ASCENDING KEY IS ENTRY-7-3G      NC2334.2
012000                                 INDEXED BY X3.                   NC2334.2
012100         05  ENTRY-7-3G.                                          NC2334.2
012200           06  CHARS-7-3           PIC X.                         NC2334.2
012300           06  ENTRY-7-3           PIC 9.                         NC2334.2
012400         05  GRP-7-4-ENTRY       OCCURS 2                         NC2334.2
012500                                 ASCENDING KEY IS ENTRY-7-4G      NC2334.2
012600                                 INDEXED BY X4.                   NC2334.2
012700           06  ENTRY-7-4G.                                        NC2334.2
012800             07  CHARS-7-4       PIC X.                           NC2334.2
012900             07  ENTRY-7-4       PIC 9.                           NC2334.2
013000           06  GRP-7-5-ENTRY     OCCURS 2                         NC2334.2
013100                                 ASCENDING KEY IS ENTRY-7-5G      NC2334.2
013200                                 INDEXED BY X5.                   NC2334.2
013300             07  ENTRY-7-5G.                                      NC2334.2
013400               08  CHARS-7-5     PIC X.                           NC2334.2
013500               08  ENTRY-7-5     PIC 9.                           NC2334.2
013600             07  GRP-7-6-ENTRY   OCCURS 2                         NC2334.2
013700                                 ASCENDING KEY IS ENTRY-7-6G      NC2334.2
013800                                 INDEXED BY X6.                   NC2334.2
013900               08  ENTRY-7-6G.                                    NC2334.2
014000                 09  CHARS-7-6   PIC X.                           NC2334.2
014100                 09  ENTRY-7-6   PIC 9.                           NC2334.2
014200               08  GRP-7-7-ENTRY OCCURS 2                         NC2334.2
014300                                 ASCENDING KEY IS ENTRY-7-7G      NC2334.2
014400                                 INDEXED BY X7.                   NC2334.2
014500                 09  ENTRY-7-7G.                                  NC2334.2
014600                   10  CHARS-7-7 PIC X.                           NC2334.2
014700                   10  ENTRY-7-7 PIC 9.                           NC2334.2
014800                                                                  NC2334.2
014900 01  NOTE-1.                                                      NC2334.2
015000     02  FILLER                   PICTURE X(74)  VALUE            NC2334.2
015100     "NOTE 1 - CORRECT AND COMPUTED DATA ARE EQUAL BUT THE AT END NC2334.2
015200-    "PATH WAS TAKEN".                                            NC2334.2
015300     02  FILLER                   PICTURE X(46)  VALUE SPACES.    NC2334.2
015400 01  NOTE-2.                                                      NC2334.2
015500     02  FILLER                   PICTURE X(112)  VALUE           NC2334.2
015600     "NOTE 2 - CORRECT AND COMPUTED DATA ARE NOT EQUAL. THE COMPUTNC2334.2
015700-    "ED ENTRY WAS EXTRACTED FROM THE TABLE BY SUBSCRIPTS.".      NC2334.2
015800     02  FILLER                   PICTURE X(8)  VALUE SPACES.     NC2334.2
015900                                                                  NC2334.2
016000 01  END-STMT.                                                    NC2334.2
016100     02  FILLER         PICTURE X(7)  VALUE "AT END ".            NC2334.2
016200     02  END-IDX                  PICTURE X(5)  VALUE SPACES.     NC2334.2
016300     02  FILLER                   PICTURE XXX  VALUE " = ".       NC2334.2
016400     02  IDX-VALU                 PICTURE 99  VALUE 00.           NC2334.2
016500     02  FILLER                   PICTURE XXX  VALUE SPACES.      NC2334.2
016600 01  TEST-RESULTS.                                                NC2334.2
016700     02 FILLER                   PIC X      VALUE SPACE.          NC2334.2
016800     02 FEATURE                  PIC X(20)  VALUE SPACE.          NC2334.2
016900     02 FILLER                   PIC X      VALUE SPACE.          NC2334.2
017000     02 P-OR-F                   PIC X(5)   VALUE SPACE.          NC2334.2
017100     02 FILLER                   PIC X      VALUE SPACE.          NC2334.2
017200     02  PAR-NAME.                                                NC2334.2
017300       03 FILLER                 PIC X(19)  VALUE SPACE.          NC2334.2
017400       03  PARDOT-X              PIC X      VALUE SPACE.          NC2334.2
017500       03 DOTVALUE               PIC 99     VALUE ZERO.           NC2334.2
017600     02 FILLER                   PIC X(8)   VALUE SPACE.          NC2334.2
017700     02 RE-MARK                  PIC X(61).                       NC2334.2
017800 01  TEST-COMPUTED.                                               NC2334.2
017900     02 FILLER                   PIC X(30)  VALUE SPACE.          NC2334.2
018000     02 FILLER                   PIC X(17)  VALUE                 NC2334.2
018100            "       COMPUTED=".                                   NC2334.2
018200     02 COMPUTED-X.                                               NC2334.2
018300     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          NC2334.2
018400     03 COMPUTED-N               REDEFINES COMPUTED-A             NC2334.2
018500                                 PIC -9(9).9(9).                  NC2334.2
018600     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         NC2334.2
018700     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     NC2334.2
018800     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     NC2334.2
018900     03       CM-18V0 REDEFINES COMPUTED-A.                       NC2334.2
019000         04 COMPUTED-18V0                    PIC -9(18).          NC2334.2
019100         04 FILLER                           PIC X.               NC2334.2
019200     03 FILLER PIC X(50) VALUE SPACE.                             NC2334.2
019300 01  TEST-CORRECT.                                                NC2334.2
019400     02 FILLER PIC X(30) VALUE SPACE.                             NC2334.2
019500     02 FILLER PIC X(17) VALUE "       CORRECT =".                NC2334.2
019600     02 CORRECT-X.                                                NC2334.2
019700     03 CORRECT-A                  PIC X(20) VALUE SPACE.         NC2334.2
019800     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      NC2334.2
019900     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         NC2334.2
020000     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     NC2334.2
020100     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     NC2334.2
020200     03      CR-18V0 REDEFINES CORRECT-A.                         NC2334.2
020300         04 CORRECT-18V0                     PIC -9(18).          NC2334.2
020400         04 FILLER                           PIC X.               NC2334.2
020500     03 FILLER PIC X(2) VALUE SPACE.                              NC2334.2
020600     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     NC2334.2
020700 01  CCVS-C-1.                                                    NC2334.2
020800     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PANC2334.2
020900-    "SS  PARAGRAPH-NAME                                          NC2334.2
021000-    "       REMARKS".                                            NC2334.2
021100     02 FILLER                     PIC X(20)    VALUE SPACE.      NC2334.2
021200 01  CCVS-C-2.                                                    NC2334.2
021300     02 FILLER                     PIC X        VALUE SPACE.      NC2334.2
021400     02 FILLER                     PIC X(6)     VALUE "TESTED".   NC2334.2
021500     02 FILLER                     PIC X(15)    VALUE SPACE.      NC2334.2
021600     02 FILLER                     PIC X(4)     VALUE "FAIL".     NC2334.2
021700     02 FILLER                     PIC X(94)    VALUE SPACE.      NC2334.2
021800 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       NC2334.2
021900 01  REC-CT                        PIC 99       VALUE ZERO.       NC2334.2
022000 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       NC2334.2
022100 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       NC2334.2
022200 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       NC2334.2
022300 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       NC2334.2
022400 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       NC2334.2
022500 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       NC2334.2
022600 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      NC2334.2
022700 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       NC2334.2
022800 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     NC2334.2
022900 01  CCVS-H-1.                                                    NC2334.2
023000     02  FILLER                    PIC X(39)    VALUE SPACES.     NC2334.2
023100     02  FILLER                    PIC X(42)    VALUE             NC2334.2
023200     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 NC2334.2
023300     02  FILLER                    PIC X(39)    VALUE SPACES.     NC2334.2
023400 01  CCVS-H-2A.                                                   NC2334.2
023500   02  FILLER                        PIC X(40)  VALUE SPACE.      NC2334.2
023600   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  NC2334.2
023700   02  FILLER                        PIC XXXX   VALUE             NC2334.2
023800     "4.2 ".                                                      NC2334.2
023900   02  FILLER                        PIC X(28)  VALUE             NC2334.2
024000            " COPY - NOT FOR DISTRIBUTION".                       NC2334.2
024100   02  FILLER                        PIC X(41)  VALUE SPACE.      NC2334.2
024200                                                                  NC2334.2
024300 01  CCVS-H-2B.                                                   NC2334.2
024400   02  FILLER                        PIC X(15)  VALUE             NC2334.2
024500            "TEST RESULT OF ".                                    NC2334.2
024600   02  TEST-ID                       PIC X(9).                    NC2334.2
024700   02  FILLER                        PIC X(4)   VALUE             NC2334.2
024800            " IN ".                                               NC2334.2
024900   02  FILLER                        PIC X(12)  VALUE             NC2334.2
025000     " HIGH       ".                                              NC2334.2
025100   02  FILLER                        PIC X(22)  VALUE             NC2334.2
025200            " LEVEL VALIDATION FOR ".                             NC2334.2
025300   02  FILLER                        PIC X(58)  VALUE             NC2334.2
025400     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2334.2
025500 01  CCVS-H-3.                                                    NC2334.2
025600     02  FILLER                      PIC X(34)  VALUE             NC2334.2
025700            " FOR OFFICIAL USE ONLY    ".                         NC2334.2
025800     02  FILLER                      PIC X(58)  VALUE             NC2334.2
025900     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC2334.2
026000     02  FILLER                      PIC X(28)  VALUE             NC2334.2
026100            "  COPYRIGHT   1985 ".                                NC2334.2
026200 01  CCVS-E-1.                                                    NC2334.2
026300     02 FILLER                       PIC X(52)  VALUE SPACE.      NC2334.2
026400     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              NC2334.2
026500     02 ID-AGAIN                     PIC X(9).                    NC2334.2
026600     02 FILLER                       PIC X(45)  VALUE SPACES.     NC2334.2
026700 01  CCVS-E-2.                                                    NC2334.2
026800     02  FILLER                      PIC X(31)  VALUE SPACE.      NC2334.2
026900     02  FILLER                      PIC X(21)  VALUE SPACE.      NC2334.2
027000     02 CCVS-E-2-2.                                               NC2334.2
027100         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      NC2334.2
027200         03 FILLER                   PIC X      VALUE SPACE.      NC2334.2
027300         03 ENDER-DESC               PIC X(44)  VALUE             NC2334.2
027400            "ERRORS ENCOUNTERED".                                 NC2334.2
027500 01  CCVS-E-3.                                                    NC2334.2
027600     02  FILLER                      PIC X(22)  VALUE             NC2334.2
027700            " FOR OFFICIAL USE ONLY".                             NC2334.2
027800     02  FILLER                      PIC X(12)  VALUE SPACE.      NC2334.2
027900     02  FILLER                      PIC X(58)  VALUE             NC2334.2
028000     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2334.2
028100     02  FILLER                      PIC X(13)  VALUE SPACE.      NC2334.2
028200     02 FILLER                       PIC X(15)  VALUE             NC2334.2
028300             " COPYRIGHT 1985".                                   NC2334.2
028400 01  CCVS-E-4.                                                    NC2334.2
028500     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      NC2334.2
028600     02 FILLER                       PIC X(4)   VALUE " OF ".     NC2334.2
028700     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      NC2334.2
028800     02 FILLER                       PIC X(40)  VALUE             NC2334.2
028900      "  TESTS WERE EXECUTED SUCCESSFULLY".                       NC2334.2
029000 01  XXINFO.                                                      NC2334.2
029100     02 FILLER                       PIC X(19)  VALUE             NC2334.2
029200            "*** INFORMATION ***".                                NC2334.2
029300     02 INFO-TEXT.                                                NC2334.2
029400       04 FILLER                     PIC X(8)   VALUE SPACE.      NC2334.2
029500       04 XXCOMPUTED                 PIC X(20).                   NC2334.2
029600       04 FILLER                     PIC X(5)   VALUE SPACE.      NC2334.2
029700       04 XXCORRECT                  PIC X(20).                   NC2334.2
029800     02 INF-ANSI-REFERENCE           PIC X(48).                   NC2334.2
029900 01  HYPHEN-LINE.                                                 NC2334.2
030000     02 FILLER  PIC IS X VALUE IS SPACE.                          NC2334.2
030100     02 FILLER  PIC IS X(65)    VALUE IS "************************NC2334.2
030200-    "*****************************************".                 NC2334.2
030300     02 FILLER  PIC IS X(54)    VALUE IS "************************NC2334.2
030400-    "******************************".                            NC2334.2
030500 01  CCVS-PGM-ID                     PIC X(9)   VALUE             NC2334.2
030600     "NC233A".                                                    NC2334.2
030700 PROCEDURE DIVISION.                                              NC2334.2
030800 CCVS1 SECTION.                                                   NC2334.2
030900 OPEN-FILES.                                                      NC2334.2
031000     OPEN     OUTPUT PRINT-FILE.                                  NC2334.2
031100     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   NC2334.2
031200     MOVE    SPACE TO TEST-RESULTS.                               NC2334.2
031300     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             NC2334.2
031400     GO TO CCVS1-EXIT.                                            NC2334.2
031500 CLOSE-FILES.                                                     NC2334.2
031600     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   NC2334.2
031700 TERMINATE-CCVS.                                                  NC2334.2
031800S    EXIT PROGRAM.                                                NC2334.2
031900STERMINATE-CALL.                                                  NC2334.2
032000     STOP     RUN.                                                NC2334.2
032100 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         NC2334.2
032200 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           NC2334.2
032300 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          NC2334.2
032400 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      NC2334.2
032500     MOVE "****TEST DELETED****" TO RE-MARK.                      NC2334.2
032600 PRINT-DETAIL.                                                    NC2334.2
032700     IF REC-CT NOT EQUAL TO ZERO                                  NC2334.2
032800             MOVE "." TO PARDOT-X                                 NC2334.2
032900             MOVE REC-CT TO DOTVALUE.                             NC2334.2
033000     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      NC2334.2
033100     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               NC2334.2
033200        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 NC2334.2
033300          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 NC2334.2
033400     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              NC2334.2
033500     MOVE SPACE TO CORRECT-X.                                     NC2334.2
033600     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         NC2334.2
033700     MOVE     SPACE TO RE-MARK.                                   NC2334.2
033800 HEAD-ROUTINE.                                                    NC2334.2
033900     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC2334.2
034000     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC2334.2
034100     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC2334.2
034200     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC2334.2
034300 COLUMN-NAMES-ROUTINE.                                            NC2334.2
034400     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2334.2
034500     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2334.2
034600     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        NC2334.2
034700 END-ROUTINE.                                                     NC2334.2
034800     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.NC2334.2
034900 END-RTN-EXIT.                                                    NC2334.2
035000     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2334.2
035100 END-ROUTINE-1.                                                   NC2334.2
035200      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      NC2334.2
035300      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               NC2334.2
035400      ADD PASS-COUNTER TO ERROR-HOLD.                             NC2334.2
035500*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   NC2334.2
035600      MOVE PASS-COUNTER TO CCVS-E-4-1.                            NC2334.2
035700      MOVE ERROR-HOLD TO CCVS-E-4-2.                              NC2334.2
035800      MOVE CCVS-E-4 TO CCVS-E-2-2.                                NC2334.2
035900      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           NC2334.2
036000  END-ROUTINE-12.                                                 NC2334.2
036100      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        NC2334.2
036200     IF       ERROR-COUNTER IS EQUAL TO ZERO                      NC2334.2
036300         MOVE "NO " TO ERROR-TOTAL                                NC2334.2
036400         ELSE                                                     NC2334.2
036500         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       NC2334.2
036600     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           NC2334.2
036700     PERFORM WRITE-LINE.                                          NC2334.2
036800 END-ROUTINE-13.                                                  NC2334.2
036900     IF DELETE-COUNTER IS EQUAL TO ZERO                           NC2334.2
037000         MOVE "NO " TO ERROR-TOTAL  ELSE                          NC2334.2
037100         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      NC2334.2
037200     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   NC2334.2
037300     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2334.2
037400      IF   INSPECT-COUNTER EQUAL TO ZERO                          NC2334.2
037500          MOVE "NO " TO ERROR-TOTAL                               NC2334.2
037600      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   NC2334.2
037700      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            NC2334.2
037800      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          NC2334.2
037900     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2334.2
038000 WRITE-LINE.                                                      NC2334.2
038100     ADD 1 TO RECORD-COUNT.                                       NC2334.2
038200Y    IF RECORD-COUNT GREATER 50                                   NC2334.2
038300Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          NC2334.2
038400Y        MOVE SPACE TO DUMMY-RECORD                               NC2334.2
038500Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  NC2334.2
038600Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             NC2334.2
038700Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     NC2334.2
038800Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          NC2334.2
038900Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          NC2334.2
039000Y        MOVE ZERO TO RECORD-COUNT.                               NC2334.2
039100     PERFORM WRT-LN.                                              NC2334.2
039200 WRT-LN.                                                          NC2334.2
039300     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               NC2334.2
039400     MOVE SPACE TO DUMMY-RECORD.                                  NC2334.2
039500 BLANK-LINE-PRINT.                                                NC2334.2
039600     PERFORM WRT-LN.                                              NC2334.2
039700 FAIL-ROUTINE.                                                    NC2334.2
039800     IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. NC2334.2
039900     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.NC2334.2
040000     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC2334.2
040100     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   NC2334.2
040200     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2334.2
040300     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC2334.2
040400     GO TO  FAIL-ROUTINE-EX.                                      NC2334.2
040500 FAIL-ROUTINE-WRITE.                                              NC2334.2
040600     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         NC2334.2
040700     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 NC2334.2
040800     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. NC2334.2
040900     MOVE   SPACES TO COR-ANSI-REFERENCE.                         NC2334.2
041000 FAIL-ROUTINE-EX. EXIT.                                           NC2334.2
041100 BAIL-OUT.                                                        NC2334.2
041200     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   NC2334.2
041300     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           NC2334.2
041400 BAIL-OUT-WRITE.                                                  NC2334.2
041500     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  NC2334.2
041600     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC2334.2
041700     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2334.2
041800     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC2334.2
041900 BAIL-OUT-EX. EXIT.                                               NC2334.2
042000 CCVS1-EXIT.                                                      NC2334.2
042100     EXIT.                                                        NC2334.2
042200 SECT-NC233A-001 SECTION.                                         NC2334.2
042300 TH-05-001.                                                       NC2334.2
042400 BUILD-LEVEL-1.                                                   NC2334.2
042500     ADD 1 TO SUB-1.                                              NC2334.2
042600     IF SUB-1 = 11 GO TO CHECK-ENTRIES.                           NC2334.2
042700     MOVE GRP-NAME TO ENTRY-1 (SUB-1).                            NC2334.2
042800     ADD 1 TO ADD-GRP.                                            NC2334.2
042900 BUILD-LEVEL-2.                                                   NC2334.2
043000     ADD 1 TO SUB-2.                                              NC2334.2
043100     IF SUB-2 = 11                                                NC2334.2
043200         MOVE ZERO TO SUB-2                                       NC2334.2
043300         MOVE 01 TO ADD-SEC                                       NC2334.2
043400         GO TO BUILD-LEVEL-1.                                     NC2334.2
043500     MOVE SUB-1 TO SEC-GRP.                                       NC2334.2
043600     MOVE SEC-NAME TO ENTRY-2 (SUB-1, SUB-2).                     NC2334.2
043700     ADD 1 TO ADD-SEC.                                            NC2334.2
043800 BUILD-LEVEL-3.                                                   NC2334.2
043900     ADD 1 TO SUB-3.                                              NC2334.2
044000     IF SUB-3 = 11                                                NC2334.2
044100         MOVE ZERO TO SUB-3                                       NC2334.2
044200              MOVE 01 TO ADD-ELEM                                 NC2334.2
044300              GO TO BUILD-LEVEL-2.                                NC2334.2
044400     MOVE SUB-1 TO ELEM-GRP.                                      NC2334.2
044500     MOVE SUB-2 TO ELEM-SEC.                                      NC2334.2
044600     MOVE ELEM-NAME TO ENTRY-3 (SUB-1, SUB-2, SUB-3).             NC2334.2
044700     ADD 1 TO ADD-ELEM.                                           NC2334.2
044800     GO TO BUILD-LEVEL-3.                                         NC2334.2
044900                                                                  NC2334.2
045000 CHECK-ENTRIES.                                                   NC2334.2
045100     MOVE "SEARCH ALL-FIRST LEV" TO FEATURE.                      NC2334.2
045200     MOVE "CHECK-ENTRIES       " TO PAR-NAME.                     NC2334.2
045300     MOVE "GRP02" TO GRP-HOLD-AREA.                               NC2334.2
045400     MOVE 02 TO SUB-2.                                            NC2334.2
045500     SET IDX-1 TO 1.                                              NC2334.2
045600     SEARCH ALL GRP-ENTRY AT END                                  NC2334.2
045700         PERFORM GRP-FAIL-PARGRAPH                                NC2334.2
045800         GO TO LEVEL-1-TEST-2                                     NC2334.2
045900         WHEN GRP (IDX-1) = GRP-HOLD-AREA NEXT SENTENCE.          NC2334.2
046000                                                                  NC2334.2
046100     PERFORM PASS-TH.                                             NC2334.2
046200     GO TO LEVEL-1-TEST-2.                                        NC2334.2
046300                                                                  NC2334.2
046400 GRP-FAIL-PARGRAPH.                                               NC2334.2
046500     MOVE GRP-HOLD-AREA TO CORRECT-A.                             NC2334.2
046600     IF ENTRY-1 (SUB-2) EQUAL TO GRP-HOLD-AREA                    NC2334.2
046700         MOVE "IDX-1" TO END-IDX                                  NC2334.2
046800         SET IDX-VALU TO IDX-1                                    NC2334.2
046900         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2334.2
047000         MOVE END-STMT TO COMPUTED-A  ELSE                        NC2334.2
047100     MOVE ENTRY-1 (SUB-2) TO COMPUTED-A                           NC2334.2
047200     MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.               NC2334.2
047300                                                                  NC2334.2
047400     PERFORM FAIL-TH.                                             NC2334.2
047500 LEVEL-1-TEST-2.                                                  NC2334.2
047600     MOVE "LEVEL-1-TEST-2      " TO PAR-NAME.                     NC2334.2
047700     MOVE "GRP01" TO GRP-HOLD-AREA.                               NC2334.2
047800     MOVE 01 TO SUB-2.                                            NC2334.2
047900     SET IDX-1 TO 1.                                              NC2334.2
048000     SEARCH ALL GRP-ENTRY AT END                                  NC2334.2
048100         PERFORM GRP-FAIL-PARGRAPH                                NC2334.2
048200         GO TO LEVEL-1-TEST-3                                     NC2334.2
048300         WHEN GRP (IDX-1) = GRP-HOLD-AREA NEXT SENTENCE.          NC2334.2
048400                                                                  NC2334.2
048500     PERFORM PASS-TH.                                             NC2334.2
048600 LEVEL-1-TEST-3.                                                  NC2334.2
048700     MOVE "LEVEL-1-TEST-3      " TO PAR-NAME.                     NC2334.2
048800     MOVE "GRP10" TO GRP-HOLD-AREA.                               NC2334.2
048900     MOVE 10 TO SUB-2.                                            NC2334.2
049000     SET IDX-1 TO 1.                                              NC2334.2
049100     SEARCH ALL GRP-ENTRY AT END                                  NC2334.2
049200         PERFORM GRP-FAIL-PARGRAPH                                NC2334.2
049300         GO TO LEVEL-1-TEST-4                                     NC2334.2
049400         WHEN GRP (IDX-1) = GRP-HOLD-AREA NEXT SENTENCE.          NC2334.2
049500                                                                  NC2334.2
049600     PERFORM PASS-TH.                                             NC2334.2
049700 LEVEL-1-TEST-4.                                                  NC2334.2
049800     MOVE "LEVEL-1-TEST-4      " TO PAR-NAME.                     NC2334.2
049900     MOVE "GRP05" TO GRP-HOLD-AREA.                               NC2334.2
050000     MOVE 05 TO SUB-2.                                            NC2334.2
050100     SET IDX-1 TO 05.                                             NC2334.2
050200     SEARCH ALL GRP-ENTRY                                         NC2334.2
050300         WHEN GRP (IDX-1) = GRP-HOLD-AREA GO TO PASS-TH-TEST-4.   NC2334.2
050400     PERFORM GRP-FAIL-PARGRAPH.                                   NC2334.2
050500     GO TO LEVEL-2-TEST-1.                                        NC2334.2
050600 PASS-TH-TEST-4.                                                  NC2334.2
050700                                                                  NC2334.2
050800     PERFORM PASS-TH.                                             NC2334.2
050900                                                                  NC2334.2
051000 LEVEL-2-TEST-1.                                                  NC2334.2
051100     MOVE "SEARCH ALL-SEC LEVEL" TO FEATURE.                      NC2334.2
051200     MOVE "LEVEL-2-TEST-1      " TO PAR-NAME.                     NC2334.2
051300     MOVE "SEC (01,01)" TO SEC-HOLD-AREA.                         NC2334.2
051400     MOVE 1 TO SUB-1   SUB-2.                                     NC2334.2
051500     SET IDX-1 IDX-2 TO 1.                                        NC2334.2
051600     SEARCH ALL GRP2-ENTRY AT END                                 NC2334.2
051700         PERFORM SEC-FAIL-PARGRAF                                 NC2334.2
051800         GO TO LEVEL-2-TEST-2                                     NC2334.2
051900         WHEN SEC (IDX-1, IDX-2) = "(01,01)" NEXT SENTENCE.       NC2334.2
052000                                                                  NC2334.2
052100     PERFORM PASS-TH.                                             NC2334.2
052200                                                                  NC2334.2
052300 LEVEL-2-TEST-2.                                                  NC2334.2
052400     MOVE "LEVEL-2-TEST-2      " TO PAR-NAME.                     NC2334.2
052500     MOVE "SEC (05,10)" TO SEC-HOLD-AREA.                         NC2334.2
052600     MOVE 05 TO SUB-1.                                            NC2334.2
052700     MOVE 10 TO SUB-2.                                            NC2334.2
052800     SET IDX-1 TO 5.                                              NC2334.2
052900     SET IDX-2 TO 1.                                              NC2334.2
053000     SEARCH ALL GRP2-ENTRY AT END                                 NC2334.2
053100         PERFORM SEC-FAIL-PARGRAF                                 NC2334.2
053200         GO TO LEVEL-2-TEST-3                                     NC2334.2
053300         WHEN SEC (IDX-1, IDX-2) = "(05,10)" NEXT SENTENCE.       NC2334.2
053400                                                                  NC2334.2
053500     PERFORM PASS-TH.                                             NC2334.2
053600                                                                  NC2334.2
053700 LEVEL-2-TEST-3.                                                  NC2334.2
053800     MOVE "LEVEL-2-TEST-3      " TO PAR-NAME.                     NC2334.2
053900     MOVE "SEC (10,10)" TO SEC-HOLD-AREA.                         NC2334.2
054000     SET IDX-1 TO 10.                                             NC2334.2
054100     SET IDX-2 TO 1.                                              NC2334.2
054200     MOVE 10 TO SUB-1  SUB-2.                                     NC2334.2
054300     SEARCH ALL GRP2-ENTRY AT END                                 NC2334.2
054400         PERFORM SEC-FAIL-PARGRAF                                 NC2334.2
054500         GO TO LEVEL-2-TEST-4                                     NC2334.2
054600         WHEN SEC (IDX-1, IDX-2) = "(10,10)" NEXT SENTENCE.       NC2334.2
054700                                                                  NC2334.2
054800     PERFORM PASS-TH.                                             NC2334.2
054900 LEVEL-2-TEST-4.                                                  NC2334.2
055000     MOVE "LEVEL-2-TEST-4      " TO PAR-NAME.                     NC2334.2
055100     MOVE "SEC (08,02)" TO SEC-HOLD-AREA.                         NC2334.2
055200     MOVE 08 TO SUB-1.                                            NC2334.2
055300     MOVE 02 TO SUB-2.                                            NC2334.2
055400     SET IDX-1 TO 08.                                             NC2334.2
055500     SET IDX-2 TO 01.                                             NC2334.2
055600     SEARCH ALL GRP2-ENTRY                                        NC2334.2
055700         WHEN SEC (IDX-1, IDX-2) = "(08,02)" GO TO PASS-TH-2-4.   NC2334.2
055800     PERFORM SEC-FAIL-PARGRAF.                                    NC2334.2
055900     GO TO LEVEL-3-TEST-1.                                        NC2334.2
056000 PASS-TH-2-4.                                                     NC2334.2
056100                                                                  NC2334.2
056200     PERFORM PASS-TH.                                             NC2334.2
056300     GO TO LEVEL-3-TEST-1.                                        NC2334.2
056400                                                                  NC2334.2
056500 SEC-FAIL-PARGRAF.                                                NC2334.2
056600     MOVE SEC-HOLD-AREA TO CORRECT-A.                             NC2334.2
056700     IF ENTRY-2 (SUB-1, SUB-2) EQUAL TO SEC-HOLD-AREA             NC2334.2
056800         MOVE "IDX-2" TO END-IDX                                  NC2334.2
056900         SET IDX-VALU TO IDX-2                                    NC2334.2
057000         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2334.2
057100         MOVE END-STMT TO COMPUTED-A  ELSE                        NC2334.2
057200     MOVE ENTRY-2 (SUB-1, SUB-2) TO COMPUTED-A                    NC2334.2
057300     MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.               NC2334.2
057400                                                                  NC2334.2
057500     PERFORM FAIL-TH.                                             NC2334.2
057600                                                                  NC2334.2
057700 LEVEL-3-TEST-1.                                                  NC2334.2
057800     MOVE "LEVEL-3-TEST-1      " TO PAR-NAME.                     NC2334.2
057900     MOVE "SEARCH ALL THIRD LEV" TO FEATURE.                      NC2334.2
058000     MOVE 1 TO SUB-1  SUB-2  SUB-3.                               NC2334.2
058100     MOVE "ELEM (01,01,01)" TO ELEM-HOLD-AREA.                    NC2334.2
058200     SET IDX-1 IDX-2 IDX-3 TO 1.                                  NC2334.2
058300     SEARCH ALL GRP3-ENTRY                                        NC2334.2
058400         WHEN ELEM (IDX-1, IDX-2, IDX-3) = ",01,01)"              NC2334.2
058500             GO TO PASS-TH-3-1.                                   NC2334.2
058600     PERFORM ELEM-FAIL-PARA.                                      NC2334.2
058700     GO TO LEVEL-3-TEST-2.                                        NC2334.2
058800 PASS-TH-3-1.                                                     NC2334.2
058900                                                                  NC2334.2
059000     PERFORM PASS-TH.                                             NC2334.2
059100                                                                  NC2334.2
059200 LEVEL-3-TEST-2.                                                  NC2334.2
059300     MOVE "LEVEL-3-TEST-2      " TO PAR-NAME.                     NC2334.2
059400     MOVE 05 TO SUB-1.                                            NC2334.2
059500     MOVE 06 TO SUB-2.                                            NC2334.2
059600     MOVE 07 TO SUB-3.                                            NC2334.2
059700     SET IDX-1 TO 05.                                             NC2334.2
059800     SET IDX-2 TO 06.                                             NC2334.2
059900     SET IDX-3 TO 1.                                              NC2334.2
060000     MOVE "ELEM (05,06,07)" TO ELEM-HOLD-AREA.                    NC2334.2
060100     SEARCH ALL GRP3-ENTRY AT END                                 NC2334.2
060200         PERFORM ELEM-FAIL-PARA                                   NC2334.2
060300         GO TO LEVEL-3-TEST-3                                     NC2334.2
060400         WHEN ELEM (IDX-1, IDX-2, IDX-3) = ",06,07)"              NC2334.2
060500             NEXT SENTENCE.                                       NC2334.2
060600                                                                  NC2334.2
060700     PERFORM PASS-TH.                                             NC2334.2
060800                                                                  NC2334.2
060900 LEVEL-3-TEST-3.                                                  NC2334.2
061000     MOVE "LEVEL-3-TEST-3      " TO PAR-NAME.                     NC2334.2
061100     MOVE 10 TO SUB-1 SUB-2 SUB-3.                                NC2334.2
061200     SET IDX-1 IDX-2 TO 10.                                       NC2334.2
061300     SET IDX-3 TO 1.                                              NC2334.2
061400     MOVE "ELEM (10,10,10)" TO ELEM-HOLD-AREA.                    NC2334.2
061500     SEARCH ALL GRP3-ENTRY AT END                                 NC2334.2
061600         PERFORM ELEM-FAIL-PARA                                   NC2334.2
061700         GO TO LEVEL-3-TEST-4                                     NC2334.2
061800         WHEN ELEM (IDX-1, IDX-2, IDX-3) = ",10,10)"              NC2334.2
061900             NEXT SENTENCE.                                       NC2334.2
062000                                                                  NC2334.2
062100     PERFORM PASS-TH.                                             NC2334.2
062200 LEVEL-3-TEST-4.                                                  NC2334.2
062300     MOVE "LEVEL-3-TEST-4      " TO PAR-NAME.                     NC2334.2
062400     MOVE "ELEM (07,06,05)" TO ELEM-HOLD-AREA.                    NC2334.2
062500     MOVE 07 TO SUB-1.                                            NC2334.2
062600     MOVE 06 TO SUB-2.                                            NC2334.2
062700     MOVE 05 TO SUB-3.                                            NC2334.2
062800     SET IDX-1 TO 07.                                             NC2334.2
062900     SET IDX-2 TO 06.                                             NC2334.2
063000     SET IDX-3 TO 03.                                             NC2334.2
063100     SEARCH ALL GRP3-ENTRY AT END                                 NC2334.2
063200         PERFORM ELEM-FAIL-PARA                                   NC2334.2
063300         GO TO MULT-SEARCH-TEST-1                                 NC2334.2
063400         WHEN ELEM (IDX-1, IDX-2, IDX-3) = ",06,05)"              NC2334.2
063500             NEXT SENTENCE.                                       NC2334.2
063600                                                                  NC2334.2
063700     PERFORM PASS-TH.                                             NC2334.2
063800     GO TO MULT-SEARCH-TEST-1.                                    NC2334.2
063900 ELEM-FAIL-PARA.                                                  NC2334.2
064000     MOVE ELEM-HOLD-AREA TO CORRECT-A.                            NC2334.2
064100     IF ENTRY-3 (SUB-1, SUB-2, SUB-3) EQUAL TO ELEM-HOLD-AREA     NC2334.2
064200         MOVE "IDX-3" TO END-IDX                                  NC2334.2
064300         SET IDX-VALU TO IDX-3                                    NC2334.2
064400         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2334.2
064500         MOVE END-STMT TO COMPUTED-A  ELSE                        NC2334.2
064600     MOVE ENTRY-3 (SUB-1, SUB-2, SUB-3) TO COMPUTED-A             NC2334.2
064700     MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.               NC2334.2
064800                                                                  NC2334.2
064900     PERFORM FAIL-TH.                                             NC2334.2
065000                                                                  NC2334.2
065100 MULT-SEARCH-TEST-1.                                              NC2334.2
065200     MOVE "MULT-SEARCH-TEST-1  " TO PAR-NAME.                     NC2334.2
065300     MOVE "MULTIPLE SEARCH STMT" TO FEATURE.                      NC2334.2
065400     MOVE "GRP08" TO GRP-HOLD-AREA.                               NC2334.2
065500     MOVE "SEC (08,07)" TO SEC-HOLD-AREA.                         NC2334.2
065600     SET IDX-1 IDX-2 TO 1.                                        NC2334.2
065700     SEARCH ALL GRP-ENTRY AT END GO TO MULT-SEARCH-FAIL1          NC2334.2
065800         WHEN GRP (IDX-1) = "GRP08" NEXT SENTENCE.                NC2334.2
065900     SEARCH ALL GRP2-ENTRY AT END GO TO MULT-SEARCH-FAIL          NC2334.2
066000         WHEN SEC (IDX-1, IDX-2) = "(08,07)" NEXT SENTENCE.       NC2334.2
066100                                                                  NC2334.2
066200     PERFORM PASS-TH.                                             NC2334.2
066300     GO TO MULT-SEARCH-7-INIT-3.                                  NC2334.2
066400 MULT-SEARCH-FAIL1.                                               NC2334.2
066500     MOVE GRP-HOLD-AREA TO CORRECT-A.                             NC2334.2
066600     IF ENTRY-1 (08) EQUAL TO GRP-HOLD-AREA                       NC2334.2
066700         MOVE "IDX-1" TO END-IDX                                  NC2334.2
066800         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2334.2
066900         SET IDX-VALU TO IDX-1                                    NC2334.2
067000         MOVE END-STMT TO COMPUTED-A  ELSE                        NC2334.2
067100     MOVE ENTRY-1 (08) TO COMPUTED-A                              NC2334.2
067200     MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.               NC2334.2
067300                                                                  NC2334.2
067400     PERFORM FAIL-TH.                                             NC2334.2
067500     GO TO MULT-SEARCH-7-INIT-3.                                  NC2334.2
067600 MULT-SEARCH-FAIL.                                                NC2334.2
067700     MOVE SEC-HOLD-AREA TO CORRECT-A.                             NC2334.2
067800     IF ENTRY-2 (08, 07) EQUAL TO SEC-HOLD-AREA                   NC2334.2
067900         MOVE "IDX-2" TO END-IDX                                  NC2334.2
068000         SET IDX-VALU TO IDX-2                                    NC2334.2
068100         MOVE "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK            NC2334.2
068200         MOVE END-STMT TO COMPUTED-A  ELSE                        NC2334.2
068300     MOVE ENTRY-2 (08, 07) TO COMPUTED-A                          NC2334.2
068400     MOVE "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.               NC2334.2
068500                                                                  NC2334.2
068600     PERFORM FAIL-TH.                                             NC2334.2
068700                                                                  NC2334.2
068800 MULT-SEARCH-7-INIT-3.                                            NC2334.2
068900     MOVE   "MULT-SEARCH-7-TEST-3" TO PAR-NAME.                   NC2334.2
069000     MOVE   "VI-122 6.21"          TO ANSI-REFERENCE.             NC2334.2
069100     MOVE   "A2" TO L1-HOLD.                                      NC2334.2
069200     MOVE   "B1" TO L2-HOLD.                                      NC2334.2
069300     MOVE   "C2" TO L3-HOLD.                                      NC2334.2
069400     MOVE   "D1" TO L4-HOLD.                                      NC2334.2
069500     MOVE   "E2" TO L5-HOLD.                                      NC2334.2
069600     MOVE   "F1" TO L6-HOLD.                                      NC2334.2
069700     MOVE   "G2" TO L7-HOLD.                                      NC2334.2
069800     SET     X1 X2 X3 X4 X5 X6 X7 TO 1.                           NC2334.2
069900     PERFORM MULT-SEARCH-7-INIT-3-A                               NC2334.2
070000             VARYING N1 FROM 1 BY 1 UNTIL N1 > 2                  NC2334.2
070100               AFTER N2 FROM 1 BY 1 UNTIL N2 > 2                  NC2334.2
070200               AFTER N3 FROM 1 BY 1 UNTIL N3 > 2                  NC2334.2
070300               AFTER N4 FROM 1 BY 1 UNTIL N4 > 2                  NC2334.2
070400               AFTER N5 FROM 1 BY 1 UNTIL N5 > 2                  NC2334.2
070500               AFTER N6 FROM 1 BY 1 UNTIL N6 > 2                  NC2334.2
070600               AFTER N7 FROM 1 BY 1 UNTIL N7 > 2.                 NC2334.2
070700     GO TO   MULT-SEARCH-7-TEST-3.                                NC2334.2
070800 MULT-SEARCH-7-INIT-3-A.                                          NC2334.2
070900                                                                  NC2334.2
071000     MOVE    N1 TO ENTRY-7-1 (N1).                                NC2334.2
071100     MOVE   "A" TO CHARS-7-1 (N1).                                NC2334.2
071200     MOVE    N2 TO ENTRY-7-2 (N1 N2).                             NC2334.2
071300     MOVE   "B" TO CHARS-7-2 (N1 N2).                             NC2334.2
071400     MOVE    N3 TO ENTRY-7-3 (N1 N2 N3).                          NC2334.2
071500     MOVE   "C" TO CHARS-7-3 (N1 N2 N3).                          NC2334.2
071600     MOVE    N4 TO ENTRY-7-4 (N1 N2 N3 N4).                       NC2334.2
071700     MOVE   "D" TO CHARS-7-4 (N1 N2 N3 N4).                       NC2334.2
071800     MOVE    N5 TO ENTRY-7-5 (N1 N2 N3 N4 N5).                    NC2334.2
071900     MOVE   "E" TO CHARS-7-5 (N1 N2 N3 N4 N5).                    NC2334.2
072000     MOVE    N6 TO ENTRY-7-6 (N1 N2 N3 N4 N5 N6).                 NC2334.2
072100     MOVE   "F" TO CHARS-7-6 (N1 N2 N3 N4 N5 N6).                 NC2334.2
072200     MOVE    N7 TO ENTRY-7-7 (N1 N2 N3 N4 N5 N6 N7).              NC2334.2
072300     MOVE   "G" TO CHARS-7-7 (N1 N2 N3 N4 N5 N6 N7).              NC2334.2
072400 MULT-SEARCH-7-DELETE-3.                                          NC2334.2
072500     PERFORM DE-LETE.                                             NC2334.2
072600     PERFORM PRINT-DETAIL.                                        NC2334.2
072700     GO TO   END-SEARCH-TEST.                                     NC2334.2
072800 MULT-SEARCH-7-TEST-3.                                            NC2334.2
072900     SEARCH  ALL GRP-7-1-ENTRY                                    NC2334.2
073000             AT END  GO TO MULT-SEARCH-7-FAIL-1                   NC2334.2
073100             WHEN    ENTRY-7-1G (X1) =  L1-HOLD                   NC2334.2
073200                     NEXT SENTENCE.                               NC2334.2
073300     SET     X1 TO 1.                                             NC2334.2
073400     SEARCH  ALL GRP-7-2-ENTRY                                    NC2334.2
073500             AT END  GO TO MULT-SEARCH-7-FAIL-2                   NC2334.2
073600             WHEN    ENTRY-7-2G (X1 X2) = L2-HOLD                 NC2334.2
073700                     NEXT SENTENCE.                               NC2334.2
073800     SET     X1 TO 2.                                             NC2334.2
073900     SET     X2 TO 1.                                             NC2334.2
074000     SEARCH  ALL GRP-7-3-ENTRY                                    NC2334.2
074100             AT END  GO TO MULT-SEARCH-7-FAIL-3                   NC2334.2
074200             WHEN    ENTRY-7-3G (X1 X2 X3) = L3-HOLD              NC2334.2
074300                     NEXT SENTENCE.                               NC2334.2
074400     SET     X1 TO 1.                                             NC2334.2
074500     SET     X2, X3 TO 1.                                         NC2334.2
074600     SEARCH  ALL GRP-7-4-ENTRY                                    NC2334.2
074700             AT END  GO TO MULT-SEARCH-7-FAIL-4                   NC2334.2
074800             WHEN    ENTRY-7-4G (X1 X2 X3 X4) =  L4-HOLD          NC2334.2
074900                     NEXT SENTENCE.                               NC2334.2
075000     SET     X1 TO 2.                                             NC2334.2
075100     SET     X2, X3, X4 TO 1.                                     NC2334.2
075200     SEARCH  ALL GRP-7-5-ENTRY                                    NC2334.2
075300             AT END  GO TO MULT-SEARCH-7-FAIL-5                   NC2334.2
075400             WHEN    ENTRY-7-5G (X1 X2 X3 X4 X5) = L5-HOLD        NC2334.2
075500                     NEXT SENTENCE.                               NC2334.2
075600     SET     X1 TO 1.                                             NC2334.2
075700     SET     X2, X3, X4, X5 TO 1.                                 NC2334.2
075800     SEARCH  ALL GRP-7-6-ENTRY                                    NC2334.2
075900             AT END  GO TO MULT-SEARCH-7-FAIL-6                   NC2334.2
076000             WHEN ENTRY-7-6G (X1 X2 X3 X4 X5 X6) = L6-HOLD        NC2334.2
076100                     NEXT SENTENCE.                               NC2334.2
076200     SET     X1 TO 2.                                             NC2334.2
076300     SET     X2, X3, X4, X6 TO 1.                                 NC2334.2
076400     SEARCH  ALL GRP-7-7-ENTRY                                    NC2334.2
076500             AT END  GO TO MULT-SEARCH-7-FAIL-7                   NC2334.2
076600             WHEN ENTRY-7-7G (X1 X2 X3 X4 X5 X6 X7) = L7-HOLD     NC2334.2
076700                     NEXT SENTENCE.                               NC2334.2
076800                                                                  NC2334.2
076900     PERFORM PASS-TH.                                             NC2334.2
077000     GO TO   END-SEARCH-TEST.                                     NC2334.2
077100                                                                  NC2334.2
077200 MULT-SEARCH-7-FAIL-1.                                            NC2334.2
077300     MOVE    L1-HOLD TO CORRECT-A.                                NC2334.2
077400     IF      ENTRY-7-1 (2) = L1-HOLD                              NC2334.2
077500             MOVE   "IX-1" TO END-IDX                             NC2334.2
077600             SET     IDX-VALU TO X1                               NC2334.2
077700             MOVE   "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK      NC2334.2
077800             MOVE    END-STMT TO COMPUTED-A                       NC2334.2
077900     ELSE                                                         NC2334.2
078000             MOVE    ENTRY-7-1 (2) TO COMPUTED-A                  NC2334.2
078100             MOVE   "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.     NC2334.2
078200                                                                  NC2334.2
078300     PERFORM FAIL-TH.                                             NC2334.2
078400     GO TO   END-SEARCH-TEST.                                     NC2334.2
078500                                                                  NC2334.2
078600 MULT-SEARCH-7-FAIL-2.                                            NC2334.2
078700     MOVE    L2-HOLD TO CORRECT-A.                                NC2334.2
078800     IF      ENTRY-7-2 (2 1) = L1-HOLD                            NC2334.2
078900             MOVE   "X2" TO END-IDX                               NC2334.2
079000             SET     IDX-VALU TO X2                               NC2334.2
079100             MOVE   "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK      NC2334.2
079200             MOVE    END-STMT TO COMPUTED-A                       NC2334.2
079300     ELSE                                                         NC2334.2
079400             MOVE    ENTRY-7-2 (2 1) TO COMPUTED-A                NC2334.2
079500             MOVE   "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.     NC2334.2
079600                                                                  NC2334.2
079700     PERFORM FAIL-TH.                                             NC2334.2
079800     GO TO   END-SEARCH-TEST.                                     NC2334.2
079900                                                                  NC2334.2
080000 MULT-SEARCH-7-FAIL-3.                                            NC2334.2
080100     MOVE    L3-HOLD TO CORRECT-A.                                NC2334.2
080200     IF      ENTRY-7-3 (2 1 2) = L3-HOLD                          NC2334.2
080300             MOVE   "X3" TO END-IDX                               NC2334.2
080400             SET     IDX-VALU TO X3                               NC2334.2
080500             MOVE   "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK      NC2334.2
080600             MOVE    END-STMT TO COMPUTED-A                       NC2334.2
080700     ELSE                                                         NC2334.2
080800             MOVE    ENTRY-7-3 (2 1 2) TO COMPUTED-A              NC2334.2
080900             MOVE   "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.     NC2334.2
081000                                                                  NC2334.2
081100     PERFORM FAIL-TH.                                             NC2334.2
081200     GO TO   END-SEARCH-TEST.                                     NC2334.2
081300                                                                  NC2334.2
081400 MULT-SEARCH-7-FAIL-4.                                            NC2334.2
081500     MOVE    L4-HOLD TO CORRECT-A.                                NC2334.2
081600     IF      ENTRY-7-4 (2 1 2 1) = L4-HOLD                        NC2334.2
081700             MOVE   "X4" TO END-IDX                               NC2334.2
081800             SET     IDX-VALU TO X4                               NC2334.2
081900             MOVE   "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK      NC2334.2
082000             MOVE    END-STMT TO COMPUTED-A                       NC2334.2
082100     ELSE                                                         NC2334.2
082200             MOVE    ENTRY-7-4 (2 1 2 1) TO COMPUTED-A            NC2334.2
082300             MOVE   "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.     NC2334.2
082400                                                                  NC2334.2
082500     PERFORM FAIL-TH.                                             NC2334.2
082600     GO TO   END-SEARCH-TEST.                                     NC2334.2
082700                                                                  NC2334.2
082800 MULT-SEARCH-7-FAIL-5.                                            NC2334.2
082900     MOVE    L5-HOLD TO CORRECT-A.                                NC2334.2
083000     IF      ENTRY-7-5 (2 1 2 1 2) = L5-HOLD                      NC2334.2
083100             MOVE   "X5" TO END-IDX                               NC2334.2
083200             SET     IDX-VALU TO X5                               NC2334.2
083300             MOVE   "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK      NC2334.2
083400             MOVE    END-STMT TO COMPUTED-A                       NC2334.2
083500     ELSE                                                         NC2334.2
083600             MOVE    ENTRY-7-5 (2 1 2 1 2) TO COMPUTED-A          NC2334.2
083700             MOVE   "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.     NC2334.2
083800                                                                  NC2334.2
083900     PERFORM FAIL-TH.                                             NC2334.2
084000     GO TO   END-SEARCH-TEST.                                     NC2334.2
084100                                                                  NC2334.2
084200 MULT-SEARCH-7-FAIL-6.                                            NC2334.2
084300     MOVE    L6-HOLD TO CORRECT-A.                                NC2334.2
084400     IF      ENTRY-7-6 (2 1 2 1 2 1) = L6-HOLD                    NC2334.2
084500             MOVE   "X6" TO END-IDX                               NC2334.2
084600             SET     IDX-VALU TO X6                               NC2334.2
084700             MOVE   "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK      NC2334.2
084800             MOVE    END-STMT TO COMPUTED-A                       NC2334.2
084900     ELSE                                                         NC2334.2
085000             MOVE    ENTRY-7-6 (2 1 2 1 2 1) TO COMPUTED-A        NC2334.2
085100             MOVE   "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.     NC2334.2
085200                                                                  NC2334.2
085300     PERFORM FAIL-TH.                                             NC2334.2
085400     GO TO   END-SEARCH-TEST.                                     NC2334.2
085500                                                                  NC2334.2
085600 MULT-SEARCH-7-FAIL-7.                                            NC2334.2
085700     MOVE    L7-HOLD TO CORRECT-A.                                NC2334.2
085800     IF      ENTRY-7-7 (2 1 2 1 2 1 2) = L6-HOLD                  NC2334.2
085900             MOVE   "X7" TO END-IDX                               NC2334.2
086000             SET     IDX-VALU TO X7                               NC2334.2
086100             MOVE   "SEE NOTE 1 FOR DIAGNOSTIC  " TO RE-MARK      NC2334.2
086200             MOVE    END-STMT TO COMPUTED-A                       NC2334.2
086300     ELSE                                                         NC2334.2
086400             MOVE    ENTRY-7-7 (2 1 2 1 2 1 2) TO COMPUTED-A      NC2334.2
086500             MOVE   "SEE NOTE 2 FOR DIAGNOSTIC  " TO RE-MARK.     NC2334.2
086600                                                                  NC2334.2
086700     PERFORM FAIL-TH.                                             NC2334.2
086800                                                                  NC2334.2
086900     GO TO END-SEARCH-TEST.                                       NC2334.2
087000                                                                  NC2334.2
087100 PASS-TH.                                                         NC2334.2
087200     PERFORM PASS.                                                NC2334.2
087300     PERFORM PRINT-DETAIL.                                        NC2334.2
087400 FAIL-TH.                                                         NC2334.2
087500     PERFORM FAIL.                                                NC2334.2
087600     PERFORM  PRINT-DETAIL.                                       NC2334.2
087700 END-SEARCH-TEST.                                                 NC2334.2
087800     EXIT.                                                        NC2334.2
087900 CCVS-EXIT SECTION.                                               NC2334.2
088000 CCVS-999999.                                                     NC2334.2
088100     GO TO CLOSE-FILES.                                           NC2334.2
*END-OF,NC233A                                                                  
*HEADER,COBOL,NC234A                                                            
000100 IDENTIFICATION DIVISION.                                         NC2344.2
000200 PROGRAM-ID.                                                      NC2344.2
000300     NC234A.                                                      NC2344.2
000400                                                                  NC2344.2
000500****************************************************************  NC2344.2
000600*                                                              *  NC2344.2
000700*    VALIDATION FOR:-                                          *  NC2344.2
000800*                                                              *  NC2344.2
000900*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2344.2
001000*                                                              *  NC2344.2
001100*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC2344.2
001200*                                                              *  NC2344.2
001300****************************************************************  NC2344.2
001400*                                                              *  NC2344.2
001500*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  NC2344.2
001600*                                                              *  NC2344.2
001700*        X-55  - SYSTEM PRINTER NAME.                          *  NC2344.2
001800*        X-82  - SOURCE COMPUTER NAME.                         *  NC2344.2
001900*        X-83  - OBJECT COMPUTER NAME.                         *  NC2344.2
002000*                                                              *  NC2344.2
002100****************************************************************  NC2344.2
002200*                                                                 NC2344.2
002300*    PROGRAM NC234A TESTS THE ACCESSING OF A "REDEFINED" THREE *  NC2344.2
002400*    -DIMENSIONAL TABLE USING FORMAT 1 OF THE "SEARCH"         *  NC2344.2
002500*    STATEMENT.  THE "VARYING" AND "AT END" PHRASES ARE USED.  *  NC2344.2
002600*                                                              *  NC2344.2
002700****************************************************************  NC2344.2
002800 ENVIRONMENT DIVISION.                                            NC2344.2
002900 CONFIGURATION SECTION.                                           NC2344.2
003000 SOURCE-COMPUTER.                                                 NC2344.2
003100     XXXXX082.                                                    NC2344.2
003200 OBJECT-COMPUTER.                                                 NC2344.2
003300     XXXXX083.                                                    NC2344.2
003400 INPUT-OUTPUT SECTION.                                            NC2344.2
003500 FILE-CONTROL.                                                    NC2344.2
003600     SELECT PRINT-FILE ASSIGN TO                                  NC2344.2
003700     XXXXX055.                                                    NC2344.2
003800 DATA DIVISION.                                                   NC2344.2
003900 FILE SECTION.                                                    NC2344.2
004000 FD  PRINT-FILE.                                                  NC2344.2
004100 01  PRINT-REC PICTURE X(120).                                    NC2344.2
004200 01  DUMMY-RECORD PICTURE X(120).                                 NC2344.2
004300 WORKING-STORAGE SECTION.                                         NC2344.2
004400 77  SUB-1              PICTURE S99  VALUE ZERO.                  NC2344.2
004500 77  SUB-2              PICTURE 99  VALUE ZERO.                   NC2344.2
004600 77  SUB-3              PICTURE 99  VALUE ZERO.                   NC2344.2
004700 77  CON-5              PICTURE 99  VALUE 05.                     NC2344.2
004800 77  CON-6              PICTURE 99  VALUE 06.                     NC2344.2
004900 77  CON-7              PICTURE 99  VALUE 07.                     NC2344.2
005000 77  CON-10             PICTURE 99  VALUE 10.                     NC2344.2
005100 77  GRP-HOLD-AREA      PICTURE X(5) VALUE SPACES.                NC2344.2
005200 77  SEC-HOLD-AREA      PICTURE X(11) VALUE SPACES.               NC2344.2
005300 77  ELEM-HOLD-AREA     PICTURE X(15) VALUE SPACES.               NC2344.2
005400 01  GRP-NAME.                                                    NC2344.2
005500     02  FILLER              PICTURE XXX    VALUE "GRP".          NC2344.2
005600     02  ADD-GRP             PICTURE 99     VALUE 01.             NC2344.2
005700                                                                  NC2344.2
005800 01  SEC-NAME.                                                    NC2344.2
005900     02  FILLER              PICTURE X(5)   VALUE "SEC (".        NC2344.2
006000     02  SEC-GRP             PICTURE 99     VALUE 00.             NC2344.2
006100     02  FILLER              PICTURE X      VALUE ",".            NC2344.2
006200     02  ADD-SEC             PICTURE 99     VALUE 01.             NC2344.2
006300     02  FILLER              PICTURE X      VALUE ")".            NC2344.2
006400                                                                  NC2344.2
006500 01  ELEM-NAME.                                                   NC2344.2
006600     02  FILLER              PICTURE X(6)   VALUE "ELEM (".       NC2344.2
006700     02  ELEM-GRP            PICTURE 99     VALUE 00.             NC2344.2
006800     02  FILLER              PICTURE X      VALUE ",".            NC2344.2
006900     02  ELEM-SEC            PICTURE 99     VALUE 00.             NC2344.2
007000     02  FILLER              PICTURE X      VALUE ",".            NC2344.2
007100     02  ADD-ELEM            PICTURE 99     VALUE 01.             NC2344.2
007200     02  FILLER              PICTURE X      VALUE ")".            NC2344.2
007300                                                                  NC2344.2
007400 01  3-DIMENSION-TBL.                                             NC2344.2
007500     02  GRP-ENTRY OCCURS 10 TIMES INDEXED BY IDX-1.              NC2344.2
007600         03  ENTRY-1         PICTURE X(5).                        NC2344.2
007700         03  GRP2-ENTRY OCCURS 10 TIMES INDEXED BY IDX-2.         NC2344.2
007800             04  ENTRY-2     PICTURE X(11).                       NC2344.2
007900             04  3-ENTRY OCCURS 10 TIMES INDEXED BY IDX-3.        NC2344.2
008000                 05  ENTRY-3 PICTURE X(15).                       NC2344.2
008100 01  3-DEM-TBL REDEFINES 3-DIMENSION-TBL.                         NC2344.2
008200     02  GRP-ENTRY-1 OCCURS 10 TIMES INDEXED BY IDX-1-1.          NC2344.2
008300         03  ENTRY-1-1           PIC X(5).                        NC2344.2
008400         03  GRP2-ENTRY-1 OCCURS 10 TIMES INDEXED BY IDX-2-1.     NC2344.2
008500             04  ENTRY-2-1       PIC X(11).                       NC2344.2
008600             04  GRP3-ENTRY-1 OCCURS 10 TIMES INDEXED BY IDX-3-1. NC2344.2
008700                 05  ENTRY-3-1   PIC X(15).                       NC2344.2
008800                                                                  NC2344.2
008900 01  END-STMT.                                                    NC2344.2
009000     02  FILLER              PICTURE X(7)  VALUE "AT END ".       NC2344.2
009100     02  END-IDX             PICTURE X(7)  VALUE SPACES.          NC2344.2
009200     02  FILLER              PICTURE XXX   VALUE " = ".           NC2344.2
009300     02  IDX-VALU            PICTURE 99    VALUE 00.              NC2344.2
009400     02  FILLER              PICTURE XXX   VALUE SPACES.          NC2344.2
009500 01  NOTE-1.                                                      NC2344.2
009600     02  FILLER                   PICTURE X(74)  VALUE            NC2344.2
009700     "NOTE 1 - CORRECT AND COMPUTED DATA ARE EQUAL BUT THE AT END NC2344.2
009800-    "PATH WAS TAKEN".                                            NC2344.2
009900     02  FILLER                   PICTURE X(46)  VALUE SPACES.    NC2344.2
010000 01  NOTE-2.                                                      NC2344.2
010100     02  FILLER                   PICTURE X(112)  VALUE           NC2344.2
010200     "NOTE 2 - CORRECT AND COMPUTED DATA ARE NOT EQUAL. THE COMPUTNC2344.2
010300-    "ED ENTRY WAS EXTRACTED FROM THE TABLE BY SUBSCRIPTS.".      NC2344.2
010400     02  FILLER                   PICTURE X(8)   VALUE SPACES.    NC2344.2
010500 01  TEST-RESULTS.                                                NC2344.2
010600     02 FILLER                   PIC X      VALUE SPACE.          NC2344.2
010700     02 FEATURE                  PIC X(20)  VALUE SPACE.          NC2344.2
010800     02 FILLER                   PIC X      VALUE SPACE.          NC2344.2
010900     02 P-OR-F                   PIC X(5)   VALUE SPACE.          NC2344.2
011000     02 FILLER                   PIC X      VALUE SPACE.          NC2344.2
011100     02  PAR-NAME.                                                NC2344.2
011200       03 FILLER                 PIC X(19)  VALUE SPACE.          NC2344.2
011300       03  PARDOT-X              PIC X      VALUE SPACE.          NC2344.2
011400       03 DOTVALUE               PIC 99     VALUE ZERO.           NC2344.2
011500     02 FILLER                   PIC X(8)   VALUE SPACE.          NC2344.2
011600     02 RE-MARK                  PIC X(61).                       NC2344.2
011700 01  TEST-COMPUTED.                                               NC2344.2
011800     02 FILLER                   PIC X(30)  VALUE SPACE.          NC2344.2
011900     02 FILLER                   PIC X(17)  VALUE                 NC2344.2
012000            "       COMPUTED=".                                   NC2344.2
012100     02 COMPUTED-X.                                               NC2344.2
012200     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          NC2344.2
012300     03 COMPUTED-N               REDEFINES COMPUTED-A             NC2344.2
012400                                 PIC -9(9).9(9).                  NC2344.2
012500     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         NC2344.2
012600     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     NC2344.2
012700     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     NC2344.2
012800     03       CM-18V0 REDEFINES COMPUTED-A.                       NC2344.2
012900         04 COMPUTED-18V0                    PIC -9(18).          NC2344.2
013000         04 FILLER                           PIC X.               NC2344.2
013100     03 FILLER PIC X(50) VALUE SPACE.                             NC2344.2
013200 01  TEST-CORRECT.                                                NC2344.2
013300     02 FILLER PIC X(30) VALUE SPACE.                             NC2344.2
013400     02 FILLER PIC X(17) VALUE "       CORRECT =".                NC2344.2
013500     02 CORRECT-X.                                                NC2344.2
013600     03 CORRECT-A                  PIC X(20) VALUE SPACE.         NC2344.2
013700     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      NC2344.2
013800     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         NC2344.2
013900     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     NC2344.2
014000     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     NC2344.2
014100     03      CR-18V0 REDEFINES CORRECT-A.                         NC2344.2
014200         04 CORRECT-18V0                     PIC -9(18).          NC2344.2
014300         04 FILLER                           PIC X.               NC2344.2
014400     03 FILLER PIC X(2) VALUE SPACE.                              NC2344.2
014500     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     NC2344.2
014600 01  CCVS-C-1.                                                    NC2344.2
014700     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PANC2344.2
014800-    "SS  PARAGRAPH-NAME                                          NC2344.2
014900-    "       REMARKS".                                            NC2344.2
015000     02 FILLER                     PIC X(20)    VALUE SPACE.      NC2344.2
015100 01  CCVS-C-2.                                                    NC2344.2
015200     02 FILLER                     PIC X        VALUE SPACE.      NC2344.2
015300     02 FILLER                     PIC X(6)     VALUE "TESTED".   NC2344.2
015400     02 FILLER                     PIC X(15)    VALUE SPACE.      NC2344.2
015500     02 FILLER                     PIC X(4)     VALUE "FAIL".     NC2344.2
015600     02 FILLER                     PIC X(94)    VALUE SPACE.      NC2344.2
015700 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       NC2344.2
015800 01  REC-CT                        PIC 99       VALUE ZERO.       NC2344.2
015900 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       NC2344.2
016000 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       NC2344.2
016100 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       NC2344.2
016200 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       NC2344.2
016300 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       NC2344.2
016400 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       NC2344.2
016500 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      NC2344.2
016600 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       NC2344.2
016700 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     NC2344.2
016800 01  CCVS-H-1.                                                    NC2344.2
016900     02  FILLER                    PIC X(39)    VALUE SPACES.     NC2344.2
017000     02  FILLER                    PIC X(42)    VALUE             NC2344.2
017100     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 NC2344.2
017200     02  FILLER                    PIC X(39)    VALUE SPACES.     NC2344.2
017300 01  CCVS-H-2A.                                                   NC2344.2
017400   02  FILLER                        PIC X(40)  VALUE SPACE.      NC2344.2
017500   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  NC2344.2
017600   02  FILLER                        PIC XXXX   VALUE             NC2344.2
017700     "4.2 ".                                                      NC2344.2
017800   02  FILLER                        PIC X(28)  VALUE             NC2344.2
017900            " COPY - NOT FOR DISTRIBUTION".                       NC2344.2
018000   02  FILLER                        PIC X(41)  VALUE SPACE.      NC2344.2
018100                                                                  NC2344.2
018200 01  CCVS-H-2B.                                                   NC2344.2
018300   02  FILLER                        PIC X(15)  VALUE             NC2344.2
018400            "TEST RESULT OF ".                                    NC2344.2
018500   02  TEST-ID                       PIC X(9).                    NC2344.2
018600   02  FILLER                        PIC X(4)   VALUE             NC2344.2
018700            " IN ".                                               NC2344.2
018800   02  FILLER                        PIC X(12)  VALUE             NC2344.2
018900     " HIGH       ".                                              NC2344.2
019000   02  FILLER                        PIC X(22)  VALUE             NC2344.2
019100            " LEVEL VALIDATION FOR ".                             NC2344.2
019200   02  FILLER                        PIC X(58)  VALUE             NC2344.2
019300     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2344.2
019400 01  CCVS-H-3.                                                    NC2344.2
019500     02  FILLER                      PIC X(34)  VALUE             NC2344.2
019600            " FOR OFFICIAL USE ONLY    ".                         NC2344.2
019700     02  FILLER                      PIC X(58)  VALUE             NC2344.2
019800     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC2344.2
019900     02  FILLER                      PIC X(28)  VALUE             NC2344.2
020000            "  COPYRIGHT   1985 ".                                NC2344.2
020100 01  CCVS-E-1.                                                    NC2344.2
020200     02 FILLER                       PIC X(52)  VALUE SPACE.      NC2344.2
020300     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              NC2344.2
020400     02 ID-AGAIN                     PIC X(9).                    NC2344.2
020500     02 FILLER                       PIC X(45)  VALUE SPACES.     NC2344.2
020600 01  CCVS-E-2.                                                    NC2344.2
020700     02  FILLER                      PIC X(31)  VALUE SPACE.      NC2344.2
020800     02  FILLER                      PIC X(21)  VALUE SPACE.      NC2344.2
020900     02 CCVS-E-2-2.                                               NC2344.2
021000         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      NC2344.2
021100         03 FILLER                   PIC X      VALUE SPACE.      NC2344.2
021200         03 ENDER-DESC               PIC X(44)  VALUE             NC2344.2
021300            "ERRORS ENCOUNTERED".                                 NC2344.2
021400 01  CCVS-E-3.                                                    NC2344.2
021500     02  FILLER                      PIC X(22)  VALUE             NC2344.2
021600            " FOR OFFICIAL USE ONLY".                             NC2344.2
021700     02  FILLER                      PIC X(12)  VALUE SPACE.      NC2344.2
021800     02  FILLER                      PIC X(58)  VALUE             NC2344.2
021900     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2344.2
022000     02  FILLER                      PIC X(13)  VALUE SPACE.      NC2344.2
022100     02 FILLER                       PIC X(15)  VALUE             NC2344.2
022200             " COPYRIGHT 1985".                                   NC2344.2
022300 01  CCVS-E-4.                                                    NC2344.2
022400     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      NC2344.2
022500     02 FILLER                       PIC X(4)   VALUE " OF ".     NC2344.2
022600     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      NC2344.2
022700     02 FILLER                       PIC X(40)  VALUE             NC2344.2
022800      "  TESTS WERE EXECUTED SUCCESSFULLY".                       NC2344.2
022900 01  XXINFO.                                                      NC2344.2
023000     02 FILLER                       PIC X(19)  VALUE             NC2344.2
023100            "*** INFORMATION ***".                                NC2344.2
023200     02 INFO-TEXT.                                                NC2344.2
023300       04 FILLER                     PIC X(8)   VALUE SPACE.      NC2344.2
023400       04 XXCOMPUTED                 PIC X(20).                   NC2344.2
023500       04 FILLER                     PIC X(5)   VALUE SPACE.      NC2344.2
023600       04 XXCORRECT                  PIC X(20).                   NC2344.2
023700     02 INF-ANSI-REFERENCE           PIC X(48).                   NC2344.2
023800 01  HYPHEN-LINE.                                                 NC2344.2
023900     02 FILLER  PIC IS X VALUE IS SPACE.                          NC2344.2
024000     02 FILLER  PIC IS X(65)    VALUE IS "************************NC2344.2
024100-    "*****************************************".                 NC2344.2
024200     02 FILLER  PIC IS X(54)    VALUE IS "************************NC2344.2
024300-    "******************************".                            NC2344.2
024400 01  CCVS-PGM-ID                     PIC X(9)   VALUE             NC2344.2
024500     "NC234A".                                                    NC2344.2
024600 PROCEDURE DIVISION.                                              NC2344.2
024700 CCVS1 SECTION.                                                   NC2344.2
024800 OPEN-FILES.                                                      NC2344.2
024900     OPEN     OUTPUT PRINT-FILE.                                  NC2344.2
025000     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   NC2344.2
025100     MOVE    SPACE TO TEST-RESULTS.                               NC2344.2
025200     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             NC2344.2
025300     GO TO CCVS1-EXIT.                                            NC2344.2
025400 CLOSE-FILES.                                                     NC2344.2
025500     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   NC2344.2
025600 TERMINATE-CCVS.                                                  NC2344.2
025700S    EXIT PROGRAM.                                                NC2344.2
025800STERMINATE-CALL.                                                  NC2344.2
025900     STOP     RUN.                                                NC2344.2
026000 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         NC2344.2
026100 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           NC2344.2
026200 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          NC2344.2
026300 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      NC2344.2
026400     MOVE "****TEST DELETED****" TO RE-MARK.                      NC2344.2
026500 PRINT-DETAIL.                                                    NC2344.2
026600     IF REC-CT NOT EQUAL TO ZERO                                  NC2344.2
026700             MOVE "." TO PARDOT-X                                 NC2344.2
026800             MOVE REC-CT TO DOTVALUE.                             NC2344.2
026900     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      NC2344.2
027000     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               NC2344.2
027100        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 NC2344.2
027200          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 NC2344.2
027300     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              NC2344.2
027400     MOVE SPACE TO CORRECT-X.                                     NC2344.2
027500     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         NC2344.2
027600     MOVE     SPACE TO RE-MARK.                                   NC2344.2
027700 HEAD-ROUTINE.                                                    NC2344.2
027800     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC2344.2
027900     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC2344.2
028000     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC2344.2
028100     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC2344.2
028200 COLUMN-NAMES-ROUTINE.                                            NC2344.2
028300     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2344.2
028400     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2344.2
028500     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        NC2344.2
028600 END-ROUTINE.                                                     NC2344.2
028700     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.NC2344.2
028800 END-RTN-EXIT.                                                    NC2344.2
028900     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2344.2
029000 END-ROUTINE-1.                                                   NC2344.2
029100      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      NC2344.2
029200      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               NC2344.2
029300      ADD PASS-COUNTER TO ERROR-HOLD.                             NC2344.2
029400*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   NC2344.2
029500      MOVE PASS-COUNTER TO CCVS-E-4-1.                            NC2344.2
029600      MOVE ERROR-HOLD TO CCVS-E-4-2.                              NC2344.2
029700      MOVE CCVS-E-4 TO CCVS-E-2-2.                                NC2344.2
029800      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           NC2344.2
029900  END-ROUTINE-12.                                                 NC2344.2
030000      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        NC2344.2
030100     IF       ERROR-COUNTER IS EQUAL TO ZERO                      NC2344.2
030200         MOVE "NO " TO ERROR-TOTAL                                NC2344.2
030300         ELSE                                                     NC2344.2
030400         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       NC2344.2
030500     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           NC2344.2
030600     PERFORM WRITE-LINE.                                          NC2344.2
030700 END-ROUTINE-13.                                                  NC2344.2
030800     IF DELETE-COUNTER IS EQUAL TO ZERO                           NC2344.2
030900         MOVE "NO " TO ERROR-TOTAL  ELSE                          NC2344.2
031000         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      NC2344.2
031100     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   NC2344.2
031200     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2344.2
031300      IF   INSPECT-COUNTER EQUAL TO ZERO                          NC2344.2
031400          MOVE "NO " TO ERROR-TOTAL                               NC2344.2
031500      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   NC2344.2
031600      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            NC2344.2
031700      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          NC2344.2
031800     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2344.2
031900 WRITE-LINE.                                                      NC2344.2
032000     ADD 1 TO RECORD-COUNT.                                       NC2344.2
032100Y    IF RECORD-COUNT GREATER 50                                   NC2344.2
032200Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          NC2344.2
032300Y        MOVE SPACE TO DUMMY-RECORD                               NC2344.2
032400Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  NC2344.2
032500Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             NC2344.2
032600Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     NC2344.2
032700Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          NC2344.2
032800Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          NC2344.2
032900Y        MOVE ZERO TO RECORD-COUNT.                               NC2344.2
033000     PERFORM WRT-LN.                                              NC2344.2
033100 WRT-LN.                                                          NC2344.2
033200     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               NC2344.2
033300     MOVE SPACE TO DUMMY-RECORD.                                  NC2344.2
033400 BLANK-LINE-PRINT.                                                NC2344.2
033500     PERFORM WRT-LN.                                              NC2344.2
033600 FAIL-ROUTINE.                                                    NC2344.2
033700     IF     COMPUTED-X NOT EQUAL TO SPACE                         NC2344.2
033800            GO TO    FAIL-ROUTINE-WRITE.                          NC2344.2
033900     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.NC2344.2
034000     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC2344.2
034100     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   NC2344.2
034200     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2344.2
034300     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC2344.2
034400     GO TO  FAIL-ROUTINE-EX.                                      NC2344.2
034500 FAIL-ROUTINE-WRITE.                                              NC2344.2
034600     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         NC2344.2
034700     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 NC2344.2
034800     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. NC2344.2
034900     MOVE   SPACES TO COR-ANSI-REFERENCE.                         NC2344.2
035000 FAIL-ROUTINE-EX. EXIT.                                           NC2344.2
035100 BAIL-OUT.                                                        NC2344.2
035200     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   NC2344.2
035300     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           NC2344.2
035400 BAIL-OUT-WRITE.                                                  NC2344.2
035500     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  NC2344.2
035600     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC2344.2
035700     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2344.2
035800     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC2344.2
035900 BAIL-OUT-EX. EXIT.                                               NC2344.2
036000 CCVS1-EXIT.                                                      NC2344.2
036100     EXIT.                                                        NC2344.2
036200 SECT-NC234A-001 SECTION.                                         NC2344.2
036300 TH-07-001.                                                       NC2344.2
036400 INITIALISE-TABLE.                                                NC2344.2
036500     PERFORM BUILD-TABLE VARYING SUB-1 FROM 1 BY 1                NC2344.2
036600         UNTIL SUB-1 EQUAL TO 11                                  NC2344.2
036700         AFTER SUB-2 FROM 1 BY 1 UNTIL SUB-2 EQUAL TO 11          NC2344.2
036800         AFTER SUB-3 FROM 1 BY 1 UNTIL SUB-3 EQUAL TO 11          NC2344.2
036900     GO TO CHECK-ENTRIES.                                         NC2344.2
037000                                                                  NC2344.2
037100 BUILD-TABLE.                                                     NC2344.2
037200     SET IDX-1 TO SUB-1.                                          NC2344.2
037300     SET IDX-2 TO SUB-2.                                          NC2344.2
037400     SET IDX-3 TO SUB-3.                                          NC2344.2
037500     SET ADD-GRP, SEC-GRP, ELEM-GRP TO IDX-1.                     NC2344.2
037600     MOVE GRP-NAME TO ENTRY-1 (IDX-1).                            NC2344.2
037700     SET ADD-SEC, ELEM-SEC TO IDX-2.                              NC2344.2
037800     MOVE SEC-NAME TO ENTRY-2 (IDX-1, IDX-2).                     NC2344.2
037900     SET ADD-ELEM TO IDX-3.                                       NC2344.2
038000     MOVE ELEM-NAME TO ENTRY-3 (IDX-1, IDX-2, IDX-3).             NC2344.2
038100*                                                                 NC2344.2
038200 CHECK-ENTRIES.                                                   NC2344.2
038300     MOVE "SEARCH VARYING LEV 1" TO FEATURE.                      NC2344.2
038400     MOVE "CHECK-ENTRIES" TO PAR-NAME.                            NC2344.2
038500     MOVE "GRP02" TO GRP-HOLD-AREA.                               NC2344.2
038600     MOVE 02 TO SUB-2.                                            NC2344.2
038700     SET IDX-1 TO 1.                                              NC2344.2
038800     SEARCH GRP-ENTRY VARYING IDX-1                               NC2344.2
038900         AT END                                                   NC2344.2
039000                GO TO CHECK-FAIL                                  NC2344.2
039100         WHEN ENTRY-1 (IDX-1) = GRP-HOLD-AREA                     NC2344.2
039200                PERFORM PASS                                      NC2344.2
039300                GO TO CHECK-WRITE.                                NC2344.2
039400 CHECK-DELETE.                                                    NC2344.2
039500     PERFORM DE-LETE.                                             NC2344.2
039600     GO TO CHECK-WRITE.                                           NC2344.2
039700 CHECK-FAIL.                                                      NC2344.2
039800     MOVE GRP-HOLD-AREA TO CORRECT-A.                             NC2344.2
039900     IF ENTRY-1 (SUB-2) EQUAL TO GRP-HOLD-AREA                    NC2344.2
040000         MOVE "IDX-1" TO END-IDX                                  NC2344.2
040100         SET IDX-VALU TO IDX-1                                    NC2344.2
040200         MOVE "SEE NOTE 1 FOR DIAGNOSTIC " TO RE-MARK             NC2344.2
040300         MOVE END-STMT TO COMPUTED-A ELSE                         NC2344.2
040400     MOVE ENTRY-1 (SUB-2) TO COMPUTED-A                           NC2344.2
040500     MOVE "SEE NOTE 2 FOR DIAGNOSTIC " TO RE-MARK.                NC2344.2
040600     PERFORM FAIL.                                                NC2344.2
040700 CHECK-WRITE.                                                     NC2344.2
040800     PERFORM PRINT-DETAIL.                                        NC2344.2
040900*                                                                 NC2344.2
041000 TH1-INIT-F1-2.                                                   NC2344.2
041100     MOVE "TH1-TEST-F1-2" TO PAR-NAME.                            NC2344.2
041200     MOVE "VI-122 6.21" TO ANSI-REFERENCE.                        NC2344.2
041300     MOVE "SEARCH VARYING LEV 1" TO FEATURE.                      NC2344.2
041400     MOVE "GRP01" TO GRP-HOLD-AREA.                               NC2344.2
041500     MOVE 01 TO SUB-2.                                            NC2344.2
041600     SET IDX-1-1 TO 1.                                            NC2344.2
041700 TH1-TEST-F1-2.                                                   NC2344.2
041800     SEARCH GRP-ENTRY-1 VARYING IDX-1                             NC2344.2
041900         AT END GO TO TH1-FAIL-F1-2                               NC2344.2
042000         WHEN ENTRY-1-1 (IDX-1-1) = GRP-HOLD-AREA NEXT SENTENCE.  NC2344.2
042100     PERFORM PASS                                                 NC2344.2
042200     GO TO TH1-WRITE-F1-2.                                        NC2344.2
042300 TH1-DELETE-F1-2.                                                 NC2344.2
042400     PERFORM DE-LETE.                                             NC2344.2
042500     GO TO TH1-WRITE-F1-2.                                        NC2344.2
042600 TH1-FAIL-F1-2.                                                   NC2344.2
042700     PERFORM CHECK-FAIL.                                          NC2344.2
042800 TH1-WRITE-F1-2.                                                  NC2344.2
042900     PERFORM PRINT-DETAIL.                                        NC2344.2
043000*                                                                 NC2344.2
043100 TH1-INIT-F1-3.                                                   NC2344.2
043200     MOVE "TH1-TEST-F1-3" TO PAR-NAME.                            NC2344.2
043300     MOVE "VI-122 6.21" TO ANSI-REFERENCE.                        NC2344.2
043400     MOVE "SEARCH VARYING LEV 1" TO FEATURE.                      NC2344.2
043500     MOVE "GRP10" TO GRP-HOLD-AREA.                               NC2344.2
043600     MOVE 10 TO SUB-2.                                            NC2344.2
043700     SET IDX-1-1 TO 1.                                            NC2344.2
043800 TH1-TEST-F1-3.                                                   NC2344.2
043900     SEARCH GRP-ENTRY-1 VARYING IDX-1-1                           NC2344.2
044000         AT END GO TO TH1-FAIL-F1-3                               NC2344.2
044100         WHEN ENTRY-1-1 (IDX-1-1) = GRP-HOLD-AREA NEXT SENTENCE.  NC2344.2
044200     MOVE "SEARCH VARYING LEV 2" TO FEATURE.                      NC2344.2
044300     PERFORM PASS                                                 NC2344.2
044400     GO TO TH1-WRITE-F1-3.                                        NC2344.2
044500 TH1-DELETE-F1-3.                                                 NC2344.2
044600     PERFORM DE-LETE.                                             NC2344.2
044700     GO TO TH1-WRITE-F1-3.                                        NC2344.2
044800 TH1-FAIL-F1-3.                                                   NC2344.2
044900     PERFORM CHECK-FAIL.                                          NC2344.2
045000 TH1-WRITE-F1-3.                                                  NC2344.2
045100     PERFORM PRINT-DETAIL.                                        NC2344.2
045200*                                                                 NC2344.2
045300 TH1-INIT-F1-4.                                                   NC2344.2
045400     MOVE "TH1-TEST-F1-4" TO PAR-NAME.                            NC2344.2
045500     MOVE "VI-122 6.21" TO ANSI-REFERENCE.                        NC2344.2
045600     MOVE "SEARCH VARYING LEV 1" TO FEATURE.                      NC2344.2
045700     MOVE "GRP05" TO GRP-HOLD-AREA.                               NC2344.2
045800     MOVE 05 TO SUB-2.                                            NC2344.2
045900     SET IDX-1-1 TO 05.                                           NC2344.2
046000 TH1-TEST-F1-4.                                                   NC2344.2
046100     SEARCH GRP-ENTRY-1 VARYING IDX-1-1                           NC2344.2
046200         WHEN ENTRY-1-1 (IDX-1-1) = GRP-HOLD-AREA                 NC2344.2
046300             PERFORM PASS                                         NC2344.2
046400             GO TO TH1-WRITE-F1-4.                                NC2344.2
046500     GO TO TH1-FAIL-F1-4.                                         NC2344.2
046600 TH1-DELETE-F1-4.                                                 NC2344.2
046700     PERFORM DE-LETE.                                             NC2344.2
046800     GO TO TH1-WRITE-F1-4.                                        NC2344.2
046900 TH1-FAIL-F1-4.                                                   NC2344.2
047000     PERFORM CHECK-FAIL.                                          NC2344.2
047100 TH1-WRITE-F1-4.                                                  NC2344.2
047200     PERFORM PRINT-DETAIL.                                        NC2344.2
047300*                                                                 NC2344.2
047400 TH2-INIT-F1-1.                                                   NC2344.2
047500     MOVE "SEARCH VARYING LEV 2" TO FEATURE.                      NC2344.2
047600     MOVE "VI-122 6.21" TO ANSI-REFERENCE.                        NC2344.2
047700     MOVE "TH2-TEST-F1-1" TO PAR-NAME.                            NC2344.2
047800     MOVE "SEC (01,01)" TO SEC-HOLD-AREA.                         NC2344.2
047900     MOVE  1 TO SUB-1  SUB-2.                                     NC2344.2
048000     SET IDX-1-1 IDX-2-1 TO 1.                                    NC2344.2
048100 TH2-TEST-F1-1.                                                   NC2344.2
048200     SEARCH GRP2-ENTRY-1 VARYING IDX-2-1 AT END                   NC2344.2
048300         GO TO TH2-FAIL-F1-1                                      NC2344.2
048400         WHEN ENTRY-2-1 (IDX-1-1, IDX-2-1) = SEC-HOLD-AREA        NC2344.2
048500             NEXT SENTENCE.                                       NC2344.2
048600     PERFORM PASS                                                 NC2344.2
048700     GO TO TH2-WRITE-F1-1.                                        NC2344.2
048800 TH2-DELETE-F1-1.                                                 NC2344.2
048900     PERFORM DE-LETE.                                             NC2344.2
049000     GO TO TH2-WRITE-F1-1.                                        NC2344.2
049100 TH2-FAIL-F1-1.                                                   NC2344.2
049200     PERFORM CHECK-FAIL2.                                         NC2344.2
049300 TH2-WRITE-F1-1.                                                  NC2344.2
049400     PERFORM PRINT-DETAIL.                                        NC2344.2
049500                                                                  NC2344.2
049600*                                                                 NC2344.2
049700 TH2-INIT-F1-2.                                                   NC2344.2
049800     MOVE "TH2-TEST-F1-2" TO PAR-NAME.                            NC2344.2
049900     MOVE "VI-122 6.21" TO ANSI-REFERENCE.                        NC2344.2
050000     MOVE "SEARCH VARYING LEV 2" TO FEATURE.                      NC2344.2
050100     MOVE "SEC (05,10)" TO SEC-HOLD-AREA.                         NC2344.2
050200     MOVE 05 TO SUB-1.                                            NC2344.2
050300     MOVE 10 TO SUB-2.                                            NC2344.2
050400     SET IDX-1-1 TO 5.                                            NC2344.2
050500     SET IDX-2-1 TO 1.                                            NC2344.2
050600 TH2-TEST-F1-2.                                                   NC2344.2
050700     SEARCH GRP2-ENTRY-1 VARYING IDX-2-1 AT END                   NC2344.2
050800         GO TO TH2-FAIL-F1-2                                      NC2344.2
050900         WHEN ENTRY-2-1 (IDX-1-1, IDX-2-1) = SEC-HOLD-AREA        NC2344.2
051000             NEXT SENTENCE.                                       NC2344.2
051100     PERFORM PASS                                                 NC2344.2
051200     GO TO TH2-WRITE-F1-2.                                        NC2344.2
051300 TH2-DELETE-F1-2.                                                 NC2344.2
051400     PERFORM DE-LETE.                                             NC2344.2
051500     GO TO TH2-WRITE-F1-2.                                        NC2344.2
051600 TH2-FAIL-F1-2.                                                   NC2344.2
051700     PERFORM CHECK-FAIL2.                                         NC2344.2
051800 TH2-WRITE-F1-2.                                                  NC2344.2
051900     PERFORM PRINT-DETAIL.                                        NC2344.2
052000*                                                                 NC2344.2
052100 TH2-INIT-F1-3.                                                   NC2344.2
052200     MOVE "TH2-TEST-F1-3" TO PAR-NAME.                            NC2344.2
052300     MOVE "VI-122 6.21" TO ANSI-REFERENCE.                        NC2344.2
052400     MOVE "SEARCH VARYING LEV 2" TO FEATURE.                      NC2344.2
052500     MOVE "SEC (10,10)" TO SEC-HOLD-AREA.                         NC2344.2
052600     SET IDX-1-1 TO 10.                                           NC2344.2
052700     SET IDX-2-1 TO 1.                                            NC2344.2
052800     MOVE 10 TO SUB-1  SUB-2.                                     NC2344.2
052900 TH2-TEST-F1-3.                                                   NC2344.2
053000     SEARCH GRP2-ENTRY-1 VARYING IDX-2-1 AT END                   NC2344.2
053100         GO TO TH2-FAIL-F1-3                                      NC2344.2
053200         WHEN ENTRY-2-1 (IDX-1-1, IDX-2-1) = SEC-HOLD-AREA        NC2344.2
053300             NEXT SENTENCE.                                       NC2344.2
053400     PERFORM PASS                                                 NC2344.2
053500     GO TO TH2-WRITE-F1-3.                                        NC2344.2
053600 TH2-DELETE-F1-3.                                                 NC2344.2
053700     PERFORM DE-LETE.                                             NC2344.2
053800     GO TO TH2-WRITE-F1-3.                                        NC2344.2
053900 TH2-FAIL-F1-3.                                                   NC2344.2
054000     PERFORM CHECK-FAIL2.                                         NC2344.2
054100 TH2-WRITE-F1-3.                                                  NC2344.2
054200     PERFORM PRINT-DETAIL.                                        NC2344.2
054300*                                                                 NC2344.2
054400 TH2-INIT-F1-4.                                                   NC2344.2
054500     MOVE "TH2-TEST-F1-4" TO PAR-NAME.                            NC2344.2
054600     MOVE "VI-122 6.21" TO ANSI-REFERENCE.                        NC2344.2
054700     MOVE "SEARCH VARYING LEV 2" TO FEATURE.                      NC2344.2
054800     MOVE "SEC (08,02)" TO SEC-HOLD-AREA.                         NC2344.2
054900     MOVE 08 TO SUB-1.                                            NC2344.2
055000     MOVE 02 TO SUB-2.                                            NC2344.2
055100     SET IDX-1-1 TO 08.                                           NC2344.2
055200     SET IDX-2-1 TO 01.                                           NC2344.2
055300 TH2-TEST-F1-4.                                                   NC2344.2
055400     SEARCH GRP2-ENTRY-1 VARYING IDX-2-1                          NC2344.2
055500         WHEN ENTRY-2-1 (IDX-1-1, IDX-2-1) = SEC-HOLD-AREA        NC2344.2
055600              PERFORM PASS                                        NC2344.2
055700              GO TO TH2-WRITE-F1-4.                               NC2344.2
055800     GO TO TH2-FAIL-F1-4.                                         NC2344.2
055900 TH2-DELETE-F1-4.                                                 NC2344.2
056000     PERFORM DE-LETE.                                             NC2344.2
056100     GO TO TH2-WRITE-F1-4.                                        NC2344.2
056200 TH2-FAIL-F1-4.                                                   NC2344.2
056300     PERFORM CHECK-FAIL2.                                         NC2344.2
056400 TH2-WRITE-F1-4.                                                  NC2344.2
056500     PERFORM PRINT-DETAIL.                                        NC2344.2
056600     GO TO TH3-INIT-F1-1.                                         NC2344.2
056700                                                                  NC2344.2
056800 CHECK-FAIL2.                                                     NC2344.2
056900     MOVE SEC-HOLD-AREA TO CORRECT-A.                             NC2344.2
057000     IF ENTRY-2-1 (SUB-1, SUB-2) EQUAL TO SEC-HOLD-AREA           NC2344.2
057100         MOVE "IDX-2" TO END-IDX                                  NC2344.2
057200         SET IDX-VALU TO IDX-2                                    NC2344.2
057300         MOVE "SEE NOTE 1 FOR DIAGNOSTIC " TO RE-MARK             NC2344.2
057400         MOVE END-STMT TO COMPUTED-A  ELSE                        NC2344.2
057500     MOVE ENTRY-2-1 (SUB-1, SUB-2) TO COMPUTED-A                  NC2344.2
057600     MOVE "SEE NOTE 2 FOR DIAGNOSTIC " TO RE-MARK.                NC2344.2
057700     PERFORM FAIL.                                                NC2344.2
057800*                                                                 NC2344.2
057900 TH3-INIT-F1-1.                                                   NC2344.2
058000     MOVE "TH3-TEST-F1-1" TO PAR-NAME.                            NC2344.2
058100     MOVE "VI-122 6.21" TO ANSI-REFERENCE.                        NC2344.2
058200     MOVE "SEARCH VARYING LEV 3" TO FEATURE.                      NC2344.2
058300     MOVE 1 TO SUB-1  SUB-2  SUB-3.                               NC2344.2
058400     MOVE "ELEM (01,01,01)" TO ELEM-HOLD-AREA.                    NC2344.2
058500     SET IDX-1-1 IDX-2-1 IDX-3-1 TO 1.                            NC2344.2
058600 TH3-TEST-F1-1.                                                   NC2344.2
058700     SEARCH GRP3-ENTRY-1 VARYING IDX-3                            NC2344.2
058800         WHEN ENTRY-3-1 (IDX-1-1, IDX-2-1, IDX-3-1)               NC2344.2
058900              = ELEM-HOLD-AREA                                    NC2344.2
059000              PERFORM PASS                                        NC2344.2
059100              GO TO TH3-WRITE-F1-1.                               NC2344.2
059200     GO TO TH3-FAIL-F1-1.                                         NC2344.2
059300 TH3-DELETE-F1-1.                                                 NC2344.2
059400     PERFORM DE-LETE.                                             NC2344.2
059500     GO TO TH3-WRITE-F1-1.                                        NC2344.2
059600 TH3-FAIL-F1-1.                                                   NC2344.2
059700     PERFORM CHECK-FAIL3.                                         NC2344.2
059800 TH3-WRITE-F1-1.                                                  NC2344.2
059900     PERFORM PRINT-DETAIL.                                        NC2344.2
060000*                                                                 NC2344.2
060100 TH3-INIT-F1-2.                                                   NC2344.2
060200     MOVE "TH3-TEST-F1-2" TO PAR-NAME.                            NC2344.2
060300     MOVE "VI-122 6.21" TO ANSI-REFERENCE.                        NC2344.2
060400     MOVE "SEARCH VARYING LEV 3" TO FEATURE.                      NC2344.2
060500     MOVE 05 TO SUB-1.                                            NC2344.2
060600     MOVE 06 TO SUB-2.                                            NC2344.2
060700     MOVE 07 TO SUB-3.                                            NC2344.2
060800     SET IDX-1-1 TO 05.                                           NC2344.2
060900     SET IDX-2-1 TO 06.                                           NC2344.2
061000     SET IDX-3-1 TO 1.                                            NC2344.2
061100     MOVE "ELEM (05,06,07)" TO ELEM-HOLD-AREA.                    NC2344.2
061200 TH3-TEST-F1-2.                                                   NC2344.2
061300     SEARCH GRP3-ENTRY-1 VARYING IDX-3-1 AT END                   NC2344.2
061400         GO TO TH3-FAIL-F1-2                                      NC2344.2
061500         WHEN ENTRY-3-1 (IDX-1-1, IDX-2-1, IDX-3-1)               NC2344.2
061600             = ELEM-HOLD-AREA                                     NC2344.2
061700             NEXT SENTENCE.                                       NC2344.2
061800     PERFORM PASS                                                 NC2344.2
061900     GO TO TH3-WRITE-F1-2.                                        NC2344.2
062000 TH3-DELETE-F1-2.                                                 NC2344.2
062100     PERFORM DE-LETE.                                             NC2344.2
062200     GO TO TH3-WRITE-F1-2.                                        NC2344.2
062300 TH3-FAIL-F1-2.                                                   NC2344.2
062400     PERFORM CHECK-FAIL3.                                         NC2344.2
062500 TH3-WRITE-F1-2.                                                  NC2344.2
062600     PERFORM PRINT-DETAIL.                                        NC2344.2
062700*                                                                 NC2344.2
062800 TH3-INIT-F1-3.                                                   NC2344.2
062900     MOVE "TH3-TEST-F1-3" TO PAR-NAME.                            NC2344.2
063000     MOVE "VI-122 6.21" TO ANSI-REFERENCE.                        NC2344.2
063100     MOVE "SEARCH VARYING LEV 3" TO FEATURE.                      NC2344.2
063200     MOVE 10 TO SUB-1 SUB-2 SUB-3.                                NC2344.2
063300     SET IDX-1-1 IDX-2-1 TO 10.                                   NC2344.2
063400     SET IDX-3-1 TO 1.                                            NC2344.2
063500     MOVE "ELEM (10,10,10)" TO ELEM-HOLD-AREA.                    NC2344.2
063600 TH3-TEST-F1-3.                                                   NC2344.2
063700     SEARCH GRP3-ENTRY-1 VARYING IDX-3-1 AT END                   NC2344.2
063800         GO TO TH3-FAIL-F1-3                                      NC2344.2
063900         WHEN ENTRY-3-1 (IDX-1-1, IDX-2-1, IDX-3-1)               NC2344.2
064000             = ELEM-HOLD-AREA                                     NC2344.2
064100             NEXT SENTENCE.                                       NC2344.2
064200     PERFORM PASS                                                 NC2344.2
064300     GO TO TH3-WRITE-F1-3.                                        NC2344.2
064400 TH3-DELETE-F1-3.                                                 NC2344.2
064500     PERFORM DE-LETE.                                             NC2344.2
064600     GO TO TH3-WRITE-F1-3.                                        NC2344.2
064700 TH3-FAIL-F1-3.                                                   NC2344.2
064800     PERFORM CHECK-FAIL3.                                         NC2344.2
064900 TH3-WRITE-F1-3.                                                  NC2344.2
065000     PERFORM PRINT-DETAIL.                                        NC2344.2
065100*                                                                 NC2344.2
065200 TH3-INIT-F1-4.                                                   NC2344.2
065300     MOVE "TH3-TEST-F1-4" TO PAR-NAME.                            NC2344.2
065400     MOVE "VI-122 6.21" TO ANSI-REFERENCE.                        NC2344.2
065500     MOVE "SEARCH VARYING LEV 3" TO FEATURE.                      NC2344.2
065600     MOVE "ELEM (07,06,05)" TO ELEM-HOLD-AREA.                    NC2344.2
065700     MOVE 07 TO SUB-1.                                            NC2344.2
065800     MOVE 06 TO SUB-2.                                            NC2344.2
065900     MOVE 05 TO SUB-3.                                            NC2344.2
066000     SET IDX-1-1 TO 07.                                           NC2344.2
066100     SET IDX-2-1 TO 06.                                           NC2344.2
066200     SET IDX-3-1 TO 03.                                           NC2344.2
066300 TH3-TEST-F1-4.                                                   NC2344.2
066400     SEARCH GRP3-ENTRY-1 VARYING IDX-3-1 AT END                   NC2344.2
066500         GO TO TH3-FAIL-F1-4                                      NC2344.2
066600         WHEN ENTRY-3-1 (IDX-1-1, IDX-2-1, IDX-3-1)               NC2344.2
066700         = ELEM-HOLD-AREA                                         NC2344.2
066800         NEXT SENTENCE.                                           NC2344.2
066900     PERFORM PASS                                                 NC2344.2
067000     GO TO TH3-WRITE-F1-4.                                        NC2344.2
067100 TH3-DELETE-F1-4.                                                 NC2344.2
067200     PERFORM DE-LETE.                                             NC2344.2
067300     GO TO TH3-WRITE-F1-4.                                        NC2344.2
067400 TH3-FAIL-F1-4.                                                   NC2344.2
067500     PERFORM CHECK-FAIL3.                                         NC2344.2
067600 TH3-WRITE-F1-4.                                                  NC2344.2
067700     PERFORM PRINT-DETAIL.                                        NC2344.2
067800     GO TO MLT-INIT-F1-1.                                         NC2344.2
067900*                                                                 NC2344.2
068000 CHECK-FAIL3.                                                     NC2344.2
068100     MOVE ELEM-HOLD-AREA TO CORRECT-A.                            NC2344.2
068200     IF ENTRY-3-1 (SUB-1, SUB-2, SUB-3) EQUAL TO ELEM-HOLD-AREA   NC2344.2
068300         MOVE "IDX-3-1" TO END-IDX                                NC2344.2
068400         SET IDX-VALU TO IDX-3-1                                  NC2344.2
068500         MOVE "SEE NOTE 1 FOR DIAGNOSTIC " TO RE-MARK             NC2344.2
068600         MOVE END-STMT TO COMPUTED-A ELSE                         NC2344.2
068700     MOVE ENTRY-3-1 (SUB-1, SUB-2, SUB-3) TO COMPUTED-A           NC2344.2
068800     MOVE "SEE NOTE 2 FOR DIAGNOSTIC " TO RE-MARK.                NC2344.2
068900     PERFORM FAIL.                                                NC2344.2
069000*                                                                 NC2344.2
069100 MLT-INIT-F1-1.                                                   NC2344.2
069200     MOVE "MLT-TEST-F1-1  " TO PAR-NAME.                          NC2344.2
069300     MOVE "VI-122 6.21" TO ANSI-REFERENCE.                        NC2344.2
069400     MOVE "MULTIPLE SEARCH STMT" TO FEATURE.                      NC2344.2
069500     MOVE "GRP08" TO GRP-HOLD-AREA.                               NC2344.2
069600     MOVE "SEC (08,07)" TO SEC-HOLD-AREA.                         NC2344.2
069700     SET IDX-1-1 IDX-2-1 TO 1.                                    NC2344.2
069800 MLT-TEST-F1-1.                                                   NC2344.2
069900     SEARCH GRP-ENTRY-1 VARYING IDX-1-1                           NC2344.2
070000            AT END GO TO MLT-FAIL-F1-1-A                          NC2344.2
070100            WHEN ENTRY-1-1 (IDX-1-1) = "GRP08" NEXT SENTENCE.     NC2344.2
070200     SEARCH GRP2-ENTRY-1 VARYING IDX-2-1                          NC2344.2
070300            AT END GO TO MLT-FAIL-F1-1-B                          NC2344.2
070400            WHEN ENTRY-2-1 (IDX-1-1, IDX-2-1) = SEC-HOLD-AREA     NC2344.2
070500                 NEXT SENTENCE.                                   NC2344.2
070600     PERFORM PASS                                                 NC2344.2
070700     GO TO MLT-WRITE-F1-1.                                        NC2344.2
070800 MLT-DELETE-F1-1.                                                 NC2344.2
070900     PERFORM DE-LETE.                                             NC2344.2
071000     GO TO MLT-WRITE-F1-1.                                        NC2344.2
071100*                                                                 NC2344.2
071200 MLT-FAIL-F1-1-A.                                                 NC2344.2
071300     MOVE GRP-HOLD-AREA TO CORRECT-A.                             NC2344.2
071400     IF ENTRY-1-1 (08) EQUAL TO GRP-HOLD-AREA                     NC2344.2
071500         MOVE "IDX-1-1" TO END-IDX                                NC2344.2
071600         MOVE "SEE NOTE 1 FOR DIAGNOSTIC " TO RE-MARK             NC2344.2
071700         SET IDX-VALU TO IDX-1-1                                  NC2344.2
071800         MOVE END-STMT TO COMPUTED-A ELSE                         NC2344.2
071900     MOVE ENTRY-1-1 (08) TO COMPUTED-A                            NC2344.2
072000     MOVE "SEE NOTE 2 FOR DIAGNOSTIC " TO RE-MARK.                NC2344.2
072100     PERFORM FAIL                                                 NC2344.2
072200     GO TO MLT-WRITE-F1-1.                                        NC2344.2
072300*                                                                 NC2344.2
072400 MLT-FAIL-F1-1-B.                                                 NC2344.2
072500     MOVE SEC-HOLD-AREA TO CORRECT-A.                             NC2344.2
072600     IF ENTRY-2-1 (08, 07) EQUAL TO SEC-HOLD-AREA                 NC2344.2
072700         MOVE "IDX-2-1" TO END-IDX                                NC2344.2
072800         SET IDX-VALU TO IDX-2-1                                  NC2344.2
072900         MOVE "SEE NOTE 1 FOR DIAGNOSTIC " TO RE-MARK             NC2344.2
073000         MOVE END-STMT TO COMPUTED-A ELSE                         NC2344.2
073100     MOVE ENTRY-2-1 (08, 07) TO COMPUTED-A                        NC2344.2
073200     MOVE "SEE NOTE 2 FOR DIAGNOSTIC " TO RE-MARK.                NC2344.2
073300     PERFORM FAIL.                                                NC2344.2
073400 MLT-WRITE-F1-1.                                                  NC2344.2
073500     PERFORM PRINT-DETAIL.                                        NC2344.2
073600*                                                                 NC2344.2
073700 MLT-INIT-F1-2.                                                   NC2344.2
073800     MOVE "MLT-TEST-F1-2  " TO PAR-NAME.                          NC2344.2
073900     MOVE "VI-122 6.21" TO ANSI-REFERENCE.                        NC2344.2
074000     MOVE "MULTIPLE SEARCH STMT" TO FEATURE.                      NC2344.2
074100     MOVE "GRP04" TO GRP-HOLD-AREA.                               NC2344.2
074200     MOVE "SEC (04,04)" TO SEC-HOLD-AREA.                         NC2344.2
074300     MOVE "ELEM (04,04,04)" TO ELEM-HOLD-AREA.                    NC2344.2
074400     SET IDX-1-1 IDX-2-1 IDX-3-1 TO 1.                            NC2344.2
074500 MLT-TEST-F1-2.                                                   NC2344.2
074600     SEARCH GRP-ENTRY-1  VARYING IDX-1-1 AT END                   NC2344.2
074700         GO TO MLT-FAIL-F1-2-A WHEN ENTRY-1-1 (IDX-1-1) =         NC2344.2
074800         GRP-HOLD-AREA  NEXT SENTENCE.                            NC2344.2
074900     SEARCH GRP2-ENTRY-1 VARYING IDX-2-1 AT END                   NC2344.2
075000         GO TO MLT-FAIL-F1-2-B WHEN ENTRY-2-1 (IDX-1-1, IDX-2-1) =NC2344.2
075100         SEC-HOLD-AREA  NEXT SENTENCE.                            NC2344.2
075200     SEARCH GRP3-ENTRY-1 VARYING IDX-3-1 AT END                   NC2344.2
075300         GO TO MLT-FAIL-F1-2-C WHEN ENTRY-3-1                     NC2344.2
075400             (IDX-1-1, IDX-2-1, IDX-3-1)                          NC2344.2
075500             = ELEM-HOLD-AREA NEXT SENTENCE.                      NC2344.2
075600     PERFORM PASS                                                 NC2344.2
075700     GO TO MLT-WRITE-F1-2.                                        NC2344.2
075800 MLT-DELETE-F1-2.                                                 NC2344.2
075900     PERFORM DE-LETE                                              NC2344.2
076000     GO TO MLT-WRITE-F1-2.                                        NC2344.2
076100 MLT-FAIL-F1-2-A.                                                 NC2344.2
076200     MOVE GRP-HOLD-AREA TO CORRECT-A.                             NC2344.2
076300     IF ENTRY-1-1 (04) EQUAL TO GRP-HOLD-AREA                     NC2344.2
076400         MOVE "IDX-1-1" TO END-IDX                                NC2344.2
076500         MOVE "SEE NOTE 1 FOR DIAGNOSTIC " TO RE-MARK             NC2344.2
076600         SET IDX-VALU TO IDX-1-1                                  NC2344.2
076700         MOVE END-STMT TO COMPUTED-A ELSE                         NC2344.2
076800     MOVE ENTRY-1-1 (04) TO COMPUTED-A                            NC2344.2
076900     MOVE "SEE NOTE 2 FOR DIAGNOSTIC " TO RE-MARK.                NC2344.2
077000     PERFORM FAIL.                                                NC2344.2
077100     GO TO MLT-WRITE-F1-2.                                        NC2344.2
077200                                                                  NC2344.2
077300 MLT-FAIL-F1-2-B.                                                 NC2344.2
077400     MOVE  SEC-HOLD-AREA TO CORRECT-A.                            NC2344.2
077500     IF ENTRY-2-1 (04, 04) EQUAL TO SEC-HOLD-AREA                 NC2344.2
077600         MOVE "IDX-2-1" TO END-IDX                                NC2344.2
077700         MOVE "SEE NOTE 1 FOR DIAGNOSTIC " TO RE-MARK             NC2344.2
077800         SET IDX-VALU TO IDX-2-1                                  NC2344.2
077900         MOVE END-STMT TO COMPUTED-A ELSE                         NC2344.2
078000     MOVE ENTRY-2-1 (04, 04) TO COMPUTED-A                        NC2344.2
078100     MOVE "SEE NOTE 2 FOR DIAGNOSTIC " TO RE-MARK.                NC2344.2
078200     PERFORM FAIL                                                 NC2344.2
078300     GO TO MLT-WRITE-F1-2.                                        NC2344.2
078400                                                                  NC2344.2
078500 MLT-FAIL-F1-2-C.                                                 NC2344.2
078600     MOVE ELEM-HOLD-AREA TO CORRECT-A.                            NC2344.2
078700     IF ENTRY-3-1 (04, 04, 04) EQUAL TO ELEM-HOLD-AREA            NC2344.2
078800         MOVE "IDX-3-1" TO END-IDX                                NC2344.2
078900         MOVE "SEE NOTE 1 FOR DIAGNOSTIC " TO RE-MARK             NC2344.2
079000         SET IDX-VALU TO IDX-3-1                                  NC2344.2
079100         MOVE END-STMT TO COMPUTED-A ELSE                         NC2344.2
079200     MOVE ENTRY-3-1 (04, 04, 04) TO COMPUTED-A                    NC2344.2
079300     MOVE "SEE NOTE 2 FOR DIAGNOSTIC " TO RE-MARK.                NC2344.2
079400     PERFORM FAIL.                                                NC2344.2
079500 MLT-WRITE-F1-2.                                                  NC2344.2
079600     PERFORM PRINT-DETAIL.                                        NC2344.2
079700*                                                                 NC2344.2
079800 SPC-INIT-F1-1.                                                   NC2344.2
079900     MOVE "SPC-TEST-F1-1" TO PAR-NAME.                            NC2344.2
080000     MOVE "VI-122 6.21" TO ANSI-REFERENCE.                        NC2344.2
080100     MOVE "SEARCH WITH HI INDEX" TO FEATURE.                      NC2344.2
080200     SET IDX-1-1 TO 4.                                            NC2344.2
080300 SPC-TEST-F1-1.                                                   NC2344.2
080400     SEARCH GRP-ENTRY-1 VARYING IDX-1-1                           NC2344.2
080500          AT END PERFORM PASS                                     NC2344.2
080600                 GO TO SPC-WRITE-F1-1                             NC2344.2
080700          WHEN ENTRY-1-1 (IDX-1-1) = "GRP03"                      NC2344.2
080800                 GO TO SPC-FAIL-F1-1.                             NC2344.2
080900 SPC-DELETE-F1-1.                                                 NC2344.2
081000     PERFORM DE-LETE.                                             NC2344.2
081100     GO TO SPC-WRITE-F1-1.                                        NC2344.2
081200 SPC-FAIL-F1-1.                                                   NC2344.2
081300     MOVE SPACES TO CORRECT-A.                                    NC2344.2
081400     MOVE ENTRY-1-1 (03) TO COMPUTED-A.                           NC2344.2
081500     MOVE SPACES TO RE-MARK.                                      NC2344.2
081600     PERFORM FAIL.                                                NC2344.2
081700 SPC-WRITE-F1-1.                                                  NC2344.2
081800     PERFORM PRINT-DETAIL.                                        NC2344.2
081900*                                                                 NC2344.2
082000 SP2-INIT-F1-1.                                                   NC2344.2
082100     MOVE "SP2-TEST-F1-1" TO PAR-NAME.                            NC2344.2
082200     MOVE "VI-122 6.21" TO ANSI-REFERENCE.                        NC2344.2
082300     MOVE "SEARCH WITH HI INDEX" TO FEATURE.                      NC2344.2
082400     SET IDX-1-1 TO 4.                                            NC2344.2
082500     SET IDX-2-1 TO 5.                                            NC2344.2
082600 SP2-TEST-F1-1.                                                   NC2344.2
082700     SEARCH GRP-ENTRY-1 VARYING IDX-1-1 AT END                    NC2344.2
082800         GO TO SP2-FAIL-F1-1-A                                    NC2344.2
082900     WHEN ENTRY-1-1 (IDX-1-1) = "GRP04" NEXT SENTENCE.            NC2344.2
083000     SET IDX-1-1 TO 4.                                            NC2344.2
083100     SEARCH GRP2-ENTRY-1 VARYING IDX-2-1                          NC2344.2
083200         AT END PERFORM PASS                                      NC2344.2
083300                GO TO SP2-WRITE-F1-1                              NC2344.2
083400     WHEN ENTRY-2-1 (IDX-1-1, IDX-2-1) = "SEC (04,04)"            NC2344.2
083500         GO TO SP2-FAIL-F1-1-B.                                   NC2344.2
083600 SP2-DELETE-F1-1.                                                 NC2344.2
083700     PERFORM DE-LETE.                                             NC2344.2
083800     GO TO SP2-WRITE-F1-1.                                        NC2344.2
083900 SP2-FAIL-F1-1-A.                                                 NC2344.2
084000     MOVE "GRP04" TO CORRECT-A.                                   NC2344.2
084100     IF ENTRY-1-1 (04) EQUAL TO "GRP04"                           NC2344.2
084200         MOVE "IDX-2-1" TO END-IDX                                NC2344.2
084300         MOVE "SEE NOTE 1 FOR DIAGNOSTIC " TO RE-MARK             NC2344.2
084400         SET IDX-VALU TO IDX-2-1                                  NC2344.2
084500         MOVE END-STMT TO COMPUTED-A ELSE                         NC2344.2
084600     MOVE ENTRY-1-1 (04) TO COMPUTED-A                            NC2344.2
084700     MOVE "SEE NOTE 2 FOR DIAGNOSTIC " TO RE-MARK.                NC2344.2
084800     PERFORM FAIL.                                                NC2344.2
084900     GO TO SP2-WRITE-F1-1.                                        NC2344.2
085000*                                                                 NC2344.2
085100 SP2-FAIL-F1-1-B.                                                 NC2344.2
085200     MOVE ENTRY-2-1 (04, 04) TO COMPUTED-A.                       NC2344.2
085300     MOVE SPACES TO CORRECT-A.                                    NC2344.2
085400     PERFORM FAIL.                                                NC2344.2
085500 SP2-WRITE-F1-1.                                                  NC2344.2
085600     PERFORM PRINT-DETAIL.                                        NC2344.2
085700*                                                                 NC2344.2
085800 SP3-INIT-F1-1.                                                   NC2344.2
085900     MOVE "SP3-TEST-F1-1" TO PAR-NAME.                            NC2344.2
086000     MOVE "VI-122 6.21" TO ANSI-REFERENCE.                        NC2344.2
086100     MOVE "SEARCH WITH HI INDEX" TO FEATURE.                      NC2344.2
086200     SET IDX-1-1 TO 02.                                           NC2344.2
086300 SP3-TEST-F1-1.                                                   NC2344.2
086400     SEARCH GRP-ENTRY-1 VARYING IDX-1-1                           NC2344.2
086500         AT END                                                   NC2344.2
086600                GO TO SP3-FAIL-F1-1-A                             NC2344.2
086700         WHEN ENTRY-1-1 (IDX-1-1) EQUAL TO "GRP02"                NC2344.2
086800                NEXT SENTENCE.                                    NC2344.2
086900     SET IDX-1-1 TO 02.                                           NC2344.2
087000     SET IDX-2-1 TO 01.                                           NC2344.2
087100     SEARCH GRP2-ENTRY-1 VARYING IDX-2-1                          NC2344.2
087200         AT END                                                   NC2344.2
087300                GO TO SP3-FAIL-F1-1-B                             NC2344.2
087400         WHEN ENTRY-2-1 (IDX-1-1, IDX-2-1) = "SEC (02,03)"        NC2344.2
087500                NEXT SENTENCE.                                    NC2344.2
087600     SET IDX-1-1 TO 02.                                           NC2344.2
087700     SET IDX-2-1 TO 03.                                           NC2344.2
087800     SET IDX-3-1 TO 05.                                           NC2344.2
087900     SEARCH GRP3-ENTRY-1 VARYING IDX-3-1                          NC2344.2
088000         AT END PERFORM PASS                                      NC2344.2
088100                GO TO SP3-WRITE-F1-1                              NC2344.2
088200         WHEN ENTRY-3-1 (IDX-1-1, IDX-2-1, IDX-3-1)               NC2344.2
088300              = "ELEM (02,03,04)"                                 NC2344.2
088400                GO TO SP3-FAIL-F1-1-C.                            NC2344.2
088500 SP3-DELETE-F1-1.                                                 NC2344.2
088600     PERFORM DE-LETE.                                             NC2344.2
088700     GO TO SP3-WRITE-F1-1.                                        NC2344.2
088800 SP3-FAIL-F1-1-A.                                                 NC2344.2
088900     MOVE "GRP02" TO CORRECT-A.                                   NC2344.2
089000     IF ENTRY-1-1 (02) EQUAL TO "GRP02"                           NC2344.2
089100         MOVE "IDX-1-1" TO END-IDX                                NC2344.2
089200         MOVE "SEE NOTE 1 FOR DIAGNOSTIC " TO RE-MARK             NC2344.2
089300         SET IDX-VALU TO IDX-1-1                                  NC2344.2
089400         MOVE END-STMT TO COMPUTED-A ELSE                         NC2344.2
089500     MOVE ENTRY-1-1 (02) TO COMPUTED-A                            NC2344.2
089600     MOVE "SEE NOTE 2 FOR DIAGNOSTIC " TO RE-MARK.                NC2344.2
089700     PERFORM FAIL.                                                NC2344.2
089800     GO TO SP3-WRITE-F1-1.                                        NC2344.2
089900*                                                                 NC2344.2
090000 SP3-FAIL-F1-1-B.                                                 NC2344.2
090100     MOVE "SEC (02,03)" TO CORRECT-A.                             NC2344.2
090200     IF ENTRY-2-1 (02, 03) EQUAL TO "SEC (02,03)"                 NC2344.2
090300         MOVE "IDX-2-1"  TO END-IDX                               NC2344.2
090400         MOVE "SEE NOTE 1 FOR DIAGNOSTIC " TO RE-MARK             NC2344.2
090500         SET IDX-VALU TO IDX-2-1                                  NC2344.2
090600         MOVE END-STMT TO COMPUTED-A ELSE                         NC2344.2
090700     MOVE ENTRY-2-1 (02, 03) TO COMPUTED-A                        NC2344.2
090800     MOVE "SEE NOTE 2 FOR DIAGNOSTIC " TO RE-MARK.                NC2344.2
090900     PERFORM FAIL.                                                NC2344.2
091000     GO TO SP3-WRITE-F1-1.                                        NC2344.2
091100*                                                                 NC2344.2
091200 SP3-FAIL-F1-1-C.                                                 NC2344.2
091300     MOVE "INDEX SET HIGHER THAN ENTRY" TO RE-MARK                NC2344.2
091400     MOVE SPACES TO CORRECT-A                                     NC2344.2
091500     MOVE "ELEM (02,03,04)" TO COMPUTED-A                         NC2344.2
091600     PERFORM FAIL.                                                NC2344.2
091700 SP3-WRITE-F1-1.                                                  NC2344.2
091800     PERFORM PRINT-DETAIL.                                        NC2344.2
091900*                                                                 NC2344.2
092000 CCVS-EXIT SECTION.                                               NC2344.2
092100 CCVS-999999.                                                     NC2344.2
092200     GO TO CLOSE-FILES.                                           NC2344.2
*END-OF,NC234A                                                                  
*HEADER,COBOL,NC235A                                                            
000100 IDENTIFICATION DIVISION.                                         NC2354.2
000200 PROGRAM-ID.                                                      NC2354.2
000300     NC235A.                                                      NC2354.2
000400*                                                                 NC2354.2
000500****************************************************************  NC2354.2
000600*                                                              *  NC2354.2
000700*    VALIDATION FOR:-                                          *  NC2354.2
000800*                                                              *  NC2354.2
000900*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2354.2
001000*                                                              *  NC2354.2
001100*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC2354.2
001200*                                                              *  NC2354.2
001300****************************************************************  NC2354.2
001400*                                                              *  NC2354.2
001500*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  NC2354.2
001600*                                                              *  NC2354.2
001700*        X-55  - SYSTEM PRINTER NAME.                          *  NC2354.2
001800*        X-82  - SOURCE COMPUTER NAME.                         *  NC2354.2
001900*        X-83  - OBJECT COMPUTER NAME.                         *  NC2354.2
002000*                                                              *  NC2354.2
002100****************************************************************  NC2354.2
002200*    PROGRAM NC235A TESTS THE USE OF FORMATS 1 AND 2 OF THE    *  NC2354.2
002300*    "SEARCH" STATEMENT ON A ONE DIMENSIONAL TABLE WITH A      *  NC2354.2
002400*    VARIABLE NUMBER OF OCCURRENCES.   THE TABLE IS DEFINED    *  NC2354.2
002500*    USING FORMAT 2 OF THE "OCCURS" CLAUSE.                    *  NC2354.2
002600*                                                              *  NC2354.2
002700****************************************************************  NC2354.2
002800 ENVIRONMENT DIVISION.                                            NC2354.2
002900 CONFIGURATION SECTION.                                           NC2354.2
003000 SOURCE-COMPUTER.                                                 NC2354.2
003100     XXXXX082.                                                    NC2354.2
003200 OBJECT-COMPUTER.                                                 NC2354.2
003300     XXXXX083.                                                    NC2354.2
003400 INPUT-OUTPUT SECTION.                                            NC2354.2
003500 FILE-CONTROL.                                                    NC2354.2
003600     SELECT PRINT-FILE ASSIGN TO                                  NC2354.2
003700     XXXXX055.                                                    NC2354.2
003800 DATA DIVISION.                                                   NC2354.2
003900 FILE SECTION.                                                    NC2354.2
004000 FD  PRINT-FILE.                                                  NC2354.2
004100 01  PRINT-REC PICTURE X(120).                                    NC2354.2
004200 01  DUMMY-RECORD PICTURE X(120).                                 NC2354.2
004300 WORKING-STORAGE SECTION.                                         NC2354.2
004400 77  TBL-LENGTH                  PIC 99  VALUE 26.                NC2354.2
004500 77  SUB-1                       PIC 99  VALUE ZERO.              NC2354.2
004600 01  TBL-TH309.                                                   NC2354.2
004700     02  TH309-ENTRY OCCURS 1 TO 26 DEPENDING TBL-LENGTH          NC2354.2
004800         DESCENDING KEY IS DEC-KEY INDEXED BY IDX-1, IDX-2, IDX-3.NC2354.2
004900         03  DEC-KEY             PIC XX.                          NC2354.2
005000             88  FIRSTZ VALUE "ZZ".                               NC2354.2
005100             88  LASTA  VALUE "AA".                               NC2354.2
005200             88  MIDDLE-PP VALUE "PP".                            NC2354.2
005300 01  NOTE-1.                                                      NC2354.2
005400     02  FILLER                  PIC X(74) VALUE                  NC2354.2
005500     "NOTE 1 - CORRECT AND COMPUTED DATA ARE EQUAL BUT THE AT END NC2354.2
005600-    "PATH WAS TAKEN".                                            NC2354.2
005700     02  FILLER                  PIC X(46) VALUE SPACE.           NC2354.2
005800 01  NOTE-2.                                                      NC2354.2
005900     02  FILLER                  PIC X(112) VALUE                 NC2354.2
006000     "NOTE 2 - CORRECT AND COMPUTED DATA ARE NOT EQUAL. THE COMPUTNC2354.2
006100-    "ED ENTRY WAS EXTRACTED FROM THE TABLE BY SUBSCRIPTS.".      NC2354.2
006200     02  FILLER                  PIC X(8)  VALUE SPACE.           NC2354.2
006300 01  TEST-RESULTS.                                                NC2354.2
006400     02 FILLER                   PIC X      VALUE SPACE.          NC2354.2
006500     02 FEATURE                  PIC X(20)  VALUE SPACE.          NC2354.2
006600     02 FILLER                   PIC X      VALUE SPACE.          NC2354.2
006700     02 P-OR-F                   PIC X(5)   VALUE SPACE.          NC2354.2
006800     02 FILLER                   PIC X      VALUE SPACE.          NC2354.2
006900     02  PAR-NAME.                                                NC2354.2
007000       03 FILLER                 PIC X(19)  VALUE SPACE.          NC2354.2
007100       03  PARDOT-X              PIC X      VALUE SPACE.          NC2354.2
007200       03 DOTVALUE               PIC 99     VALUE ZERO.           NC2354.2
007300     02 FILLER                   PIC X(8)   VALUE SPACE.          NC2354.2
007400     02 RE-MARK                  PIC X(61).                       NC2354.2
007500 01  TEST-COMPUTED.                                               NC2354.2
007600     02 FILLER                   PIC X(30)  VALUE SPACE.          NC2354.2
007700     02 FILLER                   PIC X(17)  VALUE                 NC2354.2
007800            "       COMPUTED=".                                   NC2354.2
007900     02 COMPUTED-X.                                               NC2354.2
008000     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          NC2354.2
008100     03 COMPUTED-N               REDEFINES COMPUTED-A             NC2354.2
008200                                 PIC -9(9).9(9).                  NC2354.2
008300     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         NC2354.2
008400     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     NC2354.2
008500     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     NC2354.2
008600     03       CM-18V0 REDEFINES COMPUTED-A.                       NC2354.2
008700         04 COMPUTED-18V0                    PIC -9(18).          NC2354.2
008800         04 FILLER                           PIC X.               NC2354.2
008900     03 FILLER PIC X(50) VALUE SPACE.                             NC2354.2
009000 01  TEST-CORRECT.                                                NC2354.2
009100     02 FILLER PIC X(30) VALUE SPACE.                             NC2354.2
009200     02 FILLER PIC X(17) VALUE "       CORRECT =".                NC2354.2
009300     02 CORRECT-X.                                                NC2354.2
009400     03 CORRECT-A                  PIC X(20) VALUE SPACE.         NC2354.2
009500     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      NC2354.2
009600     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         NC2354.2
009700     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     NC2354.2
009800     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     NC2354.2
009900     03      CR-18V0 REDEFINES CORRECT-A.                         NC2354.2
010000         04 CORRECT-18V0                     PIC -9(18).          NC2354.2
010100         04 FILLER                           PIC X.               NC2354.2
010200     03 FILLER PIC X(2) VALUE SPACE.                              NC2354.2
010300     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     NC2354.2
010400 01  CCVS-C-1.                                                    NC2354.2
010500     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PANC2354.2
010600-    "SS  PARAGRAPH-NAME                                          NC2354.2
010700-    "       REMARKS".                                            NC2354.2
010800     02 FILLER                     PIC X(20)    VALUE SPACE.      NC2354.2
010900 01  CCVS-C-2.                                                    NC2354.2
011000     02 FILLER                     PIC X        VALUE SPACE.      NC2354.2
011100     02 FILLER                     PIC X(6)     VALUE "TESTED".   NC2354.2
011200     02 FILLER                     PIC X(15)    VALUE SPACE.      NC2354.2
011300     02 FILLER                     PIC X(4)     VALUE "FAIL".     NC2354.2
011400     02 FILLER                     PIC X(94)    VALUE SPACE.      NC2354.2
011500 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       NC2354.2
011600 01  REC-CT                        PIC 99       VALUE ZERO.       NC2354.2
011700 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       NC2354.2
011800 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       NC2354.2
011900 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       NC2354.2
012000 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       NC2354.2
012100 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       NC2354.2
012200 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       NC2354.2
012300 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      NC2354.2
012400 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       NC2354.2
012500 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     NC2354.2
012600 01  CCVS-H-1.                                                    NC2354.2
012700     02  FILLER                    PIC X(39)    VALUE SPACES.     NC2354.2
012800     02  FILLER                    PIC X(42)    VALUE             NC2354.2
012900     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 NC2354.2
013000     02  FILLER                    PIC X(39)    VALUE SPACES.     NC2354.2
013100 01  CCVS-H-2A.                                                   NC2354.2
013200   02  FILLER                        PIC X(40)  VALUE SPACE.      NC2354.2
013300   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  NC2354.2
013400   02  FILLER                        PIC XXXX   VALUE             NC2354.2
013500     "4.2 ".                                                      NC2354.2
013600   02  FILLER                        PIC X(28)  VALUE             NC2354.2
013700            " COPY - NOT FOR DISTRIBUTION".                       NC2354.2
013800   02  FILLER                        PIC X(41)  VALUE SPACE.      NC2354.2
013900                                                                  NC2354.2
014000 01  CCVS-H-2B.                                                   NC2354.2
014100   02  FILLER                        PIC X(15)  VALUE             NC2354.2
014200            "TEST RESULT OF ".                                    NC2354.2
014300   02  TEST-ID                       PIC X(9).                    NC2354.2
014400   02  FILLER                        PIC X(4)   VALUE             NC2354.2
014500            " IN ".                                               NC2354.2
014600   02  FILLER                        PIC X(12)  VALUE             NC2354.2
014700     " HIGH       ".                                              NC2354.2
014800   02  FILLER                        PIC X(22)  VALUE             NC2354.2
014900            " LEVEL VALIDATION FOR ".                             NC2354.2
015000   02  FILLER                        PIC X(58)  VALUE             NC2354.2
015100     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2354.2
015200 01  CCVS-H-3.                                                    NC2354.2
015300     02  FILLER                      PIC X(34)  VALUE             NC2354.2
015400            " FOR OFFICIAL USE ONLY    ".                         NC2354.2
015500     02  FILLER                      PIC X(58)  VALUE             NC2354.2
015600     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC2354.2
015700     02  FILLER                      PIC X(28)  VALUE             NC2354.2
015800            "  COPYRIGHT   1985 ".                                NC2354.2
015900 01  CCVS-E-1.                                                    NC2354.2
016000     02 FILLER                       PIC X(52)  VALUE SPACE.      NC2354.2
016100     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              NC2354.2
016200     02 ID-AGAIN                     PIC X(9).                    NC2354.2
016300     02 FILLER                       PIC X(45)  VALUE SPACES.     NC2354.2
016400 01  CCVS-E-2.                                                    NC2354.2
016500     02  FILLER                      PIC X(31)  VALUE SPACE.      NC2354.2
016600     02  FILLER                      PIC X(21)  VALUE SPACE.      NC2354.2
016700     02 CCVS-E-2-2.                                               NC2354.2
016800         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      NC2354.2
016900         03 FILLER                   PIC X      VALUE SPACE.      NC2354.2
017000         03 ENDER-DESC               PIC X(44)  VALUE             NC2354.2
017100            "ERRORS ENCOUNTERED".                                 NC2354.2
017200 01  CCVS-E-3.                                                    NC2354.2
017300     02  FILLER                      PIC X(22)  VALUE             NC2354.2
017400            " FOR OFFICIAL USE ONLY".                             NC2354.2
017500     02  FILLER                      PIC X(12)  VALUE SPACE.      NC2354.2
017600     02  FILLER                      PIC X(58)  VALUE             NC2354.2
017700     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2354.2
017800     02  FILLER                      PIC X(13)  VALUE SPACE.      NC2354.2
017900     02 FILLER                       PIC X(15)  VALUE             NC2354.2
018000             " COPYRIGHT 1985".                                   NC2354.2
018100 01  CCVS-E-4.                                                    NC2354.2
018200     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      NC2354.2
018300     02 FILLER                       PIC X(4)   VALUE " OF ".     NC2354.2
018400     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      NC2354.2
018500     02 FILLER                       PIC X(40)  VALUE             NC2354.2
018600      "  TESTS WERE EXECUTED SUCCESSFULLY".                       NC2354.2
018700 01  XXINFO.                                                      NC2354.2
018800     02 FILLER                       PIC X(19)  VALUE             NC2354.2
018900            "*** INFORMATION ***".                                NC2354.2
019000     02 INFO-TEXT.                                                NC2354.2
019100       04 FILLER                     PIC X(8)   VALUE SPACE.      NC2354.2
019200       04 XXCOMPUTED                 PIC X(20).                   NC2354.2
019300       04 FILLER                     PIC X(5)   VALUE SPACE.      NC2354.2
019400       04 XXCORRECT                  PIC X(20).                   NC2354.2
019500     02 INF-ANSI-REFERENCE           PIC X(48).                   NC2354.2
019600 01  HYPHEN-LINE.                                                 NC2354.2
019700     02 FILLER  PIC IS X VALUE IS SPACE.                          NC2354.2
019800     02 FILLER  PIC IS X(65)    VALUE IS "************************NC2354.2
019900-    "*****************************************".                 NC2354.2
020000     02 FILLER  PIC IS X(54)    VALUE IS "************************NC2354.2
020100-    "******************************".                            NC2354.2
020200 01  CCVS-PGM-ID                     PIC X(9)   VALUE             NC2354.2
020300     "NC235A".                                                    NC2354.2
020400 PROCEDURE DIVISION.                                              NC2354.2
020500 CCVS1 SECTION.                                                   NC2354.2
020600 OPEN-FILES.                                                      NC2354.2
020700     OPEN     OUTPUT PRINT-FILE.                                  NC2354.2
020800     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   NC2354.2
020900     MOVE    SPACE TO TEST-RESULTS.                               NC2354.2
021000     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             NC2354.2
021100     GO TO CCVS1-EXIT.                                            NC2354.2
021200 CLOSE-FILES.                                                     NC2354.2
021300     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   NC2354.2
021400 TERMINATE-CCVS.                                                  NC2354.2
021500S    EXIT PROGRAM.                                                NC2354.2
021600STERMINATE-CALL.                                                  NC2354.2
021700     STOP     RUN.                                                NC2354.2
021800 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         NC2354.2
021900 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           NC2354.2
022000 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          NC2354.2
022100 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      NC2354.2
022200     MOVE "****TEST DELETED****" TO RE-MARK.                      NC2354.2
022300 PRINT-DETAIL.                                                    NC2354.2
022400     IF REC-CT NOT EQUAL TO ZERO                                  NC2354.2
022500             MOVE "." TO PARDOT-X                                 NC2354.2
022600             MOVE REC-CT TO DOTVALUE.                             NC2354.2
022700     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      NC2354.2
022800     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               NC2354.2
022900        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 NC2354.2
023000          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 NC2354.2
023100     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              NC2354.2
023200     MOVE SPACE TO CORRECT-X.                                     NC2354.2
023300     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         NC2354.2
023400     MOVE     SPACE TO RE-MARK.                                   NC2354.2
023500 HEAD-ROUTINE.                                                    NC2354.2
023600     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC2354.2
023700     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC2354.2
023800     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC2354.2
023900     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC2354.2
024000 COLUMN-NAMES-ROUTINE.                                            NC2354.2
024100     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2354.2
024200     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2354.2
024300     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        NC2354.2
024400 END-ROUTINE.                                                     NC2354.2
024500     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.NC2354.2
024600 END-RTN-EXIT.                                                    NC2354.2
024700     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2354.2
024800 END-ROUTINE-1.                                                   NC2354.2
024900      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      NC2354.2
025000      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               NC2354.2
025100      ADD PASS-COUNTER TO ERROR-HOLD.                             NC2354.2
025200*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   NC2354.2
025300      MOVE PASS-COUNTER TO CCVS-E-4-1.                            NC2354.2
025400      MOVE ERROR-HOLD TO CCVS-E-4-2.                              NC2354.2
025500      MOVE CCVS-E-4 TO CCVS-E-2-2.                                NC2354.2
025600      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           NC2354.2
025700  END-ROUTINE-12.                                                 NC2354.2
025800      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        NC2354.2
025900     IF       ERROR-COUNTER IS EQUAL TO ZERO                      NC2354.2
026000         MOVE "NO " TO ERROR-TOTAL                                NC2354.2
026100         ELSE                                                     NC2354.2
026200         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       NC2354.2
026300     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           NC2354.2
026400     PERFORM WRITE-LINE.                                          NC2354.2
026500 END-ROUTINE-13.                                                  NC2354.2
026600     IF DELETE-COUNTER IS EQUAL TO ZERO                           NC2354.2
026700         MOVE "NO " TO ERROR-TOTAL  ELSE                          NC2354.2
026800         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      NC2354.2
026900     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   NC2354.2
027000     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2354.2
027100      IF   INSPECT-COUNTER EQUAL TO ZERO                          NC2354.2
027200          MOVE "NO " TO ERROR-TOTAL                               NC2354.2
027300      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   NC2354.2
027400      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            NC2354.2
027500      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          NC2354.2
027600     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2354.2
027700 WRITE-LINE.                                                      NC2354.2
027800     ADD 1 TO RECORD-COUNT.                                       NC2354.2
027900Y    IF RECORD-COUNT GREATER 50                                   NC2354.2
028000Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          NC2354.2
028100Y        MOVE SPACE TO DUMMY-RECORD                               NC2354.2
028200Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  NC2354.2
028300Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             NC2354.2
028400Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     NC2354.2
028500Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          NC2354.2
028600Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          NC2354.2
028700Y        MOVE ZERO TO RECORD-COUNT.                               NC2354.2
028800     PERFORM WRT-LN.                                              NC2354.2
028900 WRT-LN.                                                          NC2354.2
029000     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               NC2354.2
029100     MOVE SPACE TO DUMMY-RECORD.                                  NC2354.2
029200 BLANK-LINE-PRINT.                                                NC2354.2
029300     PERFORM WRT-LN.                                              NC2354.2
029400 FAIL-ROUTINE.                                                    NC2354.2
029500     IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. NC2354.2
029600     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.NC2354.2
029700     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC2354.2
029800     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   NC2354.2
029900     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2354.2
030000     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC2354.2
030100     GO TO  FAIL-ROUTINE-EX.                                      NC2354.2
030200 FAIL-ROUTINE-WRITE.                                              NC2354.2
030300     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         NC2354.2
030400     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 NC2354.2
030500     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. NC2354.2
030600     MOVE   SPACES TO COR-ANSI-REFERENCE.                         NC2354.2
030700 FAIL-ROUTINE-EX. EXIT.                                           NC2354.2
030800 BAIL-OUT.                                                        NC2354.2
030900     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   NC2354.2
031000     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           NC2354.2
031100 BAIL-OUT-WRITE.                                                  NC2354.2
031200     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  NC2354.2
031300     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC2354.2
031400     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2354.2
031500     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC2354.2
031600 BAIL-OUT-EX. EXIT.                                               NC2354.2
031700 CCVS1-EXIT.                                                      NC2354.2
031800     EXIT.                                                        NC2354.2
031900 SECT-NC235A-001 SECTION.                                         NC2354.2
032000 TH-08-001.                                                       NC2354.2
032100 INIT-TBL-TH309.                                                  NC2354.2
032200     MOVE "ZZYYXXWWVVUUTTSSRRQQPPOONNMMLLKKJJIIHHGGFFEEDDCCBBAA"  NC2354.2
032300         TO TBL-TH309.                                            NC2354.2
032400     IF FIRSTZ (1)               AND LASTA (26)                   NC2354.2
032500         MOVE "26 ENTRY TABLE CONSTRUCTED " TO RE-MARK            NC2354.2
032600         GO TO INIT-WRITE.                                        NC2354.2
032700     MOVE "TBL ENTRIES BUILT INCORRECT" TO RE-MARK.               NC2354.2
032800     MOVE "*****" TO CORRECT-A  COMPUTED-A.                       NC2354.2
032900 INIT-WRITE.                                                      NC2354.2
033000     MOVE "INIT-TBL-TH309" TO PAR-NAME.                           NC2354.2
033100     MOVE "LEVEL 3 TBL HANDLING" TO FEATURE.                      NC2354.2
033200     PERFORM PRINT-DETAIL.                                        NC2354.2
033300*                                                                 NC2354.2
033400 IDX-INIT-F2-1.                                                   NC2354.2
033500     MOVE "IDX-TEST-F2-1 " TO PAR-NAME.                           NC2354.2
033600     MOVE "VI-121 6.21.2" TO ANSI-REFERENCE.                      NC2354.2
033700     MOVE "LEVEL 3 TBL HANDLING" TO FEATURE.                      NC2354.2
033800     SET IDX-2 TO 26.                                             NC2354.2
033900 IDX-TEST-F2-1.                                                   NC2354.2
034000     SEARCH ALL TH309-ENTRY AT END                                NC2354.2
034100          GO TO IDX-FAIL-F2-1                                     NC2354.2
034200      WHEN DEC-KEY (IDX-1) EQUAL TO "BB" NEXT SENTENCE.           NC2354.2
034300     PERFORM PASS.                                                NC2354.2
034400     GO TO IDX-WRITE-F2-1.                                        NC2354.2
034500 IDX-DELETE-F2-1.                                                 NC2354.2
034600     PERFORM DE-LETE                                              NC2354.2
034700     GO TO IDX-WRITE-F2-1.                                        NC2354.2
034800 IDX-FAIL-F2-1.                                                   NC2354.2
034900     MOVE 25 TO SUB-1                                             NC2354.2
035000     MOVE "BB" TO CORRECT-A                                       NC2354.2
035100     PERFORM PUTOUT-COMPUTED-A.                                   NC2354.2
035200     PERFORM FAIL.                                                NC2354.2
035300 IDX-WRITE-F2-1.                                                  NC2354.2
035400     PERFORM PRINT-DETAIL.                                        NC2354.2
035500*                                                                 NC2354.2
035600 IDX-INIT-F2-2.                                                   NC2354.2
035700     MOVE "IDX-TEST-F2-2 " TO PAR-NAME.                           NC2354.2
035800     MOVE "VI-121 6.21.2" TO ANSI-REFERENCE.                      NC2354.2
035900     MOVE "LEVEL 3 TBL HANDLING" TO FEATURE.                      NC2354.2
036000     SET IDX-3 TO 01.                                             NC2354.2
036100 IDX-TEST-F2-2.                                                   NC2354.2
036200     SEARCH ALL TH309-ENTRY AT END                                NC2354.2
036300         GO TO IDX-FAIL-F2-2                                      NC2354.2
036400      WHEN DEC-KEY (IDX-1) EQUAL TO "XX"                          NC2354.2
036500         PERFORM PASS                                             NC2354.2
036600         GO TO IDX-WRITE-F2-2.                                    NC2354.2
036700 IDX-DELETE-F2-2.                                                 NC2354.2
036800     PERFORM DE-LETE.                                             NC2354.2
036900     GO TO IDX-WRITE-F2-2.                                        NC2354.2
037000 IDX-FAIL-F2-2.                                                   NC2354.2
037100     MOVE 03 TO SUB-1                                             NC2354.2
037200     MOVE "XX" TO CORRECT-A                                       NC2354.2
037300     PERFORM PUTOUT-COMPUTED-A.                                   NC2354.2
037400     PERFORM FAIL.                                                NC2354.2
037500 IDX-WRITE-F2-2.                                                  NC2354.2
037600     PERFORM PRINT-DETAIL.                                        NC2354.2
037700*                                                                 NC2354.2
037800 IDX-INIT-F2-3.                                                   NC2354.2
037900     MOVE "IDX-TEST-F2-3 " TO PAR-NAME.                           NC2354.2
038000     MOVE "VI-121 6.21.2" TO ANSI-REFERENCE.                      NC2354.2
038100     MOVE "LEVEL 3 TBL HANDLING" TO FEATURE.                      NC2354.2
038200     MOVE 25 TO TBL-LENGTH.                                       NC2354.2
038300 IDX-TEST-F2-3.                                                   NC2354.2
038400     SEARCH ALL TH309-ENTRY AT END                                NC2354.2
038500         PERFORM PASS                                             NC2354.2
038600         GO TO IDX-WRITE-F2-3                                     NC2354.2
038700      WHEN DEC-KEY (IDX-1) EQUAL TO "AA"                          NC2354.2
038800         GO TO IDX-FAIL-F2-3.                                     NC2354.2
038900 IDX-DELETE-F2-3.                                                 NC2354.2
039000     PERFORM DE-LETE.                                             NC2354.2
039100     GO TO IDX-WRITE-F2-3.                                        NC2354.2
039200 IDX-FAIL-F2-3.                                                   NC2354.2
039300     MOVE "ENTRY SHOULD NOT BE FOUND  " TO RE-MARK                NC2354.2
039400     MOVE "AA" TO COMPUTED-A                                      NC2354.2
039500     PERFORM FAIL.                                                NC2354.2
039600 IDX-WRITE-F2-3.                                                  NC2354.2
039700     PERFORM PRINT-DETAIL.                                        NC2354.2
039800*                                                                 NC2354.2
039900 IDX-INIT-F1-4.                                                   NC2354.2
040000     MOVE "IDX-TEST-F1-4" TO PAR-NAME.                            NC2354.2
040100     MOVE "VI-121 6.21.2" TO ANSI-REFERENCE.                      NC2354.2
040200     MOVE "LEVEL 3 TBL HANDLING" TO FEATURE.                      NC2354.2
040300     MOVE 24 TO TBL-LENGTH.                                       NC2354.2
040400     SET IDX-3 TO 01.                                             NC2354.2
040500 IDX-TEST-F1-4.                                                   NC2354.2
040600     SEARCH TH309-ENTRY VARYING IDX-3 AT END                      NC2354.2
040700         PERFORM PASS                                             NC2354.2
040800         GO TO IDX-WRITE-F1-4                                     NC2354.2
040900      WHEN DEC-KEY (IDX-3) EQUAL TO "BB"                          NC2354.2
041000         GO TO IDX-FAIL-F1-4.                                     NC2354.2
041100 IDX-DELETE-F1-4.                                                 NC2354.2
041200     PERFORM DE-LETE.                                             NC2354.2
041300     GO TO IDX-WRITE-F1-4.                                        NC2354.2
041400 IDX-FAIL-F1-4.                                                   NC2354.2
041500     MOVE "ENTRY SHOULD NOT BE FOUND  " TO RE-MARK                NC2354.2
041600     MOVE "BB" TO COMPUTED-A                                      NC2354.2
041700     PERFORM FAIL.                                                NC2354.2
041800 IDX-WRITE-F1-4.                                                  NC2354.2
041900     PERFORM PRINT-DETAIL.                                        NC2354.2
042000*                                                                 NC2354.2
042100 IDX-INIT-F1-5.                                                   NC2354.2
042200     MOVE "IDX-TEST-F1-5 " TO PAR-NAME.                           NC2354.2
042300     MOVE "VI-121 6.21.2" TO ANSI-REFERENCE.                      NC2354.2
042400     MOVE "LEVEL 3 TBL HANDLING" TO FEATURE.                      NC2354.2
042500     SET IDX-2 TO 01.                                             NC2354.2
042600 IDX-TEST-F1-5.                                                   NC2354.2
042700     SEARCH TH309-ENTRY VARYING IDX-2 AT END                      NC2354.2
042800         GO TO IDX-FAIL-F1-5                                      NC2354.2
042900     WHEN DEC-KEY (IDX-2) EQUAL TO "KK"                           NC2354.2
043000         PERFORM PASS                                             NC2354.2
043100         GO TO IDX-WRITE-F1-5.                                    NC2354.2
043200 IDX-DELETE-F1-5.                                                 NC2354.2
043300     PERFORM DE-LETE.                                             NC2354.2
043400     GO TO IDX-WRITE-F1-5.                                        NC2354.2
043500 IDX-FAIL-F1-5.                                                   NC2354.2
043600     MOVE 16 TO SUB-1                                             NC2354.2
043700     MOVE "KK" TO CORRECT-A                                       NC2354.2
043800     PERFORM PUTOUT-COMPUTED-A.                                   NC2354.2
043900     PERFORM FAIL.                                                NC2354.2
044000 IDX-WRITE-F1-5.                                                  NC2354.2
044100     PERFORM PRINT-DETAIL.                                        NC2354.2
044200*                                                                 NC2354.2
044300 IDX-INIT-F1-6.                                                   NC2354.2
044400     MOVE "IDX-TEST-F1-6 " TO PAR-NAME.                           NC2354.2
044500     MOVE "VI-121 6.21.2" TO ANSI-REFERENCE.                      NC2354.2
044600     MOVE "LEVEL 3 TBL HANDLING" TO FEATURE.                      NC2354.2
044700     MOVE 22 TO TBL-LENGTH.                                       NC2354.2
044800     SET IDX-1 TO 09.                                             NC2354.2
044900 IDX-TEST-F1-6.                                                   NC2354.2
045000     SEARCH TH309-ENTRY VARYING IDX-1 AT END                      NC2354.2
045100         PERFORM PASS                                             NC2354.2
045200         GO TO IDX-WRITE-F1-6                                     NC2354.2
045300     WHEN TH309-ENTRY (IDX-1) EQUAL TO "DD"                       NC2354.2
045400         GO TO IDX-FAIL-F1-6.                                     NC2354.2
045500 IDX-DELETE-F1-6.                                                 NC2354.2
045600     PERFORM DE-LETE.                                             NC2354.2
045700     GO TO IDX-WRITE-F1-6.                                        NC2354.2
045800 IDX-FAIL-F1-6.                                                   NC2354.2
045900     MOVE "ENTRY SHOULD NOT BE FOUND  " TO RE-MARK                NC2354.2
046000     MOVE "DD" TO COMPUTED-A                                      NC2354.2
046100     PERFORM FAIL.                                                NC2354.2
046200 IDX-WRITE-F1-6.                                                  NC2354.2
046300     PERFORM PRINT-DETAIL.                                        NC2354.2
046400*                                                                 NC2354.2
046500 IDX-INIT-F1-7.                                                   NC2354.2
046600     MOVE "IDX-TEST-F1-7 " TO PAR-NAME.                           NC2354.2
046700     MOVE "VI-121 6.21.2" TO ANSI-REFERENCE.                      NC2354.2
046800     MOVE "LEVEL 3 TBL HANDLING" TO FEATURE.                      NC2354.2
046900     MOVE 22 TO TBL-LENGTH.                                       NC2354.2
047000     SET IDX-3 TO 23.                                             NC2354.2
047100 IDX-TEST-F1-7.                                                   NC2354.2
047200     SEARCH TH309-ENTRY VARYING IDX-3 AT END                      NC2354.2
047300         PERFORM PASS                                             NC2354.2
047400         GO TO IDX-WRITE-F1-7                                     NC2354.2
047500     WHEN TH309-ENTRY (IDX-3) EQUAL TO "DD"                       NC2354.2
047600         GO TO IDX-FAIL-F1-7.                                     NC2354.2
047700 IDX-DELETE-F1-7.                                                 NC2354.2
047800     PERFORM DE-LETE.                                             NC2354.2
047900     GO TO IDX-WRITE-F1-7.                                        NC2354.2
048000 IDX-FAIL-F1-7.                                                   NC2354.2
048100     MOVE "ENTRY SHOULD NOT BE FOUND  " TO RE-MARK                NC2354.2
048200     MOVE "DD" TO COMPUTED-A                                      NC2354.2
048300     PERFORM FAIL.                                                NC2354.2
048400 IDX-WRITE-F1-7.                                                  NC2354.2
048500     PERFORM PRINT-DETAIL.                                        NC2354.2
048600*                                                                 NC2354.2
048700 IDX-INIT-F2-8.                                                   NC2354.2
048800     MOVE "IDX-TEST-F2-8 " TO PAR-NAME.                           NC2354.2
048900     MOVE "VI-121 6.21.2" TO ANSI-REFERENCE.                      NC2354.2
049000     MOVE "LEVEL 3 TBL HANDLING" TO FEATURE.                      NC2354.2
049100     MOVE 20 TO TBL-LENGTH.                                       NC2354.2
049200     SET IDX-2 TO 21.                                             NC2354.2
049300 IDX-TEST-F2-8.                                                   NC2354.2
049400     SEARCH ALL TH309-ENTRY AT END                                NC2354.2
049500         GO TO IDX-FAIL-F2-8                                      NC2354.2
049600     WHEN DEC-KEY (IDX-1) EQUAL TO "GG"                           NC2354.2
049700         PERFORM PASS                                             NC2354.2
049800         GO TO IDX-WRITE-F2-8.                                    NC2354.2
049900 IDX-DELETE-F2-8.                                                 NC2354.2
050000     PERFORM DE-LETE.                                             NC2354.2
050100     GO TO IDX-WRITE-F2-8.                                        NC2354.2
050200 IDX-FAIL-F2-8.                                                   NC2354.2
050300     MOVE 20 TO SUB-1                                             NC2354.2
050400     MOVE "GG" TO CORRECT-A                                       NC2354.2
050500     PERFORM PUTOUT-COMPUTED-A.                                   NC2354.2
050600     PERFORM FAIL.                                                NC2354.2
050700 IDX-WRITE-F2-8.                                                  NC2354.2
050800     PERFORM PRINT-DETAIL.                                        NC2354.2
050900*                                                                 NC2354.2
051000 IDX-INIT-F2-9.                                                   NC2354.2
051100     MOVE "IDX-TEST-F2-9 " TO PAR-NAME.                           NC2354.2
051200     MOVE "VI-121 6.21.2" TO ANSI-REFERENCE.                      NC2354.2
051300     MOVE "LEVEL 3 TBL HANDLING" TO FEATURE.                      NC2354.2
051400     MOVE 20 TO TBL-LENGTH.                                       NC2354.2
051500 IDX-TEST-F2-9.                                                   NC2354.2
051600     SEARCH ALL TH309-ENTRY AT END                                NC2354.2
051700         PERFORM PASS                                             NC2354.2
051800         GO TO IDX-WRITE-F2-9                                     NC2354.2
051900     WHEN   LASTA (IDX-1)                                         NC2354.2
052000         GO TO IDX-FAIL-F2-9.                                     NC2354.2
052100 IDX-DELETE-F2-9.                                                 NC2354.2
052200     PERFORM DE-LETE.                                             NC2354.2
052300     GO TO IDX-WRITE-F2-9.                                        NC2354.2
052400 IDX-FAIL-F2-9.                                                   NC2354.2
052500     MOVE "CONDITION-NAME TEST" TO RE-MARK                        NC2354.2
052600     PERFORM FAIL                                                 NC2354.2
052700     MOVE "AA" TO COMPUTED-A.                                     NC2354.2
052800 IDX-WRITE-F2-9.                                                  NC2354.2
052900     PERFORM PRINT-DETAIL.                                        NC2354.2
053000*                                                                 NC2354.2
053100 IDX-INIT-F2-10.                                                  NC2354.2
053200     MOVE "IDX-TEST-F2-10 " TO PAR-NAME.                          NC2354.2
053300     MOVE "VI-121 6.21.2" TO ANSI-REFERENCE.                      NC2354.2
053400     MOVE "LEVEL 3 TBL HANDLING" TO FEATURE.                      NC2354.2
053500     SET IDX-1, IDX-2, IDX-3 TO 10.                               NC2354.2
053600 IDX-TEST-F2-10.                                                  NC2354.2
053700     SEARCH ALL TH309-ENTRY AT END                                NC2354.2
053800         GO TO IDX-FAIL-F2-10                                     NC2354.2
053900     WHEN DEC-KEY (IDX-1) EQUAL TO "RR"                           NC2354.2
054000         PERFORM PASS                                             NC2354.2
054100         GO TO IDX-WRITE-F2-10.                                   NC2354.2
054200 IDX-DELETE-F2-10.                                                NC2354.2
054300     PERFORM DE-LETE.                                             NC2354.2
054400     GO TO IDX-WRITE-F2-10.                                       NC2354.2
054500 IDX-FAIL-F2-10.                                                  NC2354.2
054600     MOVE 9 TO SUB-1                                              NC2354.2
054700     MOVE "RR" TO CORRECT-A                                       NC2354.2
054800     PERFORM PUTOUT-COMPUTED-A.                                   NC2354.2
054900     PERFORM FAIL.                                                NC2354.2
055000 IDX-WRITE-F2-10.                                                 NC2354.2
055100     PERFORM PRINT-DETAIL.                                        NC2354.2
055200*                                                                 NC2354.2
055300 IDX-INIT-F2-11.                                                  NC2354.2
055400     MOVE "IDX-TEST-F2-11 " TO PAR-NAME.                          NC2354.2
055500     MOVE "VI-121 6.21.2" TO ANSI-REFERENCE.                      NC2354.2
055600     MOVE "LEVEL 3 TBL HANDLING" TO FEATURE.                      NC2354.2
055700     MOVE 1 TO TBL-LENGTH.                                        NC2354.2
055800 IDX-TEST-F2-11.                                                  NC2354.2
055900     SEARCH ALL TH309-ENTRY AT END                                NC2354.2
056000         PERFORM PASS                                             NC2354.2
056100         GO TO IDX-WRITE-F2-11                                    NC2354.2
056200     WHEN DEC-KEY (IDX-1) EQUAL TO "YY"                           NC2354.2
056300         GO TO IDX-FAIL-F2-11.                                    NC2354.2
056400 IDX-DELETE-F2-11.                                                NC2354.2
056500     PERFORM DE-LETE.                                             NC2354.2
056600     GO TO IDX-WRITE-F2-11.                                       NC2354.2
056700 IDX-FAIL-F2-11.                                                  NC2354.2
056800     MOVE 2 TO SUB-1                                              NC2354.2
056900     MOVE "YY" TO COMPUTED-A                                      NC2354.2
057000     MOVE "ENTRY SHOULD NOT BE FOUND" TO RE-MARK                  NC2354.2
057100     PERFORM FAIL.                                                NC2354.2
057200 IDX-WRITE-F2-11.                                                 NC2354.2
057300     PERFORM PRINT-DETAIL.                                        NC2354.2
057400*                                                                 NC2354.2
057500 IDX-INIT-F2-12.                                                  NC2354.2
057600     MOVE "IDX-TEST-F2-12 " TO PAR-NAME.                          NC2354.2
057700     MOVE "VI-121 6.21.2" TO ANSI-REFERENCE.                      NC2354.2
057800     MOVE "LEVEL 3 TBL HANDLING" TO FEATURE.                      NC2354.2
057900     MOVE 10 TO TBL-LENGTH.                                       NC2354.2
058000 IDX-TEST-F2-12.                                                  NC2354.2
058100     SEARCH ALL TH309-ENTRY AT END                                NC2354.2
058200         PERFORM PASS                                             NC2354.2
058300         GO TO IDX-WRITE-F2-12                                    NC2354.2
058400     WHEN MIDDLE-PP (IDX-1)                                       NC2354.2
058500         GO TO IDX-FAIL-F2-12.                                    NC2354.2
058600 IDX-DELETE-F2-12.                                                NC2354.2
058700     PERFORM DE-LETE.                                             NC2354.2
058800     GO TO IDX-WRITE-F2-12.                                       NC2354.2
058900 IDX-FAIL-F2-12.                                                  NC2354.2
059000     MOVE 10 TO SUB-1                                             NC2354.2
059100     MOVE "ENTRY SHOULD NOT BE FOUND " TO RE-MARK                 NC2354.2
059200     MOVE "PP" TO COMPUTED-A                                      NC2354.2
059300     PERFORM FAIL.                                                NC2354.2
059400 IDX-WRITE-F2-12.                                                 NC2354.2
059500     PERFORM PRINT-DETAIL.                                        NC2354.2
059600*                                                                 NC2354.2
059700 IDX-INIT-F2-13.                                                  NC2354.2
059800     MOVE "IDX-TEST-F2-13 " TO PAR-NAME.                          NC2354.2
059900     MOVE "VI-121 6.21.2" TO ANSI-REFERENCE.                      NC2354.2
060000     MOVE "LEVEL 3 TBL HANDLING" TO FEATURE.                      NC2354.2
060100     MOVE 2 TO TBL-LENGTH.                                        NC2354.2
060200 IDX-TEST-F2-13.                                                  NC2354.2
060300     SEARCH ALL TH309-ENTRY AT END                                NC2354.2
060400         PERFORM PASS                                             NC2354.2
060500         GO TO IDX-WRITE-F2-13                                    NC2354.2
060600     WHEN DEC-KEY (IDX-1) EQUAL TO "XX"                           NC2354.2
060700         GO TO IDX-FAIL-F2-13.                                    NC2354.2
060800 IDX-DELETE-F2-13.                                                NC2354.2
060900     PERFORM DE-LETE.                                             NC2354.2
061000     GO TO IDX-WRITE-F2-13.                                       NC2354.2
061100 IDX-FAIL-F2-13.                                                  NC2354.2
061200     MOVE "XX" TO COMPUTED-A                                      NC2354.2
061300     MOVE "ENTRY SHOULD NOT BE FOUND " TO RE-MARK                 NC2354.2
061400     PERFORM FAIL.                                                NC2354.2
061500 IDX-WRITE-F2-13.                                                 NC2354.2
061600     PERFORM PRINT-DETAIL.                                        NC2354.2
061700     GO TO CCVS-EXIT.                                             NC2354.2
061800*                                                                 NC2354.2
061900 PUTOUT-COMPUTED-A.                                               NC2354.2
062000     IF TH309-ENTRY (SUB-1) EQUAL TO CORRECT-A                    NC2354.2
062100         MOVE "SEE NOTE 1 FOR DIAGNOSTIC" TO RE-MARK ELSE         NC2354.2
062200     MOVE "SEE NOTE 2 FOR DIAGNOSTIC" TO RE-MARK.                 NC2354.2
062300     MOVE TH309-ENTRY (SUB-1) TO COMPUTED-A.                      NC2354.2
062400 CCVS-EXIT SECTION.                                               NC2354.2
062500 CCVS-999999.                                                     NC2354.2
062600     GO TO CLOSE-FILES.                                           NC2354.2
*END-OF,NC235A                                                                  