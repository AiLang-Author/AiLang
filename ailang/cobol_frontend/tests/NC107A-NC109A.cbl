*HEADER,COBOL,NC107A                                                            
000100 IDENTIFICATION DIVISION.                                         NC1074.2
000200 PROGRAM-ID.                                                      NC1074.2
000300     NC107A.                                                      NC1074.2
000400****************************************************************  NC1074.2
000500*                                                              *  NC1074.2
000600*    VALIDATION FOR:-                                          *  NC1074.2
000700*                                                              *  NC1074.2
000800*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC1074.2
000900*                                                              *  NC1074.2
001000*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC1074.2
001100*                                                              *  NC1074.2
001200****************************************************************  NC1074.2
001300*                                                              *  NC1074.2
001400*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  NC1074.2
001500*                                                              *  NC1074.2
001600*        X-55  - SYSTEM PRINTER NAME.                          *  NC1074.2
001700*        X-82  - SOURCE COMPUTER NAME.                         *  NC1074.2
001800*        X-83  - OBJECT COMPUTER NAME.                         *  NC1074.2
001900*                                                              *  NC1074.2
002000****************************************************************  NC1074.2
002100*                                                                 NC1074.2
002200*    PROGRAM NC107A TESTS THE FOLLOWING FEATURES:                 NC1074.2
002300*                                                                 NC1074.2
002400*                  FIGURATIVE CONSTANTS                           NC1074.2
002500*                  CONTINUATION LINES                             NC1074.2
002600*                  SEPARATORS                                     NC1074.2
002700*                  JUSTIFIED CLAUSE                               NC1074.2
002800*                  SYNCHRONISED CLAUSE                            NC1074.2
002900*                  BLANK WHEN ZERO CLAUSE                         NC1074.2
003000*                  MAXIMUM LENGTH DATA-NAMES, LITERALS            NC1074.2
003100*                    AND PARAGRAPH-NAMES.                         NC1074.2
003200*                  REDEFINES CLAUSE                               NC1074.2
003300*                  USAGE CLAUSE                                   NC1074.2
003400*                  VALUE CLAUSE                                   NC1074.2
003500*                  CURRENCY SIGN CLAUSE                           NC1074.2
003600*                  DECIMAL-POINT IS COMMA CLAUSE                  NC1074.2
003700*                  NUMERIC PARAGRAPH NAMES                        NC1074.2
003800*                  CONTINUE STATEMENT                             NC1074.2
003900                                                                  NC1074.2
004000 ENVIRONMENT DIVISION.                                            NC1074.2
004100 CONFIGURATION SECTION.                                           NC1074.2
004200 SOURCE-COMPUTER.                                                 NC1074.2
004300     XXXXX082.                                                    NC1074.2
004400 OBJECT-COMPUTER.                                                 NC1074.2
004500     XXXXX083.                                                    NC1074.2
004600 SPECIAL-NAMES.                                                   NC1074.2
004700     CURRENCY SIGN IS "W"                                         NC1074.2
004800     DECIMAL-POINT IS COMMA.                                      NC1074.2
004900 INPUT-OUTPUT SECTION.                                            NC1074.2
005000 FILE-CONTROL.                                                    NC1074.2
005100     SELECT PRINT-FILE ASSIGN TO                                  NC1074.2
005200     XXXXX055.                                                    NC1074.2
005300 DATA DIVISION.                                                   NC1074.2
005400 FILE SECTION.                                                    NC1074.2
005500 FD  PRINT-FILE.                                                  NC1074.2
005600 01  PRINT-REC PICTURE X(120).                                    NC1074.2
005700 01  DUMMY-RECORD PICTURE X(120).                                 NC1074.2
005800 WORKING-STORAGE SECTION.                                         NC1074.2
005900 01  SUB1   PIC S9(3) COMP.                                       NC1074.2
006000 01  SUB2   PIC S9(3) COMP.                                       NC1074.2
006100 01  TAB-LOC.                                                     NC1074.2
006200   03  FILLER       PIC X(16)  VALUE "TABLE LOCATION: ".          NC1074.2
006300   03  TAB1         PIC ZZ9.                                      NC1074.2
006400   03  FILLER       PIC XX VALUE ", ".                            NC1074.2
006500   03  TAB2         PIC ZZ9.                                      NC1074.2
006600 77  DATA-A                             PICTURE IS X(10).         NC1074.2
006700 77  DATA-B                             PICTURE IS 9(5).          NC1074.2
006800 77  DATA-C                             PICTURE IS 9(5).          NC1074.2
006900 77  DATA-D                             PICTURE IS X(10)          NC1074.2
007000     JUSTIFIED RIGHT.                                             NC1074.2
007100 77  DATA-E                             PICTURE IS A(9)           NC1074.2
007200     JUSTIFIED.                                                   NC1074.2
007300 77  DATA-F                             PICTURE IS 9(10)          NC1074.2
007400     BLANK WHEN ZERO.                                             NC1074.2
007500 77  DATA-G  SYNCHRONIZED RIGHT   PICTURE X(5)                    NC1074.2
007600     VALUE IS "VWXYZ".                                            NC1074.2
007700 77  DATA-H                             PICTURE IS X(5)           NC1074.2
007800     VALUE IS "VWXYZ".                                            NC1074.2
007900 77  DATA-I                             PICTURE IS 9999           NC1074.2
008000     VALUE IS 12.                                                 NC1074.2
008100 77  DATA-J                             PICTURE IS WWWWW.         NC1074.2
008200 77  DATA-K                             PICTURE IS 9999999V99     NC1074.2
008300     VALUE IS 1234567,89.                                         NC1074.2
008400 77  DATA-L                             PICTURE IS 9.999.999,99.  NC1074.2
008500 77  DATA-M                       PICTURE IS W9999                NC1074.2
008600     BLANK WHEN ZERO.                                             NC1074.2
008700 77  DATA-N                             PICTURE IS X(16)          NC1074.2
008800     VALUE IS "4 SPACES ON LEFT".                                 NC1074.2
008900 77  DATA-O                             PICTURE IS X(20)          NC1074.2
009000     JUSTIFIED RIGHT.                                             NC1074.2
009100 77  DATA-P PICTURE 999 VALUE "000" BLANK WHEN ZERO.              NC1074.2
009200 77  DATA-P1 REDEFINES DATA-P PICTURE XXX.                        NC1074.2
009300 77  DATA-Q                               VALUE "QUOTE IN COL. 72"NC1074.2
009400     PICTURE X(16).                                               NC1074.2
009500 77  DATA-R                              VALUE "LITERAL ENDS AT 72NC1074.2
009600-    ""                                                           NC1074.2
009700     PICTURE X(18).                                               NC1074.2
009800 77  DATA-S             PICTURE X(20) VALUE               "OFFSET NC1074.2
009900-             "CONTINUATION ".                                    NC1074.2
010000 77  DATA-T             PICTURE X(20) VALUE  "OFFSET CONTINUATION NC1074.2
010100-                                             "".                 NC1074.2
010200 77  DATA-U             PICTURE X(20) VALUE                   "OFFNC1074.2
010300-                                                             "SETNC1074.2
010400-                                                             " CONC1074.2
010500-                                                             "NTINC1074.2
010600-                                                             "NUANC1074.2
010700-                                                               "TNC1074.2
010800-                                                             "IONNC1074.2
010900-                                                             " ".NC1074.2
011000 77  DATA-V             PICTURE X(20) VALUE SPACE.                NC1074.2
011100 77  DATA-W             PICTURE X(20) VALUE                       NC1074.2
011200     "OFFSET CONTINUATION ".                                      NC1074.2
011300 77  NUM-UTILITY        PICTURE 9999                              NC1074.2
011400     VALUE ZERO.                                                  NC1074.2
011500 01  WRK-XN-160-1 PIC X(160) VALUE                                NC1074.2
011600     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""NC1074.2
011700-    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""NC1074.2
011800-    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""NC1074.2
011900-    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""NC1074.2
012000-    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""NC1074.2
012100-    """""""""""""""""""""".                                      NC1074.2
012200 01  CHARACTER-BREAKDOWN-R.                                       NC1074.2
012300     02  FIRST-20R               PICTURE X(20).                   NC1074.2
012400     02  SECOND-20R              PICTURE X(20).                   NC1074.2
012500     02  THIRD-20R               PICTURE X(20).                   NC1074.2
012600     02  FOURTH-20R              PICTURE X(20).                   NC1074.2
012700 01  CHARACTER-BREAKDOWN-S.                                       NC1074.2
012800     02  FIRST-20S               PICTURE X(20).                   NC1074.2
012900     02  SECOND-20S              PICTURE X(20).                   NC1074.2
013000     02  THIRD-20S               PICTURE X(20).                   NC1074.2
013100     02  FOURTH-20S              PICTURE X(20).                   NC1074.2
013200 01  X80-CHARACTER-FIELD.                                         NC1074.2
013300     02  FILLER                  PICTURE X(80).                   NC1074.2
013400 01  A-DATA-NAME-30-CHARACTERS-LONG     PICTURE IS X.             NC1074.2
013500 01  LONG-PICTURE                       PICTURE IS                NC1074.2
013600      XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.                             NC1074.2
013700 01  LONG-NUMBER        PICTURE 999999999V999999999               NC1074.2
013800     VALUE IS 211113411,114311112.                                NC1074.2
013900 01  LONG-LITERAL.                                                NC1074.2
014000     02 LONG20                          PICTURE IS X(20)          NC1074.2
014100     VALUE IS "STANDARD COMPILERS M".                             NC1074.2
014200     02 LONG40                          PICTURE IS X(20)          NC1074.2
014300     VALUE IS "UST ALLOW NON-NUMERI".                             NC1074.2
014400     02 LONG60                          PICTURE IS X(20)          NC1074.2
014500     VALUE IS "C LITERALS OF AT LEA".                             NC1074.2
014600     02 LONG80                          PICTURE IS X(20)          NC1074.2
014700     VALUE IS "ST 120 CHARACTERS AN".                             NC1074.2
014800     02 LONG100                         PICTURE IS X(20)          NC1074.2
014900     VALUE IS "D NUMERIC LITERALS O".                             NC1074.2
015000     02 LONG120                         PICTURE IS X(20)          NC1074.2
015100     VALUE IS "F AT LEAST 18 DIGITS".                             NC1074.2
015200     02 LONG140                         PICTURE IS X(20)          NC1074.2
015300     VALUE IS " BUT NOW EXTENDED UP".                             NC1074.2
015400     02 LONG160                         PICTURE IS X(20)          NC1074.2
015500     VALUE IS "TO 160 DIGITS FOR 8X".                             NC1074.2
015600 01  LONG-PICTURE-A     PICTURE X(000000000000000020).            NC1074.2
015700 01  LONG-PICTURE-B     PICTURE X(15) JUSTIFIED RIGHT.            NC1074.2
015800 01  LONG-PICTURE-C     PICTURE X(000000000000000010).            NC1074.2
015900 01  REDEF1                             PICTURE IS 9 VALUE IS 9.  NC1074.2
016000 01  REDEF2 REDEFINES REDEF1            PICTURE IS X.             NC1074.2
016100 01  REDEF3                             PICTURE IS XXX            NC1074.2
016200     VALUE IS "ABC".                                              NC1074.2
016300 01  REDEF4 REDEFINES REDEF3            PICTURE IS A.             NC1074.2
016400 01  REDEF5                             PICTURE IS X(6)           NC1074.2
016500     VALUE IS "UVWXYZ".                                           NC1074.2
016600 01  REDEF6 REDEFINES REDEF5            PICTURE IS 9(6).          NC1074.2
016700 01  REDEF7 REDEFINES REDEF5            PICTURE IS A(6).          NC1074.2
016800 01  REDEF8 REDEFINES REDEF5.                                     NC1074.2
016900     02 REDEF8X.                                                  NC1074.2
017000     03 REDEF8A                         PICTURE IS XX.            NC1074.2
017100     03 REDEF8B                         PICTURE IS 99.            NC1074.2
017200     02 REDEF8C                         PICTURE IS AA.            NC1074.2
017300 01  REDEF9 REDEFINES REDEF5            PICTURE IS X(6).          NC1074.2
017400 01  REDEF10.                                                     NC1074.2
017500     02  RDFDATA1                PICTURE X(10) VALUE "ABC98765DE".NC1074.2
017600     02 RDFDATA2 PIC 9(4)V99 VALUE 9116,44.                       NC1074.2
017700     02  RDFDATA3.                                                NC1074.2
017800         08  RDFDATA4            PICTURE X(6)  VALUE "ALLDON".    NC1074.2
017900         08  RDFDATA5            PICTURE XX99  VALUE "XX66".      NC1074.2
018000     02  RDFDATA6                PICTURE A(20) VALUE              NC1074.2
018100                                                                  NC1074.2
018200     "ZYXWVUTSRQPONMLKJIHG".                                      NC1074.2
018300 01  REDEF11 REDEFINES REDEF10.                                   NC1074.2
018400     02  RDFDATA7                PICTURE X(20).                   NC1074.2
018500     02  RDF8.                                                    NC1074.2
018600         03   RDFDATA8 OCCURS 36 TIMES PICTURE XX.                NC1074.2
018700 01  REDEF12 REDEFINES REDEF10.                                   NC1074.2
018800     02  RDFDATA9                PICTURE A(3).                    NC1074.2
018900     02  RDFDATA10               PICTURE 9(5).                    NC1074.2
019000     02  RDFDATA11.                                               NC1074.2
019100         03  RDFDATA12.                                           NC1074.2
019200             04  RDFDATA13       PICTURE XX.                      NC1074.2
019300             04  RDFDATA14 OCCURS 6 TIMES PICTURE 9.              NC1074.2
019400         03  RDFDATA15           PICTURE X(8).                    NC1074.2
019500     02  RDFDATA16               PICTURE 99.                      NC1074.2
019600     02  RDFDATA17               PICTURE X(80).                   NC1074.2
019700     02  RDFDATA18               PICTURE X(14).                   NC1074.2
019800 01  REDEF13.                                                     NC1074.2
019900     02  FILLER                  PICTURE X(57)   VALUE            NC1074.2
020000     "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA". NC1074.2
020100     02  FILLER                  PICTURE X(57)   VALUE            NC1074.2
020200     "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA". NC1074.2
020300     02  FILLER                  PICTURE X(6)    VALUE "AAAAAA".  NC1074.2
020400 01  REDEF20.                                                     NC1074.2
020500   03  REDEF21                   PICTURE X(10).                   NC1074.2
020600   03  REDEF22                   REDEFINES REDEF21                NC1074.2
020700                                 PIC     X(10).                   NC1074.2
020800   03  REDEF23                   REDEFINES REDEF21                NC1074.2
020900                                 PIC     X(9).                    NC1074.2
021000 01  U1.                                                          NC1074.2
021100     02 U2   PICTURE 9 USAGE IS                                   NC1074.2
021200     DISPLAY VALUE IS 9.                                          NC1074.2
021300     02 U3                              PICTURE IS 9 USAGE IS     NC1074.2
021400     COMPUTATIONAL VALUE IS 9.                                    NC1074.2
021500     02 U4                              PICTURE IS 9 USAGE IS     NC1074.2
021600     DISPLAY VALUE IS 9.                                          NC1074.2
021700     02 U5 USAGE IS COMPUTATIONAL.                                NC1074.2
021800     03 U6                              PICTURE IS 9 USAGE IS     NC1074.2
021900     COMPUTATIONAL VALUE IS 5.                                    NC1074.2
022000     03 U7                              PICTURE IS 9 VALUE IS 6.  NC1074.2
022100     02 U8                              PICTURE IS X.             NC1074.2
022200 01  U9                           USAGE COMPUTATIONAL.            NC1074.2
022300     02 U10             PICTURE 9.                                NC1074.2
022400     02 U11             PICTURE 9  COMPUTATIONAL.                 NC1074.2
022500 01  U12.                                                         NC1074.2
022600     02 U13             PICTURE 9 USAGE IS BINARY                 NC1074.2
022700                                  VALUE 3.                        NC1074.2
022800     02 U14             PICTURE 9 USAGE IS BINARY                 NC1074.2
022900                                  VALUE 3.                        NC1074.2
023000 01  U22.                                                         NC1074.2
023100     02 U23             PICTURE 9 USAGE IS BINARY                 NC1074.2
023200                                  VALUE 4.                        NC1074.2
023300     02 U24             PICTURE 9 USAGE IS BINARY                 NC1074.2
023400                                  VALUE 4.                        NC1074.2
023500*                                                                 NC1074.2
023600*    TWO-DIMENSIONAL TABLE USED IN VALUE CLAUSE:                  NC1074.2
023700*                                                                 NC1074.2
023800 01  VALUE-TABLE.                                                 NC1074.2
023900   03  VALUE-TABLE-1              OCCURS 10.                      NC1074.2
024000     05  VALUE-TABLE-2            OCCURS 10                       NC1074.2
024100                                  PIC XX   VALUE "AZ".            NC1074.2
024200*                                                                 NC1074.2
024300*                                                                 NC1074.2
024400 01  TEST-FIELD                   PIC X(10).                      NC1074.2
024500*                                                                 NC1074.2
024600*                                                                 NC1074.2
024700 01  SEP-01. 02 SEP-02. 03 SEP-03. 04 SEP-04 PICTURE X(9) VALUE   NC1074.2
024800     "SEPARATOR".                                                 NC1074.2
024900*                                                                 NC1074.2
025000*    GROUP ITEMS USED IN JUSTIFIED TESTS.                         NC1074.2
025100*                                                                 NC1074.2
025200 01  GROUP-TO-JUST-1.                                             NC1074.2
025300     02  FILLER          PICTURE X   VALUE "A".                   NC1074.2
025400     02  FILLER          PICTURE X   VALUE "B".                   NC1074.2
025500     02  FILLER          PICTURE X   VALUE "C".                   NC1074.2
025600 01  GROUP-TO-JUST-2.                                             NC1074.2
025700     02  GROUP-TO-JUST-21.                                        NC1074.2
025800         03  FILLER      PICTURE X(5)    VALUE "ABCDE".           NC1074.2
025900         03  FILLER      PICTURE X(2)    VALUE "FG".              NC1074.2
026000     02  FILLER          PICTURE X(8)    VALUE "HIJKLMNO".        NC1074.2
026100 01  GROUP-FOR-JUST-TESTS.                                        NC1074.2
026200     02  NJUST-XN-3      PICTURE X(3)    VALUE "ABC".             NC1074.2
026300     02  NJUST-XN-5      PICTURE X(5)    VALUE "CDEFG".           NC1074.2
026400     02  NJUST-XN-15     PICTURE X(15)   VALUE "ABCDEFGHIJKLMNO". NC1074.2
026500*                                                                 NC1074.2
026600*    DATA ITEMS WITH JUSTIFIED CLAUSE.                            NC1074.2
026700*                                                                 NC1074.2
026800 01  XJ-00005            PICTURE X(5)    JUSTIFIED RIGHT.         NC1074.2
026900 01  AJ-00005            PICTURE A(5)    JUSTIFIED RIGHT.         NC1074.2
027000 01  XJ-00007            PICTURE X(7)    JUST RIGHT.              NC1074.2
027100 01  AJ-00007            PICTURE A(7)    JUSTIFIED.               NC1074.2
027200 01  GROUP-WITH-JUST-ITEMS.                                       NC1074.2
027300     02  XN-00005-NJUST      PICTURE X(5).                        NC1074.2
027400     02  XJ-00009            PICTURE X(9)    JUST.                NC1074.2
027500     02  AJ-00009            PICTURE A(9)    JUST.                NC1074.2
027600*                                                                 NC1074.2
027700*    INITIALIZATION TAKES PLACE INDEPENDENT OF ANY                NC1074.2
027800*    JUSTIFIED CLAUSE.                                            NC1074.2
027900*                                                                 NC1074.2
028000 01  XJ-00002   PICTURE X(2)   JUST  VALUE "AB".                  NC1074.2
028100 01  XJ-00003            PICTURE X(3)        JUST   VALUE "XY".   NC1074.2
028200 01  TEST-RESULTS.                                                NC1074.2
028300     02 FILLER                   PIC X      VALUE SPACE.          NC1074.2
028400     02 FEATURE                  PIC X(20)  VALUE SPACE.          NC1074.2
028500     02 FILLER                   PIC X      VALUE SPACE.          NC1074.2
028600     02 P-OR-F                   PIC X(5)   VALUE SPACE.          NC1074.2
028700     02 FILLER                   PIC X      VALUE SPACE.          NC1074.2
028800     02  PAR-NAME.                                                NC1074.2
028900       03 FILLER                 PIC X(19)  VALUE SPACE.          NC1074.2
029000       03  PARDOT-X              PIC X      VALUE SPACE.          NC1074.2
029100       03 DOTVALUE               PIC 99     VALUE ZERO.           NC1074.2
029200     02 FILLER                   PIC X(8)   VALUE SPACE.          NC1074.2
029300     02 RE-MARK                  PIC X(61).                       NC1074.2
029400 01  TEST-COMPUTED.                                               NC1074.2
029500     02 FILLER                   PIC X(30)  VALUE SPACE.          NC1074.2
029600     02 FILLER                   PIC X(17)  VALUE                 NC1074.2
029700            "       COMPUTED=".                                   NC1074.2
029800     02 COMPUTED-X.                                               NC1074.2
029900     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          NC1074.2
030000     03 COMPUTED-N               REDEFINES COMPUTED-A             NC1074.2
030100                                 PIC -9(9),9(9).                  NC1074.2
030200     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -,9(18).         NC1074.2
030300     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4),9(14).     NC1074.2
030400     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14),9(4).     NC1074.2
030500     03       CM-18V0 REDEFINES COMPUTED-A.                       NC1074.2
030600         04 COMPUTED-18V0                    PIC -9(18).          NC1074.2
030700         04 FILLER                           PIC X.               NC1074.2
030800     03 FILLER PIC X(50) VALUE SPACE.                             NC1074.2
030900 01  TEST-CORRECT.                                                NC1074.2
031000     02 FILLER PIC X(30) VALUE SPACE.                             NC1074.2
031100     02 FILLER PIC X(17) VALUE "       CORRECT =".                NC1074.2
031200     02 CORRECT-X.                                                NC1074.2
031300     03 CORRECT-A                  PIC X(20) VALUE SPACE.         NC1074.2
031400     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9),9(9).      NC1074.2
031500     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -,9(18).         NC1074.2
031600     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4),9(14).     NC1074.2
031700     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14),9(4).     NC1074.2
031800     03      CR-18V0 REDEFINES CORRECT-A.                         NC1074.2
031900         04 CORRECT-18V0                     PIC -9(18).          NC1074.2
032000         04 FILLER                           PIC X.               NC1074.2
032100     03 FILLER PIC X(2) VALUE SPACE.                              NC1074.2
032200     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     NC1074.2
032300 01  CCVS-C-1.                                                    NC1074.2
032400     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PANC1074.2
032500-    "SS  PARAGRAPH-NAME                                          NC1074.2
032600-    "       REMARKS".                                            NC1074.2
032700     02 FILLER                     PIC X(20)    VALUE SPACE.      NC1074.2
032800 01  CCVS-C-2.                                                    NC1074.2
032900     02 FILLER                     PIC X        VALUE SPACE.      NC1074.2
033000     02 FILLER                     PIC X(6)     VALUE "TESTED".   NC1074.2
033100     02 FILLER                     PIC X(15)    VALUE SPACE.      NC1074.2
033200     02 FILLER                     PIC X(4)     VALUE "FAIL".     NC1074.2
033300     02 FILLER                     PIC X(94)    VALUE SPACE.      NC1074.2
033400 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       NC1074.2
033500 01  REC-CT                        PIC 99       VALUE ZERO.       NC1074.2
033600 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       NC1074.2
033700 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       NC1074.2
033800 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       NC1074.2
033900 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       NC1074.2
034000 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       NC1074.2
034100 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       NC1074.2
034200 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      NC1074.2
034300 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       NC1074.2
034400 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     NC1074.2
034500 01  CCVS-H-1.                                                    NC1074.2
034600     02  FILLER                    PIC X(39)    VALUE SPACES.     NC1074.2
034700     02  FILLER                    PIC X(42)    VALUE             NC1074.2
034800     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 NC1074.2
034900     02  FILLER                    PIC X(39)    VALUE SPACES.     NC1074.2
035000 01  CCVS-H-2A.                                                   NC1074.2
035100   02  FILLER                        PIC X(40)  VALUE SPACE.      NC1074.2
035200   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  NC1074.2
035300   02  FILLER                        PIC XXXX   VALUE             NC1074.2
035400     "4.2 ".                                                      NC1074.2
035500   02  FILLER                        PIC X(28)  VALUE             NC1074.2
035600            " COPY - NOT FOR DISTRIBUTION".                       NC1074.2
035700   02  FILLER                        PIC X(41)  VALUE SPACE.      NC1074.2
035800                                                                  NC1074.2
035900 01  CCVS-H-2B.                                                   NC1074.2
036000   02  FILLER                        PIC X(15)  VALUE             NC1074.2
036100            "TEST RESULT OF ".                                    NC1074.2
036200   02  TEST-ID                       PIC X(9).                    NC1074.2
036300   02  FILLER                        PIC X(4)   VALUE             NC1074.2
036400            " IN ".                                               NC1074.2
036500   02  FILLER                        PIC X(12)  VALUE             NC1074.2
036600     " HIGH       ".                                              NC1074.2
036700   02  FILLER                        PIC X(22)  VALUE             NC1074.2
036800            " LEVEL VALIDATION FOR ".                             NC1074.2
036900   02  FILLER                        PIC X(58)  VALUE             NC1074.2
037000     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC1074.2
037100 01  CCVS-H-3.                                                    NC1074.2
037200     02  FILLER                      PIC X(34)  VALUE             NC1074.2
037300            " FOR OFFICIAL USE ONLY    ".                         NC1074.2
037400     02  FILLER                      PIC X(58)  VALUE             NC1074.2
037500     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC1074.2
037600     02  FILLER                      PIC X(28)  VALUE             NC1074.2
037700            "  COPYRIGHT   1985 ".                                NC1074.2
037800 01  CCVS-E-1.                                                    NC1074.2
037900     02 FILLER                       PIC X(52)  VALUE SPACE.      NC1074.2
038000     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              NC1074.2
038100     02 ID-AGAIN                     PIC X(9).                    NC1074.2
038200     02 FILLER                       PIC X(45)  VALUE SPACES.     NC1074.2
038300 01  CCVS-E-2.                                                    NC1074.2
038400     02  FILLER                      PIC X(31)  VALUE SPACE.      NC1074.2
038500     02  FILLER                      PIC X(21)  VALUE SPACE.      NC1074.2
038600     02 CCVS-E-2-2.                                               NC1074.2
038700         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      NC1074.2
038800         03 FILLER                   PIC X      VALUE SPACE.      NC1074.2
038900         03 ENDER-DESC               PIC X(44)  VALUE             NC1074.2
039000            "ERRORS ENCOUNTERED".                                 NC1074.2
039100 01  CCVS-E-3.                                                    NC1074.2
039200     02  FILLER                      PIC X(22)  VALUE             NC1074.2
039300            " FOR OFFICIAL USE ONLY".                             NC1074.2
039400     02  FILLER                      PIC X(12)  VALUE SPACE.      NC1074.2
039500     02  FILLER                      PIC X(58)  VALUE             NC1074.2
039600     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC1074.2
039700     02  FILLER                      PIC X(13)  VALUE SPACE.      NC1074.2
039800     02 FILLER                       PIC X(15)  VALUE             NC1074.2
039900             " COPYRIGHT 1985".                                   NC1074.2
040000 01  CCVS-E-4.                                                    NC1074.2
040100     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      NC1074.2
040200     02 FILLER                       PIC X(4)   VALUE " OF ".     NC1074.2
040300     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      NC1074.2
040400     02 FILLER                       PIC X(40)  VALUE             NC1074.2
040500      "  TESTS WERE EXECUTED SUCCESSFULLY".                       NC1074.2
040600 01  XXINFO.                                                      NC1074.2
040700     02 FILLER                       PIC X(19)  VALUE             NC1074.2
040800            "*** INFORMATION ***".                                NC1074.2
040900     02 INFO-TEXT.                                                NC1074.2
041000       04 FILLER                     PIC X(8)   VALUE SPACE.      NC1074.2
041100       04 XXCOMPUTED                 PIC X(20).                   NC1074.2
041200       04 FILLER                     PIC X(5)   VALUE SPACE.      NC1074.2
041300       04 XXCORRECT                  PIC X(20).                   NC1074.2
041400     02 INF-ANSI-REFERENCE           PIC X(48).                   NC1074.2
041500 01  HYPHEN-LINE.                                                 NC1074.2
041600     02 FILLER  PIC IS X VALUE IS SPACE.                          NC1074.2
041700     02 FILLER  PIC IS X(65)    VALUE IS "************************NC1074.2
041800-    "*****************************************".                 NC1074.2
041900     02 FILLER  PIC IS X(54)    VALUE IS "************************NC1074.2
042000-    "******************************".                            NC1074.2
042100 01  CCVS-PGM-ID                     PIC X(9)   VALUE             NC1074.2
042200     "NC107A".                                                    NC1074.2
042300 PROCEDURE DIVISION.                                              NC1074.2
042400 CCVS1 SECTION.                                                   NC1074.2
042500 OPEN-FILES.                                                      NC1074.2
042600     OPEN     OUTPUT PRINT-FILE.                                  NC1074.2
042700     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   NC1074.2
042800     MOVE    SPACE TO TEST-RESULTS.                               NC1074.2
042900     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             NC1074.2
043000     GO TO CCVS1-EXIT.                                            NC1074.2
043100 CLOSE-FILES.                                                     NC1074.2
043200     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   NC1074.2
043300 TERMINATE-CCVS.                                                  NC1074.2
043400S    EXIT PROGRAM.                                                NC1074.2
043500STERMINATE-CALL.                                                  NC1074.2
043600     STOP     RUN.                                                NC1074.2
043700 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         NC1074.2
043800 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           NC1074.2
043900 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          NC1074.2
044000 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      NC1074.2
044100     MOVE "****TEST DELETED****" TO RE-MARK.                      NC1074.2
044200 PRINT-DETAIL.                                                    NC1074.2
044300     IF REC-CT NOT EQUAL TO ZERO                                  NC1074.2
044400             MOVE "." TO PARDOT-X                                 NC1074.2
044500             MOVE REC-CT TO DOTVALUE.                             NC1074.2
044600     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      NC1074.2
044700     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               NC1074.2
044800        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 NC1074.2
044900          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 NC1074.2
045000     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              NC1074.2
045100     MOVE SPACE TO CORRECT-X.                                     NC1074.2
045200     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         NC1074.2
045300     MOVE     SPACE TO RE-MARK.                                   NC1074.2
045400 HEAD-ROUTINE.                                                    NC1074.2
045500     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC1074.2
045600     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC1074.2
045700     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC1074.2
045800     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC1074.2
045900 COLUMN-NAMES-ROUTINE.                                            NC1074.2
046000     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC1074.2
046100     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC1074.2
046200     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        NC1074.2
046300 END-ROUTINE.                                                     NC1074.2
046400     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.NC1074.2
046500 END-RTN-EXIT.                                                    NC1074.2
046600     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC1074.2
046700 END-ROUTINE-1.                                                   NC1074.2
046800      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      NC1074.2
046900      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               NC1074.2
047000      ADD PASS-COUNTER TO ERROR-HOLD.                             NC1074.2
047100*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   NC1074.2
047200      MOVE PASS-COUNTER TO CCVS-E-4-1.                            NC1074.2
047300      MOVE ERROR-HOLD TO CCVS-E-4-2.                              NC1074.2
047400      MOVE CCVS-E-4 TO CCVS-E-2-2.                                NC1074.2
047500      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           NC1074.2
047600  END-ROUTINE-12.                                                 NC1074.2
047700      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        NC1074.2
047800     IF       ERROR-COUNTER IS EQUAL TO ZERO                      NC1074.2
047900         MOVE "NO " TO ERROR-TOTAL                                NC1074.2
048000         ELSE                                                     NC1074.2
048100         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       NC1074.2
048200     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           NC1074.2
048300     PERFORM WRITE-LINE.                                          NC1074.2
048400 END-ROUTINE-13.                                                  NC1074.2
048500     IF DELETE-COUNTER IS EQUAL TO ZERO                           NC1074.2
048600         MOVE "NO " TO ERROR-TOTAL  ELSE                          NC1074.2
048700         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      NC1074.2
048800     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   NC1074.2
048900     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC1074.2
049000      IF   INSPECT-COUNTER EQUAL TO ZERO                          NC1074.2
049100          MOVE "NO " TO ERROR-TOTAL                               NC1074.2
049200      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   NC1074.2
049300      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            NC1074.2
049400      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          NC1074.2
049500     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC1074.2
049600 WRITE-LINE.                                                      NC1074.2
049700     ADD 1 TO RECORD-COUNT.                                       NC1074.2
049800Y    IF RECORD-COUNT GREATER 42                                   NC1074.2
049900Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          NC1074.2
050000Y        MOVE SPACE TO DUMMY-RECORD                               NC1074.2
050100Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  NC1074.2
050200Y        MOVE CCVS-H-1  TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   NC1074.2
050300Y        MOVE CCVS-H-2A TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   NC1074.2
050400Y        MOVE CCVS-H-2B TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   NC1074.2
050500Y        MOVE CCVS-H-3  TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   NC1074.2
050600Y        MOVE CCVS-C-1  TO DUMMY-RECORD  PERFORM WRT-LN           NC1074.2
050700Y        MOVE CCVS-C-2  TO DUMMY-RECORD  PERFORM WRT-LN           NC1074.2
050800Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          NC1074.2
050900Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          NC1074.2
051000Y        MOVE ZERO TO RECORD-COUNT.                               NC1074.2
051100     PERFORM WRT-LN.                                              NC1074.2
051200 WRT-LN.                                                          NC1074.2
051300     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               NC1074.2
051400     MOVE SPACE TO DUMMY-RECORD.                                  NC1074.2
051500 BLANK-LINE-PRINT.                                                NC1074.2
051600     PERFORM WRT-LN.                                              NC1074.2
051700 FAIL-ROUTINE.                                                    NC1074.2
051800     IF     COMPUTED-X NOT EQUAL TO SPACE                         NC1074.2
051900            GO TO FAIL-ROUTINE-WRITE.                             NC1074.2
052000     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.NC1074.2
052100     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC1074.2
052200     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   NC1074.2
052300     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC1074.2
052400     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC1074.2
052500     GO TO  FAIL-ROUTINE-EX.                                      NC1074.2
052600 FAIL-ROUTINE-WRITE.                                              NC1074.2
052700     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         NC1074.2
052800     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 NC1074.2
052900     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. NC1074.2
053000     MOVE   SPACES TO COR-ANSI-REFERENCE.                         NC1074.2
053100 FAIL-ROUTINE-EX. EXIT.                                           NC1074.2
053200 BAIL-OUT.                                                        NC1074.2
053300     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   NC1074.2
053400     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           NC1074.2
053500 BAIL-OUT-WRITE.                                                  NC1074.2
053600     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  NC1074.2
053700     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC1074.2
053800     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC1074.2
053900     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC1074.2
054000 BAIL-OUT-EX. EXIT.                                               NC1074.2
054100 CCVS1-EXIT.                                                      NC1074.2
054200     EXIT.                                                        NC1074.2
054300 SECT-NC107A-001 SECTION.                                         NC1074.2
054400*REMARKS-TEST.                                                    NC1074.2
054500*    MOVE   "IV-11 7.2.4" TO ANSI-REFERENCE.                      NC1074.2
054600*    MOVE   "COBOL REMARKS PARA" TO FEATURE.                      NC1074.2
054700*    MOVE   "REMARKS" TO PAR-NAME.                                NC1074.2
054800*    MOVE     "PHONY LINES SHOULDNT EXECUT" TO RE-MARK.           NC1074.2
054900*    PERFORM  PRINT-DETAIL.                                       NC1074.2
055000*NOTE-TEST-1.                                                     NC1074.2
055100*    PERFORM FAIL.                                                NC1074.2
055200*       NOTE ENTER                         GO TO NOTE-WRITE-1     NC1074.2
055300*            USE                           GO TO NOTE-WRITE-1     NC1074.2
055400*            DECLARATIVES                  GO TO NOTE-WRITE-1     NC1074.2
055500*            DATA DIVISION                 GO TO NOTE-WRITE-1     NC1074.2
055600*            COPY (SEE ALSO PROGRAM LB104) GO TO NOTE-WRITE-1     NC1074.2
055700*            THE COMPILER SHOULD "IGNORE" THE ABOVE WORDS.        NC1074.2
055800*    PERFORM PASS                                                 NC1074.2
055900*    GO      TO NOTE-WRITE-1.                                     NC1074.2
056000*NOTE-DELETE-1.                                                   NC1074.2
056100*    PERFORM DE-LETE.                                             NC1074.2
056200 NOTE-WRITE-1.                                                    NC1074.2
056300     MOVE   "NOTE RESERVED WORDS" TO FEATURE.                     NC1074.2
056400     MOVE   "NOTE-TEST-1" TO PAR-NAME.                            NC1074.2
056500     PERFORM PRINT-DETAIL.                                        NC1074.2
056600 FIG-INIT.                                                        NC1074.2
056700     MOVE   "IV-11 4.2.2.2.3" TO ANSI-REFERENCE.                  NC1074.2
056800     MOVE     SPACE TO TEST-RESULTS.                              NC1074.2
056900     MOVE "PLEASE CHECK THE COMPUTED" TO RE-MARK.                 NC1074.2
057000     PERFORM  PRINT-DETAIL.                                       NC1074.2
057100     MOVE "COLUMN TO BE CERTAIN THAT" TO RE-MARK.                 NC1074.2
057200     PERFORM  PRINT-DETAIL.                                       NC1074.2
057300     MOVE "THE CORRECT VALUES FOR THE" TO RE-MARK.                NC1074.2
057400     PERFORM  PRINT-DETAIL.                                       NC1074.2
057500     MOVE "FIGURATIVE CONSTANTS ARE" TO RE-MARK.                  NC1074.2
057600     PERFORM  PRINT-DETAIL.                                       NC1074.2
057700     MOVE "SHOWN" TO RE-MARK.                                     NC1074.2
057800     PERFORM  PRINT-DETAIL.                                       NC1074.2
057900     MOVE     "FIGURATIVE CONSTANTS" TO FEATURE.                  NC1074.2
058000 FIG-TEST-1.                                                      NC1074.2
058100     MOVE     ZERO TO  COMPUTED-18V0.                             NC1074.2
058200     MOVE "ZERO      " TO CORRECT-A.                              NC1074.2
058300     PERFORM INSPT.                                               NC1074.2
058400     GO TO    FIG-WRITE-1.                                        NC1074.2
058500 FIG-DELETE-1.                                                    NC1074.2
058600     PERFORM  DE-LETE.                                            NC1074.2
058700 FIG-WRITE-1.                                                     NC1074.2
058800     MOVE "FIG-TEST-1" TO PAR-NAME.                               NC1074.2
058900     PERFORM  PRINT-DETAIL.                                       NC1074.2
059000 FIG-TEST-2.                                                      NC1074.2
059100     MOVE   "IV-11 4.2.2.2.3" TO ANSI-REFERENCE.                  NC1074.2
059200     MOVE     SPACE TO COMPUTED-A.                                NC1074.2
059300     MOVE "SPACE     " TO CORRECT-A.                              NC1074.2
059400     PERFORM INSPT.                                               NC1074.2
059500     GO TO    FIG-WRITE-2.                                        NC1074.2
059600 FIG-DELETE-2.                                                    NC1074.2
059700     PERFORM  DE-LETE.                                            NC1074.2
059800 FIG-WRITE-2.                                                     NC1074.2
059900     MOVE "FIG-TEST-2" TO PAR-NAME.                               NC1074.2
060000     PERFORM  PRINT-DETAIL.                                       NC1074.2
060100 FIG-TEST-3.                                                      NC1074.2
060200     MOVE   "IV-11 4.2.2.2.3" TO ANSI-REFERENCE.                  NC1074.2
060300     MOVE     QUOTE TO COMPUTED-A.                                NC1074.2
060400     MOVE "QUOTE     " TO CORRECT-A.                              NC1074.2
060500     PERFORM INSPT.                                               NC1074.2
060600     GO TO    FIG-WRITE-3.                                        NC1074.2
060700 FIG-DELETE-3.                                                    NC1074.2
060800     PERFORM  DE-LETE.                                            NC1074.2
060900 FIG-WRITE-3.                                                     NC1074.2
061000     MOVE "FIG-TEST-3" TO PAR-NAME.                               NC1074.2
061100     PERFORM  PRINT-DETAIL.                                       NC1074.2
061200 FIG-TEST-4.                                                      NC1074.2
061300     MOVE   "IV-11 4.2.2.2.3" TO ANSI-REFERENCE.                  NC1074.2
061400     MOVE     HIGH-VALUE TO COMPUTED-A.                           NC1074.2
061500     MOVE "HIGH-VALUE" TO CORRECT-A.                              NC1074.2
061600     PERFORM INSPT.                                               NC1074.2
061700     GO TO    FIG-WRITE-4.                                        NC1074.2
061800 FIG-DELETE-4.                                                    NC1074.2
061900     PERFORM  DE-LETE.                                            NC1074.2
062000 FIG-WRITE-4.                                                     NC1074.2
062100     MOVE "FIG-TEST-4" TO PAR-NAME.                               NC1074.2
062200     PERFORM  PRINT-DETAIL.                                       NC1074.2
062300 FIG-TEST-5.                                                      NC1074.2
062400     MOVE   "IV-11 4.2.2.2.3" TO ANSI-REFERENCE.                  NC1074.2
062500     MOVE     LOW-VALUE TO COMPUTED-A.                            NC1074.2
062600     MOVE "LOW-VALUE " TO CORRECT-A.                              NC1074.2
062700     PERFORM INSPT.                                               NC1074.2
062800     GO TO    FIG-WRITE-5.                                        NC1074.2
062900 FIG-DELETE-5.                                                    NC1074.2
063000     PERFORM  DE-LETE.                                            NC1074.2
063100 FIG-WRITE-5.                                                     NC1074.2
063200     MOVE "FIG-TEST-5" TO PAR-NAME.                               NC1074.2
063300     PERFORM  PRINT-DETAIL.                                       NC1074.2
063400 CONTIN-INIT-A.                                                   NC1074.2
063500     MOVE     "CONTINUE A.N. LITRLS" TO FEATURE.                  NC1074.2
063600     MOVE   "IV-44 7.2.2" TO ANSI-REFERENCE.                      NC1074.2
063700     MOVE                                                  "ABCDEFNC1074.2
063800-    "GHIJ" TO DATA-A.                                            NC1074.2
063900 CONTIN-TEST-1.                                                   NC1074.2
064000     IF       DATA-A EQUAL TO "ABCDEFGHIJ"                        NC1074.2
064100              PERFORM PASS                                        NC1074.2
064200              GO TO CONTIN-WRITE-1.                               NC1074.2
064300     GO TO    CONTIN-FAIL-1.                                      NC1074.2
064400 CONTIN-DELETE-1.                                                 NC1074.2
064500     PERFORM  DE-LETE.                                            NC1074.2
064600     GO TO    CONTIN-WRITE-1.                                     NC1074.2
064700 CONTIN-FAIL-1.                                                   NC1074.2
064800     MOVE     DATA-A TO COMPUTED-A.                               NC1074.2
064900     MOVE "ABCDEFGHIJ" TO CORRECT-A.                              NC1074.2
065000     PERFORM  FAIL.                                               NC1074.2
065100 CONTIN-WRITE-1.                                                  NC1074.2
065200     MOVE "CONTIN-TEST-1" TO PAR-NAME.                            NC1074.2
065300     PERFORM  PRINT-DETAIL.                                       NC1074.2
065400 CONTIN-TEST-2.                                                   NC1074.2
065500     IF       DATA-Q EQUAL TO                   "QUOTE IN COL. 72"NC1074.2
065600     PERFORM PASS                                                 NC1074.2
065700              GO TO CONTIN-WRITE-2.                               NC1074.2
065800     PERFORM FAIL.                                                NC1074.2
065900     MOVE     DATA-Q TO COMPUTED-A.                               NC1074.2
066000     MOVE     "QUOTE IN COL. 72" TO CORRECT-A.                    NC1074.2
066100     GO       TO CONTIN-WRITE-2.                                  NC1074.2
066200 CONTIN-DELETE-2.                                                 NC1074.2
066300     PERFORM  DE-LETE.                                            NC1074.2
066400 CONTIN-WRITE-2.                                                  NC1074.2
066500     MOVE     "CONTIN-TEST-2" TO PAR-NAME.                        NC1074.2
066600     PERFORM  PRINT-DETAIL.                                       NC1074.2
066700 CONTIN-TEST-3.                                                   NC1074.2
066800     MOVE   "IV-44 7.2.2" TO ANSI-REFERENCE.                      NC1074.2
066900     IF       DATA-R EQUAL TO                  "LITERAL ENDS AT 72NC1074.2
067000-    ""                                                           NC1074.2
067100     PERFORM PASS                                                 NC1074.2
067200              GO TO CONTIN-WRITE-3.                               NC1074.2
067300     PERFORM FAIL.                                                NC1074.2
067400     MOVE     DATA-R TO COMPUTED-A.                               NC1074.2
067500     MOVE     "LITERAL ENDS AT 72" TO CORRECT-A.                  NC1074.2
067600     GO       TO CONTIN-WRITE-3.                                  NC1074.2
067700 CONTIN-DELETE-3.                                                 NC1074.2
067800     PERFORM  DE-LETE.                                            NC1074.2
067900 CONTIN-WRITE-3.                                                  NC1074.2
068000     MOVE     "CONTIN-TEST-3" TO PAR-NAME.                        NC1074.2
068100     PERFORM  PRINT-DETAIL.                                       NC1074.2
068200 CONTIN-TEST-4.                                                   NC1074.2
068300     MOVE   "IV-44 7.2.2" TO ANSI-REFERENCE.                      NC1074.2
068400     IF       DATA-W EQUAL TO DATA-S                              NC1074.2
068500              PERFORM PASS GO TO CONTIN-WRITE-4.                  NC1074.2
068600     PERFORM  FAIL.                                               NC1074.2
068700     MOVE     DATA-S TO COMPUTED-A.                               NC1074.2
068800     MOVE     DATA-W TO CORRECT-A.                                NC1074.2
068900     GO       TO CONTIN-WRITE-4.                                  NC1074.2
069000 CONTIN-DELETE-4.                                                 NC1074.2
069100     PERFORM  DE-LETE.                                            NC1074.2
069200 CONTIN-WRITE-4.                                                  NC1074.2
069300     MOVE     "CONTIN-TEST-4" TO PAR-NAME                         NC1074.2
069400     PERFORM  PRINT-DETAIL.                                       NC1074.2
069500 CONTIN-TEST-5.                                                   NC1074.2
069600     MOVE   "IV-44 7.2.2" TO ANSI-REFERENCE.                      NC1074.2
069700     IF       DATA-W EQUAL TO DATA-T                              NC1074.2
069800              PERFORM PASS GO TO CONTIN-WRITE-5.                  NC1074.2
069900     PERFORM  FAIL.                                               NC1074.2
070000     MOVE     DATA-T TO COMPUTED-A.                               NC1074.2
070100     MOVE     DATA-W TO CORRECT-A.                                NC1074.2
070200     GO       TO CONTIN-WRITE-5.                                  NC1074.2
070300 CONTIN-DELETE-5.                                                 NC1074.2
070400     PERFORM  DE-LETE.                                            NC1074.2
070500 CONTIN-WRITE-5.                                                  NC1074.2
070600     MOVE     "CONTIN-TEST-5" TO PAR-NAME                         NC1074.2
070700     PERFORM  PRINT-DETAIL.                                       NC1074.2
070800 CONTIN-TEST-6.                                                   NC1074.2
070900     MOVE   "IV-44 7.2.2" TO ANSI-REFERENCE.                      NC1074.2
071000     IF       DATA-W EQUAL TO DATA-U                              NC1074.2
071100              PERFORM PASS GO TO CONTIN-WRITE-6.                  NC1074.2
071200     PERFORM  FAIL.                                               NC1074.2
071300     MOVE     DATA-U TO COMPUTED-A.                               NC1074.2
071400     MOVE     DATA-W TO CORRECT-A.                                NC1074.2
071500     GO       TO CONTIN-WRITE-6.                                  NC1074.2
071600 CONTIN-DELETE-6.                                                 NC1074.2
071700     PERFORM  DE-LETE.                                            NC1074.2
071800 CONTIN-WRITE-6.                                                  NC1074.2
071900     MOVE     "CONTIN-TEST-6" TO PAR-NAME                         NC1074.2
072000     PERFORM  PRINT-DETAIL.                                       NC1074.2
072100 CONTIN-TEST-7.                                                   NC1074.2
072200     MOVE   "IV-44 7.2.2" TO ANSI-REFERENCE.                      NC1074.2
072300     MOVE     DATA-S TO DATA-V.                                   NC1074.2
072400     IF       DATA-W EQUAL TO DATA-V                              NC1074.2
072500              PERFORM PASS GO TO CONTIN-WRITE-7.                  NC1074.2
072600     PERFORM  FAIL.                                               NC1074.2
072700     MOVE     DATA-V TO COMPUTED-A.                               NC1074.2
072800     MOVE     DATA-W TO CORRECT-A.                                NC1074.2
072900     GO       TO CONTIN-WRITE-7.                                  NC1074.2
073000 CONTIN-DELETE-7.                                                 NC1074.2
073100     PERFORM  DE-LETE.                                            NC1074.2
073200 CONTIN-WRITE-7.                                                  NC1074.2
073300     MOVE     "CONTIN-TEST-7" TO PAR-NAME                         NC1074.2
073400     PERFORM  PRINT-DETAIL.                                       NC1074.2
073500 CONTIN-TEST-8.                                                   NC1074.2
073600     MOVE   "IV-44 7.2.2" TO ANSI-REFERENCE.                      NC1074.2
073700     IF       DATA-S EQUAL TO                "OFFSET CONTINUATION NC1074.2
073800-                                                               ""NC1074.2
073900              PERFORM PASS GO TO CONTIN-WRITE-8.                  NC1074.2
074000     PERFORM  FAIL.                                               NC1074.2
074100     MOVE                                    "OFFSET CONTINUATION NC1074.2
074200-                                                               ""NC1074.2
074300              TO COMPUTED-A.                                      NC1074.2
074400     MOVE     DATA-S TO CORRECT-A.                                NC1074.2
074500     GO       TO CONTIN-WRITE-8.                                  NC1074.2
074600 CONTIN-DELETE-8.                                                 NC1074.2
074700     PERFORM  DE-LETE.                                            NC1074.2
074800 CONTIN-WRITE-8.                                                  NC1074.2
074900     MOVE     "CONTIN-TEST-8" TO PAR-NAME                         NC1074.2
075000     PERFORM  PRINT-DETAIL.                                       NC1074.2
075100 CONTIN-TEST-9.                                                   NC1074.2
075200     MOVE   "IV-10 4.2.2.2.1.2 (2) AND IV-9 4.2.2.2.1"            NC1074.2
075300          TO ANSI-REFERENCE.                                      NC1074.2
075400     IF WRK-XN-160-1 EQUAL TO                                     NC1074.2
075500     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""NC1074.2
075600-    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""NC1074.2
075700-    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""NC1074.2
075800-    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""NC1074.2
075900-    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""NC1074.2
076000-    """""""""""""""""""""" PERFORM PASS                          NC1074.2
076100     ELSE PERFORM FAIL.                                           NC1074.2
076200     GO TO CONTIN-WRITE-9.                                        NC1074.2
076300 CONTIN-DELETE-9.                                                 NC1074.2
076400     PERFORM DE-LETE.                                             NC1074.2
076500 CONTIN-WRITE-9.                                                  NC1074.2
076600     MOVE "CONTIN-TEST-9" TO PAR-NAME.                            NC1074.2
076700     MOVE "160 PAIRS OF QUOTES" TO FEATURE.                       NC1074.2
076800     PERFORM PRINT-DETAIL.                                        NC1074.2
076900 SEP-INIT-A.                                                      NC1074.2
077000     MOVE   "IV-4 4.2.1" TO ANSI-REFERENCE.                       NC1074.2
077100     MOVE     "SEPARATORS (SPACES)" TO FEATURE.                   NC1074.2
077200 SEP-TEST-1.                                                      NC1074.2
077300                                                     PERFORM PASS.NC1074.2
077400                                                                GONC1074.2
077500                                                                TONC1074.2
077600                                                      SEP-WRITE-1.NC1074.2
077700 SEP-TEST-1-1.                                                    NC1074.2
077800                                                     PERFORM FAIL.NC1074.2
077900                                                GO TO SEP-WRITE-1.NC1074.2
078000*                                                             NOTENC1074.2
078100*                                                       SEP-TEST-1NC1074.2
078200*                                            ENTIRE PARAGRAPH IS "NC1074.2
078300*                                    RIGHT-JUSTIFIED, TO MARGIN R.NC1074.2
078400 SEP-DELETE-1.                                                    NC1074.2
078500     PERFORM  DE-LETE.                                            NC1074.2
078600 SEP-WRITE-1.                                                     NC1074.2
078700     MOVE     "SEP-TEST-1" TO PAR-NAME.                           NC1074.2
078800     PERFORM  PRINT-DETAIL.                                       NC1074.2
078900 SEP-TEST-2 SECTION.                                              NC1074.2
079000 SEP-TEST-2-PARA.                                                 NC1074.2
079100     MOVE   "IV-4 4.2.1" TO ANSI-REFERENCE.                       NC1074.2
079200 S2. PERFORM PASS. IF P-OR-F EQUAL TO "PASS " GO TO               NC1074.2
079300     SEP-WRITE-2. PERFORM FAIL. GO TO SEP-WRITE-2.                NC1074.2
079400 SEP-DELETE-2.                                                    NC1074.2
079500     PERFORM  DE-LETE.                                            NC1074.2
079600 SEP-WRITE-2.                                                     NC1074.2
079700     MOVE     "SEP-TEST-2" TO PAR-NAME.                           NC1074.2
079800     PERFORM  PRINT-DETAIL.                                       NC1074.2
079900 SEP-TEST-3.                                                      NC1074.2
080000     MOVE   "IV-4 4.2.1" TO ANSI-REFERENCE.                       NC1074.2
080100     IF       SEP-03 EQUAL TO "SEPARATOR"                         NC1074.2
080200              PERFORM PASS                                        NC1074.2
080300              GO TO SEP-WRITE-3.                                  NC1074.2
080400     PERFORM  FAIL.                                               NC1074.2
080500     MOVE     SEP-03 TO COMPUTED-A.                               NC1074.2
080600     MOVE     "SEPARATOR" TO CORRECT-A.                           NC1074.2
080700     GO       TO SEP-WRITE-3.                                     NC1074.2
080800 SEP-DELETE-3.                                                    NC1074.2
080900     PERFORM  DE-LETE.                                            NC1074.2
081000 SEP-WRITE-3.                                                     NC1074.2
081100     MOVE     "SEP-TEST-3" TO PAR-NAME.                           NC1074.2
081200     PERFORM  PRINT-DETAIL.                                       NC1074.2
081300 SEP-TEST-4.                                                      NC1074.2
081400     MOVE   "IV-4 4.2.1" TO ANSI-REFERENCE.                       NC1074.2
081500     PERFORM  FAIL.                                               NC1074.2
081600                                                                  NC1074.2
081700                                                                  NC1074.2
081800                                                                  NC1074.2
081900                                                                  NC1074.2
082000                                                                  NC1074.2
082100                                                                  NC1074.2
082200                                                                  NC1074.2
082300                                                                  NC1074.2
082400                                                                  NC1074.2
082500                                                                  NC1074.2
082600                                                                  NC1074.2
082700                                                                  NC1074.2
082800                                                                  NC1074.2
082900                                                                  NC1074.2
083000                                                                  NC1074.2
083100                                                                  NC1074.2
083200                                                                  NC1074.2
083300                                                                  NC1074.2
083400                                                                  NC1074.2
083500                                                                  NC1074.2
083600                                                                  NC1074.2
083700                                                                  NC1074.2
083800                                                                  NC1074.2
083900                                                                  NC1074.2
084000                                                                  NC1074.2
084100                                                                  NC1074.2
084200                                                                  NC1074.2
084300                                                                  NC1074.2
084400                                                                  NC1074.2
084500                                                                  NC1074.2
084600                                                                  NC1074.2
084700                                                                  NC1074.2
084800                                                                  NC1074.2
084900                                                                  NC1074.2
085000                                                                  NC1074.2
085100                                                                  NC1074.2
085200                                                                  NC1074.2
085300                                                                  NC1074.2
085400                                                                  NC1074.2
085500                                                                  NC1074.2
085600                                                                  NC1074.2
085700                                                                  NC1074.2
085800                                                                  NC1074.2
085900                                                                  NC1074.2
086000                                                                  NC1074.2
086100                                                                  NC1074.2
086200                                                                  NC1074.2
086300                                                                  NC1074.2
086400                                                                  NC1074.2
086500                                                                  NC1074.2
086600                                                                  NC1074.2
086700                                                                  NC1074.2
086800                                                                  NC1074.2
086900                                                                  NC1074.2
087000                                                                  NC1074.2
087100                                                                  NC1074.2
087200                                                                  NC1074.2
087300                                                                  NC1074.2
087400                                                                  NC1074.2
087500                                                                  NC1074.2
087600                                                                  NC1074.2
087700                                                                  NC1074.2
087800                                                                  NC1074.2
087900                                                                  NC1074.2
088000                                                                  NC1074.2
088100                                                                  NC1074.2
088200                                                                  NC1074.2
088300                                                                  NC1074.2
088400                                                                  NC1074.2
088500                                                                  NC1074.2
088600                                                                  NC1074.2
088700                                                                  NC1074.2
088800                                                                  NC1074.2
088900                                                                  NC1074.2
089000                                                                  NC1074.2
089100                                                                  NC1074.2
089200                                                                  NC1074.2
089300                                                                  NC1074.2
089400                                                                  NC1074.2
089500                                                                  NC1074.2
089600                                                                  NC1074.2
089700                                                                  NC1074.2
089800                                                                  NC1074.2
089900                                                                  NC1074.2
090000                                                                  NC1074.2
090100                                                                  NC1074.2
090200                                                                  NC1074.2
090300                                                                  NC1074.2
090400                                                                  NC1074.2
090500                                                                  NC1074.2
090600                                                                  NC1074.2
090700                                                                  NC1074.2
090800                                                                  NC1074.2
090900                                                                  NC1074.2
091000                                                                  NC1074.2
091100                                                                  NC1074.2
091200                                                                  NC1074.2
091300                                                                  NC1074.2
091400                                                                  NC1074.2
091500                                                                  NC1074.2
091600                                                                  NC1074.2
091700     SUBTRACT                                                     NC1074.2
091800              1 FROM ERROR-COUNTER.                               NC1074.2
091900     PERFORM  PASS.                                               NC1074.2
092000     GO       TO SEP-WRITE-4.                                     NC1074.2
092100 SEP-DELETE-4.                                                    NC1074.2
092200     PERFORM  DE-LETE.                                            NC1074.2
092300 SEP-WRITE-4.                                                     NC1074.2
092400     MOVE     "SEP-TEST-4" TO PAR-NAME.                           NC1074.2
092500     PERFORM  PRINT-DETAIL.                                       NC1074.2
092600 SEP-TEST-5 SECTION.                                              NC1074.2
092700 SEP-TEST-5-PARA.                                                 NC1074.2
092800*    ==-->  SEMICOLON AS SEPARATOR  <--==                         NC1074.2
092900     MOVE   "IV-4 4.2.1(2)" TO ANSI-REFERENCE.                    NC1074.2
093000 S5. PERFORM PASS, IF P-OR-F EQUAL TO "PASS " GO TO               NC1074.2
093100     SEP-WRITE-5; ELSE PERFORM FAIL, GO TO SEP-WRITE-5.           NC1074.2
093200 SEP-DELETE-5.                                                    NC1074.2
093300     PERFORM  DE-LETE.                                            NC1074.2
093400 SEP-WRITE-5.                                                     NC1074.2
093500     MOVE     "SEP-TEST-5" TO PAR-NAME.                           NC1074.2
093600     PERFORM  PRINT-DETAIL.                                       NC1074.2
093700 JUST-INIT-01.                                                    NC1074.2
093800     MOVE   "IV-24 5.6.4" TO ANSI-REFERENCE.                      NC1074.2
093900*                                                                 NC1074.2
094000*    INITIALIZATION TAKES PLACE INDEPENDENT OF ANY JUSTIFIED      NC1074.2
094100*    CLAUSE.                                                      NC1074.2
094200*    REFERENCE - X3.23-1985, PAGE VI-49, 5.15.4(1)C.              NC1074.2
094300*                                                                 NC1074.2
094400     MOVE "JUST WITH VALUE" TO FEATURE.                           NC1074.2
094500     MOVE "JUST-TEST-01" TO PAR-NAME.                             NC1074.2
094600 JUST-TEST-01-1.                                                  NC1074.2
094700     IF XJ-00002 EQUAL TO "AB"                                    NC1074.2
094800             PERFORM PASS                                         NC1074.2
094900             GO TO JUST-WRITE-01-1                                NC1074.2
095000         ELSE GO TO JUST-FAIL-01-1.                               NC1074.2
095100 JUST-DELETE-01-1.                                                NC1074.2
095200     PERFORM DE-LETE.                                             NC1074.2
095300     GO TO JUST-WRITE-01-1.                                       NC1074.2
095400 JUST-FAIL-01-1.                                                  NC1074.2
095500     PERFORM FAIL.                                                NC1074.2
095600     MOVE XJ-00002 TO COMPUTED-A.                                 NC1074.2
095700     MOVE "AB" TO CORRECT-A.                                      NC1074.2
095800 JUST-WRITE-01-1.                                                 NC1074.2
095900     MOVE 1 TO REC-CT.                                            NC1074.2
096000     PERFORM PRINT-DETAIL.                                        NC1074.2
096100 JUST-TEST-01-2.                                                  NC1074.2
096200     IF XJ-00003 EQUAL TO "XY "                                   NC1074.2
096300             PERFORM PASS                                         NC1074.2
096400             GO TO JUST-WRITE-01-2                                NC1074.2
096500         ELSE GO TO JUST-FAIL-01-2.                               NC1074.2
096600 JUST-DELETE-01-2.                                                NC1074.2
096700     PERFORM DE-LETE.                                             NC1074.2
096800     GO TO JUST-WRITE-01-2.                                       NC1074.2
096900 JUST-FAIL-01-2.                                                  NC1074.2
097000     PERFORM FAIL.                                                NC1074.2
097100     MOVE XJ-00003 TO COMPUTED-A.                                 NC1074.2
097200     MOVE "XY " TO CORRECT-A.                                     NC1074.2
097300 JUST-WRITE-01-2.                                                 NC1074.2
097400     MOVE 2 TO REC-CT.                                            NC1074.2
097500     PERFORM PRINT-DETAIL.                                        NC1074.2
097600 JUST-INIT-02.                                                    NC1074.2
097700     MOVE   "IV-24 5.6.4" TO ANSI-REFERENCE.                      NC1074.2
097800*                                                                 NC1074.2
097900*    JUSTIFIED RECEIVING ITEM TESTS                               NC1074.2
098000*        IF THE SENDING ITEM IS LARGER THAN THE RECEIVING ITEM,   NC1074.2
098100*    THEN THE LEFTMOST CHARACTERS ARE TRUNCATED.  IF THE SENDING  NC1074.2
098200*    ITEM IS SMALLER THAN THE RECEIVING ITEM, THEN THE DATA IS    NC1074.2
098300*    ALIGNED TO THE RIGHT WITH SPACES IN THE LEFTMOST CHARACTER   NC1074.2
098400*    POSITIONS.                                                   NC1074.2
098500*    REFERENCE - X3.23-1985, PAGE VI-24, 6.5.4.(1)                NC1074.2
098600*                                                                 NC1074.2
098700*    JUST-TEST-02 CONTAINS STATEMENTS OF THE FORM                 NC1074.2
098800*        MOVE ALPHANUMERIC LITERAL TO ALPHANUMERIC JUSTIFIED ITEM.NC1074.2
098900*                                                                 NC1074.2
099000     MOVE "MOVE - JUST REC ITEM" TO FEATURE.                      NC1074.2
099100     MOVE "JUST-TEST-02" TO PAR-NAME.                             NC1074.2
099200 JUST-TEST-02-1-0.                                                NC1074.2
099300     MOVE "ABC" TO XJ-00005.                                      NC1074.2
099400 JUST-TEST-02-1-1.                                                NC1074.2
099500     IF XJ-00005 EQUAL TO "  ABC"                                 NC1074.2
099600             PERFORM PASS                                         NC1074.2
099700             GO TO JUST-WRITE-02-1                                NC1074.2
099800         ELSE GO TO JUST-FAIL-02-1.                               NC1074.2
099900 JUST-DELETE-02-1.                                                NC1074.2
100000     PERFORM DE-LETE.                                             NC1074.2
100100     GO TO JUST-WRITE-02-1.                                       NC1074.2
100200 JUST-FAIL-02-1.                                                  NC1074.2
100300     PERFORM FAIL.                                                NC1074.2
100400     MOVE "  ABC" TO CORRECT-A.                                   NC1074.2
100500     MOVE XJ-00005 TO COMPUTED-A.                                 NC1074.2
100600 JUST-WRITE-02-1.                                                 NC1074.2
100700     MOVE 1 TO REC-CT.                                            NC1074.2
100800     PERFORM PRINT-DETAIL.                                        NC1074.2
100900 JUST-TEST-02-2-0.                                                NC1074.2
101000     MOVE "ABCDEFGHI" TO XJ-00005.                                NC1074.2
101100 JUST-TEST-02-2-1.                                                NC1074.2
101200     IF XJ-00005 EQUAL TO "EFGHI"                                 NC1074.2
101300             PERFORM PASS                                         NC1074.2
101400             GO TO JUST-WRITE-02-2                                NC1074.2
101500         ELSE GO TO JUST-FAIL-02-2.                               NC1074.2
101600 JUST-DELETE-02-2.                                                NC1074.2
101700     PERFORM DE-LETE.                                             NC1074.2
101800     GO TO JUST-WRITE-02-2.                                       NC1074.2
101900 JUST-FAIL-02-2.                                                  NC1074.2
102000     PERFORM FAIL.                                                NC1074.2
102100     MOVE "EFGHI" TO CORRECT-A.                                   NC1074.2
102200     MOVE XJ-00005 TO COMPUTED-A.                                 NC1074.2
102300 JUST-WRITE-02-2.                                                 NC1074.2
102400     MOVE 2 TO REC-CT.                                            NC1074.2
102500     PERFORM PRINT-DETAIL.                                        NC1074.2
102600 JUST-TEST-02-3-0.                                                NC1074.2
102700     MOVE "CDEFG" TO XJ-00005.                                    NC1074.2
102800 JUST-TEST-02-3-1.                                                NC1074.2
102900     IF XJ-00005 EQUAL TO "CDEFG"                                 NC1074.2
103000             PERFORM PASS                                         NC1074.2
103100             GO TO JUST-WRITE-02-3                                NC1074.2
103200         ELSE GO TO JUST-FAIL-02-3.                               NC1074.2
103300 JUST-DELETE-02-3.                                                NC1074.2
103400     PERFORM DE-LETE.                                             NC1074.2
103500     GO TO JUST-WRITE-02-3.                                       NC1074.2
103600 JUST-FAIL-02-3.                                                  NC1074.2
103700     PERFORM FAIL.                                                NC1074.2
103800     MOVE "CDEFG" TO CORRECT-A.                                   NC1074.2
103900     MOVE XJ-00005 TO COMPUTED-A.                                 NC1074.2
104000 JUST-WRITE-02-3.                                                 NC1074.2
104100     MOVE 3 TO REC-CT.                                            NC1074.2
104200     PERFORM PRINT-DETAIL.                                        NC1074.2
104300 JUST-INIT-03.                                                    NC1074.2
104400     MOVE   "IV-24 5.6.4" TO ANSI-REFERENCE.                      NC1074.2
104500*                                                                 NC1074.2
104600*    JUST-TEST-03 CONTAINS STATEMENTS OF THE FORM                 NC1074.2
104700*        MOVE ALPHANUMERIC ITEM TO ALPHABETIC JUSTIFIED ITEM.     NC1074.2
104800*                                                                 NC1074.2
104900     MOVE "MOVE - JUST REC ITEM" TO FEATURE.                      NC1074.2
105000     MOVE "JUST-TEST-03" TO PAR-NAME.                             NC1074.2
105100 JUST-TEST-03-1-0.                                                NC1074.2
105200     MOVE NJUST-XN-3 TO AJ-00005.                                 NC1074.2
105300 JUST-TEST-03-1-1.                                                NC1074.2
105400     IF AJ-00005 EQUAL TO "  ABC"                                 NC1074.2
105500             PERFORM PASS                                         NC1074.2
105600             GO TO JUST-WRITE-03-1                                NC1074.2
105700         ELSE GO TO JUST-FAIL-03-1.                               NC1074.2
105800 JUST-DELETE-03-1.                                                NC1074.2
105900     PERFORM DE-LETE.                                             NC1074.2
106000     GO TO JUST-WRITE-03-1.                                       NC1074.2
106100 JUST-FAIL-03-1.                                                  NC1074.2
106200     PERFORM FAIL.                                                NC1074.2
106300     MOVE "  ABC" TO CORRECT-A.                                   NC1074.2
106400     MOVE AJ-00005 TO COMPUTED-A.                                 NC1074.2
106500 JUST-WRITE-03-1.                                                 NC1074.2
106600     MOVE 1 TO REC-CT.                                            NC1074.2
106700     PERFORM PRINT-DETAIL.                                        NC1074.2
106800 JUST-TEST-03-2-0.                                                NC1074.2
106900     MOVE NJUST-XN-5 TO AJ-00005.                                 NC1074.2
107000 JUST-TEST-03-2-1.                                                NC1074.2
107100     IF AJ-00005 EQUAL TO "CDEFG"                                 NC1074.2
107200             PERFORM PASS                                         NC1074.2
107300             GO TO JUST-WRITE-03-2                                NC1074.2
107400         ELSE GO TO JUST-FAIL-03-2.                               NC1074.2
107500 JUST-DELETE-03-2.                                                NC1074.2
107600     PERFORM DE-LETE.                                             NC1074.2
107700     GO TO JUST-WRITE-03-2.                                       NC1074.2
107800 JUST-FAIL-03-2.                                                  NC1074.2
107900     PERFORM FAIL.                                                NC1074.2
108000     MOVE "CDEFG" TO CORRECT-A.                                   NC1074.2
108100     MOVE AJ-00005 TO COMPUTED-A.                                 NC1074.2
108200 JUST-WRITE-03-2.                                                 NC1074.2
108300     MOVE 2 TO REC-CT.                                            NC1074.2
108400     PERFORM PRINT-DETAIL.                                        NC1074.2
108500 JUST-TEST-03-3-0.                                                NC1074.2
108600     MOVE NJUST-XN-15 TO AJ-00005.                                NC1074.2
108700 JUST-TEST-03-3-1.                                                NC1074.2
108800     IF AJ-00005 EQUAL TO "KLMNO"                                 NC1074.2
108900             PERFORM PASS                                         NC1074.2
109000             GO TO JUST-WRITE-03-3                                NC1074.2
109100         ELSE GO TO JUST-FAIL-03-3.                               NC1074.2
109200 JUST-DELETE-03-3.                                                NC1074.2
109300     PERFORM DE-LETE.                                             NC1074.2
109400     GO TO JUST-WRITE-03-3.                                       NC1074.2
109500 JUST-FAIL-03-3.                                                  NC1074.2
109600     PERFORM FAIL.                                                NC1074.2
109700     MOVE "KLMNO" TO CORRECT-A.                                   NC1074.2
109800     MOVE AJ-00005 TO COMPUTED-A.                                 NC1074.2
109900 JUST-WRITE-03-3.                                                 NC1074.2
110000     MOVE 3 TO REC-CT.                                            NC1074.2
110100     PERFORM PRINT-DETAIL.                                        NC1074.2
110200 JUST-INIT-04.                                                    NC1074.2
110300     MOVE   "IV-24 5.6.4" TO ANSI-REFERENCE.                      NC1074.2
110400*                                                                 NC1074.2
110500*    JUST-TEST-04 CONTAINS STATEMENTS OF THE FORM                 NC1074.2
110600*        MOVE GROUP ITEM TO ALPHABETIC JUSTIFIED ITEM.            NC1074.2
110700*                                                                 NC1074.2
110800     MOVE "MOVE - JUST REC ITEM" TO FEATURE.                      NC1074.2
110900     MOVE "JUST-TEST-04" TO PAR-NAME.                             NC1074.2
111000 JUST-TEST-04-1-0.                                                NC1074.2
111100     MOVE GROUP-TO-JUST-1 TO AJ-00007.                            NC1074.2
111200 JUST-TEST-04-1-1.                                                NC1074.2
111300     IF AJ-00007 EQUAL TO "    ABC"                               NC1074.2
111400             PERFORM PASS                                         NC1074.2
111500             GO TO JUST-WRITE-04-1                                NC1074.2
111600         ELSE GO TO JUST-FAIL-04-1.                               NC1074.2
111700 JUST-DELETE-04-1.                                                NC1074.2
111800     PERFORM DE-LETE.                                             NC1074.2
111900     GO TO JUST-WRITE-04-1.                                       NC1074.2
112000 JUST-FAIL-04-1.                                                  NC1074.2
112100     PERFORM FAIL.                                                NC1074.2
112200     MOVE "    ABC" TO CORRECT-A.                                 NC1074.2
112300     MOVE AJ-00007 TO COMPUTED-A.                                 NC1074.2
112400 JUST-WRITE-04-1.                                                 NC1074.2
112500     MOVE 1 TO REC-CT.                                            NC1074.2
112600     PERFORM PRINT-DETAIL.                                        NC1074.2
112700 JUST-TEST-04-2-0.                                                NC1074.2
112800     MOVE GROUP-TO-JUST-21 TO AJ-00007.                           NC1074.2
112900 JUST-TEST-04-2-1.                                                NC1074.2
113000     IF AJ-00007 EQUAL TO "ABCDEFG"                               NC1074.2
113100             PERFORM PASS                                         NC1074.2
113200             GO TO JUST-WRITE-04-2                                NC1074.2
113300         ELSE GO TO JUST-FAIL-04-2.                               NC1074.2
113400 JUST-DELETE-04-2.                                                NC1074.2
113500     PERFORM DE-LETE.                                             NC1074.2
113600     GO TO JUST-WRITE-04-2.                                       NC1074.2
113700 JUST-FAIL-04-2.                                                  NC1074.2
113800     PERFORM FAIL.                                                NC1074.2
113900     MOVE "ABCDEFG" TO CORRECT-A.                                 NC1074.2
114000     MOVE AJ-00007 TO COMPUTED-A.                                 NC1074.2
114100 JUST-WRITE-04-2.                                                 NC1074.2
114200     MOVE 2 TO REC-CT.                                            NC1074.2
114300     PERFORM PRINT-DETAIL.                                        NC1074.2
114400 JUST-TEST-04-3-0.                                                NC1074.2
114500     MOVE GROUP-TO-JUST-2 TO AJ-00007.                            NC1074.2
114600 JUST-TEST-04-3-1.                                                NC1074.2
114700     IF AJ-00007 EQUAL TO "IJKLMNO"                               NC1074.2
114800             PERFORM PASS                                         NC1074.2
114900             GO TO JUST-WRITE-04-3                                NC1074.2
115000         ELSE GO TO JUST-FAIL-04-3.                               NC1074.2
115100 JUST-DELETE-04-3.                                                NC1074.2
115200     PERFORM DE-LETE.                                             NC1074.2
115300     GO TO JUST-WRITE-04-3.                                       NC1074.2
115400 JUST-FAIL-04-3.                                                  NC1074.2
115500     PERFORM FAIL.                                                NC1074.2
115600     MOVE "IJKLMNO" TO CORRECT-A.                                 NC1074.2
115700     MOVE AJ-00007 TO COMPUTED-A.                                 NC1074.2
115800 JUST-WRITE-04-3.                                                 NC1074.2
115900     MOVE 3 TO REC-CT.                                            NC1074.2
116000     PERFORM PRINT-DETAIL.                                        NC1074.2
116100 JUST-INIT-05.                                                    NC1074.2
116200     MOVE   "IV-24 5.6.4" TO ANSI-REFERENCE.                      NC1074.2
116300*                                                                 NC1074.2
116400*    JUST-TEST-05 CONTAINS MOVE STATEMENTS WITH A JUSTIFIED       NC1074.2
116500*    SENDING ITEM.                                                NC1074.2
116600*                                                                 NC1074.2
116700     MOVE "MOVE-JUST SEND ITEM" TO FEATURE.                       NC1074.2
116800     MOVE "JUST-TEST-05" TO PAR-NAME.                             NC1074.2
116900     MOVE "12345ABCDEFGHUXYZ   PQR" TO GROUP-WITH-JUST-ITEMS.     NC1074.2
117000     MOVE SPACE TO GROUP-FOR-JUST-TESTS.                          NC1074.2
117100 JUST-TEST-05-1-0.                                                NC1074.2
117200     MOVE AJ-00009 TO NJUST-XN-15.                                NC1074.2
117300 JUST-TEST-05-1-1.                                                NC1074.2
117400     IF NJUST-XN-15 EQUAL TO "XYZ   PQR      "                    NC1074.2
117500             PERFORM PASS                                         NC1074.2
117600             GO TO JUST-WRITE-05-1                                NC1074.2
117700         ELSE GO TO JUST-FAIL-05-1.                               NC1074.2
117800 JUST-DELETE-05-1.                                                NC1074.2
117900     PERFORM DE-LETE.                                             NC1074.2
118000     GO TO JUST-WRITE-05-1.                                       NC1074.2
118100 JUST-FAIL-05-1.                                                  NC1074.2
118200     PERFORM FAIL.                                                NC1074.2
118300     MOVE "XYZ   PQR      " TO CORRECT-A.                         NC1074.2
118400     MOVE NJUST-XN-15 TO COMPUTED-A.                              NC1074.2
118500 JUST-WRITE-05-1.                                                 NC1074.2
118600     MOVE 1 TO REC-CT.                                            NC1074.2
118700     PERFORM PRINT-DETAIL.                                        NC1074.2
118800 JUST-TEST-05-2-0.                                                NC1074.2
118900     MOVE XJ-00009 TO NJUST-XN-3.                                 NC1074.2
119000 JUST-TEST-05-2-1.                                                NC1074.2
119100     IF NJUST-XN-3 EQUAL TO "ABC"                                 NC1074.2
119200             PERFORM PASS                                         NC1074.2
119300             GO TO JUST-WRITE-05-2                                NC1074.2
119400         ELSE GO TO JUST-FAIL-05-2.                               NC1074.2
119500 JUST-DELETE-05-2.                                                NC1074.2
119600     PERFORM DE-LETE.                                             NC1074.2
119700     GO TO JUST-WRITE-05-2.                                       NC1074.2
119800 JUST-FAIL-05-2.                                                  NC1074.2
119900     PERFORM FAIL.                                                NC1074.2
120000     MOVE NJUST-XN-3 TO COMPUTED-A.                               NC1074.2
120100     MOVE "ABC" TO CORRECT-A.                                     NC1074.2
120200 JUST-WRITE-05-2.                                                 NC1074.2
120300     MOVE 2 TO REC-CT.                                            NC1074.2
120400     PERFORM PRINT-DETAIL.                                        NC1074.2
120500 JUST-INIT-06.                                                    NC1074.2
120600     MOVE   "IV-24 5.6.4" TO ANSI-REFERENCE.                      NC1074.2
120700*                                                                 NC1074.2
120800*    JUST-TEST-06 CONTAINS MOVE STATEMENTS WITH A JUSTIFIED       NC1074.2
120900*    SENDING ITEM AND A JUSTIFIED RECEIVING ITEM.                 NC1074.2
121000*                                                                 NC1074.2
121100     MOVE "MOVE - JUST TO JUST" TO FEATURE.                       NC1074.2
121200     MOVE "JUST-TEST-06" TO PAR-NAME.                             NC1074.2
121300     MOVE "12345ABCDEFGHIXYZ   PQR" TO GROUP-WITH-JUST-ITEMS.     NC1074.2
121400 JUST-TEST-06-0.                                                  NC1074.2
121500     MOVE SPACE TO XJ-00005.                                      NC1074.2
121600 JUST-TEST-06-1.                                                  NC1074.2
121700     MOVE XJ-00009 TO XJ-00005.                                   NC1074.2
121800     IF XJ-00005 EQUAL TO "EFGHI"                                 NC1074.2
121900             PERFORM PASS                                         NC1074.2
122000             GO TO JUST-WRITE-06-1                                NC1074.2
122100         ELSE GO TO JUST-FAIL-06-1.                               NC1074.2
122200 JUST-DELETE-06-1.                                                NC1074.2
122300     PERFORM DE-LETE.                                             NC1074.2
122400     GO TO JUST-WRITE-06-1.                                       NC1074.2
122500 JUST-FAIL-06-1.                                                  NC1074.2
122600     PERFORM FAIL.                                                NC1074.2
122700     MOVE "EFGHI" TO CORRECT-A.                                   NC1074.2
122800     MOVE XJ-00005 TO COMPUTED-A.                                 NC1074.2
122900 JUST-WRITE-06-1.                                                 NC1074.2
123000     MOVE 1 TO REC-CT.                                            NC1074.2
123100     PERFORM PRINT-DETAIL.                                        NC1074.2
123200 JUST-INIT-06-2.                                                  NC1074.2
123300     MOVE SPACE TO AJ-00005.                                      NC1074.2
123400 JUST-TEST-06-2.                                                  NC1074.2
123500     MOVE AJ-00009 TO AJ-00005.                                   NC1074.2
123600     IF AJ-00005 EQUAL TO "  PQR"                                 NC1074.2
123700             PERFORM PASS                                         NC1074.2
123800             GO TO JUST-WRITE-06-2                                NC1074.2
123900         ELSE GO TO JUST-FAIL-06-2.                               NC1074.2
124000 JUST-DELETE-06-2.                                                NC1074.2
124100     PERFORM DE-LETE.                                             NC1074.2
124200     GO TO JUST-WRITE-06-2.                                       NC1074.2
124300 JUST-FAIL-06-2.                                                  NC1074.2
124400     PERFORM FAIL.                                                NC1074.2
124500     MOVE "  PQR" TO CORRECT-A.                                   NC1074.2
124600     MOVE AJ-00005 TO COMPUTED-A.                                 NC1074.2
124700 JUST-WRITE-06-2.                                                 NC1074.2
124800     MOVE 2 TO REC-CT.                                            NC1074.2
124900     PERFORM PRINT-DETAIL.                                        NC1074.2
125000 JUST-INIT-06-3.                                                  NC1074.2
125100     MOVE "ABCDEFG" TO XJ-00007.                                  NC1074.2
125200     MOVE SPACE TO AJ-00007.                                      NC1074.2
125300 JUST-TEST-06-3.                                                  NC1074.2
125400     MOVE XJ-00007 TO AJ-00007.                                   NC1074.2
125500     IF AJ-00007 EQUAL TO "ABCDEFG"                               NC1074.2
125600             PERFORM PASS                                         NC1074.2
125700             GO TO JUST-WRITE-06-3                                NC1074.2
125800         ELSE GO TO JUST-FAIL-06-3.                               NC1074.2
125900 JUST-DELETE-06-3.                                                NC1074.2
126000     PERFORM DE-LETE.                                             NC1074.2
126100     GO TO JUST-WRITE-06-3.                                       NC1074.2
126200 JUST-FAIL-06-3.                                                  NC1074.2
126300     PERFORM FAIL.                                                NC1074.2
126400     MOVE "ABCDEFG" TO CORRECT-A.                                 NC1074.2
126500     MOVE AJ-00007 TO COMPUTED-A.                                 NC1074.2
126600 JUST-WRITE-06-3.                                                 NC1074.2
126700     MOVE 3 TO REC-CT.                                            NC1074.2
126800     PERFORM PRINT-DETAIL.                                        NC1074.2
126900 JUST-INIT-06-4.                                                  NC1074.2
127000     MOVE SPACE TO XJ-00005.                                      NC1074.2
127100 JUST-TEST-06-4.                                                  NC1074.2
127200     MOVE XJ-00007 TO XJ-00005.                                   NC1074.2
127300     IF XJ-00005 EQUAL TO "CDEFG"                                 NC1074.2
127400             PERFORM PASS                                         NC1074.2
127500             GO TO JUST-WRITE-06-4                                NC1074.2
127600         ELSE GO TO JUST-FAIL-06-4.                               NC1074.2
127700 JUST-DELETE-06-4.                                                NC1074.2
127800     PERFORM DE-LETE.                                             NC1074.2
127900     GO TO JUST-WRITE-06-4.                                       NC1074.2
128000 JUST-FAIL-06-4.                                                  NC1074.2
128100     PERFORM FAIL.                                                NC1074.2
128200     MOVE "CDEFG" TO CORRECT-A.                                   NC1074.2
128300     MOVE XJ-00005 TO COMPUTED-A.                                 NC1074.2
128400 JUST-WRITE-06-4.                                                 NC1074.2
128500     MOVE 4 TO REC-CT.                                            NC1074.2
128600     PERFORM PRINT-DETAIL.                                        NC1074.2
128700 JUST-INIT-06-5.                                                  NC1074.2
128800     MOVE SPACE TO XJ-00009.                                      NC1074.2
128900 JUST-TEST-06-5.                                                  NC1074.2
129000     MOVE XJ-00007 TO XJ-00009.                                   NC1074.2
129100     IF XJ-00009 EQUAL TO "  ABCDEFG"                             NC1074.2
129200             PERFORM PASS                                         NC1074.2
129300             GO TO JUST-WRITE-06-5                                NC1074.2
129400         ELSE GO TO JUST-FAIL-06-5.                               NC1074.2
129500 JUST-DELETE-06-5.                                                NC1074.2
129600     PERFORM DE-LETE.                                             NC1074.2
129700     GO TO JUST-WRITE-06-5.                                       NC1074.2
129800 JUST-FAIL-06-5.                                                  NC1074.2
129900     PERFORM FAIL.                                                NC1074.2
130000     MOVE "  ABCDEFG" TO CORRECT-A.                               NC1074.2
130100     MOVE XJ-00009 TO COMPUTED-A.                                 NC1074.2
130200 JUST-WRITE-06-5.                                                 NC1074.2
130300     MOVE 5 TO REC-CT.                                            NC1074.2
130400     PERFORM PRINT-DETAIL.                                        NC1074.2
130500     MOVE 0 TO REC-CT.                                            NC1074.2
130600 SYNC-TEST-1.                                                     NC1074.2
130700     MOVE   "VI-44 5.13.4" TO ANSI-REFERENCE.                     NC1074.2
130800     IF       DATA-G EQUAL TO DATA-H                              NC1074.2
130900              PERFORM PASS                                        NC1074.2
131000              ELSE                                                NC1074.2
131100              PERFORM FAIL.                                       NC1074.2
131200     GO TO    SYNC-WRITE-1.                                       NC1074.2
131300 SYNC-DELETE-1.                                                   NC1074.2
131400     PERFORM  DE-LETE.                                            NC1074.2
131500 SYNC-WRITE-1.                                                    NC1074.2
131600     MOVE "SYNCHRONIZED" TO FEATURE.                              NC1074.2
131700     MOVE "SYNC-TEST-1" TO PAR-NAME.                              NC1074.2
131800     PERFORM  PRINT-DETAIL.                                       NC1074.2
131900 BZERO-INIT.                                                      NC1074.2
132000     MOVE   "VI-22 5.4" TO ANSI-REFERENCE.                        NC1074.2
132100     MOVE "BLANK WHEN ZERO" TO FEATURE.                           NC1074.2
132200 BZERO-TEST-1-0.                                                  NC1074.2
132300     MOVE     0000000000 TO DATA-F.                               NC1074.2
132400 BZERO-TEST-1-1.                                                  NC1074.2
132500     IF       DATA-F EQUAL TO "          "                        NC1074.2
132600              PERFORM PASS                                        NC1074.2
132700              ELSE                                                NC1074.2
132800              GO TO BZERO-FAIL-1.                                 NC1074.2
132900     GO TO    BZERO-WRITE-1.                                      NC1074.2
133000 BZERO-DELETE-1.                                                  NC1074.2
133100     PERFORM  DE-LETE.                                            NC1074.2
133200     GO TO    BZERO-WRITE-1.                                      NC1074.2
133300 BZERO-FAIL-1.                                                    NC1074.2
133400     MOVE     DATA-F TO COMPUTED-A.                               NC1074.2
133500     MOVE "SHOULD BE BLANK" TO CORRECT-A.                         NC1074.2
133600     PERFORM  FAIL.                                               NC1074.2
133700 BZERO-WRITE-1.                                                   NC1074.2
133800     MOVE "BZERO-TEST-1" TO PAR-NAME.                             NC1074.2
133900     PERFORM  PRINT-DETAIL.                                       NC1074.2
134000 BZERO-INIT-2.                                                    NC1074.2
134100     MOVE   "VI-22 5.4" TO ANSI-REFERENCE.                        NC1074.2
134200     MOVE     0000 TO DATA-M.                                     NC1074.2
134300 BZERO-TEST-2.                                                    NC1074.2
134400     IF       DATA-M EQUAL TO SPACE                               NC1074.2
134500              PERFORM PASS                                        NC1074.2
134600              GO TO BZERO-WRITE-2.                                NC1074.2
134700     GO TO    BZERO-FAIL-2.                                       NC1074.2
134800 BZERO-DELETE-2.                                                  NC1074.2
134900     PERFORM  DE-LETE.                                            NC1074.2
135000     GO TO    BZERO-WRITE-2.                                      NC1074.2
135100 BZERO-FAIL-2.                                                    NC1074.2
135200     MOVE     DATA-M TO COMPUTED-A.                               NC1074.2
135300     MOVE "SHOULD BE BLANK" TO CORRECT-A.                         NC1074.2
135400     PERFORM  FAIL.                                               NC1074.2
135500 BZERO-WRITE-2.                                                   NC1074.2
135600     MOVE "BZERO-TEST-2" TO PAR-NAME.                             NC1074.2
135700     PERFORM  PRINT-DETAIL.                                       NC1074.2
135800 BZERO-INIT-3.                                                    NC1074.2
135900     MOVE   "VI-22 5.4" TO ANSI-REFERENCE.                        NC1074.2
136000 BZERO-TEST-3.                                                    NC1074.2
136100     IF       DATA-P1 EQUAL TO "000"                              NC1074.2
136200              PERFORM PASS GO TO BZERO-WRITE-3.                   NC1074.2
136300     GO       TO BZERO-FAIL-3.                                    NC1074.2
136400 BZERO-DELETE-3.                                                  NC1074.2
136500     PERFORM  DE-LETE.                                            NC1074.2
136600     GO       TO BZERO-WRITE-3.                                   NC1074.2
136700 BZERO-FAIL-3.                                                    NC1074.2
136800     PERFORM FAIL.                                                NC1074.2
136900     MOVE     DATA-P1 TO COMPUTED-A.                              NC1074.2
137000     MOVE     "000" TO CORRECT-A.                                 NC1074.2
137100 BZERO-WRITE-3.                                                   NC1074.2
137200     MOVE     "BZERO-TEST-3" TO PAR-NAME.                         NC1074.2
137300     PERFORM  PRINT-DETAIL.                                       NC1074.2
137400 LONG-PARAGRAPH-NAME-----INIT-1.                                  NC1074.2
137500     MOVE   "IV-9 4.2.2.2.1" TO ANSI-REFERENCE.                   NC1074.2
137600 LONG-PARAGRAPH-NAME-----TEST-1.                                  NC1074.2
137700     PERFORM  PASS.                                               NC1074.2
137800     GO TO    LONG-WRITE-1.                                       NC1074.2
137900 LONG-DELETE-1.                                                   NC1074.2
138000     PERFORM  DE-LETE.                                            NC1074.2
138100 LONG-WRITE-1.                                                    NC1074.2
138200     MOVE "LONG PARAGRAPH-NAME" TO FEATURE.                       NC1074.2
138300     MOVE "LONG-PARAGRAPH---ETC" TO PAR-NAME.                     NC1074.2
138400     PERFORM  PRINT-DETAIL.                                       NC1074.2
138500 LONG-INIT-2.                                                     NC1074.2
138600     MOVE   "IV-9 4.2.2.2.1" TO ANSI-REFERENCE.                   NC1074.2
138700 LONG-TEST-2.                                                     NC1074.2
138800     MOVE     SPACE TO A-DATA-NAME-30-CHARACTERS-LONG.            NC1074.2
138900     PERFORM  PASS.                                               NC1074.2
139000     GO TO    LONG-WRITE-2.                                       NC1074.2
139100 LONG-DELETE-2.                                                   NC1074.2
139200     PERFORM  DE-LETE.                                            NC1074.2
139300 LONG-WRITE-2.                                                    NC1074.2
139400     MOVE "LONG DATA-NAME" TO FEATURE.                            NC1074.2
139500     MOVE "LONG-TEST-2" TO PAR-NAME.                              NC1074.2
139600     PERFORM  PRINT-DETAIL.                                       NC1074.2
139700 LONG-INIT-3.                                                     NC1074.2
139800     MOVE   "IV-9 4.2.2.2.1" TO ANSI-REFERENCE.                   NC1074.2
139900 LONG-TEST-3.                                                     NC1074.2
140000     MOVE     SPACE TO LONG-PICTURE.                              NC1074.2
140100     PERFORM  PASS.                                               NC1074.2
140200     GO TO    LONG-WRITE-3.                                       NC1074.2
140300 LONG-DELETE-3.                                                   NC1074.2
140400     PERFORM  DE-LETE.                                            NC1074.2
140500 LONG-WRITE-3.                                                    NC1074.2
140600     MOVE "LONG PICTURE" TO FEATURE.                              NC1074.2
140700     MOVE "LONG-TEST-3" TO PAR-NAME.                              NC1074.2
140800     PERFORM  PRINT-DETAIL.                                       NC1074.2
140900 LONG-INIT-4.                                                     NC1074.2
141000     MOVE   "IV-9 4.2.2.2.1" TO ANSI-REFERENCE.                   NC1074.2
141100 LONG-TEST-4.                                                     NC1074.2
141200     MOVE     "LONG-TEST-4" TO PAR-NAME.                          NC1074.2
141300     IF       LONG-LITERAL EQUAL TO "STANDARD COMPILERS MUST ALLOWNC1074.2
141400-    " NON-NUMERIC LITERALS OF AT LEAST 120 CHARACTERS AND NUMERICNC1074.2
141500-    " LITERALS OF AT LEAST 18 DIGITS BUT NOW EXTENDED UPTO 160 DINC1074.2
141600-    "GITS FOR 8X"                                                NC1074.2
141700              PERFORM PASS                                        NC1074.2
141800     ELSE                                                         NC1074.2
141900              GO TO LONG-FAIL-4.                                  NC1074.2
142000     GO TO    LONG-WRITE-4.                                       NC1074.2
142100 LONG-DELETE-4.                                                   NC1074.2
142200     MOVE     "LONG-TEST-4" TO PAR-NAME.                          NC1074.2
142300     PERFORM  DE-LETE.                                            NC1074.2
142400     GO TO    LONG-WRITE-4.                                       NC1074.2
142500 LONG-FAIL-4.                                                     NC1074.2
142600     PERFORM  FAIL.                                               NC1074.2
142700     MOVE    SPACE TO TEST-RESULTS.                               NC1074.2
142800     MOVE    LONG20 TO COMPUTED-A.                                NC1074.2
142900     PERFORM PRINT-DETAIL.                                        NC1074.2
143000     MOVE    LONG40 TO COMPUTED-A.                                NC1074.2
143100     PERFORM PRINT-DETAIL.                                        NC1074.2
143200     MOVE    LONG60 TO COMPUTED-A.                                NC1074.2
143300     PERFORM PRINT-DETAIL.                                        NC1074.2
143400     MOVE    LONG80 TO COMPUTED-A.                                NC1074.2
143500     PERFORM PRINT-DETAIL.                                        NC1074.2
143600     MOVE    LONG100 TO COMPUTED-A.                               NC1074.2
143700     PERFORM PRINT-DETAIL.                                        NC1074.2
143800     MOVE    LONG120 TO COMPUTED-A.                               NC1074.2
143900     PERFORM PRINT-DETAIL.                                        NC1074.2
144000     MOVE    LONG140 TO COMPUTED-A.                               NC1074.2
144100     PERFORM PRINT-DETAIL.                                        NC1074.2
144200     MOVE    LONG160 TO COMPUTED-A.                               NC1074.2
144300     PERFORM PRINT-DETAIL.                                        NC1074.2
144400     MOVE   "SEE PROGRAM" TO RE-MARK.                             NC1074.2
144500 LONG-WRITE-4.                                                    NC1074.2
144600     MOVE "LONG NON-NUM LITERAL" TO FEATURE.                      NC1074.2
144700     PERFORM PRINT-DETAIL.                                        NC1074.2
144800 LONG-INIT-5.                                                     NC1074.2
144900     MOVE   "IV-9 4.2.2.2.1" TO ANSI-REFERENCE.                   NC1074.2
145000     MOVE    211113411,114311112 TO LONG-NUMBER.                  NC1074.2
145100 LONG-TEST-5.                                                     NC1074.2
145200     IF LONG-NUMBER EQUAL TO 211113411,114311112 PERFORM PASS     NC1074.2
145300     ELSE GO TO LONG-FAIL-5.                                      NC1074.2
145400     GO TO LONG-WRITE-5.                                          NC1074.2
145500 LONG-DELETE-5.                                                   NC1074.2
145600     PERFORM DE-LETE.                                             NC1074.2
145700     GO TO LONG-WRITE-5.                                          NC1074.2
145800 LONG-FAIL-5.                                                     NC1074.2
145900     MOVE LONG-NUMBER TO COMPUTED-N.                              NC1074.2
146000     MOVE " 211113411,114311112" TO CORRECT-A.                    NC1074.2
146100     PERFORM FAIL.                                                NC1074.2
146200 LONG-WRITE-5.                                                    NC1074.2
146300     MOVE "LONG NUMERIC LITERAL" TO FEATURE.                      NC1074.2
146400     MOVE "LONG-TEST-5" TO PAR-NAME.                              NC1074.2
146500     PERFORM PRINT-DETAIL.                                        NC1074.2
146600 LONG-INIT-6.                                                     NC1074.2
146700     MOVE   "IV-9 4.2.2.2.1" TO ANSI-REFERENCE.                   NC1074.2
146800     MOVE     "ABCDEFGHIJKLMNOPQRST" TO LONG-PICTURE-A.           NC1074.2
146900 LONG-TEST-6.                                                     NC1074.2
147000     MOVE     LONG-PICTURE-A TO LONG-PICTURE-B.                   NC1074.2
147100     MOVE     LONG-PICTURE-B TO LONG-PICTURE-C.                   NC1074.2
147200     IF       LONG-PICTURE-C EQUAL TO "FGHIJKLMNO"                NC1074.2
147300              PERFORM PASS GO TO LONG-WRITE-6.                    NC1074.2
147400     GO       TO LONG-FAIL-6.                                     NC1074.2
147500 LONG-DELETE-6.                                                   NC1074.2
147600     PERFORM  DE-LETE.                                            NC1074.2
147700     GO       TO LONG-WRITE-6.                                    NC1074.2
147800 LONG-FAIL-6.                                                     NC1074.2
147900     MOVE     LONG-PICTURE-C TO COMPUTED-A.                       NC1074.2
148000     MOVE     "FGHIJKLMNO" TO CORRECT-A.                          NC1074.2
148100     PERFORM  FAIL.                                               NC1074.2
148200 LONG-WRITE-6.                                                    NC1074.2
148300     MOVE     "LONG PICTURE" TO FEATURE.                          NC1074.2
148400     MOVE     "LONG-TEST-6" TO PAR-NAME.                          NC1074.2
148500     PERFORM  PRINT-DETAIL.                                       NC1074.2
148600 RDF-INIT-1.                                                      NC1074.2
148700     MOVE   "VI-38 5.10" TO ANSI-REFERENCE.                       NC1074.2
148800     MOVE "REDEFINES" TO FEATURE.                                 NC1074.2
148900 RDF-TEST-1-0.                                                    NC1074.2
149000     MOVE "5" TO REDEF2.                                          NC1074.2
149100 RDF-TEST-1-1.                                                    NC1074.2
149200     IF REDEF1 EQUAL TO 5 PERFORM PASS GO TO RDF-WRITE-1.         NC1074.2
149300     GO TO RDF-FAIL-1.                                            NC1074.2
149400 RDF-DELETE-1.                                                    NC1074.2
149500     PERFORM DE-LETE.                                             NC1074.2
149600     GO TO RDF-WRITE-1.                                           NC1074.2
149700 RDF-FAIL-1.                                                      NC1074.2
149800     MOVE REDEF1 TO COMPUTED-A.                                   NC1074.2
149900     MOVE "5" TO CORRECT-A.                                       NC1074.2
150000     PERFORM FAIL.                                                NC1074.2
150100 RDF-WRITE-1.                                                     NC1074.2
150200     MOVE "RDF-TEST-1" TO PAR-NAME.                               NC1074.2
150300     PERFORM PRINT-DETAIL.                                        NC1074.2
150400 RDF-INIT-2.                                                      NC1074.2
150500     MOVE   "VI-38 5.10" TO ANSI-REFERENCE.                       NC1074.2
150600 RDF-TEST-2-0.                                                    NC1074.2
150700     MOVE "W" TO REDEF4.                                          NC1074.2
150800 RDF-TEST-2-1.                                                    NC1074.2
150900     IF REDEF3 EQUAL TO "WBC" PERFORM PASS GO TO RDF-WRITE-2.     NC1074.2
151000     GO TO RDF-FAIL-2.                                            NC1074.2
151100 RDF-DELETE-2.                                                    NC1074.2
151200     PERFORM DE-LETE.                                             NC1074.2
151300     GO TO RDF-WRITE-2.                                           NC1074.2
151400 RDF-FAIL-2.                                                      NC1074.2
151500     MOVE REDEF3 TO COMPUTED-A.                                   NC1074.2
151600     MOVE "WBC" TO CORRECT-A.                                     NC1074.2
151700     PERFORM FAIL.                                                NC1074.2
151800 RDF-WRITE-2.                                                     NC1074.2
151900     MOVE "RDF-TEST-2" TO PAR-NAME.                               NC1074.2
152000     PERFORM PRINT-DETAIL.                                        NC1074.2
152100 RDF-INIT-3.                                                      NC1074.2
152200     MOVE   "VI-38 5.10" TO ANSI-REFERENCE.                       NC1074.2
152300     MOVE 123456 TO REDEF6.                                       NC1074.2
152400     MOVE "AB" TO REDEF8A.                                        NC1074.2
152500     MOVE "EF" TO REDEF8C.                                        NC1074.2
152600 RDF-TEST-3.                                                      NC1074.2
152700     IF REDEF5 EQUAL TO "AB34EF" PERFORM PASS GO TO RDF-WRITE-3.  NC1074.2
152800     GO TO RDF-FAIL-3.                                            NC1074.2
152900 RDF-DELETE-3.                                                    NC1074.2
153000     PERFORM DE-LETE.                                             NC1074.2
153100     GO TO RDF-WRITE-3.                                           NC1074.2
153200 RDF-FAIL-3.                                                      NC1074.2
153300     MOVE REDEF5 TO COMPUTED-A.                                   NC1074.2
153400     MOVE "AB34EF" TO CORRECT-A.                                  NC1074.2
153500     PERFORM FAIL.                                                NC1074.2
153600 RDF-WRITE-3.                                                     NC1074.2
153700     MOVE "RDF-TEST-3" TO PAR-NAME.                               NC1074.2
153800     PERFORM PRINT-DETAIL.                                        NC1074.2
153900 RDF-INIT-4.                                                      NC1074.2
154000     MOVE   "VI-38 5.10" TO ANSI-REFERENCE.                       NC1074.2
154100 RDF-TEST-4.                                                      NC1074.2
154200     IF RDFDATA7 EQUAL TO "ABC98765DE911644ALLD"                  NC1074.2
154300         PERFORM PASS                                             NC1074.2
154400         GO TO RDF-WRITE-4.                                       NC1074.2
154500     MOVE RDFDATA7 TO COMPUTED-A.                                 NC1074.2
154600     MOVE "ABC98765DE911644ALLD" TO CORRECT-A.                    NC1074.2
154700     PERFORM FAIL.                                                NC1074.2
154800     GO TO RDF-WRITE-4.                                           NC1074.2
154900 RDF-DELETE-4.                                                    NC1074.2
155000     PERFORM DE-LETE.                                             NC1074.2
155100 RDF-WRITE-4.                                                     NC1074.2
155200     MOVE "RDF-TEST-4 " TO PAR-NAME.                              NC1074.2
155300     PERFORM PRINT-DETAIL.                                        NC1074.2
155400 RDF-INIT-5.                                                      NC1074.2
155500     MOVE   "VI-38 5.10" TO ANSI-REFERENCE.                       NC1074.2
155600 RDF-TEST-5.                                                      NC1074.2
155700     IF RDFDATA8 (13) EQUAL TO "HG"                               NC1074.2
155800         PERFORM PASS                                             NC1074.2
155900         GO TO RDF-WRITE-5.                                       NC1074.2
156000     MOVE "HG" TO CORRECT-A.                                      NC1074.2
156100     MOVE RDFDATA8 (13) TO COMPUTED-A.                            NC1074.2
156200     PERFORM FAIL.                                                NC1074.2
156300     GO TO RDF-WRITE-5.                                           NC1074.2
156400 RDF-DELETE-5.                                                    NC1074.2
156500     PERFORM DE-LETE.                                             NC1074.2
156600 RDF-WRITE-5.                                                     NC1074.2
156700     MOVE "RDF-TEST-5 " TO PAR-NAME.                              NC1074.2
156800     PERFORM PRINT-DETAIL.                                        NC1074.2
156900 RDF-INIT-6.                                                      NC1074.2
157000     MOVE   "VI-38 5.10" TO ANSI-REFERENCE.                       NC1074.2
157100 RDF-TEST-6.                                                      NC1074.2
157200     IF RDFDATA2 EQUAL TO 9116,44                                 NC1074.2
157300         PERFORM PASS                                             NC1074.2
157400         GO TO RDF-WRITE-6.                                       NC1074.2
157500     MOVE 9116,44 TO COMPUTED-N.                                  NC1074.2
157600     MOVE RDFDATA2 TO CORRECT-N.                                  NC1074.2
157700     PERFORM FAIL.                                                NC1074.2
157800     GO TO RDF-WRITE-6.                                           NC1074.2
157900 RDF-DELETE-6.                                                    NC1074.2
158000     PERFORM DE-LETE.                                             NC1074.2
158100 RDF-WRITE-6.                                                     NC1074.2
158200     MOVE "RDF-TEST-6 " TO PAR-NAME.                              NC1074.2
158300     PERFORM PRINT-DETAIL.                                        NC1074.2
158400 RDF-INIT-7.                                                      NC1074.2
158500     MOVE   "VI-38 5.10" TO ANSI-REFERENCE.                       NC1074.2
158600 RDF-TEST-7.                                                      NC1074.2
158700     IF RDFDATA16 EQUAL TO 66                                     NC1074.2
158800         PERFORM PASS                                             NC1074.2
158900         GO TO RDF-WRITE-7.                                       NC1074.2
159000     MOVE RDFDATA16 TO COMPUTED-A.                                NC1074.2
159100     MOVE 66 TO CORRECT-A.                                        NC1074.2
159200     PERFORM FAIL.                                                NC1074.2
159300     GO TO RDF-WRITE-7.                                           NC1074.2
159400 RDF-DELETE-7.                                                    NC1074.2
159500     PERFORM DE-LETE.                                             NC1074.2
159600 RDF-WRITE-7.                                                     NC1074.2
159700     MOVE "RDF-TEST-7 " TO PAR-NAME.                              NC1074.2
159800     PERFORM PRINT-DETAIL.                                        NC1074.2
159900 RDF-INIT-8.                                                      NC1074.2
160000     MOVE   "VI-38 5.10" TO ANSI-REFERENCE.                       NC1074.2
160100     MOVE SPACE TO REDEF12.                                       NC1074.2
160200 RDF-TEST-8.                                                      NC1074.2
160300     IF REDEF11 EQUAL TO SPACE                                    NC1074.2
160400         PERFORM PASS                                             NC1074.2
160500         GO TO RDF-WRITE-8.                                       NC1074.2
160600     MOVE "SPACE EXPECTED " TO CORRECT-A.                         NC1074.2
160700     MOVE "NON BLANK CHARACTERS" TO COMPUTED-A.                   NC1074.2
160800     MOVE "REDEF11 CONTAINS NON BLANKS" TO RE-MARK.               NC1074.2
160900     PERFORM FAIL.                                                NC1074.2
161000     GO TO RDF-WRITE-8.                                           NC1074.2
161100 RDF-DELETE-8.                                                    NC1074.2
161200     PERFORM DE-LETE.                                             NC1074.2
161300 RDF-WRITE-8.                                                     NC1074.2
161400     MOVE "RDF-TEST-8 " TO PAR-NAME.                              NC1074.2
161500     PERFORM PRINT-DETAIL.                                        NC1074.2
161600 RDF-INIT-9.                                                      NC1074.2
161700     MOVE   "VI-38 5.10" TO ANSI-REFERENCE.                       NC1074.2
161800     MOVE ZERO TO REDEF12.                                        NC1074.2
161900     MOVE SPACE TO REDEF11.                                       NC1074.2
162000 RDF-TEST-9.                                                      NC1074.2
162100     IF RDFDATA18 EQUAL TO ZERO                                   NC1074.2
162200         PERFORM PASS                                             NC1074.2
162300         GO TO RDF-WRITE-9.                                       NC1074.2
162400     MOVE "00000000000000" TO CORRECT-A.                          NC1074.2
162500     MOVE RDFDATA18 TO COMPUTED-A.                                NC1074.2
162600     PERFORM FAIL.                                                NC1074.2
162700     GO TO RDF-WRITE-9.                                           NC1074.2
162800 RDF-DELETE-9.                                                    NC1074.2
162900     PERFORM DE-LETE.                                             NC1074.2
163000 RDF-WRITE-9.                                                     NC1074.2
163100     MOVE "RDF-TEST-9 " TO PAR-NAME.                              NC1074.2
163200     PERFORM PRINT-DETAIL.                                        NC1074.2
163300 RDF-INIT-10.                                                     NC1074.2
163400     MOVE   "VI-38 5.10" TO ANSI-REFERENCE.                       NC1074.2
163500     MOVE ZERO TO REDEF12.                                        NC1074.2
163600     MOVE "MOVING DATA TO A REDEFINED FIELD CAN BE RISKY "        NC1074.2
163700         TO REDEF10.                                              NC1074.2
163800 RDF-TEST-10.                                                     NC1074.2
163900     IF RDFDATA8 (14) EQUAL TO "00"                               NC1074.2
164000         PERFORM PASS                                             NC1074.2
164100         GO TO RDF-WRITE-10.                                      NC1074.2
164200     MOVE 00 TO CORRECT-A.                                        NC1074.2
164300     MOVE RDFDATA8 (14) TO COMPUTED-A.                            NC1074.2
164400     PERFORM FAIL.                                                NC1074.2
164500     GO TO RDF-WRITE-10.                                          NC1074.2
164600 RDF-DELETE-10.                                                   NC1074.2
164700     PERFORM DE-LETE.                                             NC1074.2
164800 RDF-WRITE-10.                                                    NC1074.2
164900     MOVE "RDF-TEST-10 " TO PAR-NAME.                             NC1074.2
165000     PERFORM PRINT-DETAIL.                                        NC1074.2
165100 RDF-INIT-11.                                                     NC1074.2
165200     MOVE   "VI-38 5.10" TO ANSI-REFERENCE.                       NC1074.2
165300 RDF-TEST-11.                                                     NC1074.2
165400     MOVE REDEF13 TO REDEF12.                                     NC1074.2
165500     IF REDEF10 EQUAL TO                                          NC1074.2
165600         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"         NC1074.2
165700         PERFORM PASS                                             NC1074.2
165800         GO TO RDF-WRITE-11.                                      NC1074.2
165900     MOVE "ALPHABETIC A 46 LONG" TO CORRECT-A  COMPUTED-A.        NC1074.2
166000     MOVE "FIELDS DIDNT COMPARE EQUAL " TO RE-MARK.               NC1074.2
166100     PERFORM FAIL.                                                NC1074.2
166200     GO TO RDF-WRITE-11.                                          NC1074.2
166300 RDF-DELETE-11.                                                   NC1074.2
166400     PERFORM DE-LETE.                                             NC1074.2
166500 RDF-WRITE-11.                                                    NC1074.2
166600     MOVE "RDF-TEST-11 " TO PAR-NAME.                             NC1074.2
166700     PERFORM PRINT-DETAIL.                                        NC1074.2
166800 RDF-INIT-12.                                                     NC1074.2
166900     MOVE   "VI-38 5.10.3 SR6" TO ANSI-REFERENCE.                 NC1074.2
167000     MOVE "ABC98765DE" TO REDEF20.                                NC1074.2
167100 RDF-TEST-12.                                                     NC1074.2
167200     IF    REDEF22 = "ABC98765DE"                                 NC1074.2
167300           PERFORM PASS                                           NC1074.2
167400           GO TO   RDF-WRITE-12.                                  NC1074.2
167500     GO TO RDF-FAIL-12.                                           NC1074.2
167600 RDF-DELETE-12.                                                   NC1074.2
167700     PERFORM DE-LETE.                                             NC1074.2
167800     GO TO RDF-WRITE-12.                                          NC1074.2
167900 RDF-FAIL-12.                                                     NC1074.2
168000     MOVE REDEF22 TO COMPUTED-A.                                  NC1074.2
168100     MOVE "ABC98765DE" TO CORRECT-A.                              NC1074.2
168200     PERFORM FAIL.                                                NC1074.2
168300 RDF-WRITE-12.                                                    NC1074.2
168400     MOVE "RDF-TEST-12 " TO PAR-NAME.                             NC1074.2
168500     PERFORM PRINT-DETAIL.                                        NC1074.2
168600 RDF-INIT-13.                                                     NC1074.2
168700     MOVE   "VI-38 5.10.3 SR6" TO ANSI-REFERENCE.                 NC1074.2
168800     MOVE "0987654321" TO REDEF22.                                NC1074.2
168900 RDF-TEST-13.                                                     NC1074.2
169000     IF    REDEF23 = "098765432"                                  NC1074.2
169100           PERFORM PASS                                           NC1074.2
169200           GO TO   RDF-WRITE-13.                                  NC1074.2
169300     GO TO RDF-FAIL-13.                                           NC1074.2
169400 RDF-DELETE-13.                                                   NC1074.2
169500     PERFORM DE-LETE.                                             NC1074.2
169600     GO TO RDF-WRITE-13.                                          NC1074.2
169700 RDF-FAIL-13.                                                     NC1074.2
169800     MOVE REDEF22 TO COMPUTED-A.                                  NC1074.2
169900     MOVE "098765432" TO CORRECT-A.                               NC1074.2
170000     PERFORM FAIL.                                                NC1074.2
170100 RDF-WRITE-13.                                                    NC1074.2
170200     MOVE "RDF-TEST-13" TO PAR-NAME.                              NC1074.2
170300     PERFORM PRINT-DETAIL.                                        NC1074.2
170400 USAGE-INIT-1.                                                    NC1074.2
170500     MOVE   "VI-46 5.14" TO ANSI-REFERENCE.                       NC1074.2
170600     MOVE "USAGE" TO FEATURE.                                     NC1074.2
170700 USAGE-TEST-1.                                                    NC1074.2
170800     IF       U2 GREATER THAN U7 PERFORM PASS ELSE PERFORM FAIL.  NC1074.2
170900     GO TO USAGE-WRITE-1.                                         NC1074.2
171000 USAGE-DELETE-1.                                                  NC1074.2
171100     PERFORM DE-LETE.                                             NC1074.2
171200 USAGE-WRITE-1.                                                   NC1074.2
171300     MOVE "USAGE-TEST-1" TO PAR-NAME.                             NC1074.2
171400     PERFORM PRINT-DETAIL.                                        NC1074.2
171500 USAGE-INIT-2.                                                    NC1074.2
171600     MOVE   "VI-46 5.14" TO ANSI-REFERENCE.                       NC1074.2
171700 USAGE-TEST-2.                                                    NC1074.2
171800     IF U2 EQUAL TO U4 PERFORM PASS ELSE PERFORM FAIL.            NC1074.2
171900     GO TO USAGE-WRITE-2.                                         NC1074.2
172000 USAGE-DELETE-2.                                                  NC1074.2
172100     PERFORM DE-LETE.                                             NC1074.2
172200 USAGE-WRITE-2.                                                   NC1074.2
172300     MOVE "USAGE-TEST-2" TO PAR-NAME.                             NC1074.2
172400     PERFORM PRINT-DETAIL.                                        NC1074.2
172500 USAGE-INIT-3.                                                    NC1074.2
172600     MOVE   "VI-46 5.14" TO ANSI-REFERENCE.                       NC1074.2
172700 USAGE-TEST-3.                                                    NC1074.2
172800     IF U3 EQUAL TO U4 PERFORM PASS ELSE PERFORM FAIL.            NC1074.2
172900     GO TO USAGE-WRITE-3.                                         NC1074.2
173000 USAGE-DELETE-3.                                                  NC1074.2
173100     PERFORM DE-LETE.                                             NC1074.2
173200 USAGE-WRITE-3.                                                   NC1074.2
173300     MOVE "USAGE-TEST-3" TO PAR-NAME.                             NC1074.2
173400     PERFORM PRINT-DETAIL.                                        NC1074.2
173500 USAGE-INIT-4.                                                    NC1074.2
173600     MOVE   "VI-46 5.14" TO ANSI-REFERENCE.                       NC1074.2
173700 USAGE-TEST-4-0.                                                  NC1074.2
173800     MOVE U5 TO U9.                                               NC1074.2
173900 USAGE-TEST-4-1.                                                  NC1074.2
174000     IF       U6 EQUAL TO U10                                     NC1074.2
174100         PERFORM PASS                                             NC1074.2
174200         GO TO USAGE-WRITE-4.                                     NC1074.2
174300     GO TO USAGE-FAIL-4.                                          NC1074.2
174400 USAGE-DELETE-4.                                                  NC1074.2
174500     PERFORM DE-LETE.                                             NC1074.2
174600     GO TO USAGE-WRITE-4.                                         NC1074.2
174700 USAGE-FAIL-4.                                                    NC1074.2
174800     MOVE     U10 TO COMPUTED-N.                                  NC1074.2
174900     MOVE     U6 TO CORRECT-N.                                    NC1074.2
175000     PERFORM FAIL.                                                NC1074.2
175100 USAGE-WRITE-4.                                                   NC1074.2
175200     MOVE "USAGE-TEST-4" TO PAR-NAME.                             NC1074.2
175300     PERFORM PRINT-DETAIL.                                        NC1074.2
175400 USAGE-INIT-5.                                                    NC1074.2
175500     MOVE   "VI-46 5.14" TO ANSI-REFERENCE.                       NC1074.2
175600 USAGE-TEST-5-0.                                                  NC1074.2
175700     MOVE     U5 TO U9.                                           NC1074.2
175800 USAGE-TEST-5-1.                                                  NC1074.2
175900     IF       U7 EQUAL TO U11                                     NC1074.2
176000              PERFORM PASS                                        NC1074.2
176100              GO TO USAGE-WRITE-5.                                NC1074.2
176200     MOVE     U7 TO CORRECT-N.                                    NC1074.2
176300     MOVE     U11 TO COMPUTED-N.                                  NC1074.2
176400     PERFORM FAIL.                                                NC1074.2
176500     GO TO USAGE-WRITE-5.                                         NC1074.2
176600 USAGE-DELETE-5.                                                  NC1074.2
176700     PERFORM  DE-LETE.                                            NC1074.2
176800 USAGE-WRITE-5.                                                   NC1074.2
176900     MOVE     "USAGE-TEST-5" TO PAR-NAME.                         NC1074.2
177000     PERFORM  PRINT-DETAIL.                                       NC1074.2
177100 USAGE-INIT-6.                                                    NC1074.2
177200     MOVE   "VI-47 5.14.4 GR3 GR9" TO ANSI-REFERENCE.             NC1074.2
177300 USAGE-TEST-6.                                                    NC1074.2
177400     IF      U22 GREATER THAN U12 PERFORM PASS ELSE PERFORM FAIL. NC1074.2
177500     GO TO USAGE-WRITE-6.                                         NC1074.2
177600 USAGE-DELETE-6.                                                  NC1074.2
177700     PERFORM DE-LETE.                                             NC1074.2
177800 USAGE-WRITE-6.                                                   NC1074.2
177900     MOVE "USAGE-TEST-6" TO PAR-NAME.                             NC1074.2
178000     PERFORM PRINT-DETAIL.                                        NC1074.2
178100 USAGE-INIT-7.                                                    NC1074.2
178200     MOVE   "VI-47 5.14.4 GR3 GR9" TO ANSI-REFERENCE.             NC1074.2
178300 USAGE-TEST-7.                                                    NC1074.2
178400     IF      U23 GREATER THAN U13 PERFORM PASS ELSE PERFORM FAIL. NC1074.2
178500     GO TO USAGE-WRITE-7.                                         NC1074.2
178600 USAGE-DELETE-7.                                                  NC1074.2
178700     PERFORM DE-LETE.                                             NC1074.2
178800 USAGE-WRITE-7.                                                   NC1074.2
178900     MOVE "USAGE-TEST-7" TO PAR-NAME.                             NC1074.2
179000     PERFORM PRINT-DETAIL.                                        NC1074.2
179100 VALUE-INIT-1.                                                    NC1074.2
179200     MOVE   "VI-50 5.15.6 (6)" TO ANSI-REFERENCE.                 NC1074.2
179300     MOVE   "VALUE FOR OCCURS FIELD" TO FEATURE.                  NC1074.2
179400     MOVE   "VALUE TESTS 1, 2 & 3"   TO PAR-NAME.                 NC1074.2
179500 VALUE-TEST-1.                                                    NC1074.2
179600     MOVE 1 TO SUB1.                                              NC1074.2
179700     PERFORM VALUE-TEST-2                                         NC1074.2
179800                            UNTIL SUB1 > 10.                      NC1074.2
179900     GO TO   CURR-TEST-1.                                         NC1074.2
180000 VALUE-TEST-2.                                                    NC1074.2
180100     MOVE 1 TO SUB2.                                              NC1074.2
180200     PERFORM VALUE-TEST-3                                         NC1074.2
180300                            UNTIL SUB2 > 10.                      NC1074.2
180400     ADD 1 TO SUB1.                                               NC1074.2
180500 VALUE-TEST-3.                                                    NC1074.2
180600     MOVE    SUB1 TO TAB1.                                        NC1074.2
180700     MOVE    SUB2 TO TAB2.                                        NC1074.2
180800     MOVE    TAB-LOC TO PAR-NAME.                                 NC1074.2
180900     IF      VALUE-TABLE-2  (SUB1 SUB2) = "AZ"                    NC1074.2
181000             PERFORM PASS                                         NC1074.2
181100     ELSE                                                         NC1074.2
181200             MOVE    VALUE-TABLE-2 (SUB1 SUB2) TO COMPUTED-A      NC1074.2
181300             MOVE   "AZ" TO CORRECT-A                             NC1074.2
181400             PERFORM FAIL.                                        NC1074.2
181500     PERFORM PRINT-DETAIL.                                        NC1074.2
181600     ADD 1 TO SUB2.                                               NC1074.2
181700 VALUE-DELETE-1.                                                  NC1074.2
181800     PERFORM DE-LETE.                                             NC1074.2
181900     PERFORM PRINT-DETAIL.                                        NC1074.2
182000 CURR-TEST-1.                                                     NC1074.2
182100     MOVE DATA-I TO DATA-J.                                       NC1074.2
182200     IF  DATA-J EQUAL TO "  W12" PERFORM PASS GO TO CURR-WRITE-1. NC1074.2
182300     GO TO CURR-FAIL-1.                                           NC1074.2
182400 CURR-DELETE-1.                                                   NC1074.2
182500     PERFORM DE-LETE.                                             NC1074.2
182600     GO TO CURR-WRITE-1.                                          NC1074.2
182700 CURR-FAIL-1.                                                     NC1074.2
182800     MOVE DATA-J TO COMPUTED-A.                                   NC1074.2
182900     MOVE     "  W12" TO CORRECT-A.                               NC1074.2
183000     PERFORM FAIL.                                                NC1074.2
183100 CURR-WRITE-1.                                                    NC1074.2
183200     MOVE "CURRENCY SIGN IS" TO FEATURE.                          NC1074.2
183300     MOVE "CURR-TEST-1" TO PAR-NAME.                              NC1074.2
183400     PERFORM PRINT-DETAIL.                                        NC1074.2
183500 DCOM-TEST-1.                                                     NC1074.2
183600     MOVE DATA-K TO DATA-L.                                       NC1074.2
183700     IF DATA-L EQUAL TO "1.234.567,89" PERFORM PASS               NC1074.2
183800     GO TO DCOM-WRITE-1.                                          NC1074.2
183900     GO TO DCOM-FAIL-1.                                           NC1074.2
184000 DCOM-DELETE-1.                                                   NC1074.2
184100     PERFORM DE-LETE.                                             NC1074.2
184200     GO TO DCOM-WRITE-1.                                          NC1074.2
184300 DCOM-FAIL-1.                                                     NC1074.2
184400     MOVE DATA-L TO COMPUTED-A.                                   NC1074.2
184500     MOVE "1.234.567,89" TO CORRECT-A.                            NC1074.2
184600     PERFORM FAIL.                                                NC1074.2
184700 DCOM-WRITE-1.                                                    NC1074.2
184800     MOVE "DECIMAL IS COMMA" TO FEATURE.                          NC1074.2
184900     MOVE "DCOM-TEST-1" TO PAR-NAME.                              NC1074.2
185000     PERFORM PRINT-DETAIL.                                        NC1074.2
185100 DCOM-INIT-2.                                                     NC1074.2
185200     MOVE   "123456789." TO TEST-FIELD.                           NC1074.2
185300 DCOM-TEST-2-1.                                                   NC1074.2
185400     IF     TEST-FIELD = "123456789."                             NC1074.2
185500            PERFORM PASS                                          NC1074.2
185600            GO TO DCOM-WRITE-2.                                   NC1074.2
185700     GO TO  DCOM-FAIL-2.                                          NC1074.2
185800 DCOM-DELETE-2.                                                   NC1074.2
185900     PERFORM DE-LETE.                                             NC1074.2
186000     GO TO   DCOM-WRITE-2.                                        NC1074.2
186100 DCOM-FAIL-2.                                                     NC1074.2
186200     MOVE    TEST-FIELD  TO COMPUTED-A.                           NC1074.2
186300     MOVE   "123456789." TO CORRECT-A.                            NC1074.2
186400     PERFORM FAIL.                                                NC1074.2
186500 DCOM-WRITE-2.                                                    NC1074.2
186600     MOVE   "DCOM-TEST-2" TO PAR-NAME.                            NC1074.2
186700     PERFORM PRINT-DETAIL.                                        NC1074.2
186800 DCOM-INIT-3.                                                     NC1074.2
186900     MOVE   "123456789," TO TEST-FIELD.                           NC1074.2
187000 DCOM-TEST-3-1.                                                   NC1074.2
187100     IF     TEST-FIELD = "123456789,"                             NC1074.2
187200            PERFORM PASS                                          NC1074.2
187300            GO TO DCOM-WRITE-3.                                   NC1074.2
187400     GO TO  DCOM-FAIL-3.                                          NC1074.2
187500 DCOM-DELETE-3.                                                   NC1074.2
187600     PERFORM DE-LETE.                                             NC1074.2
187700     GO TO   DCOM-WRITE-3.                                        NC1074.2
187800 DCOM-FAIL-3.                                                     NC1074.2
187900     MOVE    TEST-FIELD  TO COMPUTED-A.                           NC1074.2
188000     MOVE   "123456789," TO CORRECT-A.                            NC1074.2
188100     PERFORM FAIL.                                                NC1074.2
188200 DCOM-WRITE-3.                                                    NC1074.2
188300     MOVE   "DCOM-TEST-3" TO PAR-NAME.                            NC1074.2
188400     PERFORM PRINT-DETAIL.                                        NC1074.2
188500 NUM-INIT-A.                                                      NC1074.2
188600     MOVE     "NUMERIC PARA-NAMES" TO FEATURE.                    NC1074.2
188700     PERFORM  PRINT-DETAIL.                                       NC1074.2
188800 NUM-TEST-2.                                                      NC1074.2
188900     MOVE     3 TO NUM-UTILITY.                                   NC1074.2
189000     GO       TO 3 4 5 DEPENDING ON NUM-UTILITY.                  NC1074.2
189100     PERFORM  FAIL.                                               NC1074.2
189200     MOVE     "GO TO DEPENDING IGNORED" TO RE-MARK.               NC1074.2
189300     GO       TO NUM-WRITE-2.                                     NC1074.2
189400 NUM-DELETE-2.                                                    NC1074.2
189500     PERFORM  DE-LETE.                                            NC1074.2
189600     GO       TO NUM-WRITE-2.                                     NC1074.2
189700 4.                                                               NC1074.2
189800     PERFORM  FAIL.                                               NC1074.2
189900     MOVE     "PARAGRAPH 4 ENTERED" TO RE-MARK                    NC1074.2
190000     GO       TO NUM-WRITE-2.                                     NC1074.2
190100 5.                                                               NC1074.2
190200     PERFORM  PASS.                                               NC1074.2
190300     GO       TO NUM-WRITE-2.                                     NC1074.2
190400 3.                                                               NC1074.2
190500     PERFORM  FAIL.                                               NC1074.2
190600     MOVE     "PARAGRAPH 3 ENTERED" TO RE-MARK.                   NC1074.2
190700 NUM-WRITE-2.                                                     NC1074.2
190800     MOVE     "  GO TO DEPENDING" TO FEATURE.                     NC1074.2
190900     MOVE     "NUM-TEST-2" TO PAR-NAME.                           NC1074.2
191000     PERFORM  PRINT-DETAIL.                                       NC1074.2
191100 NUM-TEST-3.                                                      NC1074.2
191200     MOVE     ZERO TO NUM-UTILITY.                                NC1074.2
191300     PERFORM  000000000000000000000000001 THRU                    NC1074.2
191400              00000000000000000000000000001 2 TIMES.              NC1074.2
191500     IF       NUM-UTILITY EQUAL TO 220                            NC1074.2
191600              PERFORM PASS GO TO NUM-WRITE-3.                     NC1074.2
191700     GO       TO NUM-FAIL-3.                                      NC1074.2
191800 NUM-DELETE-3.                                                    NC1074.2
191900     PERFORM  DE-LETE.                                            NC1074.2
192000     GO       TO NUM-WRITE-3.                                     NC1074.2
192100 NUM-FAIL-3.                                                      NC1074.2
192200     PERFORM  FAIL.                                               NC1074.2
192300     MOVE     NUM-UTILITY TO COMPUTED-N.                          NC1074.2
192400     MOVE     220 TO CORRECT-N.                                   NC1074.2
192500                                                                  NC1074.2
192600 NUM-WRITE-3.                                                     NC1074.2
192700     MOVE     "  PERFORM THRU TIMES" TO FEATURE.                  NC1074.2
192800     MOVE     "NUM-TEST-3" TO PAR-NAME.                           NC1074.2
192900     PERFORM  PRINT-DETAIL.                                       NC1074.2
193000                                                                  NC1074.2
193100 NUM-TEST-4.                                                      NC1074.2
193200     MOVE     ZERO TO NUM-UTILITY.                                NC1074.2
193300     PERFORM  0000000000000000000000000001.                       NC1074.2
193400     IF       NUM-UTILITY EQUAL TO 1100                           NC1074.2
193500              PERFORM PASS GO TO NUM-WRITE-4.                     NC1074.2
193600     GO       TO NUM-FAIL-4.                                      NC1074.2
193700 NUM-DELETE-4.                                                    NC1074.2
193800     PERFORM  DE-LETE.                                            NC1074.2
193900     GO       TO NUM-WRITE-4.                                     NC1074.2
194000 NUM-FAIL-4.                                                      NC1074.2
194100     PERFORM  FAIL.                                               NC1074.2
194200     MOVE     NUM-UTILITY TO COMPUTED-N.                          NC1074.2
194300     MOVE     1100 TO CORRECT-N.                                  NC1074.2
194400 NUM-WRITE-4.                                                     NC1074.2
194500     MOVE     "  PERFORM SECT-NAME" TO FEATURE.                   NC1074.2
194600     MOVE     "NUM-TEST-4" TO PAR-NAME.                           NC1074.2
194700     PERFORM  PRINT-DETAIL.                                       NC1074.2
194800     GO       TO NUM-EXIT.                                        NC1074.2
194900 0000000000000000000000001 SECTION.                               NC1074.2
195000 00000000000000000000000001.                                      NC1074.2
195100     ADD      1 TO NUM-UTILITY ON SIZE ERROR GO TO NUM-ERROR.     NC1074.2
195200                                                                  NC1074.2
195300 000000000000000000000000001.                                     NC1074.2
195400     ADD      10 TO NUM-UTILITY ON SIZE ERROR GO TO NUM-ERROR.    NC1074.2
195500 0000000000000000000000000001 SECTION.                            NC1074.2
195600 00000000000000000000000000001.                                   NC1074.2
195700     ADD      100 TO NUM-UTILITY ON SIZE ERROR GO TO NUM-ERROR.   NC1074.2
195800 000000000000000000000000000001.                                  NC1074.2
195900     ADD      1000 TO NUM-UTILITY ON SIZE ERROR GO TO NUM-ERROR.  NC1074.2
196000 NUM-EXIT-SECT SECTION.                                           NC1074.2
196100 NUM-ERROR.                                                       NC1074.2
196200     MOVE     "  PERFORM" TO FEATURE.                             NC1074.2
196300     MOVE "NUM-TEST-4 " TO PAR-NAME.                              NC1074.2
196400     PERFORM  FAIL.                                               NC1074.2
196500     MOVE     NUM-UTILITY TO COMPUTED-N.                          NC1074.2
196600     MOVE     "SIZE ERROR ENCOUNTERED" TO RE-MARK.                NC1074.2
196700     PERFORM  PRINT-DETAIL.                                       NC1074.2
196800 NUM-EXIT.                                                        NC1074.2
196900     EXIT.                                                        NC1074.2
197000 NUM-TEST-5.                                                      NC1074.2
197100     MOVE "  GO TO " TO FEATURE.                                  NC1074.2
197200     GO TO 000000000000000000000000000002.                        NC1074.2
197300 NUM-DELETE-5.                                                    NC1074.2
197400     PERFORM DE-LETE.                                             NC1074.2
197500     GO TO NUM-WRITE-5.                                           NC1074.2
197600 000000000000000000000000000002.                                  NC1074.2
197700     MOVE 2222 TO NUM-UTILITY.                                    NC1074.2
197800 COMPARE-TEST-5.                                                  NC1074.2
197900     IF NUM-UTILITY EQUAL TO 2222                                 NC1074.2
198000         PERFORM PASS                                             NC1074.2
198100         GO TO NUM-WRITE-5.                                       NC1074.2
198200     MOVE 2222 TO CORRECT-A.                                      NC1074.2
198300     MOVE "GO TO PARAGRAPH NOT ENTERED" TO RE-MARK.               NC1074.2
198400     PERFORM FAIL.                                                NC1074.2
198500 NUM-WRITE-5.                                                     NC1074.2
198600     MOVE "NUM-TEST-5 " TO PAR-NAME.                              NC1074.2
198700     PERFORM PRINT-DETAIL.                                        NC1074.2
198800 CONT-INIT-1.                                                     NC1074.2
198900     MOVE   "ABCDEFGHIJ" TO TEST-FIELD.                           NC1074.2
199000     MOVE   "CONTINUE STATEMENT" TO FEATURE.                      NC1074.2
199100     MOVE   "VI-77 6.9"          TO ANSI-REFERENCE.               NC1074.2
199200 CONT-TEST-1-1.                                                   NC1074.2
199300     IF      TEST-FIELD = "ABCDEFGHIJ"                            NC1074.2
199400             CONTINUE                                             NC1074.2
199500     ELSE                                                         NC1074.2
199600             GO TO   CONT-FAIL-1.                                 NC1074.2
199700     PERFORM PASS.                                                NC1074.2
199800     GO TO   CONT-WRITE-1.                                        NC1074.2
199900 CONT-DELETE-1.                                                   NC1074.2
200000     PERFORM DE-LETE.                                             NC1074.2
200100     GO TO   CONT-WRITE-1.                                        NC1074.2
200200 CONT-FAIL-1.                                                     NC1074.2
200300     MOVE   "CONTINUE STATEMENT" TO FEATURE.                      NC1074.2
200400     MOVE    TEST-FIELD  TO COMPUTED-A.                           NC1074.2
200500     MOVE   "123456789." TO CORRECT-A.                            NC1074.2
200600     PERFORM FAIL.                                                NC1074.2
200700 CONT-WRITE-1.                                                    NC1074.2
200800     MOVE   "CONT-TEST-1" TO PAR-NAME.                            NC1074.2
200900     PERFORM PRINT-DETAIL.                                        NC1074.2
201000 CONT-INIT-2.                                                     NC1074.2
201100     MOVE   "ABCDEFGHIJ" TO TEST-FIELD.                           NC1074.2
201200     MOVE   "CONTINUE STATEMENT" TO FEATURE.                      NC1074.2
201300     MOVE   "VI-77 6.9"          TO ANSI-REFERENCE.               NC1074.2
201400 CONT-TEST-2-1.                                                   NC1074.2
201500     IF      TEST-FIELD = "ABCDEFGHIJ"                            NC1074.2
201600             PERFORM PASS                                         NC1074.2
201700             GO TO   CONT-WRITE-2                                 NC1074.2
201800     ELSE                                                         NC1074.2
201900             CONTINUE.                                            NC1074.2
202000     GO TO   CONT-FAIL-2.                                         NC1074.2
202100 CONT-DELETE-2.                                                   NC1074.2
202200     PERFORM DE-LETE.                                             NC1074.2
202300     GO TO   CONT-WRITE-2.                                        NC1074.2
202400 CONT-FAIL-2.                                                     NC1074.2
202500     MOVE    TEST-FIELD  TO COMPUTED-A.                           NC1074.2
202600     MOVE   "123456789." TO CORRECT-A.                            NC1074.2
202700     PERFORM FAIL.                                                NC1074.2
202800 CONT-WRITE-2.                                                    NC1074.2
202900     MOVE   "CONT-TEST-2" TO PAR-NAME.                            NC1074.2
203000     PERFORM PRINT-DETAIL.                                        NC1074.2
203100 CCVS-EXIT SECTION.                                               NC1074.2
203200 CCVS-999999.                                                     NC1074.2
203300     GO TO CLOSE-FILES.                                           NC1074.2
*END-OF,NC107A                                                                  
*HEADER,COBOL,NC108M                                                            
000100 IDENTIFICATION DIVISION.                                         NC1084.2
000200 PROGRAM-ID.                                                      NC1084.2
000300     NC108M.                                                      NC1084.2
000400****************************************************************  NC1084.2
000500*                                                              *  NC1084.2
000600*    VALIDATION FOR:-                                          *  NC1084.2
000700*                                                              *  NC1084.2
000800*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC1084.2
000900*                                                              *  NC1084.2
001000*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC1084.2
001100*                                                              *  NC1084.2
001200****************************************************************  NC1084.2
001300*                                                              *  NC1084.2
001400*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  NC1084.2
001500*                                                              *  NC1084.2
001600*        X-55  - SYSTEM PRINTER NAME.                          *  NC1084.2
001700*        X-82  - SOURCE COMPUTER NAME.                         *  NC1084.2
001800*        X-83  - OBJECT COMPUTER NAME.                         *  NC1084.2
001900*                                                              *  NC1084.2
002000****************************************************************  NC1084.2
002100*                                                                 NC1084.2
002200*    PROGRAM NC108M TESTS THE FOLLOWING FEATURES:                 NC1084.2
002300*                                                                 NC1084.2
002400*    COMPACT IDENTIFICATION DIVISION                              NC1084.2
002500*    COMBINED DATA DESCRIPTION CLAUSES                            NC1084.2
002600*    ABBREVIATIONS                                                NC1084.2
002700*    COBOL CHARACTER SET                                          NC1084.2
002800*    ALPHABET CLAUSE                                              NC1084.2
002900*                                                                 NC1084.2
003000 ENVIRONMENT DIVISION.                                            NC1084.2
003100 CONFIGURATION SECTION.                                           NC1084.2
003200 SOURCE-COMPUTER.                                                 NC1084.2
003300     XXXXX082.                                                    NC1084.2
003400 OBJECT-COMPUTER.                                                 NC1084.2
003500     XXXXX083.                                                    NC1084.2
003600 SPECIAL-NAMES.                                                   NC1084.2
003700A    XXXXX051                                                     NC1084.2
003800A    IS ABBREV-SWITCH                                             NC1084.2
003900A        ON  ON-SWITCH                                            NC1084.2
004000A        OFF IS  OFF-SWITCH                                       NC1084.2
004100*    ALPHABET THE-WILD-ONE IS                                     NC1084.2
004200*     "A" THRU "H" "I" ALSO "J", ALSO "K", ALSO                   NC1084.2
004300*    "L" ALSO "M" ALSO "N" "O" THROUGH "Z" "0" THRU "9",          NC1084.2
004400*                                                                 NC1084.2
004500*                                                                 NC1084.2
004600*ALPHABET-TEST-10     *****     THE WHOLE ALPHABET IS ONE LITERAL NC1084.2
004700*    WITH ALL 51 CHARACTERS IN THE COBOL CHARACTER SET.  TEST-10  NC1084.2
004800*    IS ONLY A SYNTAX CHECK ON                                    NC1084.2
004900*            ALPHABET-NAME IS LITERAL.                            NC1084.2
005000*                                                                 NC1084.2
005100*                                                                 NC1084.2
005200*    THE-BIG-OL-LITERAL-ALPHABET IS "A+0B-1C*2D/3E=4FL5G,6H;7I.8J"NC1084.2
005300*    ""9K(L)M>N<O PQRSTUVWXYZ"                                    NC1084.2
005400     ALPHABET TEST-ALPHABET IS NATIVE                             NC1084.2
005500     CURRENCY  "<".                                               NC1084.2
005600 INPUT-OUTPUT SECTION.                                            NC1084.2
005700 FILE-CONTROL.                                                    NC1084.2
005800     SELECT PRINT-FILE ASSIGN TO                                  NC1084.2
005900     XXXXX055.                                                    NC1084.2
006000 DATA DIVISION.                                                   NC1084.2
006100 FILE SECTION.                                                    NC1084.2
006200 FD  PRINT-FILE.                                                  NC1084.2
006300 01  PRINT-REC PICTURE X(120).                                    NC1084.2
006400 01  DUMMY-RECORD PICTURE X(120).                                 NC1084.2
006500 WORKING-STORAGE SECTION.                                         NC1084.2
006600 77  ONE      PICTURE 9 VALUE 1.                                  NC1084.2
006700 77  TWO      PICTURE 9 VALUE 2.                                  NC1084.2
006800 01  XCHAR-SET PICTURE X(51) VALUE     "ABCDEFGHIJKLMNOPQRSTUVWXYZNC1084.2
006900-    " 0123456789 +-*/=$,.;()><".                                 NC1084.2
007000 01  CHARACTER-QUOTE  PIC X VALUE QUOTE.                          NC1084.2
007100 01  CHARACTER-LOW PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".  NC1084.2
007200 01  COMPLETE-01.                                                 NC1084.2
007300     02 COMPLETE-F.                                               NC1084.2
007400       03 FILLER PICTURE X(90) VALUE SPACE.                       NC1084.2
007500       03 FL-LESS PICTURE <(3),<<<.99 VALUE " <1,111.11".         NC1084.2
007600     02 COMPLETE-FORMAT                                           NC1084.2
007700         REDEFINES COMPLETE-F                                     NC1084.2
007800         JUSTIFIED RIGHT                                          NC1084.2
007900         PICTURE X(5)                                             NC1084.2
008000         OCCURS 20 TIMES                                          NC1084.2
008100         USAGE IS DISPLAY.                                        NC1084.2
008200     02 MORE-COMPLETE-FORMAT                                      NC1084.2
008300         BLANK WHEN ZERO                                          NC1084.2
008400         PICTURE IS 9                                             NC1084.2
008500         SYNCHRONIZED RIGHT                                       NC1084.2
008600         DISPLAY                                                  NC1084.2
008700              VALUE IS "5".                                       NC1084.2
008800 01  PIC-GROUP.                                                   NC1084.2
008900     02 FILLER          PICTURE X(4) VALUE "AAAA".                NC1084.2
009000     02 FILLER          PIC     X(4) VALUE "BBBB".                NC1084.2
009100     02 FILLER          PIC IS  X(4) VALUE "CCCC".                NC1084.2
009200     02                 PICTURE X(4) VALUE "DDDD".                NC1084.2
009300 01  PICTURE-ITEM       PICTURE X(16) VALUE "AAAABBBBCCCCDDDD".   NC1084.2
009400 01  SEND-JUST          PICTURE X(5) VALUE "RIGHT".               NC1084.2
009500 01  RECEIVE-JUST       PICTURE X(10)               JUST.         NC1084.2
009600 01  RECEIVE-JUSTRIGHT  PICTURE X(10)               JUST RIGHT.   NC1084.2
009700 01  SEND-BLANK         PICTURE 9(5) VALUE ZERO.                  NC1084.2
009800 01  RECEIVE-BLANK      PICTURE 9(9)                BLANK ZERO.   NC1084.2
009900 01  COMP-GROUP.                                                  NC1084.2
010000     02 FILLER          PICTURE X(5) VALUE SPACE.                 NC1084.2
010100     02 FILLER          PICTURE 9(5) VALUE 77777 USAGE IS COMP.   NC1084.2
010200     02 FILLER          PICTURE X(5) VALUE SPACE.                 NC1084.2
010300 01  COMPUTATIONAL-GROUP.                                         NC1084.2
010400     02 FILLER          PICTURE X(5) VALUE SPACE.                 NC1084.2
010500     02 FILLER          PICTURE 9(5) VALUE 77777 COMPUTATIONAL.   NC1084.2
010600     02 FILLER          PICTURE X(5) VALUE SPACE.                 NC1084.2
010700 01  SYNC-GROUP.                                                  NC1084.2
010800     02                 PICTURE X(5) VALUE SPACE.                 NC1084.2
010900     02                 PICTURE 9(5) VALUE 55555 SYNC.            NC1084.2
011000     02                 PICTURE X(5) VALUE SPACE.                 NC1084.2
011100 01  SYNCHRONIZED-GROUP.                                          NC1084.2
011200     02 FILLER          PICTURE X(5) VALUE SPACE.                 NC1084.2
011300     02 FILLER          PICTURE 9(5) VALUE 55555 SYNCHRONIZED.    NC1084.2
011400     02 FILLER          PICTURE X(5) VALUE SPACE.                 NC1084.2
011500 01  SYNC-RIGHT-GROUP.                                            NC1084.2
011600     02 FILLER PICTURE X(5) VALUE SPACE.                          NC1084.2
011700     02 FILLER PICTURE 9(5) VALUE 33333 SYNC RIGHT.               NC1084.2
011800     02 FILLER PICTURE X(5) VALUE SPACE.                          NC1084.2
011900 01  SYNCHRONIZED-RIGHT-GROUP.                                    NC1084.2
012000     02 FILLER PICTURE X(5) VALUE SPACE.                          NC1084.2
012100     02 FILLER PICTURE 9(5) VALUE 33333 SYNCHRONIZED RIGHT.       NC1084.2
012200     02 FILLER PICTURE X(5) VALUE SPACE.                          NC1084.2
012300 01  SYNC-LEFT-GROUP.                                             NC1084.2
012400     02 FILLER PICTURE X(5) VALUE SPACE.                          NC1084.2
012500     02 FILLER PICTURE 9(5) VALUE 11111 SYNC LEFT.                NC1084.2
012600     02 FILLER PICTURE X(5) VALUE SPACE.                          NC1084.2
012700 01  SYNCHRONIZED-LEFT-GROUP.                                     NC1084.2
012800     02 FILLER PICTURE X(5) VALUE SPACE.                          NC1084.2
012900     02 FILLER PICTURE 9(5) VALUE 11111 SYNCHRONIZED LEFT.        NC1084.2
013000     02 FILLER PICTURE X(5) VALUE SPACE.                          NC1084.2
013100 01  TEST-FIELD            PIC X(10).                             NC1084.2
013200 01                        REDEFINES TEST-FIELD                   NC1084.2
013300                           PIC 9(9).                              NC1084.2
013400 01  TEST-RESULTS.                                                NC1084.2
013500     02 FILLER                   PIC X      VALUE SPACE.          NC1084.2
013600     02 FEATURE                  PIC X(20)  VALUE SPACE.          NC1084.2
013700     02 FILLER                   PIC X      VALUE SPACE.          NC1084.2
013800     02 P-OR-F                   PIC X(5)   VALUE SPACE.          NC1084.2
013900     02 FILLER                   PIC X      VALUE SPACE.          NC1084.2
014000     02  PAR-NAME.                                                NC1084.2
014100       03 FILLER                 PIC X(19)  VALUE SPACE.          NC1084.2
014200       03  PARDOT-X              PIC X      VALUE SPACE.          NC1084.2
014300       03 DOTVALUE               PIC 99     VALUE ZERO.           NC1084.2
014400     02 FILLER                   PIC X(8)   VALUE SPACE.          NC1084.2
014500     02 RE-MARK                  PIC X(61).                       NC1084.2
014600 01  TEST-COMPUTED.                                               NC1084.2
014700     02 FILLER                   PIC X(30)  VALUE SPACE.          NC1084.2
014800     02 FILLER                   PIC X(17)  VALUE                 NC1084.2
014900            "       COMPUTED=".                                   NC1084.2
015000     02 COMPUTED-X.                                               NC1084.2
015100     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          NC1084.2
015200     03 COMPUTED-N               REDEFINES COMPUTED-A             NC1084.2
015300                                 PIC -9(9).9(9).                  NC1084.2
015400     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         NC1084.2
015500     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     NC1084.2
015600     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     NC1084.2
015700     03       CM-18V0 REDEFINES COMPUTED-A.                       NC1084.2
015800         04 COMPUTED-18V0                    PIC -9(18).          NC1084.2
015900         04 FILLER                           PIC X.               NC1084.2
016000     03 FILLER PIC X(50) VALUE SPACE.                             NC1084.2
016100 01  TEST-CORRECT.                                                NC1084.2
016200     02 FILLER PIC X(30) VALUE SPACE.                             NC1084.2
016300     02 FILLER PIC X(17) VALUE "       CORRECT =".                NC1084.2
016400     02 CORRECT-X.                                                NC1084.2
016500     03 CORRECT-A                  PIC X(20) VALUE SPACE.         NC1084.2
016600     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      NC1084.2
016700     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         NC1084.2
016800     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     NC1084.2
016900     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     NC1084.2
017000     03      CR-18V0 REDEFINES CORRECT-A.                         NC1084.2
017100         04 CORRECT-18V0                     PIC -9(18).          NC1084.2
017200         04 FILLER                           PIC X.               NC1084.2
017300     03 FILLER PIC X(2) VALUE SPACE.                              NC1084.2
017400     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     NC1084.2
017500 01  CCVS-C-1.                                                    NC1084.2
017600     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PANC1084.2
017700-    "SS  PARAGRAPH-NAME                                          NC1084.2
017800-    "       REMARKS".                                            NC1084.2
017900     02 FILLER                     PIC X(20)    VALUE SPACE.      NC1084.2
018000 01  CCVS-C-2.                                                    NC1084.2
018100     02 FILLER                     PIC X        VALUE SPACE.      NC1084.2
018200     02 FILLER                     PIC X(6)     VALUE "TESTED".   NC1084.2
018300     02 FILLER                     PIC X(15)    VALUE SPACE.      NC1084.2
018400     02 FILLER                     PIC X(4)     VALUE "FAIL".     NC1084.2
018500     02 FILLER                     PIC X(94)    VALUE SPACE.      NC1084.2
018600 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       NC1084.2
018700 01  REC-CT                        PIC 99       VALUE ZERO.       NC1084.2
018800 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       NC1084.2
018900 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       NC1084.2
019000 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       NC1084.2
019100 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       NC1084.2
019200 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       NC1084.2
019300 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       NC1084.2
019400 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      NC1084.2
019500 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       NC1084.2
019600 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     NC1084.2
019700 01  CCVS-H-1.                                                    NC1084.2
019800     02  FILLER                    PIC X(39)    VALUE SPACES.     NC1084.2
019900     02  FILLER                    PIC X(42)    VALUE             NC1084.2
020000     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 NC1084.2
020100     02  FILLER                    PIC X(39)    VALUE SPACES.     NC1084.2
020200 01  CCVS-H-2A.                                                   NC1084.2
020300   02  FILLER                        PIC X(40)  VALUE SPACE.      NC1084.2
020400   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  NC1084.2
020500   02  FILLER                        PIC XXXX   VALUE             NC1084.2
020600     "4.2 ".                                                      NC1084.2
020700   02  FILLER                        PIC X(28)  VALUE             NC1084.2
020800            " COPY - NOT FOR DISTRIBUTION".                       NC1084.2
020900   02  FILLER                        PIC X(41)  VALUE SPACE.      NC1084.2
021000                                                                  NC1084.2
021100 01  CCVS-H-2B.                                                   NC1084.2
021200   02  FILLER                        PIC X(15)  VALUE             NC1084.2
021300            "TEST RESULT OF ".                                    NC1084.2
021400   02  TEST-ID                       PIC X(9).                    NC1084.2
021500   02  FILLER                        PIC X(4)   VALUE             NC1084.2
021600            " IN ".                                               NC1084.2
021700   02  FILLER                        PIC X(12)  VALUE             NC1084.2
021800     " HIGH       ".                                              NC1084.2
021900   02  FILLER                        PIC X(22)  VALUE             NC1084.2
022000            " LEVEL VALIDATION FOR ".                             NC1084.2
022100   02  FILLER                        PIC X(58)  VALUE             NC1084.2
022200     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC1084.2
022300 01  CCVS-H-3.                                                    NC1084.2
022400     02  FILLER                      PIC X(34)  VALUE             NC1084.2
022500            " FOR OFFICIAL USE ONLY    ".                         NC1084.2
022600     02  FILLER                      PIC X(58)  VALUE             NC1084.2
022700     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC1084.2
022800     02  FILLER                      PIC X(28)  VALUE             NC1084.2
022900            "  COPYRIGHT   1985 ".                                NC1084.2
023000 01  CCVS-E-1.                                                    NC1084.2
023100     02 FILLER                       PIC X(52)  VALUE SPACE.      NC1084.2
023200     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              NC1084.2
023300     02 ID-AGAIN                     PIC X(9).                    NC1084.2
023400     02 FILLER                       PIC X(45)  VALUE SPACES.     NC1084.2
023500 01  CCVS-E-2.                                                    NC1084.2
023600     02  FILLER                      PIC X(31)  VALUE SPACE.      NC1084.2
023700     02  FILLER                      PIC X(21)  VALUE SPACE.      NC1084.2
023800     02 CCVS-E-2-2.                                               NC1084.2
023900         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      NC1084.2
024000         03 FILLER                   PIC X      VALUE SPACE.      NC1084.2
024100         03 ENDER-DESC               PIC X(44)  VALUE             NC1084.2
024200            "ERRORS ENCOUNTERED".                                 NC1084.2
024300 01  CCVS-E-3.                                                    NC1084.2
024400     02  FILLER                      PIC X(22)  VALUE             NC1084.2
024500            " FOR OFFICIAL USE ONLY".                             NC1084.2
024600     02  FILLER                      PIC X(12)  VALUE SPACE.      NC1084.2
024700     02  FILLER                      PIC X(58)  VALUE             NC1084.2
024800     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC1084.2
024900     02  FILLER                      PIC X(13)  VALUE SPACE.      NC1084.2
025000     02 FILLER                       PIC X(15)  VALUE             NC1084.2
025100             " COPYRIGHT 1985".                                   NC1084.2
025200 01  CCVS-E-4.                                                    NC1084.2
025300     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      NC1084.2
025400     02 FILLER                       PIC X(4)   VALUE " OF ".     NC1084.2
025500     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      NC1084.2
025600     02 FILLER                       PIC X(40)  VALUE             NC1084.2
025700      "  TESTS WERE EXECUTED SUCCESSFULLY".                       NC1084.2
025800 01  XXINFO.                                                      NC1084.2
025900     02 FILLER                       PIC X(19)  VALUE             NC1084.2
026000            "*** INFORMATION ***".                                NC1084.2
026100     02 INFO-TEXT.                                                NC1084.2
026200       04 FILLER                     PIC X(8)   VALUE SPACE.      NC1084.2
026300       04 XXCOMPUTED                 PIC X(20).                   NC1084.2
026400       04 FILLER                     PIC X(5)   VALUE SPACE.      NC1084.2
026500       04 XXCORRECT                  PIC X(20).                   NC1084.2
026600     02 INF-ANSI-REFERENCE           PIC X(48).                   NC1084.2
026700 01  HYPHEN-LINE.                                                 NC1084.2
026800     02 FILLER  PIC IS X VALUE IS SPACE.                          NC1084.2
026900     02 FILLER  PIC IS X(65)    VALUE IS "************************NC1084.2
027000-    "*****************************************".                 NC1084.2
027100     02 FILLER  PIC IS X(54)    VALUE IS "************************NC1084.2
027200-    "******************************".                            NC1084.2
027300 01  CCVS-PGM-ID                     PIC X(9)   VALUE             NC1084.2
027400     "NC108M".                                                    NC1084.2
027500 PROCEDURE DIVISION.                                              NC1084.2
027600 CCVS1 SECTION.                                                   NC1084.2
027700 OPEN-FILES.                                                      NC1084.2
027800     OPEN     OUTPUT PRINT-FILE.                                  NC1084.2
027900     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   NC1084.2
028000     MOVE    SPACE TO TEST-RESULTS.                               NC1084.2
028100     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             NC1084.2
028200     GO TO CCVS1-EXIT.                                            NC1084.2
028300 CLOSE-FILES.                                                     NC1084.2
028400     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   NC1084.2
028500 TERMINATE-CCVS.                                                  NC1084.2
028600S    EXIT PROGRAM.                                                NC1084.2
028700STERMINATE-CALL.                                                  NC1084.2
028800     STOP     RUN.                                                NC1084.2
028900 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         NC1084.2
029000 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           NC1084.2
029100 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          NC1084.2
029200 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      NC1084.2
029300     MOVE "****TEST DELETED****" TO RE-MARK.                      NC1084.2
029400 PRINT-DETAIL.                                                    NC1084.2
029500     IF REC-CT NOT EQUAL TO ZERO                                  NC1084.2
029600             MOVE "." TO PARDOT-X                                 NC1084.2
029700             MOVE REC-CT TO DOTVALUE.                             NC1084.2
029800     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      NC1084.2
029900     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               NC1084.2
030000        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 NC1084.2
030100          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 NC1084.2
030200     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              NC1084.2
030300     MOVE SPACE TO CORRECT-X.                                     NC1084.2
030400     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         NC1084.2
030500     MOVE     SPACE TO RE-MARK.                                   NC1084.2
030600 HEAD-ROUTINE.                                                    NC1084.2
030700     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC1084.2
030800     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC1084.2
030900     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC1084.2
031000     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC1084.2
031100 COLUMN-NAMES-ROUTINE.                                            NC1084.2
031200     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC1084.2
031300     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC1084.2
031400     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        NC1084.2
031500 END-ROUTINE.                                                     NC1084.2
031600     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.NC1084.2
031700 END-RTN-EXIT.                                                    NC1084.2
031800     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC1084.2
031900 END-ROUTINE-1.                                                   NC1084.2
032000      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      NC1084.2
032100      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               NC1084.2
032200      ADD PASS-COUNTER TO ERROR-HOLD.                             NC1084.2
032300*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   NC1084.2
032400      MOVE PASS-COUNTER TO CCVS-E-4-1.                            NC1084.2
032500      MOVE ERROR-HOLD TO CCVS-E-4-2.                              NC1084.2
032600      MOVE CCVS-E-4 TO CCVS-E-2-2.                                NC1084.2
032700      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           NC1084.2
032800  END-ROUTINE-12.                                                 NC1084.2
032900      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        NC1084.2
033000     IF       ERROR-COUNTER IS EQUAL TO ZERO                      NC1084.2
033100         MOVE "NO " TO ERROR-TOTAL                                NC1084.2
033200         ELSE                                                     NC1084.2
033300         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       NC1084.2
033400     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           NC1084.2
033500     PERFORM WRITE-LINE.                                          NC1084.2
033600 END-ROUTINE-13.                                                  NC1084.2
033700     IF DELETE-COUNTER IS EQUAL TO ZERO                           NC1084.2
033800         MOVE "NO " TO ERROR-TOTAL  ELSE                          NC1084.2
033900         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      NC1084.2
034000     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   NC1084.2
034100     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC1084.2
034200      IF   INSPECT-COUNTER EQUAL TO ZERO                          NC1084.2
034300          MOVE "NO " TO ERROR-TOTAL                               NC1084.2
034400      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   NC1084.2
034500      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            NC1084.2
034600      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          NC1084.2
034700     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC1084.2
034800 WRITE-LINE.                                                      NC1084.2
034900     ADD 1 TO RECORD-COUNT.                                       NC1084.2
035000Y    IF RECORD-COUNT GREATER 50                                   NC1084.2
035100Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          NC1084.2
035200Y        MOVE SPACE TO DUMMY-RECORD                               NC1084.2
035300Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  NC1084.2
035400Y        MOVE CCVS-H-1  TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   NC1084.2
035500Y        MOVE CCVS-H-2A TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   NC1084.2
035600Y        MOVE CCVS-H-2B TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   NC1084.2
035700Y        MOVE CCVS-H-3  TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   NC1084.2
035800Y        MOVE CCVS-C-1  TO DUMMY-RECORD  PERFORM WRT-LN           NC1084.2
035900Y        MOVE CCVS-C-2  TO DUMMY-RECORD  PERFORM WRT-LN           NC1084.2
036000Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          NC1084.2
036100Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          NC1084.2
036200Y        MOVE ZERO TO RECORD-COUNT.                               NC1084.2
036300     PERFORM WRT-LN.                                              NC1084.2
036400 WRT-LN.                                                          NC1084.2
036500     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               NC1084.2
036600     MOVE SPACE TO DUMMY-RECORD.                                  NC1084.2
036700 BLANK-LINE-PRINT.                                                NC1084.2
036800     PERFORM WRT-LN.                                              NC1084.2
036900 FAIL-ROUTINE.                                                    NC1084.2
037000     IF     COMPUTED-X NOT EQUAL TO SPACE                         NC1084.2
037100            GO TO FAIL-ROUTINE-WRITE.                             NC1084.2
037200     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.NC1084.2
037300     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC1084.2
037400     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   NC1084.2
037500     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC1084.2
037600     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC1084.2
037700     GO TO  FAIL-ROUTINE-EX.                                      NC1084.2
037800 FAIL-ROUTINE-WRITE.                                              NC1084.2
037900     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         NC1084.2
038000     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 NC1084.2
038100     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. NC1084.2
038200     MOVE   SPACES TO COR-ANSI-REFERENCE.                         NC1084.2
038300 FAIL-ROUTINE-EX. EXIT.                                           NC1084.2
038400 BAIL-OUT.                                                        NC1084.2
038500     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   NC1084.2
038600     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           NC1084.2
038700 BAIL-OUT-WRITE.                                                  NC1084.2
038800     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  NC1084.2
038900     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC1084.2
039000     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC1084.2
039100     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC1084.2
039200 BAIL-OUT-EX. EXIT.                                               NC1084.2
039300 CCVS1-EXIT.                                                      NC1084.2
039400     EXIT.                                                        NC1084.2
039500*IDENTIFICATION DIVISION.                                         NC1084.2
039600*    NOTE THE ENTIRE IDENTIFICATION DIVISION IS OPTIONAL, WITH THENC1084.2
039700*    EXCEPTION OF THE IDENTIFICATION DIVISION AND PROGRAM-ID      NC1084.2
039800*    CLAUSES. AS A TEST, ALL THE OPTIONAL CLAUSES HAVE BEEN       NC1084.2
039900*    REMOVED. INFORMATION NORMALLY GIVEN THERE IS LISTED BELOW    NC1084.2
040000*    AS A COMMENT.  ADDITIONALLY, KEY WORDS ARE USED IN COMMENT   NC1084.2
040100*    LINES TO ASCERTAIN WHETHER COMMENTS ARE BEING SYNTAX CHECKED.NC1084.2
040200****************************************************************  NC1084.2
040300*                                                              *  NC1084.2
040400*    THIS PROGRAM FORMS PART OF THE COBOL COMPILER VALIDATION  *  NC1084.2
040500*    SYSTEM (CCVS) USED TO TEST COBOL COMPILERS FOR            *  NC1084.2
040600*    COMFORMANCE WITH THE AMERICAN NATIONAL STANDARD           *  NC1084.2
040700*    (ANSI DOCUMENT REFERENCE: X3.23-1985) AND THE STANDARD OF *  NC1084.2
040800*    THE INTERNATIONAL ORGANIZATION FOR STANDARDISATION        *  NC1084.2
040900*    (ISO DOCUMENT REFERENCE: ISO            ).                *  NC1084.2
041000*                                                              *  NC1084.2
041100*    THIS CCVS INCORPORATES ENHANCEMENTS TO THE CCVS FOR THE   *  NC1084.2
041200*    1974 STANDARD (ANSI DOCUMENT REFERENCE: X3.23-1974; ISO   *  NC1084.2
041300*    DOCUMENT REFERENCE:           ).                          *  NC1084.2
041400*                                                              *  NC1084.2
041500*    THESE ENHANCEMENTS WERE SPECIFIED BY A PROJECT TEAM WHICH *  NC1084.2
041600*    WAS FUNDED BY THE COMMISSION FOR EUROPEAN COMMUNITIES AND *  NC1084.2
041700*    WHICH WAS RESPONSIBLE FOR TECHNICAL ISSUES TO:            *  NC1084.2
041800*                                                              *  NC1084.2
041900*          THE FEDERAL SOFTWARE TESTING CENTRE                 *  NC1084.2
042000*          OFFICE OF SOFTWARE DEVELOPMENT                      *  NC1084.2
042100*                & INFORMATION TECHNOLOGY                      *  NC1084.2
042200*          TWO SKYLINE PLACE                                   *  NC1084.2
042300*          SUITE 1100                                          *  NC1084.2
042400*          5203 LEESBURG PIKE                                  *  NC1084.2
042500*          FALLS CHURCH                                        *  NC1084.2
042600*          VA 22041                                            *  NC1084.2
042700*          U.S.A.                                              *  NC1084.2
042800*                                                              *  NC1084.2
042900*    THE PROJECT TEAM MEMBERS WERE:                            *  NC1084.2
043000*                                                              *  NC1084.2
043100*          BIADI (BUREAU INTER ADMINISTRATION                  *  NC1084.2
043200*                 DE DOCUMENTATION INFORMATIQUE)               *  NC1084.2
043300*          21 RUE BARA                                         *  NC1084.2
043400*          F-92132 ISSY                                        *  NC1084.2
043500*          FRANCE                                              *  NC1084.2
043600*                                                              *  NC1084.2
043700*                                                              *  NC1084.2
043800*          GMD (GESELLSCHAFT FUR MATHEMATIK                    *  NC1084.2
043900*               UND DATENVERARBEITUNG MBH)                     *  NC1084.2
044000*          SCHLOSS BIRLINGHOVEN                                *  NC1084.2
044100*          POSTFACH 12 40                                      *  NC1084.2
044200*          D-5205 ST. AUGUSTIN 1                               *  NC1084.2
044300*          GERMANY FR                                          *  NC1084.2
044400*                                                              *  NC1084.2
044500*                                                              *  NC1084.2
044600*          NCC (THE NATIONAL COMPUTING CENTRE LTD)             *  NC1084.2
044700*          OXFORD ROAD                                         *  NC1084.2
044800*          MANCHESTER                                          *  NC1084.2
044900*          M1 7ED                                              *  NC1084.2
045000*          UNITED KINGDOM                                      *  NC1084.2
045100*                                                              *  NC1084.2
045200*                                                              *  NC1084.2
045300*    THIS TEST SUITE WAS PRODUCED BY THE NATIONAL COMPUTING    *  NC1084.2
045400*    CENTRE IN ENGLAND AND IS THE OFFICIAL CCVS TEST SUITE     *  NC1084.2
045500*    USED THROUGHOUT EUROPE AND THE UNITED STATES OF AMERICA.  *  NC1084.2
045600*                                                              *  NC1084.2
045700****************************************************************  NC1084.2
045800*                                                              *  NC1084.2
045900*    VALIDATION FOR:-                                          *  NC1084.2
046000*    " HIGH       ".                                              NC1084.2
046100*    USING CCVS85 VERSION 1.0 ISSUED IN JANUARY 1986.          *  NC1084.2
046200*                                                              *  NC1084.2
046300*    CREATION DATE     /     VALIDATION DATE                   *  NC1084.2
046400*    "4.2 ".                                                      NC1084.2
046500*                                                              *  NC1084.2
046600****************************************************************  NC1084.2
046700*                                                                 NC1084.2
046800*    PROGRAM NC108M TESTS THE FOLLOWING FEATURES:                 NC1084.2
046900*                                                                 NC1084.2
047000*            COMPACT "IDENTIFICATION DIVISION"                    NC1084.2
047100*            COMBINED DATA DESCRIPTION CLAUSES                    NC1084.2
047200*            ABBREVIATIONS                                        NC1084.2
047300*            COBOL CHARACTER SET                                  NC1084.2
047400*            ALPHABET CLAUSE                                      NC1084.2
047500*                                                                 NC1084.2
047600*                                                                 NC1084.2
047700*                                                                 NC1084.2
047800*      THE SOURCE LINES IN THE ENVIRONMENT AND DATA DIVISION      NC1084.2
047900*      SHOULD BE REPLACED AS FOLLOWS                              NC1084.2
048000*        XXXXX36   REPLACE WITH SYSTEM OUTPUT DEVICE (PRINTER)    NC1084.2
048100*                    FILE-NAME IS PRINT-FILE.                     NC1084.2
048200*        XXXXX38   REPLACE WITH SYSTEM NAME FOR A SWITCH          NC1084.2
048300*                    SWITCH-NAME IS ABBREV-SEITCH.                NC1084.2
048400*        XXXXX49   REPLACE WITH SOURCE COMPUTER NAME              NC1084.2
048500*        XXXXX50   REPLACE WITH OBJECT COMPUTER NAME              NC1084.2
048600*                                                                 NC1084.2
048700*    THE DOD COBOL TEST ROUTINES HAVE BEEN CREATED TO BE          NC1084.2
048800*    USED TO VALIDATE THAT                                        NC1084.2
048900*                                                                 NC1084.2
049000*           1  A COBOL COMPILER CONTAINS THE ELEMENTS OF THE      NC1084.2
049100*              ANSI COBOL.                                        NC1084.2
049200*                                                                 NC1084.2
049300*           2  TO PROVIDE EXAMPLES OF THE USES OF THE DIFFERENT   NC1084.2
049400*               ELEMENTS OF THE COBOL LANGUAGE.                   NC1084.2
049500*                                                                 NC1084.2
049600*           3  TO BE USED AS TEST DATA FOR PRE-PROCESSORS         NC1084.2
049700*              FLOWCHARTERS  ETC.                                 NC1084.2
049800*                                                                 NC1084.2
049900*           4  IT IS HOPED THAT EVALUATIONS  CORRECTIONS          NC1084.2
050000*               SUGGESTIONS AND COMMENTS WILL BE FORWARDED TO     NC1084.2
050100*                  NAVY PROGRAMMING LANGUAGES DIVISION            NC1084.2
050200*                    ROOM 2C319 THE PENTAGON                      NC1084.2
050300*                    WASHINGTON D C      20350.                   NC1084.2
050400*    *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * *.NC1084.2
050500*                                                                 NC1084.2
050600*    PHONE    (202) 695-4750.                                     NC1084.2
050700*                                                                 NC1084.2
050800*    *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * *.NC1084.2
050900 FMT-INIT-GF-1.                                                   NC1084.2
051000     MOVE   "COMPLETE DATA FORMAT" TO FEATURE.                    NC1084.2
051100     MOVE   "V1-6 3.2.1.1" TO ANSI-REFERENCE.                     NC1084.2
051200 FMT-TEST-GF-1.                                                   NC1084.2
051300     MOVE COMPLETE-FORMAT (19) TO COMPUTED-A.                     NC1084.2
051400     MOVE " <1,1" TO CORRECT-A.                                   NC1084.2
051500     IF   COMPLETE-FORMAT (19) EQUAL TO " <1,1"                   NC1084.2
051600          MOVE "FAILURE IF DOLLAR APPEARS" TO RE-MARK             NC1084.2
051700          GO TO FMT-WRITE-GF-1.                                   NC1084.2
051800     PERFORM FAIL.                                                NC1084.2
051900     MOVE "LESS THAN SHOULD APPEAR" TO RE-MARK.                   NC1084.2
052000     GO       TO FMT-WRITE-GF-1.                                  NC1084.2
052100 FMT-DELETE-GF-1.                                                 NC1084.2
052200     PERFORM  DE-LETE.                                            NC1084.2
052300 FMT-WRITE-GF-1.                                                  NC1084.2
052400     MOVE     "FMT-TEST-GF-1" TO PAR-NAME.                        NC1084.2
052500     PERFORM  PRINT-DETAIL.                                       NC1084.2
052600 FMT-INIT-GF-2.                                                   NC1084.2
052700     MOVE   "V1-20 5.3" TO ANSI-REFERENCE.                        NC1084.2
052800 FMT-TEST-GF-2.                                                   NC1084.2
052900     IF       MORE-COMPLETE-FORMAT NOT EQUAL TO "5"               NC1084.2
053000              PERFORM FAIL                                        NC1084.2
053100         ELSE PERFORM PASS                                        NC1084.2
053200              GO TO FMT-WRITE-GF-2.                               NC1084.2
053300     MOVE     MORE-COMPLETE-FORMAT TO COMPUTED-A.                 NC1084.2
053400     MOVE     "5" TO CORRECT-A.                                   NC1084.2
053500     GO       TO FMT-WRITE-GF-2.                                  NC1084.2
053600 FMT-DELETE-GF-2.                                                 NC1084.2
053700     PERFORM  DE-LETE.                                            NC1084.2
053800 FMT-WRITE-GF-2.                                                  NC1084.2
053900     MOVE     "FMT-TEST-GF-2" TO PAR-NAME.                        NC1084.2
054000     PERFORM  PRINT-DETAIL.                                       NC1084.2
054100 FMT-TEST-GF-3.                                                   NC1084.2
054200     MOVE     ZERO TO MORE-COMPLETE-FORMAT.                       NC1084.2
054300     IF       MORE-COMPLETE-FORMAT EQUAL TO SPACE                 NC1084.2
054400              PERFORM PASS                                        NC1084.2
054500              GO TO FMT-WRITE-GF-3.                               NC1084.2
054600     PERFORM  FAIL.                                               NC1084.2
054700     MOVE     MORE-COMPLETE-FORMAT TO COMPUTED-A.                 NC1084.2
054800     MOVE     "      (SPACES)" TO CORRECT-A.                      NC1084.2
054900     GO       TO FMT-WRITE-GF-3.                                  NC1084.2
055000 FMT-DELETE-GF-3.                                                 NC1084.2
055100     PERFORM  DE-LETE.                                            NC1084.2
055200 FMT-WRITE-GF-3.                                                  NC1084.2
055300     MOVE     "FMT-TEST-GF-3" TO PAR-NAME.                        NC1084.2
055400     PERFORM  PRINT-DETAIL.                                       NC1084.2
055500 ABR-INIT-GF-1.                                                   NC1084.2
055600     MOVE   "DATA DESCR ABBREVS -" TO FEATURE.                    NC1084.2
055700     MOVE   "VI-20 5.3" TO ANSI-REFERENCE.                        NC1084.2
055800     PERFORM  PRINT-DETAIL.                                       NC1084.2
055900 ABR-TEST-GF-1.                                                   NC1084.2
056000     IF       PIC-GROUP IS EQUAL TO PICTURE-ITEM                  NC1084.2
056100              PERFORM PASS GO TO ABR-WRITE-GF-1.                  NC1084.2
056200     GO       TO ABR-FAIL-GF-1.                                   NC1084.2
056300 ABR-DELETE-GF-1.                                                 NC1084.2
056400     PERFORM  DE-LETE.                                            NC1084.2
056500     GO       TO ABR-WRITE-GF-1.                                  NC1084.2
056600 ABR-FAIL-GF-1.                                                   NC1084.2
056700     MOVE     PIC-GROUP TO COMPUTED-A.                            NC1084.2
056800     MOVE     PICTURE-ITEM TO CORRECT-A.                          NC1084.2
056900     PERFORM  FAIL.                                               NC1084.2
057000 ABR-WRITE-GF-1.                                                  NC1084.2
057100     MOVE     "  PIC" TO FEATURE.                                 NC1084.2
057200     MOVE     "ABR-TEST-GF-1 " TO PAR-NAME.                       NC1084.2
057300     PERFORM  PRINT-DETAIL.                                       NC1084.2
057400 ABR-INIT-GF-2.                                                   NC1084.2
057500     MOVE   "VI-20 5.3" TO ANSI-REFERENCE.                        NC1084.2
057600     MOVE     SEND-JUST TO RECEIVE-JUSTRIGHT.                     NC1084.2
057700 ABR-TEST-GF-2.                                                   NC1084.2
057800     IF       RECEIVE-JUSTRIGHT EQUAL TO "     RIGHT"             NC1084.2
057900              PERFORM PASS GO TO ABR-WRITE-GF-2.                  NC1084.2
058000     GO       TO ABR-FAIL-GF-2.                                   NC1084.2
058100 ABR-DELETE-GF-2.                                                 NC1084.2
058200     PERFORM  DE-LETE.                                            NC1084.2
058300     GO       TO ABR-WRITE-GF-2.                                  NC1084.2
058400 ABR-FAIL-GF-2.                                                   NC1084.2
058500     PERFORM  FAIL.                                               NC1084.2
058600     MOVE     RECEIVE-JUSTRIGHT TO COMPUTED-A.                    NC1084.2
058700     MOVE     "     RIGHT" TO CORRECT-A.                          NC1084.2
058800 ABR-WRITE-GF-2.                                                  NC1084.2
058900     MOVE     "  JUST" TO FEATURE                                 NC1084.2
059000     MOVE     "ABR-TEST-GF-2 " TO PAR-NAME.                       NC1084.2
059100     PERFORM  PRINT-DETAIL.                                       NC1084.2
059200 ABR-INIT-GF-3.                                                   NC1084.2
059300     MOVE   "VI-20 5.3" TO ANSI-REFERENCE.                        NC1084.2
059400     MOVE     SEND-JUST TO RECEIVE-JUST.                          NC1084.2
059500 ABR-TEST-GF-3.                                                   NC1084.2
059600     IF       RECEIVE-JUST EQUAL TO "     RIGHT"                  NC1084.2
059700              PERFORM PASS GO TO ABR-WRITE-GF-3.                  NC1084.2
059800     GO       TO ABR-FAIL-GF-3.                                   NC1084.2
059900 ABR-DELETE-GF-3.                                                 NC1084.2
060000     PERFORM  DE-LETE.                                            NC1084.2
060100     GO       TO ABR-WRITE-GF-3.                                  NC1084.2
060200 ABR-FAIL-GF-3.                                                   NC1084.2
060300     PERFORM  FAIL.                                               NC1084.2
060400     MOVE     RECEIVE-JUST TO COMPUTED-A.                         NC1084.2
060500     MOVE     "     RIGHT" TO CORRECT-A.                          NC1084.2
060600 ABR-WRITE-GF-3.                                                  NC1084.2
060700     MOVE     "ABR-TEST-GF-3 " TO PAR-NAME.                       NC1084.2
060800     PERFORM  PRINT-DETAIL.                                       NC1084.2
060900 ABR-INIT-GF-4.                                                   NC1084.2
061000     MOVE   "VI-20 5.3" TO ANSI-REFERENCE.                        NC1084.2
061100     MOVE     SEND-BLANK TO RECEIVE-BLANK.                        NC1084.2
061200 ABR-TEST-GF-4.                                                   NC1084.2
061300     IF       RECEIVE-BLANK EQUAL TO "         "                  NC1084.2
061400              PERFORM PASS GO TO ABR-WRITE-GF-4.                  NC1084.2
061500     GO       TO ABR-FAIL-GF-4.                                   NC1084.2
061600 ABR-DELETE-GF-4.                                                 NC1084.2
061700     PERFORM  DE-LETE.                                            NC1084.2
061800     GO       TO ABR-WRITE-GF-4.                                  NC1084.2
061900 ABR-FAIL-GF-4.                                                   NC1084.2
062000     PERFORM  FAIL.                                               NC1084.2
062100     MOVE     RECEIVE-BLANK TO COMPUTED-A.                        NC1084.2
062200     MOVE     "      (SPACES)" TO CORRECT-A.                      NC1084.2
062300 ABR-WRITE-GF-4.                                                  NC1084.2
062400     MOVE     "  BLANK ZERO" TO FEATURE                           NC1084.2
062500     MOVE     "ABR-TEST-GF-4 " TO PAR-NAME.                       NC1084.2
062600     PERFORM  PRINT-DETAIL.                                       NC1084.2
062700 ABR-INIT-GF-5.                                                   NC1084.2
062800     MOVE   "VI-20 5.3" TO ANSI-REFERENCE.                        NC1084.2
062900 ABR-TEST-GF-5.                                                   NC1084.2
063000     IF       COMP-GROUP EQUAL TO COMPUTATIONAL-GROUP             NC1084.2
063100              PERFORM PASS GO TO ABR-WRITE-GF-5.                  NC1084.2
063200     GO       TO ABR-FAIL-GF-5.                                   NC1084.2
063300 ABR-DELETE-GF-5.                                                 NC1084.2
063400     PERFORM  DE-LETE.                                            NC1084.2
063500     GO       TO ABR-WRITE-GF-5.                                  NC1084.2
063600 ABR-FAIL-GF-5.                                                   NC1084.2
063700     PERFORM  FAIL.                                               NC1084.2
063800     MOVE     COMP-GROUP TO COMPUTED-A.                           NC1084.2
063900     MOVE     COMPUTATIONAL-GROUP TO CORRECT-A.                   NC1084.2
064000 ABR-WRITE-GF-5.                                                  NC1084.2
064100     MOVE     "  COMP" TO FEATURE.                                NC1084.2
064200     MOVE     "ABR-TEST-GF-5 " TO PAR-NAME.                       NC1084.2
064300     PERFORM  PRINT-DETAIL.                                       NC1084.2
064400 ABR-INIT-GF-6.                                                   NC1084.2
064500     MOVE   "VI-20 5.3" TO ANSI-REFERENCE.                        NC1084.2
064600 ABR-TEST-GF-6.                                                   NC1084.2
064700     IF       SYNC-GROUP EQUAL TO SYNCHRONIZED-GROUP              NC1084.2
064800              PERFORM PASS GO TO ABR-WRITE-GF-6.                  NC1084.2
064900     GO       TO ABR-FAIL-GF-6.                                   NC1084.2
065000 ABR-DELETE-GF-6.                                                 NC1084.2
065100     PERFORM  DE-LETE.                                            NC1084.2
065200     GO       TO ABR-WRITE-GF-6.                                  NC1084.2
065300 ABR-FAIL-GF-6.                                                   NC1084.2
065400     PERFORM  FAIL.                                               NC1084.2
065500     MOVE     SYNC-GROUP TO COMPUTED-A.                           NC1084.2
065600     MOVE     SYNCHRONIZED-GROUP TO CORRECT-A.                    NC1084.2
065700 ABR-WRITE-GF-6.                                                  NC1084.2
065800     MOVE     "  SYNC" TO FEATURE                                 NC1084.2
065900     MOVE     "ABR-TEST-GF-6 " TO PAR-NAME.                       NC1084.2
066000     PERFORM  PRINT-DETAIL.                                       NC1084.2
066100 ABR-INIT-GF-7.                                                   NC1084.2
066200     MOVE   "VI-20 5.3" TO ANSI-REFERENCE.                        NC1084.2
066300 ABR-TEST-GF-7.                                                   NC1084.2
066400     IF       SYNC-RIGHT-GROUP EQUAL TO SYNCHRONIZED-RIGHT-GROUP  NC1084.2
066500              PERFORM PASS GO TO ABR-WRITE-GF-7.                  NC1084.2
066600     GO       TO ABR-FAIL-GF-7.                                   NC1084.2
066700 ABR-DELETE-GF-7.                                                 NC1084.2
066800     PERFORM  DE-LETE.                                            NC1084.2
066900     GO       TO ABR-WRITE-GF-7.                                  NC1084.2
067000 ABR-FAIL-GF-7.                                                   NC1084.2
067100     PERFORM  FAIL.                                               NC1084.2
067200     MOVE     SYNC-RIGHT-GROUP TO COMPUTED-A.                     NC1084.2
067300     MOVE     SYNCHRONIZED-RIGHT-GROUP TO CORRECT-A.              NC1084.2
067400 ABR-WRITE-GF-7.                                                  NC1084.2
067500     MOVE     "ABR-TEST-GF-7 " TO PAR-NAME.                       NC1084.2
067600     PERFORM  PRINT-DETAIL.                                       NC1084.2
067700 ABR-INIT-GF-8.                                                   NC1084.2
067800     MOVE   "VI-20 5.3" TO ANSI-REFERENCE.                        NC1084.2
067900 ABR-TEST-GF-8.                                                   NC1084.2
068000     IF       SYNC-LEFT-GROUP EQUAL TO SYNCHRONIZED-LEFT-GROUP    NC1084.2
068100              PERFORM PASS GO TO ABR-WRITE-GF-8.                  NC1084.2
068200     GO       TO ABR-FAIL-GF-8.                                   NC1084.2
068300 ABR-DELETE-GF-8.                                                 NC1084.2
068400     PERFORM  DE-LETE.                                            NC1084.2
068500     GO       TO ABR-WRITE-GF-8.                                  NC1084.2
068600 ABR-FAIL-GF-8.                                                   NC1084.2
068700     PERFORM  FAIL.                                               NC1084.2
068800     MOVE     SYNC-LEFT-GROUP TO COMPUTED-A.                      NC1084.2
068900     MOVE     SYNCHRONIZED-LEFT-GROUP TO CORRECT-A.               NC1084.2
069000 ABR-WRITE-GF-8.                                                  NC1084.2
069100     MOVE     "ABR-TEST-GF-8 " TO PAR-NAME.                       NC1084.2
069200     PERFORM  PRINT-DETAIL.                                       NC1084.2
069300 ABR-INIT-GF-9.                                                   NC1084.2
069400     MOVE   "VI-20 5.3" TO ANSI-REFERENCE.                        NC1084.2
069500 ABR-TEST-GF-9.                                                   NC1084.2
069600A    MOVE ZERO TO ONE                                             NC1084.2
069700A    IF ON-SWITCH ADD 1 TO ONE.                                   NC1084.2
069800A    IF OFF-SWITCH ADD 1 TO ONE.                                  NC1084.2
069900A    IF ONE EQUAL TO 1 PERFORM PASS GO TO ABR-WRITE-GF-9          NC1084.2
070000A             ELSE MOVE 1 TO ONE GO TO ABR-FAIL-GF-9.             NC1084.2
070100 ABR-DELETE-GF-9.                                                 NC1084.2
070200     PERFORM  DE-LETE.                                            NC1084.2
070300     GO       TO ABR-WRITE-GF-9.                                  NC1084.2
070400 ABR-FAIL-GF-9.                                                   NC1084.2
070500     PERFORM  FAIL.                                               NC1084.2
070600     MOVE "NOT BOOLEAN COMPLEMENTS" TO RE-MARK.                   NC1084.2
070700 ABR-WRITE-GF-9.                                                  NC1084.2
070800     MOVE "SPECIAL-NAMES SWITCH" TO FEATURE.                      NC1084.2
070900     MOVE     "ABR-TEST-GF-9 " TO PAR-NAME.                       NC1084.2
071000     PERFORM  PRINT-DETAIL.                                       NC1084.2
071100 ABR-INIT-GF-10.                                                  NC1084.2
071200     MOVE   "VI-20 5.3" TO ANSI-REFERENCE.                        NC1084.2
071300     MOVE     ZERO TO FL-LESS.                                    NC1084.2
071400     MOVE FL-LESS  TO COMPUTED-A.                                 NC1084.2
071500     MOVE "      <.00" TO CORRECT-A.                              NC1084.2
071600 ABR-TEST-GF-10.                                                  NC1084.2
071700     IF   FL-LESS EQUAL TO "      <.00"                           NC1084.2
071800          MOVE "FAILURE IF DOLLAR APPEARS" TO RE-MARK             NC1084.2
071900          GO TO ABR-WRITE-GF-10.                                  NC1084.2
072000     GO       TO ABR-FAIL-GF-10.                                  NC1084.2
072100 ABR-DELETE-GF-10.                                                NC1084.2
072200     PERFORM  DE-LETE.                                            NC1084.2
072300     GO       TO ABR-WRITE-GF-10.                                 NC1084.2
072400 ABR-FAIL-GF-10.                                                  NC1084.2
072500     PERFORM  FAIL.                                               NC1084.2
072600     MOVE "LESS THAN SHOULD APPEAR" TO RE-MARK.                   NC1084.2
072700 ABR-WRITE-GF-10.                                                 NC1084.2
072800     MOVE     "ABR-TEST-GF-10" TO PAR-NAME.                       NC1084.2
072900     MOVE "SPECIAL-NAMES CURNCY" TO FEATURE.                      NC1084.2
073000     PERFORM  PRINT-DETAIL.                                       NC1084.2
073100 CHA-INIT-1.                                                      NC1084.2
073200     MOVE   "III-3" TO ANSI-REFERENCE.                            NC1084.2
073300 CHA-GF-1-1.                                                      NC1084.2
073400     IF       XCHAR-SET EQUAL TO                                  NC1084.2
073500     "ABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789 +-*/=$,.;()><"        NC1084.2
073600              PERFORM PASS ELSE PERFORM FAIL.                     NC1084.2
073700*        NOTE 51 CHARACTER LITERAL INCLUDES TWO SPACES BUT NO     NC1084.2
073800*             QUOTE.                                              NC1084.2
073900     GO       TO CHA-WRITE-GF-1-1.                                NC1084.2
074000 CHA-DELETE-GF-1-1.                                               NC1084.2
074100     PERFORM  DE-LETE.                                            NC1084.2
074200 CHA-WRITE-GF-1-1.                                                NC1084.2
074300     MOVE     "CHARACTER-SET" TO FEATURE.                         NC1084.2
074400     MOVE     "CHA-GF-1-1" TO PAR-NAME.                           NC1084.2
074500     PERFORM  PRINT-DETAIL.                                       NC1084.2
074600 CHA-GF-1-2.                                                      NC1084.2
074700     IF       CHARACTER-QUOTE = QUOTE                             NC1084.2
074800              PERFORM PASS ELSE PERFORM FAIL.                     NC1084.2
074900     GO       TO CHA-WRITE-GF-1-2.                                NC1084.2
075000 CHA-DELETE-GF-1-2.                                               NC1084.2
075100     PERFORM  DE-LETE.                                            NC1084.2
075200 CHA-WRITE-GF-1-2.                                                NC1084.2
075300     MOVE     "CHARACTER-SET" TO FEATURE.                         NC1084.2
075400     MOVE     "CHA-GF-1-2" TO PAR-NAME.                           NC1084.2
075500     PERFORM  PRINT-DETAIL.                                       NC1084.2
075600 CHA-GF-1-3.                                                      NC1084.2
075700     IF       CHARACTER-LOW = "abcdefghijklmnopqrstuvwxyz"        NC1084.2
075800              PERFORM PASS ELSE PERFORM FAIL.                     NC1084.2
075900     GO       TO CHA-WRITE-GF-1-3.                                NC1084.2
076000 CHA-DELETE-GF-1-3.                                               NC1084.2
076100     PERFORM  DE-LETE.                                            NC1084.2
076200 CHA-WRITE-GF-1-3.                                                NC1084.2
076300     MOVE     "CHARACTER-SET" TO FEATURE.                         NC1084.2
076400     MOVE     "CHA-GF-1-3" TO PAR-NAME.                           NC1084.2
076500     PERFORM  PRINT-DETAIL.                                       NC1084.2
076600*                                                                 NC1084.2
076700 ALPHABET-INIT-10.                                                NC1084.2
076800     MOVE   "VI-15 4.5.4 GR4" TO ANSI-REFERENCE.                  NC1084.2
076900 ALPHABET-TEST-10.                                                NC1084.2
077000     PERFORM END-ROUTINE.                                         NC1084.2
077100     MOVE    " ALPHABET-NAME     *****     CHECK THE ALPHABET-NAMENC1084.2
077200-    " IN THE SPECIAL-NAMES PARAGRAPH" TO TEST-RESULTS.           NC1084.2
077300     PERFORM PRINT-DETAIL.                                        NC1084.2
077400*                                                                 NC1084.2
077500 CCVS-EXIT SECTION.                                               NC1084.2
077600 CCVS-999999.                                                     NC1084.2
077700     GO TO CLOSE-FILES.                                           NC1084.2
*END-OF,NC108M                                                                  
*HEADER,COBOL,NC109M                                                            
000100 IDENTIFICATION DIVISION.                                         NC1094.2
000200 PROGRAM-ID.                                                      NC1094.2
000300     NC109M.                                                      NC1094.2
000400****************************************************************  NC1094.2
000500*                                                              *  NC1094.2
000600*    VALIDATION FOR:-                                          *  NC1094.2
000700*                                                              *  NC1094.2
000800*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC1094.2
000900*                                                              *  NC1094.2
001000*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC1094.2
001100*                                                              *  NC1094.2
001200****************************************************************  NC1094.2
001300*                                                              *  NC1094.2
001400*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  NC1094.2
001500*                                                              *  NC1094.2
001600*        X-55  - SYSTEM PRINTER NAME.                          *  NC1094.2
001700*        X-82  - SOURCE COMPUTER NAME.                         *  NC1094.2
001800*        X-83  - OBJECT COMPUTER NAME.                         *  NC1094.2
001900*                                                              *  NC1094.2
002000****************************************************************  NC1094.2
002100*                                                                 NC1094.2
002200*      PROGRAM NC109M TESTS FORMAT 1 OF THE ACCEPT STATEMENT      NC1094.2
002300*      AND THE GENERAL FORMAT OF THE DISPLAY STATEMENT.           NC1094.2
002400*                                                                 NC1094.2
002500*                                                                 NC1094.2
002600                                                                  NC1094.2
002700 ENVIRONMENT DIVISION.                                            NC1094.2
002800 CONFIGURATION SECTION.                                           NC1094.2
002900 SOURCE-COMPUTER.                                                 NC1094.2
003000     XXXXX082.                                                    NC1094.2
003100 OBJECT-COMPUTER.                                                 NC1094.2
003200     XXXXX083.                                                    NC1094.2
003300 INPUT-OUTPUT SECTION.                                            NC1094.2
003400 FILE-CONTROL.                                                    NC1094.2
003500     SELECT PRINT-FILE ASSIGN TO                                  NC1094.2
003600     XXXXX055.                                                    NC1094.2
003700 DATA DIVISION.                                                   NC1094.2
003800 FILE SECTION.                                                    NC1094.2
003900 FD  PRINT-FILE.                                                  NC1094.2
004000 01  PRINT-REC PICTURE X(120).                                    NC1094.2
004100 01  DUMMY-RECORD PICTURE X(120).                                 NC1094.2
004200 WORKING-STORAGE SECTION.                                         NC1094.2
004300 01  CHARACTER-BREAKDOWN-R.                                       NC1094.2
004400     02  FIRST-20R               PICTURE X(20).                   NC1094.2
004500     02  SECOND-20R              PICTURE X(20).                   NC1094.2
004600     02  THIRD-20R               PICTURE X(20).                   NC1094.2
004700     02  FOURTH-20R              PICTURE X(20).                   NC1094.2
004800 01  CHARACTER-BREAKDOWN-S.                                       NC1094.2
004900     02  FIRST-20S               PICTURE X(20).                   NC1094.2
005000     02  SECOND-20S              PICTURE X(20).                   NC1094.2
005100     02  THIRD-20S               PICTURE X(20).                   NC1094.2
005200     02  FOURTH-20S              PICTURE X(20).                   NC1094.2
005300 01  X80-CHARACTER-FIELD.                                         NC1094.2
005400     02  FILLER                  PICTURE X(80).                   NC1094.2
005500 01  ACCEPT-RESULTS.                                              NC1094.2
005600     02  FILLER                  PICTURE X(80)  VALUE             NC1094.2
005700     "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z  0123456NC1094.2
005800-    "789                 ".                                      NC1094.2
005900 01  DISPLAY-DATA.                                                NC1094.2
006000     02 DISPLAY-A.                                                NC1094.2
006100       03 DISPLAY-03               PICTURE A     VALUE "A".       NC1094.2
006200       03 DISPLAY-03A.                                            NC1094.2
006300         04 DISPLAY-04             PICTURE A     VALUE "L".       NC1094.2
006400         04 DISPLAY-04A.                                          NC1094.2
006500           05 DISPLAY-05           PICTURE A     VALUE "P".       NC1094.2
006600           05 DISPLAY-05A.                                        NC1094.2
006700             06 DISPLAY-06         PICTURE A     VALUE "H".       NC1094.2
006800             06 DISPLAY-06A.                                      NC1094.2
006900               07 DISPLAY-07       PICTURE A     VALUE "A".       NC1094.2
007000               07 DISPLAY-07A.                                    NC1094.2
007100                 08 DISPLAY-08     PICTURE A     VALUE "B".       NC1094.2
007200                 08 DISPLAY-08A.                                  NC1094.2
007300                   09 DISPLAY-09   PICTURE A     VALUE "E".       NC1094.2
007400                   09 DISPLAY-09A.                                NC1094.2
007500                     10 DISPLAY-10 PICTURE AAA   VALUE "TIC".     NC1094.2
007600     02 DISPLAY-N       PICTURE 9(10)       VALUE 0123456789.     NC1094.2
007700     02 DISPLAY-X       PICTURE X(10)       VALUE "A1B2C3D4E5".   NC1094.2
007800     02 DISPLAY-B       PICTURE X(13).                            NC1094.2
007900     02 DISPLAY-C REDEFINES DISPLAY-B.                            NC1094.2
008000       03 DISPLAY-D     PICTURE X(8).                             NC1094.2
008100       03 DISPLAY-E     PICTURE X(5).                             NC1094.2
008200     02 DISPLAY-F.                                                NC1094.2
008300       03 DISPLAY-G     PICTURE X(100)      VALUE "*001*002*003*00NC1094.2
008400-    "4*005*006*007*008*009*010*011*012*013*014*015*016*017*018*01NC1094.2
008500-    "9*020*021*022*023*024*025".                                 NC1094.2
008600       03 DISPLAY-H     PICTURE X(100)      VALUE "*026*027*028*02NC1094.2
008700-    "9*030*031*032*033*034*035*036*037*038*039*040*041*042*043*04NC1094.2
008800-    "4*045*046*047*048*049*050".                                 NC1094.2
008900     02 SEE-ABOVE       PICTURE X(9) VALUE "SEE ABOVE".           NC1094.2
009000     02 SEE-BELOW       PICTURE X(9) VALUE "SEE BELOW".           NC1094.2
009100     02 CORRECT-FOLLOWS PICTURE X(20) VALUE                       NC1094.2
009200                                      "CORRECT DATA FOLLOWS".     NC1094.2
009300     02 END-CORRECT     PICTURE X(16) VALUE                       NC1094.2
009400                                      "END CORRECT DATA".         NC1094.2
009500     02 DISPLAY-WRITER.                                           NC1094.2
009600       03 DIS-PLAYER.                                             NC1094.2
009700         04 FILLER      PICTURE X(6).                             NC1094.2
009800         04 QUOTE-SLOT  PICTURE X.                                NC1094.2
009900         04 FILLER      PICTURE X(112).                           NC1094.2
010000     02 DISPLAY-SWITCH  PICTURE 9 VALUE ZERO.                     NC1094.2
010100     02 ZERO-SPACE-QUOTE.                                         NC1094.2
010200       03 FILLER        PICTURE X VALUE ZERO.                     NC1094.2
010300       03 FILLER        PICTURE X VALUE SPACE.                    NC1094.2
010400       03 FILLER        PICTURE X VALUE QUOTE.                    NC1094.2
010500 01  LONG-LITERAL.                                                NC1094.2
010600     02 LONG20                          PICTURE IS X(20)          NC1094.2
010700     VALUE IS "STANDARD COMPILERS M".                             NC1094.2
010800     02 LONG40                          PICTURE IS X(20)          NC1094.2
010900     VALUE IS "UST ALLOW NON-NUMERI".                             NC1094.2
011000     02 LONG60                          PICTURE IS X(20)          NC1094.2
011100     VALUE IS "C LITERALS OF AT LEA".                             NC1094.2
011200     02 LONG80                          PICTURE IS X(20)          NC1094.2
011300     VALUE IS "ST 120 CHARACTERS AN".                             NC1094.2
011400     02 LONG100                         PICTURE IS X(20)          NC1094.2
011500     VALUE IS "D NUMERIC LITERALS O".                             NC1094.2
011600     02 LONG120                         PICTURE IS X(20)          NC1094.2
011700     VALUE IS "F AT LEAST 18 DIGITS".                             NC1094.2
011800 01  ACCEPT-DATA.                                                 NC1094.2
011900     02 ACCEPT-D1.                                                NC1094.2
012000       03 ACCEPT-D1-A PICTURE X(20).                              NC1094.2
012100       03 ACCEPT-D1-B PICTURE X(7).                               NC1094.2
012200     02 ACCEPT-D2       PICTURE X(27)                             NC1094.2
012300                        VALUE "ABCDEFGHIJKLMNOPQRSTUVWXY Z".      NC1094.2
012400     02 ACCEPT-D3  PICTURE 9(10) USAGE DISPLAY.                   NC1094.2
012500     02 ACCEPT-D4  PICTURE 9(10) USAGE DISPLAY VALUE 0123456789.  NC1094.2
012600     02 ACCEPT-D5  PICTURE X(11).                                 NC1094.2
012700     02  ACCEPT-D6 PICTURE X(11) VALUE "().+-*/$, =".             NC1094.2
012800     02 ACCEPT-D7  PICTURE X.                                     NC1094.2
012900     02 ACCEPT-D8 PICTURE X VALUE "9".                            NC1094.2
013000     02 ACCEPT-D9  PICTURE X.                                     NC1094.2
013100     02 ACCEPT-D10 PICTURE X VALUE "0".                           NC1094.2
013200     02 ACCEPT-D11 PICTURE A(20).                                 NC1094.2
013300     02 ACCEPT-D12 PICTURE A(20)                                  NC1094.2
013400              VALUE " ABC            XYZ ".                       NC1094.2
013500     02 ACCEPT-D13 PICTURE 9(9).                                  NC1094.2
013600     02 ACCEPT-D14 PICTURE 9(9) VALUE 012345678.                  NC1094.2
013700     02 ACCEPT-D15 PICTURE X.                                     NC1094.2
013800     02 ACCEPT-D16 PICTURE X VALUE SPACE.                         NC1094.2
013900     02 ACCEPT-D17 PICTURE X.                                     NC1094.2
014000     02 ACCEPT-D18 PICTURE X VALUE QUOTE.                         NC1094.2
014100     02 ACCEPT-D21.                                               NC1094.2
014200         03 TAB-ACCEPT PICTURE XXXX OCCURS 3 TIMES.               NC1094.2
014300     02 ACCEPT-D22 PICTURE X(12) VALUE "....ABCD....".            NC1094.2
014400 01  TAB-VALUE                    PICTURE X(21)                   NC1094.2
014500                                  VALUE "ABCDEFGHIJKLMNOPQRSTU".  NC1094.2
014600 01  NO-TAB-RECORD REDEFINES TAB-VALUE.                           NC1094.2
014700     02 X1    PICTURE X.                                          NC1094.2
014800     02 X2    PICTURE X.                                          NC1094.2
014900     02 X3    PICTURE X.                                          NC1094.2
015000     02 X4    PICTURE X.                                          NC1094.2
015100     02 X5    PICTURE X.                                          NC1094.2
015200     02 X6    PICTURE X.                                          NC1094.2
015300     02 X7    PICTURE X.                                          NC1094.2
015400     02 X8    PICTURE X.                                          NC1094.2
015500     02 X9    PICTURE X.                                          NC1094.2
015600     02 X10   PICTURE X.                                          NC1094.2
015700     02 X11   PICTURE X.                                          NC1094.2
015800     02 X12   PICTURE X.                                          NC1094.2
015900     02 X13   PICTURE X.                                          NC1094.2
016000     02 X14   PICTURE X.                                          NC1094.2
016100     02 X15   PICTURE X.                                          NC1094.2
016200     02 X16   PICTURE X.                                          NC1094.2
016300     02 X17   PICTURE X.                                          NC1094.2
016400     02 X18   PICTURE X.                                          NC1094.2
016500     02 X19   PICTURE X.                                          NC1094.2
016600     02 X20   PICTURE X.                                          NC1094.2
016700     02 X21   PICTURE X.                                          NC1094.2
016800 01  TAB-RECORD REDEFINES TAB-VALUE.                              NC1094.2
016900     02 XTAB                      PICTURE X OCCURS 21 TIMES.      NC1094.2
017000 01  DISPLAY-MIXTURE.                                             NC1094.2
017100     02 I-DATA                    PICTURE X(17)                   NC1094.2
017200                                  VALUE " IDENTIFIER DATA ".      NC1094.2
017300     02 TA-VALUE                  PICTURE X(20)                   NC1094.2
017400              VALUE "A B C D E 0102030405".                       NC1094.2
017500     02 TA-BLE REDEFINES TA-VALUE.                                NC1094.2
017600             04 PIECE-A           PICTURE XX OCCURS 5 TIMES.      NC1094.2
017700             04 PIECE-N           PICTURE 99 OCCURS 5 TIMES.      NC1094.2
017800     02 TRUE-PAIR.                                                NC1094.2
017900         03 A1                    PICTURE X(21)                   NC1094.2
018000                                  VALUE " (TOTAL 21 OPERANDS) ".  NC1094.2
018100         03 A2                    PICTURE X(11)                   NC1094.2
018200                                  VALUE "END OF DATA".            NC1094.2
018300 01  TEST-RESULTS.                                                NC1094.2
018400     02 FILLER                   PIC X      VALUE SPACE.          NC1094.2
018500     02 FEATURE                  PIC X(20)  VALUE SPACE.          NC1094.2
018600     02 FILLER                   PIC X      VALUE SPACE.          NC1094.2
018700     02 P-OR-F                   PIC X(5)   VALUE SPACE.          NC1094.2
018800     02 FILLER                   PIC X      VALUE SPACE.          NC1094.2
018900     02  PAR-NAME.                                                NC1094.2
019000       03 FILLER                 PIC X(19)  VALUE SPACE.          NC1094.2
019100       03  PARDOT-X              PIC X      VALUE SPACE.          NC1094.2
019200       03 DOTVALUE               PIC 99     VALUE ZERO.           NC1094.2
019300     02 FILLER                   PIC X(8)   VALUE SPACE.          NC1094.2
019400     02 RE-MARK                  PIC X(61).                       NC1094.2
019500 01  TEST-COMPUTED.                                               NC1094.2
019600     02 FILLER                   PIC X(30)  VALUE SPACE.          NC1094.2
019700     02 FILLER                   PIC X(17)  VALUE                 NC1094.2
019800            "       COMPUTED=".                                   NC1094.2
019900     02 COMPUTED-X.                                               NC1094.2
020000     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          NC1094.2
020100     03 COMPUTED-N               REDEFINES COMPUTED-A             NC1094.2
020200                                 PIC -9(9).9(9).                  NC1094.2
020300     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         NC1094.2
020400     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     NC1094.2
020500     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     NC1094.2
020600     03       CM-18V0 REDEFINES COMPUTED-A.                       NC1094.2
020700         04 COMPUTED-18V0                    PIC -9(18).          NC1094.2
020800         04 FILLER                           PIC X.               NC1094.2
020900     03 FILLER PIC X(50) VALUE SPACE.                             NC1094.2
021000 01  TEST-CORRECT.                                                NC1094.2
021100     02 FILLER PIC X(30) VALUE SPACE.                             NC1094.2
021200     02 FILLER PIC X(17) VALUE "       CORRECT =".                NC1094.2
021300     02 CORRECT-X.                                                NC1094.2
021400     03 CORRECT-A                  PIC X(20) VALUE SPACE.         NC1094.2
021500     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      NC1094.2
021600     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         NC1094.2
021700     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     NC1094.2
021800     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     NC1094.2
021900     03      CR-18V0 REDEFINES CORRECT-A.                         NC1094.2
022000         04 CORRECT-18V0                     PIC -9(18).          NC1094.2
022100         04 FILLER                           PIC X.               NC1094.2
022200     03 FILLER PIC X(2) VALUE SPACE.                              NC1094.2
022300     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     NC1094.2
022400 01  CCVS-C-1.                                                    NC1094.2
022500     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PANC1094.2
022600-    "SS  PARAGRAPH-NAME                                          NC1094.2
022700-    "       REMARKS".                                            NC1094.2
022800     02 FILLER                     PIC X(20)    VALUE SPACE.      NC1094.2
022900 01  CCVS-C-2.                                                    NC1094.2
023000     02 FILLER                     PIC X        VALUE SPACE.      NC1094.2
023100     02 FILLER                     PIC X(6)     VALUE "TESTED".   NC1094.2
023200     02 FILLER                     PIC X(15)    VALUE SPACE.      NC1094.2
023300     02 FILLER                     PIC X(4)     VALUE "FAIL".     NC1094.2
023400     02 FILLER                     PIC X(94)    VALUE SPACE.      NC1094.2
023500 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       NC1094.2
023600 01  REC-CT                        PIC 99       VALUE ZERO.       NC1094.2
023700 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       NC1094.2
023800 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       NC1094.2
023900 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       NC1094.2
024000 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       NC1094.2
024100 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       NC1094.2
024200 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       NC1094.2
024300 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      NC1094.2
024400 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       NC1094.2
024500 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     NC1094.2
024600 01  CCVS-H-1.                                                    NC1094.2
024700     02  FILLER                    PIC X(39)    VALUE SPACES.     NC1094.2
024800     02  FILLER                    PIC X(42)    VALUE             NC1094.2
024900     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 NC1094.2
025000     02  FILLER                    PIC X(39)    VALUE SPACES.     NC1094.2
025100 01  CCVS-H-2A.                                                   NC1094.2
025200   02  FILLER                        PIC X(40)  VALUE SPACE.      NC1094.2
025300   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  NC1094.2
025400   02  FILLER                        PIC XXXX   VALUE             NC1094.2
025500     "4.2 ".                                                      NC1094.2
025600   02  FILLER                        PIC X(28)  VALUE             NC1094.2
025700            " COPY - NOT FOR DISTRIBUTION".                       NC1094.2
025800   02  FILLER                        PIC X(41)  VALUE SPACE.      NC1094.2
025900                                                                  NC1094.2
026000 01  CCVS-H-2B.                                                   NC1094.2
026100   02  FILLER                        PIC X(15)  VALUE             NC1094.2
026200            "TEST RESULT OF ".                                    NC1094.2
026300   02  TEST-ID                       PIC X(9).                    NC1094.2
026400   02  FILLER                        PIC X(4)   VALUE             NC1094.2
026500            " IN ".                                               NC1094.2
026600   02  FILLER                        PIC X(12)  VALUE             NC1094.2
026700     " HIGH       ".                                              NC1094.2
026800   02  FILLER                        PIC X(22)  VALUE             NC1094.2
026900            " LEVEL VALIDATION FOR ".                             NC1094.2
027000   02  FILLER                        PIC X(58)  VALUE             NC1094.2
027100     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC1094.2
027200 01  CCVS-H-3.                                                    NC1094.2
027300     02  FILLER                      PIC X(34)  VALUE             NC1094.2
027400            " FOR OFFICIAL USE ONLY    ".                         NC1094.2
027500     02  FILLER                      PIC X(58)  VALUE             NC1094.2
027600     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC1094.2
027700     02  FILLER                      PIC X(28)  VALUE             NC1094.2
027800            "  COPYRIGHT   1985 ".                                NC1094.2
027900 01  CCVS-E-1.                                                    NC1094.2
028000     02 FILLER                       PIC X(52)  VALUE SPACE.      NC1094.2
028100     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              NC1094.2
028200     02 ID-AGAIN                     PIC X(9).                    NC1094.2
028300     02 FILLER                       PIC X(45)  VALUE SPACES.     NC1094.2
028400 01  CCVS-E-2.                                                    NC1094.2
028500     02  FILLER                      PIC X(31)  VALUE SPACE.      NC1094.2
028600     02  FILLER                      PIC X(21)  VALUE SPACE.      NC1094.2
028700     02 CCVS-E-2-2.                                               NC1094.2
028800         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      NC1094.2
028900         03 FILLER                   PIC X      VALUE SPACE.      NC1094.2
029000         03 ENDER-DESC               PIC X(44)  VALUE             NC1094.2
029100            "ERRORS ENCOUNTERED".                                 NC1094.2
029200 01  CCVS-E-3.                                                    NC1094.2
029300     02  FILLER                      PIC X(22)  VALUE             NC1094.2
029400            " FOR OFFICIAL USE ONLY".                             NC1094.2
029500     02  FILLER                      PIC X(12)  VALUE SPACE.      NC1094.2
029600     02  FILLER                      PIC X(58)  VALUE             NC1094.2
029700     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC1094.2
029800     02  FILLER                      PIC X(13)  VALUE SPACE.      NC1094.2
029900     02 FILLER                       PIC X(15)  VALUE             NC1094.2
030000             " COPYRIGHT 1985".                                   NC1094.2
030100 01  CCVS-E-4.                                                    NC1094.2
030200     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      NC1094.2
030300     02 FILLER                       PIC X(4)   VALUE " OF ".     NC1094.2
030400     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      NC1094.2
030500     02 FILLER                       PIC X(40)  VALUE             NC1094.2
030600      "  TESTS WERE EXECUTED SUCCESSFULLY".                       NC1094.2
030700 01  XXINFO.                                                      NC1094.2
030800     02 FILLER                       PIC X(19)  VALUE             NC1094.2
030900            "*** INFORMATION ***".                                NC1094.2
031000     02 INFO-TEXT.                                                NC1094.2
031100       04 FILLER                     PIC X(8)   VALUE SPACE.      NC1094.2
031200       04 XXCOMPUTED                 PIC X(20).                   NC1094.2
031300       04 FILLER                     PIC X(5)   VALUE SPACE.      NC1094.2
031400       04 XXCORRECT                  PIC X(20).                   NC1094.2
031500     02 INF-ANSI-REFERENCE           PIC X(48).                   NC1094.2
031600 01  HYPHEN-LINE.                                                 NC1094.2
031700     02 FILLER  PIC IS X VALUE IS SPACE.                          NC1094.2
031800     02 FILLER  PIC IS X(65)    VALUE IS "************************NC1094.2
031900-    "*****************************************".                 NC1094.2
032000     02 FILLER  PIC IS X(54)    VALUE IS "************************NC1094.2
032100-    "******************************".                            NC1094.2
032200 01  CCVS-PGM-ID                     PIC X(9)   VALUE             NC1094.2
032300     "NC109M".                                                    NC1094.2
032400 PROCEDURE DIVISION.                                              NC1094.2
032500 CCVS1 SECTION.                                                   NC1094.2
032600 OPEN-FILES.                                                      NC1094.2
032700     OPEN     OUTPUT PRINT-FILE.                                  NC1094.2
032800     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   NC1094.2
032900     MOVE    SPACE TO TEST-RESULTS.                               NC1094.2
033000     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             NC1094.2
033100     GO TO CCVS1-EXIT.                                            NC1094.2
033200 CLOSE-FILES.                                                     NC1094.2
033300     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   NC1094.2
033400 TERMINATE-CCVS.                                                  NC1094.2
033500S    EXIT PROGRAM.                                                NC1094.2
033600STERMINATE-CALL.                                                  NC1094.2
033700     STOP     RUN.                                                NC1094.2
033800 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         NC1094.2
033900 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           NC1094.2
034000 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          NC1094.2
034100 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      NC1094.2
034200     MOVE "****TEST DELETED****" TO RE-MARK.                      NC1094.2
034300 PRINT-DETAIL.                                                    NC1094.2
034400     IF REC-CT NOT EQUAL TO ZERO                                  NC1094.2
034500             MOVE "." TO PARDOT-X                                 NC1094.2
034600             MOVE REC-CT TO DOTVALUE.                             NC1094.2
034700     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      NC1094.2
034800     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               NC1094.2
034900        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 NC1094.2
035000          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 NC1094.2
035100     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              NC1094.2
035200     MOVE SPACE TO CORRECT-X.                                     NC1094.2
035300     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         NC1094.2
035400     MOVE     SPACE TO RE-MARK.                                   NC1094.2
035500 HEAD-ROUTINE.                                                    NC1094.2
035600     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC1094.2
035700     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC1094.2
035800     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC1094.2
035900     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC1094.2
036000 COLUMN-NAMES-ROUTINE.                                            NC1094.2
036100     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC1094.2
036200     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC1094.2
036300     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        NC1094.2
036400 END-ROUTINE.                                                     NC1094.2
036500     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.NC1094.2
036600 END-RTN-EXIT.                                                    NC1094.2
036700     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC1094.2
036800 END-ROUTINE-1.                                                   NC1094.2
036900      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      NC1094.2
037000      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               NC1094.2
037100      ADD PASS-COUNTER TO ERROR-HOLD.                             NC1094.2
037200*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   NC1094.2
037300      MOVE PASS-COUNTER TO CCVS-E-4-1.                            NC1094.2
037400      MOVE ERROR-HOLD TO CCVS-E-4-2.                              NC1094.2
037500      MOVE CCVS-E-4 TO CCVS-E-2-2.                                NC1094.2
037600      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           NC1094.2
037700  END-ROUTINE-12.                                                 NC1094.2
037800      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        NC1094.2
037900     IF       ERROR-COUNTER IS EQUAL TO ZERO                      NC1094.2
038000         MOVE "NO " TO ERROR-TOTAL                                NC1094.2
038100         ELSE                                                     NC1094.2
038200         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       NC1094.2
038300     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           NC1094.2
038400     PERFORM WRITE-LINE.                                          NC1094.2
038500 END-ROUTINE-13.                                                  NC1094.2
038600     IF DELETE-COUNTER IS EQUAL TO ZERO                           NC1094.2
038700         MOVE "NO " TO ERROR-TOTAL  ELSE                          NC1094.2
038800         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      NC1094.2
038900     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   NC1094.2
039000     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC1094.2
039100      IF   INSPECT-COUNTER EQUAL TO ZERO                          NC1094.2
039200          MOVE "NO " TO ERROR-TOTAL                               NC1094.2
039300      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   NC1094.2
039400      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            NC1094.2
039500      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          NC1094.2
039600     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC1094.2
039700 WRITE-LINE.                                                      NC1094.2
039800     ADD 1 TO RECORD-COUNT.                                       NC1094.2
039900Y    IF RECORD-COUNT GREATER 42                                   NC1094.2
040000Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          NC1094.2
040100Y        MOVE SPACE TO DUMMY-RECORD                               NC1094.2
040200Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  NC1094.2
040300Y        MOVE CCVS-H-1  TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   NC1094.2
040400Y        MOVE CCVS-H-2A TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   NC1094.2
040500Y        MOVE CCVS-H-2B TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   NC1094.2
040600Y        MOVE CCVS-H-3  TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   NC1094.2
040700Y        MOVE CCVS-C-1  TO DUMMY-RECORD  PERFORM WRT-LN           NC1094.2
040800Y        MOVE CCVS-C-2  TO DUMMY-RECORD  PERFORM WRT-LN           NC1094.2
040900Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          NC1094.2
041000Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          NC1094.2
041100Y        MOVE ZERO TO RECORD-COUNT.                               NC1094.2
041200     PERFORM WRT-LN.                                              NC1094.2
041300 WRT-LN.                                                          NC1094.2
041400     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               NC1094.2
041500     MOVE SPACE TO DUMMY-RECORD.                                  NC1094.2
041600 BLANK-LINE-PRINT.                                                NC1094.2
041700     PERFORM WRT-LN.                                              NC1094.2
041800 FAIL-ROUTINE.                                                    NC1094.2
041900     IF     COMPUTED-X NOT EQUAL TO SPACE                         NC1094.2
042000            GO TO FAIL-ROUTINE-WRITE.                             NC1094.2
042100     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.NC1094.2
042200     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC1094.2
042300     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   NC1094.2
042400     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC1094.2
042500     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC1094.2
042600     GO TO  FAIL-ROUTINE-EX.                                      NC1094.2
042700 FAIL-ROUTINE-WRITE.                                              NC1094.2
042800     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         NC1094.2
042900     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 NC1094.2
043000     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. NC1094.2
043100     MOVE   SPACES TO COR-ANSI-REFERENCE.                         NC1094.2
043200 FAIL-ROUTINE-EX. EXIT.                                           NC1094.2
043300 BAIL-OUT.                                                        NC1094.2
043400     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   NC1094.2
043500     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           NC1094.2
043600 BAIL-OUT-WRITE.                                                  NC1094.2
043700     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  NC1094.2
043800     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC1094.2
043900     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC1094.2
044000     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC1094.2
044100 BAIL-OUT-EX. EXIT.                                               NC1094.2
044200 CCVS1-EXIT.                                                      NC1094.2
044300     EXIT.                                                        NC1094.2
044400 SECT-NC109M-001 SECTION.                                         NC1094.2
044500 ACC-INIT-GF-1.                                                   NC1094.2
044600     MOVE   "ACCEPT" TO FEATURE.                                  NC1094.2
044700     MOVE   "V1-71 6.5.4 GR1-5" TO ANSI-REFERENCE.                NC1094.2
044800     MOVE    SPACES TO ACCEPT-D1.                                 NC1094.2
044900 ACC-TEST-GF-1.                                                   NC1094.2
045000     MOVE    "ACC-TEST-GF-1" TO PAR-NAME.                         NC1094.2
045100     ACCEPT   ACCEPT-D1.                                          NC1094.2
045200     IF       ACCEPT-D1 EQUAL TO ACCEPT-D2                        NC1094.2
045300              PERFORM PASS GO TO ACC-WRITE-GF-1.                  NC1094.2
045400*        NOTE ACCEPT ALPHABETIC LITERAL TO ALPHANUMERIC FIELD.    NC1094.2
045500     GO TO   ACC-FAIL-GF-1.                                       NC1094.2
045600 ACC-DELETE-GF-1.                                                 NC1094.2
045700     MOVE     "ACC-TEST-GF-1" TO PAR-NAME.                        NC1094.2
045800     PERFORM  DE-LETE.                                            NC1094.2
045900     GO       TO ACC-WRITE-GF-1.                                  NC1094.2
046000 ACC-FAIL-GF-1.                                                   NC1094.2
046100     PERFORM  FAIL.                                               NC1094.2
046200     MOVE     ACCEPT-D1-A TO COMPUTED-A.                          NC1094.2
046300     MOVE     "ABCDEFGHIJKLMNOPQRST" TO CORRECT-A.                NC1094.2
046400     PERFORM  PRINT-DETAIL.                                       NC1094.2
046500     MOVE     ACCEPT-D1-B TO COMPUTED-A.                          NC1094.2
046600     MOVE     "UVWXY Z" TO CORRECT-A.                             NC1094.2
046700     MOVE     "LAST 7 OF 27-CHAR FIELD" TO RE-MARK.               NC1094.2
046800 ACC-WRITE-GF-1.                                                  NC1094.2
046900     PERFORM  PRINT-DETAIL.                                       NC1094.2
047000 ACC-INIT-GF-2.                                                   NC1094.2
047100     MOVE   "V1-71 6.5.4 GR1-5" TO ANSI-REFERENCE.                NC1094.2
047200     MOVE     ZEROES TO ACCEPT-D3.                                NC1094.2
047300 ACC-TEST-GF-2.                                                   NC1094.2
047400     ACCEPT   ACCEPT-D3.                                          NC1094.2
047500     IF       ACCEPT-D3  EQUAL TO ACCEPT-D4                       NC1094.2
047600              PERFORM PASS GO TO ACC-WRITE-GF-2.                  NC1094.2
047700*        NOTE ACCEPT NUMERIC LITERAL TO NUMERIC FIELD SAME LENGTH.NC1094.2
047800     GO       TO ACC-FAIL-GF-2.                                   NC1094.2
047900 ACC-DELETE-GF-2.                                                 NC1094.2
048000     PERFORM  DE-LETE.                                            NC1094.2
048100     GO       TO ACC-WRITE-GF-2.                                  NC1094.2
048200 ACC-FAIL-GF-2.                                                   NC1094.2
048300     MOVE     ACCEPT-D3  TO COMPUTED-18V0.                        NC1094.2
048400     MOVE     ACCEPT-D4  TO CORRECT-18V0.                         NC1094.2
048500     PERFORM  FAIL.                                               NC1094.2
048600 ACC-WRITE-GF-2.                                                  NC1094.2
048700     MOVE     "ACC-TEST-GF-2 " TO PAR-NAME.                       NC1094.2
048800     PERFORM  PRINT-DETAIL.                                       NC1094.2
048900 ACC-INIT-GF-3.                                                   NC1094.2
049000     MOVE   "V1-71 6.5.4 GR1-5" TO ANSI-REFERENCE.                NC1094.2
049100     MOVE     SPACES TO ACCEPT-D5.                                NC1094.2
049200 ACC-TEST-GF-3.                                                   NC1094.2
049300     ACCEPT   ACCEPT-D5.                                          NC1094.2
049400     IF       ACCEPT-D5  EQUAL TO ACCEPT-D6                       NC1094.2
049500              PERFORM PASS GO TO ACC-WRITE-GF-3.                  NC1094.2
049600*        NOTE ACCEPT SPECIAL CHARACTERS.                          NC1094.2
049700     GO       TO ACC-FAIL-GF-3.                                   NC1094.2
049800 ACC-DELETE-GF-3.                                                 NC1094.2
049900     PERFORM  DE-LETE.                                            NC1094.2
050000     GO       TO ACC-WRITE-GF-3.                                  NC1094.2
050100 ACC-FAIL-GF-3.                                                   NC1094.2
050200     MOVE     ACCEPT-D5  TO COMPUTED-A.                           NC1094.2
050300     MOVE     ACCEPT-D6  TO CORRECT-A.                            NC1094.2
050400     PERFORM  FAIL.                                               NC1094.2
050500 ACC-WRITE-GF-3.                                                  NC1094.2
050600     MOVE     "ACC-TEST-GF-3 " TO PAR-NAME.                       NC1094.2
050700     PERFORM  PRINT-DETAIL.                                       NC1094.2
050800 ACC-INIT-GF-4.                                                   NC1094.2
050900     MOVE   "V1-71 6.5.4 GR1-5" TO ANSI-REFERENCE.                NC1094.2
051000     MOVE     SPACES TO ACCEPT-D7.                                NC1094.2
051100 ACC-TEST-GF-4.                                                   NC1094.2
051200     ACCEPT   ACCEPT-D7.                                          NC1094.2
051300     IF       ACCEPT-D7  EQUAL TO ACCEPT-D8                       NC1094.2
051400              PERFORM PASS GO TO ACC-WRITE-GF-4.                  NC1094.2
051500*        NOTE ACCEPT HIGH-VALUE.                                  NC1094.2
051600*        NOTE CHANGED TO ACCEPT AN ALPHANUMERIC 9.                NC1094.2
051700     GO       TO ACC-FAIL-GF-4.                                   NC1094.2
051800 ACC-DELETE-GF-4.                                                 NC1094.2
051900     PERFORM  DE-LETE.                                            NC1094.2
052000     GO       TO ACC-WRITE-GF-4.                                  NC1094.2
052100 ACC-FAIL-GF-4.                                                   NC1094.2
052200     MOVE     ACCEPT-D7  TO COMPUTED-A.                           NC1094.2
052300     MOVE     ACCEPT-D8  TO CORRECT-A.                            NC1094.2
052400     PERFORM  FAIL.                                               NC1094.2
052500 ACC-WRITE-GF-4.                                                  NC1094.2
052600     MOVE     "ACC-TEST-GF-4 " TO PAR-NAME.                       NC1094.2
052700     PERFORM  PRINT-DETAIL.                                       NC1094.2
052800 ACC-INIT-GF-5.                                                   NC1094.2
052900     MOVE   "V1-71 6.5.4 GR1-5" TO ANSI-REFERENCE.                NC1094.2
053000     MOVE     SPACES TO ACCEPT-D9.                                NC1094.2
053100 ACC-TEST-GF-5.                                                   NC1094.2
053200     ACCEPT   ACCEPT-D9.                                          NC1094.2
053300     IF       ACCEPT-D9  EQUAL TO ACCEPT-D10                      NC1094.2
053400              PERFORM PASS GO TO ACC-WRITE-GF-5.                  NC1094.2
053500*        NOTE CHANGED TO ACCEPT AN ALPHANUMERIC 0.                NC1094.2
053600*        NOTE ACCEPT LOW-VALUE.                                   NC1094.2
053700     GO       TO ACC-FAIL-GF-5.                                   NC1094.2
053800 ACC-DELETE-GF-5.                                                 NC1094.2
053900     PERFORM  DE-LETE.                                            NC1094.2
054000     GO       TO ACC-WRITE-GF-5.                                  NC1094.2
054100 ACC-FAIL-GF-5.                                                   NC1094.2
054200     MOVE     ACCEPT-D9  TO COMPUTED-A.                           NC1094.2
054300     MOVE     ACCEPT-D10 TO CORRECT-A.                            NC1094.2
054400     PERFORM  FAIL.                                               NC1094.2
054500 ACC-WRITE-GF-5.                                                  NC1094.2
054600     MOVE     "ACC-TEST-GF-5 " TO PAR-NAME.                       NC1094.2
054700     PERFORM  PRINT-DETAIL.                                       NC1094.2
054800 ACC-INIT-GF-6.                                                   NC1094.2
054900     MOVE   "V1-71 6.5.4 GR1-5" TO ANSI-REFERENCE.                NC1094.2
055000     MOVE     SPACES TO ACCEPT-D11.                               NC1094.2
055100 ACC-TEST-GF-6.                                                   NC1094.2
055200     ACCEPT   ACCEPT-D11.                                         NC1094.2
055300     IF       ACCEPT-D11 EQUAL TO ACCEPT-D12                      NC1094.2
055400              PERFORM PASS GO TO ACC-WRITE-GF-6.                  NC1094.2
055500*        NOTE ACCEPT ALPHABETIC LITERAL TO ALPHABETIC FIELD.      NC1094.2
055600     GO       TO ACC-FAIL-GF-6.                                   NC1094.2
055700 ACC-DELETE-GF-6.                                                 NC1094.2
055800     PERFORM  DE-LETE.                                            NC1094.2
055900     GO       TO ACC-WRITE-GF-6.                                  NC1094.2
056000 ACC-FAIL-GF-6.                                                   NC1094.2
056100     MOVE     ACCEPT-D11 TO COMPUTED-A.                           NC1094.2
056200     MOVE     ACCEPT-D12 TO CORRECT-A.                            NC1094.2
056300     PERFORM  FAIL.                                               NC1094.2
056400 ACC-WRITE-GF-6.                                                  NC1094.2
056500     MOVE     "ACC-TEST-GF-6 " TO PAR-NAME.                       NC1094.2
056600     PERFORM  PRINT-DETAIL.                                       NC1094.2
056700 ACC-INIT-GF-7.                                                   NC1094.2
056800     MOVE   "V1-71 6.5.4 GR1-5" TO ANSI-REFERENCE.                NC1094.2
056900     MOVE     ZEROES TO ACCEPT-D13.                               NC1094.2
057000 ACC-TEST-GF-7.                                                   NC1094.2
057100     ACCEPT   ACCEPT-D13.                                         NC1094.2
057200     IF       ACCEPT-D13 EQUAL TO ACCEPT-D14                      NC1094.2
057300              PERFORM PASS GO TO ACC-WRITE-GF-7.                  NC1094.2
057400*        NOTE ACCEPT NUMERIC LITERAL TO NUMERIC FIELD OF DIFFERENTNC1094.2
057500*             LENGTH.                                             NC1094.2
057600     GO       TO ACC-FAIL-GF-7.                                   NC1094.2
057700 ACC-DELETE-GF-7.                                                 NC1094.2
057800     PERFORM  DE-LETE.                                            NC1094.2
057900     GO       TO ACC-WRITE-GF-7.                                  NC1094.2
058000 ACC-FAIL-GF-7.                                                   NC1094.2
058100     MOVE     ACCEPT-D13 TO COMPUTED-A.                           NC1094.2
058200     MOVE     ACCEPT-D14 TO CORRECT-A.                            NC1094.2
058300     PERFORM  FAIL.                                               NC1094.2
058400 ACC-WRITE-GF-7.                                                  NC1094.2
058500     MOVE     "ACC-TEST-GF-7 " TO PAR-NAME.                       NC1094.2
058600     PERFORM  PRINT-DETAIL.                                       NC1094.2
058700 ACC-INIT-GF-8.                                                   NC1094.2
058800     MOVE   "V1-71 6.5.4 GR1-5" TO ANSI-REFERENCE.                NC1094.2
058900     MOVE     ZEROES TO ACCEPT-D15.                               NC1094.2
059000 ACC-TEST-GF-8.                                                   NC1094.2
059100     ACCEPT   ACCEPT-D15.                                         NC1094.2
059200     IF       ACCEPT-D15 EQUAL TO ACCEPT-D16                      NC1094.2
059300              PERFORM PASS GO TO ACC-WRITE-GF-8.                  NC1094.2
059400*        NOTE ACCEPT SINGLE SPACE.                                NC1094.2
059500     GO       TO ACC-FAIL-GF-8.                                   NC1094.2
059600 ACC-DELETE-GF-8.                                                 NC1094.2
059700     PERFORM  DE-LETE.                                            NC1094.2
059800     GO       TO ACC-WRITE-GF-8.                                  NC1094.2
059900 ACC-FAIL-GF-8.                                                   NC1094.2
060000     PERFORM  FAIL.                                               NC1094.2
060100     MOVE     ACCEPT-D15 TO COMPUTED-A.                           NC1094.2
060200     MOVE     "      (SPACES)" TO CORRECT-A.                      NC1094.2
060300 ACC-WRITE-GF-8.                                                  NC1094.2
060400     MOVE     "ACC-TEST-GF-8" TO PAR-NAME.                        NC1094.2
060500     PERFORM  PRINT-DETAIL.                                       NC1094.2
060600 ACC-INIT-GF-9.                                                   NC1094.2
060700     MOVE   "V1-71 6.5.4 GR1-5" TO ANSI-REFERENCE.                NC1094.2
060800     MOVE     ZEROES TO ACCEPT-D17.                               NC1094.2
060900 ACC-TEST-GF-9.                                                   NC1094.2
061000     ACCEPT   ACCEPT-D17.                                         NC1094.2
061100     IF       ACCEPT-D17 EQUAL TO ACCEPT-D18                      NC1094.2
061200              PERFORM PASS GO TO ACC-WRITE-GF-9.                  NC1094.2
061300*        NOTE ACCEPT A QUOTE.                                     NC1094.2
061400     GO       TO ACC-FAIL-GF-9.                                   NC1094.2
061500 ACC-DELETE-GF-9.                                                 NC1094.2
061600     PERFORM  DE-LETE.                                            NC1094.2
061700     GO       TO ACC-WRITE-GF-9.                                  NC1094.2
061800 ACC-FAIL-GF-9.                                                   NC1094.2
061900     PERFORM  FAIL.                                               NC1094.2
062000     MOVE     ACCEPT-D17 TO COMPUTED-A.                           NC1094.2
062100     MOVE     ACCEPT-D18 TO CORRECT-A.                            NC1094.2
062200 ACC-WRITE-GF-9.                                                  NC1094.2
062300     MOVE     "ACC-TEST-GF-9" TO PAR-NAME.                        NC1094.2
062400     PERFORM  PRINT-DETAIL.                                       NC1094.2
062500 ACC-INIT-GF-10.                                                  NC1094.2
062600     MOVE   "V1-71 6.5.4 GR1-5" TO ANSI-REFERENCE.                NC1094.2
062700     MOVE     "............" TO ACCEPT-D21.                       NC1094.2
062800 ACC-TEST-GF-10.                                                  NC1094.2
062900     ACCEPT   TAB-ACCEPT (2).                                     NC1094.2
063000     IF       ACCEPT-D21 EQUAL TO ACCEPT-D22                      NC1094.2
063100              PERFORM PASS GO TO ACC-WRITE-GF-10.                 NC1094.2
063200*        NOTE ACCEPT TO SUBSCRIPTED AREA.                         NC1094.2
063300     GO       TO ACC-FAIL-GF-10.                                  NC1094.2
063400 ACC-DELETE-GF-10.                                                NC1094.2
063500     PERFORM  DE-LETE.                                            NC1094.2
063600     GO       TO ACC-WRITE-GF-10.                                 NC1094.2
063700 ACC-FAIL-GF-10.                                                  NC1094.2
063800     PERFORM  FAIL.                                               NC1094.2
063900     MOVE     ACCEPT-D21 TO COMPUTED-A.                           NC1094.2
064000     MOVE     ACCEPT-D22 TO CORRECT-A.                            NC1094.2
064100 ACC-WRITE-GF-10.                                                 NC1094.2
064200     MOVE     "ACC-TEST-GF-10" TO PAR-NAME.                       NC1094.2
064300     PERFORM  PRINT-DETAIL.                                       NC1094.2
064400 ACC-INIT-GF-11.                                                  NC1094.2
064500     MOVE   "V1-71 6.5.4 GR1-5" TO ANSI-REFERENCE.                NC1094.2
064600     MOVE    SPACES TO X80-CHARACTER-FIELD.                       NC1094.2
064700 ACC-TEST-GF-11.                                                  NC1094.2
064800     ACCEPT X80-CHARACTER-FIELD.                                  NC1094.2
064900     MOVE     "ACC-TEST-GF-11" TO PAR-NAME.                       NC1094.2
065000     IF       X80-CHARACTER-FIELD EQUAL TO ACCEPT-RESULTS         NC1094.2
065100              PERFORM PASS GO TO ACC-WRITE-GF-11.                 NC1094.2
065200*        NOTE ACCEPT 80-CHARACTER LITERAL.                        NC1094.2
065300     GO       TO ACC-FAIL-GF-11.                                  NC1094.2
065400 ACC-DELETE-GF-11.                                                NC1094.2
065500     PERFORM  DE-LETE.                                            NC1094.2
065600     MOVE     "ACC-TEST-GF-11" TO PAR-NAME.                       NC1094.2
065700     GO       TO ACC-WRITE-GF-11.                                 NC1094.2
065800 ACC-FAIL-GF-11.                                                  NC1094.2
065900     MOVE     X80-CHARACTER-FIELD TO CHARACTER-BREAKDOWN-R.       NC1094.2
066000     PERFORM  FAIL.                                               NC1094.2
066100     MOVE     ACCEPT-RESULTS TO CHARACTER-BREAKDOWN-S.            NC1094.2
066200     MOVE      FIRST-20R TO COMPUTED-A.                           NC1094.2
066300     MOVE      FIRST-20S TO CORRECT-A.                            NC1094.2
066400     PERFORM  PRINT-DETAIL.                                       NC1094.2
066500     MOVE     SECOND-20R TO COMPUTED-A.                           NC1094.2
066600     MOVE     SECOND-20S TO CORRECT-A.                            NC1094.2
066700     PERFORM  PRINT-DETAIL.                                       NC1094.2
066800     MOVE      THIRD-20R TO COMPUTED-A.                           NC1094.2
066900     MOVE      THIRD-20S TO CORRECT-A.                            NC1094.2
067000     PERFORM  PRINT-DETAIL.                                       NC1094.2
067100     MOVE     FOURTH-20R TO COMPUTED-A.                           NC1094.2
067200     MOVE     FOURTH-20S TO CORRECT-A.                            NC1094.2
067300     MOVE     "LAST 20 OF 80-CHAR FIELD" TO RE-MARK.              NC1094.2
067400 ACC-WRITE-GF-11.                                                 NC1094.2
067500     PERFORM PRINT-DETAIL.                                        NC1094.2
067600 DISP-INIT-GF-1.                                                  NC1094.2
067700     MOVE   "V1-78 6.10.4" TO ANSI-REFERENCE.                     NC1094.2
067800     PERFORM BLANK-LINE-PRINT.                                    NC1094.2
067900     MOVE   "DISPLAY TESTS" TO FEATURE.                           NC1094.2
068000     MOVE   "SEE NOTE IN DISP-INIT-GF-1" TO RE-MARK.              NC1094.2
068100     PERFORM PRINT-DETAIL.                                        NC1094.2
068200     PERFORM BLANK-LINE-PRINT 4 TIMES.                            NC1094.2
068300     MOVE     "DISPLAY" TO FEATURE.                               NC1094.2
068400*        NOTE FOR THE SAKE OF CONVENIENCE IN READING THE OUTPUT,  NC1094.2
068500*             THE DISPLAY TESTS ARE CONSTRUCTED ON THE ASSUMPTION NC1094.2
068600*             THAT THE DISPLAYED OUTPUT WILL BE PRINTED ALONG     NC1094.2
068700*             WITH THE OUTPUT FROM THE WRITE STATEMENTS ---       NC1094.2
068800*             HOWEVER IT IS NOT CONSIDERED NONSTANDARD IF THE     NC1094.2
068900*             DISPLAYED DATA APPEARS ELSEWHERE IN THE LISTING, OR NC1094.2
069000*             FOR THAT MATTER, ON SOME OTHER DEVICE.              NC1094.2
069100 DISP-TEST-GF-1.                                                  NC1094.2
069200     MOVE     "DISP-TEST-GF-1 " TO PAR-NAME.                      NC1094.2
069300     PERFORM  DISPLAY-SUPPORT-1.                                  NC1094.2
069400     DISPLAY  DISPLAY-A.                                          NC1094.2
069500*        NOTE GROUP OF ALPHABETIC DATA ITEMS.                     NC1094.2
069600     MOVE     DISPLAY-A TO DIS-PLAYER.                            NC1094.2
069700     PERFORM  DISPLAY-SUPPORT-2.                                  NC1094.2
069800     GO       TO DISP-WRITE-GF-1.                                 NC1094.2
069900 DISP-DELETE-GF-1.                                                NC1094.2
070000     PERFORM  DE-LETE.                                            NC1094.2
070100 DISP-WRITE-GF-1.                                                 NC1094.2
070200     MOVE     "DISP-TEST-GF-1 " TO PAR-NAME.                      NC1094.2
070300     PERFORM  PRINT-DETAIL.                                       NC1094.2
070400 DISP-INIT-GF-2.                                                  NC1094.2
070500     MOVE   "V1-78 6.10.4" TO ANSI-REFERENCE.                     NC1094.2
070600     MOVE     "DISP-TEST-GF-2 " TO PAR-NAME.                      NC1094.2
070700 DISP-TEST-GF-2.                                                  NC1094.2
070800     PERFORM  DISPLAY-SUPPORT-1.                                  NC1094.2
070900     DISPLAY  "ALPHABETIC LITERAL".                               NC1094.2
071000*        NOTE ALPHABETIC LITERAL.                                 NC1094.2
071100     MOVE     "ALPHABETIC LITERAL" TO DIS-PLAYER.                 NC1094.2
071200     PERFORM  DISPLAY-SUPPORT-2.                                  NC1094.2
071300     GO       TO DISP-WRITE-GF-2.                                 NC1094.2
071400 DISP-DELETE-GF-2.                                                NC1094.2
071500     PERFORM  DE-LETE.                                            NC1094.2
071600 DISP-WRITE-GF-2.                                                 NC1094.2
071700     MOVE     "DISP-TEST-GF-2 " TO PAR-NAME.                      NC1094.2
071800     PERFORM  PRINT-DETAIL.                                       NC1094.2
071900 DISP-INIT-GF-3.                                                  NC1094.2
072000     MOVE   "V1-78 6.10.4" TO ANSI-REFERENCE.                     NC1094.2
072100     MOVE     "DISP-TEST-GF-3" TO PAR-NAME.                       NC1094.2
072200     MOVE     0123456789 TO DISPLAY-N.                            NC1094.2
072300 DISP-TEST-GF-3.                                                  NC1094.2
072400     PERFORM  DISPLAY-SUPPORT-1.                                  NC1094.2
072500     DISPLAY  DISPLAY-N.                                          NC1094.2
072600*        NOTE NUMERIC DATA ITEM.                                  NC1094.2
072700     MOVE     DISPLAY-N TO DIS-PLAYER.                            NC1094.2
072800     PERFORM  DISPLAY-SUPPORT-2.                                  NC1094.2
072900     GO       TO DISP-WRITE-GF-3.                                 NC1094.2
073000 DISP-DELETE-GF-3.                                                NC1094.2
073100     PERFORM  DE-LETE.                                            NC1094.2
073200 DISP-WRITE-GF-3.                                                 NC1094.2
073300     MOVE     "DISP-TEST-GF-3 " TO PAR-NAME.                      NC1094.2
073400     PERFORM  PRINT-DETAIL.                                       NC1094.2
073500 DISP-INIT-GF-4.                                                  NC1094.2
073600     MOVE   "V1-78 6.10.4" TO ANSI-REFERENCE.                     NC1094.2
073700     MOVE     "DISP-TEST-GF-4" TO PAR-NAME.                       NC1094.2
073800 DISP-TEST-GF-4.                                                  NC1094.2
073900     PERFORM  DISPLAY-SUPPORT-1.                                  NC1094.2
074000     DISPLAY  9876543210.                                         NC1094.2
074100*        NOTE NUMERIC LITERAL.                                    NC1094.2
074200     MOVE     9876543210 TO DIS-PLAYER.                           NC1094.2
074300     PERFORM  DISPLAY-SUPPORT-2.                                  NC1094.2
074400     GO       TO DISP-WRITE-GF-4.                                 NC1094.2
074500 DISP-DELETE-GF-4.                                                NC1094.2
074600     PERFORM  DE-LETE.                                            NC1094.2
074700 DISP-WRITE-GF-4.                                                 NC1094.2
074800     MOVE     "DISP-TEST-GF-4 " TO PAR-NAME.                      NC1094.2
074900     PERFORM  PRINT-DETAIL.                                       NC1094.2
075000 DISP-INIT-GF-5.                                                  NC1094.2
075100     MOVE   "V1-78 6.10.4" TO ANSI-REFERENCE.                     NC1094.2
075200     MOVE   "DISP-TEST-GF-5" TO PAR-NAME.                         NC1094.2
075300     MOVE   "A1B2C3D4E5" TO DISPLAY-X.                            NC1094.2
075400 DISP-TEST-GF-5.                                                  NC1094.2
075500     PERFORM  DISPLAY-SUPPORT-1.                                  NC1094.2
075600     DISPLAY  DISPLAY-X.                                          NC1094.2
075700*        NOTE ALPHANUMERIC DATA ITEM.                             NC1094.2
075800     MOVE     DISPLAY-X TO DIS-PLAYER.                            NC1094.2
075900     PERFORM  DISPLAY-SUPPORT-2.                                  NC1094.2
076000     GO       TO DISP-WRITE-GF-5.                                 NC1094.2
076100 DISP-DELETE-GF-5.                                                NC1094.2
076200     PERFORM  DE-LETE.                                            NC1094.2
076300 DISP-WRITE-GF-5.                                                 NC1094.2
076400     MOVE     "DISP-TEST-GF-5 " TO PAR-NAME.                      NC1094.2
076500     PERFORM  PRINT-DETAIL.                                       NC1094.2
076600 DISP-INIT-GF-6.                                                  NC1094.2
076700     MOVE   "V1-78 6.10.4" TO ANSI-REFERENCE.                     NC1094.2
076800     MOVE   "DISP-TEST-GF-6" TO PAR-NAME.                         NC1094.2
076900 DISP-TEST-GF-6.                                                  NC1094.2
077000     PERFORM  DISPLAY-SUPPORT-1.                                  NC1094.2
077100     DISPLAY  "12345 ///// ALPHANUMERIC LITERAL".                 NC1094.2
077200*        NOTE ALPHANUMERIC LITERAL.                               NC1094.2
077300     MOVE     "12345 ///// ALPHANUMERIC LITERAL" TO DIS-PLAYER.   NC1094.2
077400     PERFORM  DISPLAY-SUPPORT-2.                                  NC1094.2
077500     GO       TO DISP-WRITE-GF-6.                                 NC1094.2
077600 DISP-DELETE-GF-6.                                                NC1094.2
077700     PERFORM  DE-LETE.                                            NC1094.2
077800 DISP-WRITE-GF-6.                                                 NC1094.2
077900     MOVE     "DISP-TEST-GF-6 " TO PAR-NAME.                      NC1094.2
078000     PERFORM  PRINT-DETAIL.                                       NC1094.2
078100 DISP-INIT-GF-7.                                                  NC1094.2
078200     MOVE   "V1-78 6.10.4" TO ANSI-REFERENCE.                     NC1094.2
078300     MOVE   "DISP-TEST-GF-7" TO PAR-NAME.                         NC1094.2
078400     MOVE   "ALPHABETIC" TO DISPLAY-A.                            NC1094.2
078500     MOVE    0123456789  TO DISPLAY-N.                            NC1094.2
078600     MOVE   "A1B2C3D4E5" TO DISPLAY-X.                            NC1094.2
078700 DISP-TEST-GF-7.                                                  NC1094.2
078800     MOVE     "DISP-TEST-GF-7 " TO PAR-NAME.                      NC1094.2
078900     PERFORM  DISPLAY-SUPPORT-1.                                  NC1094.2
079000     DISPLAY  DISPLAY-A DISPLAY-N DISPLAY-X " SERIES".            NC1094.2
079100*        NOTE SERIES OF THREE DATA ITEMS AND A LITERAL.           NC1094.2
079200     MOVE     "ALPHABETIC0123456789A1B2C3D4E5 SERIES"             NC1094.2
079300              TO DIS-PLAYER.                                      NC1094.2
079400     PERFORM  DISPLAY-SUPPORT-2.                                  NC1094.2
079500     GO       TO DISP-WRITE-GF-7.                                 NC1094.2
079600 DISP-DELETE-GF-7.                                                NC1094.2
079700     PERFORM  DE-LETE.                                            NC1094.2
079800 DISP-WRITE-GF-7.                                                 NC1094.2
079900     MOVE     "DISP-TEST-GF-7 " TO PAR-NAME.                      NC1094.2
080000     PERFORM  PRINT-DETAIL.                                       NC1094.2
080100 DISP-INIT-GF-8.                                                  NC1094.2
080200     MOVE   "V1-78 6.10.4" TO ANSI-REFERENCE.                     NC1094.2
080300     MOVE     "DISP-TEST-GF-8 " TO PAR-NAME.                      NC1094.2
080400 DISP-TEST-GF-8.                                                  NC1094.2
080500     PERFORM  DISPLAY-SUPPORT-1.                                  NC1094.2
080600     DISPLAY  ZERO SPACE QUOTE.                                   NC1094.2
080700*        NOTE SERIES OF FIGURATIVE CONSTANTS --- ONLY ONE OCCUR-  NC1094.2
080800*             RANCE OF EACH CHARACTER SHOULD APPEAR.              NC1094.2
080900     MOVE     ZERO-SPACE-QUOTE TO DIS-PLAYER.                     NC1094.2
081000     PERFORM  DISPLAY-SUPPORT-2.                                  NC1094.2
081100     GO       TO DISP-WRITE-GF-8.                                 NC1094.2
081200 DISP-DELETE-GF-8.                                                NC1094.2
081300     PERFORM  DE-LETE.                                            NC1094.2
081400 DISP-WRITE-GF-8.                                                 NC1094.2
081500     MOVE     "DISP-TEST-GF-8 " TO PAR-NAME.                      NC1094.2
081600     PERFORM  PRINT-DETAIL.                                       NC1094.2
081700 DISP-INIT-GF-9.                                                  NC1094.2
081800     MOVE   "V1-78 6.10.4" TO ANSI-REFERENCE.                     NC1094.2
081900     MOVE     "DISP-TEST-GF-9 " TO PAR-NAME.                      NC1094.2
082000     MOVE     "REDEFINE-INFO" TO DISPLAY-B.                       NC1094.2
082100 DISP-TEST-GF-9.                                                  NC1094.2
082200     PERFORM  DISPLAY-SUPPORT-1.                                  NC1094.2
082300     DISPLAY  DISPLAY-C.                                          NC1094.2
082400*        NOTE DISPLAY DATA ITEM WHICH CONTAINS A REDEFINES CLAUSE.NC1094.2
082500     MOVE     "REDEFINE-INFO" TO DIS-PLAYER.                      NC1094.2
082600     PERFORM  DISPLAY-SUPPORT-2.                                  NC1094.2
082700     GO       TO DISP-WRITE-GF-9.                                 NC1094.2
082800 DISP-DELETE-GF-9.                                                NC1094.2
082900     PERFORM  DE-LETE.                                            NC1094.2
083000 DISP-WRITE-GF-9.                                                 NC1094.2
083100     MOVE     "DISP-TEST-GF-9 " TO PAR-NAME.                      NC1094.2
083200     PERFORM  PRINT-DETAIL.                                       NC1094.2
083300 DISP-INIT-GF-10.                                                 NC1094.2
083400     MOVE   "V1-78 6.10.4" TO ANSI-REFERENCE.                     NC1094.2
083500     MOVE     "DISP-TEST-GF-10 " TO PAR-NAME.                     NC1094.2
083600 DISP-TEST-GF-10.                                                 NC1094.2
083700     PERFORM  DISPLAY-SUPPORT-1.                                  NC1094.2
083800     DISPLAY  DISPLAY-F.                                          NC1094.2
083900*        NOTE 200-CHARACTER GROUP ITEM --- ACTUAL NUMBER OF       NC1094.2
084000*             CHARACTERS DISPLAYED DEPENDS UPON THE SYSTEM.       NC1094.2
084100     MOVE     DISPLAY-G TO DIS-PLAYER.                            NC1094.2
084200     MOVE     1 TO DISPLAY-SWITCH.                                NC1094.2
084300*        NOTE THE "CORRECT" RESULT IS WRITTEN AS TWO 100-CHARACTERNC1094.2
084400*             LINES, BUT THE DIVISION OF THE DISPLAYED "COMPUTED" NC1094.2
084500*             DATA DEPENDS UPON THE SYSTEM.                       NC1094.2
084600     PERFORM  DISPLAY-SUPPORT-2.                                  NC1094.2
084700     GO       TO DISP-WRITE-GF-10.                                NC1094.2
084800 DISP-DELETE-GF-10.                                               NC1094.2
084900     PERFORM  DE-LETE.                                            NC1094.2
085000 DISP-WRITE-GF-10.                                                NC1094.2
085100     MOVE     "DISP-TEST-GF-10 " TO PAR-NAME.                     NC1094.2
085200     PERFORM  PRINT-DETAIL.                                       NC1094.2
085300 DISP-INIT-GF-11.                                                 NC1094.2
085400     MOVE   "V1-78 6.10.4" TO ANSI-REFERENCE.                     NC1094.2
085500     MOVE     "DISP-TEST-GF-11 " TO PAR-NAME.                     NC1094.2
085600     MOVE   "ABCDEFGHIJKLMNOPQRSTU" TO TAB-VALUE.                 NC1094.2
085700 DISP-TEST-GF-11.                                                 NC1094.2
085800     PERFORM  DISPLAY-SUPPORT-1.                                  NC1094.2
085900     DISPLAY  X21 X20 X19 X18 X17 X16 X15 X14 X13 X12 X11 X10 X9  NC1094.2
086000              X8  X7  X6  X5  X4  X3  X2  X1.                     NC1094.2
086100*        NOTE 21 ELEMENTARY ALPHABETIC DATA ITEMS.                NC1094.2
086200     MOVE     "UTSRQPONMLKJIHGFEDCBA" TO DIS-PLAYER.              NC1094.2
086300     PERFORM  DISPLAY-SUPPORT-2.                                  NC1094.2
086400     GO       TO DISP-WRITE-GF-11.                                NC1094.2
086500 DISP-DELETE-GF-11.                                               NC1094.2
086600     PERFORM  DE-LETE.                                            NC1094.2
086700 DISP-WRITE-GF-11.                                                NC1094.2
086800     MOVE     "DISP-TEST-GF-11 " TO PAR-NAME.                     NC1094.2
086900     PERFORM  PRINT-DETAIL.                                       NC1094.2
087000 DISP-INIT-GF-12.                                                 NC1094.2
087100     MOVE   "V1-78 6.10.4" TO ANSI-REFERENCE.                     NC1094.2
087200     MOVE     "DISP-TEST-GF-12 " TO PAR-NAME.                     NC1094.2
087300     MOVE   "ABCDEFGHIJKLMNOPQRSTU" TO TAB-VALUE.                 NC1094.2
087400 DISP-TEST-GF-12.                                                 NC1094.2
087500     PERFORM  DISPLAY-SUPPORT-1.                                  NC1094.2
087600     DISPLAY  XTAB (1)  XTAB (2)  XTAB (3)  XTAB (4)              NC1094.2
087700              XTAB (5)  XTAB (6)  XTAB (7)  XTAB (8)              NC1094.2
087800              XTAB (9)  XTAB (10)  XTAB (11)  XTAB (12)           NC1094.2
087900              XTAB (13)  XTAB (14)  XTAB (15)  XTAB (16)          NC1094.2
088000              XTAB (17)  XTAB (18)  XTAB (19)  XTAB (20)          NC1094.2
088100              XTAB (21).                                          NC1094.2
088200*        NOTE 21 SUBSCRIPTED DATA ITEMS.                          NC1094.2
088300     MOVE     "ABCDEFGHIJKLMNOPQRSTU" TO DIS-PLAYER.              NC1094.2
088400     PERFORM  DISPLAY-SUPPORT-2.                                  NC1094.2
088500     GO       TO DISP-WRITE-GF-12.                                NC1094.2
088600 DISP-DELETE-GF-12.                                               NC1094.2
088700     PERFORM  DE-LETE.                                            NC1094.2
088800 DISP-WRITE-GF-12.                                                NC1094.2
088900     MOVE     "DISP-TEST-GF-12 " TO PAR-NAME.                     NC1094.2
089000     PERFORM  PRINT-DETAIL.                                       NC1094.2
089100 DISP-INIT-GF-13.                                                 NC1094.2
089200     MOVE   "V1-78 6.10.4" TO ANSI-REFERENCE.                     NC1094.2
089300     MOVE     "DISP-TEST-GF-13 " TO PAR-NAME.                     NC1094.2
089400 DISP-TEST-GF-13.                                                 NC1094.2
089500     PERFORM  DISPLAY-SUPPORT-1.                                  NC1094.2
089600     DISPLAY  "QUOTE " QUOTE " ASTERISK " "*" " NUMERIC LITERALS "NC1094.2
089700              21 SPACE  35  I-DATA PIECE-A (1) PIECE-A (2) PIECE-ANC1094.2
089800              (3) PIECE-A (4) PIECE-A (5) PIECE-N (1) PIECE-N (2) NC1094.2
089900              PIECE-N (3) PIECE-N (4) PIECE-N (5) A1 A2.          NC1094.2
090000     MOVE       "QUOTE   ASTERISK * NUMERIC LITERALS 21 35 IDENTIFNC1094.2
090100-    "IER DATA A B C D E 0102030405 (TOTAL 21 OPERANDS) END OF DATNC1094.2
090200-    "A" TO DIS-PLAYER.                                           NC1094.2
090300*        NOTE 21 MIXED IDENTIFIERS AND LITERALS.                  NC1094.2
090400     MOVE    QUOTE TO QUOTE-SLOT.                                 NC1094.2
090500     PERFORM DISPLAY-SUPPORT-2.                                   NC1094.2
090600     GO      TO DISP-WRITE-GF-13.                                 NC1094.2
090700 DISP-DELETE-GF-13.                                               NC1094.2
090800     PERFORM DE-LETE.                                             NC1094.2
090900 DISP-WRITE-GF-13.                                                NC1094.2
091000     MOVE   "DISP-TEST-GF-13 " TO PAR-NAME.                       NC1094.2
091100     PERFORM PRINT-DETAIL.                                        NC1094.2
091200 DISP-INIT-GF-14.                                                 NC1094.2
091300*    ===---> "ALL" LITERAL  <--===                                NC1094.2
091400     MOVE   "V1-78 6.10.4" TO ANSI-REFERENCE.                     NC1094.2
091500     MOVE     "DISP-TEST-GF-14 GR3 " TO PAR-NAME.                 NC1094.2
091600 DISP-TEST-GF-14.                                                 NC1094.2
091700     PERFORM  DISPLAY-SUPPORT-1.                                  NC1094.2
091800     DISPLAY  ALL "ABCD"                                          NC1094.2
091900*        NOTE "ALL" LITERAL.                                      NC1094.2
092000     MOVE     "ABCD" TO DIS-PLAYER.                               NC1094.2
092100     PERFORM  DISPLAY-SUPPORT-2.                                  NC1094.2
092200     GO       TO DISP-WRITE-GF-14.                                NC1094.2
092300 DISP-DELETE-GF-14.                                               NC1094.2
092400     PERFORM  DE-LETE.                                            NC1094.2
092500 DISP-WRITE-GF-14.                                                NC1094.2
092600     MOVE     "DISP-TEST-GF-14 " TO PAR-NAME.                     NC1094.2
092700     PERFORM  PRINT-DETAIL.                                       NC1094.2
092800     GO TO   CCVS-EXIT.                                           NC1094.2
092900 DISPLAY-SUPPORT-1.                                               NC1094.2
093000     PERFORM  BLANK-LINE-PRINT.                                   NC1094.2
093100     MOVE     SPACE TO P-OR-F.                                    NC1094.2
093200     MOVE     SEE-BELOW TO COMPUTED-A.                            NC1094.2
093300     MOVE     SEE-BELOW TO CORRECT-A.                             NC1094.2
093400     PERFORM  PRINT-DETAIL.                                       NC1094.2
093500     MOVE     SPACE TO FEATURE.                                   NC1094.2
093600     DISPLAY  TEST-RESULTS.                                       NC1094.2
093700 DISPLAY-SUPPORT-2.                                               NC1094.2
093800     MOVE     SPACE TO TEST-RESULTS.                              NC1094.2
093900     DISPLAY  TEST-RESULTS.                                       NC1094.2
094000     MOVE     SPACE TO TEST-RESULTS.                              NC1094.2
094100     PERFORM  PRINT-DETAIL.                                       NC1094.2
094200     MOVE     CORRECT-FOLLOWS TO RE-MARK.                         NC1094.2
094300     PERFORM  PRINT-DETAIL.                                       NC1094.2
094400     PERFORM  BLANK-LINE-PRINT.                                   NC1094.2
094500     MOVE     DISPLAY-WRITER TO TEST-RESULTS.                     NC1094.2
094600     PERFORM  PRINT-DETAIL.                                       NC1094.2
094700     IF       DISPLAY-SWITCH EQUAL TO 1                           NC1094.2
094800              MOVE ZERO TO DISPLAY-SWITCH                         NC1094.2
094900              MOVE DISPLAY-H TO DIS-PLAYER                        NC1094.2
095000              MOVE DISPLAY-WRITER TO TEST-RESULTS                 NC1094.2
095100              PERFORM PRINT-DETAIL.                               NC1094.2
095200     MOVE     SPACE TO TEST-RESULTS.                              NC1094.2
095300     PERFORM  BLANK-LINE-PRINT.                                   NC1094.2
095400     IF       DISPLAY-SWITCH EQUAL TO 1                           NC1094.2
095500              MOVE "SEE NOTE IN DISP-TEST-GF-10" TO RE-MARK.      NC1094.2
095600              PERFORM PRINT-DETAIL.                               NC1094.2
095700     MOVE     "DISPLAY" TO FEATURE.                               NC1094.2
095800     MOVE     SEE-ABOVE TO COMPUTED-A.                            NC1094.2
095900     MOVE     SEE-ABOVE TO CORRECT-A.                             NC1094.2
096000     MOVE     END-CORRECT TO RE-MARK.                             NC1094.2
096100     MOVE    "ERRORS ENCOUNTERED" TO ENDER-DESC.                  NC1094.2
096200 CCVS-EXIT SECTION.                                               NC1094.2
096300 CCVS-999999.                                                     NC1094.2
096400     GO TO CLOSE-FILES.                                           NC1094.2
*END-OF,NC109M                                                                  
*HEADER,DATA*,NC109M                                                            
ABCDEFGHIJKLMNOPQRSTUVWXY Z                                                     
0123456789                                                                      
().+-*/$, =                                                                     
9                                                                               
0                                                                               
 ABC            XYZ                                                             
0123456789                                                                      
                                                                                
"                                                                               
ABCD                                                                            
A B C D E F G H I J K L M N O P Q R S T U V W X Y Z  0123456789                 
*END-OF,NC109M                                                                  