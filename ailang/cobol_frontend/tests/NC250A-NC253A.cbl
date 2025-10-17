*HEADER,COBOL,NC250A                                                            
000100 IDENTIFICATION DIVISION.                                         NC2504.2
000200 PROGRAM-ID.                                                      NC2504.2
000300     NC250A.                                                      NC2504.2
000400                                                                  NC2504.2
000500****************************************************************  NC2504.2
000600*                                                              *  NC2504.2
000700*    VALIDATION FOR:-                                          *  NC2504.2
000800*                                                              *  NC2504.2
000900*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2504.2
001000*                                                              *  NC2504.2
001100*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC2504.2
001200*                                                              *  NC2504.2
001300****************************************************************  NC2504.2
001400*                                                              *  NC2504.2
001500*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  NC2504.2
001600*                                                              *  NC2504.2
001700*        X-55  - SYSTEM PRINTER NAME.                          *  NC2504.2
001800*        X-82  - SOURCE COMPUTER NAME.                         *  NC2504.2
001900*        X-83  - OBJECT COMPUTER NAME.                         *  NC2504.2
002000*                                                              *  NC2504.2
002100****************************************************************  NC2504.2
002200*                                                                 NC2504.2
002300*    PROGRAM NC250A TESTS THE GENERAL FORMAT OF THE "IF" STATEMENTNC2504.2
002400*    A VARIETY OF QUALIFIED DATA-NAMES AND CONDITION-NAMES        NC2504.2
002500*    ARE USED.                                                    NC2504.2
002600*                                                                 NC2504.2
002700                                                                  NC2504.2
002800 ENVIRONMENT DIVISION.                                            NC2504.2
002900 CONFIGURATION SECTION.                                           NC2504.2
003000 SOURCE-COMPUTER.                                                 NC2504.2
003100     XXXXX082.                                                    NC2504.2
003200 OBJECT-COMPUTER.                                                 NC2504.2
003300     XXXXX083.                                                    NC2504.2
003400 INPUT-OUTPUT SECTION.                                            NC2504.2
003500 FILE-CONTROL.                                                    NC2504.2
003600     SELECT PRINT-FILE ASSIGN TO                                  NC2504.2
003700     XXXXX055.                                                    NC2504.2
003800 DATA DIVISION.                                                   NC2504.2
003900 FILE SECTION.                                                    NC2504.2
004000 FD  PRINT-FILE.                                                  NC2504.2
004100 01  PRINT-REC PICTURE X(120).                                    NC2504.2
004200 01  DUMMY-RECORD PICTURE X(120).                                 NC2504.2
004300 WORKING-STORAGE SECTION.                                         NC2504.2
004400 01  WRK-DU-1V0-1 PIC 9 VALUE 1.                                  NC2504.2
004500 01  WRK-DU-1V0-2 PIC 9 VALUE 2.                                  NC2504.2
004600 01  WRK-DU-1V0-3 PIC 9 VALUE 3.                                  NC2504.2
004700 01  WRK-DU-1V0-4 PIC 9 VALUE ZERO.                               NC2504.2
004800 01  WRK-DU-2V0-1 PIC 99 VALUE 10.                                NC2504.2
004900 01  WRK-DU-2V0-2 PIC 99 VALUE 11.                                NC2504.2
005000 01  WRK-DU-2V0-3 PIC 99 VALUE 12.                                NC2504.2
005100 77  SMALL-VALU   PICTURE 99 VALUE 7.                             NC2504.2
005200 77  SMALLER-VALU PICTURE 99 VALUE 6.                             NC2504.2
005300 77  SMALLEST-VALU   PICTURE 99 VALUE 5.                          NC2504.2
005400 77  EVEN-SMALLER PICTURE 99 VALUE 1.                             NC2504.2
005500 77  WRK-DS-02V00                 PICTURE S99.                    NC2504.2
005600     88 TEST-2NUC-COND-99         VALUE 99.                       NC2504.2
005700 77  WRK-DS-06V06     PICTURE S9(6)V9(6).                         NC2504.2
005800 77  WRK-DS-12V00-S REDEFINES WRK-DS-06V06                        NC2504.2
005900                                  PICTURE S9(12).                 NC2504.2
006000 77  A02TWOS-DS-02V00             PICTURE S99    VALUE 22.        NC2504.2
006100 77  WRK-DS-01V00                 PICTURE S9.                     NC2504.2
006200 77  A02TWOS-DS-03V02             PICTURE S999V99 VALUE +022.00.  NC2504.2
006300 77  A990-DS-0201P                PICTURE S99P   VALUE 990.       NC2504.2
006400 77  A02ONES-DS-02V00             PICTURE S99    VALUE 11.        NC2504.2
006500 77  A01ONE-DS-P0801              PICTURE SP(8)9 VALUE .000000001.NC2504.2
006600 77  ATWO-DS-01V00                PICTURE S9     VALUE 2.         NC2504.2
006700 77  WRK-XN-00001                 PICTURE X.                      NC2504.2
006800 77  WRK-XN-00005                 PICTURE X(5).                   NC2504.2
006900 77  TWO  PICTURE 9 VALUE 2.                                      NC2504.2
007000 77  THREE PICTURE 9 VALUE 3.                                     NC2504.2
007100 77  SEVEN PICTURE 9 VALUE 7.                                     NC2504.2
007200 77  EIGHT PICTURE 9 VALUE 8.                                     NC2504.2
007300 77  NINE  PICTURE 9 VALUE 9.                                     NC2504.2
007400 77  TEN  PICTURE 99 VALUE 10.                                    NC2504.2
007500 77  TWENTY PICTURE 99 VALUE 20.                                  NC2504.2
007600 77  ALTERCOUNT PICTURE 999 VALUE ZERO.                           NC2504.2
007700 77  XRAY PICTURE IS X.                                           NC2504.2
007800 77  IF-D1 PICTURE S9(4)V9(2) VALUE 0.                            NC2504.2
007900 77  IF-D2 PICTURE S9(4)V9(2) VALUE ZERO.                         NC2504.2
008000 77  IF-D3 PICTURE X(10) VALUE "0000000000".                      NC2504.2
008100 77  IF-D4 PICTURE X(15) VALUE "               ".                 NC2504.2
008200 77  IF-D5 PICTURE X(10) VALUE ALL QUOTE.                         NC2504.2
008300 77  IF-D6 PICTURE A(10) VALUE "BABABABABA".                      NC2504.2
008400 77  IF-D7 PICTURE S9(6)V9(4) VALUE +123.45.                      NC2504.2
008500 77  IF-D8 PICTURE 9(6)V9(4) VALUE 12300.                         NC2504.2
008600 77  IF-D9 PICTURE X(3) VALUE "123".                              NC2504.2
008700 77  IF-D11 PICTURE X(6) VALUE "ABCDEF".                          NC2504.2
008800 77  IF-D13 PICTURE 9(6)V9(4) VALUE 12300.                        NC2504.2
008900 77  IF-D14 PICTURE S9(4)V9(2) VALUE +123.45.                     NC2504.2
009000 77  IF-D15 PICTURE S999PP VALUE 12300.                           NC2504.2
009100 77  IF-D16 PICTURE PP99 VALUE .0012.                             NC2504.2
009200 77  IF-D17 PICTURE SV9(4) VALUE .0012.                           NC2504.2
009300 77  IF-D18 PICTURE X(10) VALUE "BABABABABA".                     NC2504.2
009400 77  IF-D19 PICTURE X(10) VALUE "ABCDEF    ".                     NC2504.2
009500 77  IF-D23 PICTURE $9,9B9.90+.                                   NC2504.2
009600 77  IF-D24 PICTURE X(10) VALUE "$1,2 3.40+".                     NC2504.2
009700 77  IF-D25 PICTURE ABABX0A.                                      NC2504.2
009800 77  IF-D26 PICTURE X(8) VALUE "A C D0E".                         NC2504.2
009900 77  IF-D27 PICTURE IS 9(6)V9(4) VALUE IS 2137.45                 NC2504.2
010000     USAGE IS COMPUTATIONAL.                                      NC2504.2
010100 77  IF-D28 PICTURE IS 999999V9999 VALUE IS 2137.45.              NC2504.2
010200 77  IF-D31 PICTURE S9(6) VALUE -123.                             NC2504.2
010300 77  IF-D32 PICTURE S9(4)V99.                                     NC2504.2
010400     88  A  VALUE 1.                                              NC2504.2
010500     88  B VALUES ARE 2 THRU 4.                                   NC2504.2
010600     88  C VALUE IS ZERO.                                         NC2504.2
010700     88  D VALUE IS +12.34.                                       NC2504.2
010800     88  E  VALUE IS .01, .11, .21 .81.                           NC2504.2
010900     88  F  VALUE IS 100 THRU 128 1000 THRU 1280 -9 THRU -2.      NC2504.2
011000     88  G  VALUE IS 8765.43 1234 THRU 5678 5 -9999 THRU 10.      NC2504.2
011100 77  IF-D33 PICTURE X(4).                                         NC2504.2
011200     88  B   VALUE QUOTE.                                         NC2504.2
011300     88  C   VALUE SPACE.                                         NC2504.2
011400     88 D VALUE ALL "BAC".                                        NC2504.2
011500 77  IF-D34 PICTURE A(4).                                         NC2504.2
011600     88  B VALUE "A A ".                                          NC2504.2
011700 77  IF-D37 PICTURE 9(5) VALUE 12345.                             NC2504.2
011800 77  IF-D38 PICTURE X(9) VALUE "12345    ".                       NC2504.2
011900 77  CCON-1 PICTURE 99 VALUE 11.                                  NC2504.2
012000 77  CCON-2 PICTURE 99 VALUE 12.                                  NC2504.2
012100 77  CCON-3 PICTURE 99 VALUE 13.                                  NC2504.2
012200 77  COMP-SGN1  PICTURE S9(1) VALUE +9 COMPUTATIONAL.             NC2504.2
012300 77  COMP-SGN2  PICTURE S9(18) VALUE +3 COMPUTATIONAL.            NC2504.2
012400 77  COMP-SGN3  PICTURE S9(1) VALUE -5 COMPUTATIONAL.             NC2504.2
012500 77  COMP-SGN4  PICTURE S9(18) VALUE -3167598765431 COMPUTATIONAL.NC2504.2
012600 77  START-POINT        PICTURE 9(6) COMPUTATIONAL.               NC2504.2
012700 77  INC-VALUE          PICTURE 9(6) COMPUTATIONAL.               NC2504.2
012800 77  SWITCH-PFM-1 PICTURE 9 VALUE ZERO.                           NC2504.2
012900 77  SWITCH-PFM-2 PICTURE 9 VALUE ZERO.                           NC2504.2
013000 77  PFM-11-COUNTER PICTURE 999 VALUE ZERO.                       NC2504.2
013100 77  PFM-12-COUNTER  PICTURE 999 VALUE 100.                       NC2504.2
013200 77  PFM-12-ANS1  PICTURE 999 VALUE ZERO.                         NC2504.2
013300 77  PFM-12-ANS2  PICTURE 999 VALUE ZERO.                         NC2504.2
013400 01  SUBSCRIPT-6 PICTURE 99999 VALUE ZERO.                        NC2504.2
013500 01  IF-TABLE.                                                    NC2504.2
013600     02 IF-ELEM PICTURE X OCCURS 12 TIMES.                        NC2504.2
013700 01  QUOTE-DATA.                                                  NC2504.2
013800     02 QU-1 PICTURE X(3) VALUE "123".                            NC2504.2
013900     02 QU-2 PICTURE X VALUE QUOTE.                               NC2504.2
014000     02 QU-3 PICTURE X(6) VALUE "ABC456".                         NC2504.2
014100 01  IF-D10.                                                      NC2504.2
014200     02  D1 PICTURE X(2) VALUE "01".                              NC2504.2
014300     02  D2 PICTURE X(2) VALUE "23".                              NC2504.2
014400     02  D3.                                                      NC2504.2
014500     03  D4 PICTURE X(4) VALUE "4567".                            NC2504.2
014600     03 D5 PICTURE X(4) VALUE "8912".                             NC2504.2
014700 01  IF-D12.                                                      NC2504.2
014800     02  D1 PICTURE X(3) VALUE "ABC".                             NC2504.2
014900     02  D2.                                                      NC2504.2
015000     03  D3.                                                      NC2504.2
015100     04  D4      PICTURE XX     VALUE "DE".                       NC2504.2
015200     04  D5 PICTURE X VALUE "F".                                  NC2504.2
015300 01  IF-D20.                                                      NC2504.2
015400     02  FILLER    PICTURE 9(5)   VALUE ZERO.                     NC2504.2
015500     02  D1 PICTURE 9(2) VALUE 12.                                NC2504.2
015600     02  D2 PICTURE 9 VALUE 3.                                    NC2504.2
015700     02  D3 PICTURE 9(2) VALUE 45.                                NC2504.2
015800 01  IF-D21.                                                      NC2504.2
015900     02  D1 PICTURE 9(5) VALUE ZEROS.                             NC2504.2
016000     02  D2 PICTURE 9(5) VALUE 12345.                             NC2504.2
016100 01  IF-D22.                                                      NC2504.2
016200     02  D1 PICTURE A(2) VALUE "AB".                              NC2504.2
016300     02  D2 PICTURE A(4) VALUE "CDEF".                            NC2504.2
016400 01  IF-D35.                                                      NC2504.2
016500     02  AA PICTURE X(2).                                         NC2504.2
016600     88  A1 VALUE "AA".                                           NC2504.2
016700     88  A2 VALUE "AB".                                           NC2504.2
016800     02  BB PICTURE IS X(2).                                      NC2504.2
016900     88  B1 VALUE "CC".                                           NC2504.2
017000     88  B2 VALUE "CD".                                           NC2504.2
017100     02 BB-2 REDEFINES BB.                                        NC2504.2
017200     03  AAA PICTURE X.                                           NC2504.2
017300     88  AA1 VALUE "A".                                           NC2504.2
017400     88  AA2 VALUE "C".                                           NC2504.2
017500     03  BBB PICTURE X.                                           NC2504.2
017600     88  BB1    VALUE "B".                                        NC2504.2
017700     88  BB2 VALUE "D".                                           NC2504.2
017800 01  IF-D36  PICTURE X(120) VALUE IS    "ABCDEFGHIJKLMNOPQRSTUVWXYNC2504.2
017900-    "Z1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890ABCDEFGHIJKLMNC2504.2
018000-    "NOPQRSTUVWXYZ1234567890ABCDEFGHIJKL".                       NC2504.2
018100 01  IF-D40 PICTURE 9(5) VALUE 12345                              NC2504.2
018200              COMPUTATIONAL SYNCHRONIZED RIGHT.                   NC2504.2
018300     88 IF-D40A VALUE ZERO THRU 10000.                            NC2504.2
018400     88 IF-D40B VALUE 10001 THRU 99999.                           NC2504.2
018500     88 IF-D40C VALUE 99999.                                      NC2504.2
018600 01  PERFORM1  PICTURE XXX  VALUE SPACES.                         NC2504.2
018700 01  PERFORM2  PICTURE S999 VALUE 20.                             NC2504.2
018800 01  PERFORM3  PICTURE  9  VALUE  5.                              NC2504.2
018900 01  PERFORM4  PICTURE S99V9.                                     NC2504.2
019000 01  PERFORM5  PICTURE S99V9  VALUE 10.0.                         NC2504.2
019100 01  PERFORM6  PICTURE  99V9.                                     NC2504.2
019200 01  PERFORM7.                                                    NC2504.2
019300     02  PERFORM8  OCCURS 7 TIMES  PICTURE  99V9.                 NC2504.2
019400 01  PERFORM9  PICTURE 9   VALUE 3.                               NC2504.2
019500 01  PERFORM10  PICTURE  S9  VALUE -1.                            NC2504.2
019600 01  PERFORM11  PICTURE  99  VALUE 6.                             NC2504.2
019700 01  PERFORM12.                                                   NC2504.2
019800     02 PERFORM13  OCCURS 4 TIMES.                                NC2504.2
019900         03 PERFORM14  OCCURS 20 TIMES  PICTURE  99V9.            NC2504.2
020000         03 PERFORM15  OCCURS 10 TIMES.                           NC2504.2
020100             04 PERFORM16  OCCURS 5 TIMES  PICTURE 99V9.          NC2504.2
020200 01  PERFORM17          PICTURE 9(6) COMPUTATIONAL.               NC2504.2
020300 01  PERFORM18          PICTURE 9(6) COMPUTATIONAL.               NC2504.2
020400 01  PERFORM-KEY  PICTURE 9.                                      NC2504.2
020500 01  PERFORM-SEVEN-LEVEL-TABLE.                                   NC2504.2
020600   03   PFM71                OCCURS 2.                            NC2504.2
020700     05  PFM72               OCCURS 2.                            NC2504.2
020800       07  PFM73             OCCURS 2.                            NC2504.2
020900         09  PFM74           OCCURS 2.                            NC2504.2
021000           11  PFM75         OCCURS 2.                            NC2504.2
021100             13  PFM76       OCCURS 2.                            NC2504.2
021200               15  PFM77     OCCURS 2.                            NC2504.2
021300                 17  PFM77-1 PIC X.                               NC2504.2
021400 01  S1                      PIC S9(3) COMP.                      NC2504.2
021500 01  S2                      PIC S9(3) COMP.                      NC2504.2
021600 01  S3                      PIC S9(3) COMP.                      NC2504.2
021700 01  S4                      PIC S9(3) COMP.                      NC2504.2
021800 01  S5                      PIC S9(3) COMP.                      NC2504.2
021900 01  S6                      PIC S9(3) COMP.                      NC2504.2
022000 01  S7                      PIC S9(3) COMP.                      NC2504.2
022100 01  PFM-7-TOT               PIC S9(3) COMP.                      NC2504.2
022200 01  PFM-F4-24-TOT           PIC S9(3) COMP.                      NC2504.2
022300 01  PFM-A                   PIC S9(3) COMP.                      NC2504.2
022400 01  PFM-B                   PIC S9(3) COMP.                      NC2504.2
022500 01  FILLER-A.                                                    NC2504.2
022600   03  PFM-F4-25-A           PIC S9(3) COMP OCCURS 10.            NC2504.2
022700 01  FILLER-B.                                                    NC2504.2
022800   03  PFM-F4-25-B           PIC S9(3) COMP OCCURS 10.            NC2504.2
022900 01  FILLER-C.                                                    NC2504.2
023000   03  PFM-F4-25-C           PIC S9(3) COMP OCCURS 10.            NC2504.2
023100 01  RECEIVING-TABLE.                                             NC2504.2
023200     03 TBL-ELEMEN-A.                                             NC2504.2
023300         05 TBL-ELEMEN-B          PICTURE X(18).                  NC2504.2
023400         05 TBL-ELEMEN-C          PICTURE X(18).                  NC2504.2
023500     03  TBL-ELEMEN-D.                                            NC2504.2
023600         05 TBL-ELEMEN-E          PICTURE X OCCURS 36 TIMES.      NC2504.2
023700 01  LITERAL-SPLITTER.                                            NC2504.2
023800     02 PART1                     PICTURE X(20).                  NC2504.2
023900     02 PART2                     PICTURE X(20).                  NC2504.2
024000     02 PART3                     PICTURE X(20).                  NC2504.2
024100     02 PART4                     PICTURE X(20).                  NC2504.2
024200 01  LITERAL-TABLE REDEFINES LITERAL-SPLITTER.                    NC2504.2
024300     02 80PARTS                   PICTURE X      OCCURS 80 TIMES. NC2504.2
024400 01  GRP-FOR-88-LEVELS.                                           NC2504.2
024500     03 WRK-DS-02V00-COND         PICTURE 99.                     NC2504.2
024600         88 COND-1                VALUE IS 01 THRU 05.            NC2504.2
024700         88 COND-2                VALUES ARE 06 THRU 10           NC2504.2
024800                                           16 THRU 20  00.        NC2504.2
024900         88 COND-3                VALUES 11 THRU 15.              NC2504.2
025000 01  GRP-MOVE-CONSTANTS.                                          NC2504.2
025100     03 GRP-GROUP-MOVE-FROM.                                      NC2504.2
025200         04 GRP-ALPHABETIC.                                       NC2504.2
025300             05 ALPHABET-AN-00026 PICTURE A(26)                   NC2504.2
025400                        VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".       NC2504.2
025500         04 GRP-NUMERIC.                                          NC2504.2
025600             05 DIGITS-DV-10V00   PICTURE 9(10) VALUE 0123456789. NC2504.2
025700         05 DIGITS-DU-06V04-S REDEFINES DIGITS-DV-10V00           NC2504.2
025800                                  PICTURE 9(6)V9999.              NC2504.2
025900         04 GRP-ALPHANUMERIC.                                     NC2504.2
026000             05 ALPHANUMERIC-XN-00049 PICTURE X(50)               NC2504.2
026100     VALUE  "ABCDEFGHIJKLMNOPQRSTUVWXYZ+-><=$,;.()/* 0123456789". NC2504.2
026200             05 FILLER                PICTURE X  VALUE QUOTE.     NC2504.2
026300 01  GRP-FOR-2N058.                                               NC2504.2
026400     02 SUB-GRP-FOR-2N058-A.                                      NC2504.2
026500         03 ELEM-FOR-2N058-A PICTURE 999  VALUE ZEROES.           NC2504.2
026600         03 ELEM-FOR-2N058-B PICTURE XXX  VALUE ZEROS.            NC2504.2
026700         03 ELEM-FOR-2N058-C PICTURE XXX  VALUE SPACES.           NC2504.2
026800         03 ELEM-FOR-2N058-D PICTURE X(6) VALUE ALL "ABC".        NC2504.2
026900         03 ELEM-FOR-2N058-E PICTURE XXX  VALUE ALL "Z".          NC2504.2
027000         03 ELEM-FOR-2N058-F PICTURE XXX  VALUE ALL SPACES.       NC2504.2
027100         03 ELEM-FOR-2N058-G PICTURE XXX  VALUE ALL ZEROES.       NC2504.2
027200         03 ELEM-FOR-2N058-H PICTURE 999  VALUE ALL ZEROS.        NC2504.2
027300         03 ELEM-FOR-2N058-I PICTURE XXX  VALUE QUOTES.           NC2504.2
027400         03 ELEM-FOR-2N058-J PICTURE XXX  VALUE ALL QUOTES.       NC2504.2
027500         03 ELEM-FOR-2N058-K PICTURE XXX  VALUE ALL HIGH-VALUES.  NC2504.2
027600         03 ELEM-FOR-2N058-L PICTURE XXX  VALUE ALL LOW-VALUES.   NC2504.2
027700         03 ELEM-FOR-2N058-M PICTURE XXX  VALUE HIGH-VALUES.      NC2504.2
027800         03 ELEM-FOR-2N058-N PICTURE XXX  VALUE LOW-VALUES.       NC2504.2
027900     02 SUB-GRP-FOR-2N058-B.                                      NC2504.2
028000         03 SUB-SUB-BA.                                           NC2504.2
028100             04 ELEM-FOR-2N058-A  PICTURE 999.                    NC2504.2
028200             04 ELEM-FOR-2N058-B  PICTURE XXX.                    NC2504.2
028300             04 ELEM-FOR-2N058-C  PICTURE XXX.                    NC2504.2
028400             04 ELEM-FOR-2N058-D  PICTURE X(6).                   NC2504.2
028500         03 SUB-SUB-BB.                                           NC2504.2
028600             04 ELEM-FOR-2N058-E  PICTURE XXX.                    NC2504.2
028700             04 ELEM-FOR-2N058-F  PICTURE XXX.                    NC2504.2
028800             04 ELEM-FOR-2N058-G  PICTURE XXX.                    NC2504.2
028900             04 ELEM-FOR-2N058-H  PICTURE 999.                    NC2504.2
029000         03 SUB-SUB-BC.                                           NC2504.2
029100             04 ELEM-FOR-2N058-I  PICTURE XXX.                    NC2504.2
029200             04 ELEM-FOR-2N058-J  PICTURE XXX.                    NC2504.2
029300             04 ELEM-FOR-2N058-K  PICTURE XXX.                    NC2504.2
029400             04 ELEM-FOR-2N058-L  PICTURE XXX.                    NC2504.2
029500             04 ELEM-FOR-2N058-M  PICTURE XXX.                    NC2504.2
029600             04 ELEM-FOR-2N058-N  PICTURE XXX.                    NC2504.2
029700 01  CHARACTER-BREAKDOWN-S.                                       NC2504.2
029800     02   FIRST-20S PICTURE X(20).                                NC2504.2
029900     02  SECOND-20S PICTURE X(20).                                NC2504.2
030000     02   THIRD-20S PICTURE X(20).                                NC2504.2
030100     02  FOURTH-20S PICTURE X(20).                                NC2504.2
030200     02   FIFTH-20S PICTURE X(20).                                NC2504.2
030300     02   SIXTH-20S PICTURE X(20).                                NC2504.2
030400     02 SEVENTH-20S PICTURE X(20).                                NC2504.2
030500     02  EIGHTH-20S PICTURE X(20).                                NC2504.2
030600     02   NINTH-20S PICTURE X(20).                                NC2504.2
030700     02   TENTH-20S PICTURE X(20).                                NC2504.2
030800 01  CHARACTER-BREAKDOWN-R.                                       NC2504.2
030900     02   FIRST-20R PICTURE X(20).                                NC2504.2
031000     02  SECOND-20R PICTURE X(20).                                NC2504.2
031100     02   THIRD-20R PICTURE X(20).                                NC2504.2
031200     02  FOURTH-20R PICTURE X(20).                                NC2504.2
031300     02   FIFTH-20R PICTURE X(20).                                NC2504.2
031400     02   SIXTH-20R PICTURE X(20).                                NC2504.2
031500     02 SEVENTH-20R PICTURE X(20).                                NC2504.2
031600     02  EIGHTH-20R PICTURE X(20).                                NC2504.2
031700     02   NINTH-20R PICTURE X(20).                                NC2504.2
031800     02   TENTH-20R PICTURE X(20).                                NC2504.2
031900 01  TABLE-80.                                                    NC2504.2
032000     02  ELMT OCCURS 3 TIMES PIC 9.                               NC2504.2
032100     88  A80  VALUES ARE ZERO THRU 7.                             NC2504.2
032200     88  B80  VALUE 8.                                            NC2504.2
032300     88  C80  VALUES ARE 7, 8 THROUGH 9.                          NC2504.2
032400                                                                  NC2504.2
032500 01  TABLE-86.                                                    NC2504.2
032600     88  A86  VALUE "ABC".                                        NC2504.2
032700     88  B86  VALUE "ABCABC".                                     NC2504.2
032800     88  C86  VALUE "   ABC".                                     NC2504.2
032900     02  DATANAME-86  PIC XXX  VALUE "ABC".                       NC2504.2
033000     02  DNAME-86.                                                NC2504.2
033100         03  FILLER  PIC X  VALUE "A".                            NC2504.2
033200         03  FILLER  PIC X  VALUE "B".                            NC2504.2
033300         03  FILLER  PIC X   VALUE "C".                           NC2504.2
033400 01  FIGCON-DATA.                                                 NC2504.2
033500     02 SPACE-X         PICTURE X(10) VALUE "          ".         NC2504.2
033600     02 QUOTE-X         PICTURE X(5)  VALUE QUOTE.                NC2504.2
033700     02 LOW-VAL         PICTURE X(5)  VALUE LOW-VALUE.            NC2504.2
033800     02 ABC PICTURE XXX VALUE "ABC".                              NC2504.2
033900     02 ONE23           PICTURE 9999  VALUE 123.                  NC2504.2
034000     02 ZERO-C          PICTURE 9(10) VALUE 0 COMPUTATIONAL.      NC2504.2
034100     02 ZERO-D          PICTURE 9     VALUE ZERO USAGE DISPLAY.   NC2504.2
034200 01  TEST-RESULTS.                                                NC2504.2
034300     02 FILLER                   PIC X      VALUE SPACE.          NC2504.2
034400     02 FEATURE                  PIC X(20)  VALUE SPACE.          NC2504.2
034500     02 FILLER                   PIC X      VALUE SPACE.          NC2504.2
034600     02 P-OR-F                   PIC X(5)   VALUE SPACE.          NC2504.2
034700     02 FILLER                   PIC X      VALUE SPACE.          NC2504.2
034800     02  PAR-NAME.                                                NC2504.2
034900       03 FILLER                 PIC X(19)  VALUE SPACE.          NC2504.2
035000       03  PARDOT-X              PIC X      VALUE SPACE.          NC2504.2
035100       03 DOTVALUE               PIC 99     VALUE ZERO.           NC2504.2
035200     02 FILLER                   PIC X(8)   VALUE SPACE.          NC2504.2
035300     02 RE-MARK                  PIC X(61).                       NC2504.2
035400 01  TEST-COMPUTED.                                               NC2504.2
035500     02 FILLER                   PIC X(30)  VALUE SPACE.          NC2504.2
035600     02 FILLER                   PIC X(17)  VALUE                 NC2504.2
035700            "       COMPUTED=".                                   NC2504.2
035800     02 COMPUTED-X.                                               NC2504.2
035900     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          NC2504.2
036000     03 COMPUTED-N               REDEFINES COMPUTED-A             NC2504.2
036100                                 PIC -9(9).9(9).                  NC2504.2
036200     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         NC2504.2
036300     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     NC2504.2
036400     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     NC2504.2
036500     03       CM-18V0 REDEFINES COMPUTED-A.                       NC2504.2
036600         04 COMPUTED-18V0                    PIC -9(18).          NC2504.2
036700         04 FILLER                           PIC X.               NC2504.2
036800     03 FILLER PIC X(50) VALUE SPACE.                             NC2504.2
036900 01  TEST-CORRECT.                                                NC2504.2
037000     02 FILLER PIC X(30) VALUE SPACE.                             NC2504.2
037100     02 FILLER PIC X(17) VALUE "       CORRECT =".                NC2504.2
037200     02 CORRECT-X.                                                NC2504.2
037300     03 CORRECT-A                  PIC X(20) VALUE SPACE.         NC2504.2
037400     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      NC2504.2
037500     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         NC2504.2
037600     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     NC2504.2
037700     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     NC2504.2
037800     03      CR-18V0 REDEFINES CORRECT-A.                         NC2504.2
037900         04 CORRECT-18V0                     PIC -9(18).          NC2504.2
038000         04 FILLER                           PIC X.               NC2504.2
038100     03 FILLER PIC X(2) VALUE SPACE.                              NC2504.2
038200     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     NC2504.2
038300 01  CCVS-C-1.                                                    NC2504.2
038400     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PANC2504.2
038500-    "SS  PARAGRAPH-NAME                                          NC2504.2
038600-    "       REMARKS".                                            NC2504.2
038700     02 FILLER                     PIC X(20)    VALUE SPACE.      NC2504.2
038800 01  CCVS-C-2.                                                    NC2504.2
038900     02 FILLER                     PIC X        VALUE SPACE.      NC2504.2
039000     02 FILLER                     PIC X(6)     VALUE "TESTED".   NC2504.2
039100     02 FILLER                     PIC X(15)    VALUE SPACE.      NC2504.2
039200     02 FILLER                     PIC X(4)     VALUE "FAIL".     NC2504.2
039300     02 FILLER                     PIC X(94)    VALUE SPACE.      NC2504.2
039400 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       NC2504.2
039500 01  REC-CT                        PIC 99       VALUE ZERO.       NC2504.2
039600 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       NC2504.2
039700 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       NC2504.2
039800 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       NC2504.2
039900 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       NC2504.2
040000 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       NC2504.2
040100 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       NC2504.2
040200 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      NC2504.2
040300 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       NC2504.2
040400 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     NC2504.2
040500 01  CCVS-H-1.                                                    NC2504.2
040600     02  FILLER                    PIC X(39)    VALUE SPACES.     NC2504.2
040700     02  FILLER                    PIC X(42)    VALUE             NC2504.2
040800     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 NC2504.2
040900     02  FILLER                    PIC X(39)    VALUE SPACES.     NC2504.2
041000 01  CCVS-H-2A.                                                   NC2504.2
041100   02  FILLER                        PIC X(40)  VALUE SPACE.      NC2504.2
041200   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  NC2504.2
041300   02  FILLER                        PIC XXXX   VALUE             NC2504.2
041400     "4.2 ".                                                      NC2504.2
041500   02  FILLER                        PIC X(28)  VALUE             NC2504.2
041600            " COPY - NOT FOR DISTRIBUTION".                       NC2504.2
041700   02  FILLER                        PIC X(41)  VALUE SPACE.      NC2504.2
041800                                                                  NC2504.2
041900 01  CCVS-H-2B.                                                   NC2504.2
042000   02  FILLER                        PIC X(15)  VALUE             NC2504.2
042100            "TEST RESULT OF ".                                    NC2504.2
042200   02  TEST-ID                       PIC X(9).                    NC2504.2
042300   02  FILLER                        PIC X(4)   VALUE             NC2504.2
042400            " IN ".                                               NC2504.2
042500   02  FILLER                        PIC X(12)  VALUE             NC2504.2
042600     " HIGH       ".                                              NC2504.2
042700   02  FILLER                        PIC X(22)  VALUE             NC2504.2
042800            " LEVEL VALIDATION FOR ".                             NC2504.2
042900   02  FILLER                        PIC X(58)  VALUE             NC2504.2
043000     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2504.2
043100 01  CCVS-H-3.                                                    NC2504.2
043200     02  FILLER                      PIC X(34)  VALUE             NC2504.2
043300            " FOR OFFICIAL USE ONLY    ".                         NC2504.2
043400     02  FILLER                      PIC X(58)  VALUE             NC2504.2
043500     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC2504.2
043600     02  FILLER                      PIC X(28)  VALUE             NC2504.2
043700            "  COPYRIGHT   1985 ".                                NC2504.2
043800 01  CCVS-E-1.                                                    NC2504.2
043900     02 FILLER                       PIC X(52)  VALUE SPACE.      NC2504.2
044000     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              NC2504.2
044100     02 ID-AGAIN                     PIC X(9).                    NC2504.2
044200     02 FILLER                       PIC X(45)  VALUE SPACES.     NC2504.2
044300 01  CCVS-E-2.                                                    NC2504.2
044400     02  FILLER                      PIC X(31)  VALUE SPACE.      NC2504.2
044500     02  FILLER                      PIC X(21)  VALUE SPACE.      NC2504.2
044600     02 CCVS-E-2-2.                                               NC2504.2
044700         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      NC2504.2
044800         03 FILLER                   PIC X      VALUE SPACE.      NC2504.2
044900         03 ENDER-DESC               PIC X(44)  VALUE             NC2504.2
045000            "ERRORS ENCOUNTERED".                                 NC2504.2
045100 01  CCVS-E-3.                                                    NC2504.2
045200     02  FILLER                      PIC X(22)  VALUE             NC2504.2
045300            " FOR OFFICIAL USE ONLY".                             NC2504.2
045400     02  FILLER                      PIC X(12)  VALUE SPACE.      NC2504.2
045500     02  FILLER                      PIC X(58)  VALUE             NC2504.2
045600     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2504.2
045700     02  FILLER                      PIC X(13)  VALUE SPACE.      NC2504.2
045800     02 FILLER                       PIC X(15)  VALUE             NC2504.2
045900             " COPYRIGHT 1985".                                   NC2504.2
046000 01  CCVS-E-4.                                                    NC2504.2
046100     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      NC2504.2
046200     02 FILLER                       PIC X(4)   VALUE " OF ".     NC2504.2
046300     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      NC2504.2
046400     02 FILLER                       PIC X(40)  VALUE             NC2504.2
046500      "  TESTS WERE EXECUTED SUCCESSFULLY".                       NC2504.2
046600 01  XXINFO.                                                      NC2504.2
046700     02 FILLER                       PIC X(19)  VALUE             NC2504.2
046800            "*** INFORMATION ***".                                NC2504.2
046900     02 INFO-TEXT.                                                NC2504.2
047000       04 FILLER                     PIC X(8)   VALUE SPACE.      NC2504.2
047100       04 XXCOMPUTED                 PIC X(20).                   NC2504.2
047200       04 FILLER                     PIC X(5)   VALUE SPACE.      NC2504.2
047300       04 XXCORRECT                  PIC X(20).                   NC2504.2
047400     02 INF-ANSI-REFERENCE           PIC X(48).                   NC2504.2
047500 01  HYPHEN-LINE.                                                 NC2504.2
047600     02 FILLER  PIC IS X VALUE IS SPACE.                          NC2504.2
047700     02 FILLER  PIC IS X(65)    VALUE IS "************************NC2504.2
047800-    "*****************************************".                 NC2504.2
047900     02 FILLER  PIC IS X(54)    VALUE IS "************************NC2504.2
048000-    "******************************".                            NC2504.2
048100 01  CCVS-PGM-ID                     PIC X(9)   VALUE             NC2504.2
048200     "NC250A".                                                    NC2504.2
048300 PROCEDURE DIVISION.                                              NC2504.2
048400 CCVS1 SECTION.                                                   NC2504.2
048500 OPEN-FILES.                                                      NC2504.2
048600     OPEN     OUTPUT PRINT-FILE.                                  NC2504.2
048700     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   NC2504.2
048800     MOVE    SPACE TO TEST-RESULTS.                               NC2504.2
048900     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             NC2504.2
049000     GO TO CCVS1-EXIT.                                            NC2504.2
049100 CLOSE-FILES.                                                     NC2504.2
049200     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   NC2504.2
049300 TERMINATE-CCVS.                                                  NC2504.2
049400     STOP     RUN.                                                NC2504.2
049500 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         NC2504.2
049600 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           NC2504.2
049700 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          NC2504.2
049800 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      NC2504.2
049900     MOVE "****TEST DELETED****" TO RE-MARK.                      NC2504.2
050000 PRINT-DETAIL.                                                    NC2504.2
050100     IF REC-CT NOT EQUAL TO ZERO                                  NC2504.2
050200             MOVE "." TO PARDOT-X                                 NC2504.2
050300             MOVE REC-CT TO DOTVALUE.                             NC2504.2
050400     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      NC2504.2
050500     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               NC2504.2
050600        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 NC2504.2
050700          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 NC2504.2
050800     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              NC2504.2
050900     MOVE SPACE TO CORRECT-X.                                     NC2504.2
051000     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         NC2504.2
051100     MOVE     SPACE TO RE-MARK.                                   NC2504.2
051200 HEAD-ROUTINE.                                                    NC2504.2
051300     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC2504.2
051400     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC2504.2
051500     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC2504.2
051600     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC2504.2
051700 COLUMN-NAMES-ROUTINE.                                            NC2504.2
051800     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2504.2
051900     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2504.2
052000     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        NC2504.2
052100 END-ROUTINE.                                                     NC2504.2
052200     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.NC2504.2
052300 END-RTN-EXIT.                                                    NC2504.2
052400     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2504.2
052500 END-ROUTINE-1.                                                   NC2504.2
052600      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      NC2504.2
052700      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               NC2504.2
052800      ADD PASS-COUNTER TO ERROR-HOLD.                             NC2504.2
052900*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   NC2504.2
053000      MOVE PASS-COUNTER TO CCVS-E-4-1.                            NC2504.2
053100      MOVE ERROR-HOLD TO CCVS-E-4-2.                              NC2504.2
053200      MOVE CCVS-E-4 TO CCVS-E-2-2.                                NC2504.2
053300      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           NC2504.2
053400  END-ROUTINE-12.                                                 NC2504.2
053500      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        NC2504.2
053600     IF       ERROR-COUNTER IS EQUAL TO ZERO                      NC2504.2
053700         MOVE "NO " TO ERROR-TOTAL                                NC2504.2
053800         ELSE                                                     NC2504.2
053900         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       NC2504.2
054000     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           NC2504.2
054100     PERFORM WRITE-LINE.                                          NC2504.2
054200 END-ROUTINE-13.                                                  NC2504.2
054300     IF DELETE-COUNTER IS EQUAL TO ZERO                           NC2504.2
054400         MOVE "NO " TO ERROR-TOTAL  ELSE                          NC2504.2
054500         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      NC2504.2
054600     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   NC2504.2
054700     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2504.2
054800      IF   INSPECT-COUNTER EQUAL TO ZERO                          NC2504.2
054900          MOVE "NO " TO ERROR-TOTAL                               NC2504.2
055000      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   NC2504.2
055100      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            NC2504.2
055200      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          NC2504.2
055300     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2504.2
055400 WRITE-LINE.                                                      NC2504.2
055500     ADD 1 TO RECORD-COUNT.                                       NC2504.2
055600Y    IF RECORD-COUNT GREATER 50                                   NC2504.2
055700Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          NC2504.2
055800Y        MOVE SPACE TO DUMMY-RECORD                               NC2504.2
055900Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  NC2504.2
056000Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             NC2504.2
056100Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     NC2504.2
056200Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          NC2504.2
056300Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          NC2504.2
056400Y        MOVE ZERO TO RECORD-COUNT.                               NC2504.2
056500     PERFORM WRT-LN.                                              NC2504.2
056600 WRT-LN.                                                          NC2504.2
056700     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               NC2504.2
056800     MOVE SPACE TO DUMMY-RECORD.                                  NC2504.2
056900 BLANK-LINE-PRINT.                                                NC2504.2
057000     PERFORM WRT-LN.                                              NC2504.2
057100 FAIL-ROUTINE.                                                    NC2504.2
057200     IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. NC2504.2
057300     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.NC2504.2
057400     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC2504.2
057500     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   NC2504.2
057600     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2504.2
057700     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC2504.2
057800     GO TO  FAIL-ROUTINE-EX.                                      NC2504.2
057900 FAIL-ROUTINE-WRITE.                                              NC2504.2
058000     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         NC2504.2
058100     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 NC2504.2
058200     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. NC2504.2
058300     MOVE   SPACES TO COR-ANSI-REFERENCE.                         NC2504.2
058400 FAIL-ROUTINE-EX. EXIT.                                           NC2504.2
058500 BAIL-OUT.                                                        NC2504.2
058600     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   NC2504.2
058700     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           NC2504.2
058800 BAIL-OUT-WRITE.                                                  NC2504.2
058900     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  NC2504.2
059000     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC2504.2
059100     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2504.2
059200     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC2504.2
059300 BAIL-OUT-EX. EXIT.                                               NC2504.2
059400 CCVS1-EXIT.                                                      NC2504.2
059500     EXIT.                                                        NC2504.2
059600 SECT-NC201A-001 SECTION.                                         NC2504.2
059700*                                                                 NC2504.2
059800 IF--INIT-A.                                                      NC2504.2
059900     MOVE   "VI-89 6.15" TO ANSI-REFERENCE.                       NC2504.2
060000     PERFORM END-ROUTINE.                                         NC2504.2
060100     MOVE SPACE TO TEST-RESULTS.                                  NC2504.2
060200     MOVE "THE FOLLOWING TESTS        " TO RE-MARK.               NC2504.2
060300     PERFORM PRINT-DETAIL.                                        NC2504.2
060400     MOVE "COMPARE FIGURATIVE         " TO RE-MARK.               NC2504.2
060500     PERFORM PRINT-DETAIL.                                        NC2504.2
060600     MOVE "CONSTANTS, SIGN OF DATA,   " TO RE-MARK.               NC2504.2
060700     PERFORM PRINT-DETAIL.                                        NC2504.2
060800     MOVE "AND CONDITION-NAMES        " TO RE-MARK.               NC2504.2
060900     PERFORM PRINT-DETAIL.                                        NC2504.2
061000     MOVE "IN VARYING COMBINATIONS.   " TO RE-MARK.               NC2504.2
061100     PERFORM PRINT-DETAIL.                                        NC2504.2
061200     MOVE "COMPARE--           " TO FEATURE.                      NC2504.2
061300     PERFORM PRINT-DETAIL.                                        NC2504.2
061400     MOVE "  FIG. CONSTANTS    " TO FEATURE.                      NC2504.2
061500 IF--TEST-1.                                                      NC2504.2
061600     IF ZEROES IS EQUAL TO IF-D3 PERFORM PASS ELSE PERFORM FAIL.  NC2504.2
061700*        NOTE FIGURATIVE ZEROES VS ALPHANUMERIC FIELD.            NC2504.2
061800     GO TO IF--WRITE-1.                                           NC2504.2
061900 IF--DELETE-1.                                                    NC2504.2
062000     PERFORM DE-LETE.                                             NC2504.2
062100 IF--WRITE-1.                                                     NC2504.2
062200     MOVE "IF--TEST-1 " TO PAR-NAME.                              NC2504.2
062300     PERFORM PRINT-DETAIL.                                        NC2504.2
062400 IF--TEST-2.                                                      NC2504.2
062500     IF SPACES EQUAL TO IF-D4 PERFORM PASS ELSE PERFORM FAIL.     NC2504.2
062600*        NOTE FIGURATIVE SPACES VS ALPHANUMERIC FIELD.            NC2504.2
062700     GO TO IF--WRITE-2.                                           NC2504.2
062800 IF--DELETE-2.                                                    NC2504.2
062900     PERFORM DE-LETE.                                             NC2504.2
063000 IF--WRITE-2.                                                     NC2504.2
063100     MOVE "IF--TEST-2 " TO PAR-NAME.                              NC2504.2
063200     PERFORM PRINT-DETAIL.                                        NC2504.2
063300 IF--TEST-3.                                                      NC2504.2
063400     IF QUOTES EQUAL TO IF-D5 PERFORM PASS ELSE PERFORM FAIL.     NC2504.2
063500*        NOTE FIGURATIVE QUOTES VS ALPHANUMERIC FIELD.            NC2504.2
063600     GO TO IF--WRITE-3.                                           NC2504.2
063700 IF--DELETE-3.                                                    NC2504.2
063800     PERFORM DE-LETE.                                             NC2504.2
063900 IF--WRITE-3.                                                     NC2504.2
064000     MOVE "IF--TEST-3 " TO PAR-NAME.                              NC2504.2
064100     PERFORM PRINT-DETAIL.                                        NC2504.2
064200 IF--TEST-4.                                                      NC2504.2
064300     IF IF-D6 EQUAL TO ALL "BA" PERFORM PASS ELSE PERFORM FAIL.   NC2504.2
064400*        NOTE ALL ANY LITERAL VS ALPHANUMERIC FIELD.              NC2504.2
064500     GO TO IF--WRITE-4.                                           NC2504.2
064600 IF--DELETE-4.                                                    NC2504.2
064700     PERFORM DE-LETE.                                             NC2504.2
064800 IF--WRITE-4.                                                     NC2504.2
064900     MOVE "IF--TEST-4 " TO PAR-NAME.                              NC2504.2
065000     PERFORM PRINT-DETAIL.                                        NC2504.2
065100 IF--TEST-5.                                                      NC2504.2
065200     IF IF-D4 GREATER THAN SPACES PERFORM FAIL ELSE               NC2504.2
065300     PERFORM PASS.                                                NC2504.2
065400*        NOTE FIG-SPACES VS ALPHANUMERIC FIELD.                   NC2504.2
065500     GO TO IF--WRITE-5.                                           NC2504.2
065600 IF--DELETE-5.                                                    NC2504.2
065700     PERFORM DE-LETE.                                             NC2504.2
065800 IF--WRITE-5.                                                     NC2504.2
065900     MOVE "IF--TEST-5 " TO PAR-NAME.                              NC2504.2
066000     PERFORM PRINT-DETAIL.                                        NC2504.2
066100 IF--TEST-6.                                                      NC2504.2
066200     IF QUOTES GREATER THAN IF-D5 PERFORM FAIL ELSE PERFORM PASS. NC2504.2
066300*        NOTE FIG-QUOTES VS ALPHANUMERIC FIELD.                   NC2504.2
066400     GO TO IF--WRITE-6.                                           NC2504.2
066500 IF--DELETE-6.                                                    NC2504.2
066600     PERFORM DE-LETE.                                             NC2504.2
066700 IF--WRITE-6.                                                     NC2504.2
066800     MOVE "IF--TEST-6 " TO PAR-NAME.                              NC2504.2
066900     PERFORM PRINT-DETAIL.                                        NC2504.2
067000 IF--TEST-7.                                                      NC2504.2
067100     IF ALL "BA" GREATER THAN IF-D6 PERFORM FAIL                  NC2504.2
067200     ELSE PERFORM PASS.                                           NC2504.2
067300*    NOTE ALL ANY LITERAL VS ALPHA FIELD.                         NC2504.2
067400     GO TO IF--WRITE-7.                                           NC2504.2
067500 IF--DELETE-7.                                                    NC2504.2
067600     PERFORM DE-LETE.                                             NC2504.2
067700 IF--WRITE-7.                                                     NC2504.2
067800     MOVE "IF--TEST-7 " TO PAR-NAME.                              NC2504.2
067900     PERFORM PRINT-DETAIL.                                        NC2504.2
068000 IF--INIT-B.                                                      NC2504.2
068100     MOVE "  UNEQUAL LENGTHS   " TO FEATURE.                      NC2504.2
068200 IF--TEST-8.                                                      NC2504.2
068300     IF IF-D22 GREATER THAN IF-D19 PERFORM FAIL ELSE PERFORM PASS.NC2504.2
068400*        NOTE ALPHANUMERIC GROUP VS ALPHANUMERIC FIELD.           NC2504.2
068500*    NOTE UNEQUAL LENGTHS.                                        NC2504.2
068600     GO TO IF--WRITE-8.                                           NC2504.2
068700 IF--DELETE-8.                                                    NC2504.2
068800     PERFORM DE-LETE.                                             NC2504.2
068900 IF--WRITE-8.                                                     NC2504.2
069000     MOVE "IF--TEST-8 " TO PAR-NAME.                              NC2504.2
069100     PERFORM PRINT-DETAIL.                                        NC2504.2
069200 IF--INIT-C.                                                      NC2504.2
069300     MOVE "  POSITIVE          " TO FEATURE.                      NC2504.2
069400 IF--TEST-9.                                                      NC2504.2
069500     IF IF-D1 IS NOT POSITIVE PERFORM PASS ELSE PERFORM FAIL.     NC2504.2
069600*        NOTE POSITIVE TEST ON ZERO VALUE.                        NC2504.2
069700     GO TO IF--WRITE-9.                                           NC2504.2
069800 IF--DELETE-9.                                                    NC2504.2
069900     PERFORM DE-LETE.                                             NC2504.2
070000 IF--WRITE-9.                                                     NC2504.2
070100     MOVE "IF--TEST-9 " TO PAR-NAME.                              NC2504.2
070200     PERFORM PRINT-DETAIL.                                        NC2504.2
070300 IF--TEST-10.                                                     NC2504.2
070400     IF IF-D8 POSITIVE PERFORM PASS ELSE      PERFORM FAIL.       NC2504.2
070500*        NOTE POSITIVE TEST ON UNSIGNED VALUE.                    NC2504.2
070600     GO TO IF--WRITE-10.                                          NC2504.2
070700 IF--DELETE-10.                                                   NC2504.2
070800     PERFORM DE-LETE.                                             NC2504.2
070900 IF--WRITE-10.                                                    NC2504.2
071000     MOVE "IF--TEST-10" TO PAR-NAME.                              NC2504.2
071100     PERFORM PRINT-DETAIL.                                        NC2504.2
071200 IF--TEST-11.                                                     NC2504.2
071300     IF IF-D16 POSITIVE PERFORM PASS ELSE PERFORM FAIL.           NC2504.2
071400*        NOTE POSITIVE TEST ON SCALED VALUE.                      NC2504.2
071500     GO TO IF--WRITE-11.                                          NC2504.2
071600 IF--DELETE-11.                                                   NC2504.2
071700     PERFORM DE-LETE.                                             NC2504.2
071800 IF--WRITE-11.                                                    NC2504.2
071900     MOVE "IF--TEST-11" TO PAR-NAME.                              NC2504.2
072000     PERFORM PRINT-DETAIL.                                        NC2504.2
072100 IF--TEST-12.                                                     NC2504.2
072200     IF IF-D27 POSITIVE PERFORM PASS ELSE PERFORM FAIL.           NC2504.2
072300*        NOTE POSITIVE TEST ON COMPUTATIONAL FIELD.               NC2504.2
072400     GO TO IF--WRITE-12.                                          NC2504.2
072500 IF--DELETE-12.                                                   NC2504.2
072600     PERFORM DE-LETE.                                             NC2504.2
072700 IF--WRITE-12.                                                    NC2504.2
072800     MOVE "IF--TEST-12" TO PAR-NAME.                              NC2504.2
072900     PERFORM PRINT-DETAIL.                                        NC2504.2
073000 IF--TEST-13.                                                     NC2504.2
073100     IF IF-D28 POSITIVE PERFORM PASS ELSE PERFORM FAIL.           NC2504.2
073200*        NOTE POSITIVE TEST ON NUMERIC DISPLAY IFELD.             NC2504.2
073300     GO TO IF--WRITE-13.                                          NC2504.2
073400 IF--DELETE-13.                                                   NC2504.2
073500     PERFORM DE-LETE.                                             NC2504.2
073600 IF--WRITE-13.                                                    NC2504.2
073700     MOVE "IF--TEST-13" TO PAR-NAME.                              NC2504.2
073800     PERFORM PRINT-DETAIL.                                        NC2504.2
073900 IF--TEST-14.                                                     NC2504.2
074000     IF IF-D31 IS POSITIVE PERFORM FAIL ELSE PERFORM PASS.        NC2504.2
074100*        NOTE POSITIVE TEST ON NEGATIVE FIELD.                    NC2504.2
074200     GO TO IF--WRITE-14.                                          NC2504.2
074300 IF--DELETE-14.                                                   NC2504.2
074400     PERFORM DE-LETE.                                             NC2504.2
074500 IF--WRITE-14.                                                    NC2504.2
074600     MOVE "IF--TEST-14" TO PAR-NAME.                              NC2504.2
074700     PERFORM PRINT-DETAIL.                                        NC2504.2
074800 IF--TEST-15.                                                     NC2504.2
074900     IF IF-D31 IS NOT POSITIVE PERFORM PASS ELSE PERFORM FAIL.    NC2504.2
075000*        NOTE NOT POSITIVE TEST ON NEGATIVE VALUE.                NC2504.2
075100     GO TO IF--WRITE-15.                                          NC2504.2
075200 IF--DELETE-15.                                                   NC2504.2
075300     PERFORM DE-LETE.                                             NC2504.2
075400 IF--WRITE-15.                                                    NC2504.2
075500     MOVE "IF--TEST-15" TO PAR-NAME.                              NC2504.2
075600     PERFORM PRINT-DETAIL.                                        NC2504.2
075700 IF--TEST-16.                                                     NC2504.2
075800     IF IF-D28 IS NOT POSITIVE PERFORM FAIL ELSE PERFORM PASS.    NC2504.2
075900*        NOTE NOT POSITIVE TEST ON UNSIGNED FIELD.                NC2504.2
076000     GO TO IF--WRITE-16.                                          NC2504.2
076100 IF--DELETE-16.                                                   NC2504.2
076200     PERFORM DE-LETE.                                             NC2504.2
076300 IF--WRITE-16.                                                    NC2504.2
076400     MOVE "IF--TEST-16" TO PAR-NAME.                              NC2504.2
076500     PERFORM PRINT-DETAIL.                                        NC2504.2
076600 IF--INIT-D.                                                      NC2504.2
076700     MOVE "  NEGATIVE          " TO FEATURE.                      NC2504.2
076800 IF--TEST-17.                                                     NC2504.2
076900     IF IF-D31 IS NEGATIVE PERFORM PASS ELSE PERFORM FAIL.        NC2504.2
077000*        NOTE NEGATIVE TEST ON NEGATIVE VALUE.                    NC2504.2
077100     GO TO IF--WRITE-17.                                          NC2504.2
077200 IF--DELETE-17.                                                   NC2504.2
077300     PERFORM DE-LETE.                                             NC2504.2
077400 IF--WRITE-17.                                                    NC2504.2
077500     MOVE "IF--TEST-17" TO PAR-NAME.                              NC2504.2
077600     PERFORM PRINT-DETAIL.                                        NC2504.2
077700 IF--TEST-18.                                                     NC2504.2
077800     IF IF-D31 IS NOT NEGATIVE PERFORM FAIL ELSE PERFORM PASS.    NC2504.2
077900*        NOTE NOT NEGATIVE TEST ON NEGATIVE VALUE.                NC2504.2
078000     GO TO IF--WRITE-18.                                          NC2504.2
078100 IF--DELETE-18.                                                   NC2504.2
078200     PERFORM DE-LETE.                                             NC2504.2
078300 IF--WRITE-18.                                                    NC2504.2
078400     MOVE "IF--TEST-18" TO PAR-NAME.                              NC2504.2
078500     PERFORM PRINT-DETAIL.                                        NC2504.2
078600 IF--TEST-19.                                                     NC2504.2
078700     IF IF-D16 NOT NEGATIVE PERFORM PASS ELSE PERFORM FAIL.       NC2504.2
078800*        NOTE NOT NEGATIVE TEST ON UNSIGNED FIELD.                NC2504.2
078900     GO TO IF--WRITE-19.                                          NC2504.2
079000 IF--DELETE-19.                                                   NC2504.2
079100     PERFORM DE-LETE.                                             NC2504.2
079200 IF--WRITE-19.                                                    NC2504.2
079300     MOVE "IF--TEST-19" TO PAR-NAME.                              NC2504.2
079400     PERFORM PRINT-DETAIL.                                        NC2504.2
079500 IF--INIT-E.                                                      NC2504.2
079600     MOVE "  ZERO              " TO FEATURE.                      NC2504.2
079700 IF--TEST-20.                                                     NC2504.2
079800     IF IF-D1 IS ZERO PERFORM PASS ELSE PERFORM FAIL.             NC2504.2
079900*        NOTE ZERO TEST ON ZERO VALUE.                            NC2504.2
080000     GO TO IF--WRITE-20.                                          NC2504.2
080100 IF--DELETE-20.                                                   NC2504.2
080200     PERFORM DE-LETE.                                             NC2504.2
080300 IF--WRITE-20.                                                    NC2504.2
080400     MOVE "IF--TEST-20" TO PAR-NAME.                              NC2504.2
080500     PERFORM PRINT-DETAIL.                                        NC2504.2
080600 IF--TEST-21.                                                     NC2504.2
080700     IF IF-D10 NOT EQUAL TO ZERO                                  NC2504.2
080800         PERFORM PASS  ELSE                                       NC2504.2
080900     MOVE IF-D10 TO COMPUTED-A                                    NC2504.2
081000     MOVE ZERO TO CORRECT-N                                       NC2504.2
081100     PERFORM FAIL.                                                NC2504.2
081200*    NOTE NOT EQUAL TO ZERO TEST ON NON-ZERO VALUE.               NC2504.2
081300     GO TO IF--WRITE-21.                                          NC2504.2
081400 IF--DELETE-21.                                                   NC2504.2
081500     PERFORM DE-LETE.                                             NC2504.2
081600 IF--WRITE-21.                                                    NC2504.2
081700     MOVE "IF--TEST-21" TO PAR-NAME.                              NC2504.2
081800     PERFORM PRINT-DETAIL.                                        NC2504.2
081900 IF--INIT-F.                                                      NC2504.2
082000     MOVE "  CONDITION-NAMES   " TO FEATURE.                      NC2504.2
082100 IF--TEST-22.                                                     NC2504.2
082200     MOVE 1 TO IF-D32. IF A OF IF-D32 PERFORM PASS                NC2504.2
082300     ELSE PERFORM FAIL.                                           NC2504.2
082400*        NOTE TEST OF SIGNED NUMERIC FIELD FOR SINGLE VALUE.      NC2504.2
082500     GO TO IF--WRITE-22.                                          NC2504.2
082600 IF--DELETE-22.                                                   NC2504.2
082700     PERFORM DE-LETE.                                             NC2504.2
082800 IF--WRITE-22.                                                    NC2504.2
082900     MOVE "IF--TEST-22" TO PAR-NAME.                              NC2504.2
083000     PERFORM PRINT-DETAIL.                                        NC2504.2
083100 IF--TEST-23.                                                     NC2504.2
083200     MOVE 3 TO IF-D32. IF B OF IF-D32 PERFORM PASS                NC2504.2
083300     ELSE PERFORM FAIL.                                           NC2504.2
083400*        NOTE TEST OF SIGNED NUMERIC FIELD FOR MULTIPLE VALUES.   NC2504.2
083500     GO TO IF--WRITE-23.                                          NC2504.2
083600 IF--DELETE-23.                                                   NC2504.2
083700     PERFORM DE-LETE.                                             NC2504.2
083800 IF--WRITE-23.                                                    NC2504.2
083900     MOVE "IF--TEST-23" TO PAR-NAME.                              NC2504.2
084000     PERFORM PRINT-DETAIL.                                        NC2504.2
084100 IF--TEST-24.                                                     NC2504.2
084200             MOVE ZERO   TO IF-D32. IF C OF IF-D32 PERFORM PASS   NC2504.2
084300     ELSE PERFORM FAIL.                                           NC2504.2
084400*        NOTE TEST OF SIGNED NUMERIC FIELD FOR FIG-ZERO.          NC2504.2
084500     GO TO IF--WRITE-24.                                          NC2504.2
084600 IF--DELETE-24.                                                   NC2504.2
084700     PERFORM DE-LETE.                                             NC2504.2
084800 IF--WRITE-24.                                                    NC2504.2
084900     MOVE "IF--TEST-24" TO PAR-NAME.                              NC2504.2
085000     PERFORM PRINT-DETAIL.                                        NC2504.2
085100 IF--TEST-25.                                                     NC2504.2
085200     MOVE +12.34 TO IF-D32.                                       NC2504.2
085300     IF D OF IF-D32 PERFORM PASS ELSE PERFORM FAIL.               NC2504.2
085400*    NOTE SIGNED CONDITION-NAME.                                  NC2504.2
085500     GO TO IF--WRITE-25.                                          NC2504.2
085600 IF--DELETE-25.                                                   NC2504.2
085700     PERFORM DE-LETE.                                             NC2504.2
085800 IF--WRITE-25.                                                    NC2504.2
085900     MOVE "IF--TEST-25" TO PAR-NAME.                              NC2504.2
086000     PERFORM PRINT-DETAIL.                                        NC2504.2
086100 IF--TEST-26.                                                     NC2504.2
086200     MOVE QUOTE  TO IF-D33. IF B OF IF-D33 AND NOT B OF IF-D32    NC2504.2
086300     PERFORM PASS ELSE PERFORM FAIL.                              NC2504.2
086400*        NOTE TEST OF ALPHANUMERIC FIELD FOR FIG-QUOTES.          NC2504.2
086500     GO TO IF--WRITE-26.                                          NC2504.2
086600 IF--DELETE-26.                                                   NC2504.2
086700     PERFORM DE-LETE.                                             NC2504.2
086800 IF--WRITE-26.                                                    NC2504.2
086900     MOVE "IF--TEST-26" TO PAR-NAME.                              NC2504.2
087000     PERFORM PRINT-DETAIL.                                        NC2504.2
087100 IF--TEST-27.                                                     NC2504.2
087200     MOVE SPACE TO IF-D33. IF C OF IF-D33 PERFORM PASS            NC2504.2
087300     ELSE PERFORM FAIL.                                           NC2504.2
087400*        NOTE TEST OF ALPHANUMERIC FIELD FOR FIG-SPACES.          NC2504.2
087500     GO TO IF--WRITE-27.                                          NC2504.2
087600 IF--DELETE-27.                                                   NC2504.2
087700     PERFORM DE-LETE.                                             NC2504.2
087800 IF--WRITE-27.                                                    NC2504.2
087900     MOVE "IF--TEST-27" TO PAR-NAME.                              NC2504.2
088000     PERFORM PRINT-DETAIL.                                        NC2504.2
088100 IF--TEST-28.                                                     NC2504.2
088200     MOVE "BACB" TO IF-D33. IF D OF IF-D33 PERFORM PASS           NC2504.2
088300     ELSE PERFORM FAIL.                                           NC2504.2
088400*        NOTE TEST OF ALPHANUMERIC FIELD FOR ALL ANY LITERAL.     NC2504.2
088500     GO TO IF--WRITE-28.                                          NC2504.2
088600 IF--DELETE-28.                                                   NC2504.2
088700     PERFORM DE-LETE.                                             NC2504.2
088800 IF--WRITE-28.                                                    NC2504.2
088900     MOVE "IF--TEST-28" TO PAR-NAME.                              NC2504.2
089000     PERFORM PRINT-DETAIL.                                        NC2504.2
089100 IF--TEST-29.                                                     NC2504.2
089200     IF NOT B OF IF-D34 PERFORM PASS ELSE PERFORM FAIL.           NC2504.2
089300     GO TO IF--WRITE-29.                                          NC2504.2
089400 IF--DELETE-29.                                                   NC2504.2
089500     PERFORM DE-LETE.                                             NC2504.2
089600 IF--WRITE-29.                                                    NC2504.2
089700     MOVE "IF--TEST-29" TO PAR-NAME.                              NC2504.2
089800     PERFORM PRINT-DETAIL.                                        NC2504.2
089900 IF--TEST-30.                                                     NC2504.2
090000     MOVE "ABCD" TO IF-D35.                                       NC2504.2
090100     IF A2 AND B2 PERFORM PASS ELSE PERFORM FAIL.                 NC2504.2
090200     GO TO IF--WRITE-30.                                          NC2504.2
090300 IF--DELETE-30.                                                   NC2504.2
090400     PERFORM DE-LETE.                                             NC2504.2
090500 IF--WRITE-30.                                                    NC2504.2
090600     MOVE "IF--TEST-30" TO PAR-NAME.                              NC2504.2
090700     PERFORM PRINT-DETAIL.                                        NC2504.2
090800 IF--TEST-31.                                                     NC2504.2
090900     MOVE .21 TO IF-D32.                                          NC2504.2
091000     IF E PERFORM PASS ELSE PERFORM FAIL.                         NC2504.2
091100*    NOTE TESTS VALUE SERIES.                                     NC2504.2
091200     GO TO IF--WRITE-31.                                          NC2504.2
091300 IF--DELETE-31.                                                   NC2504.2
091400     PERFORM DE-LETE.                                             NC2504.2
091500 IF--WRITE-31.                                                    NC2504.2
091600     MOVE "IF--TEST-31" TO PAR-NAME.                              NC2504.2
091700     PERFORM PRINT-DETAIL.                                        NC2504.2
091800 IF--TEST-32.                                                     NC2504.2
091900     MOVE 1279.99 TO IF-D32.                                      NC2504.2
092000     IF F PERFORM PASS ELSE PERFORM FAIL.                         NC2504.2
092100*    NOTE TESTS VALUE RANGE SERIES.                               NC2504.2
092200     GO TO IF--WRITE-32.                                          NC2504.2
092300 IF--DELETE-32.                                                   NC2504.2
092400     PERFORM DE-LETE.                                             NC2504.2
092500 IF--WRITE-32.                                                    NC2504.2
092600     MOVE "IF--TEST-32" TO PAR-NAME.                              NC2504.2
092700     PERFORM PRINT-DETAIL.                                        NC2504.2
092800 IF--TEST-33.                                                     NC2504.2
092900     MOVE -4321.88 TO IF-D32.                                     NC2504.2
093000     IF G PERFORM PASS ELSE PERFORM FAIL.                         NC2504.2
093100*    NOTE TESTS VALUE SERIES RANGE SERIES.                        NC2504.2
093200     GO TO IF--WRITE-33.                                          NC2504.2
093300 IF--DELETE-33.                                                   NC2504.2
093400     PERFORM DE-LETE.                                             NC2504.2
093500 IF--WRITE-33.                                                    NC2504.2
093600     MOVE "IF--TEST-33" TO PAR-NAME.                              NC2504.2
093700     PERFORM PRINT-DETAIL.                                        NC2504.2
093800 IF--INIT-G.                                                      NC2504.2
093900     PERFORM END-ROUTINE.                                         NC2504.2
094000     MOVE SPACES TO FEATURE.                                      NC2504.2
094100     MOVE "THE FOLLOWING TESTS USE ARITHMETIC-EXPRESSIONS"        NC2504.2
094200     TO RE-MARK.                                                  NC2504.2
094300     PERFORM PRINT-DETAIL.                                        NC2504.2
094400     MOVE "IN RELATION OR SIGN CONDITIONS."                       NC2504.2
094500     TO RE-MARK.                                                  NC2504.2
094600     PERFORM PRINT-DETAIL.                                        NC2504.2
094700     MOVE "  EQUAL             " TO FEATURE.                      NC2504.2
094800 IF--TEST-34.                                                     NC2504.2
094900     IF  1 + (TWO * 3) EQUAL TO (TWO * 3) + 1                     NC2504.2
095000               PERFORM PASS                                       NC2504.2
095100     ELSE                                                         NC2504.2
095200               PERFORM FAIL.                                      NC2504.2
095300     GO TO IF--WRITE-34.                                          NC2504.2
095400 IF--DELETE-34.                                                   NC2504.2
095500     PERFORM DE-LETE.                                             NC2504.2
095600 IF--WRITE-34.                                                    NC2504.2
095700     MOVE "IF--TEST-34" TO PAR-NAME.                              NC2504.2
095800     PERFORM PRINT-DETAIL.                                        NC2504.2
095900 IF--TEST-35.                                                     NC2504.2
096000     IF 9 + TWO + 2 * 3 EQUAL TO 2 * 3 + TWO + 9                  NC2504.2
096100         PERFORM PASS                                             NC2504.2
096200     ELSE                                                         NC2504.2
096300         PERFORM FAIL.                                            NC2504.2
096400     GO TO IF--WRITE-35.                                          NC2504.2
096500 IF--DELETE-35.                                                   NC2504.2
096600     PERFORM DE-LETE.                                             NC2504.2
096700 IF--WRITE-35.                                                    NC2504.2
096800     MOVE "IF--TEST-35" TO PAR-NAME.                              NC2504.2
096900     PERFORM PRINT-DETAIL.                                        NC2504.2
097000 IF--TEST-36.                                                     NC2504.2
097100     IF  NINE ** 2 EQUAL TO 9 ** 2                                NC2504.2
097200              PERFORM PASS                                        NC2504.2
097300     ELSE                                                         NC2504.2
097400              PERFORM FAIL.                                       NC2504.2
097500     GO TO IF--WRITE-36.                                          NC2504.2
097600 IF--DELETE-36.                                                   NC2504.2
097700     PERFORM DE-LETE.                                             NC2504.2
097800 IF--WRITE-36.                                                    NC2504.2
097900     MOVE "IF--TEST-36" TO PAR-NAME.                              NC2504.2
098000     PERFORM PRINT-DETAIL.                                        NC2504.2
098100 IF--TEST-37.                                                     NC2504.2
098200     IF 100 + (TWENTY + 3.4) + .05 EQUAL TO                       NC2504.2
098300        .05 + (100 + TWENTY) + 3.4                                NC2504.2
098400            PERFORM PASS                                          NC2504.2
098500     ELSE                                                         NC2504.2
098600            PERFORM FAIL.                                         NC2504.2
098700     GO TO IF--WRITE-37.                                          NC2504.2
098800 IF--DELETE-37.                                                   NC2504.2
098900     PERFORM DE-LETE.                                             NC2504.2
099000 IF--WRITE-37.                                                    NC2504.2
099100     MOVE "IF--TEST-37" TO PAR-NAME.                              NC2504.2
099200     PERFORM PRINT-DETAIL.                                        NC2504.2
099300 IF--INIT-H.                                                      NC2504.2
099400     MOVE "  GREATER           " TO FEATURE.                      NC2504.2
099500 IF--TEST-38.                                                     NC2504.2
099600     IF NINE * 8 IS GREATER THAN  9 * 7 + 8 PERFORM PASS          NC2504.2
099700     ELSE PERFORM FAIL.                                           NC2504.2
099800     GO TO IF--WRITE-38.                                          NC2504.2
099900 IF--DELETE-38.                                                   NC2504.2
100000     PERFORM DE-LETE.                                             NC2504.2
100100 IF--WRITE-38.                                                    NC2504.2
100200     MOVE "IF--TEST-38" TO PAR-NAME.                              NC2504.2
100300     PERFORM PRINT-DETAIL.                                        NC2504.2
100400 IF--TEST-39.                                                     NC2504.2
100500     IF  10 ** 2 + 25 GREATER THAN IF-D14 PERFORM PASS ELSE       NC2504.2
100600     PERFORM FAIL.                                                NC2504.2
100700     GO TO IF--WRITE-39.                                          NC2504.2
100800 IF--DELETE-39.                                                   NC2504.2
100900     PERFORM DE-LETE.                                             NC2504.2
101000 IF--WRITE-39.                                                    NC2504.2
101100     MOVE "IF--TEST-39" TO PAR-NAME.                              NC2504.2
101200     PERFORM PRINT-DETAIL.                                        NC2504.2
101300 IF--TEST-40.                                                     NC2504.2
101400     IF 1000 GREATER THAN TEN ** 3 - 1 PERFORM PASS ELSE PERFORM  NC2504.2
101500     FAIL.                                                        NC2504.2
101600     GO TO IF--WRITE-40.                                          NC2504.2
101700 IF--DELETE-40.                                                   NC2504.2
101800     PERFORM DE-LETE.                                             NC2504.2
101900 IF--WRITE-40.                                                    NC2504.2
102000     MOVE "IF--TEST-40" TO PAR-NAME.                              NC2504.2
102100     PERFORM PRINT-DETAIL.                                        NC2504.2
102200 IF--INIT-I.                                                      NC2504.2
102300     MOVE "  LESS              " TO FEATURE.                      NC2504.2
102400 IF--TEST-41.                                                     NC2504.2
102500     IF 1000 LESS THAN 10 ** THREE + 1 PERFORM PASS ELSE          NC2504.2
102600     PERFORM FAIL.                                                NC2504.2
102700     GO TO IF--WRITE-41.                                          NC2504.2
102800 IF--DELETE-41.                                                   NC2504.2
102900     PERFORM DE-LETE.                                             NC2504.2
103000 IF--WRITE-41.                                                    NC2504.2
103100     MOVE "IF--TEST-41" TO PAR-NAME.                              NC2504.2
103200     PERFORM PRINT-DETAIL.                                        NC2504.2
103300 IF--TEST-42.                                                     NC2504.2
103400     IF 10 ** 2 + 20 LESS THAN IF-D14 PERFORM PASS ELSE           NC2504.2
103500     PERFORM FAIL.                                                NC2504.2
103600     GO TO IF--WRITE-42.                                          NC2504.2
103700 IF--DELETE-42.                                                   NC2504.2
103800     PERFORM DE-LETE.                                             NC2504.2
103900 IF--WRITE-42.                                                    NC2504.2
104000     MOVE "IF--TEST-42" TO PAR-NAME.                              NC2504.2
104100     PERFORM PRINT-DETAIL.                                        NC2504.2
104200 IF--TEST-43.                                                     NC2504.2
104300     IF 9 * 8 LESS THAN 9 * 7 + TEN PERFORM PASS ELSE PERFORM     NC2504.2
104400     FAIL.                                                        NC2504.2
104500     GO TO IF--WRITE-43.                                          NC2504.2
104600 IF--DELETE-43.                                                   NC2504.2
104700     PERFORM DE-LETE.                                             NC2504.2
104800 IF--WRITE-43.                                                    NC2504.2
104900     MOVE "IF--TEST-43" TO PAR-NAME.                              NC2504.2
105000     PERFORM PRINT-DETAIL.                                        NC2504.2
105100 IF--TEST-44-45.                                                  NC2504.2
105200     MOVE SPACES TO TEST-RESULTS.                                 NC2504.2
105300     MOVE "NOT USED" TO RE-MARK.                                  NC2504.2
105400     MOVE "IF--TEST-44" TO PAR-NAME.                              NC2504.2
105500     PERFORM PRINT-DETAIL.                                        NC2504.2
105600     MOVE "NOT USED" TO RE-MARK.                                  NC2504.2
105700     MOVE "IF--TEST-45" TO PAR-NAME.                              NC2504.2
105800     PERFORM PRINT-DETAIL.                                        NC2504.2
105900 IF--INIT-J.                                                      NC2504.2
106000     MOVE "  NOT EQUAL          " TO FEATURE.                     NC2504.2
106100     PERFORM PRINT-DETAIL.                                        NC2504.2
106200 IF--TEST-46.                                                     NC2504.2
106300     IF NINE * 9 - 7 * SEVEN NOT EQUAL - (SEVEN * 7) + 9 * NINE   NC2504.2
106400                PERFORM FAIL                                      NC2504.2
106500     ELSE                                                         NC2504.2
106600                PERFORM PASS.                                     NC2504.2
106700                                                                  NC2504.2
106800     GO TO IF--WRITE-46.                                          NC2504.2
106900 IF--DELETE-46.                                                   NC2504.2
107000     PERFORM DE-LETE.                                             NC2504.2
107100 IF--WRITE-46.                                                    NC2504.2
107200     MOVE "IF--TEST-46" TO PAR-NAME.                              NC2504.2
107300     PERFORM PRINT-DETAIL.                                        NC2504.2
107400 IF--TEST-47.                                                     NC2504.2
107500     IF IF-D14 - IF-D7 NOT EQUAL - IF-D7 + IF-D14                 NC2504.2
107600     PERFORM FAIL ELSE PERFORM PASS.                              NC2504.2
107700     GO TO IF--WRITE-47.                                          NC2504.2
107800 IF--DELETE-47.                                                   NC2504.2
107900     PERFORM DE-LETE.                                             NC2504.2
108000 IF--WRITE-47.                                                    NC2504.2
108100     MOVE "IF--TEST-47" TO PAR-NAME.                              NC2504.2
108200     PERFORM PRINT-DETAIL.                                        NC2504.2
108300 IF--INIT-K.                                                      NC2504.2
108400     MOVE "  NOT GREATER       " TO FEATURE.                      NC2504.2
108500 IF--TEST-48.                                                     NC2504.2
108600     IF NINE * 8 IS NOT GREATER THAN 9 * SEVEN + 8 THEN           NC2504.2
108700                   PERFORM FAIL                                   NC2504.2
108800     ELSE                                                         NC2504.2
108900                   PERFORM PASS.                                  NC2504.2
109000     GO TO IF--WRITE-48.                                          NC2504.2
109100 IF--DELETE-48.                                                   NC2504.2
109200     PERFORM DE-LETE.                                             NC2504.2
109300 IF--WRITE-48.                                                    NC2504.2
109400     MOVE "IF--TEST-48" TO PAR-NAME.                              NC2504.2
109500     PERFORM PRINT-DETAIL.                                        NC2504.2
109600 IF--TEST-49.                                                     NC2504.2
109700     IF 10 ** 2 + 25 NOT GREATER THAN IF-D14 PERFORM FAIL ELSE    NC2504.2
109800     PERFORM PASS.                                                NC2504.2
109900     GO TO IF--WRITE-49.                                          NC2504.2
110000 IF--DELETE-49.                                                   NC2504.2
110100     PERFORM DE-LETE.                                             NC2504.2
110200 IF--WRITE-49.                                                    NC2504.2
110300     MOVE "IF--TEST-49" TO PAR-NAME.                              NC2504.2
110400     PERFORM PRINT-DETAIL.                                        NC2504.2
110500 IF--TEST-50.                                                     NC2504.2
110600     IF 1000 NOT GREATER THAN 10 ** THREE - 1 PERFORM FAIL ELSE   NC2504.2
110700     PERFORM PASS.                                                NC2504.2
110800     GO TO IF--WRITE-50.                                          NC2504.2
110900 IF--DELETE-50.                                                   NC2504.2
111000     PERFORM DE-LETE.                                             NC2504.2
111100 IF--WRITE-50.                                                    NC2504.2
111200     MOVE "IF--TEST-50" TO PAR-NAME.                              NC2504.2
111300     PERFORM PRINT-DETAIL.                                        NC2504.2
111400 IF--INIT-L.                                                      NC2504.2
111500     MOVE "  NOT LESS          " TO FEATURE.                      NC2504.2
111600 IF--TEST-51.                                                     NC2504.2
111700     IF 1000 NOT LESS THAN TEN ** 3 + 1 PERFORM FAIL ELSE         NC2504.2
111800     PERFORM PASS.                                                NC2504.2
111900     GO TO IF--WRITE-51.                                          NC2504.2
112000 IF--DELETE-51.                                                   NC2504.2
112100     PERFORM DE-LETE.                                             NC2504.2
112200 IF--WRITE-51.                                                    NC2504.2
112300     MOVE "IF--TEST-51" TO PAR-NAME.                              NC2504.2
112400     PERFORM PRINT-DETAIL.                                        NC2504.2
112500 IF--TEST-52.                                                     NC2504.2
112600     IF 10 ** 2 + 20 NOT LESS THAN IF-D14 PERFORM FAIL ELSE       NC2504.2
112700     PERFORM PASS.                                                NC2504.2
112800     GO TO IF--WRITE-52.                                          NC2504.2
112900 IF--DELETE-52.                                                   NC2504.2
113000     PERFORM DE-LETE.                                             NC2504.2
113100 IF--WRITE-52.                                                    NC2504.2
113200     MOVE "IF--TEST-52" TO PAR-NAME.                              NC2504.2
113300     PERFORM PRINT-DETAIL.                                        NC2504.2
113400 IF--TEST-53.                                                     NC2504.2
113500     IF NINE * 8 NOT LESS THAN 9 * 7 + TEN PERFORM FAIL ELSE      NC2504.2
113600     PERFORM PASS.                                                NC2504.2
113700     GO TO IF--WRITE-53.                                          NC2504.2
113800 IF--DELETE-53.                                                   NC2504.2
113900     PERFORM DE-LETE.                                             NC2504.2
114000 IF--WRITE-53.                                                    NC2504.2
114100     MOVE "IF--TEST-53" TO PAR-NAME.                              NC2504.2
114200     PERFORM PRINT-DETAIL.                                        NC2504.2
114300 IF--INIT-M.                                                      NC2504.2
114400     MOVE "  POS, NEG, ZERO    " TO FEATURE.                      NC2504.2
114500 IF--TEST-54.                                                     NC2504.2
114600     IF 9 ** TWO + (180 - 90) IS NOT POSITIVE PERFORM FAIL ELSE   NC2504.2
114700     PERFORM PASS.                                                NC2504.2
114800     GO TO IF--WRITE-54.                                          NC2504.2
114900 IF--DELETE-54.                                                   NC2504.2
115000     PERFORM DE-LETE.                                             NC2504.2
115100 IF--WRITE-54.                                                    NC2504.2
115200     MOVE "IF--TEST-54" TO PAR-NAME.                              NC2504.2
115300     PERFORM PRINT-DETAIL.                                        NC2504.2
115400 IF--TEST-55.                                                     NC2504.2
115500     IF NINE ** 2 + (90 - 180) IS POSITIVE PERFORM FAIL ELSE      NC2504.2
115600     PERFORM PASS.                                                NC2504.2
115700     GO TO IF--WRITE-55.                                          NC2504.2
115800 IF--DELETE-55.                                                   NC2504.2
115900     PERFORM DE-LETE.                                             NC2504.2
116000 IF--WRITE-55.                                                    NC2504.2
116100     MOVE "IF--TEST-55" TO PAR-NAME.                              NC2504.2
116200     PERFORM PRINT-DETAIL.                                        NC2504.2
116300 IF--TEST-56.                                                     NC2504.2
116400     IF 8 * EIGHT - 8 * 8 NOT ZERO                                NC2504.2
116500             PERFORM FAIL ELSE PERFORM PASS.                      NC2504.2
116600     GO TO IF--WRITE-56.                                          NC2504.2
116700 IF--DELETE-56.                                                   NC2504.2
116800     PERFORM DE-LETE.                                             NC2504.2
116900 IF--WRITE-56.                                                    NC2504.2
117000     MOVE "IF--TEST-56" TO PAR-NAME.                              NC2504.2
117100     PERFORM PRINT-DETAIL.                                        NC2504.2
117200 IF--TEST-57-58.                                                  NC2504.2
117300     MOVE SPACES TO TEST-RESULTS.                                 NC2504.2
117400     MOVE "NOT USED" TO RE-MARK.                                  NC2504.2
117500     MOVE "IF--TEST-57" TO PAR-NAME.                              NC2504.2
117600     PERFORM PRINT-DETAIL.                                        NC2504.2
117700     MOVE "NOT USED" TO RE-MARK.                                  NC2504.2
117800     MOVE "IF--TEST-58" TO PAR-NAME.                              NC2504.2
117900     PERFORM PRINT-DETAIL.                                        NC2504.2
118000     MOVE "  POS, NEG, ZERO     " TO FEATURE.                     NC2504.2
118100 IF--TEST-59.                                                     NC2504.2
118200     IF 10 ** THREE + 99 - (1500 - 400) IS NEGATIVE PERFORM PASS  NC2504.2
118300     ELSE PERFORM FAIL.                                           NC2504.2
118400     GO TO IF--WRITE-59.                                          NC2504.2
118500 IF--DELETE-59.                                                   NC2504.2
118600     PERFORM DE-LETE.                                             NC2504.2
118700 IF--WRITE-59.                                                    NC2504.2
118800     MOVE "IF--TEST-59" TO PAR-NAME.                              NC2504.2
118900     PERFORM PRINT-DETAIL.                                        NC2504.2
119000 IF--TEST-60.                                                     NC2504.2
119100     IF TEN ** 3 + 99 - (1500 - 400) IS NOT POSITIVE PERFORM PASS NC2504.2
119200     ELSE PERFORM FAIL.                                           NC2504.2
119300     GO TO IF--WRITE-60.                                          NC2504.2
119400 IF--DELETE-60.                                                   NC2504.2
119500     PERFORM DE-LETE.                                             NC2504.2
119600 IF--WRITE-60.                                                    NC2504.2
119700     MOVE "IF--TEST-60" TO PAR-NAME.                              NC2504.2
119800     PERFORM PRINT-DETAIL.                                        NC2504.2
119900 IF--TEST-61.                                                     NC2504.2
120000     IF 8 * EIGHT - 8 * 8 IS ZERO                                 NC2504.2
120100             PERFORM PASS ELSE PERFORM FAIL.                      NC2504.2
120200     GO TO IF--WRITE-61.                                          NC2504.2
120300 IF--DELETE-61.                                                   NC2504.2
120400     PERFORM DE-LETE.                                             NC2504.2
120500 IF--WRITE-61.                                                    NC2504.2
120600     MOVE "IF--TEST-61" TO PAR-NAME.                              NC2504.2
120700     PERFORM PRINT-DETAIL.                                        NC2504.2
120800 IF--TEST-62.                                                     NC2504.2
120900     MOVE SPACES TO TEST-RESULTS.                                 NC2504.2
121000     MOVE "NOT USED" TO RE-MARK.                                  NC2504.2
121100     MOVE "IF--TEST-62" TO PAR-NAME.                              NC2504.2
121200     PERFORM PRINT-DETAIL.                                        NC2504.2
121300     MOVE "POS, NEG, ZERO      " TO FEATURE.                      NC2504.2
121400 IF--TEST-63.                                                     NC2504.2
121500     IF 10 ** THREE + 99 - (1500 - 400) IS NOT NEGATIVE           NC2504.2
121600              PERFORM FAIL ELSE PERFORM PASS.                     NC2504.2
121700     GO TO IF--WRITE-63.                                          NC2504.2
121800 IF--DELETE-63.                                                   NC2504.2
121900     PERFORM DE-LETE.                                             NC2504.2
122000 IF--WRITE-63.                                                    NC2504.2
122100     MOVE "IF--TEST-63" TO PAR-NAME.                              NC2504.2
122200     PERFORM PRINT-DETAIL.                                        NC2504.2
122300 IF--INIT-N.                                                      NC2504.2
122400     MOVE "  SYMBOLS > < =     " TO FEATURE.                      NC2504.2
122500 IF--TEST-64.                                                     NC2504.2
122600     IF TEN * 10 - 10 * 10 = - TEN * 10 + 10 * 10                 NC2504.2
122700               PERFORM PASS                                       NC2504.2
122800     ELSE                                                         NC2504.2
122900               PERFORM FAIL.                                      NC2504.2
123000     GO TO IF--WRITE-64.                                          NC2504.2
123100 IF--DELETE-64.                                                   NC2504.2
123200     PERFORM DE-LETE.                                             NC2504.2
123300 IF--WRITE-64.                                                    NC2504.2
123400     MOVE "IF--TEST-64" TO PAR-NAME.                              NC2504.2
123500     PERFORM PRINT-DETAIL.                                        NC2504.2
123600 IF--TEST-65.                                                     NC2504.2
123700     IF NINE * 8 > 9 * 7 + 8 PERFORM PASS ELSE PERFORM FAIL.      NC2504.2
123800     GO TO IF--WRITE-65.                                          NC2504.2
123900 IF--DELETE-65.                                                   NC2504.2
124000     PERFORM DE-LETE.                                             NC2504.2
124100 IF--WRITE-65.                                                    NC2504.2
124200     MOVE "IF--TEST-65" TO PAR-NAME.                              NC2504.2
124300     PERFORM PRINT-DETAIL.                                        NC2504.2
124400 IF--TEST-66.                                                     NC2504.2
124500     IF 1000 < 10 ** THREE + 1 PERFORM PASS ELSE PERFORM FAIL.    NC2504.2
124600     GO TO IF--WRITE-66.                                          NC2504.2
124700 IF--DELETE-66.                                                   NC2504.2
124800     PERFORM DE-LETE.                                             NC2504.2
124900 IF--WRITE-66.                                                    NC2504.2
125000     MOVE "IF--TEST-66" TO PAR-NAME.                              NC2504.2
125100     PERFORM PRINT-DETAIL.                                        NC2504.2
125200 IF--TEST-67.                                                     NC2504.2
125300     IF 100 + TWENTY + 3.4 + .05 NOT = 100 + TWENTY + 3.4 + 0.6   NC2504.2
125400          PERFORM PASS                                            NC2504.2
125500     ELSE                                                         NC2504.2
125600          PERFORM FAIL.                                           NC2504.2
125700     GO TO IF--WRITE-67.                                          NC2504.2
125800 IF--DELETE-67.                                                   NC2504.2
125900     PERFORM DE-LETE.                                             NC2504.2
126000 IF--WRITE-67.                                                    NC2504.2
126100     MOVE "IF--TEST-67" TO PAR-NAME.                              NC2504.2
126200     PERFORM PRINT-DETAIL.                                        NC2504.2
126300 IF--TEST-68.                                                     NC2504.2
126400     IF NINE * 8 NOT > 9 * 7 + 8 PERFORM FAIL ELSE PERFORM PASS.  NC2504.2
126500     GO TO IF--WRITE-68.                                          NC2504.2
126600 IF--DELETE-68.                                                   NC2504.2
126700     PERFORM DE-LETE.                                             NC2504.2
126800 IF--WRITE-68.                                                    NC2504.2
126900     MOVE "IF--TEST-68" TO PAR-NAME.                              NC2504.2
127000     PERFORM PRINT-DETAIL.                                        NC2504.2
127100 IF--TEST-69.                                                     NC2504.2
127200     IF 1000 NOT < 10 ** THREE + 1 PERFORM FAIL ELSE PERFORM PASS.NC2504.2
127300     GO TO IF--WRITE-69.                                          NC2504.2
127400 IF--DELETE-69.                                                   NC2504.2
127500     PERFORM DE-LETE.                                             NC2504.2
127600 IF--WRITE-69.                                                    NC2504.2
127700     MOVE "IF--TEST-69" TO PAR-NAME.                              NC2504.2
127800     PERFORM PRINT-DETAIL.                                        NC2504.2
127900 IF--TEST-70.                                                     NC2504.2
128000     MOVE SPACES TO TEST-RESULTS.                                 NC2504.2
128100     MOVE "NOT USED" TO RE-MARK.                                  NC2504.2
128200     MOVE "IF--TEST-70" TO PAR-NAME.                              NC2504.2
128300     PERFORM PRINT-DETAIL.                                        NC2504.2
128400 IF--INIT-N1.                                                     NC2504.2
128500     PERFORM END-ROUTINE.                                         NC2504.2
128600     MOVE SPACES TO FEATURE.                                      NC2504.2
128700     MOVE "THE FOLLOWING TESTS COMBINATIONS OF"                   NC2504.2
128800             TO RE-MARK.                                          NC2504.2
128900     PERFORM PRINT-DETAIL.                                        NC2504.2
129000     MOVE "RELATIONAL AND SIZE ERROR CONDITIONS."                 NC2504.2
129100             TO RE-MARK.                                          NC2504.2
129200     PERFORM PRINT-DETAIL.                                        NC2504.2
129300 IF--TEST-71.                                                     NC2504.2
129400     MOVE     "X" TO WRK-XN-00001.                                NC2504.2
129500     MOVE     ZERO TO WRK-DS-01V00.                               NC2504.2
129600     IF       WRK-XN-00001 IS EQUAL TO "X"                        NC2504.2
129700              MOVE "Z" TO WRK-XN-00001                            NC2504.2
129800              ADD 1 TO WRK-DS-01V00 ON SIZE ERROR                 NC2504.2
129900              MOVE "Y" TO WRK-XN-00001                            NC2504.2
130000              ELSE                                                NC2504.2
130100              ADD 2 TO WRK-DS-01V00 ON SIZE ERROR                 NC2504.2
130200              MOVE "W" TO WRK-XN-00001.                           NC2504.2
130300     IF  WRK-XN-00001 EQUAL TO "Z" AND                            NC2504.2
130400              WRK-DS-01V00 EQUAL TO 1                             NC2504.2
130500              PERFORM PASS ELSE PERFORM FAIL.                     NC2504.2
130600*    NOTE     COMBINATION OF RELATIONAL AND SIZE ERROR CONDITIONS.NC2504.2
130700     GO       TO IF--WRITE-71.                                    NC2504.2
130800 IF--DELETE-71.                                                   NC2504.2
130900     PERFORM  DE-LETE.                                            NC2504.2
131000 IF--WRITE-71.                                                    NC2504.2
131100     MOVE     "  INCL SIZE ERROR" TO FEATURE.                     NC2504.2
131200     MOVE     "IF--TEST-71" TO PAR-NAME.                          NC2504.2
131300     PERFORM  PRINT-DETAIL.                                       NC2504.2
131400 IF--INIT-O.                                                      NC2504.2
131500     MOVE     "  UNEQUAL LENGTHS" TO FEATURE.                     NC2504.2
131600 IF--TEST-73.                                                     NC2504.2
131700     MOVE     "X" TO WRK-XN-00001.                                NC2504.2
131800     MOVE     "X    " TO WRK-XN-00005.                            NC2504.2
131900     IF       WRK-XN-00001 IS EQUAL TO WRK-XN-00005               NC2504.2
132000              PERFORM PASS ELSE PERFORM FAIL.                     NC2504.2
132100*    NOTE     EQUAL QUANTITIES IN UNEQUAL LENGTH FIELDS.          NC2504.2
132200     GO       TO IF--WRITE-73.                                    NC2504.2
132300 IF--DELETE-73.                                                   NC2504.2
132400     PERFORM  DE-LETE.                                            NC2504.2
132500 IF--WRITE-73.                                                    NC2504.2
132600     MOVE     "IF--TEST-73" TO PAR-NAME.                          NC2504.2
132700     PERFORM  PRINT-DETAIL.                                       NC2504.2
132800 IF--TEST-74.                                                     NC2504.2
132900     MOVE     "X" TO WRK-XN-00001.                                NC2504.2
133000     MOVE     "Y    " TO WRK-XN-00005.                            NC2504.2
133100     IF       WRK-XN-00001 IS NOT EQUAL TO WRK-XN-00005           NC2504.2
133200              PERFORM PASS ELSE PERFORM FAIL.                     NC2504.2
133300*    NOTE     UNEQUAL QUANTITIES IN UNEQUAL LENGTH FIELDS.        NC2504.2
133400     GO       TO IF--WRITE-74.                                    NC2504.2
133500 IF--DELETE-74.                                                   NC2504.2
133600     PERFORM  DE-LETE.                                            NC2504.2
133700 IF--WRITE-74.                                                    NC2504.2
133800     MOVE     "IF--TEST-74" TO PAR-NAME.                          NC2504.2
133900     PERFORM  PRINT-DETAIL.                                       NC2504.2
134000 IF--TEST-75.                                                     NC2504.2
134100     MOVE     "X" TO WRK-XN-00001.                                NC2504.2
134200     MOVE     "X   X" TO WRK-XN-00005.                            NC2504.2
134300     IF       WRK-XN-00001 IS NOT EQUAL TO WRK-XN-00005           NC2504.2
134400              PERFORM PASS ELSE PERFORM FAIL.                     NC2504.2
134500*    NOTE     UNEQUAL QUANTITIES IN UNEQUAL LENGTH FIELDS.        NC2504.2
134600     GO       TO IF--WRITE-75.                                    NC2504.2
134700 IF--DELETE-75.                                                   NC2504.2
134800     PERFORM  DE-LETE.                                            NC2504.2
134900 IF--WRITE-75.                                                    NC2504.2
135000     MOVE     "IF--TEST-75" TO PAR-NAME.                          NC2504.2
135100     PERFORM PRINT-DETAIL.                                        NC2504.2
135200 IF--INIT-P.                                                      NC2504.2
135300     MOVE     "  UNEQUAL LENGTHS" TO FEATURE.                     NC2504.2
135400 IF--TEST-77.                                                     NC2504.2
135500     IF       IF-D37 NOT EQUAL TO IF-D21                          NC2504.2
135600              PERFORM PASS GO TO IF--WRITE-77.                    NC2504.2
135700*    NOTE     NUMERIC VS GROUP COMPARISON, UNEQUAL LENGTHS.       NC2504.2
135800     GO       TO IF--FAIL-77.                                     NC2504.2
135900 IF--DELETE-77.                                                   NC2504.2
136000     PERFORM DE-LETE.                                             NC2504.2
136100     GO       TO IF--WRITE-11.                                    NC2504.2
136200 IF--FAIL-77.                                                     NC2504.2
136300     PERFORM  FAIL.                                               NC2504.2
136400     MOVE     "IF-D37 SHOULD PAD ON RIGHT" TO RE-MARK.            NC2504.2
136500 IF--WRITE-77.                                                    NC2504.2
136600     MOVE     "IF--TEST-77" TO PAR-NAME.                          NC2504.2
136700     PERFORM  PRINT-DETAIL.                                       NC2504.2
136800 IF--TEST-78.                                                     NC2504.2
136900     IF       IF-D37 EQUAL TO IF-D38                              NC2504.2
137000              PERFORM PASS GO TO IF--WRITE-78.                    NC2504.2
137100*    NOTE     NUMERIC VS ALPHANUMERIC COMPARISON, UNEQUAL LENGTHS.NC2504.2
137200     GO       TO IF--FAIL-78.                                     NC2504.2
137300 IF--DELETE-78.                                                   NC2504.2
137400     PERFORM  DE-LETE.                                            NC2504.2
137500     GO       TO IF--WRITE-78.                                    NC2504.2
137600 IF--FAIL-78.                                                     NC2504.2
137700     PERFORM  FAIL.                                               NC2504.2
137800     MOVE     "IF-D37 SHOULD PAD ON RIGHT" TO RE-MARK.            NC2504.2
137900 IF--WRITE-78.                                                    NC2504.2
138000     MOVE     "IF--TEST-78" TO PAR-NAME.                          NC2504.2
138100     PERFORM  PRINT-DETAIL.                                       NC2504.2
138200 IF--TEST-79.                                                     NC2504.2
138300     MOVE ZERO TO IF-D10.                                         NC2504.2
138400     IF D3 OF IF-D10 EQUAL TO "00000000"                          NC2504.2
138500         PERFORM PASS                                             NC2504.2
138600         GO TO IF-WRITE-79.                                       NC2504.2
138700     MOVE D3 IN IF-D10 TO COMPUTED-A.                             NC2504.2
138800     MOVE "00000000" TO CORRECT-A.                                NC2504.2
138900     PERFORM FAIL.                                                NC2504.2
139000     GO TO IF-WRITE-79.                                           NC2504.2
139100 IF-DELETE-79.                                                    NC2504.2
139200     PERFORM DE-LETE.                                             NC2504.2
139300 IF-WRITE-79.                                                     NC2504.2
139400     MOVE "QUALIFIED GROUP   " TO FEATURE.                        NC2504.2
139500     MOVE "IF--TEST-79 " TO PAR-NAME.                             NC2504.2
139600     PERFORM PRINT-DETAIL.                                        NC2504.2
139700 IF--INIT-80.                                                     NC2504.2
139800     PERFORM END-ROUTINE.                                         NC2504.2
139900     MOVE SPACES TO FEATURE.                                      NC2504.2
140000     MOVE "THESE SPECIAL CONDITION-   " TO RE-MARK.               NC2504.2
140100     PERFORM PRINT-DETAIL.                                        NC2504.2
140200     MOVE "NAME TESTS VERIFY THE    " TO RE-MARK.                 NC2504.2
140300     PERFORM PRINT-DETAIL.                                        NC2504.2
140400     MOVE "ABILITY OF THE COMPILER TO " TO RE-MARK.               NC2504.2
140500     PERFORM PRINT-DETAIL.                                        NC2504.2
140600     MOVE "ACCEPT SUBSCRIPTED 88 LEVEL" TO RE-MARK.               NC2504.2
140700     PERFORM PRINT-DETAIL.                                        NC2504.2
140800*    NOTE *******                  ******                *********NC2504.2
140900*        *****  A NOTE AS THE FIRST STATEMENT IN THIS ******      NC2504.2
141000*               PARAGRAPH WILL BYPASS ALL THE SPECIAL *****       NC2504.2
141100*               CONDITION-NAME TESTS, BUT A NOTE STATEMENT        NC2504.2
141200*               MIGHT NEED TO BE INSERTED IN EACH TEST            NC2504.2
141300*               SO THE SYNTAX WOULD BE IGNORED BY THE COMPILER.   NC2504.2
141400     MOVE "OCCURS WITH 88 LEVEL" TO FEATURE.                      NC2504.2
141500     MOVE 123 TO TABLE-80.                                        NC2504.2
141600     GO TO IF--TEST-80.                                           NC2504.2
141700 IF-DELETE-80.                                                    NC2504.2
141800     PERFORM DE-LETE.                                             NC2504.2
141900     MOVE "IF--TEST-80" TO PAR-NAME.                              NC2504.2
142000     MOVE "TEST-80 THRU 85 DELETED  " TO RE-MARK.                 NC2504.2
142100     PERFORM PRINT-DETAIL.                                        NC2504.2
142200     ADD 5 TO DELETE-COUNTER.                                     NC2504.2
142300     GO TO IF--TEST-86.                                           NC2504.2
142400 IF--TEST-80.                                                     NC2504.2
142500     IF A80 (2)                                                   NC2504.2
142600         PERFORM PASS ELSE                                        NC2504.2
142700     PERFORM FAIL.                                                NC2504.2
142800*    NOTE ELMT(2) SHOULD CONTAIN A 2 WHICH IS CONTAINED IN        NC2504.2
142900*        THE VALUE OF THE A80 88 LEVEL.                           NC2504.2
143000     GO TO IF-WRITE-80.                                           NC2504.2
143100 IF--DELETE-80.                                                   NC2504.2
143200     PERFORM DE-LETE.                                             NC2504.2
143300 IF-WRITE-80.                                                     NC2504.2
143400     MOVE "IF--TEST-80" TO PAR-NAME.                              NC2504.2
143500     PERFORM PRINT-DETAIL.                                        NC2504.2
143600 IF--TEST-81.                                                     NC2504.2
143700     IF C80 (1)                                                   NC2504.2
143800         PERFORM FAIL ELSE                                        NC2504.2
143900     PERFORM PASS.                                                NC2504.2
144000*    NOTE ELMT(1) SHOULD CONTAIN A 1 WHICH IS NOT CONTAINED       NC2504.2
144100*        IN THE VALUE OF THE C80 88 LEVEL.                        NC2504.2
144200     GO TO IF-WRITE-81.                                           NC2504.2
144300 IF-DELETE-81.                                                    NC2504.2
144400     PERFORM DE-LETE.                                             NC2504.2
144500 IF-WRITE-81.                                                     NC2504.2
144600     MOVE "IF--TEST-81" TO PAR-NAME.                              NC2504.2
144700     PERFORM PRINT-DETAIL.                                        NC2504.2
144800 IF--TEST-82.                                                     NC2504.2
144900     IF B80 (3)                                                   NC2504.2
145000         PERFORM FAIL ELSE                                        NC2504.2
145100     PERFORM PASS.                                                NC2504.2
145200*    NOTE ELMT(3) SHOULD CONTAIN A 3 WHICH IS NOT CONTAINED       NC2504.2
145300*        IN THE VALUE OF THE B80 88 LEVEL.                        NC2504.2
145400     GO TO IF-WRITE-82.                                           NC2504.2
145500 IF-DELETE-82.                                                    NC2504.2
145600     PERFORM DE-LETE.                                             NC2504.2
145700 IF-WRITE-82.                                                     NC2504.2
145800     MOVE "IF--TEST-82" TO PAR-NAME.                              NC2504.2
145900     PERFORM PRINT-DETAIL.                                        NC2504.2
146000 IF--TEST-83.                                                     NC2504.2
146100     IF NOT A80 OF TABLE-80 (3)                                   NC2504.2
146200         PERFORM FAIL ELSE                                        NC2504.2
146300     PERFORM PASS.                                                NC2504.2
146400*    NOTE ELMT(3) SHOULD CONTAIN A 3 BUT THE NOT CONDITION        NC2504.2
146500*        SHOULD CAUSE THE TEST TO FAIL EVEN THOUGH THE A80        NC2504.2
146600*        VALUE INCLUDES THE VALUE 3.                              NC2504.2
146700     GO TO IF-WRITE-83.                                           NC2504.2
146800 IF-DELETE-83.                                                    NC2504.2
146900     PERFORM DE-LETE.                                             NC2504.2
147000 IF-WRITE-83.                                                     NC2504.2
147100     MOVE "IF--TEST-83" TO PAR-NAME.                              NC2504.2
147200     PERFORM PRINT-DETAIL.                                        NC2504.2
147300 IF--TEST-84.                                                     NC2504.2
147400     IF NOT B80 (1)                                               NC2504.2
147500         PERFORM PASS ELSE                                        NC2504.2
147600     PERFORM FAIL.                                                NC2504.2
147700*    NOTE ELMT(1) CONTAINS A 1 AND THE VALUE OF B80 IS 8          NC2504.2
147800*        SO, SAYING NOT 8 IS TRUE.                                NC2504.2
147900     GO TO IF-WRITE-84.                                           NC2504.2
148000 IF-DELETE-84.                                                    NC2504.2
148100     PERFORM DE-LETE.                                             NC2504.2
148200 IF-WRITE-84.                                                     NC2504.2
148300     MOVE "IF--TEST-84" TO PAR-NAME.                              NC2504.2
148400     PERFORM PRINT-DETAIL.                                        NC2504.2
148500 IF--TEST-85.                                                     NC2504.2
148600     IF C80 OF TABLE-80 (2)                                       NC2504.2
148700         PERFORM FAIL ELSE                                        NC2504.2
148800     PERFORM PASS.                                                NC2504.2
148900*    NOTE ELMT(2) IS 2 AND THE VALUES OF C80 DO NOT CONTAIN A 2.  NC2504.2
149000     GO TO IF-WRITE-85.                                           NC2504.2
149100 IF-DELETE-85.                                                    NC2504.2
149200     PERFORM DE-LETE.                                             NC2504.2
149300 IF-WRITE-85.                                                     NC2504.2
149400     MOVE "IF--TEST-85" TO PAR-NAME.                              NC2504.2
149500     PERFORM PRINT-DETAIL.                                        NC2504.2
149600 IF--TEST-86.                                                     NC2504.2
149700     IF A86                                                       NC2504.2
149800         PERFORM FAIL ELSE                                        NC2504.2
149900     PERFORM PASS.                                                NC2504.2
150000*    NOTE A86 (ABC   ) SHOULD NOT EQUAL TABLE-86 (ABCABC).        NC2504.2
150100     GO TO IF-WRITE-86.                                           NC2504.2
150200 IF-DELETE-86.                                                    NC2504.2
150300     PERFORM DE-LETE.                                             NC2504.2
150400 IF-WRITE-86.                                                     NC2504.2
150500     MOVE "IF--TEST-86" TO PAR-NAME.                              NC2504.2
150600     PERFORM PRINT-DETAIL.                                        NC2504.2
150700 IF--TEST-87.                                                     NC2504.2
150800     IF NOT B86                                                   NC2504.2
150900         PERFORM FAIL ELSE                                        NC2504.2
151000     PERFORM PASS.                                                NC2504.2
151100*    NOTE B86 (ABCABC) SHOULD EQUAL TABLE-86 (ABCABC) THUS        NC2504.2
151200*        FAILING THE TEST.                                        NC2504.2
151300     GO TO IF-WRITE-87.                                           NC2504.2
151400 IF-DELETE-87.                                                    NC2504.2
151500     PERFORM DE-LETE.                                             NC2504.2
151600 IF-WRITE-87.                                                     NC2504.2
151700     MOVE "IF--TEST-87" TO PAR-NAME.                              NC2504.2
151800     PERFORM PRINT-DETAIL.                                        NC2504.2
151900 IF--TEST-88.                                                     NC2504.2
152000     MOVE SPACES TO DATANAME-86.                                  NC2504.2
152100     IF C86                                                       NC2504.2
152200         PERFORM PASS ELSE                                        NC2504.2
152300     PERFORM FAIL.                                                NC2504.2
152400*    NOTE TABLE-86 (   ABC) SHOULD EQUAL C86 (   ABC).            NC2504.2
152500     GO TO IF-WRITE-88.                                           NC2504.2
152600 IF-DELETE-88.                                                    NC2504.2
152700     PERFORM DE-LETE.                                             NC2504.2
152800 IF-WRITE-88.                                                     NC2504.2
152900     MOVE "IF--TEST-88" TO PAR-NAME.                              NC2504.2
153000     PERFORM PRINT-DETAIL.                                        NC2504.2
153100 IF--INIT-R.                                                      NC2504.2
153200     MOVE     "FIGCON < = > D-NAME" TO FEATURE.                   NC2504.2
153300 IF--TEST-89.                                                     NC2504.2
153400     IF       ZEROS NOT < LOW-VAL                                 NC2504.2
153500              PERFORM PASS ELSE PERFORM FAIL.                     NC2504.2
153600     GO       TO IF--WRITE-89.                                    NC2504.2
153700 IF--DELETE-89.                                                   NC2504.2
153800     PERFORM DE-LETE.                                             NC2504.2
153900 IF--WRITE-89.                                                    NC2504.2
154000     MOVE     "IF--TEST-89 " TO PAR-NAME.                         NC2504.2
154100     PERFORM  PRINT-DETAIL.                                       NC2504.2
154200 IF--TEST-90.                                                     NC2504.2
154300     IF       ZEROS < ONE23                                       NC2504.2
154400              PERFORM PASS ELSE PERFORM FAIL.                     NC2504.2
154500     GO       TO IF--WRITE-90.                                    NC2504.2
154600 IF--DELETE-90.                                                   NC2504.2
154700     PERFORM DE-LETE.                                             NC2504.2
154800 IF--WRITE-90.                                                    NC2504.2
154900     MOVE     "IF--TEST-90 " TO PAR-NAME.                         NC2504.2
155000     PERFORM  PRINT-DETAIL.                                       NC2504.2
155100 IF--TEST-91.                                                     NC2504.2
155200     IF       ZEROS = ZERO-C                                      NC2504.2
155300              PERFORM PASS ELSE PERFORM FAIL.                     NC2504.2
155400     GO       TO IF--WRITE-91.                                    NC2504.2
155500 IF--DELETE-91.                                                   NC2504.2
155600     PERFORM DE-LETE.                                             NC2504.2
155700 IF--WRITE-91.                                                    NC2504.2
155800     MOVE     "IF--TEST-91 " TO PAR-NAME.                         NC2504.2
155900     PERFORM PRINT-DETAIL.                                        NC2504.2
156000 IF--TEST-92.                                                     NC2504.2
156100     IF       ZEROS NOT = ZERO-D                                  NC2504.2
156200              PERFORM FAIL ELSE PERFORM PASS.                     NC2504.2
156300     GO       TO IF--WRITE-92.                                    NC2504.2
156400 IF--DELETE-92.                                                   NC2504.2
156500     PERFORM DE-LETE.                                             NC2504.2
156600 IF--WRITE-92.                                                    NC2504.2
156700     MOVE     "IF--TEST-92 " TO PAR-NAME.                         NC2504.2
156800     PERFORM  PRINT-DETAIL.                                       NC2504.2
156900 IF--TEST-93.                                                     NC2504.2
157000     IF       SPACES = SPACE-X                                    NC2504.2
157100              PERFORM PASS ELSE PERFORM FAIL.                     NC2504.2
157200     GO       TO IF--WRITE-93.                                    NC2504.2
157300 IF--DELETE-93.                                                   NC2504.2
157400     PERFORM DE-LETE.                                             NC2504.2
157500 IF--WRITE-93.                                                    NC2504.2
157600     MOVE     "IF--TEST-93 " TO PAR-NAME.                         NC2504.2
157700     PERFORM  PRINT-DETAIL.                                       NC2504.2
157800 IF--TEST-94.                                                     NC2504.2
157900     IF       SPACES NOT = QUOTE-X                                NC2504.2
158000              PERFORM PASS ELSE PERFORM FAIL.                     NC2504.2
158100     GO       TO IF--WRITE-94.                                    NC2504.2
158200 IF--DELETE-94.                                                   NC2504.2
158300     PERFORM DE-LETE.                                             NC2504.2
158400 IF--WRITE-94.                                                    NC2504.2
158500     MOVE     "IF--TEST-94 " TO PAR-NAME.                         NC2504.2
158600     PERFORM  PRINT-DETAIL.                                       NC2504.2
158700 IF--TEST-95.                                                     NC2504.2
158800     IF       SPACES > ABC OR < ABC                               NC2504.2
158900              PERFORM PASS ELSE PERFORM FAIL.                     NC2504.2
159000     GO       TO IF--WRITE-95.                                    NC2504.2
159100 IF--DELETE-95.                                                   NC2504.2
159200     PERFORM DE-LETE.                                             NC2504.2
159300 IF--WRITE-95.                                                    NC2504.2
159400     MOVE     "IF--TEST-95 " TO PAR-NAME.                         NC2504.2
159500     PERFORM  PRINT-DETAIL.                                       NC2504.2
159600 IF--TEST-96.                                                     NC2504.2
159700     IF       QUOTES NOT > QUOTE-X                                NC2504.2
159800              PERFORM PASS ELSE PERFORM FAIL.                     NC2504.2
159900     GO       TO IF--WRITE-96.                                    NC2504.2
160000 IF--DELETE-96.                                                   NC2504.2
160100     PERFORM DE-LETE.                                             NC2504.2
160200 IF--WRITE-96.                                                    NC2504.2
160300     MOVE     "IF--TEST-96 " TO PAR-NAME.                         NC2504.2
160400     PERFORM  PRINT-DETAIL.                                       NC2504.2
160500 IF--TEST-97.                                                     NC2504.2
160600     IF       QUOTES NOT = ZERO-D                                 NC2504.2
160700              PERFORM PASS ELSE PERFORM FAIL.                     NC2504.2
160800     GO       TO IF--WRITE-97.                                    NC2504.2
160900 IF--DELETE-97.                                                   NC2504.2
161000     PERFORM DE-LETE.                                             NC2504.2
161100 IF--WRITE-97.                                                    NC2504.2
161200     MOVE     "IF--TEST-97 " TO PAR-NAME.                         NC2504.2
161300     PERFORM  PRINT-DETAIL.                                       NC2504.2
161400 IF--TEST-98.                                                     NC2504.2
161500     IF       HIGH-VALUES > LOW-VAL                               NC2504.2
161600              PERFORM PASS ELSE PERFORM FAIL.                     NC2504.2
161700     GO       TO IF--WRITE-98.                                    NC2504.2
161800 IF--DELETE-98.                                                   NC2504.2
161900     PERFORM DE-LETE.                                             NC2504.2
162000 IF--WRITE-98.                                                    NC2504.2
162100     MOVE     "IF--TEST-98 " TO PAR-NAME.                         NC2504.2
162200     PERFORM  PRINT-DETAIL.                                       NC2504.2
162300 IF--TEST-99.                                                     NC2504.2
162400     IF       HIGH-VALUES > ABC                                   NC2504.2
162500              PERFORM PASS ELSE PERFORM FAIL.                     NC2504.2
162600     GO       TO IF--WRITE-99.                                    NC2504.2
162700 IF--DELETE-99.                                                   NC2504.2
162800     PERFORM DE-LETE.                                             NC2504.2
162900 IF--WRITE-99.                                                    NC2504.2
163000     MOVE     "IF--TEST-99 " TO PAR-NAME.                         NC2504.2
163100     PERFORM  PRINT-DETAIL.                                       NC2504.2
163200 IF--TEST-100.                                                    NC2504.2
163300     IF       HIGH-VALUES NOT > ONE23                             NC2504.2
163400              PERFORM FAIL ELSE PERFORM PASS.                     NC2504.2
163500     GO       TO IF--WRITE-100.                                   NC2504.2
163600 IF--DELETE-100.                                                  NC2504.2
163700     PERFORM DE-LETE.                                             NC2504.2
163800 IF--WRITE-100.                                                   NC2504.2
163900     MOVE     "IF--TEST-100" TO PAR-NAME.                         NC2504.2
164000     PERFORM  PRINT-DETAIL.                                       NC2504.2
164100 IF--TEST-101.                                                    NC2504.2
164200     IF       HIGH-VALUES = ZERO-D                                NC2504.2
164300              PERFORM FAIL ELSE PERFORM PASS.                     NC2504.2
164400     GO       TO IF--WRITE-101.                                   NC2504.2
164500 IF--DELETE-101.                                                  NC2504.2
164600     PERFORM DE-LETE.                                             NC2504.2
164700 IF--WRITE-101.                                                   NC2504.2
164800     MOVE     "IF--TEST-101" TO PAR-NAME.                         NC2504.2
164900     PERFORM  PRINT-DETAIL.                                       NC2504.2
165000 IF--TEST-102.                                                    NC2504.2
165100     IF       LOW-VALUES = LOW-VAL                                NC2504.2
165200              PERFORM PASS ELSE PERFORM FAIL.                     NC2504.2
165300     GO       TO IF--WRITE-102.                                   NC2504.2
165400 IF--DELETE-102.                                                  NC2504.2
165500     PERFORM DE-LETE.                                             NC2504.2
165600 IF--WRITE-102.                                                   NC2504.2
165700     MOVE     "IF--TEST-102" TO PAR-NAME.                         NC2504.2
165800     PERFORM  PRINT-DETAIL.                                       NC2504.2
165900 IF--TEST-103.                                                    NC2504.2
166000     IF       LOW-VALUES < ABC                                    NC2504.2
166100              PERFORM PASS ELSE PERFORM FAIL.                     NC2504.2
166200     GO       TO IF--WRITE-103.                                   NC2504.2
166300 IF--DELETE-103.                                                  NC2504.2
166400     PERFORM DE-LETE.                                             NC2504.2
166500 IF--WRITE-103.                                                   NC2504.2
166600     MOVE     "IF--TEST-103" TO PAR-NAME.                         NC2504.2
166700     PERFORM  PRINT-DETAIL.                                       NC2504.2
166800 IF--TEST-104.                                                    NC2504.2
166900     IF       ALL "00" < ONE23                                    NC2504.2
167000              PERFORM PASS ELSE PERFORM FAIL.                     NC2504.2
167100     GO       TO IF--WRITE-104.                                   NC2504.2
167200 IF--DELETE-104.                                                  NC2504.2
167300     PERFORM DE-LETE.                                             NC2504.2
167400 IF--WRITE-104.                                                   NC2504.2
167500     MOVE     "IF--TEST-104" TO PAR-NAME.                         NC2504.2
167600     PERFORM  PRINT-DETAIL.                                       NC2504.2
167700 IF--TEST-105.                                                    NC2504.2
167800     IF       ALL ZEROES = ZERO-D                                 NC2504.2
167900              PERFORM PASS ELSE PERFORM FAIL.                     NC2504.2
168000     GO       TO IF--WRITE-105.                                   NC2504.2
168100 IF--DELETE-105.                                                  NC2504.2
168200     PERFORM DE-LETE.                                             NC2504.2
168300 IF--WRITE-105.                                                   NC2504.2
168400     MOVE     "IF--TEST-105" TO PAR-NAME.                         NC2504.2
168500     PERFORM  PRINT-DETAIL.                                       NC2504.2
168600 IF--TEST-106.                                                    NC2504.2
168700     IF       ALL "00" NOT > ZERO-D                               NC2504.2
168800              PERFORM PASS ELSE PERFORM FAIL.                     NC2504.2
168900     GO       TO IF--WRITE-106.                                   NC2504.2
169000 IF--DELETE-106.                                                  NC2504.2
169100     PERFORM DE-LETE.                                             NC2504.2
169200 IF--WRITE-106.                                                   NC2504.2
169300     MOVE     "IF--TEST-106" TO PAR-NAME.                         NC2504.2
169400     PERFORM  PRINT-DETAIL.                                       NC2504.2
169500 IF--TEST-107.                                                    NC2504.2
169600     IF       ALL "A" = SPACE-X                                   NC2504.2
169700              PERFORM FAIL ELSE PERFORM PASS.                     NC2504.2
169800     GO       TO IF--WRITE-107.                                   NC2504.2
169900 IF--DELETE-107.                                                  NC2504.2
170000     PERFORM DE-LETE.                                             NC2504.2
170100 IF--WRITE-107.                                                   NC2504.2
170200     MOVE     "IF--TEST-107" TO PAR-NAME.                         NC2504.2
170300     PERFORM  PRINT-DETAIL.                                       NC2504.2
170400 IF--TEST-108.                                                    NC2504.2
170500     IF       ALL "A" > ABC                                       NC2504.2
170600              PERFORM FAIL ELSE PERFORM PASS.                     NC2504.2
170700     GO       TO IF--WRITE-108.                                   NC2504.2
170800 IF--DELETE-108.                                                  NC2504.2
170900     PERFORM DE-LETE.                                             NC2504.2
171000 IF--WRITE-108.                                                   NC2504.2
171100     MOVE     "IF--TEST-108" TO PAR-NAME.                         NC2504.2
171200     PERFORM  PRINT-DETAIL.                                       NC2504.2
171300 IF--TEST-109.                                                    NC2504.2
171400     IF       IF-D4 ALPHABETIC                                    NC2504.2
171500              PERFORM PASS ELSE PERFORM FAIL.                     NC2504.2
171600     GO       TO IF--WRITE-109.                                   NC2504.2
171700 IF--DELETE-109.                                                  NC2504.2
171800     PERFORM  DE-LETE.                                            NC2504.2
171900 IF--WRITE-109.                                                   NC2504.2
172000     MOVE     "CLASS --- ALPHABETIC" TO FEATURE.                  NC2504.2
172100     MOVE     "IF--TEST-109" TO PAR-NAME.                         NC2504.2
172200     PERFORM  PRINT-DETAIL.                                       NC2504.2
172300 IF--INIT-S.                                                      NC2504.2
172400     MOVE     "SIGN --- ZERO" TO FEATURE.                         NC2504.2
172500 IF--TEST-110.                                                    NC2504.2
172600     IF      SMALLEST-VALU GREATER THAN SMALL-VALU                NC2504.2
172700         AND IS NOT LESS THAN EVEN-SMALLER OR SMALLER-VALU        NC2504.2
172800             MOVE "CONDITION FALSE" TO CORRECT-A                  NC2504.2
172900             MOVE "CONDITION TRUE " TO COMPUTED-A                 NC2504.2
173000             PERFORM FAIL                                         NC2504.2
173100             GO TO IF--WRITE-110.                                 NC2504.2
173200     PERFORM PASS.                                                NC2504.2
173300     GO TO IF--WRITE-110.                                         NC2504.2
173400 IF--DELETE-110.                                                  NC2504.2
173500     PERFORM  DE-LETE.                                            NC2504.2
173600 IF--WRITE-110.                                                   NC2504.2
173700     MOVE     "IF--TEST-110" TO PAR-NAME.                         NC2504.2
173800     MOVE "ABBREV CONDITIONS" TO FEATURE.                         NC2504.2
173900     PERFORM  PRINT-DETAIL.                                       NC2504.2
174000 IF--TEST-111.                                                    NC2504.2
174100     IF SMALLEST-VALU LESS THAN SMALL-VALU AND                    NC2504.2
174200     (SMALLEST-VALU GREATER THAN EVEN-SMALLER OR SMALLER-VALU)    NC2504.2
174300              PERFORM PASS GO TO IF--WRITE-111.                   NC2504.2
174400     MOVE "CONDITION TRUE" TO CORRECT-A.                          NC2504.2
174500     MOVE "CONDITION FALSE" TO COMPUTED-A.                        NC2504.2
174600     PERFORM  FAIL.                                               NC2504.2
174700     GO       TO IF--WRITE-111.                                   NC2504.2
174800 IF--DELETE-111.                                                  NC2504.2
174900     PERFORM  DE-LETE.                                            NC2504.2
175000 IF--WRITE-111.                                                   NC2504.2
175100     MOVE     "IF--TEST-111" TO PAR-NAME.                         NC2504.2
175200     PERFORM  PRINT-DETAIL.                                       NC2504.2
175300 IF--TEST-112.                                                    NC2504.2
175400     IF       IF-D40B                                             NC2504.2
175500              PERFORM PASS ELSE PERFORM FAIL.                     NC2504.2
175600     GO       TO IF--WRITE-112.                                   NC2504.2
175700 IF--DELETE-112.                                                  NC2504.2
175800     PERFORM  DE-LETE.                                            NC2504.2
175900 IF--WRITE-112.                                                   NC2504.2
176000     MOVE "CONDITION---NAME" TO FEATURE.                          NC2504.2
176100     MOVE     "IF--TEST-112" TO PAR-NAME.                         NC2504.2
176200     PERFORM  PRINT-DETAIL.                                       NC2504.2
176300 IF--INIT-T.                                                      NC2504.2
176400     MOVE "ABBREV---CONDITION" TO FEATURE.                        NC2504.2
176500 IF--TEST-113.                                                    NC2504.2
176600     IF SMALLEST-VALU LESS THAN SMALL-VALU AND (SMALLEST-VALU NOT NC2504.2
176700     GREATER THAN EVEN-SMALLER OR SMALLER-VALU)                   NC2504.2
176800             PERFORM PASS                                         NC2504.2
176900         GO TO IF--WRITE-113.                                     NC2504.2
177000     MOVE "CONDITION TRUE" TO CORRECT-A.                          NC2504.2
177100     MOVE "CONDITION FALSE" TO COMPUTED-A.                        NC2504.2
177200     PERFORM FAIL.                                                NC2504.2
177300     GO TO IF--WRITE-113.                                         NC2504.2
177400 IF--DELETE-113.                                                  NC2504.2
177500     PERFORM  DE-LETE.                                            NC2504.2
177600 IF--WRITE-113.                                                   NC2504.2
177700     MOVE     "IF--TEST-113" TO PAR-NAME.                         NC2504.2
177800     PERFORM  PRINT-DETAIL.                                       NC2504.2
177900 IF--TEST-114.                                                    NC2504.2
178000     IF      SMALLEST-VALU LESS THAN SMALL-VALU                   NC2504.2
178100             AND NOT EVEN-SMALLER OR SMALLER-VALU                 NC2504.2
178200             PERFORM PASS                                         NC2504.2
178300             GO TO IF--WRITE-114                                  NC2504.2
178400             ELSE                                                 NC2504.2
178500             PERFORM FAIL                                         NC2504.2
178600             MOVE "CONDITION FALSE" TO CORRECT-A                  NC2504.2
178700             MOVE "CONDITION TRUE" TO COMPUTED-A                  NC2504.2
178800             GO TO IF--WRITE-114.                                 NC2504.2
178900 IF--DELETE-114.                                                  NC2504.2
179000     PERFORM  DE-LETE.                                            NC2504.2
179100 IF--WRITE-114.                                                   NC2504.2
179200     MOVE     "IF--TEST-114" TO PAR-NAME.                         NC2504.2
179300     PERFORM  PRINT-DETAIL.                                       NC2504.2
179400 IF--TEST-115.                                                    NC2504.2
179500     IF COMP-SGN1 IS POSITIVE                                     NC2504.2
179600         PERFORM PASS                                             NC2504.2
179700         GO TO IF--WRITE-115.                                     NC2504.2
179800     MOVE "POSITIVE EXPECTED" TO CORRECT-A.                       NC2504.2
179900     MOVE COMP-SGN1 TO COMPUTED-14V4.                             NC2504.2
180000     PERFORM FAIL.                                                NC2504.2
180100     GO TO IF--WRITE-115.                                         NC2504.2
180200 IF--DELETE-115.                                                  NC2504.2
180300     PERFORM DE-LETE.                                             NC2504.2
180400 IF--WRITE-115.                                                   NC2504.2
180500     MOVE "POS/NEG SIGN TEST" TO FEATURE.                         NC2504.2
180600     MOVE "IF--TEST-115" TO PAR-NAME.                             NC2504.2
180700     PERFORM PRINT-DETAIL.                                        NC2504.2
180800 IF--TEST-116.                                                    NC2504.2
180900     IF COMP-SGN2 NOT POSITIVE                                    NC2504.2
181000         MOVE COMP-SGN2 TO COMPUTED-14V4                          NC2504.2
181100         MOVE "POSITIVE EXPECTED" TO CORRECT-A                    NC2504.2
181200         PERFORM FAIL                                             NC2504.2
181300         GO TO IF--WRITE-116.                                     NC2504.2
181400     PERFORM PASS.                                                NC2504.2
181500     GO TO IF--WRITE-116.                                         NC2504.2
181600 IF--DELETE-116.                                                  NC2504.2
181700     PERFORM DE-LETE.                                             NC2504.2
181800 IF--WRITE-116.                                                   NC2504.2
181900     MOVE "IF--TEST-116" TO PAR-NAME.                             NC2504.2
182000     PERFORM PRINT-DETAIL.                                        NC2504.2
182100 IF--TEST-117.                                                    NC2504.2
182200     IF COMP-SGN3 NOT NEGATIVE                                    NC2504.2
182300         MOVE COMP-SGN3 TO COMPUTED-14V4                          NC2504.2
182400         MOVE "NEGATIVE EXPECTED" TO CORRECT-A                    NC2504.2
182500         PERFORM FAIL                                             NC2504.2
182600         GO TO IF--WRITE-117.                                     NC2504.2
182700     PERFORM PASS.                                                NC2504.2
182800     GO TO IF--WRITE-117.                                         NC2504.2
182900 IF--DELETE-117.                                                  NC2504.2
183000     PERFORM DE-LETE.                                             NC2504.2
183100 IF--WRITE-117.                                                   NC2504.2
183200     MOVE "IF--TEST-117" TO PAR-NAME.                             NC2504.2
183300     PERFORM PRINT-DETAIL.                                        NC2504.2
183400 IF--TEST-118.                                                    NC2504.2
183500     IF COMP-SGN4 NOT POSITIVE                                    NC2504.2
183600         PERFORM PASS                                             NC2504.2
183700         GO TO IF--WRITE-118.                                     NC2504.2
183800     MOVE COMP-SGN4 TO COMPUTED-14V4.                             NC2504.2
183900     MOVE "NEGATIVE EXPECTED" TO CORRECT-A.                       NC2504.2
184000     PERFORM FAIL.                                                NC2504.2
184100     GO TO IF--WRITE-118.                                         NC2504.2
184200 IF--DELETE-118.                                                  NC2504.2
184300     PERFORM DE-LETE.                                             NC2504.2
184400 IF--WRITE-118.                                                   NC2504.2
184500     MOVE "IF--TEST-118" TO PAR-NAME.                             NC2504.2
184600     PERFORM PRINT-DETAIL.                                        NC2504.2
184700 IF--TEST-119.                                                    NC2504.2
184800     MOVE SPACES TO TEST-RESULTS.                                 NC2504.2
184900     MOVE "NOT USED" TO RE-MARK.                                  NC2504.2
185000     MOVE "IF--TEST-119" TO PAR-NAME.                             NC2504.2
185100     PERFORM PRINT-DETAIL.                                        NC2504.2
185200 IF--TEST-120.                                                    NC2504.2
185300     MOVE     -10 TO WRK-DS-06V06.                                NC2504.2
185400     ADD      +10 TO WRK-DS-06V06.                                NC2504.2
185500     IF       WRK-DS-06V06 NEGATIVE                               NC2504.2
185600              PERFORM FAIL-120-121                                NC2504.2
185700              MOVE "NEGATIVE ZERO DETECTED" TO RE-MARK            NC2504.2
185800              GO TO IF--WRITE-120.                                NC2504.2
185900     IF       WRK-DS-06V06 POSITIVE                               NC2504.2
186000              PERFORM FAIL-120-121                                NC2504.2
186100              MOVE "POSITIVE ZERO DETECTED" TO RE-MARK            NC2504.2
186200              GO TO IF--WRITE-120.                                NC2504.2
186300     IF       WRK-DS-06V06 ZERO                                   NC2504.2
186400              PERFORM PASS GO TO IF--WRITE-120.                   NC2504.2
186500     PERFORM  FAIL-120-121.                                       NC2504.2
186600     MOVE     "NEITHER POS, NEG, NOR ZERO" TO RE-MARK.            NC2504.2
186700     GO       TO IF--WRITE-120.                                   NC2504.2
186800 IF--DELETE-120.                                                  NC2504.2
186900     PERFORM  DE-LETE.                                            NC2504.2
187000 IF--WRITE-120.                                                   NC2504.2
187100     MOVE     "SIGN TEST ON ZERO" TO FEATURE.                     NC2504.2
187200     MOVE     "IF--TEST-120" TO PAR-NAME.                         NC2504.2
187300     PERFORM  PRINT-DETAIL.                                       NC2504.2
187400     GO       TO IF--EXIT-120.                                    NC2504.2
187500 FAIL-120-121.                                                    NC2504.2
187600     PERFORM  FAIL.                                               NC2504.2
187700     MOVE     WRK-DS-06V06 TO COMPUTED-N.                         NC2504.2
187800     MOVE     ZERO TO CORRECT-N.                                  NC2504.2
187900 IF--EXIT-120.                                                    NC2504.2
188000     EXIT.                                                        NC2504.2
188100 IF--TEST-121.                                                    NC2504.2
188200     MOVE     10 TO WRK-DS-06V06.                                 NC2504.2
188300     SUBTRACT 10 FROM WRK-DS-06V06.                               NC2504.2
188400     IF       WRK-DS-06V06 NEGATIVE                               NC2504.2
188500              PERFORM FAIL-120-121                                NC2504.2
188600              MOVE "NEGATIVE ZERO DETECTED" TO RE-MARK            NC2504.2
188700              GO TO IF--WRITE-121.                                NC2504.2
188800     IF       WRK-DS-06V06 POSITIVE                               NC2504.2
188900              PERFORM FAIL-120-121                                NC2504.2
189000              MOVE "POSITIVE ZERO DETECTED" TO RE-MARK            NC2504.2
189100              GO TO IF--WRITE-121.                                NC2504.2
189200                                                                  NC2504.2
189300     IF       WRK-DS-06V06 ZERO                                   NC2504.2
189400              PERFORM PASS GO TO IF--WRITE-121.                   NC2504.2
189500     PERFORM  FAIL-120-121.                                       NC2504.2
189600     MOVE     "NEITHER POS, NEG, NOR ZERO" TO RE-MARK.            NC2504.2
189700     GO       TO IF--WRITE-120.                                   NC2504.2
189800 IF--DELETE-121.                                                  NC2504.2
189900     PERFORM  DE-LETE.                                            NC2504.2
190000 IF--WRITE-121.                                                   NC2504.2
190100     MOVE     "IF--TEST-121" TO PAR-NAME.                         NC2504.2
190200     PERFORM  PRINT-DETAIL.                                       NC2504.2
190300 IF-INIT-122.                                                     NC2504.2
190400     MOVE   "VI-89 6.15" TO ANSI-REFERENCE.                       NC2504.2
190500     MOVE    1 TO WRK-DU-1V0-1.                                   NC2504.2
190600     MOVE    2 TO WRK-DU-1V0-2.                                   NC2504.2
190700     MOVE    3 TO WRK-DU-1V0-3.                                   NC2504.2
190800     MOVE    0 TO WRK-DU-1V0-4.                                   NC2504.2
190900 IF-TEST-122.                                                     NC2504.2
191000     IF NOT (WRK-DU-1V0-1 NOT GREATER WRK-DU-1V0-2 AND            NC2504.2
191100         WRK-DU-1V0-3 AND NOT WRK-DU-1V0-4) GO TO BUMMER-122      NC2504.2
191200         ELSE NEXT SENTENCE.                                      NC2504.2
191300     PERFORM PASS.                                                NC2504.2
191400     GO TO IF-WRITE-122.                                          NC2504.2
191500 IF-DELETE-122.                                                   NC2504.2
191600     PERFORM DE-LETE.                                             NC2504.2
191700     GO TO IF-WRITE-122.                                          NC2504.2
191800 BUMMER-122.                                                      NC2504.2
191900     PERFORM FAIL.                                                NC2504.2
192000     MOVE "RESULT TRUE" TO COMPUTED-A.                            NC2504.2
192100     MOVE "SHOULD BE FALSE" TO CORRECT-A.                         NC2504.2
192200 IF-WRITE-122.                                                    NC2504.2
192300     MOVE "IF-TEST-122" TO PAR-NAME.                              NC2504.2
192400     MOVE "ABR. COM. REL. CONDT" TO FEATURE.                      NC2504.2
192500     PERFORM PRINT-DETAIL.                                        NC2504.2
192600 IF-INIT-123.                                                     NC2504.2
192700     MOVE   "VI-89 6.15" TO ANSI-REFERENCE.                       NC2504.2
192800     MOVE 9 TO WRK-DU-1V0-1.                                      NC2504.2
192900     MOVE 8 TO WRK-DU-1V0-2.                                      NC2504.2
193000     MOVE 7 TO WRK-DU-1V0-3.                                      NC2504.2
193100 IF-LOGICAL-CONN-TEST-123.                                        NC2504.2
193200     IF WRK-DU-1V0-1 > WRK-DU-1V0-2 AND NOT < WRK-DU-2V0-1 OR     NC2504.2
193300             WRK-DU-2V0-2 OR NOT WRK-DU-2V0-3 AND WRK-DU-1V0-3    NC2504.2
193400             PERFORM PASS                                         NC2504.2
193500     ELSE                                                         NC2504.2
193600             PERFORM FAIL MOVE "FALSE RESULT FOUND" TO COMPUTED-A NC2504.2
193700             MOVE "SHOULD BE TRUE" TO CORRECT-A.                  NC2504.2
193800     GO TO IF-WRITE-123.                                          NC2504.2
193900 IF-DELETE-123.                                                   NC2504.2
194000     PERFORM DE-LETE.                                             NC2504.2
194100 IF-WRITE-123.                                                    NC2504.2
194200     MOVE "IF-TEST-123" TO PAR-NAME.                              NC2504.2
194300     MOVE "LOGICAL CONNECTIVES" TO FEATURE.                       NC2504.2
194400     PERFORM PRINT-DETAIL.                                        NC2504.2
194500     PERFORM END-ROUTINE.                                         NC2504.2
194600     MOVE    " COLLATING-AND-ALPHABET-TEST-9  SYNTAX CHECK IN OBJENC2504.2
194700-    "CT-COMPUTER AND SPECIAL-NAMES" TO TEST-RESULTS.             NC2504.2
194800     PERFORM PRINT-DETAIL.                                        NC2504.2
194900     MOVE SPACE TO TEST-RESULTS.                                  NC2504.2
195000 IF-INIT-124.                                                     NC2504.2
195100*    ===-->  ARITHMETIC EXPRESSION CONTAINING ZERO  <--===        NC2504.2
195200     MOVE   "VI-58 6.3.1.5 AND VI-51 6.2" TO ANSI-REFERENCE.      NC2504.2
195300     MOVE    4 TO WRK-DU-1V0-1.                                   NC2504.2
195400     MOVE   "IF-TEST-124" TO PAR-NAME.                            NC2504.2
195500 IF-TEST-124.                                                     NC2504.2
195600     IF      ZERO - WRK-DU-1V0-1 IS NEGATIVE                      NC2504.2
195700             PERFORM PASS                                         NC2504.2
195800     ELSE                                                         NC2504.2
195900             PERFORM FAIL                                         NC2504.2
196000             MOVE "POSITIVE RESULT FOUND" TO COMPUTED-A           NC2504.2
196100             MOVE "SHOULD BE NEGATIVE" TO CORRECT-A.              NC2504.2
196200     GO TO IF-WRITE-124.                                          NC2504.2
196300 IF-DELETE-124.                                                   NC2504.2
196400     PERFORM DE-LETE.                                             NC2504.2
196500 IF-WRITE-124.                                                    NC2504.2
196600     MOVE "IF-TEST-124" TO PAR-NAME.                              NC2504.2
196700     MOVE "LOGICAL CONNECTIVES" TO FEATURE.                       NC2504.2
196800     PERFORM PRINT-DETAIL.                                        NC2504.2
196900 CCVS-EXIT SECTION.                                               NC2504.2
197000 CCVS-999999.                                                     NC2504.2
197100     GO TO CLOSE-FILES.                                           NC2504.2
*END-OF,NC250A                                                                  
*HEADER,COBOL,NC251A                                                            
000100 IDENTIFICATION DIVISION.                                         NC2514.2
000200 PROGRAM-ID.                                                      NC2514.2
000300     NC251A.                                                      NC2514.2
000400****************************************************************  NC2514.2
000500*                                                              *  NC2514.2
000600*    VALIDATION FOR:-                                          *  NC2514.2
000700*                                                              *  NC2514.2
000800*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2514.2
000900*                                                              *  NC2514.2
001000*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC2514.2
001100*                                                              *  NC2514.2
001200****************************************************************  NC2514.2
001300*                                                              *  NC2514.2
001400*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  NC2514.2
001500*                                                              *  NC2514.2
001600*        X-55  - SYSTEM PRINTER NAME.                          *  NC2514.2
001700*        X-82  - SOURCE COMPUTER NAME.                         *  NC2514.2
001800*        X-83  - OBJECT COMPUTER NAME.                         *  NC2514.2
001900*                                                              *  NC2514.2
002000****************************************************************  NC2514.2
002100*                                                              *  NC2514.2
002200*    THIS PROGRAM TESTS FORMAT 5 OF THE DIVIDE STATEMENT.      *  NC2514.2
002300*                                                              *  NC2514.2
002400****************************************************************  NC2514.2
002500 ENVIRONMENT DIVISION.                                            NC2514.2
002600 CONFIGURATION SECTION.                                           NC2514.2
002700 SOURCE-COMPUTER.                                                 NC2514.2
002800     XXXXX082.                                                    NC2514.2
002900 OBJECT-COMPUTER.                                                 NC2514.2
003000     XXXXX083.                                                    NC2514.2
003100 INPUT-OUTPUT SECTION.                                            NC2514.2
003200 FILE-CONTROL.                                                    NC2514.2
003300     SELECT PRINT-FILE ASSIGN TO                                  NC2514.2
003400     XXXXX055.                                                    NC2514.2
003500 DATA DIVISION.                                                   NC2514.2
003600 FILE SECTION.                                                    NC2514.2
003700 FD  PRINT-FILE.                                                  NC2514.2
003800 01  PRINT-REC PICTURE X(120).                                    NC2514.2
003900 01  DUMMY-RECORD PICTURE X(120).                                 NC2514.2
004000 WORKING-STORAGE SECTION.                                         NC2514.2
004100 01  WRK-DU-1V17-1 PIC 9V9(17) VALUE 3.14159265358979323.         NC2514.2
004200 01  WRK-DU-1V5-1 PIC 9V9(5).                                     NC2514.2
004300 01  WRK-NE-1 PIC .9999/99999,99999,99.                           NC2514.2
004400 01  WS-REMAINDERS.                                               NC2514.2
004500   03  WS-REM                    PIC 99 OCCURS 20.                NC2514.2
004600 01  WRK-XN-00001-1              PIC X.                           NC2514.2
004700 01  WRK-XN-00001-2              PIC X.                           NC2514.2
004800 01  WS-46.                                                       NC2514.2
004900   03  WS-1-20                   PIC X(20).                       NC2514.2
005000   03  WS-21-40                  PIC X(20).                       NC2514.2
005100   03  WS-41-46                  PIC X(6).                        NC2514.2
005200 77  11A                PICTURE 9999  VALUE 9.                    NC2514.2
005300 77  11B   PICTURE 99; VALUE 8.                                   NC2514.2
005400 77  1111C PICTURE 99 VALUE 9.                                    NC2514.2
005500 77  WRK-DS-02V00                 PICTURE S99.                    NC2514.2
005600     88 TEST-2NUC-COND-99         VALUE 99.                       NC2514.2
005700 77  A99-DS-02V00                 PICTURE S99    VALUE 99.        NC2514.2
005800 77  WRK-DS-18V00                 PICTURE S9(18).                 NC2514.2
005900 77  WRK-DU-2V1-1                 PICTURE S99V9.                  NC2514.2
006000 77  A18ONES-DS-18V00             PICTURE S9(18)                  NC2514.2
006100                                  VALUE 111111111111111111.       NC2514.2
006200 77  A18TWOS-DS-18V00             PICTURE S9(18)                  NC2514.2
006300                                  VALUE 222222222222222222.       NC2514.2
006400 77  WRK-DS-05V00                 PICTURE S9(5).                  NC2514.2
006500 77  A02TWOS-DU-02V00             PICTURE 99     VALUE 22.        NC2514.2
006600 77  A02TWOS-DS-03V02             PICTURE S999V99 VALUE +022.00.  NC2514.2
006700 77  ATWO-DS-01V00                PICTURE S9     VALUE 2.         NC2514.2
006800 77  AZERO-DS-05V05               PICTURE S9(5)V9(5) VALUE ZERO.  NC2514.2
006900 77  WRK-DS-06V06                 PICTURE S9(6)V9(6).             NC2514.2
007000 77  WRK-DS-0201P                 PICTURE S99P.                   NC2514.2
007100 77  A05ONES-DS-05V00             PICTURE S9(5)  VALUE 11111.     NC2514.2
007200 77  WRK-DS-09V00                 PICTURE S9(9).                  NC2514.2
007300 77  WRK-DS-09V09                 PICTURE S9(9)V9(9).             NC2514.2
007400 77  WRK-DS-18V00-S REDEFINES WRK-DS-09V09                        NC2514.2
007500                                  PICTURE S9(18).                 NC2514.2
007600 77  XRAY                    PICTURE IS X.                        NC2514.2
007700 77  W-1                     PICTURE IS 9.                        NC2514.2
007800 77  W-2                     PICTURE IS 99.                       NC2514.2
007900 77  W-3                     PICTURE IS 999.                      NC2514.2
008000 77  W-5                PICTURE 99  VALUE ZERO.                   NC2514.2
008100 77  W-9                     PICTURE 999.                         NC2514.2
008200 77  W-11               PICTURE S99V9.                            NC2514.2
008300 77  D-1                PICTURE S9V99  VALUE 1.06.                NC2514.2
008400 77  D-7                PICTURE S99V99  VALUE 1.09.               NC2514.2
008500 77  ONE                     PICTURE IS 9      VALUE IS 1.        NC2514.2
008600 77  TWO                     PICTURE IS S9     VALUE IS 2.        NC2514.2
008700 77  THREE                   PICTURE IS S9     VALUE IS 3.        NC2514.2
008800 77  FOUR                    PICTURE IS S9     VALUE IS 4.        NC2514.2
008900 77  FIVE                    PICTURE IS S9     VALUE IS 5.        NC2514.2
009000 77  SIX                     PICTURE IS S9     VALUE IS 6.        NC2514.2
009100 77  SEVEN                   PICTURE IS S9     VALUE IS 7.        NC2514.2
009200 77  EIGHT                   PICTURE IS 9      VALUE IS 8.        NC2514.2
009300 77  NINE                    PICTURE IS S9     VALUE IS 9.        NC2514.2
009400 77  TEN                     PICTURE IS S99    VALUE IS 10.       NC2514.2
009500 77  FIFTEEN                 PICTURE IS S99    VALUE IS 15.       NC2514.2
009600 77  TWENTY                  PICTURE IS S99    VALUE IS 20.       NC2514.2
009700 77  TWENTY-5                PICTURE IS S99    VALUE IS 25.       NC2514.2
009800 77  25COUNT PICTURE 999 VALUE ZERO.                              NC2514.2
009900 77  25ANS PICTURE  99 VALUE ZERO.                                NC2514.2
010000 77  25REM PICTURE 99 VALUE ZERO.                                 NC2514.2
010100 77  DIV-30-Y1 PICTURE 999 USAGE COMP SYNC RIGHT VALUE 31.        NC2514.2
010200 77  DIV-30-Y2 PICTURE 999 USAGE COMP VALUE 54.                   NC2514.2
010300 77  DIV-30-Y3 PICTURE 999 VALUE 151.                             NC2514.2
010400 77  DIV-30-Y4         PICTURE 9(4) SYNC RIGHT VALUE 1010.        NC2514.2
010500 77  DIV-Z1-30 PICTURE 999 USAGE COMP VALUE ZERO.                 NC2514.2
010600 77  DIV-Z2-30 PICTURE 999 SYNC RIGHT VALUE ZERO.                 NC2514.2
010700 77  DIV-Z3-30 PICTURE 999 USAGE COMP SYNC RIGHT VALUE ZERO.      NC2514.2
010800 77  DIV-Z4-30 PICTURE 999 VALUE ZERO.                            NC2514.2
010900 77  DIV-30-A1 PICTURE 999 SYNC RIGHT VALUE ZERO.                 NC2514.2
011000 77  DIV-30-A2 PICTURE 999 VALUE ZERO.                            NC2514.2
011100 77  DIV-30-A3 PICTURE 999 USAGE COMP SYNC RIGHT VALUE ZERO.      NC2514.2
011200 77  DIV-30-A4 PICTURE 999 USAGE COMP VALUE ZERO.                 NC2514.2
011300 01  DIV-ENTRIES.                                                 NC2514.2
011400     02 DIV11                PICTURE 999       VALUE 105.         NC2514.2
011500     02 DIV12                PICTURE 9999      VALUE 1000.        NC2514.2
011600     02 DIV13                PICTURE 999.                         NC2514.2
011700     02 DIV14                PICTURE 99.                          NC2514.2
011800     02 DIV15                PICTURE 9V9       VALUE 1.1.         NC2514.2
011900     02 DIV16                PICTURE 99V99     VALUE 89.10.       NC2514.2
012000     02 DIV17                PICTURE 99V99.                       NC2514.2
012100     02 DIV18                PICTURE 9999.                        NC2514.2
012200     02 DIV19                PICTURE 99        VALUE 14.          NC2514.2
012300     02 DIV20                PICTURE 9999      VALUE 2147.        NC2514.2
012400     02 DIV21                PICTURE 999.                         NC2514.2
012500     02 DIV22                     PICTURE 99.                     NC2514.2
012600 01  WRK-DU-05V00-0001            PIC 9(5).                       NC2514.2
012700 01  WRK-DS-05V00-0002            PIC S9(5).                      NC2514.2
012800 01  WRK-CS-05V00-0003            PIC S9(5) COMP.                 NC2514.2
012900 01  WRK-DU-04V02-0004            PIC 9(4)V9(2).                  NC2514.2
013000 01  WRK-DS-04V01-0005            PIC S9(4)V9.                    NC2514.2
013100 01  NE-0008                      PIC $9(4).99-.                  NC2514.2
013200 01  NE-0009                      PIC ***99.                      NC2514.2
013300 01  NE-04V01-0006     PIC ****.9.                                NC2514.2
013400 01  GRP-0010.                                                    NC2514.2
013500     02 WRK-DU-03V00-L-0011       PIC 9(03) SYNC LEFT.            NC2514.2
013600     02 WRK-O005F-0012        OCCURS   5  TIMES.                  NC2514.2
013700        03 WRK-O003F-0013     OCCURS   3  TIMES.                  NC2514.2
013800           05 WRK-DS-03V04-O003F-0014 PIC S9(3)V9999              NC2514.2
013900                                            OCCURS 3 TIMES.       NC2514.2
014000 01  DS-02V00-0001                PIC S99  VALUE  16.             NC2514.2
014100 01  DS-03V00-0002                PIC S999 VALUE  174.            NC2514.2
014200 01  CS-05V00-0003                PIC S9(5) COMP  VALUE 10.       NC2514.2
014300 01    TA--X           PIC 9(5)  COMP VALUE ZERO.                 NC2514.2
014400 01  MINUS-NAMES.                                                 NC2514.2
014500     02  WHOLE-FIELD              PICTURE S9(18).                 NC2514.2
014600     02  PLUS-NAME1  PICTURE S9(18) VALUE +333333333333333333.    NC2514.2
014700     02  EVEN-NAME1  PICTURE S9(18) VALUE +1.                     NC2514.2
014800     02  PLUS-NAME2  PICTURE S9(18) VALUE +999999999999999999.    NC2514.2
014900     02  ALPHA-LIT                PICTURE X(5)  VALUE SPACE.      NC2514.2
015000     02  SNEG-LIT2                PICTURE S9(5)  VALUE -70718.    NC2514.2
015100 01  TEST-RESULTS.                                                NC2514.2
015200     02 FILLER                   PIC X      VALUE SPACE.          NC2514.2
015300     02 FEATURE                  PIC X(20)  VALUE SPACE.          NC2514.2
015400     02 FILLER                   PIC X      VALUE SPACE.          NC2514.2
015500     02 P-OR-F                   PIC X(5)   VALUE SPACE.          NC2514.2
015600     02 FILLER                   PIC X      VALUE SPACE.          NC2514.2
015700     02  PAR-NAME.                                                NC2514.2
015800       03 FILLER                 PIC X(19)  VALUE SPACE.          NC2514.2
015900       03  PARDOT-X              PIC X      VALUE SPACE.          NC2514.2
016000       03 DOTVALUE               PIC 99     VALUE ZERO.           NC2514.2
016100     02 FILLER                   PIC X(8)   VALUE SPACE.          NC2514.2
016200     02 RE-MARK                  PIC X(61).                       NC2514.2
016300 01  TEST-COMPUTED.                                               NC2514.2
016400     02 FILLER                   PIC X(30)  VALUE SPACE.          NC2514.2
016500     02 FILLER                   PIC X(17)  VALUE                 NC2514.2
016600            "       COMPUTED=".                                   NC2514.2
016700     02 COMPUTED-X.                                               NC2514.2
016800     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          NC2514.2
016900     03 COMPUTED-N               REDEFINES COMPUTED-A             NC2514.2
017000                                 PIC -9(9).9(9).                  NC2514.2
017100     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         NC2514.2
017200     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     NC2514.2
017300     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     NC2514.2
017400     03       CM-18V0 REDEFINES COMPUTED-A.                       NC2514.2
017500         04 COMPUTED-18V0                    PIC -9(18).          NC2514.2
017600         04 FILLER                           PIC X.               NC2514.2
017700     03 FILLER PIC X(50) VALUE SPACE.                             NC2514.2
017800 01  TEST-CORRECT.                                                NC2514.2
017900     02 FILLER PIC X(30) VALUE SPACE.                             NC2514.2
018000     02 FILLER PIC X(17) VALUE "       CORRECT =".                NC2514.2
018100     02 CORRECT-X.                                                NC2514.2
018200     03 CORRECT-A                  PIC X(20) VALUE SPACE.         NC2514.2
018300     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      NC2514.2
018400     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         NC2514.2
018500     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     NC2514.2
018600     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     NC2514.2
018700     03      CR-18V0 REDEFINES CORRECT-A.                         NC2514.2
018800         04 CORRECT-18V0                     PIC -9(18).          NC2514.2
018900         04 FILLER                           PIC X.               NC2514.2
019000     03 FILLER PIC X(2) VALUE SPACE.                              NC2514.2
019100     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     NC2514.2
019200 01  CCVS-C-1.                                                    NC2514.2
019300     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PANC2514.2
019400-    "SS  PARAGRAPH-NAME                                          NC2514.2
019500-    "       REMARKS".                                            NC2514.2
019600     02 FILLER                     PIC X(20)    VALUE SPACE.      NC2514.2
019700 01  CCVS-C-2.                                                    NC2514.2
019800     02 FILLER                     PIC X        VALUE SPACE.      NC2514.2
019900     02 FILLER                     PIC X(6)     VALUE "TESTED".   NC2514.2
020000     02 FILLER                     PIC X(15)    VALUE SPACE.      NC2514.2
020100     02 FILLER                     PIC X(4)     VALUE "FAIL".     NC2514.2
020200     02 FILLER                     PIC X(94)    VALUE SPACE.      NC2514.2
020300 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       NC2514.2
020400 01  REC-CT                        PIC 99       VALUE ZERO.       NC2514.2
020500 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       NC2514.2
020600 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       NC2514.2
020700 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       NC2514.2
020800 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       NC2514.2
020900 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       NC2514.2
021000 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       NC2514.2
021100 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      NC2514.2
021200 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       NC2514.2
021300 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     NC2514.2
021400 01  CCVS-H-1.                                                    NC2514.2
021500     02  FILLER                    PIC X(39)    VALUE SPACES.     NC2514.2
021600     02  FILLER                    PIC X(42)    VALUE             NC2514.2
021700     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 NC2514.2
021800     02  FILLER                    PIC X(39)    VALUE SPACES.     NC2514.2
021900 01  CCVS-H-2A.                                                   NC2514.2
022000   02  FILLER                        PIC X(40)  VALUE SPACE.      NC2514.2
022100   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  NC2514.2
022200   02  FILLER                        PIC XXXX   VALUE             NC2514.2
022300     "4.2 ".                                                      NC2514.2
022400   02  FILLER                        PIC X(28)  VALUE             NC2514.2
022500            " COPY - NOT FOR DISTRIBUTION".                       NC2514.2
022600   02  FILLER                        PIC X(41)  VALUE SPACE.      NC2514.2
022700                                                                  NC2514.2
022800 01  CCVS-H-2B.                                                   NC2514.2
022900   02  FILLER                        PIC X(15)  VALUE             NC2514.2
023000            "TEST RESULT OF ".                                    NC2514.2
023100   02  TEST-ID                       PIC X(9).                    NC2514.2
023200   02  FILLER                        PIC X(4)   VALUE             NC2514.2
023300            " IN ".                                               NC2514.2
023400   02  FILLER                        PIC X(12)  VALUE             NC2514.2
023500     " HIGH       ".                                              NC2514.2
023600   02  FILLER                        PIC X(22)  VALUE             NC2514.2
023700            " LEVEL VALIDATION FOR ".                             NC2514.2
023800   02  FILLER                        PIC X(58)  VALUE             NC2514.2
023900     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2514.2
024000 01  CCVS-H-3.                                                    NC2514.2
024100     02  FILLER                      PIC X(34)  VALUE             NC2514.2
024200            " FOR OFFICIAL USE ONLY    ".                         NC2514.2
024300     02  FILLER                      PIC X(58)  VALUE             NC2514.2
024400     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC2514.2
024500     02  FILLER                      PIC X(28)  VALUE             NC2514.2
024600            "  COPYRIGHT   1985 ".                                NC2514.2
024700 01  CCVS-E-1.                                                    NC2514.2
024800     02 FILLER                       PIC X(52)  VALUE SPACE.      NC2514.2
024900     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              NC2514.2
025000     02 ID-AGAIN                     PIC X(9).                    NC2514.2
025100     02 FILLER                       PIC X(45)  VALUE SPACES.     NC2514.2
025200 01  CCVS-E-2.                                                    NC2514.2
025300     02  FILLER                      PIC X(31)  VALUE SPACE.      NC2514.2
025400     02  FILLER                      PIC X(21)  VALUE SPACE.      NC2514.2
025500     02 CCVS-E-2-2.                                               NC2514.2
025600         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      NC2514.2
025700         03 FILLER                   PIC X      VALUE SPACE.      NC2514.2
025800         03 ENDER-DESC               PIC X(44)  VALUE             NC2514.2
025900            "ERRORS ENCOUNTERED".                                 NC2514.2
026000 01  CCVS-E-3.                                                    NC2514.2
026100     02  FILLER                      PIC X(22)  VALUE             NC2514.2
026200            " FOR OFFICIAL USE ONLY".                             NC2514.2
026300     02  FILLER                      PIC X(12)  VALUE SPACE.      NC2514.2
026400     02  FILLER                      PIC X(58)  VALUE             NC2514.2
026500     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2514.2
026600     02  FILLER                      PIC X(13)  VALUE SPACE.      NC2514.2
026700     02 FILLER                       PIC X(15)  VALUE             NC2514.2
026800             " COPYRIGHT 1985".                                   NC2514.2
026900 01  CCVS-E-4.                                                    NC2514.2
027000     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      NC2514.2
027100     02 FILLER                       PIC X(4)   VALUE " OF ".     NC2514.2
027200     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      NC2514.2
027300     02 FILLER                       PIC X(40)  VALUE             NC2514.2
027400      "  TESTS WERE EXECUTED SUCCESSFULLY".                       NC2514.2
027500 01  XXINFO.                                                      NC2514.2
027600     02 FILLER                       PIC X(19)  VALUE             NC2514.2
027700            "*** INFORMATION ***".                                NC2514.2
027800     02 INFO-TEXT.                                                NC2514.2
027900       04 FILLER                     PIC X(8)   VALUE SPACE.      NC2514.2
028000       04 XXCOMPUTED                 PIC X(20).                   NC2514.2
028100       04 FILLER                     PIC X(5)   VALUE SPACE.      NC2514.2
028200       04 XXCORRECT                  PIC X(20).                   NC2514.2
028300     02 INF-ANSI-REFERENCE           PIC X(48).                   NC2514.2
028400 01  HYPHEN-LINE.                                                 NC2514.2
028500     02 FILLER  PIC IS X VALUE IS SPACE.                          NC2514.2
028600     02 FILLER  PIC IS X(65)    VALUE IS "************************NC2514.2
028700-    "*****************************************".                 NC2514.2
028800     02 FILLER  PIC IS X(54)    VALUE IS "************************NC2514.2
028900-    "******************************".                            NC2514.2
029000 01  CCVS-PGM-ID                     PIC X(9)   VALUE             NC2514.2
029100     "NC251A".                                                    NC2514.2
029200 PROCEDURE DIVISION.                                              NC2514.2
029300 CCVS1 SECTION.                                                   NC2514.2
029400 OPEN-FILES.                                                      NC2514.2
029500     OPEN     OUTPUT PRINT-FILE.                                  NC2514.2
029600     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   NC2514.2
029700     MOVE    SPACE TO TEST-RESULTS.                               NC2514.2
029800     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             NC2514.2
029900     GO TO CCVS1-EXIT.                                            NC2514.2
030000 CLOSE-FILES.                                                     NC2514.2
030100     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   NC2514.2
030200 TERMINATE-CCVS.                                                  NC2514.2
030300S    EXIT PROGRAM.                                                NC2514.2
030400STERMINATE-CALL.                                                  NC2514.2
030500     STOP     RUN.                                                NC2514.2
030600 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         NC2514.2
030700 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           NC2514.2
030800 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          NC2514.2
030900 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      NC2514.2
031000     MOVE "****TEST DELETED****" TO RE-MARK.                      NC2514.2
031100 PRINT-DETAIL.                                                    NC2514.2
031200     IF REC-CT NOT EQUAL TO ZERO                                  NC2514.2
031300             MOVE "." TO PARDOT-X                                 NC2514.2
031400             MOVE REC-CT TO DOTVALUE.                             NC2514.2
031500     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      NC2514.2
031600     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               NC2514.2
031700        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 NC2514.2
031800          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 NC2514.2
031900     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              NC2514.2
032000     MOVE SPACE TO CORRECT-X.                                     NC2514.2
032100     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         NC2514.2
032200     MOVE     SPACE TO RE-MARK.                                   NC2514.2
032300 HEAD-ROUTINE.                                                    NC2514.2
032400     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC2514.2
032500     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC2514.2
032600     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC2514.2
032700     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC2514.2
032800 COLUMN-NAMES-ROUTINE.                                            NC2514.2
032900     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2514.2
033000     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2514.2
033100     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        NC2514.2
033200 END-ROUTINE.                                                     NC2514.2
033300     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.NC2514.2
033400 END-RTN-EXIT.                                                    NC2514.2
033500     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2514.2
033600 END-ROUTINE-1.                                                   NC2514.2
033700      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      NC2514.2
033800      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               NC2514.2
033900      ADD PASS-COUNTER TO ERROR-HOLD.                             NC2514.2
034000*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   NC2514.2
034100      MOVE PASS-COUNTER TO CCVS-E-4-1.                            NC2514.2
034200      MOVE ERROR-HOLD TO CCVS-E-4-2.                              NC2514.2
034300      MOVE CCVS-E-4 TO CCVS-E-2-2.                                NC2514.2
034400      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           NC2514.2
034500  END-ROUTINE-12.                                                 NC2514.2
034600      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        NC2514.2
034700     IF       ERROR-COUNTER IS EQUAL TO ZERO                      NC2514.2
034800         MOVE "NO " TO ERROR-TOTAL                                NC2514.2
034900         ELSE                                                     NC2514.2
035000         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       NC2514.2
035100     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           NC2514.2
035200     PERFORM WRITE-LINE.                                          NC2514.2
035300 END-ROUTINE-13.                                                  NC2514.2
035400     IF DELETE-COUNTER IS EQUAL TO ZERO                           NC2514.2
035500         MOVE "NO " TO ERROR-TOTAL  ELSE                          NC2514.2
035600         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      NC2514.2
035700     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   NC2514.2
035800     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2514.2
035900      IF   INSPECT-COUNTER EQUAL TO ZERO                          NC2514.2
036000          MOVE "NO " TO ERROR-TOTAL                               NC2514.2
036100      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   NC2514.2
036200      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            NC2514.2
036300      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          NC2514.2
036400     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2514.2
036500 WRITE-LINE.                                                      NC2514.2
036600     ADD 1 TO RECORD-COUNT.                                       NC2514.2
036700Y    IF RECORD-COUNT GREATER 50                                   NC2514.2
036800Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          NC2514.2
036900Y        MOVE SPACE TO DUMMY-RECORD                               NC2514.2
037000Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  NC2514.2
037100Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             NC2514.2
037200Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     NC2514.2
037300Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          NC2514.2
037400Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          NC2514.2
037500Y        MOVE ZERO TO RECORD-COUNT.                               NC2514.2
037600     PERFORM WRT-LN.                                              NC2514.2
037700 WRT-LN.                                                          NC2514.2
037800     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               NC2514.2
037900     MOVE SPACE TO DUMMY-RECORD.                                  NC2514.2
038000 BLANK-LINE-PRINT.                                                NC2514.2
038100     PERFORM WRT-LN.                                              NC2514.2
038200 FAIL-ROUTINE.                                                    NC2514.2
038300     IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. NC2514.2
038400     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.NC2514.2
038500     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC2514.2
038600     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   NC2514.2
038700     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2514.2
038800     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC2514.2
038900     GO TO  FAIL-ROUTINE-EX.                                      NC2514.2
039000 FAIL-ROUTINE-WRITE.                                              NC2514.2
039100     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         NC2514.2
039200     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 NC2514.2
039300     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. NC2514.2
039400     MOVE   SPACES TO COR-ANSI-REFERENCE.                         NC2514.2
039500 FAIL-ROUTINE-EX. EXIT.                                           NC2514.2
039600 BAIL-OUT.                                                        NC2514.2
039700     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   NC2514.2
039800     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           NC2514.2
039900 BAIL-OUT-WRITE.                                                  NC2514.2
040000     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  NC2514.2
040100     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC2514.2
040200     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2514.2
040300     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC2514.2
040400 BAIL-OUT-EX. EXIT.                                               NC2514.2
040500 CCVS1-EXIT.                                                      NC2514.2
040600     EXIT.                                                        NC2514.2
040700 SECT-NC251A-001 SECTION.                                         NC2514.2
040800 DIV-TEST-4.                                                      NC2514.2
040900     DIVIDE DIV16 BY DIV15 GIVING DIV17 REMAINDER DIV18.          NC2514.2
041000     IF DIV18 IS EQUAL TO ZERO                                    NC2514.2
041100              PERFORM PASS                                        NC2514.2
041200              GO TO DIV-WRITE-4.                                  NC2514.2
041300     PERFORM FAIL.                                                NC2514.2
041400     MOVE DIV18 TO COMPUTED-N.                                    NC2514.2
041500     MOVE "0000" TO CORRECT-A.                                    NC2514.2
041600     GO TO DIV-WRITE-4.                                           NC2514.2
041700 DIV-DELETE-4.                                                    NC2514.2
041800     PERFORM DE-LETE.                                             NC2514.2
041900 DIV-WRITE-4.                                                     NC2514.2
042000     MOVE "DIV-TEST-4" TO PAR-NAME.                               NC2514.2
042100     PERFORM PRINT-DETAIL.                                        NC2514.2
042200 DIV-TEST-5.                                                      NC2514.2
042300     MOVE ZERO TO DIV21.                                          NC2514.2
042400     MOVE ZERO TO DIV22.                                          NC2514.2
042500     DIVIDE DIV20 BY DIV19 GIVING DIV21 ROUNDED REMAINDER         NC2514.2
042600     DIV22.                                                       NC2514.2
042700     IF DIV22 IS EQUAL TO 05                                      NC2514.2
042800              PERFORM PASS                                        NC2514.2
042900              GO TO DIV-WRITE-5.                                  NC2514.2
043000     PERFORM FAIL.                                                NC2514.2
043100     MOVE DIV22 TO COMPUTED-N.                                    NC2514.2
043200     MOVE "+05" TO CORRECT-A.                                     NC2514.2
043300     GO TO DIV-WRITE-5.                                           NC2514.2
043400 DIV-DELETE-5.                                                    NC2514.2
043500     PERFORM DE-LETE.                                             NC2514.2
043600 DIV-WRITE-5.                                                     NC2514.2
043700     MOVE "DIV-TEST-5" TO PAR-NAME.                               NC2514.2
043800     PERFORM PRINT-DETAIL.                                        NC2514.2
043900*                                                                 NC2514.2
044000 DIV-INIT-F5-3.                                                   NC2514.2
044100     MOVE   "VI-82 6.11.4 GR4" TO ANSI-REFERENCE.                 NC2514.2
044200     MOVE   "DIV-TEST-F5-3-0"  TO PAR-NAME.                       NC2514.2
044300     MOVE    40   TO 25COUNT.                                     NC2514.2
044400     MOVE    ZERO TO 25ANS.                                       NC2514.2
044500     MOVE    ZERO TO 25REM.                                       NC2514.2
044600     MOVE    1    TO REC-CT.                                      NC2514.2
044700 DIV-TEST-F5-3-0.                                                 NC2514.2
044800     DIVIDE  100 BY 25COUNT GIVING 25ANS REMAINDER 25REM          NC2514.2
044900             ON SIZE ERROR                                        NC2514.2
045000             MOVE   "SIZE ERROR SHOULD NOT HAVE OCCURED"          NC2514.2
045100                  TO RE-MARK                                      NC2514.2
045200             PERFORM FAIL                                         NC2514.2
045300             PERFORM PRINT-DETAIL                                 NC2514.2
045400             GO TO   DIV-TEST-F5-3-1.                             NC2514.2
045500     PERFORM PASS.                                                NC2514.2
045600     PERFORM PRINT-DETAIL.                                        NC2514.2
045700     GO TO   DIV-TEST-F5-3-1.                                     NC2514.2
045800 DIV-DELETE-F5-3.                                                 NC2514.2
045900     PERFORM DE-LETE.                                             NC2514.2
046000     PERFORM PRINT-DETAIL.                                        NC2514.2
046100     GO TO   DIV-INIT-F5-4.                                       NC2514.2
046200 DIV-TEST-F5-3-1.                                                 NC2514.2
046300     MOVE   "DIV-TEST-F5-3-1" TO PAR-NAME.                        NC2514.2
046400     ADD     1 TO REC-CT.                                         NC2514.2
046500     IF      25ANS NOT = 2                                        NC2514.2
046600             MOVE    2 TO CORRECT-N                               NC2514.2
046700             MOVE    25ANS TO COMPUTED-N                          NC2514.2
046800             MOVE   "INVALID QUOTIENT" TO RE-MARK                 NC2514.2
046900             PERFORM FAIL                                         NC2514.2
047000             PERFORM PRINT-DETAIL                                 NC2514.2
047100     ELSE                                                         NC2514.2
047200             PERFORM PASS                                         NC2514.2
047300             PERFORM PRINT-DETAIL.                                NC2514.2
047400 DIV-TEST-F5-3-2.                                                 NC2514.2
047500     MOVE   "DIV-TEST-F5-3-2" TO PAR-NAME.                        NC2514.2
047600     ADD     1 TO REC-CT.                                         NC2514.2
047700     IF      25REM NOT = 20                                       NC2514.2
047800             MOVE    25REM TO COMPUTED-N                          NC2514.2
047900             MOVE    20    TO CORRECT-N                           NC2514.2
048000             MOVE   "INVALID REMAINDER" TO RE-MARK                NC2514.2
048100             PERFORM FAIL                                         NC2514.2
048200             PERFORM PRINT-DETAIL                                 NC2514.2
048300     ELSE                                                         NC2514.2
048400             PERFORM PASS                                         NC2514.2
048500             PERFORM PRINT-DETAIL.                                NC2514.2
048600*                                                                 NC2514.2
048700 DIV-INIT-F5-4.                                                   NC2514.2
048800     MOVE   "VI-82 6.11.4 GR4" TO ANSI-REFERENCE.                 NC2514.2
048900     MOVE   "DIV-TEST-F5-4-0"  TO PAR-NAME.                       NC2514.2
049000     MOVE    ZERO TO 25COUNT.                                     NC2514.2
049100     MOVE    ZERO TO 25ANS.                                       NC2514.2
049200     MOVE    ZERO TO 25REM.                                       NC2514.2
049300     MOVE    1    TO REC-CT.                                      NC2514.2
049400 DIV-TEST-F5-4-0.                                                 NC2514.2
049500     DIVIDE  100 BY 25COUNT GIVING 25ANS REMAINDER 25REM          NC2514.2
049600             ON SIZE ERROR                                        NC2514.2
049700             PERFORM PASS                                         NC2514.2
049800             PERFORM PRINT-DETAIL                                 NC2514.2
049900             GO TO   DIV-TEST-F5-4-1.                             NC2514.2
050000     MOVE   "ON SIZE ERROR SHOULD HAVE OCCURRED" TO RE-MARK.      NC2514.2
050100     PERFORM FAIL.                                                NC2514.2
050200     PERFORM PRINT-DETAIL.                                        NC2514.2
050300     GO TO   DIV-TEST-F5-4-1.                                     NC2514.2
050400 DIV-DELETE-F5-4.                                                 NC2514.2
050500     PERFORM DE-LETE.                                             NC2514.2
050600     PERFORM PRINT-DETAIL.                                        NC2514.2
050700     GO TO   DIV-INIT-F5-5.                                       NC2514.2
050800 DIV-TEST-F5-4-1.                                                 NC2514.2
050900     MOVE   "DIV-TEST-F5-4-1" TO PAR-NAME.                        NC2514.2
051000     ADD     1 TO REC-CT.                                         NC2514.2
051100     IF      25ANS NOT = 0                                        NC2514.2
051200             MOVE    0 TO CORRECT-N                               NC2514.2
051300             MOVE    25ANS TO COMPUTED-N                          NC2514.2
051400             MOVE   "INVALID QUOTIENT" TO RE-MARK                 NC2514.2
051500             PERFORM FAIL                                         NC2514.2
051600             PERFORM PRINT-DETAIL                                 NC2514.2
051700     ELSE                                                         NC2514.2
051800             PERFORM PASS                                         NC2514.2
051900             PERFORM PRINT-DETAIL.                                NC2514.2
052000 DIV-TEST-F5-4-2.                                                 NC2514.2
052100     MOVE   "DIV-TEST-F5-4-2" TO PAR-NAME.                        NC2514.2
052200     ADD     1 TO REC-CT.                                         NC2514.2
052300     IF      25REM NOT = ZERO                                     NC2514.2
052400             MOVE    25REM TO COMPUTED-N                          NC2514.2
052500             MOVE    ZERO  TO CORRECT-N                           NC2514.2
052600             MOVE   "INVALID REMAINDER" TO RE-MARK                NC2514.2
052700             PERFORM FAIL                                         NC2514.2
052800             PERFORM PRINT-DETAIL                                 NC2514.2
052900     ELSE                                                         NC2514.2
053000             PERFORM PASS                                         NC2514.2
053100             PERFORM PRINT-DETAIL.                                NC2514.2
053200*                                                                 NC2514.2
053300 DIV-INIT-F5-5.                                                   NC2514.2
053400     MOVE   "VI-82 6.11.4 GR4" TO ANSI-REFERENCE.                 NC2514.2
053500     MOVE   "DIV-TEST-F5-5-0"  TO PAR-NAME.                       NC2514.2
053600     MOVE    3    TO 25COUNT.                                     NC2514.2
053700     MOVE    ZERO TO 25ANS.                                       NC2514.2
053800     MOVE    ZERO TO 25REM.                                       NC2514.2
053900     MOVE    1    TO REC-CT.                                      NC2514.2
054000 DIV-TEST-F5-5-0.                                                 NC2514.2
054100     DIVIDE  100 BY 25COUNT GIVING 25ANS REMAINDER 25REM          NC2514.2
054200             ON SIZE ERROR                                        NC2514.2
054300             MOVE   "SIZE ERROR SHOULD NOT HAVE OCCURED"          NC2514.2
054400                  TO RE-MARK                                      NC2514.2
054500             PERFORM FAIL                                         NC2514.2
054600             PERFORM PRINT-DETAIL                                 NC2514.2
054700             GO TO   DIV-TEST-F5-5-1.                             NC2514.2
054800     PERFORM PASS.                                                NC2514.2
054900     PERFORM PRINT-DETAIL.                                        NC2514.2
055000     GO TO   DIV-TEST-F5-5-1.                                     NC2514.2
055100 DIV-DELETE-F5-5.                                                 NC2514.2
055200     PERFORM DE-LETE.                                             NC2514.2
055300     PERFORM PRINT-DETAIL.                                        NC2514.2
055400     GO TO   DIV-TEST-12.                                         NC2514.2
055500 DIV-TEST-F5-5-1.                                                 NC2514.2
055600     MOVE   "DIV-TEST-F5-5-1" TO PAR-NAME.                        NC2514.2
055700     ADD     1 TO REC-CT.                                         NC2514.2
055800     IF      25ANS NOT = 33                                       NC2514.2
055900             MOVE    33    TO CORRECT-N                           NC2514.2
056000             MOVE    25ANS TO COMPUTED-N                          NC2514.2
056100             MOVE   "INVALID QUOTIENT" TO RE-MARK                 NC2514.2
056200             PERFORM FAIL                                         NC2514.2
056300             PERFORM PRINT-DETAIL                                 NC2514.2
056400     ELSE                                                         NC2514.2
056500             PERFORM PASS                                         NC2514.2
056600             PERFORM PRINT-DETAIL.                                NC2514.2
056700 DIV-TEST-F5-5-2.                                                 NC2514.2
056800     MOVE   "DIV-TEST-F5-5-2" TO PAR-NAME.                        NC2514.2
056900     ADD     1 TO REC-CT.                                         NC2514.2
057000     IF      25REM NOT = 1                                        NC2514.2
057100             MOVE    25REM TO COMPUTED-N                          NC2514.2
057200             MOVE    1     TO CORRECT-N                           NC2514.2
057300             MOVE   "INVALID REMAINDER" TO RE-MARK                NC2514.2
057400             PERFORM FAIL                                         NC2514.2
057500             PERFORM PRINT-DETAIL                                 NC2514.2
057600     ELSE                                                         NC2514.2
057700             PERFORM PASS                                         NC2514.2
057800             PERFORM PRINT-DETAIL.                                NC2514.2
057900*                                                                 NC2514.2
058000 DIV-TEST-12.                                                     NC2514.2
058100     DIVIDE 230 BY DIV-30-Y2 GIVING DIV-Z2-30 REMAINDER           NC2514.2
058200     DIV-30-A2.                                                   NC2514.2
058300     IF DIV-Z2-30 EQUAL TO 4 AND DIV-30-A2 EQUAL TO 14            NC2514.2
058400              PERFORM PASS                                        NC2514.2
058500              GO TO DIV-WRITE-12.                                 NC2514.2
058600     PERFORM FAIL.                                                NC2514.2
058700     MOVE   4 TO CORRECT-N.                                       NC2514.2
058800     MOVE DIV-30-A3 TO COMPUTED-N.                                NC2514.2
058900     GO TO DIV-WRITE-12.                                          NC2514.2
059000 DIV-DELETE-12.                                                   NC2514.2
059100     PERFORM DE-LETE.                                             NC2514.2
059200 DIV-WRITE-12.                                                    NC2514.2
059300     MOVE "DIV-TEST-12" TO PAR-NAME.                              NC2514.2
059400     PERFORM PRINT-DETAIL.                                        NC2514.2
059500*                                                                 NC2514.2
059600 DIV-INIT-F5-7.                                                   NC2514.2
059700     MOVE   "VI-82 6.11.4 GR4" TO ANSI-REFERENCE.                 NC2514.2
059800     MOVE   "DIV-TEST-F5-7-0"  TO PAR-NAME.                       NC2514.2
059900     MOVE    151  TO DIV-30-Y3.                                   NC2514.2
060000     MOVE    ZERO TO DIV-Z3-30.                                   NC2514.2
060100     MOVE    ZERO TO DIV-30-A3.                                   NC2514.2
060200     MOVE    1    TO REC-CT.                                      NC2514.2
060300 DIV-TEST-F5-7-0.                                                 NC2514.2
060400     DIVIDE 681 BY DIV-30-Y3 GIVING DIV-Z3-30 REMAINDER           NC2514.2
060500     DIV-30-A3.                                                   NC2514.2
060600     GO TO   DIV-TEST-F5-7-1.                                     NC2514.2
060700 DIV-DELETE-F5-7.                                                 NC2514.2
060800     PERFORM DE-LETE.                                             NC2514.2
060900     PERFORM PRINT-DETAIL.                                        NC2514.2
061000     GO TO   DIV-INIT-F5-8.                                       NC2514.2
061100 DIV-TEST-F5-7-1.                                                 NC2514.2
061200     MOVE   "DIV-TEST-F5-7-1" TO PAR-NAME.                        NC2514.2
061300     ADD     1 TO REC-CT.                                         NC2514.2
061400     IF      DIV-Z3-30 NOT EQUAL TO 4                             NC2514.2
061500             MOVE    4     TO CORRECT-N                           NC2514.2
061600             MOVE    DIV-Z3-30 TO COMPUTED-N                      NC2514.2
061700             MOVE   "INVALID QUOTIENT" TO RE-MARK                 NC2514.2
061800             PERFORM FAIL                                         NC2514.2
061900             PERFORM PRINT-DETAIL                                 NC2514.2
062000     ELSE                                                         NC2514.2
062100             PERFORM PASS                                         NC2514.2
062200             PERFORM PRINT-DETAIL.                                NC2514.2
062300 DIV-TEST-F5-7-2.                                                 NC2514.2
062400     MOVE   "DIV-TEST-F5-7-2" TO PAR-NAME.                        NC2514.2
062500     ADD     1 TO REC-CT.                                         NC2514.2
062600     IF      DIV-30-A3 NOT EQUAL TO 77                            NC2514.2
062700             MOVE    DIV-30-A3 TO COMPUTED-N                      NC2514.2
062800             MOVE    77    TO CORRECT-N                           NC2514.2
062900             MOVE   "INVALID REMAINDER" TO RE-MARK                NC2514.2
063000             PERFORM FAIL                                         NC2514.2
063100             PERFORM PRINT-DETAIL                                 NC2514.2
063200     ELSE                                                         NC2514.2
063300             PERFORM PASS                                         NC2514.2
063400             PERFORM PRINT-DETAIL.                                NC2514.2
063500*                                                                 NC2514.2
063600 DIV-INIT-F5-8.                                                   NC2514.2
063700     MOVE   "VI-82 6.11.4 GR4" TO ANSI-REFERENCE.                 NC2514.2
063800     MOVE   "DIV-TEST-F5-8-0"  TO PAR-NAME.                       NC2514.2
063900     MOVE    1010 TO DIV-30-Y4.                                   NC2514.2
064000     MOVE    ZERO TO DIV-Z4-30.                                   NC2514.2
064100     MOVE    ZERO TO DIV-30-A4.                                   NC2514.2
064200     MOVE    1    TO REC-CT.                                      NC2514.2
064300 DIV-TEST-F5-8-0.                                                 NC2514.2
064400     DIVIDE  4150 BY DIV-30-Y4 GIVING DIV-Z4-30 REMAINDER         NC2514.2
064500     DIV-30-A4.                                                   NC2514.2
064600     GO TO   DIV-TEST-F5-8-1.                                     NC2514.2
064700 DIV-DELETE-F5-8.                                                 NC2514.2
064800     PERFORM DE-LETE.                                             NC2514.2
064900     PERFORM PRINT-DETAIL.                                        NC2514.2
065000     GO TO   DIV-INIT-F5-9.                                       NC2514.2
065100 DIV-TEST-F5-8-1.                                                 NC2514.2
065200     MOVE   "DIV-TEST-F5-8-1" TO PAR-NAME.                        NC2514.2
065300     ADD     1 TO REC-CT.                                         NC2514.2
065400     IF      DIV-Z4-30 NOT EQUAL TO 4                             NC2514.2
065500             MOVE    4     TO CORRECT-N                           NC2514.2
065600             MOVE    DIV-Z4-30 TO COMPUTED-N                      NC2514.2
065700             MOVE   "INVALID QUOTIENT" TO RE-MARK                 NC2514.2
065800             PERFORM FAIL                                         NC2514.2
065900             PERFORM PRINT-DETAIL                                 NC2514.2
066000     ELSE                                                         NC2514.2
066100             PERFORM PASS                                         NC2514.2
066200             PERFORM PRINT-DETAIL.                                NC2514.2
066300 DIV-TEST-F5-8-2.                                                 NC2514.2
066400     MOVE   "DIV-TEST-F5-8-2" TO PAR-NAME.                        NC2514.2
066500     ADD     1 TO REC-CT.                                         NC2514.2
066600     IF      DIV-30-A4 NOT EQUAL TO 110                           NC2514.2
066700             MOVE    DIV-30-A4 TO COMPUTED-N                      NC2514.2
066800             MOVE    110   TO CORRECT-N                           NC2514.2
066900             MOVE   "INVALID REMAINDER" TO RE-MARK                NC2514.2
067000             PERFORM FAIL                                         NC2514.2
067100             PERFORM PRINT-DETAIL                                 NC2514.2
067200     ELSE                                                         NC2514.2
067300             PERFORM PASS                                         NC2514.2
067400             PERFORM PRINT-DETAIL.                                NC2514.2
067500*                                                                 NC2514.2
067600*                                                                 NC2514.2
067700 DIV-INIT-F5-9.                                                   NC2514.2
067800     MOVE   "VI-82 6.11.4 GR4" TO ANSI-REFERENCE.                 NC2514.2
067900     MOVE   "DIV-TEST-F5-9-0"  TO PAR-NAME.                       NC2514.2
068000     MOVE    31   TO DIV-30-Y1.                                   NC2514.2
068100     MOVE    ZERO TO DIV-Z1-30.                                   NC2514.2
068200     MOVE    ZERO TO DIV-30-A1.                                   NC2514.2
068300     MOVE    1    TO REC-CT.                                      NC2514.2
068400 DIV-TEST-F5-9-0.                                                 NC2514.2
068500     DIVIDE 150 BY DIV-30-Y1 GIVING DIV-Z1-30 REMAINDER DIV-30-A1.NC2514.2
068600     GO TO   DIV-TEST-F5-9-1.                                     NC2514.2
068700 DIV-DELETE-F5-9.                                                 NC2514.2
068800     PERFORM DE-LETE.                                             NC2514.2
068900     PERFORM PRINT-DETAIL.                                        NC2514.2
069000     GO TO   DIV-INIT-F5-10.                                      NC2514.2
069100 DIV-TEST-F5-9-1.                                                 NC2514.2
069200     MOVE   "DIV-TEST-F5-9-1" TO PAR-NAME.                        NC2514.2
069300     ADD     1 TO REC-CT.                                         NC2514.2
069400     IF      DIV-Z1-30 NOT EQUAL TO 4                             NC2514.2
069500             MOVE    4     TO CORRECT-N                           NC2514.2
069600             MOVE    DIV-Z1-30 TO COMPUTED-N                      NC2514.2
069700             MOVE   "INVALID QUOTIENT" TO RE-MARK                 NC2514.2
069800             PERFORM FAIL                                         NC2514.2
069900             PERFORM PRINT-DETAIL                                 NC2514.2
070000     ELSE                                                         NC2514.2
070100             PERFORM PASS                                         NC2514.2
070200             PERFORM PRINT-DETAIL.                                NC2514.2
070300 DIV-TEST-F5-9-2.                                                 NC2514.2
070400     MOVE   "DIV-TEST-F5-9-2" TO PAR-NAME.                        NC2514.2
070500     ADD     1 TO REC-CT.                                         NC2514.2
070600     IF      DIV-30-A1 NOT EQUAL TO 26                            NC2514.2
070700             MOVE    DIV-30-A4 TO COMPUTED-N                      NC2514.2
070800             MOVE    26    TO CORRECT-N                           NC2514.2
070900             MOVE   "INVALID REMAINDER" TO RE-MARK                NC2514.2
071000             PERFORM FAIL                                         NC2514.2
071100             PERFORM PRINT-DETAIL                                 NC2514.2
071200     ELSE                                                         NC2514.2
071300             PERFORM PASS                                         NC2514.2
071400             PERFORM PRINT-DETAIL.                                NC2514.2
071500*                                                                 NC2514.2
071600 DIV-INIT-F5-10.                                                  NC2514.2
071700     MOVE   "VI-82 6.11.4 GR8" TO ANSI-REFERENCE.                 NC2514.2
071800     MOVE   "DIV-TEST-F5-10-0" TO PAR-NAME.                       NC2514.2
071900     MOVE    40   TO 25COUNT.                                     NC2514.2
072000     MOVE    ZERO TO 25ANS.                                       NC2514.2
072100     MOVE    ZERO TO 25REM.                                       NC2514.2
072200     MOVE    1    TO REC-CT.                                      NC2514.2
072300 DIV-TEST-F5-10-0.                                                NC2514.2
072400     DIVIDE  100 BY 25COUNT GIVING 25ANS REMAINDER 25REM          NC2514.2
072500         NOT ON SIZE ERROR                                        NC2514.2
072600             PERFORM PASS                                         NC2514.2
072700             PERFORM PRINT-DETAIL                                 NC2514.2
072800             GO TO   DIV-TEST-F5-10-1.                            NC2514.2
072900     MOVE   "NOT ON SIZE ERROR SHOULD HAVE EXECUTED" TO RE-MARK.  NC2514.2
073000     PERFORM FAIL.                                                NC2514.2
073100     PERFORM PRINT-DETAIL.                                        NC2514.2
073200     GO TO   DIV-TEST-F5-10-1.                                    NC2514.2
073300 DIV-DELETE-F5-10.                                                NC2514.2
073400     PERFORM DE-LETE.                                             NC2514.2
073500     PERFORM PRINT-DETAIL.                                        NC2514.2
073600     GO TO   DIV-INIT-F5-11.                                      NC2514.2
073700 DIV-TEST-F5-10-1.                                                NC2514.2
073800     MOVE   "DIV-TEST-F5-10-1" TO PAR-NAME.                       NC2514.2
073900     ADD     1 TO REC-CT.                                         NC2514.2
074000     IF      25ANS NOT = 2                                        NC2514.2
074100             MOVE    2 TO CORRECT-N                               NC2514.2
074200             MOVE    25ANS TO COMPUTED-N                          NC2514.2
074300             MOVE   "INVALID QUOTIENT" TO RE-MARK                 NC2514.2
074400             PERFORM FAIL                                         NC2514.2
074500             PERFORM PRINT-DETAIL                                 NC2514.2
074600     ELSE                                                         NC2514.2
074700             PERFORM PASS                                         NC2514.2
074800             PERFORM PRINT-DETAIL.                                NC2514.2
074900 DIV-TEST-F5-10-2.                                                NC2514.2
075000     MOVE   "DIV-TEST-F5-10-2" TO PAR-NAME.                       NC2514.2
075100     ADD     1 TO REC-CT.                                         NC2514.2
075200     IF      25REM NOT = 20                                       NC2514.2
075300             MOVE    25REM TO COMPUTED-N                          NC2514.2
075400             MOVE    20    TO CORRECT-N                           NC2514.2
075500             MOVE   "INVALID REMAINDER" TO RE-MARK                NC2514.2
075600             PERFORM FAIL                                         NC2514.2
075700             PERFORM PRINT-DETAIL                                 NC2514.2
075800     ELSE                                                         NC2514.2
075900             PERFORM PASS                                         NC2514.2
076000             PERFORM PRINT-DETAIL.                                NC2514.2
076100*                                                                 NC2514.2
076200 DIV-INIT-F5-11.                                                  NC2514.2
076300     MOVE   "VI-82 6.11.4 GR8" TO ANSI-REFERENCE.                 NC2514.2
076400     MOVE   "DIV-TEST-F5-11-0"  TO PAR-NAME.                      NC2514.2
076500     MOVE    ZERO TO 25COUNT.                                     NC2514.2
076600     MOVE    ZERO TO 25ANS.                                       NC2514.2
076700     MOVE    ZERO TO 25REM.                                       NC2514.2
076800     MOVE    1    TO REC-CT.                                      NC2514.2
076900 DIV-TEST-F5-11-0.                                                NC2514.2
077000     DIVIDE  100 BY 25COUNT GIVING 25ANS REMAINDER 25REM          NC2514.2
077100         NOT ON SIZE ERROR                                        NC2514.2
077200             MOVE   "NOT ON SIZE ERROR SHOULD NOT HAVE EXECUTED"  NC2514.2
077300                  TO RE-MARK                                      NC2514.2
077400             PERFORM FAIL                                         NC2514.2
077500             PERFORM PRINT-DETAIL                                 NC2514.2
077600             GO TO   DIV-TEST-F5-11-1.                            NC2514.2
077700     PERFORM PASS.                                                NC2514.2
077800     PERFORM PRINT-DETAIL.                                        NC2514.2
077900     GO TO   DIV-TEST-F5-11-1.                                    NC2514.2
078000 DIV-DELETE-F5-11.                                                NC2514.2
078100     PERFORM DE-LETE.                                             NC2514.2
078200     PERFORM PRINT-DETAIL.                                        NC2514.2
078300     GO TO   DIV-INIT-F5-12.                                      NC2514.2
078400 DIV-TEST-F5-11-1.                                                NC2514.2
078500     MOVE   "DIV-TEST-F5-11-1" TO PAR-NAME.                       NC2514.2
078600     ADD     1 TO REC-CT.                                         NC2514.2
078700     IF      25ANS NOT = 0                                        NC2514.2
078800             MOVE    0 TO CORRECT-N                               NC2514.2
078900             MOVE    25ANS TO COMPUTED-N                          NC2514.2
079000             MOVE   "INVALID QUOTIENT" TO RE-MARK                 NC2514.2
079100             PERFORM FAIL                                         NC2514.2
079200             PERFORM PRINT-DETAIL                                 NC2514.2
079300     ELSE                                                         NC2514.2
079400             PERFORM PASS                                         NC2514.2
079500             PERFORM PRINT-DETAIL.                                NC2514.2
079600 DIV-TEST-F5-11-2.                                                NC2514.2
079700     MOVE   "DIV-TEST-F5-11-2" TO PAR-NAME.                       NC2514.2
079800     ADD     1 TO REC-CT.                                         NC2514.2
079900     IF      25REM NOT = ZERO                                     NC2514.2
080000             MOVE    25REM TO COMPUTED-N                          NC2514.2
080100             MOVE    ZERO  TO CORRECT-N                           NC2514.2
080200             MOVE   "INVALID REMAINDER" TO RE-MARK                NC2514.2
080300             PERFORM FAIL                                         NC2514.2
080400             PERFORM PRINT-DETAIL                                 NC2514.2
080500     ELSE                                                         NC2514.2
080600             PERFORM PASS                                         NC2514.2
080700             PERFORM PRINT-DETAIL.                                NC2514.2
080800*                                                                 NC2514.2
080900 DIV-INIT-F5-12.                                                  NC2514.2
081000     MOVE   "VI-82 6.11.4 GR8" TO ANSI-REFERENCE.                 NC2514.2
081100     MOVE   "DIV-TEST-F5-12-0" TO PAR-NAME.                       NC2514.2
081200     MOVE    40   TO 25COUNT.                                     NC2514.2
081300     MOVE    ZERO TO 25ANS.                                       NC2514.2
081400     MOVE    ZERO TO 25REM.                                       NC2514.2
081500     MOVE    1    TO REC-CT.                                      NC2514.2
081600 DIV-TEST-F5-12-0.                                                NC2514.2
081700     DIVIDE  100 BY 25COUNT GIVING 25ANS REMAINDER 25REM          NC2514.2
081800             ON SIZE ERROR                                        NC2514.2
081900             MOVE   "ON SIZE ERROR SHOULD NOT HAVE EXECUTED"      NC2514.2
082000                  TO RE-MARK                                      NC2514.2
082100             PERFORM FAIL                                         NC2514.2
082200             PERFORM PRINT-DETAIL                                 NC2514.2
082300             GO TO   DIV-TEST-F5-12-1                             NC2514.2
082400         NOT ON SIZE ERROR                                        NC2514.2
082500             PERFORM PASS                                         NC2514.2
082600             PERFORM PRINT-DETAIL                                 NC2514.2
082700             GO TO   DIV-TEST-F5-12-1.                            NC2514.2
082800 DIV-DELETE-F5-12.                                                NC2514.2
082900     PERFORM DE-LETE.                                             NC2514.2
083000     PERFORM PRINT-DETAIL.                                        NC2514.2
083100     GO TO   DIV-INIT-F5-13.                                      NC2514.2
083200 DIV-TEST-F5-12-1.                                                NC2514.2
083300     MOVE   "DIV-TEST-F5-12-1" TO PAR-NAME.                       NC2514.2
083400     ADD     1 TO REC-CT.                                         NC2514.2
083500     IF      25ANS NOT = 2                                        NC2514.2
083600             MOVE    2 TO CORRECT-N                               NC2514.2
083700             MOVE    25ANS TO COMPUTED-N                          NC2514.2
083800             MOVE   "INVALID QUOTIENT" TO RE-MARK                 NC2514.2
083900             PERFORM FAIL                                         NC2514.2
084000             PERFORM PRINT-DETAIL                                 NC2514.2
084100     ELSE                                                         NC2514.2
084200             PERFORM PASS                                         NC2514.2
084300             PERFORM PRINT-DETAIL.                                NC2514.2
084400 DIV-TEST-F5-12-2.                                                NC2514.2
084500     MOVE   "DIV-TEST-F5-12-2" TO PAR-NAME.                       NC2514.2
084600     ADD     1 TO REC-CT.                                         NC2514.2
084700     IF      25REM NOT = 20                                       NC2514.2
084800             MOVE    25REM TO COMPUTED-N                          NC2514.2
084900             MOVE    20    TO CORRECT-N                           NC2514.2
085000             MOVE   "INVALID REMAINDER" TO RE-MARK                NC2514.2
085100             PERFORM FAIL                                         NC2514.2
085200             PERFORM PRINT-DETAIL                                 NC2514.2
085300     ELSE                                                         NC2514.2
085400             PERFORM PASS                                         NC2514.2
085500             PERFORM PRINT-DETAIL.                                NC2514.2
085600*                                                                 NC2514.2
085700 DIV-INIT-F5-13.                                                  NC2514.2
085800     MOVE   "VI-82 6.11.4 GR8" TO ANSI-REFERENCE.                 NC2514.2
085900     MOVE   "DIV-TEST-F5-13-0"  TO PAR-NAME.                      NC2514.2
086000     MOVE    ZERO TO 25COUNT.                                     NC2514.2
086100     MOVE    ZERO TO 25ANS.                                       NC2514.2
086200     MOVE    ZERO TO 25REM.                                       NC2514.2
086300     MOVE    1    TO REC-CT.                                      NC2514.2
086400 DIV-TEST-F5-13-0.                                                NC2514.2
086500     DIVIDE  100 BY 25COUNT GIVING 25ANS REMAINDER 25REM          NC2514.2
086600             ON SIZE ERROR                                        NC2514.2
086700             PERFORM PASS                                         NC2514.2
086800             PERFORM PRINT-DETAIL                                 NC2514.2
086900             GO TO   DIV-TEST-F5-13-1                             NC2514.2
087000         NOT ON SIZE ERROR                                        NC2514.2
087100             MOVE   "NOT ON SIZE ERROR SHOULD NOT HAVE EXECUTED"  NC2514.2
087200                  TO RE-MARK                                      NC2514.2
087300             PERFORM FAIL                                         NC2514.2
087400             PERFORM PRINT-DETAIL                                 NC2514.2
087500             GO TO   DIV-TEST-F5-13-1.                            NC2514.2
087600 DIV-DELETE-F5-13.                                                NC2514.2
087700     PERFORM DE-LETE.                                             NC2514.2
087800     PERFORM PRINT-DETAIL.                                        NC2514.2
087900     GO TO   DIV-INIT-F5-14.                                      NC2514.2
088000 DIV-TEST-F5-13-1.                                                NC2514.2
088100     MOVE   "DIV-TEST-F5-13-1" TO PAR-NAME.                       NC2514.2
088200     ADD     1 TO REC-CT.                                         NC2514.2
088300     IF      25ANS NOT = 0                                        NC2514.2
088400             MOVE    0 TO CORRECT-N                               NC2514.2
088500             MOVE    25ANS TO COMPUTED-N                          NC2514.2
088600             MOVE   "INVALID QUOTIENT" TO RE-MARK                 NC2514.2
088700             PERFORM FAIL                                         NC2514.2
088800             PERFORM PRINT-DETAIL                                 NC2514.2
088900     ELSE                                                         NC2514.2
089000             PERFORM PASS                                         NC2514.2
089100             PERFORM PRINT-DETAIL.                                NC2514.2
089200 DIV-TEST-F5-13-2.                                                NC2514.2
089300     MOVE   "DIV-TEST-F5-13-2" TO PAR-NAME.                       NC2514.2
089400     ADD     1 TO REC-CT.                                         NC2514.2
089500     IF      25REM NOT = ZERO                                     NC2514.2
089600             MOVE    25REM TO COMPUTED-N                          NC2514.2
089700             MOVE    ZERO  TO CORRECT-N                           NC2514.2
089800             MOVE   "INVALID REMAINDER" TO RE-MARK                NC2514.2
089900             PERFORM FAIL                                         NC2514.2
090000             PERFORM PRINT-DETAIL                                 NC2514.2
090100     ELSE                                                         NC2514.2
090200             PERFORM PASS                                         NC2514.2
090300             PERFORM PRINT-DETAIL.                                NC2514.2
090400*                                                                 NC2514.2
090500 DIV-INIT-F5-14.                                                  NC2514.2
090600     MOVE   "VI-82 6.11.4 GR9" TO ANSI-REFERENCE.                 NC2514.2
090700     MOVE   "DIV-TEST-F5-14-0" TO PAR-NAME.                       NC2514.2
090800     MOVE    40    TO 25COUNT.                                    NC2514.2
090900     MOVE    ZERO  TO 25ANS.                                      NC2514.2
091000     MOVE    ZERO  TO 25REM.                                      NC2514.2
091100     MOVE    1     TO REC-CT.                                     NC2514.2
091200     MOVE    SPACE TO WRK-XN-00001-1.                             NC2514.2
091300     MOVE    SPACE TO WRK-XN-00001-2.                             NC2514.2
091400 DIV-TEST-F5-14-0.                                                NC2514.2
091500     DIVIDE  100 BY 25COUNT GIVING 25ANS REMAINDER 25REM          NC2514.2
091600             ON SIZE ERROR                                        NC2514.2
091700             MOVE   "A" TO WRK-XN-00001-1                         NC2514.2
091800     END-DIVIDE                                                   NC2514.2
091900     MOVE   "B" TO WRK-XN-00001-2.                                NC2514.2
092000     GO TO   DIV-TEST-F5-14-1.                                    NC2514.2
092100 DIV-DELETE-F5-14.                                                NC2514.2
092200     PERFORM DE-LETE.                                             NC2514.2
092300     PERFORM PRINT-DETAIL.                                        NC2514.2
092400     GO TO   DIV-INIT-F5-15.                                      NC2514.2
092500 DIV-TEST-F5-14-1.                                                NC2514.2
092600     MOVE   "DIV-TEST-F5-14-1" TO PAR-NAME.                       NC2514.2
092700     ADD     1 TO REC-CT.                                         NC2514.2
092800     IF      25ANS NOT = 2                                        NC2514.2
092900             MOVE    2 TO CORRECT-N                               NC2514.2
093000             MOVE    25ANS TO COMPUTED-N                          NC2514.2
093100             MOVE   "INVALID QUOTIENT" TO RE-MARK                 NC2514.2
093200             PERFORM FAIL                                         NC2514.2
093300             PERFORM PRINT-DETAIL                                 NC2514.2
093400     ELSE                                                         NC2514.2
093500             PERFORM PASS                                         NC2514.2
093600             PERFORM PRINT-DETAIL.                                NC2514.2
093700 DIV-TEST-F5-14-2.                                                NC2514.2
093800     MOVE   "DIV-TEST-F5-14-2" TO PAR-NAME.                       NC2514.2
093900     ADD     1 TO REC-CT.                                         NC2514.2
094000     IF      25REM NOT = 20                                       NC2514.2
094100             MOVE    25REM TO COMPUTED-N                          NC2514.2
094200             MOVE    20    TO CORRECT-N                           NC2514.2
094300             MOVE   "INVALID REMAINDER" TO RE-MARK                NC2514.2
094400             PERFORM FAIL                                         NC2514.2
094500             PERFORM PRINT-DETAIL                                 NC2514.2
094600     ELSE                                                         NC2514.2
094700             PERFORM PASS                                         NC2514.2
094800             PERFORM PRINT-DETAIL.                                NC2514.2
094900 DIV-TEST-F5-14-3.                                                NC2514.2
095000     MOVE   "DIV-TEST-F5-14-3" TO PAR-NAME.                       NC2514.2
095100     ADD     1 TO REC-CT.                                         NC2514.2
095200     IF      WRK-XN-00001-1 = SPACE                               NC2514.2
095300             PERFORM PASS                                         NC2514.2
095400             PERFORM PRINT-DETAIL                                 NC2514.2
095500     ELSE                                                         NC2514.2
095600             MOVE   "ON SIZE ERROR SHOULD NOT HAVE EXECUTED"      NC2514.2
095700                  TO RE-MARK                                      NC2514.2
095800             MOVE    SPACE TO CORRECT-A                           NC2514.2
095900             MOVE    WRK-XN-00001-1 TO COMPUTED-A                 NC2514.2
096000             PERFORM FAIL                                         NC2514.2
096100             PERFORM PRINT-DETAIL.                                NC2514.2
096200 DIV-TEST-F5-14-4.                                                NC2514.2
096300     MOVE   "DIV-TEST-F5-14-4" TO PAR-NAME.                       NC2514.2
096400     ADD     1 TO REC-CT.                                         NC2514.2
096500     IF      WRK-XN-00001-2 = "B"                                 NC2514.2
096600             PERFORM PASS                                         NC2514.2
096700             PERFORM PRINT-DETAIL                                 NC2514.2
096800     ELSE                                                         NC2514.2
096900             MOVE   "SCOPE TERMINATOR IGNORED" TO RE-MARK         NC2514.2
097000             MOVE   "B" TO CORRECT-A                              NC2514.2
097100             MOVE    WRK-XN-00001-2 TO COMPUTED-A                 NC2514.2
097200             PERFORM FAIL                                         NC2514.2
097300             PERFORM PRINT-DETAIL.                                NC2514.2
097400*                                                                 NC2514.2
097500 DIV-INIT-F5-15.                                                  NC2514.2
097600     MOVE   "VI-82 6.11.4 GR9" TO ANSI-REFERENCE.                 NC2514.2
097700     MOVE   "DIV-TEST-F5-15-0"  TO PAR-NAME.                      NC2514.2
097800     MOVE    ZERO  TO 25COUNT.                                    NC2514.2
097900     MOVE    ZERO  TO 25ANS.                                      NC2514.2
098000     MOVE    ZERO  TO 25REM.                                      NC2514.2
098100     MOVE    SPACE TO WRK-XN-00001-1.                             NC2514.2
098200     MOVE    SPACE TO WRK-XN-00001-2.                             NC2514.2
098300     MOVE    1     TO REC-CT.                                     NC2514.2
098400 DIV-TEST-F5-15-0.                                                NC2514.2
098500     DIVIDE  100 BY 25COUNT GIVING 25ANS REMAINDER 25REM          NC2514.2
098600             ON SIZE ERROR                                        NC2514.2
098700             MOVE   "A" TO WRK-XN-00001-1                         NC2514.2
098800     END-DIVIDE                                                   NC2514.2
098900     MOVE   "B" TO WRK-XN-00001-2.                                NC2514.2
099000     GO TO   DIV-TEST-F5-15-1.                                    NC2514.2
099100 DIV-DELETE-F5-15.                                                NC2514.2
099200     PERFORM DE-LETE.                                             NC2514.2
099300     PERFORM PRINT-DETAIL.                                        NC2514.2
099400     GO TO   DIV-INIT-F5-16.                                      NC2514.2
099500 DIV-TEST-F5-15-1.                                                NC2514.2
099600     MOVE   "DIV-TEST-F5-15-1" TO PAR-NAME.                       NC2514.2
099700     ADD     1 TO REC-CT.                                         NC2514.2
099800     IF      25ANS NOT = 0                                        NC2514.2
099900             MOVE    0 TO CORRECT-N                               NC2514.2
100000             MOVE    25ANS TO COMPUTED-N                          NC2514.2
100100             MOVE   "INVALID QUOTIENT" TO RE-MARK                 NC2514.2
100200             PERFORM FAIL                                         NC2514.2
100300             PERFORM PRINT-DETAIL                                 NC2514.2
100400     ELSE                                                         NC2514.2
100500             PERFORM PASS                                         NC2514.2
100600             PERFORM PRINT-DETAIL.                                NC2514.2
100700 DIV-TEST-F5-15-2.                                                NC2514.2
100800     MOVE   "DIV-TEST-F5-15-2" TO PAR-NAME.                       NC2514.2
100900     ADD     1 TO REC-CT.                                         NC2514.2
101000     IF      25REM NOT = ZERO                                     NC2514.2
101100             MOVE    25REM TO COMPUTED-N                          NC2514.2
101200             MOVE    ZERO  TO CORRECT-N                           NC2514.2
101300             MOVE   "INVALID REMAINDER" TO RE-MARK                NC2514.2
101400             PERFORM FAIL                                         NC2514.2
101500             PERFORM PRINT-DETAIL                                 NC2514.2
101600     ELSE                                                         NC2514.2
101700             PERFORM PASS                                         NC2514.2
101800             PERFORM PRINT-DETAIL.                                NC2514.2
101900 DIV-TEST-F5-15-3.                                                NC2514.2
102000     MOVE   "DIV-TEST-F5-15-3" TO PAR-NAME.                       NC2514.2
102100     ADD     1 TO REC-CT.                                         NC2514.2
102200     IF      WRK-XN-00001-1 = "A"                                 NC2514.2
102300             PERFORM PASS                                         NC2514.2
102400             PERFORM PRINT-DETAIL                                 NC2514.2
102500     ELSE                                                         NC2514.2
102600             MOVE   "ON SIZE ERROR SHOULD HAVE EXECUTED"          NC2514.2
102700                  TO RE-MARK                                      NC2514.2
102800             MOVE   "A" TO CORRECT-A                              NC2514.2
102900             MOVE    WRK-XN-00001-1 TO COMPUTED-A                 NC2514.2
103000             PERFORM FAIL                                         NC2514.2
103100             PERFORM PRINT-DETAIL.                                NC2514.2
103200 DIV-TEST-F5-15-4.                                                NC2514.2
103300     MOVE   "DIV-TEST-F5-15-4" TO PAR-NAME.                       NC2514.2
103400     ADD     1 TO REC-CT.                                         NC2514.2
103500     IF      WRK-XN-00001-2 = "B"                                 NC2514.2
103600             PERFORM PASS                                         NC2514.2
103700             PERFORM PRINT-DETAIL                                 NC2514.2
103800     ELSE                                                         NC2514.2
103900             MOVE   "SCOPE TERMINATOR IGNORED" TO RE-MARK         NC2514.2
104000             MOVE    SPACE TO CORRECT-A                           NC2514.2
104100             MOVE    WRK-XN-00001-1 TO COMPUTED-A                 NC2514.2
104200             PERFORM FAIL                                         NC2514.2
104300             PERFORM PRINT-DETAIL.                                NC2514.2
104400*                                                                 NC2514.2
104500 DIV-INIT-F5-16.                                                  NC2514.2
104600     MOVE   "VI-82 6.11.4 GR9" TO ANSI-REFERENCE.                 NC2514.2
104700     MOVE   "DIV-TEST-F5-16-0" TO PAR-NAME.                       NC2514.2
104800     MOVE    40    TO 25COUNT.                                    NC2514.2
104900     MOVE    ZERO  TO 25ANS.                                      NC2514.2
105000     MOVE    ZERO  TO 25REM.                                      NC2514.2
105100     MOVE    1     TO REC-CT.                                     NC2514.2
105200     MOVE    SPACE TO WRK-XN-00001-1.                             NC2514.2
105300     MOVE    SPACE TO WRK-XN-00001-2.                             NC2514.2
105400 DIV-TEST-F5-16-0.                                                NC2514.2
105500     DIVIDE  100 BY 25COUNT GIVING 25ANS REMAINDER 25REM          NC2514.2
105600         NOT ON SIZE ERROR                                        NC2514.2
105700             MOVE   "A" TO WRK-XN-00001-1                         NC2514.2
105800     END-DIVIDE                                                   NC2514.2
105900     MOVE   "B" TO WRK-XN-00001-2.                                NC2514.2
106000     GO TO   DIV-TEST-F5-16-1.                                    NC2514.2
106100 DIV-DELETE-F5-16.                                                NC2514.2
106200     PERFORM DE-LETE.                                             NC2514.2
106300     PERFORM PRINT-DETAIL.                                        NC2514.2
106400     GO TO   DIV-INIT-F5-17.                                      NC2514.2
106500 DIV-TEST-F5-16-1.                                                NC2514.2
106600     MOVE   "DIV-TEST-F5-16-1" TO PAR-NAME.                       NC2514.2
106700     ADD     1 TO REC-CT.                                         NC2514.2
106800     IF      25ANS NOT = 2                                        NC2514.2
106900             MOVE    2 TO CORRECT-N                               NC2514.2
107000             MOVE    25ANS TO COMPUTED-N                          NC2514.2
107100             MOVE   "INVALID QUOTIENT" TO RE-MARK                 NC2514.2
107200             PERFORM FAIL                                         NC2514.2
107300             PERFORM PRINT-DETAIL                                 NC2514.2
107400     ELSE                                                         NC2514.2
107500             PERFORM PASS                                         NC2514.2
107600             PERFORM PRINT-DETAIL.                                NC2514.2
107700 DIV-TEST-F5-16-2.                                                NC2514.2
107800     MOVE   "DIV-TEST-F5-16-2" TO PAR-NAME.                       NC2514.2
107900     ADD     1 TO REC-CT.                                         NC2514.2
108000     IF      25REM NOT = 20                                       NC2514.2
108100             MOVE    25REM TO COMPUTED-N                          NC2514.2
108200             MOVE    20    TO CORRECT-N                           NC2514.2
108300             MOVE   "INVALID REMAINDER" TO RE-MARK                NC2514.2
108400             PERFORM FAIL                                         NC2514.2
108500             PERFORM PRINT-DETAIL                                 NC2514.2
108600     ELSE                                                         NC2514.2
108700             PERFORM PASS                                         NC2514.2
108800             PERFORM PRINT-DETAIL.                                NC2514.2
108900 DIV-TEST-F5-16-3.                                                NC2514.2
109000     MOVE   "DIV-TEST-F5-16-3" TO PAR-NAME.                       NC2514.2
109100     ADD     1 TO REC-CT.                                         NC2514.2
109200     IF      WRK-XN-00001-1 = "A"                                 NC2514.2
109300             PERFORM PASS                                         NC2514.2
109400             PERFORM PRINT-DETAIL                                 NC2514.2
109500     ELSE                                                         NC2514.2
109600             MOVE   "NOT ON SIZE ERROR SHOULD HAVE EXECUTED"      NC2514.2
109700                  TO RE-MARK                                      NC2514.2
109800             MOVE   "A" TO CORRECT-A                              NC2514.2
109900             MOVE    WRK-XN-00001-1 TO COMPUTED-A                 NC2514.2
110000             PERFORM FAIL                                         NC2514.2
110100             PERFORM PRINT-DETAIL.                                NC2514.2
110200 DIV-TEST-F5-16-4.                                                NC2514.2
110300     MOVE   "DIV-TEST-F5-16-4" TO PAR-NAME.                       NC2514.2
110400     ADD     1 TO REC-CT.                                         NC2514.2
110500     IF      WRK-XN-00001-2 = "B"                                 NC2514.2
110600             PERFORM PASS                                         NC2514.2
110700             PERFORM PRINT-DETAIL                                 NC2514.2
110800     ELSE                                                         NC2514.2
110900             MOVE   "SCOPE TERMINATOR IGNORED" TO RE-MARK         NC2514.2
111000             MOVE   "B" TO CORRECT-A                              NC2514.2
111100             MOVE    WRK-XN-00001-2 TO COMPUTED-A                 NC2514.2
111200             PERFORM FAIL                                         NC2514.2
111300             PERFORM PRINT-DETAIL.                                NC2514.2
111400*                                                                 NC2514.2
111500 DIV-INIT-F5-17.                                                  NC2514.2
111600     MOVE   "VI-82 6.11.4 GR9" TO ANSI-REFERENCE.                 NC2514.2
111700     MOVE   "DIV-TEST-F5-17-0"  TO PAR-NAME.                      NC2514.2
111800     MOVE    ZERO  TO 25COUNT.                                    NC2514.2
111900     MOVE    ZERO  TO 25ANS.                                      NC2514.2
112000     MOVE    ZERO  TO 25REM.                                      NC2514.2
112100     MOVE    SPACE TO WRK-XN-00001-1.                             NC2514.2
112200     MOVE    SPACE TO WRK-XN-00001-2.                             NC2514.2
112300     MOVE    1     TO REC-CT.                                     NC2514.2
112400 DIV-TEST-F5-17-0.                                                NC2514.2
112500     DIVIDE  100 BY 25COUNT GIVING 25ANS REMAINDER 25REM          NC2514.2
112600         NOT ON SIZE ERROR                                        NC2514.2
112700             MOVE   "A" TO WRK-XN-00001-1                         NC2514.2
112800     END-DIVIDE                                                   NC2514.2
112900     MOVE   "B" TO WRK-XN-00001-2.                                NC2514.2
113000     GO TO   DIV-TEST-F5-17-1.                                    NC2514.2
113100 DIV-DELETE-F5-17.                                                NC2514.2
113200     PERFORM DE-LETE.                                             NC2514.2
113300     PERFORM PRINT-DETAIL.                                        NC2514.2
113400     GO TO   DIV-INIT-F5-18.                                      NC2514.2
113500 DIV-TEST-F5-17-1.                                                NC2514.2
113600     MOVE   "DIV-TEST-F5-17-1" TO PAR-NAME.                       NC2514.2
113700     ADD     1 TO REC-CT.                                         NC2514.2
113800     IF      25ANS NOT = 0                                        NC2514.2
113900             MOVE    0 TO CORRECT-N                               NC2514.2
114000             MOVE    25ANS TO COMPUTED-N                          NC2514.2
114100             MOVE   "INVALID QUOTIENT" TO RE-MARK                 NC2514.2
114200             PERFORM FAIL                                         NC2514.2
114300             PERFORM PRINT-DETAIL                                 NC2514.2
114400     ELSE                                                         NC2514.2
114500             PERFORM PASS                                         NC2514.2
114600             PERFORM PRINT-DETAIL.                                NC2514.2
114700 DIV-TEST-F5-17-2.                                                NC2514.2
114800     MOVE   "DIV-TEST-F5-17-2" TO PAR-NAME.                       NC2514.2
114900     ADD     1 TO REC-CT.                                         NC2514.2
115000     IF      25REM NOT = ZERO                                     NC2514.2
115100             MOVE    25REM TO COMPUTED-N                          NC2514.2
115200             MOVE    ZERO  TO CORRECT-N                           NC2514.2
115300             MOVE   "INVALID REMAINDER" TO RE-MARK                NC2514.2
115400             PERFORM FAIL                                         NC2514.2
115500             PERFORM PRINT-DETAIL                                 NC2514.2
115600     ELSE                                                         NC2514.2
115700             PERFORM PASS                                         NC2514.2
115800             PERFORM PRINT-DETAIL.                                NC2514.2
115900 DIV-TEST-F5-17-3.                                                NC2514.2
116000     MOVE   "DIV-TEST-F5-17-3" TO PAR-NAME.                       NC2514.2
116100     ADD     1 TO REC-CT.                                         NC2514.2
116200     IF      WRK-XN-00001-1 = SPACE                               NC2514.2
116300             PERFORM PASS                                         NC2514.2
116400             PERFORM PRINT-DETAIL                                 NC2514.2
116500     ELSE                                                         NC2514.2
116600             MOVE   "NOT ON SIZE ERROR SHOULD NOT HAVE EXECUTED"  NC2514.2
116700                  TO RE-MARK                                      NC2514.2
116800             MOVE    SPACE TO CORRECT-A                           NC2514.2
116900             MOVE    WRK-XN-00001-1 TO COMPUTED-A                 NC2514.2
117000             PERFORM FAIL                                         NC2514.2
117100             PERFORM PRINT-DETAIL.                                NC2514.2
117200 DIV-TEST-F5-17-4.                                                NC2514.2
117300     MOVE   "DIV-TEST-F5-17-4" TO PAR-NAME.                       NC2514.2
117400     ADD     1 TO REC-CT.                                         NC2514.2
117500     IF      WRK-XN-00001-2 = "B"                                 NC2514.2
117600             PERFORM PASS                                         NC2514.2
117700             PERFORM PRINT-DETAIL                                 NC2514.2
117800     ELSE                                                         NC2514.2
117900             MOVE   "SCOPE TERMINATOR IGNORED" TO RE-MARK         NC2514.2
118000             MOVE   "B" TO CORRECT-A                              NC2514.2
118100             MOVE    WRK-XN-00001-2 TO COMPUTED-A                 NC2514.2
118200             PERFORM FAIL                                         NC2514.2
118300             PERFORM PRINT-DETAIL.                                NC2514.2
118400*                                                                 NC2514.2
118500 DIV-INIT-F5-18.                                                  NC2514.2
118600     MOVE   "VI-82 6.11.4 GR8" TO ANSI-REFERENCE.                 NC2514.2
118700     MOVE   "DIV-TEST-F5-18-0" TO PAR-NAME.                       NC2514.2
118800     MOVE    40   TO 25COUNT.                                     NC2514.2
118900     MOVE    ZERO  TO 25ANS.                                      NC2514.2
119000     MOVE    ZERO  TO 25REM.                                      NC2514.2
119100     MOVE    1     TO REC-CT.                                     NC2514.2
119200     MOVE    SPACE TO WRK-XN-00001-1.                             NC2514.2
119300     MOVE    SPACE TO WRK-XN-00001-2.                             NC2514.2
119400 DIV-TEST-F5-18-0.                                                NC2514.2
119500     DIVIDE  100 BY 25COUNT GIVING 25ANS REMAINDER 25REM          NC2514.2
119600             ON SIZE ERROR                                        NC2514.2
119700             MOVE   "A" TO WRK-XN-00001-1                         NC2514.2
119800         NOT ON SIZE ERROR                                        NC2514.2
119900             MOVE   "B" TO WRK-XN-00001-1                         NC2514.2
120000     END-DIVIDE                                                   NC2514.2
120100     MOVE   "C" TO WRK-XN-00001-2.                                NC2514.2
120200     GO TO   DIV-TEST-F5-18-1.                                    NC2514.2
120300 DIV-DELETE-F5-18.                                                NC2514.2
120400     PERFORM DE-LETE.                                             NC2514.2
120500     PERFORM PRINT-DETAIL.                                        NC2514.2
120600     GO TO   DIV-INIT-F5-19.                                      NC2514.2
120700 DIV-TEST-F5-18-1.                                                NC2514.2
120800     MOVE   "DIV-TEST-F5-18-1" TO PAR-NAME.                       NC2514.2
120900     ADD     1 TO REC-CT.                                         NC2514.2
121000     IF      25ANS NOT = 2                                        NC2514.2
121100             MOVE    2 TO CORRECT-N                               NC2514.2
121200             MOVE    25ANS TO COMPUTED-N                          NC2514.2
121300             MOVE   "INVALID QUOTIENT" TO RE-MARK                 NC2514.2
121400             PERFORM FAIL                                         NC2514.2
121500             PERFORM PRINT-DETAIL                                 NC2514.2
121600     ELSE                                                         NC2514.2
121700             PERFORM PASS                                         NC2514.2
121800             PERFORM PRINT-DETAIL.                                NC2514.2
121900 DIV-TEST-F5-18-2.                                                NC2514.2
122000     MOVE   "DIV-TEST-F5-18-2" TO PAR-NAME.                       NC2514.2
122100     ADD     1 TO REC-CT.                                         NC2514.2
122200     IF      25REM NOT = 20                                       NC2514.2
122300             MOVE    25REM TO COMPUTED-N                          NC2514.2
122400             MOVE    20    TO CORRECT-N                           NC2514.2
122500             MOVE   "INVALID REMAINDER" TO RE-MARK                NC2514.2
122600             PERFORM FAIL                                         NC2514.2
122700             PERFORM PRINT-DETAIL                                 NC2514.2
122800     ELSE                                                         NC2514.2
122900             PERFORM PASS                                         NC2514.2
123000             PERFORM PRINT-DETAIL.                                NC2514.2
123100 DIV-TEST-F5-18-3.                                                NC2514.2
123200     MOVE   "DIV-TEST-F5-18-3" TO PAR-NAME.                       NC2514.2
123300     ADD     1 TO REC-CT.                                         NC2514.2
123400     IF      WRK-XN-00001-1 = "B"                                 NC2514.2
123500             PERFORM PASS                                         NC2514.2
123600             PERFORM PRINT-DETAIL                                 NC2514.2
123700     ELSE                                                         NC2514.2
123800             MOVE   "NOT ON SIZE ERROR SHOULD HAVE EXECUTED"      NC2514.2
123900                  TO RE-MARK                                      NC2514.2
124000             MOVE   "B" TO CORRECT-A                              NC2514.2
124100             MOVE    WRK-XN-00001-1 TO COMPUTED-A                 NC2514.2
124200             PERFORM FAIL                                         NC2514.2
124300             PERFORM PRINT-DETAIL.                                NC2514.2
124400 DIV-TEST-F5-18-4.                                                NC2514.2
124500     MOVE   "DIV-TEST-F5-18-4" TO PAR-NAME.                       NC2514.2
124600     ADD     1 TO REC-CT.                                         NC2514.2
124700     IF      WRK-XN-00001-2 = "C"                                 NC2514.2
124800             PERFORM PASS                                         NC2514.2
124900             PERFORM PRINT-DETAIL                                 NC2514.2
125000     ELSE                                                         NC2514.2
125100             MOVE   "SCOPE TERMINATOR IGNORED" TO RE-MARK         NC2514.2
125200             MOVE   "C"    TO CORRECT-A                           NC2514.2
125300             MOVE    WRK-XN-00001-2 TO COMPUTED-A                 NC2514.2
125400             PERFORM FAIL                                         NC2514.2
125500             PERFORM PRINT-DETAIL.                                NC2514.2
125600*                                                                 NC2514.2
125700 DIV-INIT-F5-19.                                                  NC2514.2
125800     MOVE   "VI-82 6.11.4 GR8" TO ANSI-REFERENCE.                 NC2514.2
125900     MOVE   "DIV-TEST-F5-19-0"  TO PAR-NAME.                      NC2514.2
126000     MOVE    ZERO  TO 25COUNT.                                    NC2514.2
126100     MOVE    ZERO  TO 25ANS.                                      NC2514.2
126200     MOVE    ZERO  TO 25REM.                                      NC2514.2
126300     MOVE    1     TO REC-CT.                                     NC2514.2
126400     MOVE    SPACE TO WRK-XN-00001-1.                             NC2514.2
126500     MOVE    SPACE TO WRK-XN-00001-2.                             NC2514.2
126600 DIV-TEST-F5-19-0.                                                NC2514.2
126700     DIVIDE  100 BY 25COUNT GIVING 25ANS REMAINDER 25REM          NC2514.2
126800             ON SIZE ERROR                                        NC2514.2
126900             MOVE   "A" TO WRK-XN-00001-1                         NC2514.2
127000         NOT ON SIZE ERROR                                        NC2514.2
127100             MOVE   "B" TO WRK-XN-00001-1                         NC2514.2
127200     END-DIVIDE                                                   NC2514.2
127300     MOVE   "C" TO WRK-XN-00001-2.                                NC2514.2
127400     GO TO   DIV-TEST-F5-19-1.                                    NC2514.2
127500 DIV-DELETE-F5-19.                                                NC2514.2
127600     PERFORM DE-LETE.                                             NC2514.2
127700     PERFORM PRINT-DETAIL.                                        NC2514.2
127800     GO TO   DIV-INIT-F5-20.                                      NC2514.2
127900 DIV-TEST-F5-19-1.                                                NC2514.2
128000     MOVE   "DIV-TEST-F5-19-1" TO PAR-NAME.                       NC2514.2
128100     ADD     1 TO REC-CT.                                         NC2514.2
128200     IF      25ANS NOT = 0                                        NC2514.2
128300             MOVE    0 TO CORRECT-N                               NC2514.2
128400             MOVE    25ANS TO COMPUTED-N                          NC2514.2
128500             MOVE   "INVALID QUOTIENT" TO RE-MARK                 NC2514.2
128600             PERFORM FAIL                                         NC2514.2
128700             PERFORM PRINT-DETAIL                                 NC2514.2
128800     ELSE                                                         NC2514.2
128900             PERFORM PASS                                         NC2514.2
129000             PERFORM PRINT-DETAIL.                                NC2514.2
129100 DIV-TEST-F5-19-2.                                                NC2514.2
129200     MOVE   "DIV-TEST-F5-19-2" TO PAR-NAME.                       NC2514.2
129300     ADD     1 TO REC-CT.                                         NC2514.2
129400     IF      25REM NOT = ZERO                                     NC2514.2
129500             MOVE    25REM TO COMPUTED-N                          NC2514.2
129600             MOVE    ZERO  TO CORRECT-N                           NC2514.2
129700             MOVE   "INVALID REMAINDER" TO RE-MARK                NC2514.2
129800             PERFORM FAIL                                         NC2514.2
129900             PERFORM PRINT-DETAIL                                 NC2514.2
130000     ELSE                                                         NC2514.2
130100             PERFORM PASS                                         NC2514.2
130200             PERFORM PRINT-DETAIL.                                NC2514.2
130300 DIV-TEST-F5-19-3.                                                NC2514.2
130400     MOVE   "DIV-TEST-F5-19-3" TO PAR-NAME.                       NC2514.2
130500     ADD     1 TO REC-CT.                                         NC2514.2
130600     IF      WRK-XN-00001-1 = "A"                                 NC2514.2
130700             PERFORM PASS                                         NC2514.2
130800             PERFORM PRINT-DETAIL                                 NC2514.2
130900     ELSE                                                         NC2514.2
131000             MOVE   "ON SIZE ERROR SHOULD HAVE EXECUTED"          NC2514.2
131100                  TO RE-MARK                                      NC2514.2
131200             MOVE   "A" TO CORRECT-A                              NC2514.2
131300             MOVE    WRK-XN-00001-1 TO COMPUTED-A                 NC2514.2
131400             PERFORM FAIL                                         NC2514.2
131500             PERFORM PRINT-DETAIL.                                NC2514.2
131600 DIV-TEST-F5-19-4.                                                NC2514.2
131700     MOVE   "DIV-TEST-F5-19-4" TO PAR-NAME.                       NC2514.2
131800     ADD     1 TO REC-CT.                                         NC2514.2
131900     IF      WRK-XN-00001-2 = "C"                                 NC2514.2
132000             PERFORM PASS                                         NC2514.2
132100             PERFORM PRINT-DETAIL                                 NC2514.2
132200     ELSE                                                         NC2514.2
132300             MOVE   "SCOPE TERMINATOR IGNORED" TO RE-MARK         NC2514.2
132400             MOVE   "C"    TO CORRECT-A                           NC2514.2
132500             MOVE    WRK-XN-00001-2 TO COMPUTED-A                 NC2514.2
132600             PERFORM FAIL                                         NC2514.2
132700             PERFORM PRINT-DETAIL.                                NC2514.2
132800*                                                                 NC2514.2
132900 DIV-INIT-F5-20.                                                  NC2514.2
133000     MOVE   "VI-82 6.11.4 GR4" TO ANSI-REFERENCE.                 NC2514.2
133100     MOVE   "DIV-TEST-F5-20-0"  TO PAR-NAME.                      NC2514.2
133200     MOVE ZERO TO 25ANS.                                          NC2514.2
133300     MOVE ZERO TO 25REM.                                          NC2514.2
133400     MOVE ZERO TO WS-REMAINDERS.                                  NC2514.2
133500     MOVE 6    TO 25COUNT.                                        NC2514.2
133600     MOVE 1    TO REC-CT.                                         NC2514.2
133700 DIV-TEST-F5-20-0.                                                NC2514.2
133800     DIVIDE  100 BY 25COUNT GIVING 25ANS                          NC2514.2
133900             REMAINDER WS-REM (25ANS)                             NC2514.2
134000             ON SIZE ERROR                                        NC2514.2
134100             MOVE   "SIZE ERROR SHOULD NOT HAVE OCCURED"          NC2514.2
134200                  TO RE-MARK                                      NC2514.2
134300             PERFORM FAIL                                         NC2514.2
134400             PERFORM PRINT-DETAIL                                 NC2514.2
134500             GO TO   DIV-TEST-F5-20-1.                            NC2514.2
134600     PERFORM PASS.                                                NC2514.2
134700     PERFORM PRINT-DETAIL.                                        NC2514.2
134800     GO TO   DIV-TEST-F5-20-1.                                    NC2514.2
134900 DIV-DELETE-F5-20.                                                NC2514.2
135000     PERFORM DE-LETE.                                             NC2514.2
135100     PERFORM PRINT-DETAIL.                                        NC2514.2
135200     GO TO   CCVS-EXIT.                                           NC2514.2
135300 DIV-TEST-F5-20-1.                                                NC2514.2
135400     MOVE   "DIV-TEST-F5-20-1" TO PAR-NAME.                       NC2514.2
135500     ADD     1 TO REC-CT.                                         NC2514.2
135600     IF      25ANS NOT = 16                                       NC2514.2
135700             MOVE    16 TO CORRECT-N                              NC2514.2
135800             MOVE    25ANS TO COMPUTED-N                          NC2514.2
135900             MOVE   "INVALID QUOTIENT" TO RE-MARK                 NC2514.2
136000             PERFORM FAIL                                         NC2514.2
136100             PERFORM PRINT-DETAIL                                 NC2514.2
136200     ELSE                                                         NC2514.2
136300             PERFORM PASS                                         NC2514.2
136400             PERFORM PRINT-DETAIL.                                NC2514.2
136500 DIV-TEST-F5-20-2.                                                NC2514.2
136600     MOVE   "DIV-TEST-F5-20-2" TO PAR-NAME.                       NC2514.2
136700     ADD     1 TO REC-CT.                                         NC2514.2
136800     IF      WS-REM (25ANS) NOT = 4                               NC2514.2
136900             MOVE    WS-REM (25ANS) TO COMPUTED-N                 NC2514.2
137000             MOVE    4     TO CORRECT-N                           NC2514.2
137100             MOVE   "INVALID REMAINDER" TO RE-MARK                NC2514.2
137200             PERFORM FAIL                                         NC2514.2
137300             PERFORM PRINT-DETAIL                                 NC2514.2
137400             ADD     1 TO REC-CT                                  NC2514.2
137500             MOVE    25ANS TO COMPUTED-N                          NC2514.2
137600             MOVE    16    TO CORRECT-N                           NC2514.2
137700             MOVE   "INVALID SUBSCRIPT FOR REMAINDER" TO RE-MARK  NC2514.2
137800             PERFORM FAIL                                         NC2514.2
137900             PERFORM PRINT-DETAIL                                 NC2514.2
138000     ELSE                                                         NC2514.2
138100             PERFORM PASS                                         NC2514.2
138200             PERFORM PRINT-DETAIL.                                NC2514.2
138300*                                                                 NC2514.2
138400 DIV-INIT-F5-21.                                                  NC2514.2
138500     MOVE "DIV-TEST-F5-21" TO PAR-NAME.                           NC2514.2
138600     MOVE 10.0 TO WRK-DU-2V1-1.                                   NC2514.2
138700     MOVE    ZERO TO REC-CT.                                      NC2514.2
138800 DIVIDE-REMAINDER-TEST-7.                                         NC2514.2
138900     DIVIDE WRK-DU-1V17-1 BY WRK-DU-2V1-1 GIVING WRK-DU-1V5-1     NC2514.2
139000     REMAINDER WRK-NE-1 ON SIZE ERROR GO TO DIV-FAIL-F5-21.       NC2514.2
139100     GO TO DIV-TEST-F5-21-1.                                      NC2514.2
139200 DIV-DELETE-F5-21.                                                NC2514.2
139300     PERFORM DE-LETE.                                             NC2514.2
139400     PERFORM PRINT-DETAIL.                                        NC2514.2
139500     GO TO CCVS-EXIT.                                             NC2514.2
139600 DIV-FAIL-F5-21.                                                  NC2514.2
139700     PERFORM FAIL.                                                NC2514.2
139800     MOVE "SIZE ERROR BAD" TO RE-MARK.                            NC2514.2
139900     PERFORM PRINT-DETAIL.                                        NC2514.2
140000 DIV-TEST-F5-21-1.                                                NC2514.2
140100     MOVE "DIV-TEST-F5-21-1" TO ANSI-REFERENCE.                   NC2514.2
140200     MOVE 1 TO REC-CT.                                            NC2514.2
140300     IF WRK-DU-1V5-1 = 0.31415 PERFORM PASS PERFORM PRINT-DETAIL  NC2514.2
140400     ELSE                                                         NC2514.2
140500     PERFORM FAIL MOVE WRK-DU-1V5-1 TO COMPUTED-N MOVE 0.31415    NC2514.2
140600     TO CORRECT-N PERFORM PRINT-DETAIL.                           NC2514.2
140700     ADD 1 TO REC-CT.                                             NC2514.2
140800 DIV-TEST-F5-21-2.                                                NC2514.2
140900     MOVE "DIV-TEST-F5-21-2" TO ANSI-REFERENCE.                   NC2514.2
141000     IF WRK-NE-1 = ".0000/92653,58979,32"  PERFORM PASS           NC2514.2
141100     PERFORM PRINT-DETAIL ELSE                                    NC2514.2
141200     PERFORM FAIL MOVE WRK-NE-1 TO COMPUTED-A MOVE                NC2514.2
141300     ".0000/92653,58979,32"  TO CORRECT-A PERFORM PRINT-DETAIL.   NC2514.2
141400*                                                                 NC2514.2
141500 CCVS-EXIT SECTION.                                               NC2514.2
141600 CCVS-999999.                                                     NC2514.2
141700     GO TO CLOSE-FILES.                                           NC2514.2
*END-OF,NC251A                                                                  
*HEADER,COBOL,NC252A                                                            
000100 IDENTIFICATION DIVISION.                                         NC2524.2
000200 PROGRAM-ID.                                                      NC2524.2
000300     NC252A.                                                      NC2524.2
000400****************************************************************  NC2524.2
000500*                                                              *  NC2524.2
000600*    VALIDATION FOR:-                                          *  NC2524.2
000700*                                                              *  NC2524.2
000800*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2524.2
000900*                                                              *  NC2524.2
001000*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC2524.2
001100*                                                              *  NC2524.2
001200****************************************************************  NC2524.2
001300*                                                              *  NC2524.2
001400*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  NC2524.2
001500*                                                              *  NC2524.2
001600*        X-55  - SYSTEM PRINTER NAME.                          *  NC2524.2
001700*        X-82  - SOURCE COMPUTER NAME.                         *  NC2524.2
001800*        X-83  - OBJECT COMPUTER NAME.                         *  NC2524.2
001900*                                                              *  NC2524.2
002000****************************************************************  NC2524.2
002100*                                                              *  NC2524.2
002200*    THIS PROGRAM TESTS THE "REDEFINES" AND "RENAMES" CLAUSES. *  NC2524.2
002300*                                                              *  NC2524.2
002400****************************************************************  NC2524.2
002500 ENVIRONMENT DIVISION.                                            NC2524.2
002600 CONFIGURATION SECTION.                                           NC2524.2
002700 SOURCE-COMPUTER.                                                 NC2524.2
002800     XXXXX082.                                                    NC2524.2
002900 OBJECT-COMPUTER.                                                 NC2524.2
003000     XXXXX083.                                                    NC2524.2
003100 INPUT-OUTPUT SECTION.                                            NC2524.2
003200 FILE-CONTROL.                                                    NC2524.2
003300     SELECT PRINT-FILE ASSIGN TO                                  NC2524.2
003400     XXXXX055.                                                    NC2524.2
003500 DATA DIVISION.                                                   NC2524.2
003600 FILE SECTION.                                                    NC2524.2
003700 FD  PRINT-FILE.                                                  NC2524.2
003800 01  PRINT-REC PICTURE X(120).                                    NC2524.2
003900 01  DUMMY-RECORD PICTURE X(120).                                 NC2524.2
004000 WORKING-STORAGE SECTION.                                         NC2524.2
004100 01  WS-REMAINDERS.                                               NC2524.2
004200   03  WS-REM                    PIC 99 OCCURS 20.                NC2524.2
004300 01  WRK-XN-00001-1              PIC X.                           NC2524.2
004400 01  WRK-XN-00001-2              PIC X.                           NC2524.2
004500 01  WS-46.                                                       NC2524.2
004600   03  WS-1-20                   PIC X(20).                       NC2524.2
004700   03  WS-21-40                  PIC X(20).                       NC2524.2
004800   03  WS-41-46                  PIC X(6).                        NC2524.2
004900 77  11A                PICTURE 9999  VALUE 9.                    NC2524.2
005000 77  11B   PICTURE 99; VALUE 8.                                   NC2524.2
005100 77  1111C PICTURE 99 VALUE 9.                                    NC2524.2
005200 77  WRK-DS-02V00                 PICTURE S99.                    NC2524.2
005300     88 TEST-2NUC-COND-99         VALUE 99.                       NC2524.2
005400 77  A99-DS-02V00                 PICTURE S99    VALUE 99.        NC2524.2
005500 77  WRK-DS-18V00                 PICTURE S9(18).                 NC2524.2
005600 77  A18ONES-DS-18V00             PICTURE S9(18)                  NC2524.2
005700                                  VALUE 111111111111111111.       NC2524.2
005800 77  A18TWOS-DS-18V00             PICTURE S9(18)                  NC2524.2
005900                                  VALUE 222222222222222222.       NC2524.2
006000 77  WRK-DS-05V00                 PICTURE S9(5).                  NC2524.2
006100 77  A02TWOS-DU-02V00             PICTURE 99     VALUE 22.        NC2524.2
006200 77  A02TWOS-DS-03V02             PICTURE S999V99 VALUE +022.00.  NC2524.2
006300 77  ATWO-DS-01V00                PICTURE S9     VALUE 2.         NC2524.2
006400 77  AZERO-DS-05V05               PICTURE S9(5)V9(5) VALUE ZERO.  NC2524.2
006500 77  WRK-DS-06V06                 PICTURE S9(6)V9(6).             NC2524.2
006600 77  WRK-DS-0201P                 PICTURE S99P.                   NC2524.2
006700 77  A05ONES-DS-05V00             PICTURE S9(5)  VALUE 11111.     NC2524.2
006800 77  WRK-DS-09V00                 PICTURE S9(9).                  NC2524.2
006900 77  WRK-DS-09V09                 PICTURE S9(9)V9(9).             NC2524.2
007000 77  WRK-DS-18V00-S REDEFINES WRK-DS-09V09                        NC2524.2
007100                                  PICTURE S9(18).                 NC2524.2
007200 77  XRAY                    PICTURE IS X.                        NC2524.2
007300 77  W-1                     PICTURE IS 9.                        NC2524.2
007400 77  W-2                     PICTURE IS 99.                       NC2524.2
007500 77  W-3                     PICTURE IS 999.                      NC2524.2
007600 77  W-5                PICTURE 99  VALUE ZERO.                   NC2524.2
007700 77  W-9                     PICTURE 999.                         NC2524.2
007800 77  W-11               PICTURE S99V9.                            NC2524.2
007900 77  D-1                PICTURE S9V99  VALUE 1.06.                NC2524.2
008000 77  D-7                PICTURE S99V99  VALUE 1.09.               NC2524.2
008100 77  ONE                     PICTURE IS 9      VALUE IS 1.        NC2524.2
008200 77  TWO                     PICTURE IS S9     VALUE IS 2.        NC2524.2
008300 77  THREE                   PICTURE IS S9     VALUE IS 3.        NC2524.2
008400 77  FOUR                    PICTURE IS S9     VALUE IS 4.        NC2524.2
008500 77  FIVE                    PICTURE IS S9     VALUE IS 5.        NC2524.2
008600 77  SIX                     PICTURE IS S9     VALUE IS 6.        NC2524.2
008700 77  SEVEN                   PICTURE IS S9     VALUE IS 7.        NC2524.2
008800 77  EIGHT                   PICTURE IS 9      VALUE IS 8.        NC2524.2
008900 77  NINE                    PICTURE IS S9     VALUE IS 9.        NC2524.2
009000 77  TEN                     PICTURE IS S99    VALUE IS 10.       NC2524.2
009100 77  FIFTEEN                 PICTURE IS S99    VALUE IS 15.       NC2524.2
009200 77  TWENTY                  PICTURE IS S99    VALUE IS 20.       NC2524.2
009300 77  TWENTY-5                PICTURE IS S99    VALUE IS 25.       NC2524.2
009400    1 COMPUTE-DATA.                                               NC2524.2
009500                                                                02NC2524.2
009600     COMPUTE-1                    PICTURE 999V9999  VALUE ZERO.   NC2524.2
009700     2 COMPUTE-1A            PICTURE 9(3)V9(4) VALUE 654.1873.    NC2524.2
009800     2 COMPUTE-2             PICTURE 9999V9    VALUE ZERO.        NC2524.2
009900     02 COMPUTE-3            PICTURE 999V99    VALUE ZERO.        NC2524.2
010000     2 COMPUTE-3A            PICTURE 999V99    VALUE 86.14.       NC2524.2
010100     2 COMPUTE-3B            PICTURE 999V99    VALUE 33.75.       NC2524.2
010200     2 COMPUTE-4             PICTURE 999       VALUE ZERO.        NC2524.2
010300     2 COMPUTE-4A            PICTURE 999       VALUE 124.         NC2524.2
010400     2 COMPUTE-4B            PICTURE 999       VALUE 217.         NC2524.2
010500     2 COMPUTE-5             PICTURE 9999V99   VALUE ZERO.        NC2524.2
010600     02 COMPUTE-5A           PICTURE 999V9     VALUE 11.1.        NC2524.2
010700     2 COMPUTE-6             PICTURE 999V9     VALUE ZERO.        NC2524.2
010800     2 COMPUTE-6A            PICTURE 999V9     VALUE 374.4.       NC2524.2
010900     2 COMPUTE-7             PICTURE 999       VALUE ZERO.        NC2524.2
011000     2 COMPUTE-8             PICTURE 999       VALUE ZERO.        NC2524.2
011100     02 COMPUTE-9            PICTURE 9999      VALUE ZERO.        NC2524.2
011200     2 COMPUTE-10            PICTURE 999V9999  VALUE ZERO.        NC2524.2
011300     2 COMPUTE-11            PICTURE 999V9     VALUE ZERO.        NC2524.2
011400     2 COMPUTE-11A           PICTURE 999V9     VALUE 371.2.       NC2524.2
011500     2 COMPUTE-11B           PICTURE 999V9     VALUE 468.9.       NC2524.2
011600     2 COMPUTE-12            PICTURE 99V99     VALUE ZERO.        NC2524.2
011700     2 COMPUTE-12A           PICTURE 999V9     VALUE 336.4.       NC2524.2
011800     2 COMPUTE-12B           PICTURE 999V9     VALUE 281.7.       NC2524.2
011900 01  RENAMES-DATA.                                                NC2524.2
012000     02  NAME1.                                                   NC2524.2
012100         03 NAME1A PICTURE XX VALUE SPACE.                        NC2524.2
012200         03 NAME1B PICTURE XXX VALUE SPACE.                       NC2524.2
012300     02  NAME2 PICTURE X(10) VALUE SPACE.                         NC2524.2
012400     02  NAME3.                                                   NC2524.2
012500         09 NAME3A PICTURE XXX VALUE SPACE.                       NC2524.2
012600         09 NAME3B PICTURE XX VALUE SPACE.                        NC2524.2
012700 66  RENAME1 RENAMES NAME1 THRU NAME3.                            NC2524.2
012800 66  RENAME2 RENAMES NAME1A THRU NAME1B.                          NC2524.2
012900 66  RENAME3 RENAMES NAME2.                                       NC2524.2
013000 66  RENAME4 RENAMES NAME1.                                       NC2524.2
013100 01  GRP-FOR-RENAMES.                                             NC2524.2
013200     03  SUB-GRP-FOR-RENAMES-1.                                   NC2524.2
013300     05  ELEM-FOR-RENAMES-1      PICTURE X    VALUE "X".          NC2524.2
013400     05  FILLER                  PICTURE XX   VALUE SPACE.        NC2524.2
013500     03  SUB-GRP-FOR-RENAMES-2.                                   NC2524.2
013600     49  ELEM-FOR-RENAMES-2      PICTURE 999  VALUE 123.          NC2524.2
013700     49  FILLER                  PICTURE 9    VALUE ZERO.         NC2524.2
013800     49  ELEM-FOR-RENAMES-3      PICTURE XXXX VALUE ZERO.         NC2524.2
013900     66  RENAMES-TEST-1 RENAMES ELEM-FOR-RENAMES-2.               NC2524.2
014000     66  RENAMES-TEST-2 RENAMES SUB-GRP-FOR-RENAMES-1             NC2524.2
014100         OF GRP-FOR-RENAMES.                                      NC2524.2
014200     66  RENAMES-TEST-3 RENAMES SUB-GRP-FOR-RENAMES-1             NC2524.2
014300         THRU ELEM-FOR-RENAMES-2.                                 NC2524.2
014400     66  RENAMES-TEST-4 RENAMES ELEM-FOR-RENAMES-1                NC2524.2
014500         THRU ELEM-FOR-RENAMES-2 IN GRP-FOR-RENAMES.              NC2524.2
014600 01  T-RENAMES-DATA.                                              NC2524.2
014700     02 TAG-1.                                                    NC2524.2
014800        03 TAG-1A       PICTURE XXXX.                             NC2524.2
014900         03 TAG-1B      PICTURE XXXXXX.                           NC2524.2
015000     02 NAME-2          PICTURE XXXXXXX.                          NC2524.2
015100 66  RENAME-5 RENAMES TAG-1A THRU TAG-1B.                         NC2524.2
015200 66  RENAME-6 RENAMES TAG-1A THRU NAME-2 OF T-RENAMES-DATA.       NC2524.2
015300 01  U-RENAMES-DATA.                                              NC2524.2
015400     02 UNIT-1.                                                   NC2524.2
015500         03 UNIT-1A     PICTURE XXXXXXX VALUE "VERMONT".          NC2524.2
015600         03 UNIT-1B     PICTURE XXXX    VALUE "OHIO".             NC2524.2
015700     02 NAME-2          PICTURE XXXXX   VALUE "MAINE".            NC2524.2
015800 66  RENAME-5 RENAMES UNIT-1A THROUGH UNIT-1B.                    NC2524.2
015900 66  RENAME-6 RENAMES UNIT-1A THRU NAME-2 OF U-RENAMES-DATA.      NC2524.2
016000 01  V-RENAMES-DATA.                                              NC2524.2
016100     02 ITEM-1          PICTURE X(5).                             NC2524.2
016200     02 TABLE-2.                                                  NC2524.2
016300         03 TABLE-ITEM-2 PICTURE XXX OCCURS 5 TIMES.              NC2524.2
016400 66  RENAME-7 RENAMES ITEM-1 THRU TABLE-2.                        NC2524.2
016500 01  W-RENAMES-DATA.                                              NC2524.2
016600     02 WIDGET-1        PICTURE 99V9.                             NC2524.2
016700     02 WIDGET-2        PICTURE ***,***.**.                       NC2524.2
016800     02 WIDGET-3        PICTURE XXXX.                             NC2524.2
016900     02 WIDGET-4        PICTURE 9(4).                             NC2524.2
017000     02 WIDGET-5        PICTURE 9(4).                             NC2524.2
017100 66  RENAME-8  RENAMES WIDGET-1 THRU WIDGET-3.                    NC2524.2
017200 66  RENAME-9  RENAMES WIDGET-3 THRU WIDGET-5.                    NC2524.2
017300 66  RENAME-10 RENAMES WIDGET-4 THRU WIDGET-5.                    NC2524.2
017400 66  RENAME-11 RENAMES WIDGET-2.                                  NC2524.2
017500 66  RENAME-12 RENAMES WIDGET-4.                                  NC2524.2
017600 01  REDEF10.                                                     NC2524.2
017700     02  RDFDATA1                PICTURE X(10) VALUE "ABC98765DE".NC2524.2
017800     02  RDFDATA2                PICTURE 9(4)V99 VALUE 9116.44.   NC2524.2
017900     02  RDFDATA3.                                                NC2524.2
018000         08  RDFDATA4            PICTURE X(6)  VALUE "ALLDON".    NC2524.2
018100         08  RDFDATA5            PICTURE XX99  VALUE "XX66".      NC2524.2
018200     02  RDF3 REDEFINES RDFDATA3.                                 NC2524.2
018300         03  RDF3-4              PICTURE X(8).                    NC2524.2
018400         03  RDF3-5              PIC 99.                          NC2524.2
018500         03  RDF3-5-1 REDEFINES RDF3-5.                           NC2524.2
018600             04  RDF3-5-14  PIC 9.                                NC2524.2
018700             04  RDF3-5-15  PIC 9.                                NC2524.2
018800                 88  HARD  VALUE 0.                               NC2524.2
018900                 88 SOFT  VALUE 1.                                NC2524.2
019000     02  RDFDATA6                PICTURE A(20) VALUE              NC2524.2
019100     "ZYXWVUTSRQPONMLKJIHG".                                      NC2524.2
019200     66  RDF3-5-16 RENAMES RDF3-5.                                NC2524.2
019300 01  REDEF11 REDEFINES REDEF10.                                   NC2524.2
019400     02  RDFDATA7                PICTURE X(20).                   NC2524.2
019500     02  RDF8.                                                    NC2524.2
019600         03   RDFDATA8 OCCURS 36 TIMES PICTURE XX.                NC2524.2
019700     02  RDEF8 REDEFINES RDF8.                                    NC2524.2
019800         03  RDF8-1              PICTURE X(50).                   NC2524.2
019900         03  RDF8-2              PIC X(9).                        NC2524.2
020000         03  RDF8-3 REDEFINES RDF8-2.                             NC2524.2
020100             04  RDF8-4          PIC X(5).                        NC2524.2
020200             04  RDF8-5          PICTURE XX.                      NC2524.2
020300             04  RDF8-6  PIC XX.                                  NC2524.2
020400         03  RDF8-8              PIC X(13).                       NC2524.2
020500     66  RDF8-7 RENAMES RDF8-5 THRU RDF8-6.                       NC2524.2
020600 01  REDEF12 REDEFINES REDEF10.                                   NC2524.2
020700     02  RDFDATA9                PICTURE A(3).                    NC2524.2
020800     02  RDFDATA10     PIC 9(5).                                  NC2524.2
020900     02  RDFDATA11.                                               NC2524.2
021000         03  RDFDATA12.                                           NC2524.2
021100             04  RDFDATA13       PICTURE XX.                      NC2524.2
021200             04  RDFDATA14 OCCURS 6 TIMES PICTURE 9.              NC2524.2
021300         03  RDFDATA15           PICTURE X(8).                    NC2524.2
021400     02  RDFDATA16               PICTURE 99.                      NC2524.2
021500     02  RDFDATA17               PICTURE X(80).                   NC2524.2
021600     02  RDFDATA18               PICTURE X(14).                   NC2524.2
021700 01  GRP-REDEF125   REDEFINES REDEF10.                            NC2524.2
021800     02 AN0020-X-0001             PIC X(26).                      NC2524.2
021900     02 AN0002-O036F-X-0002       PIC XX  OCCURS 36 TIMES.        NC2524.2
022000 01  WRK-DU-05V00-0001            PIC 9(5).                       NC2524.2
022100 01  WRK-DS-05V00-0002            PIC S9(5).                      NC2524.2
022200 01  WRK-CS-05V00-0003            PIC S9(5) COMP.                 NC2524.2
022300 01  WRK-DU-04V02-0004            PIC 9(4)V9(2).                  NC2524.2
022400 01  WRK-DS-04V01-0005            PIC S9(4)V9.                    NC2524.2
022500 01  NE-0008                      PIC $9(4).99-.                  NC2524.2
022600 01  NE-0009                      PIC ***99.                      NC2524.2
022700 01  NE-04V01-0006     PIC ****.9.                                NC2524.2
022800 01  GRP-0010.                                                    NC2524.2
022900     02 WRK-DU-03V00-L-0011       PIC 9(03) SYNC LEFT.            NC2524.2
023000     02 WRK-O005F-0012        OCCURS   5  TIMES.                  NC2524.2
023100        03 WRK-O003F-0013     OCCURS   3  TIMES.                  NC2524.2
023200           05 WRK-DS-03V04-0003F-0014 PIC S9(3)V9999              NC2524.2
023300                                            OCCURS 3 TIMES.       NC2524.2
023400 01  DS-02V00-0001                PIC S99  VALUE  16.             NC2524.2
023500 01  DS-03V00-0002                PIC S999 VALUE  174.            NC2524.2
023600 01  CS-05V00-0003                PIC S9(5) COMP  VALUE 10.       NC2524.2
023700 01    TA--X           PIC 9(5)  COMP VALUE ZERO.                 NC2524.2
023800 01  REDEF13.                                                     NC2524.2
023900     02  FILLER                  PICTURE X(57)   VALUE            NC2524.2
024000     "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA". NC2524.2
024100     02  FILLER                  PICTURE X(57)   VALUE            NC2524.2
024200     "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA". NC2524.2
024300     02  FILLER                  PICTURE X(6)    VALUE "AAAAAA".  NC2524.2
024400 01  MINUS-NAMES.                                                 NC2524.2
024500     02  WHOLE-FIELD              PICTURE S9(18).                 NC2524.2
024600     02  PLUS-NAME1  PICTURE S9(18) VALUE +333333333333333333.    NC2524.2
024700     02  EVEN-NAME1  PICTURE S9(18) VALUE +1.                     NC2524.2
024800     02  PLUS-NAME2  PICTURE S9(18) VALUE +999999999999999999.    NC2524.2
024900     02  ALPHA-LIT                PICTURE X(5)  VALUE SPACE.      NC2524.2
025000     02  SNEG-LIT2                PICTURE S9(5)  VALUE -70718.    NC2524.2
025100 01  TEST-RESULTS.                                                NC2524.2
025200     02 FILLER                   PIC X      VALUE SPACE.          NC2524.2
025300     02 FEATURE                  PIC X(20)  VALUE SPACE.          NC2524.2
025400     02 FILLER                   PIC X      VALUE SPACE.          NC2524.2
025500     02 P-OR-F                   PIC X(5)   VALUE SPACE.          NC2524.2
025600     02 FILLER                   PIC X      VALUE SPACE.          NC2524.2
025700     02  PAR-NAME.                                                NC2524.2
025800       03 FILLER                 PIC X(19)  VALUE SPACE.          NC2524.2
025900       03  PARDOT-X              PIC X      VALUE SPACE.          NC2524.2
026000       03 DOTVALUE               PIC 99     VALUE ZERO.           NC2524.2
026100     02 FILLER                   PIC X(8)   VALUE SPACE.          NC2524.2
026200     02 RE-MARK                  PIC X(61).                       NC2524.2
026300 01  TEST-COMPUTED.                                               NC2524.2
026400     02 FILLER                   PIC X(30)  VALUE SPACE.          NC2524.2
026500     02 FILLER                   PIC X(17)  VALUE                 NC2524.2
026600            "       COMPUTED=".                                   NC2524.2
026700     02 COMPUTED-X.                                               NC2524.2
026800     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          NC2524.2
026900     03 COMPUTED-N               REDEFINES COMPUTED-A             NC2524.2
027000                                 PIC -9(9).9(9).                  NC2524.2
027100     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         NC2524.2
027200     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     NC2524.2
027300     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     NC2524.2
027400     03       CM-18V0 REDEFINES COMPUTED-A.                       NC2524.2
027500         04 COMPUTED-18V0                    PIC -9(18).          NC2524.2
027600         04 FILLER                           PIC X.               NC2524.2
027700     03 FILLER PIC X(50) VALUE SPACE.                             NC2524.2
027800 01  TEST-CORRECT.                                                NC2524.2
027900     02 FILLER PIC X(30) VALUE SPACE.                             NC2524.2
028000     02 FILLER PIC X(17) VALUE "       CORRECT =".                NC2524.2
028100     02 CORRECT-X.                                                NC2524.2
028200     03 CORRECT-A                  PIC X(20) VALUE SPACE.         NC2524.2
028300     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      NC2524.2
028400     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         NC2524.2
028500     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     NC2524.2
028600     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     NC2524.2
028700     03      CR-18V0 REDEFINES CORRECT-A.                         NC2524.2
028800         04 CORRECT-18V0                     PIC -9(18).          NC2524.2
028900         04 FILLER                           PIC X.               NC2524.2
029000     03 FILLER PIC X(2) VALUE SPACE.                              NC2524.2
029100     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     NC2524.2
029200 01  CCVS-C-1.                                                    NC2524.2
029300     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PANC2524.2
029400-    "SS  PARAGRAPH-NAME                                          NC2524.2
029500-    "       REMARKS".                                            NC2524.2
029600     02 FILLER                     PIC X(20)    VALUE SPACE.      NC2524.2
029700 01  CCVS-C-2.                                                    NC2524.2
029800     02 FILLER                     PIC X        VALUE SPACE.      NC2524.2
029900     02 FILLER                     PIC X(6)     VALUE "TESTED".   NC2524.2
030000     02 FILLER                     PIC X(15)    VALUE SPACE.      NC2524.2
030100     02 FILLER                     PIC X(4)     VALUE "FAIL".     NC2524.2
030200     02 FILLER                     PIC X(94)    VALUE SPACE.      NC2524.2
030300 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       NC2524.2
030400 01  REC-CT                        PIC 99       VALUE ZERO.       NC2524.2
030500 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       NC2524.2
030600 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       NC2524.2
030700 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       NC2524.2
030800 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       NC2524.2
030900 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       NC2524.2
031000 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       NC2524.2
031100 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      NC2524.2
031200 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       NC2524.2
031300 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     NC2524.2
031400 01  CCVS-H-1.                                                    NC2524.2
031500     02  FILLER                    PIC X(39)    VALUE SPACES.     NC2524.2
031600     02  FILLER                    PIC X(42)    VALUE             NC2524.2
031700     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 NC2524.2
031800     02  FILLER                    PIC X(39)    VALUE SPACES.     NC2524.2
031900 01  CCVS-H-2A.                                                   NC2524.2
032000   02  FILLER                        PIC X(40)  VALUE SPACE.      NC2524.2
032100   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  NC2524.2
032200   02  FILLER                        PIC XXXX   VALUE             NC2524.2
032300     "4.2 ".                                                      NC2524.2
032400   02  FILLER                        PIC X(28)  VALUE             NC2524.2
032500            " COPY - NOT FOR DISTRIBUTION".                       NC2524.2
032600   02  FILLER                        PIC X(41)  VALUE SPACE.      NC2524.2
032700                                                                  NC2524.2
032800 01  CCVS-H-2B.                                                   NC2524.2
032900   02  FILLER                        PIC X(15)  VALUE             NC2524.2
033000            "TEST RESULT OF ".                                    NC2524.2
033100   02  TEST-ID                       PIC X(9).                    NC2524.2
033200   02  FILLER                        PIC X(4)   VALUE             NC2524.2
033300            " IN ".                                               NC2524.2
033400   02  FILLER                        PIC X(12)  VALUE             NC2524.2
033500     " HIGH       ".                                              NC2524.2
033600   02  FILLER                        PIC X(22)  VALUE             NC2524.2
033700            " LEVEL VALIDATION FOR ".                             NC2524.2
033800   02  FILLER                        PIC X(58)  VALUE             NC2524.2
033900     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2524.2
034000 01  CCVS-H-3.                                                    NC2524.2
034100     02  FILLER                      PIC X(34)  VALUE             NC2524.2
034200            " FOR OFFICIAL USE ONLY    ".                         NC2524.2
034300     02  FILLER                      PIC X(58)  VALUE             NC2524.2
034400     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC2524.2
034500     02  FILLER                      PIC X(28)  VALUE             NC2524.2
034600            "  COPYRIGHT   1985 ".                                NC2524.2
034700 01  CCVS-E-1.                                                    NC2524.2
034800     02 FILLER                       PIC X(52)  VALUE SPACE.      NC2524.2
034900     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              NC2524.2
035000     02 ID-AGAIN                     PIC X(9).                    NC2524.2
035100     02 FILLER                       PIC X(45)  VALUE SPACES.     NC2524.2
035200 01  CCVS-E-2.                                                    NC2524.2
035300     02  FILLER                      PIC X(31)  VALUE SPACE.      NC2524.2
035400     02  FILLER                      PIC X(21)  VALUE SPACE.      NC2524.2
035500     02 CCVS-E-2-2.                                               NC2524.2
035600         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      NC2524.2
035700         03 FILLER                   PIC X      VALUE SPACE.      NC2524.2
035800         03 ENDER-DESC               PIC X(44)  VALUE             NC2524.2
035900            "ERRORS ENCOUNTERED".                                 NC2524.2
036000 01  CCVS-E-3.                                                    NC2524.2
036100     02  FILLER                      PIC X(22)  VALUE             NC2524.2
036200            " FOR OFFICIAL USE ONLY".                             NC2524.2
036300     02  FILLER                      PIC X(12)  VALUE SPACE.      NC2524.2
036400     02  FILLER                      PIC X(58)  VALUE             NC2524.2
036500     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2524.2
036600     02  FILLER                      PIC X(13)  VALUE SPACE.      NC2524.2
036700     02 FILLER                       PIC X(15)  VALUE             NC2524.2
036800             " COPYRIGHT 1985".                                   NC2524.2
036900 01  CCVS-E-4.                                                    NC2524.2
037000     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      NC2524.2
037100     02 FILLER                       PIC X(4)   VALUE " OF ".     NC2524.2
037200     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      NC2524.2
037300     02 FILLER                       PIC X(40)  VALUE             NC2524.2
037400      "  TESTS WERE EXECUTED SUCCESSFULLY".                       NC2524.2
037500 01  XXINFO.                                                      NC2524.2
037600     02 FILLER                       PIC X(19)  VALUE             NC2524.2
037700            "*** INFORMATION ***".                                NC2524.2
037800     02 INFO-TEXT.                                                NC2524.2
037900       04 FILLER                     PIC X(8)   VALUE SPACE.      NC2524.2
038000       04 XXCOMPUTED                 PIC X(20).                   NC2524.2
038100       04 FILLER                     PIC X(5)   VALUE SPACE.      NC2524.2
038200       04 XXCORRECT                  PIC X(20).                   NC2524.2
038300     02 INF-ANSI-REFERENCE           PIC X(48).                   NC2524.2
038400 01  HYPHEN-LINE.                                                 NC2524.2
038500     02 FILLER  PIC IS X VALUE IS SPACE.                          NC2524.2
038600     02 FILLER  PIC IS X(65)    VALUE IS "************************NC2524.2
038700-    "*****************************************".                 NC2524.2
038800     02 FILLER  PIC IS X(54)    VALUE IS "************************NC2524.2
038900-    "******************************".                            NC2524.2
039000 01  CCVS-PGM-ID                     PIC X(9)   VALUE             NC2524.2
039100     "NC252A".                                                    NC2524.2
039200 PROCEDURE DIVISION.                                              NC2524.2
039300 CCVS1 SECTION.                                                   NC2524.2
039400 OPEN-FILES.                                                      NC2524.2
039500     OPEN     OUTPUT PRINT-FILE.                                  NC2524.2
039600     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   NC2524.2
039700     MOVE    SPACE TO TEST-RESULTS.                               NC2524.2
039800     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             NC2524.2
039900     GO TO CCVS1-EXIT.                                            NC2524.2
040000 CLOSE-FILES.                                                     NC2524.2
040100     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   NC2524.2
040200 TERMINATE-CCVS.                                                  NC2524.2
040300     STOP     RUN.                                                NC2524.2
040400 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         NC2524.2
040500 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           NC2524.2
040600 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          NC2524.2
040700 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      NC2524.2
040800     MOVE "****TEST DELETED****" TO RE-MARK.                      NC2524.2
040900 PRINT-DETAIL.                                                    NC2524.2
041000     IF REC-CT NOT EQUAL TO ZERO                                  NC2524.2
041100             MOVE "." TO PARDOT-X                                 NC2524.2
041200             MOVE REC-CT TO DOTVALUE.                             NC2524.2
041300     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      NC2524.2
041400     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               NC2524.2
041500        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 NC2524.2
041600          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 NC2524.2
041700     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              NC2524.2
041800     MOVE SPACE TO CORRECT-X.                                     NC2524.2
041900     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         NC2524.2
042000     MOVE     SPACE TO RE-MARK.                                   NC2524.2
042100 HEAD-ROUTINE.                                                    NC2524.2
042200     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC2524.2
042300     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC2524.2
042400     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC2524.2
042500     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC2524.2
042600 COLUMN-NAMES-ROUTINE.                                            NC2524.2
042700     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2524.2
042800     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2524.2
042900     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        NC2524.2
043000 END-ROUTINE.                                                     NC2524.2
043100     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.NC2524.2
043200 END-RTN-EXIT.                                                    NC2524.2
043300     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2524.2
043400 END-ROUTINE-1.                                                   NC2524.2
043500      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      NC2524.2
043600      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               NC2524.2
043700      ADD PASS-COUNTER TO ERROR-HOLD.                             NC2524.2
043800*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   NC2524.2
043900      MOVE PASS-COUNTER TO CCVS-E-4-1.                            NC2524.2
044000      MOVE ERROR-HOLD TO CCVS-E-4-2.                              NC2524.2
044100      MOVE CCVS-E-4 TO CCVS-E-2-2.                                NC2524.2
044200      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           NC2524.2
044300  END-ROUTINE-12.                                                 NC2524.2
044400      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        NC2524.2
044500     IF       ERROR-COUNTER IS EQUAL TO ZERO                      NC2524.2
044600         MOVE "NO " TO ERROR-TOTAL                                NC2524.2
044700         ELSE                                                     NC2524.2
044800         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       NC2524.2
044900     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           NC2524.2
045000     PERFORM WRITE-LINE.                                          NC2524.2
045100 END-ROUTINE-13.                                                  NC2524.2
045200     IF DELETE-COUNTER IS EQUAL TO ZERO                           NC2524.2
045300         MOVE "NO " TO ERROR-TOTAL  ELSE                          NC2524.2
045400         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      NC2524.2
045500     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   NC2524.2
045600     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2524.2
045700      IF   INSPECT-COUNTER EQUAL TO ZERO                          NC2524.2
045800          MOVE "NO " TO ERROR-TOTAL                               NC2524.2
045900      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   NC2524.2
046000      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            NC2524.2
046100      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          NC2524.2
046200     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2524.2
046300 WRITE-LINE.                                                      NC2524.2
046400     ADD 1 TO RECORD-COUNT.                                       NC2524.2
046500Y    IF RECORD-COUNT GREATER 50                                   NC2524.2
046600Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          NC2524.2
046700Y        MOVE SPACE TO DUMMY-RECORD                               NC2524.2
046800Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  NC2524.2
046900Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             NC2524.2
047000Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     NC2524.2
047100Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          NC2524.2
047200Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          NC2524.2
047300Y        MOVE ZERO TO RECORD-COUNT.                               NC2524.2
047400     PERFORM WRT-LN.                                              NC2524.2
047500 WRT-LN.                                                          NC2524.2
047600     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               NC2524.2
047700     MOVE SPACE TO DUMMY-RECORD.                                  NC2524.2
047800 BLANK-LINE-PRINT.                                                NC2524.2
047900     PERFORM WRT-LN.                                              NC2524.2
048000 FAIL-ROUTINE.                                                    NC2524.2
048100     IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. NC2524.2
048200     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.NC2524.2
048300     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC2524.2
048400     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   NC2524.2
048500     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2524.2
048600     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC2524.2
048700     GO TO  FAIL-ROUTINE-EX.                                      NC2524.2
048800 FAIL-ROUTINE-WRITE.                                              NC2524.2
048900     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         NC2524.2
049000     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 NC2524.2
049100     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. NC2524.2
049200     MOVE   SPACES TO COR-ANSI-REFERENCE.                         NC2524.2
049300 FAIL-ROUTINE-EX. EXIT.                                           NC2524.2
049400 BAIL-OUT.                                                        NC2524.2
049500     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   NC2524.2
049600     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           NC2524.2
049700 BAIL-OUT-WRITE.                                                  NC2524.2
049800     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  NC2524.2
049900     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC2524.2
050000     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2524.2
050100     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC2524.2
050200 BAIL-OUT-EX. EXIT.                                               NC2524.2
050300 CCVS1-EXIT.                                                      NC2524.2
050400     EXIT.                                                        NC2524.2
050500 SECT-NC252A-001 SECTION.                                         NC2524.2
050600 RDF-INIT.                                                        NC2524.2
050700     MOVE "REDEFINES " TO FEATURE.                                NC2524.2
050800 RDF-TEST-1.                                                      NC2524.2
050900     IF HARD                                                      NC2524.2
051000         MOVE RDF3-5-15 TO COMPUTED-A                             NC2524.2
051100         MOVE 6 TO CORRECT-A                                      NC2524.2
051200         PERFORM FAIL                                             NC2524.2
051300         GO TO RDF-WRITE-1.                                       NC2524.2
051400*    NOTE 88 LEVEL CONDITION TEST ON REDEFINED FIELD.             NC2524.2
051500     PERFORM PASS.                                                NC2524.2
051600     GO TO RDF-WRITE-1.                                           NC2524.2
051700 RDF-DELETE-1.                                                    NC2524.2
051800     PERFORM DE-LETE.                                             NC2524.2
051900 RDF-WRITE-1.                                                     NC2524.2
052000     MOVE "RDF-TEST-1 " TO PAR-NAME.                              NC2524.2
052100     PERFORM PRINT-DETAIL.                                        NC2524.2
052200 RDF-TEST-2.                                                      NC2524.2
052300     IF RDF3-5-16 EQUAL TO 66                                     NC2524.2
052400         PERFORM PASS                                             NC2524.2
052500         GO TO RDF-WRITE-2.                                       NC2524.2
052600*    NOTE USING A RENAMES DATANAME THAT IS ALSO REDEFINED.        NC2524.2
052700     MOVE RDF3-5-16 TO COMPUTED-A.                                NC2524.2
052800     MOVE 66 TO CORRECT-A.                                        NC2524.2
052900     PERFORM FAIL.                                                NC2524.2
053000     GO TO RDF-WRITE-2.                                           NC2524.2
053100 RDF-DELETE-2.                                                    NC2524.2
053200     PERFORM DE-LETE.                                             NC2524.2
053300 RDF-WRITE-2.                                                     NC2524.2
053400     MOVE "RDF-TEST-2 " TO PAR-NAME.                              NC2524.2
053500     PERFORM PRINT-DETAIL.                                        NC2524.2
053600 RDF-TEST-003.                                                    NC2524.2
053700     IF   AN0002-O036F-X-0002  (8)  EQUAL TO  "LK"                NC2524.2
053800          PERFORM  PASS                                           NC2524.2
053900          GO TO   RDF-WRITE-003.                                  NC2524.2
054000*                                                                 NC2524.2
054100*    NOTE  REFERENCING    SUBSCRIPTED DATA ITEM WHICH IS          NC2524.2
054200*          SUBORDINATE TO A REDEFINES CLAUSE.                     NC2524.2
054300*                                                                 NC2524.2
054400     MOVE  AN0002-O036F-X-0002 (8) TO  COMPUTED-A.                NC2524.2
054500     MOVE   "LK"  TO CORRECT-A.                                   NC2524.2
054600     PERFORM  FAIL.                                               NC2524.2
054700     GO TO    RDF-WRITE-003.                                      NC2524.2
054800 RDF-DELETE-003.                                                  NC2524.2
054900     PERFORM  DE-LETE.                                            NC2524.2
055000 RDF-WRITE-003.                                                   NC2524.2
055100     MOVE     "RDF-TEST-003"  TO  PAR-NAME.                       NC2524.2
055200     PERFORM  PRINT-DETAIL.                                       NC2524.2
055300 RDF-TEST-4.                                                      NC2524.2
055400     IF RDFDATA7 EQUAL TO "ABC98765DE911644ALLD"                  NC2524.2
055500         PERFORM PASS                                             NC2524.2
055600         GO TO RDF-WRITE-4.                                       NC2524.2
055700*    NOTE THIS IS THE FIRST REFERENCE TO THESE REDEFINED          NC2524.2
055800*        DATANAMES, SO, THE FIELDS CONTAIN THE WORKING-STORAGE    NC2524.2
055900*        ASSIGNED VALUES.                                         NC2524.2
056000     MOVE RDFDATA7 TO COMPUTED-A.                                 NC2524.2
056100     MOVE "ABC98765DE911644ALLD" TO CORRECT-A.                    NC2524.2
056200     PERFORM FAIL.                                                NC2524.2
056300     GO TO RDF-WRITE-4.                                           NC2524.2
056400 RDF-DELETE-4.                                                    NC2524.2
056500     PERFORM DE-LETE.                                             NC2524.2
056600 RDF-WRITE-4.                                                     NC2524.2
056700     MOVE "RDF-TEST-4 " TO PAR-NAME.                              NC2524.2
056800     PERFORM PRINT-DETAIL.                                        NC2524.2
056900 RDF-TEST-5.                                                      NC2524.2
057000     IF RDFDATA8 (13) EQUAL TO "HG"                               NC2524.2
057100         PERFORM PASS                                             NC2524.2
057200         GO TO RDF-WRITE-5.                                       NC2524.2
057300     MOVE "HG" TO CORRECT-A.                                      NC2524.2
057400     MOVE RDFDATA8 (13) TO COMPUTED-A.                            NC2524.2
057500     PERFORM FAIL.                                                NC2524.2
057600     GO TO RDF-WRITE-5.                                           NC2524.2
057700 RDF-DELETE-5.                                                    NC2524.2
057800     PERFORM DE-LETE.                                             NC2524.2
057900 RDF-WRITE-5.                                                     NC2524.2
058000     MOVE "RDF-TEST-5 " TO PAR-NAME.                              NC2524.2
058100     PERFORM PRINT-DETAIL.                                        NC2524.2
058200 RDF-TEST-6.                                                      NC2524.2
058300     IF RDFDATA2 EQUAL TO 9116.44                                 NC2524.2
058400         PERFORM PASS                                             NC2524.2
058500         GO TO RDF-WRITE-6.                                       NC2524.2
058600     MOVE 9116.44 TO COMPUTED-N.                                  NC2524.2
058700     MOVE RDFDATA2 TO CORRECT-N.                                  NC2524.2
058800     PERFORM FAIL.                                                NC2524.2
058900     GO TO RDF-WRITE-6.                                           NC2524.2
059000 RDF-DELETE-6.                                                    NC2524.2
059100     PERFORM DE-LETE.                                             NC2524.2
059200 RDF-WRITE-6.                                                     NC2524.2
059300     MOVE "RDF-TEST-6 " TO PAR-NAME.                              NC2524.2
059400     PERFORM PRINT-DETAIL.                                        NC2524.2
059500 RDF-TEST-7.                                                      NC2524.2
059600     IF RDFDATA16 EQUAL TO 66                                     NC2524.2
059700         PERFORM PASS                                             NC2524.2
059800         GO TO RDF-WRITE-7.                                       NC2524.2
059900     MOVE RDFDATA16 TO COMPUTED-A.                                NC2524.2
060000     MOVE 66 TO CORRECT-A.                                        NC2524.2
060100     PERFORM FAIL.                                                NC2524.2
060200     GO TO RDF-WRITE-7.                                           NC2524.2
060300 RDF-DELETE-7.                                                    NC2524.2
060400     PERFORM DE-LETE.                                             NC2524.2
060500 RDF-WRITE-7.                                                     NC2524.2
060600     MOVE "RDF-TEST-7 " TO PAR-NAME.                              NC2524.2
060700     PERFORM PRINT-DETAIL.                                        NC2524.2
060800 RDF-TEST-8.                                                      NC2524.2
060900     MOVE SPACE TO REDEF12.                                       NC2524.2
061000     IF REDEF11 EQUAL TO SPACE                                    NC2524.2
061100         PERFORM PASS                                             NC2524.2
061200         GO TO RDF-WRITE-8.                                       NC2524.2
061300     MOVE "SPACE EXPECTED " TO CORRECT-A.                         NC2524.2
061400     MOVE "NON BLANK CHARACTERS" TO COMPUTED-A.                   NC2524.2
061500     MOVE "REDEF11 CONTAINS NON BLANKS" TO RE-MARK.               NC2524.2
061600     PERFORM FAIL.                                                NC2524.2
061700     GO TO RDF-WRITE-8.                                           NC2524.2
061800 RDF-DELETE-8.                                                    NC2524.2
061900     PERFORM DE-LETE.                                             NC2524.2
062000 RDF-WRITE-8.                                                     NC2524.2
062100     MOVE "RDF-TEST-8 " TO PAR-NAME.                              NC2524.2
062200     PERFORM PRINT-DETAIL.                                        NC2524.2
062300 RDF-TEST-9.                                                      NC2524.2
062400     MOVE ZERO TO REDEF12.                                        NC2524.2
062500     MOVE SPACE TO REDEF11.                                       NC2524.2
062600*    NOTE  CHECKS RDFDATA18 WHICH SHOULD NOT BE DISTURBED BY THE  NC2524.2
062700*        MOVE SPACE STATEMENT TO A SHORTER REDEFINED AREA.        NC2524.2
062800     IF RDFDATA18 EQUAL TO ZERO                                   NC2524.2
062900         PERFORM PASS                                             NC2524.2
063000         GO TO RDF-WRITE-9.                                       NC2524.2
063100     MOVE "00000000000000" TO CORRECT-A.                          NC2524.2
063200     MOVE RDFDATA18 TO COMPUTED-A.                                NC2524.2
063300     PERFORM FAIL.                                                NC2524.2
063400     GO TO RDF-WRITE-9.                                           NC2524.2
063500 RDF-DELETE-9.                                                    NC2524.2
063600     PERFORM DE-LETE.                                             NC2524.2
063700 RDF-WRITE-9.                                                     NC2524.2
063800     MOVE "RDF-TEST-9 " TO PAR-NAME.                              NC2524.2
063900     PERFORM PRINT-DETAIL.                                        NC2524.2
064000 RDF-TEST-10.                                                     NC2524.2
064100     MOVE ZERO TO REDEF12.                                        NC2524.2
064200     MOVE "MOVING DATA TO A REDEFINED FIELD CAN BE RISKY "        NC2524.2
064300         TO REDEF10.                                              NC2524.2
064400     IF RDFDATA8 (14) EQUAL TO "00"                               NC2524.2
064500         PERFORM PASS                                             NC2524.2
064600         GO TO RDF-WRITE-10.                                      NC2524.2
064700     MOVE 00 TO CORRECT-A.                                        NC2524.2
064800     MOVE RDFDATA8 (14) TO COMPUTED-A.                            NC2524.2
064900     PERFORM FAIL.                                                NC2524.2
065000     GO TO RDF-WRITE-10.                                          NC2524.2
065100 RDF-DELETE-10.                                                   NC2524.2
065200     PERFORM DE-LETE.                                             NC2524.2
065300 RDF-WRITE-10.                                                    NC2524.2
065400     MOVE "RDF-TEST-10 " TO PAR-NAME.                             NC2524.2
065500     PERFORM PRINT-DETAIL.                                        NC2524.2
065600 RDF-INIT-11.                                                     NC2524.2
065700     MOVE REDEF13 TO REDEF12.                                     NC2524.2
065800     MOVE "RDF-TEST-11 " TO PAR-NAME.                             NC2524.2
065900 RDF-TEST-11.                                                     NC2524.2
066000     IF REDEF10 EQUAL TO                                          NC2524.2
066100         "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"         NC2524.2
066200         PERFORM PASS                                             NC2524.2
066300         PERFORM PRINT-DETAIL                                     NC2524.2
066400         GO TO   RDF-TEST-12.                                     NC2524.2
066500     MOVE    1 TO REC-CT.                                         NC2524.2
066600     MOVE    REDEF10 TO WS-46.                                    NC2524.2
066700     MOVE   "AAAAAAAAAAAAAAAAAAAA" TO CORRECT-A.                  NC2524.2
066800     MOVE    WS-1-20 TO COMPUTED-A.                               NC2524.2
066900     MOVE   "FIELDS DIDNT COMPARE EQUAL " TO RE-MARK.             NC2524.2
067000     PERFORM FAIL.                                                NC2524.2
067100     PERFORM PRINT-DETAIL.                                        NC2524.2
067200     ADD     1 TO REC-CT.                                         NC2524.2
067300     MOVE   "AAAAAAAAAAAAAAAAAAAA" TO CORRECT-A.                  NC2524.2
067400     MOVE    WS-21-40 TO COMPUTED-A.                              NC2524.2
067500     MOVE   "FIELDS DIDNT COMPARE EQUAL " TO RE-MARK.             NC2524.2
067600     PERFORM FAIL.                                                NC2524.2
067700     PERFORM PRINT-DETAIL.                                        NC2524.2
067800     ADD     1 TO REC-CT.                                         NC2524.2
067900     MOVE   "AAAAAA"  TO CORRECT-A.                               NC2524.2
068000     MOVE    WS-41-46 TO COMPUTED-A.                              NC2524.2
068100     MOVE   "FIELDS DIDNT COMPARE EQUAL " TO RE-MARK.             NC2524.2
068200     PERFORM FAIL.                                                NC2524.2
068300     PERFORM PRINT-DETAIL.                                        NC2524.2
068400     GO TO   RDF-TEST-12.                                         NC2524.2
068500 RDF-DELETE-11.                                                   NC2524.2
068600     PERFORM DE-LETE.                                             NC2524.2
068700 RDF-TEST-12.                                                     NC2524.2
068800     MOVE 11 TO RDFDATA16.                                        NC2524.2
068900*    NOTE 88 LEVEL TEST ON REDEFINED AREA.                        NC2524.2
069000     IF  SOFT                                                     NC2524.2
069100         PERFORM PASS                                             NC2524.2
069200     ELSE                                                         NC2524.2
069300         MOVE "CONDITION-NAME TEST" TO RE-MARK                    NC2524.2
069400         PERFORM FAIL.                                            NC2524.2
069500     GO TO RDF-WRITE-12.                                          NC2524.2
069600 RDF-DELETE-12.                                                   NC2524.2
069700     PERFORM DE-LETE.                                             NC2524.2
069800 RDF-WRITE-12.                                                    NC2524.2
069900     MOVE "RDF-TEST-12" TO PAR-NAME.                              NC2524.2
070000     PERFORM PRINT-DETAIL.                                        NC2524.2
070100 RDF-TEST-13.                                                     NC2524.2
070200     MOVE REDEF13 TO REDEF12.                                     NC2524.2
070300     MOVE SPACE TO REDEF10.                                       NC2524.2
070400     IF RDF8-7 EQUAL TO SPACE                                     NC2524.2
070500         MOVE RDF8-7 TO COMPUTED-A                                NC2524.2
070600         MOVE "AAAA" TO CORRECT-A                                 NC2524.2
070700         PERFORM FAIL                                             NC2524.2
070800         GO TO RDF-WRITE-13.                                      NC2524.2
070900     PERFORM PASS.                                                NC2524.2
071000     GO TO RDF-WRITE-13.                                          NC2524.2
071100 RDF-DELETE-13.                                                   NC2524.2
071200     PERFORM DE-LETE.                                             NC2524.2
071300 RDF-WRITE-13.                                                    NC2524.2
071400     MOVE "RDF-TEST-13 " TO PAR-NAME.                             NC2524.2
071500     PERFORM PRINT-DETAIL.                                        NC2524.2
071600 RDF-TEST-14.                                                     NC2524.2
071700     MOVE SPACE TO REDEF12.                                       NC2524.2
071800     MOVE REDEF13 TO REDEF10.                                     NC2524.2
071900     IF RDF8-3 EQUAL TO "AAAAAAAAA"                               NC2524.2
072000         MOVE RDF8-3 TO COMPUTED-A                                NC2524.2
072100         MOVE SPACE TO CORRECT-A                                  NC2524.2
072200         PERFORM FAIL                                             NC2524.2
072300         GO TO RDF-WRITE-14.                                      NC2524.2
072400     PERFORM PASS.                                                NC2524.2
072500     GO TO RDF-WRITE-14.                                          NC2524.2
072600 RDF-DELETE-14.                                                   NC2524.2
072700     PERFORM DE-LETE.                                             NC2524.2
072800 RDF-WRITE-14.                                                    NC2524.2
072900     MOVE "RDF-TEST-14 " TO PAR-NAME.                             NC2524.2
073000     PERFORM PRINT-DETAIL.                                        NC2524.2
073100 RNM-INIT.                                                        NC2524.2
073200     PERFORM END-ROUTINE.                                         NC2524.2
073300     MOVE "RENAMES" TO FEATURE.                                   NC2524.2
073400 RENAM-TEST-1.                                                    NC2524.2
073500     MOVE "AB" TO NAME1A.                                         NC2524.2
073600     MOVE "CD" TO NAME1B.                                         NC2524.2
073700     IF RENAME4 EQUAL TO "ABCD "                                  NC2524.2
073800              PERFORM PASS                                        NC2524.2
073900              GO TO RNM-WRITE-1.                                  NC2524.2
074000     MOVE RENAME4 TO COMPUTED-A.                                  NC2524.2
074100     MOVE "ABCD" TO CORRECT-A.                                    NC2524.2
074200*    NOTE  CORRECT ANSWER IS   ABCD-BLANK.                        NC2524.2
074300     PERFORM FAIL.                                                NC2524.2
074400     GO TO RNM-WRITE-1.                                           NC2524.2
074500 RNM-DELETE-1.                                                    NC2524.2
074600     PERFORM DE-LETE.                                             NC2524.2
074700 RNM-WRITE-1.                                                     NC2524.2
074800     MOVE "RENAM-TEST-1" TO PAR-NAME.                             NC2524.2
074900     PERFORM PRINT-DETAIL.                                        NC2524.2
075000 RENAM-TEST-2.                                                    NC2524.2
075100     MOVE ALL "A" TO RENAMES-DATA.                                NC2524.2
075200     IF RENAME3 EQUAL TO "AAAAAAAAAA"                             NC2524.2
075300              PERFORM PASS                                        NC2524.2
075400              GO TO RNM-WRITE-2.                                  NC2524.2
075500     MOVE RENAME3 TO COMPUTED-A.                                  NC2524.2
075600     MOVE "AAAAAAAAAA" TO CORRECT-A.                              NC2524.2
075700     PERFORM FAIL.                                                NC2524.2
075800     GO TO RNM-WRITE-2.                                           NC2524.2
075900 RNM-DELETE-2.                                                    NC2524.2
076000     PERFORM DE-LETE.                                             NC2524.2
076100 RNM-WRITE-2.                                                     NC2524.2
076200     MOVE "RENAM-TEST-2" TO PAR-NAME.                             NC2524.2
076300     PERFORM PRINT-DETAIL.                                        NC2524.2
076400 RENAM-TEST-3.                                                    NC2524.2
076500     MOVE ALL "A" TO RENAMES-DATA.                                NC2524.2
076600     MOVE ALL "X" TO RENAME1.                                     NC2524.2
076700     IF NAME1 NOT EQUAL TO "XXXXX" GO TO RNM-FAIL-3.              NC2524.2
076800     IF NAME2 NOT EQUAL TO "XXXXXXXXXX" GO TO RNM-FAIL-3.         NC2524.2
076900     IF NAME3 NOT EQUAL TO "XXXXX" GO TO RNM-FAIL-3.              NC2524.2
077000     PERFORM PASS.                                                NC2524.2
077100     GO TO RNM-WRITE-3.                                           NC2524.2
077200 RNM-DELETE-3.                                                    NC2524.2
077300     PERFORM DE-LETE.                                             NC2524.2
077400     GO TO RNM-WRITE-3.                                           NC2524.2
077500 RNM-FAIL-3.                                                      NC2524.2
077600     MOVE RENAMES-DATA TO COMPUTED-A.                             NC2524.2
077700     MOVE "XXXXXXXXXXXXXXXXXXXX" TO CORRECT-A.                    NC2524.2
077800     PERFORM FAIL.                                                NC2524.2
077900 RNM-WRITE-3.                                                     NC2524.2
078000     MOVE "RENAM-TEST-3" TO PAR-NAME.                             NC2524.2
078100     PERFORM PRINT-DETAIL.                                        NC2524.2
078200 RENAM-TEST-4.                                                    NC2524.2
078300     IF RENAMES-TEST-1 EQUAL TO 123                               NC2524.2
078400         PERFORM PASS                                             NC2524.2
078500         GO TO RENAM-WRITE-4.                                     NC2524.2
078600     MOVE RENAMES-TEST-1 TO COMPUTED-A.                           NC2524.2
078700     MOVE 123 TO CORRECT-A.                                       NC2524.2
078800     PERFORM FAIL.                                                NC2524.2
078900     GO TO RENAM-WRITE-4.                                         NC2524.2
079000 RENAM-DELETE-4.                                                  NC2524.2
079100     PERFORM DE-LETE.                                             NC2524.2
079200 RENAM-WRITE-4.                                                   NC2524.2
079300     MOVE "RENAM-TEST-4" TO PAR-NAME.                             NC2524.2
079400     PERFORM PRINT-DETAIL.                                        NC2524.2
079500 RENAM-TEST-5.                                                    NC2524.2
079600     IF RENAMES-TEST-3 EQUAL TO "X  123"                          NC2524.2
079700         PERFORM PASS                                             NC2524.2
079800         GO TO RENAM-WRITE-5.                                     NC2524.2
079900     MOVE RENAMES-TEST-3 TO COMPUTED-A.                           NC2524.2
080000     MOVE "X  123" TO CORRECT-A.                                  NC2524.2
080100     PERFORM FAIL.                                                NC2524.2
080200     GO TO RENAM-WRITE-5.                                         NC2524.2
080300 RENAM-DELETE-5.                                                  NC2524.2
080400     PERFORM DE-LETE.                                             NC2524.2
080500 RENAM-WRITE-5.                                                   NC2524.2
080600     MOVE "RENAM-TEST-5" TO PAR-NAME.                             NC2524.2
080700     PERFORM PRINT-DETAIL.                                        NC2524.2
080800 RENAM-TEST-6.                                                    NC2524.2
080900     IF RENAMES-TEST-4 EQUAL TO "X  123"                          NC2524.2
081000         PERFORM PASS                                             NC2524.2
081100         GO TO RENAM-WRITE-6.                                     NC2524.2
081200     MOVE RENAMES-TEST-4 TO COMPUTED-A.                           NC2524.2
081300     MOVE "X  123" TO CORRECT-A.                                  NC2524.2
081400     PERFORM FAIL.                                                NC2524.2
081500     GO TO RENAM-WRITE-6.                                         NC2524.2
081600 RENAM-DELETE-6.                                                  NC2524.2
081700     PERFORM DE-LETE.                                             NC2524.2
081800 RENAM-WRITE-6.                                                   NC2524.2
081900     MOVE "RENAM-TEST-6" TO PAR-NAME.                             NC2524.2
082000     PERFORM PRINT-DETAIL.                                        NC2524.2
082100 RENAM-TEST-7.                                                    NC2524.2
082200     IF RENAMES-TEST-2 EQUAL TO "X  "                             NC2524.2
082300         PERFORM PASS                                             NC2524.2
082400         GO TO RENAM-WRITE-7.                                     NC2524.2
082500     MOVE RENAMES-TEST-2 TO COMPUTED-A.                           NC2524.2
082600     MOVE "X  " TO CORRECT-A.                                     NC2524.2
082700     PERFORM FAIL.                                                NC2524.2
082800     GO TO RENAM-WRITE-7.                                         NC2524.2
082900 RENAM-DELETE-7.                                                  NC2524.2
083000     PERFORM DE-LETE.                                             NC2524.2
083100 RENAM-WRITE-7.                                                   NC2524.2
083200     MOVE "RENAM-TEST-7" TO PAR-NAME.                             NC2524.2
083300     PERFORM PRINT-DETAIL.                                        NC2524.2
083400 RENAM-INIT-C.                                                    NC2524.2
083500     MOVE     "QUALIFIED RENAMES" TO FEATURE.                     NC2524.2
083600 RENAM-TEST-8.                                                    NC2524.2
083700     MOVE     "IOWA" TO TAG-1A.                                   NC2524.2
083800     MOVE     "OREGON" TO TAG-1B.                                 NC2524.2
083900     MOVE     "CALIFORNIA" TO RENAME-5 OF T-RENAMES-DATA.         NC2524.2
084000     IF       TAG-1 EQUAL TO "CALIFORNIA"                         NC2524.2
084100              PERFORM PASS GO TO RENAM-WRITE-8.                   NC2524.2
084200     GO       TO RENAM-FAIL-8.                                    NC2524.2
084300 RENAM-DELETE-8.                                                  NC2524.2
084400     PERFORM  DE-LETE.                                            NC2524.2
084500     GO       TO RENAM-WRITE-8.                                   NC2524.2
084600 RENAM-FAIL-8.                                                    NC2524.2
084700     PERFORM FAIL.                                                NC2524.2
084800     MOVE     TAG-1 TO COMPUTED-A.                                NC2524.2
084900     MOVE     "CALIFORNIA" TO CORRECT-A.                          NC2524.2
085000 RENAM-WRITE-8.                                                   NC2524.2
085100     MOVE     "RENAM-TEST-8 " TO PAR-NAME.                        NC2524.2
085200     PERFORM  PRINT-DETAIL.                                       NC2524.2
085300 RENAM-TEST-9.                                                    NC2524.2
085400     IF       UNIT-1 EQUAL TO "VERMONTOHIO"                       NC2524.2
085500              PERFORM PASS GO TO RENAM-WRITE-9.                   NC2524.2
085600*        NOTE THIS TEST FURTHER CHECKS THE RESULTS OF             NC2524.2
085700*             THE PREVIOUS TEST - THIS ITEM SHOULD BE UNCHANGED.  NC2524.2
085800     GO       TO RENAM-FAIL-9.                                    NC2524.2
085900 RENAM-DELETE-9.                                                  NC2524.2
086000     PERFORM  DE-LETE.                                            NC2524.2
086100     GO       TO RENAM-WRITE-9.                                   NC2524.2
086200 RENAM-FAIL-9.                                                    NC2524.2
086300     PERFORM FAIL.                                                NC2524.2
086400     MOVE     UNIT-1 TO COMPUTED-A.                               NC2524.2
086500     MOVE     "VERMONTOHIO" TO CORRECT-A.                         NC2524.2
086600 RENAM-WRITE-9.                                                   NC2524.2
086700     MOVE     "RENAM-TEST-9 " TO PAR-NAME.                        NC2524.2
086800     PERFORM  PRINT-DETAIL.                                       NC2524.2
086900 RENAM-TEST-10.                                                   NC2524.2
087000     MOVE     "IOWAOREGONFLORIDA" TO T-RENAMES-DATA.              NC2524.2
087100     IF       RENAME-6 IN T-RENAMES-DATA EQUAL TO                 NC2524.2
087200              "IOWAOREGONFLORIDA"                                 NC2524.2
087300              PERFORM PASS GO TO RENAM-WRITE-10.                  NC2524.2
087400     GO       TO RENAM-FAIL-10.                                   NC2524.2
087500 RENAM-DELETE-10.                                                 NC2524.2
087600     PERFORM  DE-LETE.                                            NC2524.2
087700     GO       TO RENAM-WRITE-10.                                  NC2524.2
087800 RENAM-FAIL-10.                                                   NC2524.2
087900     PERFORM FAIL.                                                NC2524.2
088000     MOVE    RENAME-6 IN T-RENAMES-DATA TO COMPUTED-A.            NC2524.2
088100     MOVE   "IOWAOREGONFLORIDA" TO CORRECT-A.                     NC2524.2
088200 RENAM-WRITE-10.                                                  NC2524.2
088300     MOVE   "RENAM-TEST-10" TO PAR-NAME.                          NC2524.2
088400     PERFORM PRINT-DETAIL.                                        NC2524.2
088500 RENAM-TEST-11.                                                   NC2524.2
088600     MOVE   "BOSTO" TO ITEM-1.                                    NC2524.2
088700     MOVE   "N M" TO TABLE-ITEM-2 (1).                            NC2524.2
088800     MOVE   "ASS" TO TABLE-ITEM-2 (2).                            NC2524.2
088900     MOVE   "ACH" TO TABLE-ITEM-2 (3).                            NC2524.2
089000     MOVE   "USE" TO TABLE-ITEM-2 (4).                            NC2524.2
089100     MOVE   "TTS" TO TABLE-ITEM-2 (5).                            NC2524.2
089200     IF      RENAME-7 EQUAL TO "BOSTON MASSACHUSETTS"             NC2524.2
089300             PERFORM PASS GO TO RENAM-WRITE-11.                   NC2524.2
089400     GO      TO RENAM-FAIL-11.                                    NC2524.2
089500 RENAM-DELETE-11.                                                 NC2524.2
089600     PERFORM  DE-LETE.                                            NC2524.2
089700     GO       TO RENAM-WRITE-11.                                  NC2524.2
089800 RENAM-FAIL-11.                                                   NC2524.2
089900     PERFORM FAIL.                                                NC2524.2
090000     MOVE     RENAME-7 TO COMPUTED-A.                             NC2524.2
090100     MOVE     "BOSTON MASSACHUSETTS" TO CORRECT-A.                NC2524.2
090200 RENAM-WRITE-11.                                                  NC2524.2
090300     MOVE     "RENAMES A TABLE" TO FEATURE.                       NC2524.2
090400     MOVE     "RENAM-TEST-11" TO PAR-NAME.                        NC2524.2
090500     PERFORM  PRINT-DETAIL.                                       NC2524.2
090600 RENAM-INIT-D.                                                    NC2524.2
090700     MOVE     "RENAMED DATA ---" TO FEATURE.                      NC2524.2
090800     PERFORM  PRINT-DETAIL.                                       NC2524.2
090900 RENAM-TEST-12.                                                   NC2524.2
091000     MOVE     SPACE TO W-RENAMES-DATA.                            NC2524.2
091100     MOVE     12.3 TO WIDGET-1.                                   NC2524.2
091200     MOVE     45678.9 TO WIDGET-2.                                NC2524.2
091300     MOVE     ZERO TO WIDGET-3.                                   NC2524.2
091400     IF       RENAME-8 EQUAL TO "123*45,678.900000"               NC2524.2
091500              PERFORM PASS GO TO RENAM-WRITE-12.                  NC2524.2
091600     GO       TO RENAM-FAIL-12.                                   NC2524.2
091700 RENAM-DELETE-12.                                                 NC2524.2
091800     PERFORM  DE-LETE.                                            NC2524.2
091900     GO       TO RENAM-WRITE-12.                                  NC2524.2
092000 RENAM-FAIL-12.                                                   NC2524.2
092100     PERFORM FAIL.                                                NC2524.2
092200     MOVE     RENAME-8 TO COMPUTED-A.                             NC2524.2
092300     MOVE     "123*45,678.900000" TO CORRECT-A.                   NC2524.2
092400 RENAM-WRITE-12.                                                  NC2524.2
092500     MOVE     "  GROUP COMPARISON" TO FEATURE                     NC2524.2
092600     MOVE     "RENAM-TEST-12" TO PAR-NAME.                        NC2524.2
092700     PERFORM  PRINT-DETAIL.                                       NC2524.2
092800 RENAM-TEST-13.                                                   NC2524.2
092900     MOVE     SPACE TO W-RENAMES-DATA.                            NC2524.2
093000     MOVE     "123456789" TO RENAME-10.                           NC2524.2
093100     IF       RENAME-9 EQUAL TO "    12345678"                    NC2524.2
093200              PERFORM PASS GO TO RENAM-WRITE-13.                  NC2524.2
093300     GO       TO RENAM-FAIL-13.                                   NC2524.2
093400 RENAM-DELETE-13.                                                 NC2524.2
093500     PERFORM  DE-LETE.                                            NC2524.2
093600     GO       TO RENAM-WRITE-13.                                  NC2524.2
093700 RENAM-FAIL-13.                                                   NC2524.2
093800     PERFORM FAIL.                                                NC2524.2
093900     MOVE     RENAME-9 TO COMPUTED-A                              NC2524.2
094000     MOVE     "    12345678" TO CORRECT-A.                        NC2524.2
094100 RENAM-WRITE-13.                                                  NC2524.2
094200     MOVE     "  GRP MOVE, COMPARE" TO FEATURE.                   NC2524.2
094300     MOVE     "RENAM-TEST-13" TO PAR-NAME.                        NC2524.2
094400     PERFORM  PRINT-DETAIL.                                       NC2524.2
094500 RENAM-TEST-14.                                                   NC2524.2
094600     MOVE     SPACE TO W-RENAMES-DATA.                            NC2524.2
094700     MOVE     123456 TO RENAME-10                                 NC2524.2
094800     IF       WIDGET-4 EQUAL TO 1234                              NC2524.2
094900              PERFORM PASS GO TO RENAM-WRITE-14.                  NC2524.2
095000     GO       TO RENAM-FAIL-14.                                   NC2524.2
095100 RENAM-DELETE-14.                                                 NC2524.2
095200     PERFORM  DE-LETE.                                            NC2524.2
095300     GO       TO RENAM-WRITE-14.                                  NC2524.2
095400 RENAM-FAIL-14.                                                   NC2524.2
095500     PERFORM FAIL.                                                NC2524.2
095600     MOVE     WIDGET-4 TO COMPUTED-N.                             NC2524.2
095700     MOVE     1234 TO CORRECT-N.                                  NC2524.2
095800 RENAM-WRITE-14.                                                  NC2524.2
095900     MOVE     "  GROUP MOVE" TO FEATURE.                          NC2524.2
096000     MOVE     "RENAM-TEST-14" TO PAR-NAME.                        NC2524.2
096100     PERFORM  PRINT-DETAIL.                                       NC2524.2
096200 RENAM-TEST-15.                                                   NC2524.2
096300     MOVE     SPACE TO W-RENAMES-DATA.                            NC2524.2
096400     MOVE     234.5 TO RENAME-11.                                 NC2524.2
096500     IF       WIDGET-2 EQUAL TO "****234.50"                      NC2524.2
096600              PERFORM PASS GO TO RENAM-WRITE-15.                  NC2524.2
096700     GO       TO RENAM-FAIL-15.                                   NC2524.2
096800 RENAM-DELETE-15.                                                 NC2524.2
096900     PERFORM  DE-LETE.                                            NC2524.2
097000     GO       TO RENAM-WRITE-15.                                  NC2524.2
097100 RENAM-FAIL-15.                                                   NC2524.2
097200     PERFORM FAIL.                                                NC2524.2
097300     MOVE     WIDGET-2 TO COMPUTED-A.                             NC2524.2
097400     MOVE     "****234.50" TO CORRECT-A.                          NC2524.2
097500 RENAM-WRITE-15.                                                  NC2524.2
097600     MOVE     "  EDITED MOVE" TO FEATURE.                         NC2524.2
097700     MOVE     "RENAM-TEST-15" TO PAR-NAME.                        NC2524.2
097800     PERFORM  PRINT-DETAIL.                                       NC2524.2
097900 RENAM-INIT-E.                                                    NC2524.2
098000     MOVE     "  ADD, SIZE ERROR" TO FEATURE.                     NC2524.2
098100*        NOTE THE NEXT TWO TESTS ARE INTERRELATED.                NC2524.2
098200 RENAM-TEST-16.                                                   NC2524.2
098300     MOVE     8000 TO WIDGET-4.                                   NC2524.2
098400     ADD      3500 TO RENAME-12 ON SIZE ERROR                     NC2524.2
098500              PERFORM PASS GO TO RENAM-WRITE-16.                  NC2524.2
098600     GO       TO RENAM-FAIL-16.                                   NC2524.2
098700 RENAM-DELETE-16.                                                 NC2524.2
098800     PERFORM  DE-LETE.                                            NC2524.2
098900     GO       TO RENAM-WRITE-16.                                  NC2524.2
099000 RENAM-FAIL-16.                                                   NC2524.2
099100     PERFORM FAIL.                                                NC2524.2
099200     MOVE     "SIZE ERROR DID NOT OCCUR" TO RE-MARK.              NC2524.2
099300 RENAM-WRITE-16.                                                  NC2524.2
099400     MOVE     "RENAM-TEST-16" TO PAR-NAME.                        NC2524.2
099500     PERFORM  PRINT-DETAIL.                                       NC2524.2
099600 RENAM-TEST-17.                                                   NC2524.2
099700     IF       RENAME-12 EQUAL TO 8000                             NC2524.2
099800              PERFORM PASS GO TO RENAM-WRITE-17.                  NC2524.2
099900     GO       TO RENAM-FAIL-17.                                   NC2524.2
100000 RENAM-DELETE-17.                                                 NC2524.2
100100     PERFORM  DE-LETE.                                            NC2524.2
100200     GO       TO RENAM-WRITE-17.                                  NC2524.2
100300 RENAM-FAIL-17.                                                   NC2524.2
100400     PERFORM FAIL.                                                NC2524.2
100500     MOVE     RENAME-12 TO COMPUTED-N.                            NC2524.2
100600     MOVE     8000 TO CORRECT-N.                                  NC2524.2
100700 RENAM-WRITE-17.                                                  NC2524.2
100800     MOVE     "RENAM-TEST-17" TO PAR-NAME.                        NC2524.2
100900     PERFORM  PRINT-DETAIL.                                       NC2524.2
101000 RENAM-TEST-18.                                                   NC2524.2
101100     MOVE     SPACE TO U-RENAMES-DATA.                            NC2524.2
101200     MOVE     "CHICAGO ILLINOIS" TO RENAME-5 OF U-RENAMES-DATA.   NC2524.2
101300     IF       U-RENAMES-DATA EQUAL TO "CHICAGO ILL     "          NC2524.2
101400              PERFORM PASS GO TO RENAM-WRITE-18.                  NC2524.2
101500     PERFORM  FAIL.                                               NC2524.2
101600     GO       TO RENAM-FAIL-18.                                   NC2524.2
101700 RENAM-DELETE-18.                                                 NC2524.2
101800     PERFORM  DE-LETE.                                            NC2524.2
101900     GO       TO RENAM-WRITE-18.                                  NC2524.2
102000 RENAM-FAIL-18.                                                   NC2524.2
102100     PERFORM  FAIL.                                               NC2524.2
102200     MOVE     U-RENAMES-DATA TO COMPUTED-A.                       NC2524.2
102300     MOVE     "CHICAGO ILL     " TO CORRECT-A.                    NC2524.2
102400 RENAM-WRITE-18.                                                  NC2524.2
102500     MOVE     "  THROUGH" TO FEATURE.                             NC2524.2
102600     MOVE     "RENAM-TEST-18" TO PAR-NAME.                        NC2524.2
102700     PERFORM  PRINT-DETAIL.                                       NC2524.2
102800*                                                                 NC2524.2
102900 COMPUTING SECTION.                                               NC2524.2
103000 COMPUTE-INIT.                                                    NC2524.2
103100     MOVE SPACES TO TEST-RESULTS.                                 NC2524.2
103200     PERFORM END-ROUTINE.                                         NC2524.2
103300     MOVE "THE COMPUTED RESULT FOR THE FOLLOWING TESTS"           NC2524.2
103400             TO RE-MARK.                                          NC2524.2
103500     PERFORM PRINT-DETAIL.                                        NC2524.2
103600     MOVE "IS ALLOWED TO DEVIATE FROM THE INDICATED"              NC2524.2
103700             TO RE-MARK.                                          NC2524.2
103800     PERFORM PRINT-DETAIL.                                        NC2524.2
103900     MOVE "CORRECT RESULT BY" TO RE-MARK.                         NC2524.2
104000     PERFORM PRINT-DETAIL.                                        NC2524.2
104100     MOVE "+ OR - (CORRECT RESULT * (.2 ** 5))."                  NC2524.2
104200             TO RE-MARK.                                          NC2524.2
104300     PERFORM PRINT-DETAIL.                                        NC2524.2
104400     MOVE "COMPUTE        " TO FEATURE.                           NC2524.2
104500 COMP-TEST-1.                                                     NC2524.2
104600     COMPUTE COMPUTE-1 = COMPUTE-1A.                              NC2524.2
104700     IF ( COMPUTE-1 < 654.20038) AND                              NC2524.2
104800        ( COMPUTE-1 > 654.17422) THEN                             NC2524.2
104900              PERFORM PASS                                        NC2524.2
105000              GO TO COMP-WRITE-1.                                 NC2524.2
105100     PERFORM FAIL.                                                NC2524.2
105200     MOVE COMPUTE-1 TO COMPUTED-N.                                NC2524.2
105300     MOVE "+654.1873" TO CORRECT-A.                               NC2524.2
105400     GO TO COMP-WRITE-1.                                          NC2524.2
105500 COMP-DELETE-1.                                                   NC2524.2
105600     PERFORM DE-LETE.                                             NC2524.2
105700 COMP-WRITE-1.                                                    NC2524.2
105800     MOVE "COMP-TEST-1" TO PAR-NAME.                              NC2524.2
105900     PERFORM PRINT-DETAIL.                                        NC2524.2
106000 COMP-TEST-2.                                                     NC2524.2
106100     COMPUTE COMPUTE-2 = 2233.9                                   NC2524.2
106200     IF ( COMPUTE-2 < 2233.94468) AND                             NC2524.2
106300         ( COMPUTE-2 > 2233.85532) THEN                           NC2524.2
106400              PERFORM PASS                                        NC2524.2
106500              GO TO COMP-WRITE-2.                                 NC2524.2
106600     PERFORM FAIL.                                                NC2524.2
106700     MOVE COMPUTE-2 TO COMPUTED-N.                                NC2524.2
106800     MOVE "+2233.9" TO CORRECT-A.                                 NC2524.2
106900     GO TO COMP-WRITE-2.                                          NC2524.2
107000 COMP-DELETE-2.                                                   NC2524.2
107100     PERFORM DE-LETE.                                             NC2524.2
107200 COMP-WRITE-2.                                                    NC2524.2
107300     MOVE "COMP-TEST-2" TO PAR-NAME.                              NC2524.2
107400     PERFORM PRINT-DETAIL.                                        NC2524.2
107500 COMP-TEST-3.                                                     NC2524.2
107600     COMPUTE COMPUTE-3 = COMPUTE-3A - COMPUTE-3B.                 NC2524.2
107700     IF ( COMPUTE-3 NOT < 52.39105) AND                           NC2524.2
107800        ( COMPUTE-3 NOT > 52.38895) THEN                          NC2524.2
107900              PERFORM FAIL                                        NC2524.2
108000               MOVE COMPUTE-3 TO COMPUTED-N                       NC2524.2
108100              MOVE "+52.39" TO CORRECT-A                          NC2524.2
108200              GO TO COMP-WRITE-3.                                 NC2524.2
108300     PERFORM PASS.                                                NC2524.2
108400     GO TO COMP-WRITE-3.                                          NC2524.2
108500 COMP-DELETE-3.                                                   NC2524.2
108600     PERFORM DE-LETE.                                             NC2524.2
108700 COMP-WRITE-3.                                                    NC2524.2
108800     MOVE "COMP-TEST-3" TO PAR-NAME.                              NC2524.2
108900     PERFORM PRINT-DETAIL.                                        NC2524.2
109000 COMP-TEST-4.                                                     NC2524.2
109100     COMPUTE COMPUTE-4 = COMPUTE-4A + COMPUTE-4B.                 NC2524.2
109200        IF COMPUTE-4 NOT = 341                                    NC2524.2
109300              PERFORM FAIL                                        NC2524.2
109400              MOVE COMPUTE-4 TO COMPUTED-N                        NC2524.2
109500              MOVE "+341" TO CORRECT-A                            NC2524.2
109600              GO TO COMP-WRITE-4.                                 NC2524.2
109700     PERFORM PASS.                                                NC2524.2
109800     GO TO COMP-WRITE-4.                                          NC2524.2
109900 COMP-DELETE-4.                                                   NC2524.2
110000     PERFORM DE-LETE.                                             NC2524.2
110100 COMP-WRITE-4.                                                    NC2524.2
110200     MOVE "COMP-TEST-4" TO PAR-NAME.                              NC2524.2
110300     PERFORM PRINT-DETAIL.                                        NC2524.2
110400 COMP-TEST-5.                                                     NC2524.2
110500     COMPUTE COMPUTE-5 = COMPUTE-5A * 36.1                        NC2524.2
110600     IF ( COMPUTE-5 > 400.71801) OR                               NC2524.2
110700        ( COMPUTE-5 < 400.70199) THEN                             NC2524.2
110800              PERFORM FAIL                                        NC2524.2
110900              MOVE COMPUTE-5 TO COMPUTED-N                        NC2524.2
111000              MOVE "+400.71" TO CORRECT-A                         NC2524.2
111100              GO TO COMP-WRITE-5.                                 NC2524.2
111200     PERFORM PASS.                                                NC2524.2
111300     GO TO COMP-WRITE-5.                                          NC2524.2
111400 COMP-DELETE-5.                                                   NC2524.2
111500     PERFORM DE-LETE.                                             NC2524.2
111600 COMP-WRITE-5.                                                    NC2524.2
111700     MOVE "COMP-TEST-5" TO PAR-NAME.                              NC2524.2
111800     PERFORM PRINT-DETAIL.                                        NC2524.2
111900 COMP-TEST-6.                                                     NC2524.2
112000     COMPUTE COMPUTE-6 = COMPUTE-6A / 6.0                         NC2524.2
112100     IF ( COMPUTE-6 > 62.40125) OR                                NC2524.2
112200        ( COMPUTE-6 < 62.39875) THEN                              NC2524.2
112300              PERFORM FAIL                                        NC2524.2
112400              MOVE COMPUTE-6 TO COMPUTED-N                        NC2524.2
112500              MOVE "+062.40" TO CORRECT-A                         NC2524.2
112600              GO TO COMP-WRITE-6.                                 NC2524.2
112700     PERFORM PASS.                                                NC2524.2
112800     GO TO COMP-WRITE-6.                                          NC2524.2
112900 COMP-DELETE-6.                                                   NC2524.2
113000     PERFORM DE-LETE.                                             NC2524.2
113100 COMP-WRITE-6.                                                    NC2524.2
113200     MOVE "COMP-TEST-6" TO PAR-NAME.                              NC2524.2
113300     PERFORM PRINT-DETAIL.                                        NC2524.2
113400 COMP-TEST-7.                                                     NC2524.2
113500     COMPUTE COMPUTE-7 = 2.0 ** 4.                                NC2524.2
113600        IF COMPUTE-7 = 16                                         NC2524.2
113700              PERFORM PASS                                        NC2524.2
113800              GO TO COMP-WRITE-7.                                 NC2524.2
113900     PERFORM FAIL.                                                NC2524.2
114000     MOVE COMPUTE-7 TO COMPUTED-N.                                NC2524.2
114100     MOVE "+16" TO CORRECT-A.                                     NC2524.2
114200     GO TO COMP-WRITE-7.                                          NC2524.2
114300 COMP-DELETE-7.                                                   NC2524.2
114400     PERFORM DE-LETE.                                             NC2524.2
114500 COMP-WRITE-7.                                                    NC2524.2
114600     MOVE "COMP-TEST-7" TO PAR-NAME.                              NC2524.2
114700     PERFORM PRINT-DETAIL.                                        NC2524.2
114800 COMP-TEST-8.                                                     NC2524.2
114900     COMPUTE COMPUTE-8 = (((24.0 + 1) * (60 - 10)) / 125) ** 2.   NC2524.2
115000             IF COMPUTE-8 = 100                                   NC2524.2
115100          PERFORM PASS                                            NC2524.2
115200          GO TO COMP-WRITE-8.                                     NC2524.2
115300     PERFORM FAIL.                                                NC2524.2
115400     MOVE COMPUTE-8 TO COMPUTED-N.                                NC2524.2
115500     MOVE "+100" TO CORRECT-A.                                    NC2524.2
115600     GO TO COMP-WRITE-8.                                          NC2524.2
115700 COMP-DELETE-8.                                                   NC2524.2
115800     PERFORM DE-LETE.                                             NC2524.2
115900 COMP-WRITE-8.                                                    NC2524.2
116000     MOVE "COMP-TEST-8" TO PAR-NAME.                              NC2524.2
116100     PERFORM PRINT-DETAIL.                                        NC2524.2
116200 COMP-TEST-9.                                                     NC2524.2
116300     COMPUTE COMPUTE-9 ROUNDED = COMPUTE-6A * 7.0                 NC2524.2
116400     IF (COMPUTE-9 > 2621.05242) OR                               NC2524.2
116500        (COMPUTE-9 < 2620.94758) THEN                             NC2524.2
116600              PERFORM FAIL                                        NC2524.2
116700              MOVE COMPUTE-9 TO COMPUTED-N                        NC2524.2
116800              MOVE "+2621" TO CORRECT-A                           NC2524.2
116900              GO TO COMP-WRITE-9.                                 NC2524.2
117000     PERFORM PASS.                                                NC2524.2
117100     GO TO COMP-WRITE-9.                                          NC2524.2
117200 COMP-DELETE-9.                                                   NC2524.2
117300     PERFORM DE-LETE.                                             NC2524.2
117400 COMP-WRITE-9.                                                    NC2524.2
117500     MOVE "COMP-TEST-9" TO PAR-NAME.                              NC2524.2
117600     PERFORM PRINT-DETAIL.                                        NC2524.2
117700 COMP-TEST-10.                                                    NC2524.2
117800     COMPUTE COMPUTE-10 = COMPUTE-1A + COMPUTE-6A ON SIZE ERROR   NC2524.2
117900     MOVE "R" TO XRAY.                                            NC2524.2
118000     IF XRAY EQUAL TO "R"                                         NC2524.2
118100              PERFORM PASS                                        NC2524.2
118200              GO TO COMP-WRITE-10.                                NC2524.2
118300     PERFORM FAIL.                                                NC2524.2
118400     MOVE "OSE NOT EXECUTED" TO RE-MARK.                          NC2524.2
118500     GO TO COMP-WRITE-10.                                         NC2524.2
118600 COMP-DELETE-10.                                                  NC2524.2
118700     PERFORM DE-LETE.                                             NC2524.2
118800 COMP-WRITE-10.                                                   NC2524.2
118900     MOVE "COMP-TEST-10" TO PAR-NAME.                             NC2524.2
119000     PERFORM PRINT-DETAIL.                                        NC2524.2
119100 COMP-TEST-11.                                                    NC2524.2
119200     IF (COMPUTE-10 > 0.00002) OR                                 NC2524.2
119300        (COMPUTE-10 < -0.00002)                                   NC2524.2
119400              PERFORM FAIL                                        NC2524.2
119500              MOVE COMPUTE-10 TO COMPUTED-N                       NC2524.2
119600              MOVE ZERO TO CORRECT-N                              NC2524.2
119700              GO TO COMP-WRITE-11.                                NC2524.2
119800     PERFORM PASS.                                                NC2524.2
119900     GO TO COMP-WRITE-11.                                         NC2524.2
120000 COMP-DELETE-11.                                                  NC2524.2
120100     PERFORM DE-LETE.                                             NC2524.2
120200 COMP-WRITE-11.                                                   NC2524.2
120300     MOVE "COMP-TEST-11" TO PAR-NAME.                             NC2524.2
120400     PERFORM PRINT-DETAIL.                                        NC2524.2
120500 COMP-TEST-12.                                                    NC2524.2
120600     COMPUTE COMPUTE-11 = COMPUTE-11A + COMPUTE-11B - 121.6       NC2524.2
120700     IF ( COMPUTE-11 < 718.51437) AND                             NC2524.2
120800        ( COMPUTE-11 > 718.48563) THEN                            NC2524.2
120900              PERFORM PASS                                        NC2524.2
121000              GO TO COMP-WRITE-12.                                NC2524.2
121100     PERFORM FAIL.                                                NC2524.2
121200     MOVE COMPUTE-11 TO COMPUTED-N.                               NC2524.2
121300     MOVE "+718.5" TO CORRECT-A.                                  NC2524.2
121400     GO TO COMP-WRITE-12.                                         NC2524.2
121500 COMP-DELETE-12.                                                  NC2524.2
121600     PERFORM DE-LETE.                                             NC2524.2
121700 COMP-WRITE-12.                                                   NC2524.2
121800     MOVE "COMP-TEST-12" TO PAR-NAME.                             NC2524.2
121900     PERFORM PRINT-DETAIL.                                        NC2524.2
122000 COMP-TEST-13.                                                    NC2524.2
122100     COMPUTE COMPUTE-12 = COMPUTE-12A * 5.1 / 281.7.              NC2524.2
122200     IF (COMPUTE-12 < 6.09012) AND                                NC2524.2
122300        (COMPUTE-12 > 6.08988) THEN                               NC2524.2
122400              PERFORM PASS                                        NC2524.2
122500              GO TO COMP-WRITE-13.                                NC2524.2
122600     PERFORM FAIL.                                                NC2524.2
122700     MOVE COMPUTE-12 TO COMPUTED-N.                               NC2524.2
122800     MOVE "+6.09" TO CORRECT-A.                                   NC2524.2
122900     GO TO COMP-WRITE-13.                                         NC2524.2
123000 COMP-DELETE-13.                                                  NC2524.2
123100     PERFORM DE-LETE.                                             NC2524.2
123200 COMP-WRITE-13.                                                   NC2524.2
123300     MOVE "COMP-TEST-13" TO PAR-NAME.                             NC2524.2
123400     PERFORM PRINT-DETAIL.                                        NC2524.2
123500 COMPUTE-ROUTINE SECTION.                                         NC2524.2
123600 COMPUTE-TEST.                                                    NC2524.2
123700     MOVE "COMPUTE" TO FEATURE.                                   NC2524.2
123800     MOVE ZERO TO W-1.                                            NC2524.2
123900     MOVE ZERO TO W-2.                                            NC2524.2
124000 COMP-TEST-14.                                                    NC2524.2
124100     COMPUTE W-1 = NINE.                                          NC2524.2
124200        IF W-1 = 9                                                NC2524.2
124300              PERFORM PASS                                        NC2524.2
124400              GO TO COMP-WRITE-14.                                NC2524.2
124500     PERFORM FAIL.                                                NC2524.2
124600     MOVE W-1 TO COMPUTED-A.                                      NC2524.2
124700     MOVE 9 TO W-1.                                               NC2524.2
124800     MOVE 9 TO CORRECT-A.                                         NC2524.2
124900     GO TO COMP-WRITE-14.                                         NC2524.2
125000 COMP-DELETE-14.                                                  NC2524.2
125100     PERFORM DE-LETE.                                             NC2524.2
125200 COMP-WRITE-14.                                                   NC2524.2
125300     MOVE "COMP-TEST-14" TO PAR-NAME.                             NC2524.2
125400     PERFORM PRINT-DETAIL.                                        NC2524.2
125500 COMP-TEST-15.                                                    NC2524.2
125600     COMPUTE W-2 = W-1 + 20.                                      NC2524.2
125700        IF W-2 = 29                                               NC2524.2
125800              PERFORM PASS                                        NC2524.2
125900              GO TO COMP-WRITE-15.                                NC2524.2
126000     PERFORM FAIL.                                                NC2524.2
126100     MOVE W-2 TO COMPUTED-N.                                      NC2524.2
126200     MOVE "+29" TO CORRECT-A.                                     NC2524.2
126300     MOVE 29 TO W-2.                                              NC2524.2
126400     GO TO COMP-WRITE-15.                                         NC2524.2
126500 COMP-DELETE-15.                                                  NC2524.2
126600     PERFORM DE-LETE.                                             NC2524.2
126700 COMP-WRITE-15.                                                   NC2524.2
126800     MOVE "COMP-TEST-15" TO PAR-NAME.                             NC2524.2
126900     PERFORM PRINT-DETAIL.                                        NC2524.2
127000 COMP-TEST-16.                                                    NC2524.2
127100     MOVE ZERO TO W-11.                                           NC2524.2
127200     COMPUTE W-11 = W-1 - W-2.                                    NC2524.2
127300     IF ( W-11 > -20.00040) AND                                   NC2524.2
127400        ( W-11 < -19.99960) THEN                                  NC2524.2
127500              PERFORM PASS                                        NC2524.2
127600              GO TO COMP-WRITE-16.                                NC2524.2
127700     PERFORM FAIL.                                                NC2524.2
127800     MOVE W-11 TO COMPUTED-N.                                     NC2524.2
127900     MOVE "-20" TO CORRECT-A.                                     NC2524.2
128000     GO TO COMP-WRITE-16.                                         NC2524.2
128100 COMP-DELETE-16.                                                  NC2524.2
128200     PERFORM DE-LETE.                                             NC2524.2
128300 COMP-WRITE-16.                                                   NC2524.2
128400     MOVE "COMP-TEST-16" TO PAR-NAME.                             NC2524.2
128500     PERFORM PRINT-DETAIL.                                        NC2524.2
128600 COMP-TEST-17.                                                    NC2524.2
128700     MOVE ZERO TO W-3.                                            NC2524.2
128800     COMPUTE W-3 = TEN * 30.                                      NC2524.2
128900        IF W-3 = 300                                              NC2524.2
129000              PERFORM PASS                                        NC2524.2
129100              GO TO COMP-WRITE-17.                                NC2524.2
129200     PERFORM FAIL.                                                NC2524.2
129300     MOVE W-3 TO COMPUTED-N.                                      NC2524.2
129400     MOVE "+300" TO CORRECT-A.                                    NC2524.2
129500     GO TO COMP-WRITE-17.                                         NC2524.2
129600 COMP-DELETE-17.                                                  NC2524.2
129700     PERFORM DE-LETE.                                             NC2524.2
129800 COMP-WRITE-17.                                                   NC2524.2
129900     MOVE "COMP-TEST-17" TO PAR-NAME.                             NC2524.2
130000     PERFORM PRINT-DETAIL.                                        NC2524.2
130100 COMP-TEST-18.                                                    NC2524.2
130200     MOVE ZERO TO W-5.                                            NC2524.2
130300     COMPUTE W-5 = 42 / SEVEN.                                    NC2524.2
130400        IF W-5 = 6                                                NC2524.2
130500              PERFORM PASS                                        NC2524.2
130600              GO TO COMP-WRITE-18.                                NC2524.2
130700     PERFORM FAIL.                                                NC2524.2
130800     MOVE W-5 TO COMPUTED-N.                                      NC2524.2
130900     MOVE "+6" TO CORRECT-A.                                      NC2524.2
131000     GO TO COMP-WRITE-18.                                         NC2524.2
131100 COMP-DELETE-18.                                                  NC2524.2
131200     PERFORM DE-LETE.                                             NC2524.2
131300 COMP-WRITE-18.                                                   NC2524.2
131400     MOVE "COMP-TEST-18" TO PAR-NAME.                             NC2524.2
131500     PERFORM PRINT-DETAIL.                                        NC2524.2
131600 COMP-TEST-19.                                                    NC2524.2
131700     MOVE ZERO TO W-2.                                            NC2524.2
131800     COMPUTE W-2 = FOUR ** 3.                                     NC2524.2
131900        IF W-2 = 64                                               NC2524.2
132000              PERFORM PASS                                        NC2524.2
132100              GO TO COMP-WRITE-19.                                NC2524.2
132200     PERFORM FAIL.                                                NC2524.2
132300     MOVE W-2 TO COMPUTED-N.                                      NC2524.2
132400     MOVE "+64" TO CORRECT-A.                                     NC2524.2
132500     GO TO COMP-WRITE-19.                                         NC2524.2
132600 COMP-DELETE-19.                                                  NC2524.2
132700     PERFORM DE-LETE.                                             NC2524.2
132800 COMP-WRITE-19.                                                   NC2524.2
132900     MOVE "COMP-TEST-19" TO PAR-NAME.                             NC2524.2
133000     PERFORM PRINT-DETAIL.                                        NC2524.2
133100 COMP-TEST-20.                                                    NC2524.2
133200     MOVE 555 TO W-3.                                             NC2524.2
133300     COMPUTE W-3 = TWENTY-5 + 101 + 222.                          NC2524.2
133400        IF W-3 = 348                                              NC2524.2
133500              PERFORM PASS                                        NC2524.2
133600              GO TO COMP-WRITE-20.                                NC2524.2
133700              PERFORM FAIL.                                       NC2524.2
133800              MOVE W-3 TO COMPUTED-N.                             NC2524.2
133900              MOVE "+348" TO CORRECT-A.                           NC2524.2
134000     GO TO COMP-WRITE-20.                                         NC2524.2
134100 COMP-DELETE-20.                                                  NC2524.2
134200     PERFORM DE-LETE.                                             NC2524.2
134300 COMP-WRITE-20.                                                   NC2524.2
134400     MOVE "COMP-TEST-20" TO PAR-NAME.                             NC2524.2
134500     PERFORM PRINT-DETAIL.                                        NC2524.2
134600 COMP-TEST-21.                                                    NC2524.2
134700     MOVE ZERO TO W-9.                                            NC2524.2
134800     COMPUTE W-9 = TWO * (3 + 4).                                 NC2524.2
134900        IF W-9 = 14                                               NC2524.2
135000              PERFORM PASS                                        NC2524.2
135100              GO TO COMP-WRITE-21.                                NC2524.2
135200     PERFORM FAIL.                                                NC2524.2
135300     MOVE W-9 TO COMPUTED-N.                                      NC2524.2
135400     MOVE "+14" TO CORRECT-A.                                     NC2524.2
135500     GO TO COMP-WRITE-21.                                         NC2524.2
135600 COMP-DELETE-21.                                                  NC2524.2
135700     PERFORM DE-LETE.                                             NC2524.2
135800 COMP-WRITE-21.                                                   NC2524.2
135900     MOVE "COMP-TEST-21" TO PAR-NAME.                             NC2524.2
136000     PERFORM PRINT-DETAIL.                                        NC2524.2
136100  COMP-TEST-22.                                                   NC2524.2
136200     MOVE ZERO TO W-9.                                            NC2524.2
136300     COMPUTE W-9 = (TWO + (3 * FOUR) / (2 * THREE)) ** 2 - 1.     NC2524.2
136400             IF W-9 = 15     PERFORM PASS                         NC2524.2
136500         GO TO COMP-WRITE-22.                                     NC2524.2
136600     PERFORM FAIL.                                                NC2524.2
136700     MOVE W-9 TO COMPUTED-N.                                      NC2524.2
136800     MOVE "+15" TO CORRECT-A.                                     NC2524.2
136900     GO TO COMP-WRITE-22.                                         NC2524.2
137000 COMP-DELETE-22.                                                  NC2524.2
137100     PERFORM DE-LETE.                                             NC2524.2
137200 COMP-WRITE-22.                                                   NC2524.2
137300     MOVE "COMP-TEST-22" TO PAR-NAME.                             NC2524.2
137400     PERFORM PRINT-DETAIL.                                        NC2524.2
137500 COMP-TEST-23.                                                    NC2524.2
137600     MOVE ZERO TO XRAY.                                           NC2524.2
137700     MOVE 10 TO W-2.                                              NC2524.2
137800     COMPUTE W-2 = 96 + TWENTY ON SIZE ERROR                      NC2524.2
137900     MOVE 8 TO XRAY.                                              NC2524.2
138000     IF XRAY IS EQUAL TO "8"                                      NC2524.2
138100              PERFORM PASS                                        NC2524.2
138200              GO TO COMP-WRITE-23.                                NC2524.2
138300     PERFORM FAIL.                                                NC2524.2
138400     MOVE "8" TO CORRECT-A.                                       NC2524.2
138500     MOVE XRAY TO COMPUTED-A.                                     NC2524.2
138600     MOVE "OSE NOT EXECUTED" TO RE-MARK.                          NC2524.2
138700     GO TO COMP-WRITE-23.                                         NC2524.2
138800 COMP-DELETE-23.                                                  NC2524.2
138900     PERFORM DE-LETE.                                             NC2524.2
139000 COMP-WRITE-23.                                                   NC2524.2
139100     MOVE "COMP-TEST-23" TO PAR-NAME.                             NC2524.2
139200     PERFORM PRINT-DETAIL.                                        NC2524.2
139300 COMP-TEST-24.                                                    NC2524.2
139400        IF W-2 = 10                                               NC2524.2
139500              PERFORM PASS                                        NC2524.2
139600              GO TO COMP-WRITE-24.                                NC2524.2
139700     PERFORM FAIL.                                                NC2524.2
139800     MOVE W-2 TO COMPUTED-A.                                      NC2524.2
139900     MOVE "10" TO CORRECT-A.                                      NC2524.2
140000     MOVE "NOT PROTECTED BY OES" TO RE-MARK.                      NC2524.2
140100     GO TO COMP-WRITE-24.                                         NC2524.2
140200 COMP-DELETE-24.                                                  NC2524.2
140300     PERFORM DE-LETE.                                             NC2524.2
140400 COMP-WRITE-24.                                                   NC2524.2
140500     MOVE "COMP-TEST-24" TO PAR-NAME.                             NC2524.2
140600     PERFORM PRINT-DETAIL.                                        NC2524.2
140700 COMP-TEST-25.                                                    NC2524.2
140800     MOVE ZERO TO W-11.                                           NC2524.2
140900     COMPUTE W-11 ROUNDED = D-1 + D-7.                            NC2524.2
141000     IF ( W-11 < 2.20004) AND                                     NC2524.2
141100        ( W-11 > 2.19996) THEN                                    NC2524.2
141200              PERFORM PASS                                        NC2524.2
141300              GO TO COMP-WRITE-25.                                NC2524.2
141400     PERFORM FAIL.                                                NC2524.2
141500     MOVE W-11 TO COMPUTED-N.                                     NC2524.2
141600     MOVE "+2.2" TO CORRECT-A.                                    NC2524.2
141700     GO TO COMP-WRITE-25.                                         NC2524.2
141800 COMP-DELETE-25.                                                  NC2524.2
141900     PERFORM DE-LETE.                                             NC2524.2
142000 COMP-WRITE-25.                                                   NC2524.2
142100     MOVE "COMP-TEST-25" TO PAR-NAME.                             NC2524.2
142200     PERFORM PRINT-DETAIL.                                        NC2524.2
142300 COMP-TEST-26.                                                    NC2524.2
142400     MOVE ZERO TO W-11.                                           NC2524.2
142500     COMPUTE W-11 ROUNDED = 25 / 10.                              NC2524.2
142600     IF ( W-11 < 2.50005) AND                                     NC2524.2
142700        ( W-11 > 2.49995) THEN                                    NC2524.2
142800              PERFORM PASS                                        NC2524.2
142900              GO TO COMP-WRITE-26.                                NC2524.2
143000     PERFORM FAIL.                                                NC2524.2
143100     MOVE W-11 TO COMPUTED-N.                                     NC2524.2
143200     MOVE "+2.5" TO CORRECT-A.                                    NC2524.2
143300     GO TO COMP-WRITE-26.                                         NC2524.2
143400 COMP-DELETE-26.                                                  NC2524.2
143500     PERFORM DE-LETE.                                             NC2524.2
143600 COMP-WRITE-26.                                                   NC2524.2
143700     MOVE "COMP-TEST-26" TO PAR-NAME.                             NC2524.2
143800     PERFORM PRINT-DETAIL.                                        NC2524.2
143900 CTST-END.                                                        NC2524.2
144000     EXIT.                                                        NC2524.2
144100 COMP-INIT-A.                                                     NC2524.2
144200     MOVE     "COMPUTE" TO FEATURE.                               NC2524.2
144300 COMP-TEST-27.                                                    NC2524.2
144400     MOVE     ZERO TO WRK-DS-02V00.                               NC2524.2
144500     COMPUTE  WRK-DS-02V00 = -9.                                  NC2524.2
144600        IF WRK-DS-02V00 = -9                                      NC2524.2
144700              PERFORM PASS                                        NC2524.2
144800              GO TO COMP-WRITE-27.                                NC2524.2
144900     MOVE     WRK-DS-02V00 TO COMPUTED-N.                         NC2524.2
145000     MOVE     -9 TO CORRECT-N.                                    NC2524.2
145100     PERFORM  FAIL.                                               NC2524.2
145200     GO TO COMP-WRITE-27.                                         NC2524.2
145300 COMP-DELETE-27.                                                  NC2524.2
145400     PERFORM DE-LETE.                                             NC2524.2
145500 COMP-WRITE-27.                                                   NC2524.2
145600     MOVE     "COMP-TEST-27" TO PAR-NAME.                         NC2524.2
145700     PERFORM  PRINT-DETAIL.                                       NC2524.2
145800 COMP-TEST-28.                                                    NC2524.2
145900     MOVE     ZERO TO WRK-DS-02V00.                               NC2524.2
146000     COMPUTE  WRK-DS-02V00 = A99-DS-02V00.                        NC2524.2
146100        IF WRK-DS-02V00 = 99                                      NC2524.2
146200              PERFORM PASS                                        NC2524.2
146300              GO TO COMP-WRITE-28.                                NC2524.2
146400     MOVE     WRK-DS-02V00 TO COMPUTED-N.                         NC2524.2
146500     MOVE     99 TO CORRECT-N.                                    NC2524.2
146600     PERFORM  FAIL.                                               NC2524.2
146700     GO TO COMP-WRITE-28.                                         NC2524.2
146800 COMP-DELETE-28.                                                  NC2524.2
146900     PERFORM DE-LETE.                                             NC2524.2
147000 COMP-WRITE-28.                                                   NC2524.2
147100     MOVE     "COMP-TEST-28" TO PAR-NAME.                         NC2524.2
147200     PERFORM  PRINT-DETAIL.                                       NC2524.2
147300 COMP-TEST-29.                                                    NC2524.2
147400     MOVE     ZERO TO WRK-DS-18V00.                               NC2524.2
147500     COMPUTE  WRK-DS-18V00 = A18ONES-DS-18V00 + A18ONES-DS-18V00. NC2524.2
147600     IF       WRK-DS-18V00 = 222222222222222222                   NC2524.2
147700              PERFORM PASS                                        NC2524.2
147800              GO TO COMP-WRITE-29.                                NC2524.2
147900     MOVE     WRK-DS-18V00 TO COMPUTED-18V0.                      NC2524.2
148000     MOVE     222222222222222222 TO CORRECT-18V0.                 NC2524.2
148100     PERFORM  FAIL.                                               NC2524.2
148200     GO TO COMP-WRITE-29.                                         NC2524.2
148300 COMP-DELETE-29.                                                  NC2524.2
148400     PERFORM DE-LETE.                                             NC2524.2
148500 COMP-WRITE-29.                                                   NC2524.2
148600     MOVE     "COMP-TEST-29" TO PAR-NAME.                         NC2524.2
148700     PERFORM  PRINT-DETAIL.                                       NC2524.2
148800 COMP-TEST-30.                                                    NC2524.2
148900     MOVE     ZERO TO WRK-DS-18V00.                               NC2524.2
149000     COMPUTE  WRK-DS-18V00 = A18TWOS-DS-18V00 - A18ONES-DS-18V00. NC2524.2
149100     IF       WRK-DS-18V00 = 111111111111111111                   NC2524.2
149200              PERFORM PASS                                        NC2524.2
149300              GO TO COMP-WRITE-30.                                NC2524.2
149400     MOVE     WRK-DS-18V00 TO COMPUTED-18V0.                      NC2524.2
149500     MOVE     111111111111111111 TO CORRECT-18V0.                 NC2524.2
149600     PERFORM  FAIL.                                               NC2524.2
149700     GO TO COMP-WRITE-30.                                         NC2524.2
149800 COMP-DELETE-30.                                                  NC2524.2
149900     PERFORM DE-LETE.                                             NC2524.2
150000 COMP-WRITE-30.                                                   NC2524.2
150100     MOVE     "COMP-TEST-30" TO PAR-NAME.                         NC2524.2
150200     PERFORM  PRINT-DETAIL.                                       NC2524.2
150300 COMP-TEST-31.                                                    NC2524.2
150400     MOVE   ZERO TO   TA--X.                                      NC2524.2
150500     COMPUTE     TA--X  =  3 * A02TWOS-DU-02V00.                  NC2524.2
150600        IF TA--X = 66                                             NC2524.2
150700              PERFORM PASS                                        NC2524.2
150800              GO TO COMP-WRITE-31.                                NC2524.2
150900     MOVE       TA--X TO COMPUTED-N                               NC2524.2
151000     MOVE     66 TO CORRECT-N.                                    NC2524.2
151100     PERFORM  FAIL.                                               NC2524.2
151200     GO TO COMP-WRITE-31.                                         NC2524.2
151300 COMP-DELETE-31.                                                  NC2524.2
151400     PERFORM DE-LETE.                                             NC2524.2
151500 COMP-WRITE-31.                                                   NC2524.2
151600     MOVE     "COMP-TEST-31" TO PAR-NAME.                         NC2524.2
151700     PERFORM  PRINT-DETAIL.                                       NC2524.2
151800 COMP-TEST-32.                                                    NC2524.2
151900     MOVE     ZERO TO WRK-DS-05V00.                               NC2524.2
152000     COMPUTE  WRK-DS-05V00 = A02TWOS-DU-02V00 / A02TWOS-DS-03V02. NC2524.2
152100        IF WRK-DS-05V00 = 1                                       NC2524.2
152200              PERFORM PASS                                        NC2524.2
152300              GO TO COMP-WRITE-32.                                NC2524.2
152400     MOVE     WRK-DS-05V00 TO COMPUTED-N.                         NC2524.2
152500     MOVE     1 TO CORRECT-N.                                     NC2524.2
152600     PERFORM  FAIL.                                               NC2524.2
152700     GO TO COMP-WRITE-32.                                         NC2524.2
152800 COMP-DELETE-32.                                                  NC2524.2
152900     PERFORM DE-LETE.                                             NC2524.2
153000 COMP-WRITE-32.                                                   NC2524.2
153100     MOVE     "COMP-TEST-32" TO PAR-NAME.                         NC2524.2
153200     PERFORM  PRINT-DETAIL.                                       NC2524.2
153300 COMP-TEST-33.                                                    NC2524.2
153400     MOVE     ZERO TO WRK-DS-05V00.                               NC2524.2
153500     COMPUTE  WRK-DS-05V00 = 3 ** ATWO-DS-01V00.                  NC2524.2
153600        IF WRK-DS-05V00 = 9                                       NC2524.2
153700              PERFORM PASS                                        NC2524.2
153800              GO TO COMP-WRITE-33.                                NC2524.2
153900     MOVE     WRK-DS-05V00 TO COMPUTED-N.                         NC2524.2
154000     MOVE     9 TO CORRECT-N.                                     NC2524.2
154100     PERFORM  FAIL.                                               NC2524.2
154200     GO TO COMP-WRITE-33.                                         NC2524.2
154300 COMP-DELETE-33.                                                  NC2524.2
154400              PERFORM DE-LETE.                                    NC2524.2
154500 COMP-WRITE-33.                                                   NC2524.2
154600     MOVE     "COMP-TEST-33" TO PAR-NAME.                         NC2524.2
154700     PERFORM  PRINT-DETAIL.                                       NC2524.2
154800 COMP-TEST-34.                                                    NC2524.2
154900     MOVE     ZERO TO WRK-DS-02V00.                               NC2524.2
155000     COMPUTE  WRK-DS-02V00 ROUNDED  = A99-DS-02V00                NC2524.2
155100              +   AZERO-DS-05V05 - 2.5.                           NC2524.2
155200        IF WRK-DS-02V00 = 97                                      NC2524.2
155300              PERFORM PASS                                        NC2524.2
155400              GO TO COMP-WRITE-34.                                NC2524.2
155500     MOVE     WRK-DS-02V00 TO COMPUTED-N.                         NC2524.2
155600     MOVE     97 TO CORRECT-N.                                    NC2524.2
155700     PERFORM  FAIL.                                               NC2524.2
155800     GO TO COMP-WRITE-34.                                         NC2524.2
155900 COMP-DELETE-34.                                                  NC2524.2
156000     PERFORM DE-LETE.                                             NC2524.2
156100 COMP-WRITE-34.                                                   NC2524.2
156200     MOVE     "COMP-TEST-34" TO PAR-NAME.                         NC2524.2
156300     PERFORM  PRINT-DETAIL.                                       NC2524.2
156400 COMP-TEST-35.                                                    NC2524.2
156500     MOVE     ZERO TO WRK-DS-02V00.                               NC2524.2
156600     COMPUTE  WRK-DS-02V00 = A99-DS-02V00 + AZERO-DS-05V05        NC2524.2
156700              ON SIZE ERROR                                       NC2524.2
156800              MOVE "SIZE ERR SHOULD NOT EXCUTE" TO RE-MARK        NC2524.2
156900              PERFORM FAIL                                        NC2524.2
157000              GO TO COMP-WRITE-35.                                NC2524.2
157100     PERFORM  PASS.                                               NC2524.2
157200     GO       TO COMP-WRITE-35.                                   NC2524.2
157300 COMP-DELETE-35.                                                  NC2524.2
157400     PERFORM  DE-LETE.                                            NC2524.2
157500 COMP-WRITE-35.                                                   NC2524.2
157600     MOVE     "COMP-TEST-35" TO PAR-NAME.                         NC2524.2
157700     PERFORM  PRINT-DETAIL.                                       NC2524.2
157800 COMP-TEST-36.                                                    NC2524.2
157900     IF       TEST-2NUC-COND-99                                   NC2524.2
158000              PERFORM PASS                                        NC2524.2
158100              GO TO COMP-WRITE-36.                                NC2524.2
158200*    NOTE     THIS TEST DEPENDS UPON THE RESULT OF COMP-TEST-35.  NC2524.2
158300     MOVE     WRK-DS-02V00 TO COMPUTED-N.                         NC2524.2
158400     MOVE     99 TO CORRECT-N.                                    NC2524.2
158500     PERFORM  FAIL.                                               NC2524.2
158600     GO TO COMP-WRITE-36.                                         NC2524.2
158700 COMP-DELETE-36.                                                  NC2524.2
158800     PERFORM DE-LETE.                                             NC2524.2
158900 COMP-WRITE-36.                                                   NC2524.2
159000     MOVE     "COMP-TEST-36" TO PAR-NAME.                         NC2524.2
159100     PERFORM  PRINT-DETAIL.                                       NC2524.2
159200 COMP-TEST-37.                                                    NC2524.2
159300     MOVE     ZERO TO WRK-DS-0201P.                               NC2524.2
159400     COMPUTE  WRK-DS-0201P ROUNDED = A05ONES-DS-05V00 / 5         NC2524.2
159500              ON SIZE ERROR                                       NC2524.2
159600              PERFORM PASS                                        NC2524.2
159700              GO TO COMP-WRITE-37.                                NC2524.2
159800     MOVE     "ON SIZE ERROR NOT EXECUTED" TO RE-MARK.            NC2524.2
159900     PERFORM  FAIL.                                               NC2524.2
160000     GO TO COMP-WRITE-37.                                         NC2524.2
160100 COMP-DELETE-37.                                                  NC2524.2
160200     PERFORM DE-LETE.                                             NC2524.2
160300 COMP-WRITE-37.                                                   NC2524.2
160400     MOVE     "COMP-TEST-37" TO PAR-NAME.                         NC2524.2
160500     PERFORM  PRINT-DETAIL.                                       NC2524.2
160600 COMP-TEST-38.                                                    NC2524.2
160700        IF WRK-DS-0201P = ZERO                                    NC2524.2
160800              PERFORM PASS                                        NC2524.2
160900              GO TO COMP-WRITE-38.                                NC2524.2
161000*    NOTE     THIS TEST DEPENDS UPON THE RESULT OF COMP-TEST-37.  NC2524.2
161100     MOVE     WRK-DS-0201P TO COMPUTED-N.                         NC2524.2
161200     MOVE     ZERO TO CORRECT-N.                                  NC2524.2
161300     PERFORM  FAIL.                                               NC2524.2
161400     GO TO COMP-WRITE-38.                                         NC2524.2
161500 COMP-DELETE-38.                                                  NC2524.2
161600     PERFORM DE-LETE.                                             NC2524.2
161700 COMP-WRITE-38.                                                   NC2524.2
161800     MOVE     "COMP-TEST-38" TO PAR-NAME.                         NC2524.2
161900     PERFORM  PRINT-DETAIL.                                       NC2524.2
162000 COMP-TEST-39-42.                                                 NC2524.2
162100     MOVE SPACES TO TEST-RESULTS.                                 NC2524.2
162200     MOVE "NOT USED" TO RE-MARK.                                  NC2524.2
162300     MOVE "COMP-TEST-39" TO PAR-NAME.                             NC2524.2
162400     PERFORM PRINT-DETAIL.                                        NC2524.2
162500     MOVE "NOT USED" TO RE-MARK.                                  NC2524.2
162600     MOVE "COMP-TEST-40" TO PAR-NAME.                             NC2524.2
162700     PERFORM PRINT-DETAIL.                                        NC2524.2
162800     MOVE "NOT USED" TO RE-MARK.                                  NC2524.2
162900     MOVE "COMP-TEST-41" TO PAR-NAME.                             NC2524.2
163000     PERFORM PRINT-DETAIL.                                        NC2524.2
163100     MOVE "NOT USED" TO RE-MARK.                                  NC2524.2
163200     MOVE "COMP-TEST-42" TO PAR-NAME.                             NC2524.2
163300     PERFORM PRINT-DETAIL.                                        NC2524.2
163400     MOVE "COMPUTE" TO FEATURE.                                   NC2524.2
163500 COMP-TEST-43.                                                    NC2524.2
163600     MOVE    ZEROS TO WHOLE-FIELD.                                NC2524.2
163700     COMPUTE WHOLE-FIELD =                                        NC2524.2
163800     (1 + (2 - (3 + (4 - (5 + (6 - (7 + (8 - (9 + (10 -           NC2524.2
163900     EVEN-NAME1)))))))))).                                        NC2524.2
164000     IF (WHOLE-FIELD < 10.0002) AND                               NC2524.2
164100        (WHOLE-FIELD > 9.9998) PERFORM PASS                       NC2524.2
164200         GO TO COMP-WRITE-43.                                     NC2524.2
164300     MOVE WHOLE-FIELD TO COMPUTED-18V0.                           NC2524.2
164400     MOVE 10 TO CORRECT-18V0.                                     NC2524.2
164500     PERFORM FAIL.                                                NC2524.2
164600     GO TO COMP-WRITE-43.                                         NC2524.2
164700 COMP-DELETE-43.                                                  NC2524.2
164800     PERFORM DE-LETE.                                             NC2524.2
164900 COMP-WRITE-43.                                                   NC2524.2
165000     MOVE "COMP-TEST-43" TO PAR-NAME.                             NC2524.2
165100     PERFORM PRINT-DETAIL.                                        NC2524.2
165200 COMP-TEST-44.                                                    NC2524.2
165300     MOVE    ZEROS TO WHOLE-FIELD.                                NC2524.2
165400     COMPUTE  WHOLE-FIELD =                                       NC2524.2
165500     (ONE + (TWO - (THREE + (FOUR - (FIVE + (SIX - (SEVEN +       NC2524.2
165600         (EIGHT - (NINE + (TEN - EVEN-NAME1)))))))))).            NC2524.2
165700        IF WHOLE-FIELD = 10    PERFORM PASS                       NC2524.2
165800         GO TO COMP-WRITE-44.                                     NC2524.2
165900     MOVE WHOLE-FIELD TO COMPUTED-18V0.                           NC2524.2
166000     MOVE 10 TO CORRECT-18V0.                                     NC2524.2
166100     PERFORM FAIL.                                                NC2524.2
166200     GO TO COMP-WRITE-44.                                         NC2524.2
166300 COMP-DELETE-44.                                                  NC2524.2
166400     PERFORM DE-LETE.                                             NC2524.2
166500 COMP-WRITE-44.                                                   NC2524.2
166600     MOVE "COMP-TEST-44" TO PAR-NAME.                             NC2524.2
166700     PERFORM PRINT-DETAIL.                                        NC2524.2
166800 COMP-INT-045.                                                    NC2524.2
166900     MOVE    "COMPUTE SERIES"  TO FEATURE.                        NC2524.2
167000     MOVE    "COMP-TEST-045"  TO PAR-NAME.                        NC2524.2
167100 COMP-TEST-045.                                                   NC2524.2
167200     COMPUTE  WRK-DS-05V00-0002                                   NC2524.2
167300              WRK-DS-04V01-0005   ROUNDED                         NC2524.2
167400              WRK-DS-03V04-0003F-0014  (2, 2, 2) =  174 / 16.     NC2524.2
167500*                                                                 NC2524.2
167600*    IDENTIFIER SERIES  -  WITH AND WITHOUT ROUNDED  -            NC2524.2
167700*             SUBSCRIPTED DATA ITEM.                              NC2524.2
167800*                                                                 NC2524.2
167900     MOVE "COMP-TEST-045-1" TO PAR-NAME.                          NC2524.2
168000        IF WRK-DS-05V00-0002 NOT = 10                             NC2524.2
168100              MOVE      +00010  TO  CORRECT-N                     NC2524.2
168200              MOVE     WRK-DS-05V00-0002   TO COMPUTED-N          NC2524.2
168300              PERFORM COMP-WRITE-045 GO TO COMP-TEST-045-2.       NC2524.2
168400     PERFORM PASS.  PERFORM COMP-WRITE-045.                       NC2524.2
168500 COMP-TEST-045-2.                                                 NC2524.2
168600     MOVE "COMP-TEST-045-2" TO PAR-NAME.                          NC2524.2
168700     IF (WRK-DS-04V01-0005 > 10.9002180) OR                       NC2524.2
168800        (WRK-DS-04V01-0005 < 10.8997820) PERFORM FAIL             NC2524.2
168900              MOVE +10.9 TO CORRECT-N                             NC2524.2
169000              MOVE WRK-DS-04V01-0005 TO COMPUTED-N                NC2524.2
169100              PERFORM COMP-WRITE-045 GO TO COMP-TEST-045-3.       NC2524.2
169200     PERFORM PASS.  PERFORM COMP-WRITE-045.                       NC2524.2
169300 COMP-TEST-045-3.                                                 NC2524.2
169400     MOVE "COMP-TEST-045-3" TO PAR-NAME.                          NC2524.2
169500     IF (WRK-DS-03V04-0003F-0014 (2, 2, 2) > 10.87521750) OR      NC2524.2
169600        (WRK-DS-03V04-0003F-0014 (2, 2, 2) < 10.87479250)         NC2524.2
169700             PERFORM FAIL MOVE +010.8750 TO CORRECT-N             NC2524.2
169800             MOVE WRK-DS-03V04-0003F-0014 (2, 2, 2) TO COMPUTED-N NC2524.2
169900             GO TO COMP-WRITE-045.                                NC2524.2
170000     PERFORM PASS.                                                NC2524.2
170100     GO TO      COMP-WRITE-045.                                   NC2524.2
170200 COMP-DELETE-045.                                                 NC2524.2
170300     PERFORM  DE-LETE.                                            NC2524.2
170400 COMP-WRITE-045.                                                  NC2524.2
170500     PERFORM  PRINT-DETAIL.                                       NC2524.2
170600 COMP-TEST-045-EXIT.                                              NC2524.2
170700     EXIT.                                                        NC2524.2
170800 CCVS-EXIT SECTION.                                               NC2524.2
170900 CCVS-999999.                                                     NC2524.2
171000     GO TO CLOSE-FILES.                                           NC2524.2
*END-OF,NC252A                                                                  
*HEADER,COBOL,NC253A                                                            
000100 IDENTIFICATION DIVISION.                                         NC2534.2
000200 PROGRAM-ID.                                                      NC2534.2
000300     NC253A.                                                      NC2534.2
000400****************************************************************  NC2534.2
000500*                                                              *  NC2534.2
000600*    VALIDATION FOR:-                                          *  NC2534.2
000700*                                                              *  NC2534.2
000800*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2534.2
000900*                                                              *  NC2534.2
001000*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC2534.2
001100*                                                              *  NC2534.2
001200****************************************************************  NC2534.2
001300*                                                              *  NC2534.2
001400*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  NC2534.2
001500*                                                              *  NC2534.2
001600*        X-55  - SYSTEM PRINTER NAME.                          *  NC2534.2
001700*        X-82  - SOURCE COMPUTER NAME.                         *  NC2534.2
001800*        X-83  - OBJECT COMPUTER NAME.                         *  NC2534.2
001900*                                                              *  NC2534.2
002000****************************************************************  NC2534.2
002100                                                                  NC2534.2
002200*                                                                 NC2534.2
002300*    PROGRAM NC202A TESTS FORMAT3 OF THE SUBTRACT STATEMENT.      NC2534.2
002400*                                                                 NC2534.2
002500*                                                                 NC2534.2
002600 ENVIRONMENT DIVISION.                                            NC2534.2
002700 CONFIGURATION SECTION.                                           NC2534.2
002800 SOURCE-COMPUTER.                                                 NC2534.2
002900     XXXXX082.                                                    NC2534.2
003000 OBJECT-COMPUTER.                                                 NC2534.2
003100     XXXXX083.                                                    NC2534.2
003200 INPUT-OUTPUT SECTION.                                            NC2534.2
003300 FILE-CONTROL.                                                    NC2534.2
003400     SELECT PRINT-FILE ASSIGN TO                                  NC2534.2
003500     XXXXX055.                                                    NC2534.2
003600 DATA DIVISION.                                                   NC2534.2
003700 FILE SECTION.                                                    NC2534.2
003800 FD  PRINT-FILE.                                                  NC2534.2
003900 01  PRINT-REC PICTURE X(120).                                    NC2534.2
004000 01  DUMMY-RECORD PICTURE X(120).                                 NC2534.2
004100 WORKING-STORAGE SECTION.                                         NC2534.2
004200 01  TABLE1.                                                      NC2534.2
004300     02  RECORD1                 PICTURE 99.                      NC2534.2
004400     02  RECORD2                 PICTURE 99                       NC2534.2
004500                                 OCCURS 2 TIMES                   NC2534.2
004600                                 INDEXED BY INDEX1.               NC2534.2
004700     02  RECORD3                 PICTURE 99.                      NC2534.2
004800 01  TABLE2.                                                      NC2534.2
004900     02  RECORD1                 PICTURE 99.                      NC2534.2
005000     02  RECORD2                 PICTURE 99                       NC2534.2
005100                                 OCCURS 2 TIMES                   NC2534.2
005200                                 INDEXED BY INDEX2.               NC2534.2
005300     02  RECORD3                 PICTURE 99.                      NC2534.2
005400 77  WRK-AN-00001                PICTURE A.                       NC2534.2
005500 77  WRK-XN-00001                PICTURE X.                       NC2534.2
005600 77  WRK-DS-01V00                PICTURE S9.                      NC2534.2
005700 77  WRK-DS-02V00                PICTURE S99.                     NC2534.2
005800 77  WRK-DS-06V06                PICTURE S9(6)V9(6).              NC2534.2
005900 77  WRK-DS-05V00                PICTURE S9(5).                   NC2534.2
006000 77  AZERO-DS-05V05              PICTURE S9(5)V9(5) VALUE ZERO.   NC2534.2
006100 77  WRK-DS-09V09                PICTURE S9(9)V9(9).              NC2534.2
006200 77  WRK-DS-18V00-S REDEFINES WRK-DS-09V09 PICTURE S9(18).        NC2534.2
006300 77  A18ONES-DS-09V09            PICTURE S9(9)V9(9)               NC2534.2
006400                                 VALUE 111111111.111111111.       NC2534.2
006500 77  WRK-DS-18V00       PICTURE S9(18) VALUE 111111111111111111.  NC2534.2
006600 77  A05ONES-DS-05V00            PICTURE S9(5) VALUE 11111.       NC2534.2
006700 77  A99-DS-02V00                PICTURE S99   VALUE 99.          NC2534.2
006800 77  WRK-DS-03V00                PICTURE S999.                    NC2534.2
006900 77  WRK-DS-06V00                PICTURE S9(6).                   NC2534.2
007000 77  WRK-DS-0201P                PICTURE S99P.                    NC2534.2
007100 77  WRK-DS-03V10                PICTURE S9(3)V9(10).             NC2534.2
007200 77  ADD-1                   PICTURE S9(8)V99  VALUE 1.           NC2534.2
007300 77  ADD-2                   PICTURE S9(6)V9(4) VALUE 1.          NC2534.2
007400 77  ADD-3                   PICTURE S9(5)     VALUE -1.          NC2534.2
007500 77  ADD-4                   PICTURE 9         VALUE 9.           NC2534.2
007600 77  ADD-5                   PICTURE 9         VALUE 9.           NC2534.2
007700 77  ADD-6                   PICTURE 9(5)      VALUE 99999.       NC2534.2
007800 77  ADD-7                   PICTURE 9         VALUE 1.           NC2534.2
007900 77  ADD-8                   PICTURE 9.                           NC2534.2
008000 77  ADD-9                   PICTURE S9(8)V99  VALUE 5.9.         NC2534.2
008100 77  ADD-10                  PICTURE 9(5)      VALUE 52800.       NC2534.2
008200 77  ADD-11                  PICTURE 99999.                       NC2534.2
008300 77  ADD-12                  PICTURE PP9       VALUE .001.        NC2534.2
008400 77  ADD-13                  PICTURE 9PP       VALUE 100.         NC2534.2
008500 77  ADD-14                  PICTURE 999V999.                     NC2534.2
008600 77  W-1                     PICTURE IS 9.                        NC2534.2
008700 77  W-2                     PICTURE IS 99.                       NC2534.2
008800 77  W-3                     PICTURE IS 999.                      NC2534.2
008900 77  W-4                PICTURE 9  VALUE 0.                       NC2534.2
009000 77  W-6                     PICTURE IS 999    VALUE IS ZERO.     NC2534.2
009100 77  W-9                     PICTURE 999.                         NC2534.2
009200 77  D-5                PICTURE S999  VALUE -1.                   NC2534.2
009300 77  D-9                PICTURE 9(4)V9(4)  VALUE 111.1189.        NC2534.2
009400 77  ONE                PICTURE 9  VALUE 1.                       NC2534.2
009500 77  TWO                PICTURE S9  VALUE 2.                      NC2534.2
009600 77  THREE              PICTURE S9  VALUE 3.                      NC2534.2
009700 77  FOUR               PICTURE S9  VALUE 4.                      NC2534.2
009800 77  FIVE               PICTURE S9  VALUE 5.                      NC2534.2
009900 77  SIX                PICTURE S9  VALUE 6.                      NC2534.2
010000 77  SEVEN              PICTURE S9  VALUE 7.                      NC2534.2
010100 77  EIGHT              PICTURE 9  VALUE 8.                       NC2534.2
010200 77  NINE               PICTURE S9  VALUE 9.                      NC2534.2
010300 77  TEN                PICTURE S99  VALUE 10.                    NC2534.2
010400 77  FIFTEEN            PICTURE S99  VALUE 15.                    NC2534.2
010500 77  TWENTY             PICTURE S99  VALUE 20.                    NC2534.2
010600 77  TWENTY-5           PICTURE S99  VALUE 25.                    NC2534.2
010700 01  WRK-DS-09V00                 PICTURE S9(9)  VALUE ZERO.      NC2534.2
010800 01  GRP-FOR-ADD-CORR-1.                                          NC2534.2
010900     02  GRP-SUBTRACT-CORR-1.                                     NC2534.2
011000     03  FILLER                  PICTURE S99  VALUE 91.           NC2534.2
011100     03  ADD-CORR-2              PICTURE S99  VALUE 22.           NC2534.2
011200     03  ADD-CORR-1              PICTURE S99 VALUE 11.            NC2534.2
011300     03  ADD-CORR-A              PICTURE S99 VALUE 93.            NC2534.2
011400     03  ADD-CORR-4              PICTURE S99 VALUE 44.            NC2534.2
011500     03  ADD-CORR-3              PICTURE S99 VALUE 33.            NC2534.2
011600     03  ADD-CORR-6              PICTURE S99 VALUE 66.            NC2534.2
011700     03  ADD-CORR-5              PICTURE S99 VALUE 55.            NC2534.2
011800     03  ADD-CORR-8              PICTURE S99 VALUE 88.            NC2534.2
011900     03  ADD-CORR-7              PICTURE S99 VALUE 77.            NC2534.2
012000     03  ADD-CORR-9              PICTURE S99 VALUE 99.            NC2534.2
012100 01  GRP-FOR-ADD-CORR-R.                                          NC2534.2
012200     02  GRP-SUBTRACT-CORR-1.                                     NC2534.2
012300     05  ADD-CORR-1              PICTURE 99.                      NC2534.2
012400     05  ADD-CORR-2              PICTURE 99.                      NC2534.2
012500     05  ADD-CORR-3              PICTURE 99.                      NC2534.2
012600     05  ADD-CORR-4              PICTURE 99.                      NC2534.2
012700     05  ADD-CORR-5              PICTURE 9P.                      NC2534.2
012800     05  ADD-CORR-6              PICTURE 999.                     NC2534.2
012900     05  ADD-CORR-7              PICTURE 99.                      NC2534.2
013000     05  ADD-CORR-8              PICTURE 99.                      NC2534.2
013100     05  ADD-CORR-9              PICTURE 99.                      NC2534.2
013200     05  FILLER                  PICTURE 99.                      NC2534.2
013300 01  GRP-FOR-ADD-CORR-2.                                          NC2534.2
013400     02  GRP-ADD-SUB-CORR.                                        NC2534.2
013500     03  GRP-SUBTRACT-CORR-1.                                     NC2534.2
013600     04  ADD-CORR-1              PICTURE S99  VALUE 11.           NC2534.2
013700     04  ADD-CORR-2              PICTURE S99  VALUE 22.           NC2534.2
013800     04  ADD-CORR-5              PICTURE S99  VALUE 55.           NC2534.2
013900     04  ADD-CORR-4              PICTURE S99  VALUE 44.           NC2534.2
014000     04  ADD-CORR-3              PICTURE S99  VALUE 33.           NC2534.2
014100     04  ADD-CORR-6              PICTURE S99  VALUE 66.           NC2534.2
014200     04  ADD-CORR-7              PICTURE S99  VALUE 77.           NC2534.2
014300     04  ADD-CORR-8              PICTURE S99  VALUE 88.           NC2534.2
014400     04  ADD-CORR-9              PICTURE S99  VALUE 99.           NC2534.2
014500     04  ADD-CORR-B              PICTURE S99  VALUE 92.           NC2534.2
014600     04  ADD-CORR-0              PICTURE S99  VALUE 00.           NC2534.2
014700 01  GRP-FOR-ADD-CORR-A.                                          NC2534.2
014800     02  GRP-SUBTRACT-CORR-3.                                     NC2534.2
014900         03  GRP-SUBTRACT-CORR-1.                                 NC2534.2
015000             05  ADD-CORR-4      PICTURE S999   VALUE 044.        NC2534.2
015100             05  ADD-CORR-3      PICTURE S999   VALUE 033.        NC2534.2
015200             05  ADD-CORR-2      PICTURE S999   VALUE 022.        NC2534.2
015300             05  ADD-CORR-1      PICTURE S999   VALUE 111.        NC2534.2
015400 01  ADD-15.                                                      NC2534.2
015500     02 FIELD1               PICTURE 99999     VALUE 1.           NC2534.2
015600     02 FIELD2               PICTURE 999V99    VALUE 32.1.        NC2534.2
015700     02 FIELD3               PICTURE 999V9     VALUE 123.4.       NC2534.2
015800 01  ADD-16.                                                      NC2534.2
015900     02 FIELD1               PICTURE 99999     VALUE 99999.       NC2534.2
016000     02 FIELD2               PICTURE 999V99    VALUE 745.67.      NC2534.2
016100     02 FIELD3               PICTURE 999V9     VALUE 432.1.       NC2534.2
016200 01  SUBTRACT-DATA.                                               NC2534.2
016300     02 SUBTR-1              PICTURE 9         VALUE 1.           NC2534.2
016400     02 SUBTR-2              PICTURE S99       VALUE 99.          NC2534.2
016500     02 SUBTR-3              PICTURE S9V99     VALUE -1.          NC2534.2
016600     02 SUBTR-4              PICTURE SPP9      VALUE .001.        NC2534.2
016700     02 SUBTR-5              PICTURE S9PP      VALUE 100.         NC2534.2
016800     02  SUBTR-6                  PICTURE 9 VALUE 1.              NC2534.2
016900     02  SUBTR-7                  PICTURE S99  VALUE 99.          NC2534.2
017000     02  SUBTR-8                  PICTURE S9V99  VALUE -9.99.     NC2534.2
017100     02 SUBTR-9              PICTURE SV999.                       NC2534.2
017200     02  SUBTR-10                 PICTURE S999  VALUE 100.        NC2534.2
017300     02 SUBTR-11             PICTURE S999V999.                    NC2534.2
017400     02 SUBTR-12.                                                 NC2534.2
017500     03 SUBTR-13             PICTURE 9         VALUE 1.           NC2534.2
017600     03 SUBTR-14             PICTURE S9V999    VALUE -1.725.      NC2534.2
017700     03 SUBTR-15             PICTURE S99V99    VALUE 76.76.       NC2534.2
017800     02 SUBTR-16.                                                 NC2534.2
017900     03 SUBTR-13             PICTURE 9         VALUE 2.           NC2534.2
018000     03 SUBTR-14             PICTURE S9V99     VALUE .23.         NC2534.2
018100     03 SUBTR-15             PICTURE S9V99     VALUE 1.           NC2534.2
018200 01  CORR-DATA-1.                                                 NC2534.2
018300     03 XYZ-1                PICTURE IS 99     VALUE IS ZERO.     NC2534.2
018400     03 XYZ-2                PICTURE IS 99     VALUE IS ZERO.     NC2534.2
018500     03 XYZ-3                PICTURE IS 99     VALUE IS ZERO.     NC2534.2
018600     03 XYZ-4                PICTURE IS 99     VALUE IS ZERO.     NC2534.2
018700     03 XYZ-5                PICTURE IS 99     VALUE IS ZERO.     NC2534.2
018800     03 XYZ-6                PICTURE IS 99     VALUE IS ZERO.     NC2534.2
018900 01  CORR-DATA-2.                                                 NC2534.2
019000     03 XYZ-1                PICTURE IS 99     VALUE IS ZERO.     NC2534.2
019100     03 XYZ-2                PICTURE IS 99     VALUE IS ZERO.     NC2534.2
019200     03 XYZ-3                PICTURE IS 99     VALUE IS ZERO.     NC2534.2
019300     03 XYZ-4                PICTURE IS 99     VALUE IS ZERO.     NC2534.2
019400     03 XYZ-5                PICTURE IS 99     VALUE IS ZERO.     NC2534.2
019500     03 XYZ-6                PICTURE IS 99     VALUE IS ZERO.     NC2534.2
019600 01  CORR-DATA-3.                                                 NC2534.2
019700     03 XYZ-4                PICTURE IS 99     VALUE IS ZERO.     NC2534.2
019800     03 XYZ-3                PICTURE IS 99     VALUE IS ZERO.     NC2534.2
019900     03 XYZ-6                PICTURE IS 99     VALUE IS ZERO.     NC2534.2
020000     03 XYZ-5                PICTURE IS 99     VALUE IS ZERO.     NC2534.2
020100     03 XYZ-2                PICTURE IS 99     VALUE IS ZERO.     NC2534.2
020200     03 XYZ-1                PICTURE IS 99     VALUE IS ZERO.     NC2534.2
020300 01  CORR-DATA-4.                                                 NC2534.2
020400     03 XYZ-11               PICTURE IS 99.                       NC2534.2
020500     03 XYZ-12               PICTURE IS 99.                       NC2534.2
020600     03 XYZ-13               PICTURE IS 99.                       NC2534.2
020700     03 XYZ-14               PICTURE IS 99.                       NC2534.2
020800     03 XYZ-15               PICTURE IS 99.                       NC2534.2
020900     03 XYZ-16               PICTURE IS 99.                       NC2534.2
021000 01  CORR-DATA-5.                                                 NC2534.2
021100     03 XYZ-1                PICTURE 99.                          NC2534.2
021200     03 XYZ-2                PICTURE 99.                          NC2534.2
021300     03 XYZ-13               PICTURE IS 99.                       NC2534.2
021400     03 XYZ-14               PICTURE IS 99.                       NC2534.2
021500     03 FILLER               PICTURE IS 99.                       NC2534.2
021600     03 XYZ-11               PICTURE IS 99.                       NC2534.2
021700     03 XYZ-12               PICTURE IS 99.                       NC2534.2
021800 01  CORR-DATA-6.                                                 NC2534.2
021900     03 XYZ-11               PICTURE IS 99.                       NC2534.2
022000     03 XYZ-12               PICTURE IS 99.                       NC2534.2
022100     03 FILLER               PICTURE IS 99.                       NC2534.2
022200     03 XYZ-1                PICTURE IS 99.                       NC2534.2
022300     03 XYZ-2                PICTURE IS 9(2).                     NC2534.2
022400     03 FILLER               PICTURE IS 99.                       NC2534.2
022500 01  CORR-DATA-7.                                                 NC2534.2
022600     02 XYZ-1                PICTURE 99V99     VALUE 10.45.       NC2534.2
022700     02 XYZ-6                PICTURE 999V9     VALUE 100.5.       NC2534.2
022800     02 XYZ-11               PICTURE 99V9      VALUE ZERO.        NC2534.2
022900     02 XYZ-2                PICTURE 99V9      VALUE 0.9.         NC2534.2
023000 01  42-DATANAMES.                                                NC2534.2
023100     02  DNAME1   PICTURE 9      VALUE 1        COMPUTATIONAL.    NC2534.2
023200     02  DNAME2   PICTURE 99      VALUE 1  COMPUTATIONAL.         NC2534.2
023300     02  DNAME3   PICTURE 999     VALUE 1  COMPUTATIONAL.         NC2534.2
023400     02  DNAME4   PICTURE 9(4)    VALUE 1  COMPUTATIONAL.         NC2534.2
023500     02  DNAME5   PICTURE 9(5)    VALUE 1  COMPUTATIONAL.         NC2534.2
023600     02  DNAME6   PICTURE 9(6)    VALUE 1  COMPUTATIONAL.         NC2534.2
023700     02  DNAME7   PICTURE 9(7)    VALUE 1  COMPUTATIONAL.         NC2534.2
023800     02  DNAME8   PICTURE 9(8)    VALUE 1  COMPUTATIONAL.         NC2534.2
023900     02  DNAME9   PICTURE 9(9)    VALUE 1  COMPUTATIONAL.         NC2534.2
024000     02  DNAME10  PICTURE 9(10)   VALUE 1.                        NC2534.2
024100     02  DNAME11  PICTURE 9(11)   VALUE 1.                        NC2534.2
024200     02  DNAME12  PICTURE 9(12)   VALUE 1.                        NC2534.2
024300     02  DNAME13  PICTURE 9(13)   VALUE 1.                        NC2534.2
024400     02  DNAME14  PICTURE 9(14)   VALUE 1.                        NC2534.2
024500     02  DNAME15  PICTURE 9(15)   VALUE 1.                        NC2534.2
024600     02  DNAME16  PICTURE 9(16)   VALUE 1.                        NC2534.2
024700     02  DNAME17  PICTURE 9(17)   VALUE 1.                        NC2534.2
024800     02  DNAME18  PICTURE 9(18)   VALUE 1.                        NC2534.2
024900     02  DNAME19  PICTURE 9       VALUE 1.                        NC2534.2
025000     02  DNAME20  PICTURE 99      VALUE 1.                        NC2534.2
025100     02  DNAME21  PICTURE 999     VALUE 1.                        NC2534.2
025200     02  DNAME22  PICTURE 9(18)  VALUE ZERO.                      NC2534.2
025300     02  DNAME23  PICTURE 9(18)  VALUE ZERO.                      NC2534.2
025400     02  DNAME24  PICTURE 9(18)  VALUE ZERO.                      NC2534.2
025500     02  DNAME25  PICTURE 9(18)  VALUE ZERO.                      NC2534.2
025600     02  DNAME26  PICTURE 9(18)  VALUE ZERO.                      NC2534.2
025700     02  DNAME27  PICTURE 9(18)  VALUE ZERO.                      NC2534.2
025800     02  DNAME28  PICTURE 9(18)  VALUE ZERO.                      NC2534.2
025900     02  DNAME29  PICTURE 9(18)  VALUE ZERO.                      NC2534.2
026000     02  DNAME30   PICTURE 9(18)  VALUE ZERO COMPUTATIONAL.       NC2534.2
026100     02  DNAME31   PICTURE 9(18)  VALUE ZERO COMPUTATIONAL.       NC2534.2
026200     02  DNAME32   PICTURE 9(18)  VALUE ZERO COMPUTATIONAL.       NC2534.2
026300     02  DNAME33   PICTURE 9(18)  VALUE ZERO COMPUTATIONAL.       NC2534.2
026400     02  DNAME34   PICTURE 9(18)  VALUE ZERO COMPUTATIONAL.       NC2534.2
026500     02  DNAME35   PICTURE 9(18)  VALUE ZERO COMPUTATIONAL.       NC2534.2
026600     02  DNAME36   PICTURE 9(18)  VALUE ZERO COMPUTATIONAL.       NC2534.2
026700     02  DNAME37   PICTURE 9(18)  VALUE ZERO COMPUTATIONAL.       NC2534.2
026800     02  DNAME38   PICTURE 9(18)  VALUE ZERO COMPUTATIONAL.       NC2534.2
026900     02  DNAME39   PICTURE 9(18)  VALUE ZERO COMPUTATIONAL.       NC2534.2
027000     02  DNAME40   PICTURE 9(18)  VALUE ZERO COMPUTATIONAL.       NC2534.2
027100     02  DNAME41   PICTURE 9(18)  VALUE ZERO COMPUTATIONAL.       NC2534.2
027200     02  DNAME42   PICTURE 9(18)  VALUE ZERO COMPUTATIONAL.       NC2534.2
027300 01  TEST-RESULTS.                                                NC2534.2
027400     02 FILLER                   PIC X      VALUE SPACE.          NC2534.2
027500     02 FEATURE                  PIC X(20)  VALUE SPACE.          NC2534.2
027600     02 FILLER                   PIC X      VALUE SPACE.          NC2534.2
027700     02 P-OR-F                   PIC X(5)   VALUE SPACE.          NC2534.2
027800     02 FILLER                   PIC X      VALUE SPACE.          NC2534.2
027900     02  PAR-NAME.                                                NC2534.2
028000       03 FILLER                 PIC X(19)  VALUE SPACE.          NC2534.2
028100       03  PARDOT-X              PIC X      VALUE SPACE.          NC2534.2
028200       03 DOTVALUE               PIC 99     VALUE ZERO.           NC2534.2
028300     02 FILLER                   PIC X(8)   VALUE SPACE.          NC2534.2
028400     02 RE-MARK                  PIC X(61).                       NC2534.2
028500 01  TEST-COMPUTED.                                               NC2534.2
028600     02 FILLER                   PIC X(30)  VALUE SPACE.          NC2534.2
028700     02 FILLER                   PIC X(17)  VALUE                 NC2534.2
028800            "       COMPUTED=".                                   NC2534.2
028900     02 COMPUTED-X.                                               NC2534.2
029000     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          NC2534.2
029100     03 COMPUTED-N               REDEFINES COMPUTED-A             NC2534.2
029200                                 PIC -9(9).9(9).                  NC2534.2
029300     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         NC2534.2
029400     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     NC2534.2
029500     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     NC2534.2
029600     03       CM-18V0 REDEFINES COMPUTED-A.                       NC2534.2
029700         04 COMPUTED-18V0                    PIC -9(18).          NC2534.2
029800         04 FILLER                           PIC X.               NC2534.2
029900     03 FILLER PIC X(50) VALUE SPACE.                             NC2534.2
030000 01  TEST-CORRECT.                                                NC2534.2
030100     02 FILLER PIC X(30) VALUE SPACE.                             NC2534.2
030200     02 FILLER PIC X(17) VALUE "       CORRECT =".                NC2534.2
030300     02 CORRECT-X.                                                NC2534.2
030400     03 CORRECT-A                  PIC X(20) VALUE SPACE.         NC2534.2
030500     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      NC2534.2
030600     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         NC2534.2
030700     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     NC2534.2
030800     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     NC2534.2
030900     03      CR-18V0 REDEFINES CORRECT-A.                         NC2534.2
031000         04 CORRECT-18V0                     PIC -9(18).          NC2534.2
031100         04 FILLER                           PIC X.               NC2534.2
031200     03 FILLER PIC X(2) VALUE SPACE.                              NC2534.2
031300     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     NC2534.2
031400 01  CCVS-C-1.                                                    NC2534.2
031500     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PANC2534.2
031600-    "SS  PARAGRAPH-NAME                                          NC2534.2
031700-    "       REMARKS".                                            NC2534.2
031800     02 FILLER                     PIC X(20)    VALUE SPACE.      NC2534.2
031900 01  CCVS-C-2.                                                    NC2534.2
032000     02 FILLER                     PIC X        VALUE SPACE.      NC2534.2
032100     02 FILLER                     PIC X(6)     VALUE "TESTED".   NC2534.2
032200     02 FILLER                     PIC X(15)    VALUE SPACE.      NC2534.2
032300     02 FILLER                     PIC X(4)     VALUE "FAIL".     NC2534.2
032400     02 FILLER                     PIC X(94)    VALUE SPACE.      NC2534.2
032500 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       NC2534.2
032600 01  REC-CT                        PIC 99       VALUE ZERO.       NC2534.2
032700 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       NC2534.2
032800 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       NC2534.2
032900 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       NC2534.2
033000 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       NC2534.2
033100 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       NC2534.2
033200 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       NC2534.2
033300 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      NC2534.2
033400 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       NC2534.2
033500 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     NC2534.2
033600 01  CCVS-H-1.                                                    NC2534.2
033700     02  FILLER                    PIC X(39)    VALUE SPACES.     NC2534.2
033800     02  FILLER                    PIC X(42)    VALUE             NC2534.2
033900     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 NC2534.2
034000     02  FILLER                    PIC X(39)    VALUE SPACES.     NC2534.2
034100 01  CCVS-H-2A.                                                   NC2534.2
034200   02  FILLER                        PIC X(40)  VALUE SPACE.      NC2534.2
034300   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  NC2534.2
034400   02  FILLER                        PIC XXXX   VALUE             NC2534.2
034500     "4.2 ".                                                      NC2534.2
034600   02  FILLER                        PIC X(28)  VALUE             NC2534.2
034700            " COPY - NOT FOR DISTRIBUTION".                       NC2534.2
034800   02  FILLER                        PIC X(41)  VALUE SPACE.      NC2534.2
034900                                                                  NC2534.2
035000 01  CCVS-H-2B.                                                   NC2534.2
035100   02  FILLER                        PIC X(15)  VALUE             NC2534.2
035200            "TEST RESULT OF ".                                    NC2534.2
035300   02  TEST-ID                       PIC X(9).                    NC2534.2
035400   02  FILLER                        PIC X(4)   VALUE             NC2534.2
035500            " IN ".                                               NC2534.2
035600   02  FILLER                        PIC X(12)  VALUE             NC2534.2
035700     " HIGH       ".                                              NC2534.2
035800   02  FILLER                        PIC X(22)  VALUE             NC2534.2
035900            " LEVEL VALIDATION FOR ".                             NC2534.2
036000   02  FILLER                        PIC X(58)  VALUE             NC2534.2
036100     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2534.2
036200 01  CCVS-H-3.                                                    NC2534.2
036300     02  FILLER                      PIC X(34)  VALUE             NC2534.2
036400            " FOR OFFICIAL USE ONLY    ".                         NC2534.2
036500     02  FILLER                      PIC X(58)  VALUE             NC2534.2
036600     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".NC2534.2
036700     02  FILLER                      PIC X(28)  VALUE             NC2534.2
036800            "  COPYRIGHT   1985 ".                                NC2534.2
036900 01  CCVS-E-1.                                                    NC2534.2
037000     02 FILLER                       PIC X(52)  VALUE SPACE.      NC2534.2
037100     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              NC2534.2
037200     02 ID-AGAIN                     PIC X(9).                    NC2534.2
037300     02 FILLER                       PIC X(45)  VALUE SPACES.     NC2534.2
037400 01  CCVS-E-2.                                                    NC2534.2
037500     02  FILLER                      PIC X(31)  VALUE SPACE.      NC2534.2
037600     02  FILLER                      PIC X(21)  VALUE SPACE.      NC2534.2
037700     02 CCVS-E-2-2.                                               NC2534.2
037800         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      NC2534.2
037900         03 FILLER                   PIC X      VALUE SPACE.      NC2534.2
038000         03 ENDER-DESC               PIC X(44)  VALUE             NC2534.2
038100            "ERRORS ENCOUNTERED".                                 NC2534.2
038200 01  CCVS-E-3.                                                    NC2534.2
038300     02  FILLER                      PIC X(22)  VALUE             NC2534.2
038400            " FOR OFFICIAL USE ONLY".                             NC2534.2
038500     02  FILLER                      PIC X(12)  VALUE SPACE.      NC2534.2
038600     02  FILLER                      PIC X(58)  VALUE             NC2534.2
038700     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".NC2534.2
038800     02  FILLER                      PIC X(13)  VALUE SPACE.      NC2534.2
038900     02 FILLER                       PIC X(15)  VALUE             NC2534.2
039000             " COPYRIGHT 1985".                                   NC2534.2
039100 01  CCVS-E-4.                                                    NC2534.2
039200     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      NC2534.2
039300     02 FILLER                       PIC X(4)   VALUE " OF ".     NC2534.2
039400     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      NC2534.2
039500     02 FILLER                       PIC X(40)  VALUE             NC2534.2
039600      "  TESTS WERE EXECUTED SUCCESSFULLY".                       NC2534.2
039700 01  XXINFO.                                                      NC2534.2
039800     02 FILLER                       PIC X(19)  VALUE             NC2534.2
039900            "*** INFORMATION ***".                                NC2534.2
040000     02 INFO-TEXT.                                                NC2534.2
040100       04 FILLER                     PIC X(8)   VALUE SPACE.      NC2534.2
040200       04 XXCOMPUTED                 PIC X(20).                   NC2534.2
040300       04 FILLER                     PIC X(5)   VALUE SPACE.      NC2534.2
040400       04 XXCORRECT                  PIC X(20).                   NC2534.2
040500     02 INF-ANSI-REFERENCE           PIC X(48).                   NC2534.2
040600 01  HYPHEN-LINE.                                                 NC2534.2
040700     02 FILLER  PIC IS X VALUE IS SPACE.                          NC2534.2
040800     02 FILLER  PIC IS X(65)    VALUE IS "************************NC2534.2
040900-    "*****************************************".                 NC2534.2
041000     02 FILLER  PIC IS X(54)    VALUE IS "************************NC2534.2
041100-    "******************************".                            NC2534.2
041200 01  CCVS-PGM-ID                     PIC X(9)   VALUE             NC2534.2
041300     "NC253A".                                                    NC2534.2
041400 PROCEDURE DIVISION.                                              NC2534.2
041500 CCVS1 SECTION.                                                   NC2534.2
041600 OPEN-FILES.                                                      NC2534.2
041700     OPEN     OUTPUT PRINT-FILE.                                  NC2534.2
041800     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   NC2534.2
041900     MOVE    SPACE TO TEST-RESULTS.                               NC2534.2
042000     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             NC2534.2
042100     GO TO CCVS1-EXIT.                                            NC2534.2
042200 CLOSE-FILES.                                                     NC2534.2
042300     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   NC2534.2
042400 TERMINATE-CCVS.                                                  NC2534.2
042500S    EXIT PROGRAM.                                                NC2534.2
042600STERMINATE-CALL.                                                  NC2534.2
042700     STOP     RUN.                                                NC2534.2
042800 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         NC2534.2
042900 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           NC2534.2
043000 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          NC2534.2
043100 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      NC2534.2
043200     MOVE "****TEST DELETED****" TO RE-MARK.                      NC2534.2
043300 PRINT-DETAIL.                                                    NC2534.2
043400     IF REC-CT NOT EQUAL TO ZERO                                  NC2534.2
043500             MOVE "." TO PARDOT-X                                 NC2534.2
043600             MOVE REC-CT TO DOTVALUE.                             NC2534.2
043700     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      NC2534.2
043800     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               NC2534.2
043900        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 NC2534.2
044000          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 NC2534.2
044100     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              NC2534.2
044200     MOVE SPACE TO CORRECT-X.                                     NC2534.2
044300     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         NC2534.2
044400     MOVE     SPACE TO RE-MARK.                                   NC2534.2
044500 HEAD-ROUTINE.                                                    NC2534.2
044600     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC2534.2
044700     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  NC2534.2
044800     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC2534.2
044900     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  NC2534.2
045000 COLUMN-NAMES-ROUTINE.                                            NC2534.2
045100     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2534.2
045200     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2534.2
045300     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        NC2534.2
045400 END-ROUTINE.                                                     NC2534.2
045500     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.NC2534.2
045600 END-RTN-EXIT.                                                    NC2534.2
045700     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2534.2
045800 END-ROUTINE-1.                                                   NC2534.2
045900      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      NC2534.2
046000      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               NC2534.2
046100      ADD PASS-COUNTER TO ERROR-HOLD.                             NC2534.2
046200*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   NC2534.2
046300      MOVE PASS-COUNTER TO CCVS-E-4-1.                            NC2534.2
046400      MOVE ERROR-HOLD TO CCVS-E-4-2.                              NC2534.2
046500      MOVE CCVS-E-4 TO CCVS-E-2-2.                                NC2534.2
046600      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           NC2534.2
046700  END-ROUTINE-12.                                                 NC2534.2
046800      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        NC2534.2
046900     IF       ERROR-COUNTER IS EQUAL TO ZERO                      NC2534.2
047000         MOVE "NO " TO ERROR-TOTAL                                NC2534.2
047100         ELSE                                                     NC2534.2
047200         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       NC2534.2
047300     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           NC2534.2
047400     PERFORM WRITE-LINE.                                          NC2534.2
047500 END-ROUTINE-13.                                                  NC2534.2
047600     IF DELETE-COUNTER IS EQUAL TO ZERO                           NC2534.2
047700         MOVE "NO " TO ERROR-TOTAL  ELSE                          NC2534.2
047800         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      NC2534.2
047900     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   NC2534.2
048000     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2534.2
048100      IF   INSPECT-COUNTER EQUAL TO ZERO                          NC2534.2
048200          MOVE "NO " TO ERROR-TOTAL                               NC2534.2
048300      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   NC2534.2
048400      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            NC2534.2
048500      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          NC2534.2
048600     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           NC2534.2
048700 WRITE-LINE.                                                      NC2534.2
048800     ADD 1 TO RECORD-COUNT.                                       NC2534.2
048900Y    IF RECORD-COUNT GREATER 50                                   NC2534.2
049000Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          NC2534.2
049100Y        MOVE SPACE TO DUMMY-RECORD                               NC2534.2
049200Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  NC2534.2
049300Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             NC2534.2
049400Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     NC2534.2
049500Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          NC2534.2
049600Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          NC2534.2
049700Y        MOVE ZERO TO RECORD-COUNT.                               NC2534.2
049800     PERFORM WRT-LN.                                              NC2534.2
049900 WRT-LN.                                                          NC2534.2
050000     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               NC2534.2
050100     MOVE SPACE TO DUMMY-RECORD.                                  NC2534.2
050200 BLANK-LINE-PRINT.                                                NC2534.2
050300     PERFORM WRT-LN.                                              NC2534.2
050400 FAIL-ROUTINE.                                                    NC2534.2
050500     IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. NC2534.2
050600     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.NC2534.2
050700     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC2534.2
050800     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   NC2534.2
050900     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2534.2
051000     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC2534.2
051100     GO TO  FAIL-ROUTINE-EX.                                      NC2534.2
051200 FAIL-ROUTINE-WRITE.                                              NC2534.2
051300     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         NC2534.2
051400     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 NC2534.2
051500     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. NC2534.2
051600     MOVE   SPACES TO COR-ANSI-REFERENCE.                         NC2534.2
051700 FAIL-ROUTINE-EX. EXIT.                                           NC2534.2
051800 BAIL-OUT.                                                        NC2534.2
051900     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   NC2534.2
052000     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           NC2534.2
052100 BAIL-OUT-WRITE.                                                  NC2534.2
052200     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  NC2534.2
052300     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 NC2534.2
052400     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   NC2534.2
052500     MOVE   SPACES TO INF-ANSI-REFERENCE.                         NC2534.2
052600 BAIL-OUT-EX. EXIT.                                               NC2534.2
052700 CCVS1-EXIT.                                                      NC2534.2
052800     EXIT.                                                        NC2534.2
052900*                                                                 NC2534.2
053000 SECT-NC253A-001 SECTION.                                         NC2534.2
053100 BUILD-TABLE1.                                                    NC2534.2
053200     MOVE 06 TO RECORD1 OF TABLE1.                                NC2534.2
053300     MOVE 01 TO RECORD2 OF TABLE1 (1).                            NC2534.2
053400     MOVE 02 TO RECORD2 OF TABLE1 (2).                            NC2534.2
053500     MOVE 07 TO RECORD3 OF TABLE1.                                NC2534.2
053600 BUILD-TABLE2.                                                    NC2534.2
053700     MOVE 08 TO RECORD1 OF TABLE2.                                NC2534.2
053800     MOVE 03 TO RECORD2 OF TABLE2 (1).                            NC2534.2
053900     MOVE 04 TO RECORD2 OF TABLE2 (2).                            NC2534.2
054000     MOVE 09 TO RECORD3 OF TABLE2.                                NC2534.2
054100*                                                                 NC2534.2
054200 SUB-INIT-F3-1.                                                   NC2534.2
054300     PERFORM END-ROUTINE.                                         NC2534.2
054400     MOVE "SUB-TEST-F3-1" TO PAR-NAME.                            NC2534.2
054500     MOVE   "VI-134 6.25.4 GR3" TO ANSI-REFERENCE.                NC2534.2
054600     MOVE   "SUBTRACT SERIES " TO FEATURE.                        NC2534.2
054700     MOVE ZERO TO GRP-FOR-ADD-CORR-R.                             NC2534.2
054800     MOVE   11  TO  ADD-CORR-1 OF GRP-FOR-ADD-CORR-1.             NC2534.2
054900     MOVE   22  TO  ADD-CORR-2 OF GRP-FOR-ADD-CORR-1.             NC2534.2
055000     MOVE   33  TO  ADD-CORR-3 OF GRP-FOR-ADD-CORR-1.             NC2534.2
055100     MOVE   44  TO  ADD-CORR-4 OF GRP-FOR-ADD-CORR-1.             NC2534.2
055200     MOVE   55  TO  ADD-CORR-5 OF GRP-FOR-ADD-CORR-1.             NC2534.2
055300     MOVE   66  TO  ADD-CORR-6 OF GRP-FOR-ADD-CORR-1.             NC2534.2
055400     MOVE   77  TO  ADD-CORR-7 OF GRP-FOR-ADD-CORR-1.             NC2534.2
055500     MOVE   88  TO  ADD-CORR-8 OF GRP-FOR-ADD-CORR-1.             NC2534.2
055600     MOVE   99  TO  ADD-CORR-9 OF GRP-FOR-ADD-CORR-1.             NC2534.2
055700 SUB-TEST-F3-1.                                                   NC2534.2
055800     SUBTRACT CORRESPONDING GRP-FOR-ADD-CORR-1 FROM               NC2534.2
055900         GRP-FOR-ADD-CORR-R.                                      NC2534.2
056000     IF GRP-FOR-ADD-CORR-R EQUAL TO "11223344506677889900"        NC2534.2
056100         PERFORM PASS                                             NC2534.2
056200         GO TO SUB-WRITE-F3-1.                                    NC2534.2
056300     GO TO SUB-FAIL-F3-1.                                         NC2534.2
056400 SUB-DELETE-F3-1.                                                 NC2534.2
056500     PERFORM DE-LETE.                                             NC2534.2
056600     GO TO SUB-WRITE-F3-1.                                        NC2534.2
056700 SUB-FAIL-F3-1.                                                   NC2534.2
056800     MOVE GRP-FOR-ADD-CORR-R TO COMPUTED-A.                       NC2534.2
056900     MOVE "11223344506677889900" TO CORRECT-A.                    NC2534.2
057000     PERFORM FAIL.                                                NC2534.2
057100 SUB-WRITE-F3-1.                                                  NC2534.2
057200     PERFORM PRINT-DETAIL.                                        NC2534.2
057300*                                                                 NC2534.2
057400 SUB-INIT-F3-2.                                                   NC2534.2
057500     MOVE "SUB-TEST-F3-2" TO PAR-NAME.                            NC2534.2
057600     MOVE ZERO TO GRP-FOR-ADD-CORR-R.                             NC2534.2
057700 SUB-TEST-F3-2.                                                   NC2534.2
057800     SUBTRACT CORRESPONDING GRP-ADD-SUB-CORR FROM                 NC2534.2
057900         GRP-FOR-ADD-CORR-R ROUNDED.                              NC2534.2
058000     IF GRP-FOR-ADD-CORR-R EQUAL TO "11223344606677889900"        NC2534.2
058100         PERFORM PASS                                             NC2534.2
058200         GO TO SUB-WRITE-F3-2.                                    NC2534.2
058300     GO TO SUB-FAIL-F3-2.                                         NC2534.2
058400 SUB-DELETE-F3-2.                                                 NC2534.2
058500     PERFORM DE-LETE.                                             NC2534.2
058600     GO TO SUB-WRITE-F3-2.                                        NC2534.2
058700 SUB-FAIL-F3-2.                                                   NC2534.2
058800     MOVE GRP-FOR-ADD-CORR-R TO COMPUTED-A.                       NC2534.2
058900     MOVE "11223344606677889900" TO CORRECT-A.                    NC2534.2
059000     PERFORM FAIL.                                                NC2534.2
059100 SUB-WRITE-F3-2.                                                  NC2534.2
059200     PERFORM PRINT-DETAIL.                                        NC2534.2
059300*                                                                 NC2534.2
059400 SUB-INIT-F3-3.                                                   NC2534.2
059500     MOVE  1      TO SUBTR-13 OF SUBTR-12.                        NC2534.2
059600     MOVE -1.725  TO SUBTR-14 OF SUBTR-12.                        NC2534.2
059700     MOVE 76.76   TO SUBTR-15 OF SUBTR-12.                        NC2534.2
059800     MOVE  2      TO SUBTR-13 OF SUBTR-16.                        NC2534.2
059900     MOVE  0.23   TO SUBTR-14 OF SUBTR-16.                        NC2534.2
060000     MOVE  1      TO SUBTR-15 OF SUBTR-16.                        NC2534.2
060100 SUB-INIT-F3-3-1.                                                 NC2534.2
060200     MOVE "SUB-TEST-F3-3-1" TO PAR-NAME.                          NC2534.2
060300     MOVE SPACE TO WRK-AN-00001.                                  NC2534.2
060400 SUB-TEST-F3-3-1.                                                 NC2534.2
060500     SUBTRACT CORRESPONDING SUBTR-12 FROM SUBTR-16 ROUNDED ON     NC2534.2
060600     SIZE ERROR                                                   NC2534.2
060700           MOVE "G" TO WRK-AN-00001.                              NC2534.2
060800     IF WRK-AN-00001 EQUAL TO "G"                                 NC2534.2
060900           PERFORM PASS                                           NC2534.2
061000           GO TO SUB-WRITE-F3-3-1.                                NC2534.2
061100     GO TO SUB-FAIL-F3-3-1.                                       NC2534.2
061200 SUB-DELETE-F3-3-1.                                               NC2534.2
061300     PERFORM DE-LETE.                                             NC2534.2
061400     GO TO SUB-WRITE-F3-3-1.                                      NC2534.2
061500 SUB-FAIL-F3-3-1.                                                 NC2534.2
061600     PERFORM FAIL.                                                NC2534.2
061700     MOVE "ON SIZE ERROR SHOULD BE EXECUTED" TO RE-MARK.          NC2534.2
061800 SUB-WRITE-F3-3-1.                                                NC2534.2
061900     PERFORM PRINT-DETAIL.                                        NC2534.2
062000*                                                                 NC2534.2
062100 SUB-INIT-F3-3-2.                                                 NC2534.2
062200     MOVE "SUB-TEST-F3-3-2" TO PAR-NAME.                          NC2534.2
062300 SUB-TEST-F3-3-2.                                                 NC2534.2
062400     IF SUBTR-13 OF SUBTR-16 NOT EQUAL TO 1                       NC2534.2
062500             GO TO SUB-FAIL-F3-3-2.                               NC2534.2
062600     PERFORM PASS.                                                NC2534.2
062700     GO TO SUB-WRITE-F3-3-2.                                      NC2534.2
062800 SUB-DELETE-F3-3-2.                                               NC2534.2
062900     PERFORM DE-LETE.                                             NC2534.2
063000     GO TO SUB-WRITE-F3-3-2.                                      NC2534.2
063100 SUB-FAIL-F3-3-2.                                                 NC2534.2
063200     PERFORM FAIL.                                                NC2534.2
063300     MOVE SUBTR-13 OF SUBTR-16 TO COMPUTED-N.                     NC2534.2
063400     MOVE "+1" TO CORRECT-A.                                      NC2534.2
063500     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
063600        TO RE-MARK.                                               NC2534.2
063700 SUB-WRITE-F3-3-2.                                                NC2534.2
063800     PERFORM PRINT-DETAIL.                                        NC2534.2
063900*                                                                 NC2534.2
064000 SUB-INIT-F3-3-3.                                                 NC2534.2
064100     MOVE "SUB-TEST-F3-3-3" TO PAR-NAME.                          NC2534.2
064200 SUB-TEST-F3-3-3.                                                 NC2534.2
064300     IF SUBTR-14 OF SUBTR-16 IS NOT EQUAL TO 1.96                 NC2534.2
064400             GO TO SUB-FAIL-F3-3-3.                               NC2534.2
064500     PERFORM PASS.                                                NC2534.2
064600     GO TO SUB-WRITE-F3-3-3.                                      NC2534.2
064700 SUB-DELETE-F3-3-3.                                               NC2534.2
064800     PERFORM DE-LETE.                                             NC2534.2
064900     GO TO SUB-WRITE-F3-3-3.                                      NC2534.2
065000 SUB-FAIL-F3-3-3.                                                 NC2534.2
065100     PERFORM FAIL.                                                NC2534.2
065200     MOVE SUBTR-14 OF SUBTR-16 TO COMPUTED-N.                     NC2534.2
065300     MOVE "+1.96" TO CORRECT-A.                                   NC2534.2
065400     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
065500        TO RE-MARK.                                               NC2534.2
065600 SUB-WRITE-F3-3-3.                                                NC2534.2
065700     PERFORM PRINT-DETAIL.                                        NC2534.2
065800*                                                                 NC2534.2
065900 SUB-INIT-3-3-4.                                                  NC2534.2
066000     MOVE "SUB-TEST-3-3-4" TO PAR-NAME.                           NC2534.2
066100 SUB-TEST-3-3-4.                                                  NC2534.2
066200     IF SUBTR-15 OF SUBTR-16 NOT EQUAL TO 1                       NC2534.2
066300            GO TO SUB-FAIL-3-3-4.                                 NC2534.2
066400     PERFORM PASS                                                 NC2534.2
066500     GO TO SUB-WRITE-3-3-4.                                       NC2534.2
066600 SUB-DELETE-3-3-4.                                                NC2534.2
066700     PERFORM DE-LETE.                                             NC2534.2
066800     GO TO SUB-WRITE-3-3-4.                                       NC2534.2
066900 SUB-FAIL-3-3-4.                                                  NC2534.2
067000     PERFORM FAIL.                                                NC2534.2
067100     MOVE SUBTR-15 OF SUBTR-16 TO COMPUTED-N.                     NC2534.2
067200     MOVE "+1" TO CORRECT-A.                                      NC2534.2
067300     MOVE "WRONGLY AFFECTED BY SIZE ERROR" TO RE-MARK.            NC2534.2
067400 SUB-WRITE-3-3-4.                                                 NC2534.2
067500     PERFORM PRINT-DETAIL.                                        NC2534.2
067600*                                                                 NC2534.2
067700 SUB-INIT-F3-4.                                                   NC2534.2
067800     MOVE "SUB-TEST-F3-4" TO PAR-NAME.                            NC2534.2
067900     MOVE "050506060000" TO CORR-DATA-2.                          NC2534.2
068000     MOVE "999999999999" TO CORR-DATA-3.                          NC2534.2
068100 SUB-TEST-F3-4.                                                   NC2534.2
068200     SUBTRACT CORRESPONDING CORR-DATA-2 FROM CORR-DATA-3.         NC2534.2
068300     IF CORR-DATA-3 EQUAL TO "939399999494"                       NC2534.2
068400         PERFORM PASS                                             NC2534.2
068500         GO TO SUB-WRITE-F3-4.                                    NC2534.2
068600     GO TO SUB-FAIL-F3-4.                                         NC2534.2
068700 SUB-DELETE-F3-4.                                                 NC2534.2
068800     PERFORM DE-LETE.                                             NC2534.2
068900     GO TO SUB-WRITE-F3-4.                                        NC2534.2
069000 SUB-FAIL-F3-4.                                                   NC2534.2
069100     MOVE 939399999494 TO CORRECT-A.                              NC2534.2
069200     MOVE CORR-DATA-3 TO COMPUTED-A.                              NC2534.2
069300     PERFORM FAIL.                                                NC2534.2
069400 SUB-WRITE-F3-4.                                                  NC2534.2
069500     PERFORM PRINT-DETAIL.                                        NC2534.2
069600*                                                                 NC2534.2
069700 SUB-INIT-F3-5.                                                   NC2534.2
069800     MOVE "SUB-TEST-F3-5" TO PAR-NAME.                            NC2534.2
069900     MOVE 999955995511 TO CORR-DATA-1.                            NC2534.2
070000     MOVE 123456107890 TO CORR-DATA-6.                            NC2534.2
070100 SUB-TEST-F3-5.                                                   NC2534.2
070200     SUBTRACT CORRESPONDING CORR-DATA-6 FROM CORR-DATA-1.         NC2534.2
070300     IF CORR-DATA-1 EQUAL TO "892155995511"                       NC2534.2
070400         PERFORM PASS                                             NC2534.2
070500         GO TO SUB-WRITE-F3-5.                                    NC2534.2
070600     GO TO SUB-FAIL-F3-5.                                         NC2534.2
070700 SUB-DELETE-F3-5.                                                 NC2534.2
070800     PERFORM DE-LETE.                                             NC2534.2
070900     GO TO SUB-WRITE-F3-5.                                        NC2534.2
071000 SUB-FAIL-F3-5.                                                   NC2534.2
071100     MOVE 892155995511 TO CORRECT-A.                              NC2534.2
071200     MOVE CORR-DATA-1 TO COMPUTED-A.                              NC2534.2
071300     PERFORM FAIL.                                                NC2534.2
071400 SUB-WRITE-F3-5.                                                  NC2534.2
071500     PERFORM PRINT-DETAIL.                                        NC2534.2
071600*                                                                 NC2534.2
071700 SUB-INIT-F3-6.                                                   NC2534.2
071800     MOVE "555555000055" TO CORR-DATA-6.                          NC2534.2
071900     MOVE "SUB-TEST-F3-6" TO PAR-NAME.                            NC2534.2
072000 SUB-TEST-F3-6.                                                   NC2534.2
072100     SUBTRACT CORRESPONDING CORR-DATA-6 FROM CORR-DATA-1          NC2534.2
072200     IF CORR-DATA-1 EQUAL TO 892155995511                         NC2534.2
072300         PERFORM PASS                                             NC2534.2
072400         GO TO SUB-WRITE-F3-6.                                    NC2534.2
072500     GO TO SUB-FAIL-F3-6.                                         NC2534.2
072600 SUB-DELETE-F3-6.                                                 NC2534.2
072700     PERFORM DE-LETE.                                             NC2534.2
072800     GO TO SUB-WRITE-F3-6.                                        NC2534.2
072900 SUB-FAIL-F3-6.                                                   NC2534.2
073000     MOVE 892155995511 TO CORRECT-A.                              NC2534.2
073100     MOVE CORR-DATA-1 TO COMPUTED-A.                              NC2534.2
073200     PERFORM FAIL.                                                NC2534.2
073300 SUB-WRITE-F3-6.                                                  NC2534.2
073400     PERFORM PRINT-DETAIL.                                        NC2534.2
073500*                                                                 NC2534.2
073600 SUB-INIT-F3-7.                                                   NC2534.2
073700     MOVE "SUB-TEST-F3-7" TO PAR-NAME.                            NC2534.2
073800     MOVE 99999999999999 TO CORR-DATA-5.                          NC2534.2
073900     MOVE 111111111111 TO CORR-DATA-1.                            NC2534.2
074000 SUB-TEST-F3-7.                                                   NC2534.2
074100     SUBTRACT CORRESPONDING CORR-DATA-1 FROM CORR-DATA-5.         NC2534.2
074200     IF CORR-DATA-5 EQUAL TO "88889999999999"                     NC2534.2
074300         PERFORM PASS                                             NC2534.2
074400         GO TO SUB-WRITE-F3-7.                                    NC2534.2
074500     GO TO SUB-FAIL-F3-7.                                         NC2534.2
074600 SUB-DELETE-F3-7.                                                 NC2534.2
074700     PERFORM DE-LETE.                                             NC2534.2
074800     GO TO SUB-WRITE-F3-7.                                        NC2534.2
074900 SUB-FAIL-F3-7.                                                   NC2534.2
075000     PERFORM FAIL.                                                NC2534.2
075100     MOVE CORR-DATA-5 TO COMPUTED-A.                              NC2534.2
075200     MOVE "88889999999999" TO CORRECT-A.                          NC2534.2
075300 SUB-WRITE-F3-7.                                                  NC2534.2
075400     PERFORM PRINT-DETAIL.                                        NC2534.2
075500*                                                                 NC2534.2
075600 SUB-INIT-F3-8.                                                   NC2534.2
075700     MOVE   "SUB-TEST-F3-8" TO PAR-NAME.                          NC2534.2
075800     MOVE   "VI-134 6.25.4 GR3" TO ANSI-REFERENCE.                NC2534.2
075900     PERFORM BUILD-TABLE1.                                        NC2534.2
076000     PERFORM BUILD-TABLE2.                                        NC2534.2
076100 SUB-TEST-F3-8-0.                                                 NC2534.2
076200     SUBTRACT CORRESPONDING TABLE1 FROM TABLE2.                   NC2534.2
076300 SUB-TEST-F3-8-1.                                                 NC2534.2
076400     IF      RECORD1 OF TABLE2 = 02                               NC2534.2
076500         AND RECORD2 OF TABLE2 (1) = 03                           NC2534.2
076600         AND RECORD2 OF TABLE2 (2) = 04                           NC2534.2
076700         AND RECORD3 OF TABLE2 = 02                               NC2534.2
076800             PERFORM PASS                                         NC2534.2
076900             GO TO  SUB-WRITE-F3-8.                               NC2534.2
077000     GO TO   SUB-FAIL-F3-8.                                       NC2534.2
077100 SUB-DELETE-F3-8.                                                 NC2534.2
077200     PERFORM DE-LETE.                                             NC2534.2
077300     GO TO   SUB-WRITE-F3-8.                                      NC2534.2
077400 SUB-FAIL-F3-8.                                                   NC2534.2
077500     PERFORM FAIL.                                                NC2534.2
077600     MOVE    TABLE2 TO COMPUTED-A.                                NC2534.2
077700     MOVE  "02030402" TO CORRECT-A.                               NC2534.2
077800 SUB-WRITE-F3-8.                                                  NC2534.2
077900     PERFORM PRINT-DETAIL.                                        NC2534.2
078000*                                                                 NC2534.2
078100 SUB-INIT-F3-9.                                                   NC2534.2
078200*    ===-->  NO SIZE ERROR  <--===                                NC2534.2
078300     MOVE   "VI-133 6.25" TO ANSI-REFERENCE.                      NC2534.2
078400     MOVE    SPACE  TO WRK-AN-00001.                              NC2534.2
078500     MOVE    0      TO REC-CT.                                    NC2534.2
078600     MOVE    1      TO SUBTR-13 OF SUBTR-12.                      NC2534.2
078700     MOVE   -1.725  TO SUBTR-14 OF SUBTR-12.                      NC2534.2
078800     MOVE    6.76   TO SUBTR-15 OF SUBTR-12.                      NC2534.2
078900     MOVE    2      TO SUBTR-13 OF SUBTR-16.                      NC2534.2
079000     MOVE     .23   TO SUBTR-14 OF SUBTR-16.                      NC2534.2
079100     MOVE    1      TO SUBTR-15 OF SUBTR-16.                      NC2534.2
079200 SUB-TEST-F3-9-0.                                                 NC2534.2
079300     SUBTRACT CORRESPONDING SUBTR-12 FROM SUBTR-16 ROUNDED        NC2534.2
079400             ON SIZE ERROR                                        NC2534.2
079500             MOVE   "G" TO WRK-AN-00001.                          NC2534.2
079600*                                                                 NC2534.2
079700 SUB-INIT-F3-9-1.                                                 NC2534.2
079800     MOVE   "SUB-TEST-F3-9-1" TO PAR-NAME.                        NC2534.2
079900     ADD     1 TO REC-CT.                                         NC2534.2
080000 SUB-TEST-F3-9-1.                                                 NC2534.2
080100     IF      WRK-AN-00001    NOT = SPACE                          NC2534.2
080200             GO TO SUB-FAIL-F3-9-1.                               NC2534.2
080300     PERFORM PASS                                                 NC2534.2
080400     GO TO SUB-WRITE-F3-9-1.                                      NC2534.2
080500 SUB-DELETE-F3-9-1.                                               NC2534.2
080600     PERFORM DE-LETE.                                             NC2534.2
080700     GO TO SUB-WRITE-F3-9-1.                                      NC2534.2
080800 SUB-FAIL-F3-9-1.                                                 NC2534.2
080900     MOVE   "SUBTRACT CORRESPONDING FAILED"                       NC2534.2
081000          TO RE-MARK                                              NC2534.2
081100     MOVE    WRK-AN-00001  TO COMPUTED-X                          NC2534.2
081200     MOVE    SPACE TO CORRECT-X                                   NC2534.2
081300     PERFORM FAIL.                                                NC2534.2
081400 SUB-WRITE-F3-9-1.                                                NC2534.2
081500     PERFORM PRINT-DETAIL.                                        NC2534.2
081600*                                                                 NC2534.2
081700 SUB-INIT-F3-9-2.                                                 NC2534.2
081800     MOVE  "SUB-TEST-F3-9-2" TO PAR-NAME.                         NC2534.2
081900     ADD    1 TO REC-CT.                                          NC2534.2
082000 SUB-TEST-F3-9-2.                                                 NC2534.2
082100     IF     SUBTR-13 OF SUBTR-16 NOT EQUAL TO 1                   NC2534.2
082200            GO TO SUB-FAIL-F3-9-2.                                NC2534.2
082300     PERFORM PASS                                                 NC2534.2
082400     GO TO SUB-WRITE-F3-9-2.                                      NC2534.2
082500 SUB-DELETE-F3-9-2.                                               NC2534.2
082600     PERFORM DE-LETE.                                             NC2534.2
082700     GO TO SUB-WRITE-F3-9-2.                                      NC2534.2
082800 SUB-FAIL-F3-9-2.                                                 NC2534.2
082900     MOVE    SUBTR-13 OF SUBTR-16 TO COMPUTED-N                   NC2534.2
083000     MOVE   "+1" TO CORRECT-A                                     NC2534.2
083100     MOVE "SUBTRACT CORRESPONDING FAILED"                         NC2534.2
083200           TO RE-MARK                                             NC2534.2
083300     PERFORM FAIL.                                                NC2534.2
083400 SUB-WRITE-F3-9-2.                                                NC2534.2
083500     PERFORM PRINT-DETAIL.                                        NC2534.2
083600*                                                                 NC2534.2
083700 SUB-INIT-F3-9-3.                                                 NC2534.2
083800     MOVE  "SUB-TEST-F3-9-3" TO PAR-NAME.                         NC2534.2
083900     ADD    1 TO REC-CT.                                          NC2534.2
084000 SUB-TEST-F3-9-3.                                                 NC2534.2
084100     IF     SUBTR-14 OF SUBTR-16 IS NOT EQUAL TO 1.96             NC2534.2
084200            GO TO SUB-FAIL-F3-9-3.                                NC2534.2
084300     PERFORM PASS                                                 NC2534.2
084400     GO TO SUB-WRITE-F3-9-3.                                      NC2534.2
084500 SUB-DELETE-F3-9-3.                                               NC2534.2
084600     PERFORM DE-LETE.                                             NC2534.2
084700     GO TO SUB-WRITE-F3-9-3.                                      NC2534.2
084800 SUB-FAIL-F3-9-3.                                                 NC2534.2
084900     MOVE   SUBTR-14 OF SUBTR-16 TO COMPUTED-N                    NC2534.2
085000     MOVE   "+1.96" TO CORRECT-A                                  NC2534.2
085100     MOVE "SUBTRACT CORRESPONDING FAILED"                         NC2534.2
085200          TO RE-MARK                                              NC2534.2
085300     PERFORM FAIL.                                                NC2534.2
085400 SUB-WRITE-F3-9-3.                                                NC2534.2
085500     PERFORM PRINT-DETAIL.                                        NC2534.2
085600*                                                                 NC2534.2
085700 SUB-INIT-F3-9-4.                                                 NC2534.2
085800     MOVE  "SUB-TEST-F3-9-4" TO PAR-NAME.                         NC2534.2
085900     ADD    1 TO REC-CT.                                          NC2534.2
086000 SUB-TEST-F3-9-4.                                                 NC2534.2
086100     IF     SUBTR-15 OF SUBTR-16 NOT EQUAL TO -5.76               NC2534.2
086200            GO TO SUB-FAIL-F3-9-4.                                NC2534.2
086300     PERFORM PASS                                                 NC2534.2
086400     GO TO SUB-WRITE-F3-9-4.                                      NC2534.2
086500 SUB-DELETE-F3-9-4.                                               NC2534.2
086600     PERFORM DE-LETE.                                             NC2534.2
086700     GO TO SUB-WRITE-F3-9-4.                                      NC2534.2
086800 SUB-FAIL-F3-9-4.                                                 NC2534.2
086900     MOVE    SUBTR-15 OF SUBTR-16 TO COMPUTED-N                   NC2534.2
087000     MOVE   "-5.76" TO CORRECT-A                                  NC2534.2
087100     MOVE "SUBRACT CORRESPONDING FAILED"                          NC2534.2
087200          TO RE-MARK                                              NC2534.2
087300     PERFORM FAIL.                                                NC2534.2
087400 SUB-WRITE-F3-9-4.                                                NC2534.2
087500     PERFORM PRINT-DETAIL.                                        NC2534.2
087600*                                                                 NC2534.2
087700 SUB-INIT-F3-10.                                                  NC2534.2
087800*    ===-->  NEW SIZE ERROR TESTS  <--===                         NC2534.2
087900*    ===-->      SIZE ERROR        <--===                         NC2534.2
088000     MOVE   "VI-133 6.25" TO ANSI-REFERENCE.                      NC2534.2
088100     MOVE    SPACE  TO WRK-AN-00001.                              NC2534.2
088200     MOVE    0      TO REC-CT.                                    NC2534.2
088300     MOVE    1      TO SUBTR-13 OF SUBTR-12.                      NC2534.2
088400     MOVE   -1.725  TO SUBTR-14 OF SUBTR-12.                      NC2534.2
088500     MOVE   76.76   TO SUBTR-15 OF SUBTR-12.                      NC2534.2
088600     MOVE    2      TO SUBTR-13 OF SUBTR-16.                      NC2534.2
088700     MOVE     .23   TO SUBTR-14 OF SUBTR-16.                      NC2534.2
088800     MOVE    1      TO SUBTR-15 OF SUBTR-16.                      NC2534.2
088900 SUB-TEST-F3-10-0.                                                NC2534.2
089000     SUBTRACT CORRESPONDING SUBTR-12 FROM SUBTR-16 ROUNDED        NC2534.2
089100         NOT ON SIZE ERROR                                        NC2534.2
089200             MOVE   "G" TO WRK-AN-00001.                          NC2534.2
089300*                                                                 NC2534.2
089400 SUB-INIT-F3-10-1.                                                NC2534.2
089500     MOVE   "SUB-TEST-F3-10-1" TO PAR-NAME.                       NC2534.2
089600     ADD     1 TO REC-CT.                                         NC2534.2
089700 SUB-TEST-F3-10-1.                                                NC2534.2
089800     IF      WRK-AN-00001 EQUAL TO "G"                            NC2534.2
089900             GO TO SUB-FAIL-F3-10-1.                              NC2534.2
090000     PERFORM PASS                                                 NC2534.2
090100     GO TO SUB-WRITE-F3-10-1.                                     NC2534.2
090200 SUB-DELETE-F3-10-1.                                              NC2534.2
090300     PERFORM DE-LETE.                                             NC2534.2
090400     GO TO SUB-WRITE-F3-10-1.                                     NC2534.2
090500 SUB-FAIL-F3-10-1.                                                NC2534.2
090600     MOVE   "NOT ON SIZE ERROR SHOULD NOT BE EXECUTED"            NC2534.2
090700          TO RE-MARK                                              NC2534.2
090800     MOVE    WRK-AN-00001  TO COMPUTED-X                          NC2534.2
090900     MOVE    SPACE TO CORRECT-X                                   NC2534.2
091000     PERFORM FAIL.                                                NC2534.2
091100 SUB-WRITE-F3-10-1.                                               NC2534.2
091200     PERFORM PRINT-DETAIL.                                        NC2534.2
091300*                                                                 NC2534.2
091400 SUB-INIT-F3-10-2.                                                NC2534.2
091500     MOVE  "SUB-TEST-F3-10-2" TO PAR-NAME.                        NC2534.2
091600     ADD    1 TO REC-CT.                                          NC2534.2
091700 SUB-TEST-F3-10-2.                                                NC2534.2
091800     IF     SUBTR-13 OF SUBTR-16 NOT EQUAL TO 1                   NC2534.2
091900            GO TO SUB-FAIL-F3-10-2.                               NC2534.2
092000     PERFORM PASS                                                 NC2534.2
092100     GO TO SUB-WRITE-F3-10-2.                                     NC2534.2
092200 SUB-DELETE-F3-10-2.                                              NC2534.2
092300     PERFORM DE-LETE.                                             NC2534.2
092400     GO TO SUB-WRITE-F3-10-2.                                     NC2534.2
092500 SUB-FAIL-F3-10-2.                                                NC2534.2
092600     MOVE    SUBTR-13 OF SUBTR-16 TO COMPUTED-N                   NC2534.2
092700     MOVE   "+1" TO CORRECT-A                                     NC2534.2
092800     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
092900          TO RE-MARK                                              NC2534.2
093000     PERFORM FAIL.                                                NC2534.2
093100 SUB-WRITE-F3-10-2.                                               NC2534.2
093200     PERFORM PRINT-DETAIL.                                        NC2534.2
093300*                                                                 NC2534.2
093400 SUB-INIT-F3-10-3.                                                NC2534.2
093500     MOVE  "SUB-TEST-F3-10-3" TO PAR-NAME.                        NC2534.2
093600     ADD    1 TO REC-CT.                                          NC2534.2
093700 SUB-TEST-F3-10-3.                                                NC2534.2
093800     IF     SUBTR-14 OF SUBTR-16 IS NOT EQUAL TO 1.96             NC2534.2
093900            GO TO SUB-FAIL-F3-10-3.                               NC2534.2
094000     PERFORM PASS                                                 NC2534.2
094100     GO TO SUB-WRITE-F3-10-3.                                     NC2534.2
094200 SUB-DELETE-F3-10-3.                                              NC2534.2
094300     PERFORM DE-LETE.                                             NC2534.2
094400     GO TO SUB-WRITE-F3-10-3.                                     NC2534.2
094500 SUB-FAIL-F3-10-3.                                                NC2534.2
094600     MOVE   SUBTR-14 OF SUBTR-16 TO COMPUTED-N                    NC2534.2
094700     MOVE   "+1.96" TO CORRECT-A                                  NC2534.2
094800     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
094900          TO RE-MARK                                              NC2534.2
095000     PERFORM FAIL.                                                NC2534.2
095100 SUB-WRITE-F3-10-3.                                               NC2534.2
095200     PERFORM PRINT-DETAIL.                                        NC2534.2
095300*                                                                 NC2534.2
095400 SUB-INIT-F3-10-4.                                                NC2534.2
095500     MOVE  "SUB-TEST-F3-10-4" TO PAR-NAME.                        NC2534.2
095600     ADD    1 TO REC-CT.                                          NC2534.2
095700 SUB-TEST-F3-10-4.                                                NC2534.2
095800     IF     SUBTR-15 OF SUBTR-16 NOT EQUAL TO 1                   NC2534.2
095900            GO TO SUB-FAIL-F3-10-4.                               NC2534.2
096000     PERFORM PASS                                                 NC2534.2
096100     GO TO SUB-WRITE-F3-10-4.                                     NC2534.2
096200 SUB-DELETE-F3-10-4.                                              NC2534.2
096300     PERFORM DE-LETE.                                             NC2534.2
096400     GO TO SUB-WRITE-F3-10-4.                                     NC2534.2
096500 SUB-FAIL-F3-10-4.                                                NC2534.2
096600     MOVE    SUBTR-15 OF SUBTR-16 TO COMPUTED-N                   NC2534.2
096700     MOVE   "+1" TO CORRECT-A                                     NC2534.2
096800     MOVE "WRONGLY AFFECTED BY SIZE ERROR" TO RE-MARK             NC2534.2
096900     PERFORM FAIL.                                                NC2534.2
097000 SUB-WRITE-F3-10-4.                                               NC2534.2
097100     PERFORM PRINT-DETAIL.                                        NC2534.2
097200*                                                                 NC2534.2
097300 SUB-INIT-F3-11.                                                  NC2534.2
097400*    ===-->  NEW SIZE ERROR TESTS  <--===                         NC2534.2
097500*    ===-->   NO SIZE ERROR        <--===                         NC2534.2
097600     MOVE   "VI-133 6.25" TO ANSI-REFERENCE.                      NC2534.2
097700     MOVE    SPACE  TO WRK-AN-00001.                              NC2534.2
097800     MOVE    0      TO REC-CT.                                    NC2534.2
097900     MOVE    1      TO SUBTR-13 OF SUBTR-12.                      NC2534.2
098000     MOVE   -1.725  TO SUBTR-14 OF SUBTR-12.                      NC2534.2
098100     MOVE    6.76   TO SUBTR-15 OF SUBTR-12.                      NC2534.2
098200     MOVE    2      TO SUBTR-13 OF SUBTR-16.                      NC2534.2
098300     MOVE     .23   TO SUBTR-14 OF SUBTR-16.                      NC2534.2
098400     MOVE    1      TO SUBTR-15 OF SUBTR-16.                      NC2534.2
098500 SUB-TEST-F3-11-0.                                                NC2534.2
098600     SUBTRACT CORRESPONDING SUBTR-12 FROM SUBTR-16 ROUNDED        NC2534.2
098700         NOT ON SIZE ERROR                                        NC2534.2
098800             MOVE   "G" TO WRK-AN-00001.                          NC2534.2
098900*                                                                 NC2534.2
099000 SUB-INIT-F3-11-1.                                                NC2534.2
099100     MOVE   "SUB-TEST-F3-11-1" TO PAR-NAME.                       NC2534.2
099200     ADD     1 TO REC-CT.                                         NC2534.2
099300 SUB-TEST-F3-11-1.                                                NC2534.2
099400     IF      WRK-AN-00001 EQUAL TO SPACE                          NC2534.2
099500             GO TO SUB-FAIL-F3-11-1.                              NC2534.2
099600     PERFORM PASS                                                 NC2534.2
099700     GO TO SUB-WRITE-F3-11-1.                                     NC2534.2
099800 SUB-DELETE-F3-11-1.                                              NC2534.2
099900     PERFORM DE-LETE.                                             NC2534.2
100000     GO TO SUB-WRITE-F3-11-1.                                     NC2534.2
100100 SUB-FAIL-F3-11-1.                                                NC2534.2
100200     MOVE   "NOT ON SIZE ERROR SHOULD BE EXECUTED"                NC2534.2
100300           TO RE-MARK                                             NC2534.2
100400     MOVE    WRK-AN-00001  TO COMPUTED-X                          NC2534.2
100500     MOVE   "G"    TO CORRECT-X                                   NC2534.2
100600     PERFORM FAIL.                                                NC2534.2
100700 SUB-WRITE-F3-11-1.                                               NC2534.2
100800     PERFORM PRINT-DETAIL.                                        NC2534.2
100900*                                                                 NC2534.2
101000 SUB-INIT-F3-11-2.                                                NC2534.2
101100     MOVE  "SUB-TEST-F3-11-1" TO PAR-NAME.                        NC2534.2
101200     ADD    1 TO REC-CT.                                          NC2534.2
101300 SUB-TEST-F3-11-2.                                                NC2534.2
101400     IF     SUBTR-13 OF SUBTR-16 NOT EQUAL TO 1                   NC2534.2
101500            GO TO SUB-FAIL-F3-11-2.                               NC2534.2
101600     PERFORM PASS                                                 NC2534.2
101700     GO TO SUB-WRITE-F3-11-2.                                     NC2534.2
101800 SUB-DELETE-F3-11-2.                                              NC2534.2
101900     PERFORM DE-LETE.                                             NC2534.2
102000     GO TO SUB-WRITE-F3-11-2.                                     NC2534.2
102100 SUB-FAIL-F3-11-2.                                                NC2534.2
102200     MOVE    SUBTR-13 OF SUBTR-16 TO COMPUTED-N                   NC2534.2
102300     MOVE   "+1" TO CORRECT-A                                     NC2534.2
102400     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
102500          TO RE-MARK                                              NC2534.2
102600     PERFORM FAIL.                                                NC2534.2
102700 SUB-WRITE-F3-11-2.                                               NC2534.2
102800     PERFORM PRINT-DETAIL.                                        NC2534.2
102900*                                                                 NC2534.2
103000 SUB-INIT-F3-11-3.                                                NC2534.2
103100     MOVE  "SUB-TEST-F3-11-3" TO PAR-NAME.                        NC2534.2
103200     ADD    1 TO REC-CT.                                          NC2534.2
103300 SUB-TEST-F3-11-3.                                                NC2534.2
103400     IF     SUBTR-14 OF SUBTR-16 IS NOT EQUAL TO 1.96             NC2534.2
103500            GO TO SUB-FAIL-F3-11-3.                               NC2534.2
103600     PERFORM PASS                                                 NC2534.2
103700     GO TO SUB-WRITE-F3-11-3.                                     NC2534.2
103800 SUB-DELETE-F3-11-3.                                              NC2534.2
103900     PERFORM DE-LETE.                                             NC2534.2
104000     GO TO SUB-WRITE-F3-11-3.                                     NC2534.2
104100 SUB-FAIL-F3-11-3.                                                NC2534.2
104200     MOVE   SUBTR-14 OF SUBTR-16 TO COMPUTED-N                    NC2534.2
104300     MOVE   "+1.96" TO CORRECT-A                                  NC2534.2
104400     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
104500          TO RE-MARK                                              NC2534.2
104600     PERFORM FAIL.                                                NC2534.2
104700 SUB-WRITE-F3-11-3.                                               NC2534.2
104800     PERFORM PRINT-DETAIL.                                        NC2534.2
104900*                                                                 NC2534.2
105000 SUB-INIT-F3-11-4.                                                NC2534.2
105100     MOVE  "SUB-TEST-F3-11-4" TO PAR-NAME.                        NC2534.2
105200     ADD    1 TO REC-CT.                                          NC2534.2
105300 SUB-TEST-F3-11-4.                                                NC2534.2
105400     IF     SUBTR-15 OF SUBTR-16 NOT EQUAL TO -5.76               NC2534.2
105500            GO TO SUB-FAIL-F3-11-4.                               NC2534.2
105600     PERFORM PASS                                                 NC2534.2
105700     GO TO SUB-WRITE-F3-11-4.                                     NC2534.2
105800 SUB-DELETE-F3-11-4.                                              NC2534.2
105900     PERFORM DE-LETE.                                             NC2534.2
106000     GO TO SUB-WRITE-F3-11-4.                                     NC2534.2
106100 SUB-FAIL-F3-11-4.                                                NC2534.2
106200     MOVE    SUBTR-15 OF SUBTR-16 TO COMPUTED-N                   NC2534.2
106300     MOVE   "-5.76" TO CORRECT-A                                  NC2534.2
106400     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
106500          TO RE-MARK                                              NC2534.2
106600     PERFORM FAIL.                                                NC2534.2
106700 SUB-WRITE-F3-11-4.                                               NC2534.2
106800     PERFORM PRINT-DETAIL.                                        NC2534.2
106900*                                                                 NC2534.2
107000 SUB-INIT-F3-12.                                                  NC2534.2
107100*    ===-->  NEW SIZE ERROR TESTS  <--===                         NC2534.2
107200*    ===-->      SIZE ERROR        <--===                         NC2534.2
107300     MOVE   "VI-133 6.25" TO ANSI-REFERENCE.                      NC2534.2
107400     MOVE    SPACE  TO WRK-AN-00001.                              NC2534.2
107500     MOVE    0      TO REC-CT.                                    NC2534.2
107600     MOVE    1      TO SUBTR-13 OF SUBTR-12.                      NC2534.2
107700     MOVE   -1.725  TO SUBTR-14 OF SUBTR-12.                      NC2534.2
107800     MOVE   76.76   TO SUBTR-15 OF SUBTR-12.                      NC2534.2
107900     MOVE    2      TO SUBTR-13 OF SUBTR-16.                      NC2534.2
108000     MOVE     .23   TO SUBTR-14 OF SUBTR-16.                      NC2534.2
108100     MOVE    1      TO SUBTR-15 OF SUBTR-16.                      NC2534.2
108200 SUB-TEST-F3-12-0.                                                NC2534.2
108300     SUBTRACT CORRESPONDING SUBTR-12 FROM SUBTR-16 ROUNDED        NC2534.2
108400             ON SIZE ERROR                                        NC2534.2
108500             MOVE   "A" TO WRK-AN-00001                           NC2534.2
108600         NOT ON SIZE ERROR                                        NC2534.2
108700             MOVE   "B" TO WRK-AN-00001.                          NC2534.2
108800*                                                                 NC2534.2
108900 SUB-INIT-F3-12-1.                                                NC2534.2
109000     MOVE   "SUB-TEST-F3-12-1" TO PAR-NAME.                       NC2534.2
109100     ADD     1 TO REC-CT.                                         NC2534.2
109200 SUB-TEST-F3-12-1.                                                NC2534.2
109300     IF      WRK-AN-00001    NOT = "A"                            NC2534.2
109400             GO TO SUB-FAIL-F3-12-1.                              NC2534.2
109500     PERFORM PASS                                                 NC2534.2
109600     GO TO SUB-WRITE-F3-12-1.                                     NC2534.2
109700 SUB-DELETE-F3-12-1.                                              NC2534.2
109800     PERFORM DE-LETE.                                             NC2534.2
109900     GO TO SUB-WRITE-F3-12-1.                                     NC2534.2
110000 SUB-FAIL-F3-12-1.                                                NC2534.2
110100     MOVE   "ON SIZE ERROR SHOULD BE EXECUTED"                    NC2534.2
110200          TO RE-MARK                                              NC2534.2
110300     MOVE    WRK-AN-00001  TO COMPUTED-X                          NC2534.2
110400     MOVE   "A"    TO CORRECT-X                                   NC2534.2
110500     PERFORM FAIL.                                                NC2534.2
110600 SUB-WRITE-F3-12-1.                                               NC2534.2
110700     PERFORM PRINT-DETAIL.                                        NC2534.2
110800*                                                                 NC2534.2
110900 SUB-INIT-F3-12-2.                                                NC2534.2
111000     MOVE  "SUB-TEST-F3-12-2" TO PAR-NAME.                        NC2534.2
111100     ADD    1 TO REC-CT.                                          NC2534.2
111200 SUB-TEST-F3-12-2.                                                NC2534.2
111300     IF     SUBTR-13 OF SUBTR-16 NOT EQUAL TO 1                   NC2534.2
111400            GO TO SUB-FAIL-F3-12-2.                               NC2534.2
111500     PERFORM PASS                                                 NC2534.2
111600     GO TO SUB-WRITE-F3-12-2.                                     NC2534.2
111700 SUB-DELETE-F3-12-2.                                              NC2534.2
111800     PERFORM DE-LETE.                                             NC2534.2
111900     GO TO SUB-WRITE-F3-12-2.                                     NC2534.2
112000 SUB-FAIL-F3-12-2.                                                NC2534.2
112100     MOVE    SUBTR-13 OF SUBTR-16 TO COMPUTED-N                   NC2534.2
112200     MOVE   "+1" TO CORRECT-A                                     NC2534.2
112300     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
112400          TO RE-MARK                                              NC2534.2
112500     PERFORM FAIL.                                                NC2534.2
112600 SUB-WRITE-F3-12-2.                                               NC2534.2
112700     PERFORM PRINT-DETAIL.                                        NC2534.2
112800*                                                                 NC2534.2
112900 SUB-INIT-F3-12-3.                                                NC2534.2
113000     MOVE  "SUB-TEST-F3-12-3" TO PAR-NAME.                        NC2534.2
113100     ADD    1 TO REC-CT.                                          NC2534.2
113200 SUB-TEST-F3-12-3.                                                NC2534.2
113300     IF     SUBTR-14 OF SUBTR-16 IS NOT EQUAL TO 1.96             NC2534.2
113400            GO TO SUB-FAIL-F3-12-3.                               NC2534.2
113500     PERFORM PASS                                                 NC2534.2
113600     GO TO SUB-WRITE-F3-12-3.                                     NC2534.2
113700 SUB-DELETE-F3-12-3.                                              NC2534.2
113800     PERFORM DE-LETE.                                             NC2534.2
113900     GO TO SUB-WRITE-F3-12-3.                                     NC2534.2
114000 SUB-FAIL-F3-12-3.                                                NC2534.2
114100     MOVE   SUBTR-14 OF SUBTR-16 TO COMPUTED-N                    NC2534.2
114200     MOVE   "+1.96" TO CORRECT-A                                  NC2534.2
114300     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
114400          TO RE-MARK                                              NC2534.2
114500     PERFORM FAIL.                                                NC2534.2
114600 SUB-WRITE-F3-12-3.                                               NC2534.2
114700     PERFORM PRINT-DETAIL.                                        NC2534.2
114800*                                                                 NC2534.2
114900 SUB-INIT-F3-12-4.                                                NC2534.2
115000     MOVE  "SUB-TEST-F3-12-4" TO PAR-NAME.                        NC2534.2
115100     ADD    1 TO REC-CT.                                          NC2534.2
115200 SUB-TEST-F3-12-4.                                                NC2534.2
115300     IF     SUBTR-15 OF SUBTR-16 NOT EQUAL TO 1                   NC2534.2
115400            GO TO SUB-FAIL-F3-12-4.                               NC2534.2
115500     PERFORM PASS                                                 NC2534.2
115600     GO TO SUB-WRITE-F3-12-4.                                     NC2534.2
115700 SUB-DELETE-F3-12-4.                                              NC2534.2
115800     PERFORM DE-LETE.                                             NC2534.2
115900     GO TO SUB-WRITE-F3-12-4.                                     NC2534.2
116000 SUB-FAIL-F3-12-4.                                                NC2534.2
116100     MOVE    SUBTR-15 OF SUBTR-16 TO COMPUTED-N                   NC2534.2
116200     MOVE   "+1" TO CORRECT-A                                     NC2534.2
116300     MOVE "WRONGLY AFFECTED BY SIZE ERROR" TO RE-MARK             NC2534.2
116400     PERFORM FAIL.                                                NC2534.2
116500 SUB-WRITE-F3-12-4.                                               NC2534.2
116600     PERFORM PRINT-DETAIL.                                        NC2534.2
116700*                                                                 NC2534.2
116800 SUB-INIT-F3-13.                                                  NC2534.2
116900*    ===-->  NEW SIZE ERROR TESTS  <--===                         NC2534.2
117000*    ===-->   NO SIZE ERROR        <--===                         NC2534.2
117100     MOVE   "VI-133 6.25" TO ANSI-REFERENCE.                      NC2534.2
117200     MOVE    SPACE  TO WRK-AN-00001.                              NC2534.2
117300     MOVE    0      TO REC-CT.                                    NC2534.2
117400     MOVE    1      TO SUBTR-13 OF SUBTR-12.                      NC2534.2
117500     MOVE   -1.725  TO SUBTR-14 OF SUBTR-12.                      NC2534.2
117600     MOVE    6.76   TO SUBTR-15 OF SUBTR-12.                      NC2534.2
117700     MOVE    2      TO SUBTR-13 OF SUBTR-16.                      NC2534.2
117800     MOVE     .23   TO SUBTR-14 OF SUBTR-16.                      NC2534.2
117900     MOVE    1      TO SUBTR-15 OF SUBTR-16.                      NC2534.2
118000 SUB-TEST-F3-13-0.                                                NC2534.2
118100     SUBTRACT CORRESPONDING SUBTR-12 FROM SUBTR-16 ROUNDED        NC2534.2
118200             ON SIZE ERROR                                        NC2534.2
118300             MOVE   "A" TO WRK-AN-00001                           NC2534.2
118400         NOT ON SIZE ERROR                                        NC2534.2
118500             MOVE   "B" TO WRK-AN-00001.                          NC2534.2
118600*                                                                 NC2534.2
118700 SUB-INIT-F3-13-1.                                                NC2534.2
118800     MOVE   "SUB-TEST-F3-13-1" TO PAR-NAME.                       NC2534.2
118900     ADD     1 TO REC-CT.                                         NC2534.2
119000 SUB-TEST-F3-13-1.                                                NC2534.2
119100     IF      WRK-AN-00001    NOT = "B"                            NC2534.2
119200             GO TO SUB-FAIL-F3-13-1.                              NC2534.2
119300     PERFORM PASS                                                 NC2534.2
119400     GO TO SUB-WRITE-F3-13-1.                                     NC2534.2
119500 SUB-DELETE-F3-13-1.                                              NC2534.2
119600     PERFORM DE-LETE.                                             NC2534.2
119700     GO TO SUB-WRITE-F3-13-1.                                     NC2534.2
119800 SUB-FAIL-F3-13-1.                                                NC2534.2
119900     MOVE   "NOT ON SIZE ERROR SHOULD BE EXECUTED"                NC2534.2
120000           TO RE-MARK                                             NC2534.2
120100     MOVE    WRK-AN-00001  TO COMPUTED-X                          NC2534.2
120200     MOVE   "B"    TO CORRECT-X                                   NC2534.2
120300     PERFORM FAIL.                                                NC2534.2
120400 SUB-WRITE-F3-13-1.                                               NC2534.2
120500     PERFORM PRINT-DETAIL.                                        NC2534.2
120600*                                                                 NC2534.2
120700 SUB-INIT-F3-13-2.                                                NC2534.2
120800     MOVE  "SUB-TEST-F3-13-2" TO PAR-NAME.                        NC2534.2
120900     ADD    1 TO REC-CT.                                          NC2534.2
121000 SUB-TEST-F3-13-2.                                                NC2534.2
121100     IF     SUBTR-13 OF SUBTR-16 NOT EQUAL TO 1                   NC2534.2
121200            GO TO SUB-FAIL-F3-13-2.                               NC2534.2
121300     PERFORM PASS                                                 NC2534.2
121400     GO TO SUB-WRITE-F3-13-2.                                     NC2534.2
121500 SUB-DELETE-F3-13-2.                                              NC2534.2
121600     PERFORM DE-LETE.                                             NC2534.2
121700     GO TO SUB-WRITE-F3-13-2.                                     NC2534.2
121800 SUB-FAIL-F3-13-2.                                                NC2534.2
121900            MOVE    SUBTR-13 OF SUBTR-16 TO COMPUTED-N            NC2534.2
122000            MOVE   "+1" TO CORRECT-A                              NC2534.2
122100            MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"NC2534.2
122200                 TO RE-MARK                                       NC2534.2
122300            PERFORM FAIL.                                         NC2534.2
122400 SUB-WRITE-F3-13-2.                                               NC2534.2
122500            PERFORM PRINT-DETAIL.                                 NC2534.2
122600*                                                                 NC2534.2
122700 SUB-INIT-F3-13-3.                                                NC2534.2
122800     MOVE  "SUB-TEST-F3-13-3" TO PAR-NAME.                        NC2534.2
122900     ADD    1 TO REC-CT.                                          NC2534.2
123000 SUB-TEST-F3-13-3.                                                NC2534.2
123100     IF     SUBTR-14 OF SUBTR-16 IS NOT EQUAL TO 1.96             NC2534.2
123200            GO TO SUB-FAIL-F3-13-3.                               NC2534.2
123300     PERFORM PASS                                                 NC2534.2
123400     GO TO SUB-WRITE-F3-13-3.                                     NC2534.2
123500 SUB-DELETE-F3-13-3.                                              NC2534.2
123600     PERFORM DE-LETE.                                             NC2534.2
123700     GO TO SUB-WRITE-F3-13-3.                                     NC2534.2
123800 SUB-FAIL-F3-13-3.                                                NC2534.2
123900     MOVE   SUBTR-14 OF SUBTR-16 TO COMPUTED-N                    NC2534.2
124000     MOVE   "+1.96" TO CORRECT-A                                  NC2534.2
124100     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
124200          TO RE-MARK                                              NC2534.2
124300     PERFORM FAIL.                                                NC2534.2
124400 SUB-WRITE-F3-13-3.                                               NC2534.2
124500     PERFORM PRINT-DETAIL.                                        NC2534.2
124600*                                                                 NC2534.2
124700 SUB-INIT-F3-13-4.                                                NC2534.2
124800     MOVE  "SUB-TEST-F3-13-4" TO PAR-NAME.                        NC2534.2
124900     ADD    1 TO REC-CT.                                          NC2534.2
125000 SUB-TEST-F3-13-4.                                                NC2534.2
125100     IF     SUBTR-15 OF SUBTR-16 NOT EQUAL TO -5.76               NC2534.2
125200            GO TO SUB-FAIL-F3-13-4.                               NC2534.2
125300     PERFORM PASS                                                 NC2534.2
125400     GO TO SUB-WRITE-F3-13-4.                                     NC2534.2
125500 SUB-FAIL-F3-13-4.                                                NC2534.2
125600     MOVE    SUBTR-15 OF SUBTR-16 TO COMPUTED-N                   NC2534.2
125700     MOVE   "-5.76" TO CORRECT-A                                  NC2534.2
125800     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
125900        TO RE-MARK                                                NC2534.2
126000     PERFORM FAIL.                                                NC2534.2
126100 SUB-WRITE-F3-13-4.                                               NC2534.2
126200     PERFORM PRINT-DETAIL.                                        NC2534.2
126300*                                                                 NC2534.2
126400 SUB-INIT-F3-14.                                                  NC2534.2
126500*    ===-->  NEW SIZE ERROR TESTS  <--===                         NC2534.2
126600*    ===-->      SIZE ERROR        <--===                         NC2534.2
126700     MOVE   "VI-133 6.25" TO ANSI-REFERENCE.                      NC2534.2
126800     MOVE    SPACE  TO WRK-AN-00001.                              NC2534.2
126900     MOVE    SPACE  TO WRK-XN-00001.                              NC2534.2
127000     MOVE    0      TO REC-CT.                                    NC2534.2
127100     MOVE    1      TO SUBTR-13 OF SUBTR-12.                      NC2534.2
127200     MOVE   -1.725  TO SUBTR-14 OF SUBTR-12.                      NC2534.2
127300     MOVE   76.76   TO SUBTR-15 OF SUBTR-12.                      NC2534.2
127400     MOVE    2      TO SUBTR-13 OF SUBTR-16.                      NC2534.2
127500     MOVE     .23   TO SUBTR-14 OF SUBTR-16.                      NC2534.2
127600     MOVE    1      TO SUBTR-15 OF SUBTR-16.                      NC2534.2
127700 SUB-TEST-F3-14-0.                                                NC2534.2
127800     SUBTRACT CORRESPONDING SUBTR-12 FROM SUBTR-16 ROUNDED        NC2534.2
127900             ON SIZE ERROR                                        NC2534.2
128000             MOVE   "A" TO WRK-AN-00001                           NC2534.2
128100     END-SUBTRACT                                                 NC2534.2
128200     MOVE   "Z"  TO WRK-XN-00001.                                 NC2534.2
128300*                                                                 NC2534.2
128400 SUB-INIT-F3-14-1.                                                NC2534.2
128500     MOVE   "SUB-TEST-F3-14-1" TO PAR-NAME.                       NC2534.2
128600     ADD     1 TO REC-CT.                                         NC2534.2
128700 SUB-TEST-F3-14-1.                                                NC2534.2
128800     IF      WRK-AN-00001    NOT = "A"                            NC2534.2
128900             GO TO SUB-FAIL-F3-14-1.                              NC2534.2
129000     PERFORM PASS                                                 NC2534.2
129100     GO TO SUB-WRITE-F3-14-1.                                     NC2534.2
129200 SUB-DELETE-F3-14-1.                                              NC2534.2
129300     PERFORM DE-LETE.                                             NC2534.2
129400     GO TO SUB-WRITE-F3-14-1.                                     NC2534.2
129500 SUB-FAIL-F3-14-1.                                                NC2534.2
129600     MOVE   "ON SIZE ERROR SHOULD BE EXECUTED"                    NC2534.2
129700          TO RE-MARK                                              NC2534.2
129800     MOVE    WRK-AN-00001  TO COMPUTED-X                          NC2534.2
129900     MOVE   "A"    TO CORRECT-X                                   NC2534.2
130000     PERFORM FAIL.                                                NC2534.2
130100 SUB-WRITE-F3-14-1.                                               NC2534.2
130200     PERFORM PRINT-DETAIL.                                        NC2534.2
130300*                                                                 NC2534.2
130400 SUB-INIT-F3-14-2.                                                NC2534.2
130500     MOVE  "SUB-TEST-F3-14-2" TO PAR-NAME.                        NC2534.2
130600     ADD    1 TO REC-CT.                                          NC2534.2
130700 SUB-TEST-F3-14-2.                                                NC2534.2
130800     IF     SUBTR-13 OF SUBTR-16 NOT EQUAL TO 1                   NC2534.2
130900            GO TO SUB-FAIL-F3-14-2.                               NC2534.2
131000     PERFORM PASS                                                 NC2534.2
131100     GO TO SUB-WRITE-F3-14-2.                                     NC2534.2
131200 SUB-DELETE-F3-14-2.                                              NC2534.2
131300     PERFORM DE-LETE.                                             NC2534.2
131400     GO TO SUB-WRITE-F3-14-2.                                     NC2534.2
131500 SUB-FAIL-F3-14-2.                                                NC2534.2
131600     MOVE    SUBTR-13 OF SUBTR-16 TO COMPUTED-N                   NC2534.2
131700     MOVE   "+1" TO CORRECT-A                                     NC2534.2
131800     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
131900          TO RE-MARK                                              NC2534.2
132000     PERFORM FAIL.                                                NC2534.2
132100 SUB-WRITE-F3-14-2.                                               NC2534.2
132200     PERFORM PRINT-DETAIL.                                        NC2534.2
132300*                                                                 NC2534.2
132400 SUB-INIT-F3-14-3.                                                NC2534.2
132500     MOVE  "SUB-TEST-F3-14-3" TO PAR-NAME.                        NC2534.2
132600     ADD    1 TO REC-CT.                                          NC2534.2
132700 SUB-TEST-F3-14-3.                                                NC2534.2
132800     IF     SUBTR-14 OF SUBTR-16 IS NOT EQUAL TO 1.96             NC2534.2
132900            GO TO SUB-FAIL-F3-14-3.                               NC2534.2
133000     PERFORM PASS                                                 NC2534.2
133100     GO TO SUB-WRITE-F3-14-3.                                     NC2534.2
133200 SUB-DELETE-F3-14-3.                                              NC2534.2
133300     PERFORM DE-LETE.                                             NC2534.2
133400     GO TO SUB-WRITE-F3-14-3.                                     NC2534.2
133500 SUB-FAIL-F3-14-3.                                                NC2534.2
133600     MOVE   SUBTR-14 OF SUBTR-16 TO COMPUTED-N                    NC2534.2
133700     MOVE   "+1.96" TO CORRECT-A                                  NC2534.2
133800     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
133900          TO RE-MARK                                              NC2534.2
134000     PERFORM FAIL.                                                NC2534.2
134100 SUB-WRITE-F3-14-3.                                               NC2534.2
134200     PERFORM PRINT-DETAIL.                                        NC2534.2
134300*                                                                 NC2534.2
134400 SUB-INIT-F3-14-4.                                                NC2534.2
134500     MOVE  "SUB-TEST-F3-14-4" TO PAR-NAME.                        NC2534.2
134600     ADD     1 TO REC-CT.                                         NC2534.2
134700 SUB-TEST-F3-14-4.                                                NC2534.2
134800     IF     SUBTR-15 OF SUBTR-16 NOT EQUAL TO 1                   NC2534.2
134900            GO TO SUB-FAIL-F3-14-4.                               NC2534.2
135000     PERFORM PASS                                                 NC2534.2
135100     GO TO SUB-WRITE-F3-14-4.                                     NC2534.2
135200 SUB-DELETE-F3-14-4.                                              NC2534.2
135300     PERFORM DE-LETE.                                             NC2534.2
135400     GO TO SUB-WRITE-F3-14-4.                                     NC2534.2
135500 SUB-FAIL-F3-14-4.                                                NC2534.2
135600     MOVE    SUBTR-15 OF SUBTR-16 TO COMPUTED-N                   NC2534.2
135700     MOVE   "+1" TO CORRECT-A                                     NC2534.2
135800     MOVE   "WRONGLY AFFECTED BY SIZE ERROR" TO RE-MARK           NC2534.2
135900     PERFORM FAIL.                                                NC2534.2
136000 SUB-WRITE-F3-14-4.                                               NC2534.2
136100     PERFORM PRINT-DETAIL.                                        NC2534.2
136200*                                                                 NC2534.2
136300 SUB-INIT-F3-14-5.                                                NC2534.2
136400     MOVE   "SUB-TEST-F3-14-5" TO PAR-NAME.                       NC2534.2
136500     ADD     1 TO REC-CT.                                         NC2534.2
136600 SUB-TEST-F3-14-5.                                                NC2534.2
136700     IF      WRK-XN-00001 NOT = "Z"                               NC2534.2
136800             GO TO SUB-FAIL-F3-14-5.                              NC2534.2
136900     PERFORM PASS                                                 NC2534.2
137000     GO TO SUB-WRITE-F3-14-5.                                     NC2534.2
137100 SUB-DELETE-F3-14-5.                                              NC2534.2
137200     PERFORM DE-LETE.                                             NC2534.2
137300     GO TO SUB-WRITE-F3-14-5.                                     NC2534.2
137400 SUB-FAIL-F3-14-5.                                                NC2534.2
137500     MOVE   "SCOPE TERMINATOR IGNORED" TO RE-MARK                 NC2534.2
137600     MOVE   "Z"   TO COMPUTED-X                                   NC2534.2
137700     MOVE    WRK-AN-00001 TO CORRECT-X                            NC2534.2
137800     PERFORM FAIL.                                                NC2534.2
137900 SUB-WRITE-F3-14-5.                                               NC2534.2
138000     PERFORM PRINT-DETAIL.                                        NC2534.2
138100*                                                                 NC2534.2
138200 SUB-INIT-F3-15.                                                  NC2534.2
138300*    ===-->  NEW SIZE ERROR TESTS  <--===                         NC2534.2
138400*    ===-->   NO SIZE ERROR        <--===                         NC2534.2
138500     MOVE   "VI-133 6.25" TO ANSI-REFERENCE.                      NC2534.2
138600     MOVE    SPACE  TO WRK-AN-00001.                              NC2534.2
138700     MOVE    SPACE  TO WRK-XN-00001.                              NC2534.2
138800     MOVE    0      TO REC-CT.                                    NC2534.2
138900     MOVE    1      TO SUBTR-13 OF SUBTR-12.                      NC2534.2
139000     MOVE   -1.725  TO SUBTR-14 OF SUBTR-12.                      NC2534.2
139100     MOVE    6.76   TO SUBTR-15 OF SUBTR-12.                      NC2534.2
139200     MOVE    2      TO SUBTR-13 OF SUBTR-16.                      NC2534.2
139300     MOVE     .23   TO SUBTR-14 OF SUBTR-16.                      NC2534.2
139400     MOVE    1      TO SUBTR-15 OF SUBTR-16.                      NC2534.2
139500 SUB-TEST-F3-15-0.                                                NC2534.2
139600     SUBTRACT CORRESPONDING SUBTR-12 FROM SUBTR-16 ROUNDED        NC2534.2
139700             ON SIZE ERROR                                        NC2534.2
139800             MOVE   "A" TO WRK-AN-00001                           NC2534.2
139900     END-SUBTRACT                                                 NC2534.2
140000     MOVE   "Z"  TO WRK-XN-00001.                                 NC2534.2
140100*                                                                 NC2534.2
140200 SUB-INIT-F3-15-1.                                                NC2534.2
140300     MOVE   "SUB-TEST-F3-15-1" TO PAR-NAME.                       NC2534.2
140400     ADD     1 TO REC-CT.                                         NC2534.2
140500 SUB-TEST-F3-15-1.                                                NC2534.2
140600     IF      WRK-AN-00001    = "A"                                NC2534.2
140700             GO TO SUB-FAIL-F3-15-1.                              NC2534.2
140800     PERFORM PASS                                                 NC2534.2
140900     GO TO SUB-WRITE-F3-15-1.                                     NC2534.2
141000 SUB-FAIL-F3-15-1.                                                NC2534.2
141100     MOVE   "ON SIZE ERROR SHOULD NOT BE EXECUTED"                NC2534.2
141200          TO RE-MARK                                              NC2534.2
141300     MOVE    WRK-AN-00001  TO COMPUTED-X                          NC2534.2
141400     MOVE    SPACE TO CORRECT-X                                   NC2534.2
141500     PERFORM FAIL.                                                NC2534.2
141600 SUB-WRITE-F3-15-1.                                               NC2534.2
141700     PERFORM PRINT-DETAIL.                                        NC2534.2
141800*                                                                 NC2534.2
141900 SUB-INIT-F3-15-2.                                                NC2534.2
142000     MOVE  "SUB-TEST-F3-15-2" TO PAR-NAME.                        NC2534.2
142100     ADD    1 TO REC-CT.                                          NC2534.2
142200 SUB-TEST-F3-15-2.                                                NC2534.2
142300     IF     SUBTR-13 OF SUBTR-16 NOT EQUAL TO 1                   NC2534.2
142400            GO TO SUB-FAIL-F3-15-2.                               NC2534.2
142500     PERFORM PASS                                                 NC2534.2
142600     GO TO SUB-WRITE-F3-15-2.                                     NC2534.2
142700 SUB-DELETE-F3-15-2.                                              NC2534.2
142800     PERFORM DE-LETE.                                             NC2534.2
142900     GO TO SUB-WRITE-F3-15-2.                                     NC2534.2
143000 SUB-FAIL-F3-15-2.                                                NC2534.2
143100     MOVE    SUBTR-13 OF SUBTR-16 TO COMPUTED-N                   NC2534.2
143200     MOVE   "+1" TO CORRECT-A                                     NC2534.2
143300     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
143400          TO RE-MARK                                              NC2534.2
143500     PERFORM FAIL.                                                NC2534.2
143600 SUB-WRITE-F3-15-2.                                               NC2534.2
143700     PERFORM PRINT-DETAIL.                                        NC2534.2
143800*                                                                 NC2534.2
143900 SUB-INIT-F3-15-3.                                                NC2534.2
144000     MOVE  "SUB-TEST-F3-15-3" TO PAR-NAME.                        NC2534.2
144100     ADD    1 TO REC-CT.                                          NC2534.2
144200 SUB-TEST-F3-15-3.                                                NC2534.2
144300     IF     SUBTR-14 OF SUBTR-16 IS NOT EQUAL TO 1.96             NC2534.2
144400            GO TO SUB-FAIL-F3-15-3.                               NC2534.2
144500     PERFORM PASS                                                 NC2534.2
144600     GO TO SUB-WRITE-F3-15-3.                                     NC2534.2
144700 SUB-DELETE-F3-15-3.                                              NC2534.2
144800     PERFORM DE-LETE.                                             NC2534.2
144900     GO TO SUB-WRITE-F3-15-3.                                     NC2534.2
145000 SUB-FAIL-F3-15-3.                                                NC2534.2
145100     MOVE   SUBTR-14 OF SUBTR-16 TO COMPUTED-N                    NC2534.2
145200     MOVE   "+1.96" TO CORRECT-A                                  NC2534.2
145300     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
145400          TO RE-MARK                                              NC2534.2
145500     PERFORM FAIL.                                                NC2534.2
145600 SUB-WRITE-F3-15-3.                                               NC2534.2
145700     PERFORM PRINT-DETAIL.                                        NC2534.2
145800*                                                                 NC2534.2
145900 SUB-INIT-F3-15-4.                                                NC2534.2
146000     MOVE  "SUB-TEST-F3-15-4" TO PAR-NAME.                        NC2534.2
146100     ADD     1 TO REC-CT.                                         NC2534.2
146200 SUB-TEST-F3-15-4.                                                NC2534.2
146300     IF     SUBTR-15 OF SUBTR-16 NOT EQUAL TO -5.76               NC2534.2
146400            GO TO SUB-FAIL-F3-15-4.                               NC2534.2
146500     PERFORM PASS                                                 NC2534.2
146600     GO TO SUB-WRITE-F3-15-4.                                     NC2534.2
146700 SUB-DELETE-F3-15-4.                                              NC2534.2
146800     PERFORM DE-LETE.                                             NC2534.2
146900     GO TO SUB-WRITE-F3-15-4.                                     NC2534.2
147000 SUB-FAIL-F3-15-4.                                                NC2534.2
147100     MOVE    SUBTR-15 OF SUBTR-16 TO COMPUTED-N                   NC2534.2
147200     MOVE   "-5.76" TO CORRECT-A                                  NC2534.2
147300     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
147400          TO RE-MARK                                              NC2534.2
147500     PERFORM FAIL.                                                NC2534.2
147600 SUB-WRITE-F3-15-4.                                               NC2534.2
147700     PERFORM PRINT-DETAIL.                                        NC2534.2
147800*                                                                 NC2534.2
147900 SUB-INIT-F3-15-5.                                                NC2534.2
148000     MOVE   "SUB-TEST-F3-15-5" TO PAR-NAME.                       NC2534.2
148100     ADD     1 TO REC-CT.                                         NC2534.2
148200 SUB-TEST-F3-15-5.                                                NC2534.2
148300     IF      WRK-XN-00001 NOT = "Z"                               NC2534.2
148400             GO TO SUB-FAIL-F3-15-5.                              NC2534.2
148500     PERFORM PASS                                                 NC2534.2
148600     GO TO SUB-WRITE-F3-15-5.                                     NC2534.2
148700 SUB-DELETE-F3-15-5.                                              NC2534.2
148800     PERFORM DE-LETE.                                             NC2534.2
148900     GO TO SUB-WRITE-F3-15-5.                                     NC2534.2
149000 SUB-FAIL-F3-15-5.                                                NC2534.2
149100     MOVE   "SCOPE TERMINATOR IGNORED" TO RE-MARK                 NC2534.2
149200     MOVE   "Z"   TO COMPUTED-X                                   NC2534.2
149300     MOVE    WRK-AN-00001 TO CORRECT-X                            NC2534.2
149400     PERFORM FAIL.                                                NC2534.2
149500 SUB-WRITE-F3-15-5.                                               NC2534.2
149600     PERFORM PRINT-DETAIL.                                        NC2534.2
149700*                                                                 NC2534.2
149800 SUB-INIT-F3-16.                                                  NC2534.2
149900*    ===-->  NEW SIZE ERROR TESTS  <--===                         NC2534.2
150000*    ===-->      SIZE ERROR        <--===                         NC2534.2
150100     MOVE   "VI-133 6.25" TO ANSI-REFERENCE.                      NC2534.2
150200     MOVE    SPACE  TO WRK-AN-00001.                              NC2534.2
150300     MOVE    SPACE  TO WRK-XN-00001.                              NC2534.2
150400     MOVE    0      TO REC-CT.                                    NC2534.2
150500     MOVE    1      TO SUBTR-13 OF SUBTR-12.                      NC2534.2
150600     MOVE   -1.725  TO SUBTR-14 OF SUBTR-12.                      NC2534.2
150700     MOVE   76.76   TO SUBTR-15 OF SUBTR-12.                      NC2534.2
150800     MOVE    2      TO SUBTR-13 OF SUBTR-16.                      NC2534.2
150900     MOVE     .23   TO SUBTR-14 OF SUBTR-16.                      NC2534.2
151000     MOVE    1      TO SUBTR-15 OF SUBTR-16.                      NC2534.2
151100 SUB-TEST-F3-16-0.                                                NC2534.2
151200     SUBTRACT CORRESPONDING SUBTR-12 FROM SUBTR-16 ROUNDED        NC2534.2
151300             ON SIZE ERROR                                        NC2534.2
151400             MOVE   "A" TO WRK-AN-00001                           NC2534.2
151500         NOT ON SIZE ERROR                                        NC2534.2
151600             MOVE   "B" TO WRK-AN-00001                           NC2534.2
151700     END-SUBTRACT                                                 NC2534.2
151800     MOVE   "Z" TO WRK-XN-00001.                                  NC2534.2
151900*                                                                 NC2534.2
152000 SUB-INIT-F3-16-1.                                                NC2534.2
152100     MOVE   "SUB-TEST-F3-16-1" TO PAR-NAME.                       NC2534.2
152200     ADD     1 TO REC-CT.                                         NC2534.2
152300 SUB-TEST-F3-16-1.                                                NC2534.2
152400     IF      WRK-AN-00001    NOT = "A"                            NC2534.2
152500             GO TO SUB-FAIL-F3-16-1.                              NC2534.2
152600     PERFORM PASS                                                 NC2534.2
152700     GO TO SUB-WRITE-F3-16-1.                                     NC2534.2
152800 SUB-DELETE-F3-16-1.                                              NC2534.2
152900     PERFORM DE-LETE.                                             NC2534.2
153000     GO TO SUB-WRITE-F3-16-1.                                     NC2534.2
153100 SUB-FAIL-F3-16-1.                                                NC2534.2
153200     MOVE   "ON SIZE ERROR SHOULD BE EXECUTED"                    NC2534.2
153300          TO RE-MARK                                              NC2534.2
153400     MOVE    WRK-AN-00001  TO COMPUTED-X                          NC2534.2
153500     MOVE   "A"    TO CORRECT-X                                   NC2534.2
153600     PERFORM FAIL.                                                NC2534.2
153700 SUB-WRITE-F3-16-1.                                               NC2534.2
153800     PERFORM PRINT-DETAIL.                                        NC2534.2
153900*                                                                 NC2534.2
154000 SUB-INIT-F3-16-2.                                                NC2534.2
154100     MOVE  "SUB-TEST-F3-16-2" TO PAR-NAME.                        NC2534.2
154200     ADD    1 TO REC-CT.                                          NC2534.2
154300 SUB-TEST-F3-16-2.                                                NC2534.2
154400     IF     SUBTR-13 OF SUBTR-16 NOT EQUAL TO 1                   NC2534.2
154500            GO TO SUB-FAIL-F3-16-2.                               NC2534.2
154600     PERFORM PASS                                                 NC2534.2
154700     GO TO SUB-WRITE-F3-16-2.                                     NC2534.2
154800 SUB-DELETE-F3-16-2.                                              NC2534.2
154900     PERFORM DE-LETE.                                             NC2534.2
155000     GO TO SUB-WRITE-F3-16-2.                                     NC2534.2
155100 SUB-FAIL-F3-16-2.                                                NC2534.2
155200     MOVE    SUBTR-13 OF SUBTR-16 TO COMPUTED-N                   NC2534.2
155300     MOVE   "+1" TO CORRECT-A                                     NC2534.2
155400     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
155500          TO RE-MARK                                              NC2534.2
155600     PERFORM FAIL.                                                NC2534.2
155700 SUB-WRITE-F3-16-2.                                               NC2534.2
155800     PERFORM PRINT-DETAIL.                                        NC2534.2
155900*                                                                 NC2534.2
156000 SUB-INIT-F3-16-3.                                                NC2534.2
156100     MOVE  "SUB-TEST-F3-16-3" TO PAR-NAME.                        NC2534.2
156200     ADD    1 TO REC-CT.                                          NC2534.2
156300 SUB-TEST-F3-16-3.                                                NC2534.2
156400     IF     SUBTR-14 OF SUBTR-16 IS NOT EQUAL TO 1.96             NC2534.2
156500            GO TO SUB-FAIL-F3-16-3.                               NC2534.2
156600     PERFORM PASS                                                 NC2534.2
156700     GO TO SUB-WRITE-F3-16-3.                                     NC2534.2
156800 SUB-DELETE-F3-16-3.                                              NC2534.2
156900     PERFORM DE-LETE.                                             NC2534.2
157000     GO TO SUB-WRITE-F3-16-3.                                     NC2534.2
157100 SUB-FAIL-F3-16-3.                                                NC2534.2
157200     MOVE   SUBTR-14 OF SUBTR-16 TO COMPUTED-N                    NC2534.2
157300     MOVE   "+1.96" TO CORRECT-A                                  NC2534.2
157400     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
157500          TO RE-MARK                                              NC2534.2
157600     PERFORM FAIL.                                                NC2534.2
157700 SUB-WRITE-F3-16-3.                                               NC2534.2
157800     PERFORM PRINT-DETAIL.                                        NC2534.2
157900*                                                                 NC2534.2
158000 SUB-INIT-F3-16-4.                                                NC2534.2
158100     MOVE  "SUB-TEST-F3-16-4" TO PAR-NAME.                        NC2534.2
158200     ADD     1 TO REC-CT.                                         NC2534.2
158300 SUB-TEST-F3-16-4.                                                NC2534.2
158400     IF     SUBTR-15 OF SUBTR-16 NOT EQUAL TO 1                   NC2534.2
158500            GO TO SUB-FAIL-F3-16-4.                               NC2534.2
158600     PERFORM PASS                                                 NC2534.2
158700     GO TO SUB-WRITE-F3-16-4.                                     NC2534.2
158800 SUB-DELETE-F3-16-4.                                              NC2534.2
158900     PERFORM DE-LETE.                                             NC2534.2
159000     GO TO SUB-WRITE-F3-16-4.                                     NC2534.2
159100 SUB-FAIL-F3-16-4.                                                NC2534.2
159200     MOVE    SUBTR-15 OF SUBTR-16 TO COMPUTED-N                   NC2534.2
159300     MOVE   "+1" TO CORRECT-A                                     NC2534.2
159400     MOVE   "WRONGLY AFFECTED BY SIZE ERROR" TO RE-MARK           NC2534.2
159500     PERFORM FAIL.                                                NC2534.2
159600 SUB-WRITE-F3-16-4.                                               NC2534.2
159700     PERFORM PRINT-DETAIL.                                        NC2534.2
159800*                                                                 NC2534.2
159900 SUB-INIT-F3-16-5.                                                NC2534.2
160000     MOVE   "SUB-TEST-F3-16-5" TO PAR-NAME.                       NC2534.2
160100     ADD     1 TO REC-CT.                                         NC2534.2
160200 SUB-TEST-F3-16-5.                                                NC2534.2
160300     IF      WRK-XN-00001 NOT = "Z"                               NC2534.2
160400             GO TO SUB-FAIL-F3-16-5.                              NC2534.2
160500     PERFORM PASS                                                 NC2534.2
160600     GO TO SUB-WRITE-F3-16-5.                                     NC2534.2
160700 SUB-DELETE-F3-16-5.                                              NC2534.2
160800     PERFORM DE-LETE.                                             NC2534.2
160900     GO TO SUB-WRITE-F3-16-5.                                     NC2534.2
161000 SUB-FAIL-F3-16-5.                                                NC2534.2
161100     MOVE   "SCOPE TERMINATOR IGNORED" TO RE-MARK                 NC2534.2
161200     MOVE   "Z"   TO COMPUTED-X                                   NC2534.2
161300     MOVE    WRK-AN-00001 TO CORRECT-X                            NC2534.2
161400     PERFORM FAIL.                                                NC2534.2
161500 SUB-WRITE-F3-16-5.                                               NC2534.2
161600     PERFORM PRINT-DETAIL.                                        NC2534.2
161700*                                                                 NC2534.2
161800 SUB-INIT-F3-17.                                                  NC2534.2
161900*    ===-->  NEW SIZE ERROR TESTS  <--===                         NC2534.2
162000*    ===-->   NO SIZE ERROR        <--===                         NC2534.2
162100     MOVE   "VI-133 6.25" TO ANSI-REFERENCE.                      NC2534.2
162200     MOVE    SPACE  TO WRK-AN-00001.                              NC2534.2
162300     MOVE    SPACE  TO WRK-XN-00001.                              NC2534.2
162400     MOVE    0      TO REC-CT.                                    NC2534.2
162500     MOVE    1      TO SUBTR-13 OF SUBTR-12.                      NC2534.2
162600     MOVE   -1.725  TO SUBTR-14 OF SUBTR-12.                      NC2534.2
162700     MOVE    6.76   TO SUBTR-15 OF SUBTR-12.                      NC2534.2
162800     MOVE    2      TO SUBTR-13 OF SUBTR-16.                      NC2534.2
162900     MOVE     .23   TO SUBTR-14 OF SUBTR-16.                      NC2534.2
163000     MOVE    1      TO SUBTR-15 OF SUBTR-16.                      NC2534.2
163100 SUB-TEST-F3-17-0.                                                NC2534.2
163200     SUBTRACT CORRESPONDING SUBTR-12 FROM SUBTR-16 ROUNDED        NC2534.2
163300             ON SIZE ERROR                                        NC2534.2
163400             MOVE   "A" TO WRK-AN-00001                           NC2534.2
163500         NOT ON SIZE ERROR                                        NC2534.2
163600             MOVE   "B" TO WRK-AN-00001                           NC2534.2
163700     END-SUBTRACT                                                 NC2534.2
163800     MOVE   "Z" TO WRK-XN-00001.                                  NC2534.2
163900*                                                                 NC2534.2
164000 SUB-INIT-F3-17-1.                                                NC2534.2
164100     MOVE   "SUB-TEST-F3-17-1" TO PAR-NAME.                       NC2534.2
164200     ADD     1 TO REC-CT.                                         NC2534.2
164300 SUB-TEST-F3-17-1.                                                NC2534.2
164400     IF      WRK-AN-00001    NOT = "B"                            NC2534.2
164500             GO TO SUB-FAIL-F3-17-1.                              NC2534.2
164600     PERFORM PASS                                                 NC2534.2
164700     GO TO SUB-WRITE-F3-17-1.                                     NC2534.2
164800 SUB-DELETE-F3-17-1.                                              NC2534.2
164900     PERFORM DE-LETE.                                             NC2534.2
165000     GO TO SUB-WRITE-F3-17-1.                                     NC2534.2
165100 SUB-FAIL-F3-17-1.                                                NC2534.2
165200     MOVE   "NOT ON SIZE ERROR SHOULD BE EXECUTED"                NC2534.2
165300           TO RE-MARK                                             NC2534.2
165400     MOVE    WRK-AN-00001  TO COMPUTED-X                          NC2534.2
165500     MOVE   "B"    TO CORRECT-X                                   NC2534.2
165600     PERFORM FAIL.                                                NC2534.2
165700 SUB-WRITE-F3-17-1.                                               NC2534.2
165800     PERFORM PRINT-DETAIL.                                        NC2534.2
165900*                                                                 NC2534.2
166000 SUB-INIT-F3-17-2.                                                NC2534.2
166100     MOVE  "SUB-TEST-F3-17-2" TO PAR-NAME.                        NC2534.2
166200     ADD    1 TO REC-CT.                                          NC2534.2
166300 SUB-TEST-F3-17-2.                                                NC2534.2
166400     IF     SUBTR-13 OF SUBTR-16 NOT EQUAL TO 1                   NC2534.2
166500            GO TO SUB-FAIL-F3-17-2.                               NC2534.2
166600     PERFORM PASS                                                 NC2534.2
166700     GO TO SUB-WRITE-F3-17-2.                                     NC2534.2
166800 SUB-DELETE-F3-17-2.                                              NC2534.2
166900     PERFORM DE-LETE.                                             NC2534.2
167000     GO TO SUB-WRITE-F3-17-2.                                     NC2534.2
167100 SUB-FAIL-F3-17-2.                                                NC2534.2
167200     MOVE    SUBTR-13 OF SUBTR-16 TO COMPUTED-N                   NC2534.2
167300     MOVE   "+1" TO CORRECT-A                                     NC2534.2
167400     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
167500          TO RE-MARK                                              NC2534.2
167600     PERFORM FAIL.                                                NC2534.2
167700 SUB-WRITE-F3-17-2.                                               NC2534.2
167800     PERFORM PRINT-DETAIL.                                        NC2534.2
167900*                                                                 NC2534.2
168000 SUB-INIT-F3-17-3.                                                NC2534.2
168100     MOVE  "SUB-TEST-F3-17-3" TO PAR-NAME.                        NC2534.2
168200     ADD    1 TO REC-CT.                                          NC2534.2
168300 SUB-TEST-F3-17-3.                                                NC2534.2
168400     IF     SUBTR-14 OF SUBTR-16 IS NOT EQUAL TO 1.96             NC2534.2
168500            GO TO SUB-FAIL-F3-17-3.                               NC2534.2
168600     PERFORM PASS                                                 NC2534.2
168700     GO TO SUB-WRITE-F3-17-3.                                     NC2534.2
168800 SUB-DELETE-F3-17-3.                                              NC2534.2
168900     PERFORM DE-LETE.                                             NC2534.2
169000     GO TO SUB-WRITE-F3-17-3.                                     NC2534.2
169100 SUB-FAIL-F3-17-3.                                                NC2534.2
169200     MOVE   SUBTR-14 OF SUBTR-16 TO COMPUTED-N                    NC2534.2
169300     MOVE   "+1.96" TO CORRECT-A                                  NC2534.2
169400     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
169500          TO RE-MARK                                              NC2534.2
169600     PERFORM FAIL.                                                NC2534.2
169700 SUB-WRITE-F3-17-3.                                               NC2534.2
169800     PERFORM PRINT-DETAIL.                                        NC2534.2
169900*                                                                 NC2534.2
170000 SUB-INIT-F3-17-4.                                                NC2534.2
170100     MOVE  "SUB-TEST-F3-17-4" TO PAR-NAME.                        NC2534.2
170200     ADD     1 TO REC-CT.                                         NC2534.2
170300 SUB-TEST-F3-17-4.                                                NC2534.2
170400     IF     SUBTR-15 OF SUBTR-16 NOT EQUAL TO -5.76               NC2534.2
170500            GO TO SUB-FAIL-F3-17-4.                               NC2534.2
170600     PERFORM PASS                                                 NC2534.2
170700     GO TO SUB-WRITE-F3-17-4.                                     NC2534.2
170800 SUB-DELETE-F3-17-4.                                              NC2534.2
170900     PERFORM DE-LETE.                                             NC2534.2
171000     GO TO SUB-WRITE-F3-17-4.                                     NC2534.2
171100 SUB-FAIL-F3-17-4.                                                NC2534.2
171200     MOVE    SUBTR-15 OF SUBTR-16 TO COMPUTED-N                   NC2534.2
171300     MOVE   "-5.76" TO CORRECT-A                                  NC2534.2
171400     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
171500          TO RE-MARK                                              NC2534.2
171600     PERFORM FAIL.                                                NC2534.2
171700 SUB-WRITE-F3-17-4.                                               NC2534.2
171800     PERFORM PRINT-DETAIL.                                        NC2534.2
171900*                                                                 NC2534.2
172000 SUB-INIT-F3-17-5.                                                NC2534.2
172100     MOVE   "SUB-TEST-F3-17-5" TO PAR-NAME.                       NC2534.2
172200     ADD     1 TO REC-CT.                                         NC2534.2
172300 SUB-TEST-F3-17-5.                                                NC2534.2
172400     IF      WRK-XN-00001 NOT = "Z"                               NC2534.2
172500             GO TO SUB-FAIL-F3-17-5.                              NC2534.2
172600     PERFORM PASS                                                 NC2534.2
172700     GO TO SUB-WRITE-F3-17-5.                                     NC2534.2
172800 SUB-DELETE-F3-17-5.                                              NC2534.2
172900     PERFORM DE-LETE.                                             NC2534.2
173000     GO TO SUB-WRITE-F3-17-5.                                     NC2534.2
173100 SUB-FAIL-F3-17-5.                                                NC2534.2
173200     MOVE   "SCOPE TERMINATOR IGNORED" TO RE-MARK                 NC2534.2
173300     MOVE   "Z"   TO COMPUTED-X                                   NC2534.2
173400     MOVE    WRK-AN-00001 TO CORRECT-X                            NC2534.2
173500     PERFORM FAIL.                                                NC2534.2
173600 SUB-WRITE-F3-17-5.                                               NC2534.2
173700     PERFORM PRINT-DETAIL.                                        NC2534.2
173800*                                                                 NC2534.2
173900 SUB-INIT-F3-18.                                                  NC2534.2
174000*    ===-->  NEW SIZE ERROR TESTS  <--===                         NC2534.2
174100*    ===-->      SIZE ERROR        <--===                         NC2534.2
174200     MOVE   "VI-133 6.25" TO ANSI-REFERENCE.                      NC2534.2
174300     MOVE    SPACE  TO WRK-AN-00001.                              NC2534.2
174400     MOVE    SPACE  TO WRK-XN-00001.                              NC2534.2
174500     MOVE    0      TO REC-CT.                                    NC2534.2
174600     MOVE    1      TO SUBTR-13 OF SUBTR-12.                      NC2534.2
174700     MOVE   -1.725  TO SUBTR-14 OF SUBTR-12.                      NC2534.2
174800     MOVE   76.76   TO SUBTR-15 OF SUBTR-12.                      NC2534.2
174900     MOVE    2      TO SUBTR-13 OF SUBTR-16.                      NC2534.2
175000     MOVE     .23   TO SUBTR-14 OF SUBTR-16.                      NC2534.2
175100     MOVE    1      TO SUBTR-15 OF SUBTR-16.                      NC2534.2
175200 SUB-TEST-F3-18-0.                                                NC2534.2
175300     SUBTRACT CORRESPONDING SUBTR-12 FROM SUBTR-16 ROUNDED        NC2534.2
175400             ON SIZE ERROR                                        NC2534.2
175500             MOVE   "A" TO WRK-AN-00001                           NC2534.2
175600         NOT ON SIZE ERROR                                        NC2534.2
175700             MOVE   "B" TO WRK-AN-00001                           NC2534.2
175800     END-SUBTRACT                                                 NC2534.2
175900     MOVE   "Z" TO WRK-XN-00001.                                  NC2534.2
176000*                                                                 NC2534.2
176100 SUB-INIT-F3-18-1.                                                NC2534.2
176200     MOVE   "SUB-TEST-F3-18-1" TO PAR-NAME.                       NC2534.2
176300     ADD     1 TO REC-CT.                                         NC2534.2
176400 SUB-TEST-F3-18-1.                                                NC2534.2
176500     IF      WRK-AN-00001    NOT = "A"                            NC2534.2
176600             GO TO SUB-FAIL-F3-18-1.                              NC2534.2
176700     PERFORM PASS                                                 NC2534.2
176800     GO TO SUB-WRITE-F3-18-1.                                     NC2534.2
176900 SUB-DELETE-F3-18-1.                                              NC2534.2
177000     PERFORM DE-LETE.                                             NC2534.2
177100     GO TO SUB-WRITE-F3-18-1.                                     NC2534.2
177200 SUB-FAIL-F3-18-1.                                                NC2534.2
177300     MOVE   "ON SIZE ERROR SHOULD BE EXECUTED"                    NC2534.2
177400          TO RE-MARK                                              NC2534.2
177500     MOVE    WRK-AN-00001  TO COMPUTED-X                          NC2534.2
177600     MOVE   "A"    TO CORRECT-X                                   NC2534.2
177700     PERFORM FAIL.                                                NC2534.2
177800 SUB-WRITE-F3-18-1.                                               NC2534.2
177900     PERFORM PRINT-DETAIL.                                        NC2534.2
178000*                                                                 NC2534.2
178100 SUB-INIT-F3-18-2.                                                NC2534.2
178200     MOVE  "SUB-TEST-F3-18-2" TO PAR-NAME.                        NC2534.2
178300     ADD    1 TO REC-CT.                                          NC2534.2
178400 SUB-TEST-F3-18-2.                                                NC2534.2
178500     IF     SUBTR-13 OF SUBTR-16 NOT EQUAL TO 1                   NC2534.2
178600            GO TO SUB-FAIL-F3-18-2.                               NC2534.2
178700     PERFORM PASS                                                 NC2534.2
178800     GO TO SUB-WRITE-F3-18-2.                                     NC2534.2
178900 SUB-FAIL-F3-18-2.                                                NC2534.2
179000     MOVE    SUBTR-13 OF SUBTR-16 TO COMPUTED-N                   NC2534.2
179100     MOVE   "+1" TO CORRECT-A                                     NC2534.2
179200     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
179300          TO RE-MARK                                              NC2534.2
179400     PERFORM FAIL.                                                NC2534.2
179500 SUB-WRITE-F3-18-2.                                               NC2534.2
179600     PERFORM PRINT-DETAIL.                                        NC2534.2
179700*                                                                 NC2534.2
179800 SUB-INIT-F3-18-3.                                                NC2534.2
179900     MOVE  "SUB-TEST-F3-18-3" TO PAR-NAME.                        NC2534.2
180000     ADD    1 TO REC-CT.                                          NC2534.2
180100 SUB-TEST-F3-18-3.                                                NC2534.2
180200     IF     SUBTR-14 OF SUBTR-16 IS NOT EQUAL TO 1.96             NC2534.2
180300            GO TO SUB-FAIL-F3-18-3.                               NC2534.2
180400     PERFORM PASS                                                 NC2534.2
180500     GO TO SUB-WRITE-F3-18-3.                                     NC2534.2
180600 SUB-DELETE-F3-18-3.                                              NC2534.2
180700     PERFORM DE-LETE.                                             NC2534.2
180800     GO TO SUB-WRITE-F3-18-3.                                     NC2534.2
180900 SUB-FAIL-F3-18-3.                                                NC2534.2
181000     MOVE   SUBTR-14 OF SUBTR-16 TO COMPUTED-N                    NC2534.2
181100     MOVE   "+1.96" TO CORRECT-A                                  NC2534.2
181200     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
181300          TO RE-MARK                                              NC2534.2
181400     PERFORM FAIL.                                                NC2534.2
181500 SUB-WRITE-F3-18-3.                                               NC2534.2
181600     PERFORM PRINT-DETAIL.                                        NC2534.2
181700*                                                                 NC2534.2
181800 SUB-INIT-F3-18-4.                                                NC2534.2
181900     MOVE  "SUB-TEST-F3-18-4" TO PAR-NAME.                        NC2534.2
182000     ADD     1 TO REC-CT.                                         NC2534.2
182100 SUB-TEST-F3-18-4.                                                NC2534.2
182200     IF     SUBTR-15 OF SUBTR-16 NOT EQUAL TO 1                   NC2534.2
182300            GO TO SUB-FAIL-F3-18-4.                               NC2534.2
182400     PERFORM PASS                                                 NC2534.2
182500     GO TO SUB-WRITE-F3-18-4.                                     NC2534.2
182600 SUB-DELETE-F3-18-4.                                              NC2534.2
182700     PERFORM DE-LETE.                                             NC2534.2
182800     GO TO SUB-WRITE-F3-18-4.                                     NC2534.2
182900 SUB-FAIL-F3-18-4.                                                NC2534.2
183000     MOVE    SUBTR-15 OF SUBTR-16 TO COMPUTED-N                   NC2534.2
183100     MOVE   "+1" TO CORRECT-A                                     NC2534.2
183200     MOVE "WRONGLY AFFECTED BY SIZE ERROR" TO RE-MARK             NC2534.2
183300     PERFORM FAIL.                                                NC2534.2
183400 SUB-WRITE-F3-18-4.                                               NC2534.2
183500     PERFORM PRINT-DETAIL.                                        NC2534.2
183600*                                                                 NC2534.2
183700 SUB-INIT-F3-18-5.                                                NC2534.2
183800     MOVE   "SUB-TEST-F3-18-5" TO PAR-NAME.                       NC2534.2
183900     ADD     1 TO REC-CT.                                         NC2534.2
184000 SUB-TEST-F3-18-5.                                                NC2534.2
184100     IF      WRK-XN-00001 NOT = "Z"                               NC2534.2
184200             GO TO SUB-FAIL-F3-18-5.                              NC2534.2
184300     PERFORM PASS                                                 NC2534.2
184400     GO TO SUB-WRITE-F3-18-5.                                     NC2534.2
184500 SUB-DELETE-F3-18-5.                                              NC2534.2
184600     PERFORM DE-LETE.                                             NC2534.2
184700     GO TO SUB-WRITE-F3-18-5.                                     NC2534.2
184800 SUB-FAIL-F3-18-5.                                                NC2534.2
184900     MOVE   "SCOPE TERMINATOR IGNORED" TO RE-MARK                 NC2534.2
185000     MOVE   "Z"   TO COMPUTED-X                                   NC2534.2
185100     MOVE    WRK-AN-00001 TO CORRECT-X                            NC2534.2
185200     PERFORM FAIL.                                                NC2534.2
185300 SUB-WRITE-F3-18-5.                                               NC2534.2
185400     PERFORM PRINT-DETAIL.                                        NC2534.2
185500*                                                                 NC2534.2
185600 SUB-INIT-F3-19.                                                  NC2534.2
185700*    ===-->  NEW SIZE ERROR TESTS  <--===                         NC2534.2
185800*    ===-->   NO SIZE ERROR        <--===                         NC2534.2
185900     MOVE   "VI-133 6.25" TO ANSI-REFERENCE.                      NC2534.2
186000     MOVE    SPACE  TO WRK-AN-00001.                              NC2534.2
186100     MOVE    0      TO REC-CT.                                    NC2534.2
186200     MOVE    1      TO SUBTR-13 OF SUBTR-12.                      NC2534.2
186300     MOVE   -1.725  TO SUBTR-14 OF SUBTR-12.                      NC2534.2
186400     MOVE    6.76   TO SUBTR-15 OF SUBTR-12.                      NC2534.2
186500     MOVE    2      TO SUBTR-13 OF SUBTR-16.                      NC2534.2
186600     MOVE     .23   TO SUBTR-14 OF SUBTR-16.                      NC2534.2
186700     MOVE    1      TO SUBTR-15 OF SUBTR-16.                      NC2534.2
186800 SUB-TEST-F3-19-0.                                                NC2534.2
186900     SUBTRACT CORRESPONDING SUBTR-12 FROM SUBTR-16 ROUNDED        NC2534.2
187000             ON SIZE ERROR                                        NC2534.2
187100             MOVE   "A" TO WRK-AN-00001                           NC2534.2
187200         NOT ON SIZE ERROR                                        NC2534.2
187300             MOVE   "B" TO WRK-AN-00001                           NC2534.2
187400     END-SUBTRACT                                                 NC2534.2
187500     MOVE   "Z" TO WRK-XN-00001.                                  NC2534.2
187600*                                                                 NC2534.2
187700 SUB-INIT-F3-19-1.                                                NC2534.2
187800     MOVE   "SUB-TEST-F3-19-1" TO PAR-NAME.                       NC2534.2
187900     ADD     1 TO REC-CT.                                         NC2534.2
188000 SUB-TEST-F3-19-1.                                                NC2534.2
188100     IF      WRK-AN-00001    NOT = "B"                            NC2534.2
188200             GO TO SUB-FAIL-F3-19-1.                              NC2534.2
188300     PERFORM PASS                                                 NC2534.2
188400     GO TO SUB-WRITE-F3-19-1.                                     NC2534.2
188500 SUB-DELETE-F3-19-1.                                              NC2534.2
188600     PERFORM DE-LETE.                                             NC2534.2
188700     GO TO SUB-WRITE-F3-19-1.                                     NC2534.2
188800 SUB-FAIL-F3-19-1.                                                NC2534.2
188900     MOVE   "NOT ON SIZE ERROR SHOULD BE EXECUTED"                NC2534.2
189000                TO RE-MARK                                        NC2534.2
189100     MOVE    WRK-AN-00001  TO COMPUTED-X                          NC2534.2
189200     MOVE   "B"    TO CORRECT-X                                   NC2534.2
189300     PERFORM FAIL.                                                NC2534.2
189400 SUB-WRITE-F3-19-1.                                               NC2534.2
189500     PERFORM PRINT-DETAIL.                                        NC2534.2
189600*                                                                 NC2534.2
189700 SUB-INIT-F3-19-2.                                                NC2534.2
189800     MOVE  "SUB-TEST-F3-19-2" TO PAR-NAME.                        NC2534.2
189900     ADD    1 TO REC-CT.                                          NC2534.2
190000 SUB-TEST-F3-19-2.                                                NC2534.2
190100     IF     SUBTR-13 OF SUBTR-16 NOT EQUAL TO 1                   NC2534.2
190200            GO TO SUB-FAIL-F3-19-2.                               NC2534.2
190300     PERFORM PASS                                                 NC2534.2
190400     GO TO SUB-WRITE-F3-19-2.                                     NC2534.2
190500 SUB-DELETE-F3-19-2.                                              NC2534.2
190600     PERFORM DE-LETE.                                             NC2534.2
190700     GO TO SUB-WRITE-F3-19-2.                                     NC2534.2
190800 SUB-FAIL-F3-19-2.                                                NC2534.2
190900     MOVE    SUBTR-13 OF SUBTR-16 TO COMPUTED-N                   NC2534.2
191000     MOVE   "+1" TO CORRECT-A                                     NC2534.2
191100     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
191200          TO RE-MARK                                              NC2534.2
191300     PERFORM FAIL.                                                NC2534.2
191400 SUB-WRITE-F3-19-2.                                               NC2534.2
191500     PERFORM PRINT-DETAIL.                                        NC2534.2
191600*                                                                 NC2534.2
191700 SUB-INIT-F3-19-3.                                                NC2534.2
191800     MOVE  "SUB-TEST-F3-19-3" TO PAR-NAME.                        NC2534.2
191900     ADD    1 TO REC-CT.                                          NC2534.2
192000 SUB-TEST-F3-19-3.                                                NC2534.2
192100     IF     SUBTR-14 OF SUBTR-16 IS NOT EQUAL TO 1.96             NC2534.2
192200            GO TO SUB-FAIL-F3-19-3.                               NC2534.2
192300     PERFORM PASS                                                 NC2534.2
192400     GO TO SUB-WRITE-F3-19-3.                                     NC2534.2
192500 SUB-DELETE-F3-19-3.                                              NC2534.2
192600     PERFORM DE-LETE.                                             NC2534.2
192700     GO TO SUB-WRITE-F3-19-3.                                     NC2534.2
192800 SUB-FAIL-F3-19-3.                                                NC2534.2
192900     MOVE   SUBTR-14 OF SUBTR-16 TO COMPUTED-N                    NC2534.2
193000     MOVE   "+1.96" TO CORRECT-A                                  NC2534.2
193100     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
193200          TO RE-MARK                                              NC2534.2
193300     PERFORM FAIL.                                                NC2534.2
193400 SUB-WRITE-F3-19-3.                                               NC2534.2
193500     PERFORM PRINT-DETAIL.                                        NC2534.2
193600*                                                                 NC2534.2
193700 SUB-INIT-F3-19-4.                                                NC2534.2
193800     MOVE  "SUB-TEST-F3-19-4" TO PAR-NAME.                        NC2534.2
193900     ADD     1 TO REC-CT.                                         NC2534.2
194000 SUB-TEST-F3-19-4.                                                NC2534.2
194100     IF     SUBTR-15 OF SUBTR-16 NOT EQUAL TO -5.76               NC2534.2
194200            GO TO SUB-FAIL-F3-19-4.                               NC2534.2
194300     PERFORM PASS                                                 NC2534.2
194400     GO TO SUB-WRITE-F3-19-4.                                     NC2534.2
194500 SUB-DELETE-F3-19-4.                                              NC2534.2
194600     PERFORM DE-LETE.                                             NC2534.2
194700     GO TO SUB-WRITE-F3-19-4.                                     NC2534.2
194800 SUB-FAIL-F3-19-4.                                                NC2534.2
194900     MOVE    SUBTR-15 OF SUBTR-16 TO COMPUTED-N                   NC2534.2
195000     MOVE   "-5.76" TO CORRECT-A                                  NC2534.2
195100     MOVE "WRONGLY AFFECTED BY SIZE ERROR ON OTHER OPERAND"       NC2534.2
195200          TO RE-MARK                                              NC2534.2
195300     PERFORM FAIL.                                                NC2534.2
195400 SUB-WRITE-F3-19-4.                                               NC2534.2
195500     PERFORM PRINT-DETAIL.                                        NC2534.2
195600*                                                                 NC2534.2
195700 SUB-INIT-F3-19-5.                                                NC2534.2
195800     MOVE   "SUB-TEST-F3-19-5" TO PAR-NAME.                       NC2534.2
195900     ADD     1 TO REC-CT.                                         NC2534.2
196000 SUB-TEST-F3-19-5.                                                NC2534.2
196100     IF      WRK-XN-00001 NOT = "Z"                               NC2534.2
196200             GO TO SUB-FAIL-F3-19-5.                              NC2534.2
196300     PERFORM PASS                                                 NC2534.2
196400     GO TO SUB-WRITE-F3-19-5.                                     NC2534.2
196500 SUB-DELETE-F3-19-5.                                              NC2534.2
196600     PERFORM DE-LETE.                                             NC2534.2
196700     GO TO SUB-WRITE-F3-19-5.                                     NC2534.2
196800 SUB-FAIL-F3-19-5.                                                NC2534.2
196900     MOVE   "SCOPE TERMINATOR IGNORED" TO RE-MARK                 NC2534.2
197000     MOVE   "Z"   TO COMPUTED-X                                   NC2534.2
197100     MOVE    WRK-AN-00001 TO CORRECT-X                            NC2534.2
197200     PERFORM FAIL.                                                NC2534.2
197300 SUB-WRITE-F3-19-5.                                               NC2534.2
197400     PERFORM PRINT-DETAIL.                                        NC2534.2
197500*                                                                 NC2534.2
197600 CCVS-EXIT SECTION.                                               NC2534.2
197700 CCVS-999999.                                                     NC2534.2
197800     GO TO CLOSE-FILES.                                           NC2534.2
*END-OF,NC253A                                                                  