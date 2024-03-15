000100 IDENTIFICATION DIVISION.                                        
000200 PROGRAM-ID.    LAB14.                                           
000300 AUTHOR.        SUZI Q.                                          
000400 DATE-WRITTEN.  SEPTEMBER 2005.                                  
000500 DATE-COMPILED.                                                  
000600*****************************************************************
000700*                                                                
000800*-------------P R O G R A M  D E S C R I P T I O N--------------*
000900*                                                               *
001000*    ----------------- LAB14 -----------------------            *
001100*                                                               *
001200*    PURPOSE  THIS PROGRAM HELPS TRACK MONTHLY BONUSES          * 
001300*    =======     INCLUDING CALCULATING FED TAX AND              * 
001400*                STATE TAX, NET BONUS, AS WELL AS               * 
001500*                CONTROLLING PAGE BREAKS                        * 
001600*                                                               * 
001700*    NOTE:    THIS PROGRAM USES THE COBOL SORT VERB             * 
001800*    =====                                                      * 
001900*                                                               * 
002000*    INPUT   IN-BONUS-REC     COPYLIB MEMBER BONUSCMP           * 
002100*    =====                                                      * 
002200*                                                               * 
002300*    PROCESS 1. LOAD THE STATE TAX INFO.                        * 
002400*    ======= 2. READ THE BONUS FILE.                            * 
002500*            3. MOVE THE DATA TO THE OUTPUT REPORT.             * 
002600*            4. WRITE THE OUTPUT RECORD.                        * 
002700*                                                               * 
002800*    OUTPUT  THE MONTHLY BONUS REPORT IS THE ONLY OUTPUT.       * 
002900*    ======                                                     * 
003000*                                                               * 
003100*    CALLING PROGRAM(S)  :   NONE                               * 
003200*                                                               * 
003300*    CALLED  PROGRAM(S)  :   LAB10                             *  
003400*                                                               * 
003500*---------------------------------------------------------------* 
003600*                 UPDATE LOG                                    * 
003700*---------------------------------------------------------------* 
003800*                                                               * 
003900* PERSON  PROJECT   DATE      DESCRIPTION          PROGRAM   VER* 
004000* ------  --------  --------  -------------------  -------   ---* 
004100* JOHN Q  0000-001  09/21/05  COPIED FROM LAB14    LAB14     000* 
004200***************************************************************** 
004300*                                                               * 
004400***************************************************************** 
004500*         E N V I R O N M E N T     D I V I S I O N               
004600***************************************************************** 
004700 ENVIRONMENT DIVISION.                                            
004800 INPUT-OUTPUT SECTION.                                           
004900 FILE-CONTROL.                                                   
005000     SELECT IN-FILE        ASSIGN TO INBONUS                     
005100            FILE STATUS IS WS-INFILE-STATUS-CODE.                
005200     SELECT SORT-FILE      ASSIGN TO SORTWK01.                   
005300     SELECT TAX-FILE       ASSIGN TO INSTTAX                     
005400            FILE STATUS IS WS-TAXFILE-STATUS-CODE.               
005500     SELECT OUT-REPORT     ASSIGN TO OUTREPT                     
005600            FILE STATUS IS WS-OUTFILE-STATUS-CODE.               
005700*****************************************************************
005800*                  D A T A     D I V I S I O N                   
005900*****************************************************************
006000 DATA DIVISION.                                                  
006100 FILE SECTION.                                                   
006200                                                                 
006300*****************************************************************
006400*    IN-FILE                                       INPUT         
006500*****************************************************************
006600 FD  IN-FILE                                                     
006700     RECORDING F                                                 
006800     LABEL RECORDS STANDARD                                      
006900     RECORD CONTAINS 76 CHARACTERS                               
007000     BLOCK CONTAINS 0 RECORDS                                    
007100     DATA RECORD IS IN-RECORD.                                   
007200                                                                 
007300 01  IN-RECORD.                                                  
007400     05  IR-STATE-CODE           PIC X(02).                      
007500     05  IR-LAST-NAME            PIC X(20).                      
007600     05  IR-FIRST-NAME           PIC X(15).                      
007700     05  IR-MID-INIT             PIC X(01).                      
007800     05  IR-BONUS-AMT            PIC 9(07)V99   COMP-3.          
007900     05  IR-FED-TAX-EXEMPT-CODE  PIC X(01).                      
008000     05  IR-ST-TAX-EXEMPT-CODE   PIC X(01).                      
008100     05  FILLER                  PIC X(31).                      
008200                                                                 
008300*****************************************************************
008400*    SORT-FILE                                    WORKING        
008500*****************************************************************
008600 SD  SORT-FILE                                                   
008700     RECORD CONTAINS 76 CHARACTERS                               
008800     DATA RECORD IS SORT-RECORD.                                 
008900                                                                 
009000 01  SORT-RECORD.                                                
009100     05  SR-STATE-CODE           PIC X(02).                      
009200     05  SR-LAST-NAME            PIC X(20).                      
009300     05  SR-FIRST-NAME           PIC X(15).                      
009400     05  SR-MID-INIT             PIC X(01).                      
009500     05  SR-BONUS-AMT            PIC 9(07)V99   COMP-3.          
009600     05  SR-FED-TAX-EXEMPT-CODE  PIC X(01).                      
009700         88  FED-TAX-EXEMPT              VALUE 'Y'.              
009800         88  FED-TAX-NON-EXEMPT          VALUE 'N'.              
009900     05  SR-ST-TAX-EXEMPT-CODE   PIC X(01).                      
010000         88  STATE-TAX-EXEMPT            VALUE 'Y'.              
010100         88  STATE-TAX-NON-EXEMPT        VALUE 'N'.              
010200     05  FILLER                  PIC X(31).                      
010300                                                                 
010400*****************************************************************
010500*    STATE TAX FILE FOR TABLE                      INPUT         
010600*****************************************************************
010700 FD  TAX-FILE                                                    
010800     RECORDING F                                                 
010900     LABEL RECORDS STANDARD                                      
011000     RECORD CONTAINS 80 CHARACTERS                               
011100     BLOCK CONTAINS 0 RECORDS                                    
011200     DATA RECORD IS TAX-RECORD.                                  
011300                                                                 
011400 01  TAX-RECORD.                                                 
011500     05  TAX-ENTRY               PIC X(09).                      
011600     05  TAX-FILLER              PIC X(71).                      
011700                                                                 
011800*****************************************************************
011900*    OUT-REPORT                                    OUTPUT        
012000*****************************************************************
012100 FD  OUT-REPORT                                                  
012200     RECORDING F                                                 
012300     LABEL RECORDS STANDARD                                      
012400     RECORD CONTAINS 133 CHARACTERS                              
012500     BLOCK CONTAINS 0 RECORDS                                    
012600     DATA RECORD IS OUT-RECORD.                                  
012700                                                                 
012800 01  OUT-RECORD.                                                 
012900     05  FILLER                  PIC X(133).                     
013000                                                                 
013100*****************************************************************
013200*    W O R K I N G - S T O R A G E                               
013300*****************************************************************
013400                                                                 
013500 WORKING-STORAGE SECTION.                                        
013600                                                                 
013700 01  WS-START-OF-WORKING-STORAGE.                                
013800     05 WS-START-OF-WS-MARKER           PIC X(37)                 
013900        VALUE 'LAB14 WORKING STORAGE BEGINS HERE'.                
014000***********************                                           
014100*  CONSTANTS          *                                           
014200***********************                                           
014300                                                                  
014400 01  WS-CONSTANTS.                                                
014500     05  WS-CONSTANTS-MARKER     PIC X(09)      VALUE             
014600                                                'CONSTANTS'.      
014700     05  WS-C-MAX-PAGE-LINES     PIC S9(03)     COMP-3 VALUE 46.  
014800     05  WS-C-FED-TAX-RATE       PIC S9(01)V999 COMP-3 VALUE .28. 
014900     05  WS-C-TAX-TABLE-SIZE     PIC S9(03)     COMP-3 VALUE 50.  
015000     05  WS-C-DOUBLE-BLANK-LINE  PIC X(02)      VALUE '0 '.       
015100     05  WS-C-TRIPLE-BLANK-LINE  PIC X(02)      VALUE '- '.       
015200                                                                  
015300                                                                  
015400***********************                                           
015500*  ERROR CODES        *                                           
015600***********************                                          
015700                                                                 
015800 01  WS-ERROR-CODES.                                             
015900     05  WS-ERROR-CODE-MARKER    PIC X(11) VALUE 'ERROR CODES'.  
016000     05  WS-INFILE-STATUS-CODE   PIC X(2)  VALUE '**'.           
016100     05  WS-TAXFILE-STATUS-CODE  PIC X(2)  VALUE '**'.           
016200     05  WS-OUTFILE-STATUS-CODE  PIC X(2)  VALUE '**'.           
016300     05  WS-ABORT-TRIGGER        PIC S9(1) VALUE 0.              
016400     05  WS-ABORT-CODE           PIC S9(1).                      
016500     05  WS-RETURN-CODE-DISPLAY  PIC S9(5) COMP-3.               
016600                                                                 
016700***********************                                          
016800*  VARIABLES          *                                          
016900***********************                                          
017000                                                                 
017100 01  GENERAL-VARIABLES.                                          
017200     05  WS-VARIABLES-MARKER     PIC X(09) VALUE 'VARIABLES'.    
017300     05  WS-CURRENT-STATE-GROUP  PIC X(02) VALUE '**'.           
017400     05  WS-DATE-PGM             PIC X(08) VALUE 'LAB10 '.        
017500                                                                  
017600 01  WS-TIME-VARIABLES.                                           
017700     05  WS-DATE-TIME-LONG       PIC X(21).                       
017800     05  WS-TIME-HOUR-C3         PIC S9(02) COMP-3.               
017900     05  WS-DATE-LINE            PIC X(35)  JUSTIFIED RIGHT.      
018000     05  WS-TIME-LINE            PIC X(14).                       
018100                                                                  
018200 01  WS-WORKING-VALUES.                                           
018300     05  WS-FED-TAX-AMT-C3       PIC S9(09)V99  COMP-3 VALUE 0.   
018400     05  WS-STATE-TAX-AMT-C3     PIC S9(09)V99  COMP-3 VALUE 0.   
018500     05  WS-STATE-RATE-C3        PIC V9(06)            VALUE 0.   
018600     05  WS-NET-BONUS-AMT-C3     PIC S9(09)V99  COMP-3 VALUE 0.   
018700                                                                  
018800 01  WS-RUN-STATE-SUBTOTALS.                                      
018900     05  WS-RUN-ST-SUB-FEDTX     PIC S9(09)V99  COMP-3 VALUE 0.   
019000     05  WS-RUN-ST-SUB-STATETX   PIC S9(09)V99  COMP-3 VALUE 0.   
019100     05  WS-RUN-ST-SUB-GROSS     PIC S9(09)V99  COMP-3 VALUE 0.   
019200     05  WS-RUN-ST-SUB-NET       PIC S9(09)V99  COMP-3 VALUE 0.  
019300                                                                 
019400 01  WS-RUN-GRAND-TOTALS.                                        
019500     05  WS-RUN-GTOT-FEDTX       PIC S9(09)V99  COMP-3 VALUE 0.  
019600     05  WS-RUN-GTOT-STATETX     PIC S9(09)V99  COMP-3 VALUE 0.  
019700     05  WS-RUN-GTOT-GROSS       PIC S9(09)V99  COMP-3 VALUE 0.  
019800     05  WS-RUN-GTOT-NET         PIC S9(09)V99  COMP-3 VALUE 0.  
019900                                                                 
020000**************************                                       
020100*  TAX TABLE DEFINITION  *                                       
020200**************************                                       
020300 01  TAX-TABLE.                                                  
020400     05  TAXES      OCCURS 50 TIMES                              
020500             ASCENDING KEY IS TT-STATE                           
020600             INDEXED BY TT-NDX.                                  
020700         10  TT-STATE            PIC X(02) VALUE HIGH-VALUES.    
020800         10  TT-FILLER           PIC X(01).                      
020900         10  TT-TAX              PIC V9(06).                     
021000                                                                  
021100***********************                                           
021200*  ACCUMULATORS       *                                           
021300***********************                                           
021400 01  ACCUMULATORS.                                                
021500     05  WS-ACCUMULATORS-MARKER  PIC X(12)  VALUE 'ACCUMULATORS'. 
021600     05  WS-REC-IN               PIC S9(04) COMP-3 VALUE +0.      
021700     05  WS-REC-OUT              PIC S9(04) COMP-3 VALUE +0.      
021800     05  WS-DETAIL-LINE-CTR      PIC S9(03) COMP-3.               
021900     05  WS-PAGE-CTR             PIC S9(05) COMP-3.               
022000                                                                  
022100***********************                                           
022200*  SWITCHES           *                                           
022300***********************                                           
022400 01  SWITCHES.                                                    
022500     05  WS-SWITCH-MARKER        PIC X(08) VALUE 'SWITCHES'.      
022600     05  WS-BONUS-FILE-SWITCH    PIC X(01) VALUE 'N'.             
022700         88  WS-BONUS-EOF                  VALUE 'Y'.             
022800     05  WS-SORT-FILE-SWITCH     PIC X(01) VALUE 'N'.           
022900         88  WS-SORT-EOF                   VALUE 'Y'.           
023000     05  WS-TAX-TABLE-SWITCH     PIC X(01) VALUE 'N'.           
023100         88  WS-TAX-TABLE-IS-FULL          VALUE 'Y'.           
023200     05  WS-TAX-FILE-SWITCH      PIC X(01) VALUE 'N'.           
023300         88  WS-TAX-FILE-EOF               VALUE 'Y'.           
023400 01  STATE-CODE-SWITCH           PIC X(01) VALUE 'N'.           
023500     88  STATE-CODE-FOUND                  VALUE 'Y'.           
023600                                                                
023700***********************                                         
023800*  MESSAGE STRINGS   *                                          
023900***********************                                         
024000 01  WS-MESSAGE-STRINGS.                                        
024100     05  WS-MESSAGE-MARKER       PIC X(08) VALUE 'MESSAGES'.    
024200                                                                
024300 01  WS-ABORT-MSG-STRING.                                       
024400     05  WS-ABORT-MSG-ERRLB      PIC X(06)  VALUE 'ERROR '.     
024500     05  WS-ABORT-MSG-EDESC      PIC X(20).                     
024600     05  WS-ABORT-MSG-IN         PIC X(14) VALUE ' IN PARAGRAPH '.
024700     05  WS-ABORT-MSG-PGRPH      PIC X(28).                       
024800     05  WS-ABORT-MSG-RCLBL      PIC X(08)  VALUE 'SYS RC: '.     
024900     05  WS-ABORT-MSG-SYSRC      PIC X(02).                       
025000                                                                  
025100***********************                                           
025200*  RECORD LAYOUTS     *                                           
025300***********************                                           
025400 01  WS-RECORD-LAYOUTS.                                           
025500     05 WS-RECORD-LAYOUT-MARKER  PIC X(14) VALUE                  
025600                                 'RECORD LAYOUTS'.                
025700                                                                  
025800 01  WS-REPORT-HEADER-1.                                          
025900     05 WS-RH1-ASA-CODE          PIC X(01) VALUE '1'.             
026000     05 WS-RH1-REPT-ID-LABEL     PIC X(11) VALUE 'REPORT ID:'.    
026100     05 WS-RH1-REPT-ID           PIC X(08) VALUE 'LAB14'.         
026200     05 WS-RH1-COL-DIV-1         PIC X(43) VALUE SPACES.          
026300     05 WS-RH1-REPT-TITLE        PIC X(21) VALUE                  
026400                                 "MEL'S AMAZING SHOES".           
026500     05 WS-RH1-COL-DIV-2         PIC X(33) VALUE SPACES.          
026600     05 WS-RH1-PG-NBR-LABEL      PIC X(05) VALUE 'PAGE:'.         
026700     05 WS-RH1-COL-DIV-3         PIC X(06) VALUE SPACES.          
026800     05 WS-RH1-PG-NBR            PIC ZZZZ9.                       
026900                                                                  
027000 01  WS-REPORT-HEADER-2.                                          
027100     05 WS-RH2-ASA-CODE          PIC X(01) VALUE SPACES.          
027200     05 WS-RH2-LEADER            PIC X(62) VALUE SPACES.          
027300     05 WS-RH2-REPT-TITLE        PIC X(20) VALUE                  
027400                                 'MONTHLY BONUS REPORT'.          
027500     05 WS-RH2-COL-DIV-1         PIC X(15) VALUE SPACES.          
027600     05 WS-RH2-DATE              PIC X(35) JUSTIFIED RIGHT.       
027700                                                                  
027800 01  WS-PAGE-HEADER.                                              
027900     05 WR-PH-ASA-CODE           PIC X(01)  VALUE SPACES.         
028000     05 WS-PH-LEADER             PIC X(118) VALUE SPACES.         
028100     05 WS-PH-TIME               PIC X(14)  VALUE SPACES.         
028200                                                               
028300 01  WS-CONTROL-HEADER-1.                                      
028400     05 WS-CH1-ASA-CODE          PIC X(01) VALUE '-'.          
028500     05 WS-CH1-NAME-LABEL        PIC X(04) VALUE 'NAME'.       
028600     05 WS-CH1-COL-DIV-1         PIC X(35) VALUE SPACES.       
028700     05 WS-CH1-STATE-LABEL       PIC X(05) VALUE 'STATE'.      
028800     05 WS-CH1-COL-DIV-2         PIC X(07) VALUE SPACES.       
028900     05 WS-CH1-GROSS-LABEL       PIC X(05) VALUE 'GROSS'.      
029000     05 WS-CH1-COL-DIV-3         PIC X(10) VALUE SPACES.       
029100     05 WS-CH1-TAX-LABEL         PIC X(07) VALUE 'FED TAX'.    
029200     05 WS-CH1-COL-DIV-4         PIC X(07) VALUE SPACES.       
029300     05 WS-CH1-STATE-TAX-LABEL   PIC X(09) VALUE 'STATE TAX'.  
029400     05 WS-CH1-COL-DIV-5         PIC X(11) VALUE SPACES.       
029500     05 WS-CH1-NET-LABEL         PIC X(03) VALUE 'NET'.        
029600     05 WS-CH1-COL-DIV-6         PIC X(08) VALUE SPACES.       
029700     05 WS-CH1-MSG-LABEL         PIC X(07) VALUE 'MESSAGE'.    
029800     05 WS-CH1-FILLER            PIC X(14) VALUE SPACES.       
029900                                                               
030000 01  WS-CONTROL-HEADER-2.                                       
030100     05 WS-CH2-ASA-CODE          PIC X(01)  VALUE '+'.          
030200     05 WS-CH2-UNDERSCORE-1      PIC X(132) VALUE ALL '_'.      
030300                                                                
030400 01  WS-WORKING-RECORD.                                         
030500     05 WR-ASA-CODE              PIC X(01) VALUE SPACES.        
030600     05 WR-LAST-NAME             PIC X(20).                     
030700     05 WR-COL-DIV-1             PIC X(01) VALUE SPACES.        
030800     05 WR-FIRST-NAME            PIC X(15).                     
030900     05 WR-COL-DIV-2             PIC X(01) VALUE SPACES.        
031000     05 WR-MID-INIT              PIC X(01).                     
031100     05 WR-COL-DIV-3             PIC X(03) VALUE SPACES.        
031200     05 WR-STATE-CODE            PIC X(02).                     
031300     05 WR-COL-DIV-4             PIC X(03) VALUE SPACES.        
031400     05 WR-BONUS-AMT             PIC ZZZ,ZZZ,ZZ9.99-.           
031500     05 WR-COL-DIV-5             PIC X(01) VALUE SPACES.        
031600     05 WR-FED-TAX-AMT           PIC ZZZ,ZZZ,ZZ9.99-.           
031700     05 WR-COL-DIV-6             PIC X(01) VALUE SPACES.        
031800     05 WR-STATE-TAX-AMT         PIC ZZZ,ZZZ,ZZ9.99-.           
031900     05 WR-COL-DIV-6             PIC X(01) VALUE SPACES.        
032000     05 WR-NET-BONUS-AMT         PIC ZZZ,ZZZ,ZZ9.99-.           
032100     05 WR-COL-DIV-7             PIC X(01) VALUE SPACES.        
032200     05 WR-MESSAGE               PIC X(21).                     
032300     05 WR-FILLER                PIC X(01) VALUE SPACES.        
032400                                                                
032500 01  WS-STATE-SUBTOTAL-REC.                                     
032600     05 WS-SSR-LEADER            PIC X(22) VALUE SPACES.        
032700     05 WS-SSR-LINE-LABEL        PIC X(20) VALUE                
032800                                 'TOTAL FOR STATE :   '.        
032900     05 WS-SSR-STATE             PIC X(02) VALUE '**'.          
033000     05 WS-SSR-COL-DIV-1         PIC X(03) VALUE SPACES.        
033100     05 WS-SSR-GROSS             PIC ZZZ,ZZZ,ZZ9.99-.           
033200     05 WS-SSR-COL-DIV-2         PIC X(01) VALUE SPACES.        
033300     05 WS-SSR-FEDTX             PIC ZZZ,ZZZ,ZZ9.99-.           
033400     05 WS-SSR-COL-DIV-3         PIC X(01) VALUE SPACES.        
033500     05 WS-SSR-STATETX           PIC ZZZ,ZZZ,ZZ9.99-.           
033600     05 WS-SSR-COL-DIV-4         PIC X(01) VALUE SPACES.        
033700     05 WS-SSR-NET               PIC ZZZ,ZZZ,ZZ9.99-.           
033800     05 WS-SSR-FILLER            PIC X(23) VALUE SPACES.        
033900                                                                
034000 01  WS-GRAND-TOTAL-REC.                                        
034100     05 WS-GTR-ASA-CODE          PIC X(01) VALUE '+'.           
034200     05 WS-GTR-LEADER            PIC X(21) VALUE SPACES.        
034300     05 WS-GTR-LINE-LABEL        PIC X(20) VALUE                
034400                                 'TOTAL FOR ALL   :   '.        
034500     05 WS-GTR-COL-DIV-1         PIC X(05) VALUE SPACES.        
034600     05 WS-GTR-GROSS             PIC ZZZ,ZZZ,ZZ9.99-.           
034700     05 WS-GTR-COL-DIV-2         PIC X(01) VALUE SPACES.        
034800     05 WS-GTR-FEDTX             PIC ZZZ,ZZZ,ZZ9.99-.           
034900     05 WS-GTR-COL-DIV-3         PIC X(01) VALUE SPACES.        
035000     05 WS-GTR-STATETX           PIC ZZZ,ZZZ,ZZ9.99-.           
035100     05 WS-GTR-COL-DIV-4         PIC X(01) VALUE SPACES.        
035200     05 WS-GTR-NET               PIC ZZZ,ZZZ,ZZ9.99-.           
035300     05 WS-GTR-FILLER            PIC X(23) VALUE SPACES.        
035400                                                                  
035500 01  WS-REPORT-FOOTER-1.                                          
035600     05 WS-RF1-ASA-CODE          PIC X(01) VALUE '-'.             
035700     05 WS-RF1-LEADER-1          PIC X(61) VALUE SPACES.          
035800     05 WS-RF1-MESSAGE           PIC X(28) VALUE                  
035900                                 '***    END OF REPORT    ***'.   
036000     05 WS-RF1-FILLER            PIC X(43) VALUE SPACES.          
036100                                                                  
036200 01  WS-END-OF-WORKING-STORAGE.                                   
036300     05 WS-END-OF-WS-MARKER      PIC X(35) VALUE                  
036400        'LAB14 WORKING STORAGE ENDS HERE'.                        
036500                                                                  
036600***************************************************************** 
036700*          P R O C E D U R E     D I V I S I O N                  
036800***************************************************************** 
036900                                                                  
037000 PROCEDURE DIVISION.                                              
037100 0000-MAINLINE.                                                   
037200     PERFORM 1000-INITIALIZATION THRU 1000-EXIT                   
037300     PERFORM 1500-SORT-INPUT     THRU 1500-EXIT                   
037400     PERFORM 3000-CHECK-RESULTS  THRU 3000-EXIT                   
037500     PERFORM 4000-TERMINATE      THRU 4000-EXIT                   
037600     GOBACK                                                       
037700                                                                  
037800     .                                                            
037900***************************************************************** 
038000*  INITIALIZATION ROUTINE                                         
038100***************************************************************** 
038200                                                                  
038300 1000-INITIALIZATION.                                             
038400                                                                  
038500     OPEN INPUT TAX-FILE                                          
038600     IF WS-TAXFILE-STATUS-CODE = '00' THEN                        
038700         CONTINUE                                                 
038800     ELSE                                                         
038900         MOVE '1000-INITIALIZE, ' TO WS-ABORT-MSG-PGRPH        
039000         MOVE 'OPENING TAX-FILE, ' TO WS-ABORT-MSG-EDESC          
039100         MOVE WS-TAXFILE-STATUS-CODE TO WS-ABORT-MSG-SYSRC        
039200         MOVE 1057 TO RETURN-CODE                                 
039300         PERFORM 9999-ABORT THRU 9999-EXIT                        
039400     END-IF                                                       
039500                                                                  
039600     OPEN OUTPUT OUT-REPORT                                       
039700     IF WS-OUTFILE-STATUS-CODE = '00' THEN                        
039800         CONTINUE                                                 
039900     ELSE                                                         
040000         MOVE '1000-INITIALIZE, ' TO WS-ABORT-MSG-PGRPH           
040100         MOVE 'OPENING OUT-REPORT FILE, ' TO WS-ABORT-MSG-EDESC   
040200         MOVE WS-OUTFILE-STATUS-CODE TO WS-ABORT-MSG-SYSRC        
040300         MOVE 1056 TO RETURN-CODE                                 
040400         PERFORM 9999-ABORT THRU 9999-EXIT                        
040500     END-IF                                                       
040600                                                                  
040700     SET TT-NDX TO 1                                              
040800     PERFORM 8150-READ-TAX-FILE THRU 8150-EXIT                   
040900                                                                 
041000     IF WS-TAX-FILE-EOF THEN                                     
041100        MOVE '1000-INITIATION, ' TO WS-ABORT-MSG-PGRPH           
041200        MOVE '- TAX FILE EMPTY, ' TO WS-ABORT-MSG-EDESC          
041300        MOVE WS-TAXFILE-STATUS-CODE TO WS-ABORT-MSG-SYSRC        
041400        MOVE 1089 TO RETURN-CODE                                 
041500        PERFORM 9999-ABORT THRU 9999-EXIT                        
041600     ELSE                                                        
041700        CONTINUE                                                 
041800     END-IF                                                      
041900                                                                 
042000     PERFORM 8100-LOAD-TAX-TABLE THRU 8100-EXIT                  
042100         UNTIL WS-TAX-TABLE-IS-FULL                              
042200         OR WS-TAX-FILE-EOF                                      
042300                                                                 
042400     MOVE 1 TO WS-PAGE-CTR                                       
042500     COMPUTE WS-DETAIL-LINE-CTR = WS-C-MAX-PAGE-LINES + 1        
042600                                                                  
042700     MOVE FUNCTION CURRENT-DATE TO WS-DATE-TIME-LONG              
042800     PERFORM 2700-BUILD-DATE-TIME THRU 2700-EXIT                  
042900                                                                  
043000     .                                                            
043100 1000-EXIT.                                                       
043200     EXIT.                                                        
043300                                                                  
043400***************************************************************** 
043500*  SORT INPUT ROUTINE                                             
043600***************************************************************** 
043700                                                                  
043800 1500-SORT-INPUT.                                                 
043900     SORT SORT-FILE                                               
044000         ASCENDING KEY SR-STATE-CODE                              
044100         DESCENDING KEY SR-BONUS-AMT                              
044200         USING IN-FILE                                            
044300         OUTPUT PROCEDURE IS 2000-PROCESS THRU 2000-EXIT          
044400                                                                  
044500     IF SORT-RETURN NOT = 0 THEN                                  
044600         DISPLAY 'SORT ABENDED - SORT-RETURN = ', SORT-RETURN     
044700     END-IF                                                       
044800                                                                  
044900     .                                                            
045000                                                                  
045100 1500-EXIT.                                                       
045200     EXIT.                                                        
045300                                                                  
045400***************************************************************** 
045500*  PROCESS ROUTINE                                                
045600***************************************************************** 
045700                                                                  
045800 2000-PROCESS.                                                    
045900     PERFORM 8000-READ-BONUS THRU 8000-EXIT                       
046000                                                                  
046100     IF WS-SORT-EOF THEN                                          
046200         MOVE '2000-PROCESS, ' TO WS-ABORT-MSG-PGRPH              
046300         MOVE '- INPUT FILE EMPTY, ' TO WS-ABORT-MSG-EDESC        
046400         MOVE WS-INFILE-STATUS-CODE TO WS-ABORT-MSG-SYSRC         
046500         MOVE 2088 TO RETURN-CODE                                 
046600         PERFORM 9999-ABORT THRU 9999-EXIT                        
046700     ELSE                                                         
046800         MOVE SR-STATE-CODE TO WS-CURRENT-STATE-GROUP             
046900     END-IF                                                       
047000                                                                  
047100     PERFORM UNTIL WS-SORT-EOF                                    
047200         IF WS-DETAIL-LINE-CTR > WS-C-MAX-PAGE-LINES THEN         
047300             PERFORM 2100-WRITE-REPORT-HEADER THRU 2100-EXIT      
047400             PERFORM 2200-WRITE-PAGE-HEADER THRU 2200-EXIT        
047500             PERFORM 2300-WRITE-CONTROL-HEADER THRU 2300-EXIT     
047600             MOVE 7 TO WS-DETAIL-LINE-CTR                         
047700             ADD +1 TO WS-PAGE-CTR                                
047800         END-IF                                                   
047900         IF SR-STATE-CODE = WS-CURRENT-STATE-GROUP                
048000             PERFORM 2400-WRITE-DETAIL-LINE THRU 2400-EXIT        
048100             ADD +1 TO WS-DETAIL-LINE-CTR                         
048200             PERFORM 8000-READ-BONUS THRU 8000-EXIT               
048300         ELSE                                                     
048400             PERFORM 2500-PRINT-STATE-SUBTOTAL THRU 2500-EXIT     
048500         END-IF                                                   
048600     END-PERFORM                                                  
048700                                                                  
048800     PERFORM 2500-PRINT-STATE-SUBTOTAL THRU 2500-EXIT             
048900     PERFORM 2600-PRINT-GRAND-TOTAL THRU 2600-EXIT                
049000                                                                  
049100     MOVE WS-REPORT-FOOTER-1 TO OUT-RECORD                        
049200     PERFORM 8500-WRITE-REPORT-RECORD                             
049300                                                                  
049400     .                                                            
049500                                                                  
049600 2000-EXIT.                                                       
049700     EXIT.                                                        
049800                                                                  
049900***************************************************************** 
050000*  MOVE AND WRITE REPORT HEADER                                   
050100***************************************************************** 
050200                                                                  
050300 2100-WRITE-REPORT-HEADER.                                        
050400     MOVE WS-PAGE-CTR TO WS-RH1-PG-NBR                            
050500     MOVE WS-REPORT-HEADER-1 TO OUT-RECORD                        
050600     PERFORM 8500-WRITE-REPORT-RECORD THRU 8500-EXIT              
050700     MOVE WS-REPORT-HEADER-2 TO OUT-RECORD                        
050800     PERFORM 8500-WRITE-REPORT-RECORD THRU 8500-EXIT              
050900                                                                  
051000     .                                                            
051100 2100-EXIT.                                                       
051200     EXIT.                                                        
051300                                                                  
051400***************************************************************** 
051500*  MOVE AND WRITE PAGE HEADER                                     
051600***************************************************************** 
051700                                                                  
051800 2200-WRITE-PAGE-HEADER.                                          
051900     MOVE WS-PAGE-HEADER TO OUT-RECORD                            
052000     PERFORM 8500-WRITE-REPORT-RECORD THRU 8500-EXIT              
052100                                                                  
052200     .                                                            
052300 2200-EXIT.                                                       
052400     EXIT.                                                        
052500                                                                  
052600***************************************************************** 
052700*  MOVE AND WRITE CONTROL HEADER                                  
052800***************************************************************** 
052900                                                                  
053000 2300-WRITE-CONTROL-HEADER.                                       
053100     MOVE WS-CONTROL-HEADER-1 TO OUT-RECORD                       
053200     PERFORM 8500-WRITE-REPORT-RECORD THRU 8500-EXIT              
053300     MOVE WS-CONTROL-HEADER-2 TO OUT-RECORD                       
053400     PERFORM 8500-WRITE-REPORT-RECORD THRU 8500-EXIT              
053500     MOVE SPACES              TO OUT-RECORD                       
053600     PERFORM 8500-WRITE-REPORT-RECORD THRU 8500-EXIT              
053700                                                                  
053800     .                                                            
053900 2300-EXIT.                                                       
054000     EXIT.                                                        
054100                                                                  
054200***************************************************************** 
054300*  MOVE AND WRITE DETAIL LINE                                     
054400***************************************************************** 
054500                                                                  
054600 2400-WRITE-DETAIL-LINE.                                          
054700     PERFORM 2410-BUILD-DETAIL-LINE THRU 2410-EXIT                
054800     MOVE WS-WORKING-RECORD TO OUT-RECORD                         
054900     PERFORM 8500-WRITE-REPORT-RECORD THRU 8500-EXIT              
055000     PERFORM 2405-UPDATE-RUNNING-TOTALS THRU 2405-EXIT            
055100     ADD +1 TO WS-REC-OUT                                         
055200                                                                  
055300     .                                                            
055400 2400-EXIT.                                                       
055500     EXIT.                                                        
055600***************************************************************** 
055700*  UPDATE THE RUNNING TOTALS                                      
055800***************************************************************** 
055900                                                                  
056000 2405-UPDATE-RUNNING-TOTALS.                                      
056100     ADD WS-FED-TAX-AMT-C3   TO WS-RUN-ST-SUB-FEDTX,              
056200                                WS-RUN-GTOT-FEDTX                 
056300     ADD WS-STATE-TAX-AMT-C3 TO WS-RUN-ST-SUB-STATETX,            
056400                                WS-RUN-GTOT-STATETX               
056500     ADD SR-BONUS-AMT        TO WS-RUN-ST-SUB-GROSS,              
056600                                WS-RUN-GTOT-GROSS                 
056700     ADD WS-NET-BONUS-AMT-C3 TO WS-RUN-ST-SUB-NET,                
056800                             WS-RUN-GTOT-NET                      
056900                                                                  
057000     .                                                           
057100 2405-EXIT.                                                      
057200     EXIT.                                                       
057300*****************************************************************
057400*  BUILD THE DETAIL LINE                                         
057500*****************************************************************
057600                                                                 
057700 2410-BUILD-DETAIL-LINE.                                         
057800     MOVE SR-LAST-NAME TO WR-LAST-NAME                           
057900     MOVE SR-FIRST-NAME TO WR-FIRST-NAME                         
058000     MOVE SR-MID-INIT TO WR-MID-INIT                             
058100     MOVE SR-STATE-CODE TO WR-STATE-CODE                         
058200     MOVE SR-BONUS-AMT TO WR-BONUS-AMT                           
058300                                                                 
058400*************** EXEMPT FROM STATE AND FEDERAL                    
058500     IF FED-TAX-EXEMPT AND STATE-TAX-EXEMPT THEN                 
058600         INITIALIZE WR-FED-TAX-AMT                               
058700         INITIALIZE WS-FED-TAX-AMT-C3                            
058800         INITIALIZE WR-STATE-TAX-AMT                           
058900         INITIALIZE WS-STATE-TAX-AMT-C3                        
059000         MOVE 'FULLY EXEMPT EMPLOYEE' TO WR-MESSAGE            
059100     ELSE                                                      
059200*************** EXEMPT FROM STATE, SUBJECT TO FEDERAL          
059300         IF STATE-TAX-EXEMPT THEN                              
059400             INITIALIZE WR-STATE-TAX-AMT                       
059500             INITIALIZE WS-STATE-TAX-AMT-C3                    
059600             MOVE 'STATE EXEMPT EMPLOYEE' TO WR-MESSAGE        
059700             MULTIPLY SR-BONUS-AMT BY WS-C-FED-TAX-RATE        
059800                 GIVING WS-FED-TAX-AMT-C3 ROUNDED              
059900             MOVE WS-FED-TAX-AMT-C3 TO WR-FED-TAX-AMT          
060000         ELSE                                                  
060100             SEARCH ALL TAXES                                  
060200               AT END                                          
060300                 INITIALIZE WS-STATE-RATE-C3                   
060400                 DISPLAY 'STATE TAX INFO NOT FOUND FOR ',      
060500                                  SR-STATE-CODE                
060600                 DISPLAY SR-STATE-CODE, ' TAX RATE SET TO ZERO' 
060700               WHEN TT-STATE (TT-NDX) = SR-STATE-CODE           
060800                 MOVE TT-TAX (TT-NDX) TO WS-STATE-RATE-C3       
060900             END-SEARCH                                         
061000*************** SUBJECT TO STATE, EXEMPT FROM FEDERAL           
061100             IF FED-TAX-EXEMPT THEN                             
061200                 INITIALIZE WR-FED-TAX-AMT                      
061300                 INITIALIZE WS-FED-TAX-AMT-C3                   
061400                 MOVE 'FED EXEMPT EMPLOYEE' TO WR-MESSAGE       
061500                 MULTIPLY SR-BONUS-AMT BY WS-STATE-RATE-C3      
061600                     GIVING WS-STATE-TAX-AMT-C3 ROUNDED         
061700                 MOVE WS-STATE-TAX-AMT-C3 TO WR-STATE-TAX-AMT   
061800             ELSE                                               
061900*************** SUBJECT TO BOTH STATE AND FEDERAL               
062000                 MULTIPLY SR-BONUS-AMT BY WS-C-FED-TAX-RATE     
062100                     GIVING WS-FED-TAX-AMT-C3 ROUNDED           
062200                 MOVE WS-FED-TAX-AMT-C3 TO WR-FED-TAX-AMT       
062300                 MULTIPLY SR-BONUS-AMT BY WS-STATE-RATE-C3      
062400                     GIVING WS-STATE-TAX-AMT-C3 ROUNDED           
062500                 MOVE WS-STATE-TAX-AMT-C3 TO WR-STATE-TAX-AMT     
062600                 INITIALIZE WR-MESSAGE                            
062700             END-IF                                               
062800         END-IF                                                   
062900     END-IF                                                       
063000*************** APPLIES TO ALL CASES                              
063100     COMPUTE WS-NET-BONUS-AMT-C3 =                                
063200         (SR-BONUS-AMT - WS-FED-TAX-AMT-C3 - WS-STATE-TAX-AMT-C3) 
063300     MOVE WS-NET-BONUS-AMT-C3 TO WR-NET-BONUS-AMT                 
063400                                                                  
063500     .                                                            
063600 2410-EXIT.                                                       
063700     EXIT.                                                        
063800***************************************************************** 
063900*  PRINT STATE SUBTOTAL BREAK                                     
064000***************************************************************** 
064100                                                                  
064200 2500-PRINT-STATE-SUBTOTAL.                                      
064300     MOVE WS-RUN-ST-SUB-GROSS       TO WS-SSR-GROSS              
064400     MOVE WS-RUN-ST-SUB-FEDTX       TO WS-SSR-FEDTX              
064500     MOVE WS-RUN-ST-SUB-STATETX     TO WS-SSR-STATETX            
064600     MOVE WS-RUN-ST-SUB-NET         TO WS-SSR-NET                
064700     MOVE WS-CURRENT-STATE-GROUP    TO WS-SSR-STATE              
064800                                                                 
064900     MOVE WS-STATE-SUBTOTAL-REC     TO OUT-RECORD                
065000     PERFORM 8500-WRITE-REPORT-RECORD THRU 8500-EXIT             
065100                                                                 
065200     MOVE WS-C-DOUBLE-BLANK-LINE    TO OUT-RECORD                
065300     PERFORM 8500-WRITE-REPORT-RECORD THRU 8500-EXIT             
065400     ADD +3 TO WS-DETAIL-LINE-CTR                                
065500                                                                 
065600     INITIALIZE WS-RUN-ST-SUB-GROSS                              
065700     INITIALIZE WS-RUN-ST-SUB-FEDTX                              
065800     INITIALIZE WS-RUN-ST-SUB-STATETX                            
065900     INITIALIZE WS-RUN-ST-SUB-NET                                
066000     MOVE SR-STATE-CODE TO WS-CURRENT-STATE-GROUP                 
066100                                                                  
066200     .                                                            
066300 2500-EXIT.                                                       
066400     EXIT.                                                        
066500***************************************************************** 
066600*  PRINT GRAND TOTAL FOOTER                                       
066700***************************************************************** 
066800                                                                  
066900 2600-PRINT-GRAND-TOTAL.                                          
067000     MOVE WS-RUN-GTOT-GROSS       TO WS-GTR-GROSS                 
067100     MOVE WS-RUN-GTOT-FEDTX       TO WS-GTR-FEDTX                 
067200     MOVE WS-RUN-GTOT-STATETX     TO WS-GTR-STATETX               
067300     MOVE WS-RUN-GTOT-NET         TO WS-GTR-NET                   
067400                                                                  
067500     MOVE WS-GRAND-TOTAL-REC      TO OUT-RECORD                   
067600     PERFORM 8500-WRITE-REPORT-RECORD THRU 8500-EXIT              
067700                                                                  
067800     .                                                            
067900 2600-EXIT.                                                       
068000     EXIT.                                                        
068100***************************************************************** 
068200*  BUILD DATE & TIME STRINGS                                      
068300***************************************************************** 
068400                                                                  
068500 2700-BUILD-DATE-TIME.                                            
068600     CALL WS-DATE-PGM USING WS-DATE-LINE WS-TIME-LINE             
068700     MOVE WS-DATE-LINE TO WS-RH2-DATE                             
068800     MOVE WS-TIME-LINE TO WS-PH-TIME                              
068900                                                                  
069000     .                                                            
069100 2700-EXIT.                                                       
069200     EXIT.                                                        
069300                                                                  
069400***************************************************************** 
069500*  RESULT CHECKING ROUTINE                                        
069600***************************************************************** 
069700                                                                  
069800 3000-CHECK-RESULTS.                                              
069900                                                                  
070000     DISPLAY '*** LAB14...TOTALS ***'                             
070100     DISPLAY 'TOTAL RECORDS READ    : ' WS-REC-IN                 
070200     DISPLAY 'TOTAL RECORDS WRITTEN : ' WS-REC-OUT                
070300                                                                  
070400     IF WS-REC-IN > WS-REC-OUT THEN                               
070500         DISPLAY 'INPUT REC COUNT EXCEEDS OUTPUT REC COUNT'       
070600         MOVE 98 TO RETURN-CODE                                   
070700     ELSE                                                         
070800         IF WS-REC-IN < WS-REC-OUT THEN                           
070900             DISPLAY 'OUTPUT REC COUNT EXCEEDS INPUT REC COUNT'   
071000             MOVE 99 TO RETURN-CODE                               
071100         ELSE                                                     
071200             DISPLAY 'OUTPUT REC COUNT EQUALS INPUT REC COUNT'    
071300             MOVE 0 TO RETURN-CODE                                
071400         END-IF                                                   
071500     END-IF                                                       
071600                                                                  
071700     .                                                            
071800 3000-EXIT.                                                       
071900     EXIT.                                                        
072000                                                                  
072100                                                                  
072200***************************************************************** 
072300*  FINISH ROUTINE                                                 
072400***************************************************************** 
072500                                                                  
072600 4000-TERMINATE.                                                  
072700                                                                  
072800     CLOSE OUT-REPORT                                             
072900     IF WS-OUTFILE-STATUS-CODE = '00' THEN                        
073000         CONTINUE                                                 
073100     ELSE                                                         
073200         MOVE '4000-TERMINATE, ' TO WS-ABORT-MSG-PGRPH            
073300         MOVE 'CLOSING OUT-REPORT FILE, ' TO WS-ABORT-MSG-EDESC   
073400         MOVE WS-OUTFILE-STATUS-CODE TO WS-ABORT-MSG-SYSRC        
073500         MOVE 4056 TO RETURN-CODE                                 
073600         PERFORM 9999-ABORT THRU 9999-EXIT                        
073700     END-IF                                                       
073800                                                                  
073900     .                                                            
074000 4000-EXIT.                                                       
074100     EXIT.                                                        
074200                                                                  
074300                                                                  
074400***************************************************************** 
074500*  READ FILE ROUTINE                                              
074600***************************************************************** 
074700                                                                  
074800 8000-READ-BONUS.                                                 
074900     RETURN SORT-FILE                                             
075000         AT END                                                   
075100             SET WS-SORT-EOF TO TRUE                              
075200         NOT AT END                                               
075300             ADD +1 TO WS-REC-IN                                  
075400     END-RETURN                                                   
075500                                                                  
075600     .                                                            
075700                                                                  
075800 8000-EXIT.                                                       
075900     EXIT.                                                        
076000                                                                  
076100***************************************************************** 
076200*  LOAD TAX TABLE ROUTINE                                         
076300***************************************************************** 
076400                                                                  
076500 8100-LOAD-TAX-TABLE.                                             
076600     MOVE TAX-ENTRY TO TAXES (TT-NDX)                             
076700     SET TT-NDX UP BY 1                                           
076800     IF TT-NDX > WS-C-TAX-TABLE-SIZE THEN                         
076900         SET WS-TAX-TABLE-IS-FULL TO TRUE                         
077000     ELSE                                                         
077100         PERFORM 8150-READ-TAX-FILE THRU 8150-EXIT                
077200     END-IF                                                       
077300                                                                  
077400     .                                                            
077500                                                                  
077600 8100-EXIT.                                                       
077700     EXIT.                                                        
077800                                                                  
077900***************************************************************** 
078000*  READ TAX FILE ROUTINE                                          
078100***************************************************************** 
078200                                                                  
078300 8150-READ-TAX-FILE.                                              
078400     READ TAX-FILE                                                
078500         AT END                                                   
078600             SET WS-TAX-FILE-EOF TO TRUE                       
078700     END-READ                                                  
078800     IF WS-TAXFILE-STATUS-CODE = '00' OR                       
078900         WS-TAXFILE-STATUS-CODE = '10' THEN                    
079000         CONTINUE                                              
079100     ELSE                                                      
079200         MOVE '8150-READ-TAX-FILE, ' TO WS-ABORT-MSG-PGRPH     
079300         MOVE 'READING FROM TAX FILE, ' TO WS-ABORT-MSG-EDESC  
079400         MOVE WS-TAXFILE-STATUS-CODE TO WS-ABORT-MSG-SYSRC     
079500         MOVE 8188 TO RETURN-CODE                              
079600         PERFORM 9999-ABORT THRU 9999-EXIT                     
079700     END-IF                                                    
079800                                                               
079900     .                                                         
080000                                                               
080100 8150-EXIT.                                                    
080200     EXIT.                                                     
080300                                                               
080400***************************************************************** 
080500*  WRITE REPORT RECORD                                            
080600***************************************************************** 
080700                                                                  
080800 8500-WRITE-REPORT-RECORD.                                        
080900     WRITE OUT-RECORD                                             
081000     IF WS-OUTFILE-STATUS-CODE = '00' THEN                        
081100         CONTINUE                                                 
081200     ELSE                                                         
081300         MOVE '8500-WRITE-REPORT-RECORD, ' TO WS-ABORT-MSG-PGRPH  
081400         MOVE 'WRITING TO OUT-REPORT FILE, ' TO WS-ABORT-MSG-EDESC
081500         MOVE WS-OUTFILE-STATUS-CODE TO WS-ABORT-MSG-SYSRC        
081600         MOVE 9056 TO RETURN-CODE                                 
081700         PERFORM 9999-ABORT THRU 9999-EXIT                        
081800     END-IF                                                       
081900                                                                  
082000     .                                                            
082100                                                                  
082200 8500-EXIT.                                                       
082300     EXIT.                                                        
082400                                                                  
082500***************************************************************** 
082600*  MAJOR ERROR HANDLING ROUTINE                                   
082700***************************************************************** 
082800                                                                  
082900 9999-ABORT.                                                      
083000     MOVE RETURN-CODE TO WS-RETURN-CODE-DISPLAY                   
083100     DISPLAY 'PROGRAM: LAB14, ', WS-ABORT-MSG-STRING, ', RC='     
083200         WS-RETURN-CODE-DISPLAY                                   
083300     DIVIDE 1 BY WS-ABORT-TRIGGER GIVING WS-ABORT-CODE            
083400                                                                  
083500     .                                                            
083600 9999-EXIT.                                                       
083700     EXIT.                                                        
083800                                                                  