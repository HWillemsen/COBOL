000100 IDENTIFICATION DIVISION.                                       
000200 PROGRAM-ID.    LAB9.                                           
000300 AUTHOR.        LEIGH BRITAIN.                                  
000400 INSTALLATION.  GREAT BRITAIN                                   
000500 DATE-WRITTEN.  JULY 2023.                                      
000600 DATE-COMPILED.                                                 
000700****************************************************************
000800*                      PROGRAM LAB9                            *
000900****************************************************************
001000*   THIS PROGRAM CREATES THE FIRST MONTHLY BONUS REPORT        *
001100*   WITH THE ADDITION OF FEDERAL TAX, NET, AND MESSAGE COLUMNS *
001200****************************************************************
001300                                                                
001400****************************************************************
001500*          E N V I R O N M E N T   D I V I S I O N             *
001600****************************************************************
001700 ENVIRONMENT DIVISION.                                          
001800 INPUT-OUTPUT SECTION.                                          
001900 FILE-CONTROL.                                                  
002000     SELECT IN-BONUS    ASSIGN TO INBONUS.                      
002100     SELECT OUT-REPORT  ASSIGN TO OUTREPT.                      
002200                                                                
002300****************************************************************
002400*          D A T A   D I V I S I O N                           *
002500****************************************************************
002600 DATA DIVISION.                                                 
002700 FILE SECTION.                                                  
002800                                                                
002900****************************************************************
003000*    IN-BONUS                                    INPUT         *
003100****************************************************************
003200                                                                
003300 FD  IN-BONUS                                                   
003400     RECORDING F                                                
003500     LABEL RECORDS STANDARD                                     
003600     RECORD CONTAINS 76 CHARACTERS                              
003700     BLOCK CONTAINS 0 RECORDS                                   
003800     DATA RECORD IS IN-BONUS-RECORD.                            
003900                                                                
004000 01  IN-BONUS-RECORD.                                           
004100     05 IN-RECORD PIC X(76).                                    
004200                                                                
004300****************************************************************
004400*    OUT-REPORT                                  OUTPUT        *
004500****************************************************************
004600                                                                
004700 FD  OUT-REPORT                                                 
004800     RECORDING F                                                
004900     LABEL RECORDS STANDARD                                     
005000     RECORD CONTAINS 133 CHARACTERS                             
005100     BLOCK CONTAINS 0 RECORDS                                   
005200     DATA RECORD IS OUT-REPORT-RECORD.                          
005300                                                                
005400 01  OUT-REPORT-RECORD.                                         
005500     05 OUT-RECORD              PIC X(133).                     
005600                                                                
005700****************************************************************
005800*          W O R K I N G - S T O R A G E                       *
005900****************************************************************
006000 WORKING-STORAGE SECTION.                                       
006100 01  FILLER                          PIC X(37) VALUE            
006200     ' BEGIN WORKING STORAGE FOR LAB9'.                         
006300                                                                
006400****************************************************************
006500*  ACCUMULATORS                                                *
006600****************************************************************
006700 01  ACCUMULATORS.                                              
006800     05  A-BONUS-IN                  PIC S9(04)  COMP VALUE +0. 
006900     05  A-REC-OUT                   PIC S9(04)  COMP VALUE +0. 
007000     05  A-LINE-CTR                  PIC S9(04)  COMP VALUE +0. 
007100     05  A-PAGE-CTR                  PIC S9(04)  COMP VALUE +0. 
007200                                                                
007300****************************************************************
007400*  INPUT FILE LAYOUT                                           *
007500****************************************************************
007600 01  W-IN-BONUS-REC.                                            
007700     05  W-IN-STATE                  PIC X(02).                 
007800     05  W-IN-LAST-NAME              PIC X(20).                 
007900     05  W-IN-FIRST-NAME             PIC X(15).                 
008000     05  W-IN-MIDDLE-INIT            PIC X(01).                 
008100     05  W-IN-GROSS-AMT              PIC 9(07)V99 COMP-3.       
008200     05  W-IN-FED-EXEMPT-IND         PIC X(01).                 
008300         88 TAX-EXEMPT               VALUE 'Y'.                 
008400     05  W-IN-ST-EXEMPT-IND          PIC X(01).                 
008500     05  FILLER                      PIC X(31).                 
008600                                                                
008700****************************************************************
008800*  PRINT LINES                                                 *
008900****************************************************************
009000                                                                
009100 01  PRINT-LINES.                                               
009200                                                                 
009300     05  P-OUT-REC                   PIC X(133).                 
009400                                                                 
009500     05  P-HEADER-1.                                             
009600         10  P-HDR1-CC               PIC X(01) VALUE '1'.        
009700         10  FILLER                  PIC X(11) VALUE             
009800                                         'REPORT ID:'.           
009900         10  P-HDR1-REPORT-ID        PIC X(08) VALUE SPACE.      
010000         10  FILLER                  PIC X(16) VALUE SPACE.      
010100         10  FILLER                  PIC X(23) VALUE             
010200                                       'COBOL CASE STUDY'.       
010300         10  FILLER                  PIC X(10) VALUE SPACE.      
010400         10  FILLER                  PIC X(29) VALUE             
010500                                         'MONTHLY BONUS REPORT '.
010600         10  FILLER                  PIC X(06) VALUE 'PAGE: '.   
010700         10  P-HDR1-PAGE             PIC Z9    VALUE ZERO.       
010800         10  FILLER                  PIC X(27) VALUE SPACES .    
010900                                                                 
011000     05  P-HEADER-11.                                            
011100         10  P-HDR11-CC              PIC X(01)  VALUE ' '.       
011200         10  FILLER                  PIC X(89) VALUE SPACE.      
011300         10  FILLER                  PIC X(06)  VALUE 'DATE: '.  
011400         10  P-HDR11-MONTH           PIC 9(02)  VALUE ZERO.      
011500         10  FILLER                  PIC X(01)  VALUE '/'.       
011600         10  P-HDR11-DAY             PIC 9(02)  VALUE ZERO.      
011700         10  FILLER                  PIC X(01)  VALUE '/'.       
011800         10  P-HDR11-YEAR            PIC 9(04)  VALUE ZERO.      
011900         10  FILLER                  PIC X(33) VALUE SPACE.      
012000                                                                 
012100     05  P-HEADER-12.                                            
012200         10  P-HDR12-CC              PIC X(01)  VALUE ' '.      
012300         10  FILLER                  PIC X(89) VALUE SPACE.     
012400         10  FILLER                  PIC X(06)  VALUE 'TIME: '. 
012500         10  P-HDR12-HOUR            PIC 9(02)  VALUE ZERO.     
012600         10  FILLER                  PIC X(01)  VALUE ':'.      
012700         10  P-HDR12-MINUTE          PIC 9(02)  VALUE ZERO.     
012800         10  FILLER                  PIC X(01)  VALUE ':'.      
012900         10  P-HDR12-SECOND          PIC 9(04)  VALUE ZERO.     
013000         10  FILLER                  PIC X(33) VALUE SPACE.     
013100                                                                
013200     05  P-HEADER-3.                                            
013300         10  P-HDR3-CC               PIC X(01) VALUE '-'.       
013400         10  FILLER                  PIC X(41) VALUE 'NAME'.    
013500         10  FILLER                  PIC X(09) VALUE 'STATE'.   
013600         10  FILLER                  PIC X(13) VALUE 'GROSS'.   
013700         10  FILLER                  PIC X(13) VALUE 'FED TAX'. 
013800         10  FILLER                  PIC X(10) VALUE 'NET'.     
013900         10  FILLER                  PIC X(18) VALUE 'MESSAGE'. 
014000         10  FILLER                  PIC X(24) VALUE SPACE.     
014100                                                                
014200     05  P-HEADER-4.                                            
014300         10  P-HDR4-CC               PIC X(01) VALUE '+'.       
014400         10  FILLER                  PIC X(105) VALUE ALL '_'.  
014500         10  FILLER                  PIC X(27) VALUE SPACE.     
014600                                                                
014700     05  P-HEADER-5.                                            
014800         10  P-HDR5-CC               PIC X(01) VALUE ' '.       
014900         10  FILLER                  PIC X(132) VALUE SPACES.   
015000                                                                
015100     05  P-HEADER-6.                                            
015200         10  P-HDR6-CC               PIC X(01) VALUE ' '.       
015300         10  FILLER                  PIC X(132)                 
015400                            VALUE '***   END OF REPORT   ***'.
015500     05  P-DETAIL-1.                                         
015600         10  P-DTL1-CC               PIC X(01) VALUE ' '.    
015700         10  P-DTL1-LAST-NAME        PIC X(20) VALUE SPACE.  
015800         10  FILLER                  PIC X(01) VALUE SPACE.  
015900         10  P-DTL1-FIRST-NAME       PIC X(15).              
016000         10  FILLER                  PIC X(01) VALUE SPACE.  
016100         10  P-DTL1-MIDDLE-INIT      PIC X(01).              
016200         10  FILLER                  PIC X(03) VALUE SPACE.  
016300         10  P-DTL1-STATE            PIC X(02).              
016400         10  FILLER                  PIC X(03) VALUE SPACE.  
016500         10  P-DTL1-GROSS            PIC Z,ZZZ,ZZ9.99.       
016600         10  FILLER                  PIC X(01) VALUE SPACE.  
016700         10  P-DTL1-FED-TAX          PIC Z,ZZZ,ZZ9.99.       
016800         10  FILLER                  PIC X(01) VALUE SPACE.  
016900         10  P-DTL1-NET              PIC Z,ZZZ,ZZ9.99.       
017000         10  FILLER                  PIC X(02) VALUE SPACE.  
017100         10  P-DTL1-MESSAGE          PIC X(19) VALUE SPACE.     
017200         10  FILLER                  PIC X(30) VALUE SPACE.     
017300                                                                
017400 01 VARIABLES.                                                  
017500     05  P-DTL1-FED-TAX-N        PIC 9(7)V99.                   
017600     05  P-DTL1-NET-N            PIC 9(7)V99.                   
017700                                                                
017800****************************************************************
017900*  SWITCHES                                                    *
018000****************************************************************
018100                                                                
018200 01  SWITCHES.                                                  
018300     05  SW-BONUS-EOF                PIC X(01)  VALUE 'N'.      
018400         88  BONUS-EOF                          VALUE 'Y'.      
018500                                                                
018600****************************************************************
018700*          P R O C E D U R E   D I V I S I O N                 *
018800****************************************************************  
018900 PROCEDURE DIVISION.                                              
019000                                                                  
019100****************************************************************  
019200*  MAINLINE.                                                   *  
019300****************************************************************  
019400 P0100-MAINLINE.                                                  
019500                                                                  
019600     PERFORM P0200-INITIALIZATION THRU P0299-EXIT.                
019700                                                                  
019800     PERFORM P0300-PROCESS-BONUS  THRU P0399-EXIT                 
019900         UNTIL BONUS-EOF.                                         
020000                                                                  
020100     PERFORM P0400-FINALIZATION   THRU P0499-EXIT.                
020200                                                                  
020300     MOVE +0 TO RETURN-CODE.                                      
020400                                                                  
020500     GOBACK.                                                      
020600                                                                  
020700 P0199-EXIT.                                                      
020800     EXIT.                                                        
020900                                                                  
021000****************************************************************  
021100*  LOGICAL END OF PROGRAM                                      *  
021200****************************************************************  
021300                                                                  
021400****************************************************************  
021500*  INITIALIZE.                                                 *  
021600****************************************************************  
021700 P0200-INITIALIZATION.                                            
021800                                                                  
021900     OPEN INPUT  IN-BONUS                                         
022000          OUTPUT OUT-REPORT                                       
022100                                                                  
022200     MOVE 'LAB9' TO P-HDR1-REPORT-ID.                           
022300     MOVE FUNCTION CURRENT-DATE (5:2)   TO P-HDR11-MONTH        
022400     MOVE FUNCTION CURRENT-DATE (7:2)   TO P-HDR11-DAY          
022500     MOVE FUNCTION CURRENT-DATE (1:4)   TO P-HDR11-YEAR         
022600     MOVE FUNCTION CURRENT-DATE (9:2)   TO P-HDR12-HOUR         
022700     MOVE FUNCTION CURRENT-DATE (11:2)  TO P-HDR12-MINUTE       
022800     MOVE FUNCTION CURRENT-DATE (13:4)  TO P-HDR12-SECOND       
022900                                                                
023000     PERFORM P0700-READ-BONUS THRU P0799-EXIT.                  
023100     PERFORM P0500-HEADERS THRU P0599-EXIT.                     
023200                                                                
023300 P0299-EXIT.                                                    
023400     EXIT.                                                      
023500                                                                
023600****************************************************************
023700*  PROCESS-BONUS.                                              *
023800****************************************************************
023900 P0300-PROCESS-BONUS.                                    
024000                                                         
024100     IF A-LINE-CTR > 15 THEN                             
024200         MOVE 0 TO A-LINE-CTR                            
024300         PERFORM P0500-HEADERS THRU P0599-EXIT           
024400     END-IF.                                             
024500                                                         
024600     MOVE W-IN-LAST-NAME   TO P-DTL1-LAST-NAME           
024700     MOVE W-IN-FIRST-NAME  TO P-DTL1-FIRST-NAME          
024800     MOVE W-IN-MIDDLE-INIT TO P-DTL1-MIDDLE-INIT         
024900     MOVE W-IN-STATE       TO P-DTL1-STATE               
025000     MOVE W-IN-GROSS-AMT   TO P-DTL1-GROSS               
025100                                                         
025200     IF TAX-EXEMPT                                       
025300         MOVE ZEROES                 TO P-DTL1-FED-TAX   
025400         MOVE P-DTL1-GROSS           TO P-DTL1-NET       
025500         MOVE 'TAX EXEMPT EMPLOYEE'  TO P-DTL1-MESSAGE   
025600     ELSE                                                      
025700         MULTIPLY W-IN-GROSS-AMT BY 0.28                       
025800                                     GIVING P-DTL1-FED-TAX-N   
025900         SUBTRACT P-DTL1-FED-TAX-N   FROM W-IN-GROSS-AMT       
026000                                     GIVING P-DTL1-NET-N       
026100         MOVE P-DTL1-FED-TAX-N       TO P-DTL1-FED-TAX         
026200         MOVE P-DTL1-NET-N           TO P-DTL1-NET             
026300         MOVE SPACES                 TO P-DTL1-MESSAGE         
026400     END-IF                                                    
026500                                                               
026600     MOVE P-DETAIL-1       TO P-OUT-REC                        
026700     PERFORM P0600-WRITE-REPORT THRU P0699-EXIT.               
026800     ADD +1                TO A-REC-OUT                        
026900                                                               
027000* BLANK OUT THE DETAIL LINE                                    
027100     MOVE SPACES         TO P-DETAIL-1.                        
027200                                                               
027300     PERFORM P0700-READ-BONUS   THRU P0799-EXIT.                 
027400                                                                 
027500 P0399-EXIT.                                                     
027600     EXIT.                                                       
027700                                                                 
027800**************************************************************** 
027900*  FINALIZATION.                                               * 
028000**************************************************************** 
028100 P0400-FINALIZATION.                                             
028200                                                                 
028300     IF A-LINE-CTR > 15                                          
028400         PERFORM P0500-HEADERS THRU P0599-EXIT                   
028500     END-IF                                                      
028600     PERFORM P0800-WRITE-BLANK-LINES THRU P0899-EXIT             
028700             UNTIL A-LINE-CTR = 15.                              
028800     MOVE P-HEADER-6     TO P-OUT-REC                            
028900     PERFORM P0600-WRITE-REPORT THRU P0699-EXIT.                 
029000                                                                 
029100     CLOSE IN-BONUS                                              
029200           OUT-REPORT                                            
029300                                                                 
029400     DISPLAY '*** LAB9 TOTALS ***'.                              
029500     DISPLAY 'TOTAL RECORDS READ    : ' A-BONUS-IN.              
029600     DISPLAY 'TOTAL RECORDS WRITTEN : ' A-REC-OUT.               
029700                                                                 
029800                                                                 
029900 P0499-EXIT.                                                     
030000     EXIT.                                                       
030100                                                                 
030200 EJECT                                                           
030300**************************************************************** 
030400*  HEADERS.                                                    * 
030500**************************************************************** 
030600 P0500-HEADERS.                                                  
030700                                                          
030800     ADD +1 TO A-PAGE-CTR.                                
030900     MOVE A-PAGE-CTR TO P-HDR1-PAGE.                      
031000                                                          
031100     MOVE P-HEADER-1     TO P-OUT-REC.                    
031200     PERFORM P0600-WRITE-REPORT THRU P0699-EXIT.          
031300                                                          
031400     MOVE P-HEADER-11    TO P-OUT-REC.                    
031500     PERFORM P0600-WRITE-REPORT THRU P0699-EXIT.          
031600                                                          
031700     MOVE P-HEADER-12    TO P-OUT-REC.                    
031800     PERFORM P0600-WRITE-REPORT THRU P0699-EXIT.          
031900                                                          
032000     MOVE P-HEADER-3     TO P-OUT-REC.                    
032100     PERFORM P0600-WRITE-REPORT THRU P0699-EXIT.          
032200                                                          
032300     MOVE P-HEADER-4     TO P-OUT-REC.                    
032400     PERFORM P0600-WRITE-REPORT THRU P0699-EXIT.                 
032500                                                                 
032600     MOVE '0'            TO P-DTL1-CC.                           
032700                                                                 
032800     MOVE 8              TO A-LINE-CTR.                          
032900                                                                 
033000 P0599-EXIT.                                                     
033100     EXIT.                                                       
033200                                                                 
033300**************************************************************** 
033400*  WRITE-REPORT.                                               * 
033500**************************************************************** 
033600 P0600-WRITE-REPORT.                                             
033700                                                                 
033800     WRITE OUT-REPORT-RECORD FROM P-OUT-REC.                     
033900                                                                 
034000     ADD +1 TO A-LINE-CTR.                                       
034100                                                                
034200 P0699-EXIT.                                                    
034300     EXIT.                                                      
034400                                                                
034500****************************************************************
034600*  READ-BONUS.                                                 *
034700****************************************************************
034800 P0700-READ-BONUS.                                              
034900                                                                
035000     READ IN-BONUS INTO W-IN-BONUS-REC                          
035100         AT END                                                 
035200             MOVE 'Y' TO SW-BONUS-EOF                           
035300         NOT AT END                                             
035400             ADD +1 TO A-BONUS-IN                               
035500     END-READ.                                                  
035600                                                                
035700 P0799-EXIT.                                                    
035800     EXIT.                                                       
035900                                                                 
036000 P0800-WRITE-BLANK-LINES.                                        
036100                                                                 
036200     MOVE P-HEADER-5     TO P-OUT-REC.                           
036300     PERFORM P0600-WRITE-REPORT THRU P0699-EXIT                  
036400     .                                                           
036500 P0899-EXIT.                                                     
036600     EXIT.                                                       
036700                                                                 
036800**************************************************************** 
036900*  PHYSICAL END OF PROGRAM                                     * 
037000**************************************************************** 
037100                                                                 