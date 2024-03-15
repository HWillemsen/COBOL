000100 IDENTIFICATION DIVISION.                                         
000200 PROGRAM-ID.    LAB10.                                            
000300 AUTHOR.        KJ THE AMAZING!                                   
000400 DATE-WRITTEN.  JUNE 5, 2021.                                     
000500 DATE-COMPILED.                                                   
000600******************************************************************
000700*                                                                 
000800*-------------P R O G R A M  D E S C R I P T I O N--------------* 
000900*                                                               * 
001000*    ----------------- LAB10 -----------------------            * 
001100*                                                               * 
001200*    PURPOSE  THIS PROGRAM WILL DETERMINE THE DATE AND          * 
001300*    =======  RETURN A 35-BYTE FIELD.                           * 
001400*                                                               * 
001500*    INPUT   (LINKAGE SECTION) LS-DATELINE                      * 
001600*    =====                                                      * 
001700*                                                               * 
001800*    PROCESS 1. GET THE NUMBER FOR THE DAY OF THE WEEK          * 
001900*    ======= 2. CONVERT THE NUMBER TO A WORD (E.G. MONDAY)      * 
002000*            3. GET THE DATE                                    * 
002100*            4. FORMAT THE 35-BYTE DATE FIELD                   * 
002200*            5. FORMAT THE 14-BYTE TIME FIELD                   * 
002300*                                                               * 
002400*    OUTPUT  NONE (RETURNS 35-BYTE DATE INFO VIA LINKAGE)       * 
002500*                 (RETURNS 14-BYTE TIME INFO VIA LINKAGE)       * 
002600*    ======                                                     * 
002700*                                                               * 
002800*    CALLING PROGRAM(S)  :   LAB11, AND OTHERS       -  OR  -   * 
002900*                        :   ANY PROGRAM USING CORRECT FORMAT   * 
003000*                                                               * 
003100*    CALLED  PROGRAM(S)  :   NONE                               * 
003200*                                                               * 
003300*---------------------------------------------------------------* 
003400*                 UPDATE LOG                                    * 
003500*---------------------------------------------------------------* 
003600*                                                               * 
003700* PERSON  PROJECT   DATE      DESCRIPTION          PROGRAM   VER* 
003800* ------  --------  --------  -------------------  -------   ---* 
003900* HENRY   TRAINING  11/06/99  INITIAL VERSION      LAB10A    000* 
004000* JOHN E  SHOES     01/15/21  COPIED/MODIFIED      LAB10     001* 
004100***************************************************************** 
004200*                                                               * 
004300******************************************************************
004400*         E N V I R O N M E N T     D I V I S I O N              *
004500******************************************************************
004600 ENVIRONMENT DIVISION.                                            
004700 INPUT-OUTPUT SECTION.                                            
004800 FILE-CONTROL.                                                    
004900*** NO FILES IN - NO FILES OUT                                   *
005000                                                                  
005100******************************************************************
005200*                  D A T A     D I V I S I O N                   *
005300******************************************************************
005400 DATA DIVISION.                                                   
005500 FILE SECTION.                                                    
005600                                                                  
005700******************************************************************
005800*    W O R K I N G - S T O R A G E                               *
005900******************************************************************
006000                                                                  
006100 WORKING-STORAGE SECTION.                                         
006200                                                                  
006300***********************                                           
006400*  WORK FIELDS        *                                           
006500***********************                                           
006600                                                                  
006700 01  WS-WORK-FIELDS.                                              
006800     05  WS-FILLER1              PIC X(37)                        
006900         VALUE 'LAB10 WORKING STORAGE BEGINS HERE'.               
007000     05  WS-TIME-HOUR-C3         PIC 9(02)  COMP-3 VALUE ZERO.   
007100     05  WS-DATE-TIME            PIC X(16).                      
007200     05  WS-TIME-HOUR            PIC Z9.                         
007300     05  WS-TIME                 PIC X(06)  VALUE ':MM AM'.      
007400     05  WS-TIMELINE             PIC X(14)                       
007500                                     VALUE 'TIME: HH:MM AM'.     
007600     05  WS-DATE-LITERAL         PIC X(06)  VALUE 'DATE: '.      
007700     05  WS-DAY-OF-WEEK-9        PIC 9(01)  VALUE ZERO.          
007800     05  WS-DAY-OF-WEEK-X        PIC X(10)  VALUE SPACES.        
007900     05  WS-MONTH-X              PIC X(10)  VALUE SPACES.        
008000     05  WS-DD-X.                                                
008100         10  WS-DD1              PIC X(01)  VALUE SPACE.         
008200         10  WS-DD2              PIC X(01)  VALUE SPACE.         
008300         10  WS-DD3              PIC X(01)  VALUE SPACE.         
008400     05  WS-YYYYMMDD.                                            
008500         10  WS-YYYY             PIC X(04)  VALUE SPACES.        
008600         10  WS-MM               PIC X(02)  VALUE SPACES.        
008700         10  WS-DD               PIC X(02)  VALUE SPACES.        
008800     05  WS-PLACE-MARK           PIC 9(03)  COMP-3 VALUE ZERO.    
008900     05  WS-TEST-BYTE            PIC X(01)  VALUE SPACES.         
009000         88  WS-TEST-BYTE-BLANK      VALUE SPACES.                
009100                                                                  
009200 01  WS-OUT-DATE-LAYOUT          PIC X(35)  VALUE SPACES.         
009300                                                                  
009400******************************************************************
009500*    L I N K A G E                                               *
009600******************************************************************
009700                                                                  
009800 LINKAGE SECTION.                                                 
009900                                                                  
010000 01  LS-DATELINE                 PIC X(35) JUSTIFIED RIGHT.       
010100 01  LS-TIMELINE                 PIC X(14).                       
010200                                                                  
010300******************************************************************
010400*          P R O C E D U R E     D I V I S I O N                 *
010500******************************************************************
010600                                                                  
010700 PROCEDURE DIVISION USING                                         
010800                    LS-DATELINE LS-TIMELINE.                      
010900                                                                  
011000 0000-MAINLINE.                                                   
011100                                                                  
011200     PERFORM 1000-INITIALIZATION    THRU 1000-EXIT                
011300     PERFORM 2000-FORMAT-DATELINE   THRU 2000-EXIT                
011400     PERFORM 2500-BUILD-TIME-STRING THRU 2500-EXIT                
011500     PERFORM 3000-TERMINATE         THRU 3000-EXIT                
011600                                                                  
011700     GOBACK                                                       
011800                                                                  
011900     .                                                            
012000 0000-EXIT.                                                       
012100     EXIT.                                                        
012200                                                                  
012300******************************************************************
012400*  INITIALIZATION ROUTINE                                        *
012500******************************************************************
012600                                                                  
012700 1000-INITIALIZATION.                                             
012800                                                                  
012900     MOVE FUNCTION CURRENT-DATE (1:16) TO WS-DATE-TIME            
013000     MOVE WS-DATE-TIME (1:8)           TO WS-YYYYMMDD             
013100     MOVE WS-DATE-TIME (9:2)           TO WS-TIME-HOUR-C3         
013200     ACCEPT WS-DAY-OF-WEEK-9 FROM DAY-OF-WEEK                     
013300     .                                                            
013400 1000-EXIT.                                                       
013500     EXIT.                                                        
013600                                                                  
013700******************************************************************
013800*  FORMAT-DATELINE ROUTINE                                       *
013900******************************************************************
014000                                                                  
014100 2000-FORMAT-DATELINE.                                            
014200                                                                 
014300     EVALUATE WS-DAY-OF-WEEK-9                                   
014400         WHEN   1    MOVE 'MONDAYX'    TO WS-DAY-OF-WEEK-X       
014500         WHEN   2    MOVE 'TUESDAYX'   TO WS-DAY-OF-WEEK-X       
014600         WHEN   3    MOVE 'WEDNESDAYX' TO WS-DAY-OF-WEEK-X       
014700         WHEN   4    MOVE 'THURSDAYX'  TO WS-DAY-OF-WEEK-X       
014800         WHEN   5    MOVE 'FRIDAYX'    TO WS-DAY-OF-WEEK-X       
014900         WHEN   6    MOVE 'SATURDAYX'  TO WS-DAY-OF-WEEK-X       
015000         WHEN   7    MOVE 'SUNDAYX'    TO WS-DAY-OF-WEEK-X       
015100         WHEN OTHER  MOVE 'INVALIDX'   TO WS-DAY-OF-WEEK-X       
015200     END-EVALUATE                                                
015300                                                                 
015400     EVALUATE WS-MM                                              
015500         WHEN  01    MOVE 'JANUARYX'   TO WS-MONTH-X             
015600         WHEN  02    MOVE 'FEBRUARYX'  TO WS-MONTH-X             
015700         WHEN  03    MOVE 'MARCHX'     TO WS-MONTH-X             
015800         WHEN  04    MOVE 'APRILX'     TO WS-MONTH-X             
015900         WHEN  05    MOVE 'MAYX'       TO WS-MONTH-X             
016000         WHEN  06    MOVE 'JUNEX'      TO WS-MONTH-X        
016100         WHEN  07    MOVE 'JULYX'      TO WS-MONTH-X        
016200         WHEN  08    MOVE 'AUGUSTX'    TO WS-MONTH-X        
016300         WHEN  09    MOVE 'SEPTEMBERX' TO WS-MONTH-X        
016400         WHEN  10    MOVE 'OCTOBERX'   TO WS-MONTH-X        
016500         WHEN  11    MOVE 'NOVEMBERX'  TO WS-MONTH-X        
016600         WHEN  12    MOVE 'DECEMBERX'  TO WS-MONTH-X        
016700         WHEN OTHER  MOVE 'INVALIDX'   TO WS-MONTH-X        
016800     END-EVALUATE                                           
016900                                                            
017000     IF WS-DD < '10'                                        
017100         MOVE  WS-DD (2:1) TO WS-DD1                        
017200         MOVE  'X'         TO WS-DD2                        
017300     ELSE                                                   
017400         MOVE WS-DD        TO WS-DD-X                       
017500         MOVE 'X'          TO WS-DD3                        
017600     END-IF                                                 
017700                                                            
017800     STRING WS-DATE-LITERAL                                
017900            WS-DAY-OF-WEEK-X ', '                          
018000            WS-MONTH-X       ' '                           
018100            WS-DD-X          ', '                          
018200            WS-YYYY                                        
018300         DELIMITED BY 'X'                                  
018400            INTO WS-OUT-DATE-LAYOUT                        
018500                                                           
018600     IF WS-OUT-DATE-LAYOUT (35:1) = SPACES                 
018700         MOVE +35 TO WS-PLACE-MARK                         
018800                                                           
018900         PERFORM UNTIL NOT WS-TEST-BYTE-BLANK              
019000             SUBTRACT 1 FROM WS-PLACE-MARK                 
019100             MOVE WS-OUT-DATE-LAYOUT (WS-PLACE-MARK:1)     
019200                 TO WS-TEST-BYTE                           
019300         END-PERFORM                                       
019400                                                           
019500     END-IF                                                
019600                                                                  
019700     MOVE WS-OUT-DATE-LAYOUT (1:WS-PLACE-MARK) TO LS-DATELINE     
019800                                                                  
019900     .                                                            
020000 2000-EXIT.                                                       
020100     EXIT.                                                        
020200                                                                  
020300******************************************************************
020400*  BUILD TIME STRING                                             *
020500******************************************************************
020600                                                                  
020700 2500-BUILD-TIME-STRING.                                          
020800                                                                  
020900     EVALUATE WS-TIME-HOUR-C3                                     
021000         WHEN 0                                                   
021100             MOVE  12  TO WS-TIME-HOUR                            
021200***          MOVE '12' TO WS-TIMELINE (7:2)                       
021300         WHEN 1 THRU 11                                           
021400             MOVE WS-TIME-HOUR-C3 TO WS-TIME-HOUR                
021500         WHEN 12                                                 
021600             MOVE WS-TIME-HOUR-C3 TO WS-TIME-HOUR                
021700             MOVE 'PM'            TO WS-TIMELINE (13:2)          
021800         WHEN 13 THRU 23                                         
021900             SUBTRACT 12        FROM WS-TIME-HOUR-C3             
022000             MOVE WS-TIME-HOUR-C3 TO WS-TIME-HOUR                
022100             MOVE 'PM'            TO WS-TIMELINE (13:2)          
022200         WHEN OTHER                                              
022300             DISPLAY 'PROBLEM BUILDING TIME STRING'              
022400     END-EVALUATE                                                
022500                                                                 
022600     MOVE WS-DATE-TIME (11:2)     TO WS-TIMELINE (10:2)          
022700     MOVE WS-TIME-HOUR            TO WS-TIMELINE (7:2)           
022800     MOVE WS-TIMELINE             TO LS-TIMELINE                 
022900                                                                 
023000     .                                                           
023100 2500-EXIT.                                                      
023200     EXIT.                                                        
023300******************************************************************
023400*  FINISH ROUTINE                                                *
023500******************************************************************
023600                                                                  
023700 3000-TERMINATE.                                                  
023800     MOVE +0 TO RETURN-CODE                                       
023900     .                                                            
024000 3000-EXIT.                                                       
024100     EXIT.                                                        
024200******************************************************************
024300*  PHYSICAL END OF PROGRAM                                       *
024400******************************************************************