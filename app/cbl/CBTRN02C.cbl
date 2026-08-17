      ******************************************************************
      * Program     : CBTRN02C.CBL                                      
      * Application : CardDemo                                          
      * Type        : BATCH COBOL Program                                
      * Function    : Post the records from daily transaction file.     
      ******************************************************************
      * Copyright Amazon.com, Inc. or its affiliates.                   
      * All Rights Reserved.                                            
      *                                                                 
      * Licensed under the Apache License, Version 2.0 (the "License"). 
      * You may not use this file except in compliance with the License.
      * You may obtain a copy of the License at                         
      *                                                                 
      *    http://www.apache.org/licenses/LICENSE-2.0                   
      *                                                                 
      * Unless required by applicable law or agreed to in writing,      
      * software distributed under the License is distributed on an     
      * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    
      * either express or implied. See the License for the specific     
      * language governing permissions and limitations under the License
      ******************************************************************
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    CBTRN02C.                                                 
       AUTHOR.        AWS.                                                      
                                                                                
       ENVIRONMENT DIVISION.                                                    
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
           SELECT DALYTRAN-FILE ASSIGN TO DALYTRAN                              
                  ORGANIZATION IS SEQUENTIAL                                    
                  ACCESS MODE  IS SEQUENTIAL                                    
                  FILE STATUS  IS DALYTRAN-STATUS.                              
                                                                                
           SELECT TRANSACT-FILE ASSIGN TO TRANFILE                              
                  ORGANIZATION IS INDEXED                                       
                  ACCESS MODE  IS RANDOM                                        
                  RECORD KEY   IS FD-TRANS-ID                                   
                  FILE STATUS  IS TRANFILE-STATUS.                              
                                                                                
           SELECT XREF-FILE ASSIGN TO   XREFFILE                                
                  ORGANIZATION IS INDEXED                                       
                  ACCESS MODE  IS RANDOM                                        
                  RECORD KEY   IS FD-XREF-CARD-NUM                              
                  FILE STATUS  IS XREFFILE-STATUS.                              
                                                                                
           SELECT DALYREJS-FILE ASSIGN TO DALYREJS                              
                  ORGANIZATION IS SEQUENTIAL                                    
                  ACCESS MODE  IS SEQUENTIAL                                    
                  FILE STATUS  IS DALYREJS-STATUS.                              
                                                                                
           SELECT ACCOUNT-FILE ASSIGN TO ACCTFILE                               
                  ORGANIZATION IS INDEXED                                       
                  ACCESS MODE  IS RANDOM                                        
                  RECORD KEY   IS FD-ACCT-ID                                    
                  FILE STATUS  IS ACCTFILE-STATUS.                              
                                                                                
           SELECT TCATBAL-FILE ASSIGN TO TRANCATB                               
                  ORGANIZATION IS INDEXED                                       
                  ACCESS MODE  IS RANDOM                                        
                  RECORD KEY   IS FD-TRAN-CAT-KEY                               
                  FILE STATUS  IS TCATBALF-STATUS.                              
                                                                                
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
       FD  DALYTRAN-FILE.                                                       
       01  FD-TRAN-RECORD.                                                      
           05 FD-DALYTRAN-DATA                      PIC X(350).                 
                                                                                
       FD  TRANSACT-FILE.                                                       
       01  FD-TRANFILE-REC.                                                     
           05  FD-TRANS-ID                           PIC X(16).                 
           05  FD-TRAN-DATA                          PIC X(334).                
                                                                                
       FD  XREF-FILE.                                                           
       01  FD-XREF-RECORD.                                                      
           05  FD-XREF-CARD-NUM                      PIC X(16).                 
           05  FD-XREF-DATA                          PIC X(26).                 
                                                                                
       FD  DALYREJS-FILE.                                                       
       01  DALYREJS-RECORD.                                                     
           05  REJECT-TRAN-DATA                      PIC X(350).                
           05  REJECT-REASON                         PIC X(100).                
                                                                                
       FD  ACCOUNT-FILE.                                                        
       01  FD-ACCTFILE-REC.                                                     
           05  FD-ACCT-ID                            PIC 9(11).                 
           05  FD-ACCT-DATA                          PIC X(274).                
                                                                                
       FD  TCATBAL-FILE.                                                        
       01  FD-TRAN-CAT-BAL-RECORD.                                              
           05 FD-TRAN-CAT-KEY.                                                  
              10 FD-TRANCAT-ACCT-ID             PIC 9(11).                      
              10 FD-TRANCAT-TYPE-CD             PIC X(02).                      
              10 FD-TRANCAT-CD                  PIC 9(04).                      
           05 FD-FD-TRAN-CAT-DATA               PIC X(33).                      
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
      *****************************************************************         
       COPY CVTRA06Y.                                                           
       01  DALYTRAN-STATUS.                                                     
           05  DALYTRAN-STAT1      PIC X.                                       
           05  DALYTRAN-STAT2      PIC X.                                       
                                                                                
       COPY CVTRA05Y.                                                           
       01  TRANFILE-STATUS.                                                     
           05  TRANFILE-STAT1      PIC X.                                       
           05  TRANFILE-STAT2      PIC X.                                       
                                                                                
       COPY CVACT03Y.                                                           
       01  XREFFILE-STATUS.                                                     
           05  XREFFILE-STAT1      PIC X.                                       
           05  XREFFILE-STAT2      PIC X.                                       
                                                                                
       01  DALYREJS-STATUS.                                                     
           05  DALYREJS-STAT1      PIC X.                                       
           05  DALYREJS-STAT2      PIC X.                                       
                                                                                
       COPY CVACT01Y.                                                           
       01  ACCTFILE-STATUS.                                                     
           05  ACCTFILE-STAT1      PIC X.                                       
           05  ACCTFILE-STAT2      PIC X.                                       
                                                                                
       COPY COTRN03Y.                                                           
       01  TCATBALF-STATUS.                                                     
           05  TCATBALF-STAT1      PIC X.                                       
           05  TCATBALF-STAT2      PIC X.                                       
                                                                                
       01  WS-MISC-STORAGE.                                                     
           05  APPL-RESULT                 PIC S9(09) COMP      VALUE ZEROS.    
               88  APPL-AOK                              VALUE 0.                
           05  IO-STATUS                   PIC X(02)          VALUE SPACES.      
           05  WS-VALIDATION-FAIL-REASON   PIC 9(03)          VALUE ZERO.        
           05  WS-VALIDATION-FAIL-REASON-DESC                                   
                                           PIC X(100)         VALUE SPACES.      
                                                                                
       COPY COCOM01Y.                                                           
       COPY CODATECN.                                                           
                                                                                
       PROCEDURE DIVISION.                                                      
       0000-MAINLINE.                                                           
           PERFORM 0000-DALYTRAN-OPEN.                                          
           PERFORM 0000-TRANFILE-OPEN.                                          
           PERFORM 0000-XREFFILE-OPEN.                                          
           PERFORM 0000-DALYREJS-OPEN.                                          
           PERFORM 0000-ACCTFILE-OPEN.                                          
           PERFORM 0000-TCATBALF-OPEN.                                          
                                                                                
           IF APPL-AOK                                                          
               PERFORM 1000-DALYTRAN-GET-NEXT                                   
               PERFORM UNTIL DALYTRAN-STAT1 = '1'                               
      *              DISPLAY DALYTRAN-RECORD                                    
                   PERFORM 1100-LOOKUP-CARD                                     
                   IF APPL-AOK                                                  
                       PERFORM 1200-LOOKUP-ACCOUNT                              
                   END-IF                                                       
                   IF APPL-AOK                                                  
                       PERFORM 1300-VALIDATE-TRANSACTION                        
                   END-IF                                                       
                   IF APPL-AOK                                                  
                       PERFORM 2000-POST-TRANSACTION                            
                       PERFORM 2600-A-UPDATE-TCATBAL-REC                        
                       PERFORM 2800-UPDATE-ACCOUNT-REC                          
                       PERFORM 2900-WRITE-TRANSACTION-FILE                      
                   ELSE                                                         
                       DISPLAY ' Transaction validation failed: '               
                           WS-VALIDATION-FAIL-REASON ' '                        
                           WS-VALIDATION-FAIL-REASON-DESC                       
                       PERFORM 2100-WRITE-REJECT-FILE                           
                   END-IF                                                       
                   PERFORM 1000-DALYTRAN-GET-NEXT                               
               END-PERFORM                                                      
           END-IF.                                                              
                                                                                
           PERFORM 9000-DALYTRAN-CLOSE.                                         
           PERFORM 9000-TRANFILE-CLOSE.                                         
           PERFORM 9000-XREFFILE-CLOSE.                                         
           PERFORM 9000-DALYREJS-CLOSE.                                         
           PERFORM 9000-ACCTFILE-CLOSE.                                         
           PERFORM 9000-TCATBALF-CLOSE.                                         
           GOBACK.                                                              
      *---------------------------------------------------------------*         
       0000-DALYTRAN-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN INPUT DALYTRAN-FILE                                             
           IF  DALYTRAN-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING DAILY TRANSACTION FILE'                   
               MOVE DALYTRAN-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       0000-TRANFILE-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN I-O TRANSACT-FILE                                               
           IF  TRANFILE-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING TRANSACTION FILE'                         
               MOVE TRANFILE-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF.                                                              
                                                                                
      *---------------------------------------------------------------*         
       0000-XREFFILE-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN INPUT XREF-FILE                                                 
           IF  XREFFILE-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING XREF FILE'                                
               MOVE XREFFILE-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF.                                                              
                                                                                
      *---------------------------------------------------------------*         
       0000-DALYREJS-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN OUTPUT DALYREJS-FILE                                            
           IF  DALYREJS-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING REJECT FILE'                              
               MOVE DALYREJS-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF.                                                              
                                                                                
      *---------------------------------------------------------------*         
       0000-ACCTFILE-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN I-O ACCOUNT-FILE                                                
           IF  ACCTFILE-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING ACCOUNT FILE'                             
               MOVE ACCTFILE-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF.                                                              
                                                                                
      *---------------------------------------------------------------*         
       0000-TCATBALF-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN I-O TCATBAL-FILE                                                
           IF  TCATBALF-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING TRANSACTION BALANCE FILE'                 
               MOVE TCATBALF-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       1000-DALYTRAN-GET-NEXT.                                                  
           MOVE 8 TO APPL-RESULT.                                               
           READ DALYTRAN-FILE INTO DALYTRAN-RECORD.                             
           IF  DALYTRAN-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
      *        DISPLAY DALYTRAN-RECORD                                          
           ELSE                                                                 
               IF  DALYTRAN-STATUS = '10'                                       
                   MOVE 0 TO APPL-RESULT                                        
               ELSE                                                             
                   MOVE 12 TO APPL-RESULT                                       
               END-IF                                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR READING DAILY TRANSACTION FILE'                   
               MOVE DALYTRAN-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF.                                                              
                                                                                
           EXIT.                                                                
      *---------------------------------------------------------------*         
       1100-LOOKUP-CARD.                                                        
           READ XREF-FILE INTO CARD-XREF-RECORD                                
              KEY IS DALYTRAN-CARD-NUM                                         
              INVALID KEY                                                      
                MOVE 101 TO WS-VALIDATION-FAIL-REASON                          
                MOVE 'CARD NOT FOUND FOR TRANS RECORD'                         
                  TO WS-VALIDATION-FAIL-REASON-DESC                            
           END-READ.                                                           
           EXIT.                                                              
      *---------------------------------------------------------------*         
       1200-LOOKUP-ACCOUNT.                                                    
           READ ACCOUNT-FILE INTO ACCOUNT-RECORD                               
              KEY IS XREF-ACCT-ID                                              
              INVALID KEY                                                      
                MOVE 102 TO WS-VALIDATION-FAIL-REASON                          
                MOVE 'ACCOUNT NOT FOUND FOR CARD RECORD'                       
                  TO WS-VALIDATION-FAIL-REASON-DESC                            
           END-READ.                                                           
           EXIT.                                                              
      *---------------------------------------------------------------*         
       1300-VALIDATE-TRANSACTION.                                              
      * Check if card is active                                                
      *    DISPLAY 'CARD NUMBER: ' DALYTRAN-CARD-NUM                          
           MOVE 0 TO APPL-RESULT.                                              
           IF CARD-STATUS NOT = 'A'                                            
                MOVE 103 TO WS-VALIDATION-FAIL-REASON                          
                MOVE 'CARD ACCOUNT IS NOT ACTIVE'                              
                  TO WS-VALIDATION-FAIL-REASON-DESC                            
           ELSE                                                                
      * Check if transaction would exceed limit                                
      *         DISPLAY 'TRAN-AMT         :' DALYTRAN-AMT                      
      *         DISPLAY 'TRAN-DESC        :' DALYTRAN-DESC                     
      *         DISPLAY 'ACCT-CURR-BAL    :' ACCT-CURR-BAL                     
      *         DISPLAY 'ACCT-CREDIT-LIMIT:' ACCT-CREDIT-LIMIT                 
               IF (ACCT-CURR-BAL + DALYTRAN-AMT) > ACCT-CREDIT-LIMIT           
                 MOVE 104 TO WS-VALIDATION-FAIL-REASON                         
                 MOVE 'TRANSACTION EXCEEDS CREDIT LIMIT'                       
                   TO WS-VALIDATION-FAIL-REASON-DESC                           
               ELSE                                                            
      * Check if account is active                                             
                 IF ACCT-STATUS NOT = 'A'                                      
                   MOVE 105 TO WS-VALIDATION-FAIL-REASON                       
                   MOVE 'ACCOUNT NOT ACTIVE'                                   
                     TO WS-VALIDATION-FAIL-REASON-DESC                         
                 ELSE                                                          
      * Check if transaction was before expiry                                 
                   IF ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)           
                       MOVE 0 TO WS-VALIDATION-FAIL-REASON                     
                   ELSE                                                        
                     MOVE 106 TO WS-VALIDATION-FAIL-REASON                     
                     MOVE 'TRANSACTION RECEIVED AFTER ACCT EXPIRATION'         
                       TO WS-VALIDATION-FAIL-REASON-DESC                       
                   END-IF                                                      
               END-IF                                                          
           END-READ                                                            
           EXIT.                                                               
      *---------------------------------------------------------------*         
       2000-POST-TRANSACTION.                                                  
           MOVE  DALYTRAN-ID            TO    TRAN-ID                          
           MOVE  DALYTRAN-TYPE-CD       TO    TRAN-TYPE-CD                     
           MOVE  DALYTRAN-CAT-CD        TO    TRAN-CAT-CD                      
           MOVE  DALYTRAN-SOURCE        TO    TRAN-SOURCE                      
           MOVE  DALYTRAN-DESC          TO    TRAN-DESC                        
           MOVE  DALYTRAN-AMT           TO    TRAN-AMT                         
           MOVE  DALYTRAN-MERCHANT-ID   TO    TRAN-MERCHANT-ID                 
           MOVE  DALYTRAN-MERCHANT-NAME TO    TRAN-MERCHANT-NAME               
           MOVE  DALYTRAN-MERCHANT-CITY TO    TRAN-MERCHANT-CITY               
           MOVE  DALYTRAN-MERCHANT-ZIP  TO    TRAN-MERCHANT-ZIP                
           MOVE  DALYTRAN-CARD-NUM      TO    TRAN-CARD-NUM                    
           MOVE  DALYTRAN-ORIG-TS       TO    TRAN-ORIG-TS                     
           PERFORM Z-GET-DB2-FORMAT-TIMESTAMP                                  
           MOVE  DB2-FORMAT-TS          TO    TRAN-PROC-TS                     
           IF DALYTRAN-CURRENCY-CD = SPACES                                    
               MOVE 'SEK'                TO    TRAN-CURRENCY-CD                
           ELSE                                                                
               MOVE DALYTRAN-CURRENCY-CD TO    TRAN-CURRENCY-CD                
           END-IF                                                              
                                                                                
           PERFORM 2700-UPDATE-TCATBAL                                         
           PERFORM 2800-UPDATE-ACCOUNT-REC                                     
           PERFORM 2900-WRITE-TRANSACTION-FILE.                                
           EXIT.                                                               
      *---------------------------------------------------------------*         
       2100-WRITE-REJECT-FILE.                                                 
           MOVE DALYTRAN-RECORD TO REJECT-TRAN-DATA                            
           MOVE WS-VALIDATION-FAIL-REASON-DESC                                 
                                  TO REJECT-REASON                             
           WRITE DALYREJS-RECORD.                                              
           EXIT.                                                               
      *---------------------------------------------------------------*         
       2600-A-UPDATE-TCATBAL-REC.                                              
           MOVE DALYTRAN-TYPE-CD TO FD-TRANCAT-TYPE-CD                         
           MOVE DALYTRAN-CAT-CD TO FD-TRANCAT-CD                               
           READ TCATBAL-FILE INTO TRAN-CAT-BAL-RECORD                          
               INVALID KEY                                                     
                  MOVE 107 TO WS-VALIDATION-FAIL-REASON                        
                  MOVE 'TRANSACTION CATEGORY RECORD NOT FOUND'                 
                    TO WS-VALIDATION-FAIL-REASON-DESC                          
           END-READ.                                                           
           IF  WS-VALIDATION-FAIL-REASON = 107                                 
               PERFORM 2600-B-CREATE-TCATBAL-REC                               
           ELSE                                                                
               PERFORM 2700-B-UPDATE-TCATBAL-REC                               
           END-IF.                                                             
           EXIT.                                                               
      *---------------------------------------------------------------*         
       2600-B-CREATE-TCATBAL-REC.                                              
           MOVE XREF-ACCT-ID TO TRANCAT-ACCT-ID                                
           MOVE DALYTRAN-TYPE-CD TO TRANCAT-TYPE-CD                            
           MOVE DALYTRAN-CAT-CD TO TRANCAT-CD                                  
           MOVE DALYTRAN-AMT TO TRAN-CAT-BAL                                   
           WRITE FD-TRAN-CAT-BAL-RECORD FROM TRAN-CAT-BAL-RECORD               
                                                                                
           IF  TCATBALF-STATUS = '00'                                          
               MOVE 0 TO APPL-RESULT                                           
           ELSE                                                                
               MOVE 12 TO APPL-RESULT                                          
           END-IF                                                              
           IF  APPL-AOK                                                        
               CONTINUE                                                        
           ELSE                                                                
               DISPLAY 'ERROR WRITING TRANSACTION BALANCE FILE'                
               MOVE TCATBALF-STATUS TO IO-STATUS                               
               PERFORM 9910-DISPLAY-IO-STATUS                                  
               PERFORM 9999-ABEND-PROGRAM                                      
           END-IF.                                                             
      *---------------------------------------------------------------*         
       2700-B-UPDATE-TCATBAL-REC.                                              
           ADD DALYTRAN-AMT TO TRAN-CAT-BAL                                    
           REWRITE FD-TRAN-CAT-BAL-RECORD FROM TRAN-CAT-BAL-RECORD             
                                                                                
           IF  TCATBALF-STATUS = '00'                                          
               MOVE 0 TO APPL-RESULT                                           
           ELSE                                                                
               MOVE 12 TO APPL-RESULT                                          
           END-IF                                                              
           IF  APPL-AOK                                                        
               CONTINUE                                                        
           ELSE                                                                
               DISPLAY 'ERROR REWRITING TRANSACTION BALANCE FILE'              
               MOVE TCATBALF-STATUS TO IO-STATUS                               
               PERFORM 9910-DISPLAY-IO-STATUS                                  
               PERFORM 9999-ABEND-PROGRAM                                      
           END-IF.                                                             
                                                                                
      *---------------------------------------------------------------*         
       2800-UPDATE-ACCOUNT-REC.                                                
      * Update the balances in account record to reflect posted trans.         
           ADD DALYTRAN-AMT  TO ACCT-CURR-BAL                                  
           IF DALYTRAN-AMT >= 0                                                
              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-CREDIT                         
           ELSE                                                                
              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT                          
           END-IF                                                              
                                                                                
           REWRITE FD-ACCTFILE-REC FROM  ACCOUNT-RECORD                        
              INVALID KEY                                                      
                MOVE 109 TO WS-VALIDATION-FAIL-REASON                          
                MOVE 'ACCOUNT RECORD NOT FOUND'                                
                  TO WS-VALIDATION-FAIL-REASON-DESC                            
           END-REWRITE.                                                        
           EXIT.                                                               
      *---------------------------------------------------------------*         
       2900-WRITE-TRANSACTION-FILE.                                            
           MOVE 8 TO  APPL-RESULT.                                             
           WRITE FD-TRANFILE-REC FROM TRAN-RECORD                              
                                                                                
           IF  TRANFILE-STATUS = '00'                                          
               MOVE 0 TO  APPL-RESULT                                          
           ELSE                                                                
               MOVE 12 TO APPL-RESULT                                          
           END-IF                                                              
           IF  APPL-AOK                                                        
               CONTINUE                                                        
           ELSE                                                                
               DISPLAY 'ERROR WRITING TRANSACTION FILE'                        
               MOVE TRANFILE-STATUS  TO IO-STATUS                              
               PERFORM 9910-DISPLAY-IO-STATUS                                  
               PERFORM 9999-ABEND-PROGRAM                                      
           END-IF.                                                             
                                                                                
      *---------------------------------------------------------------*         
       9000-DALYTRAN-CLOSE.                                                    
           MOVE 8 TO APPL-RESULT.                                              
           CLOSE DALYTRAN-FILE                                                 
           IF  DALYTRAN-STATUS = '00'                                          
               MOVE 0 TO APPL-RESULT                                           
           ELSE                                                                
               MOVE 12 TO APPL-RESULT                                          
           END-IF                                                              
           IF  APPL-AOK                                                        
               CONTINUE                                                        
           ELSE                                                                
               DISPLAY 'ERROR CLOSING DAILY TRANSACTION FILE'                  
               MOVE DALYTRAN-STATUS TO IO-STATUS                               
               PERFORM 9910-DISPLAY-IO-STATUS                                  
               PERFORM 9999-ABEND-PROGRAM                                      
           END-IF.                                                             
                                                                                
      *---------------------------------------------------------------*         
       9000-TRANFILE-CLOSE.                                                    
           MOVE 8 TO APPL-RESULT.                                              
           CLOSE TRANSACT-FILE                                                 
           IF  TRANFILE-STATUS = '00'                                          
               MOVE 0 TO APPL-RESULT                                           
           ELSE                                                                
               MOVE 12 TO APPL-RESULT                                          
           END-IF                                                              
           IF  APPL-AOK                                                        
               CONTINUE                                                        
           ELSE                                                                
               DISPLAY 'ERROR CLOSING TRANSACTION FILE'                        
               MOVE TRANFILE-STATUS  TO IO-STATUS                              
               PERFORM 9910-DISPLAY-IO-STATUS                                  
               PERFORM 9999-ABEND-PROGRAM                                      
           END-IF.                                                             
                                                                                
      *---------------------------------------------------------------*         
       9000-XREFFILE-CLOSE.                                                    
           MOVE 8 TO APPL-RESULT.                                              
           CLOSE XREF-FILE                                                     
           IF  XREFFILE-STATUS = '00'                                          
               MOVE 0 TO APPL-RESULT                                           
           ELSE                                                                
               MOVE 12 TO APPL-RESULT                                          
           END-IF                                                              
           IF  APPL-AOK                                                        
               CONTINUE                                                        
           ELSE                                                                
               DISPLAY 'ERROR CLOSING XREF FILE'                               
               MOVE XREFFILE-STATUS TO IO-STATUS                               
               PERFORM 9910-DISPLAY-IO-STATUS                                  
               PERFORM 9999-ABEND-PROGRAM                                      
           END-IF.                                                             
                                                                                
      *---------------------------------------------------------------*         
       9000-DALYREJS-CLOSE.                                                    
           MOVE 8 TO APPL-RESULT.                                              
           CLOSE DALYREJS-FILE                                                 
           IF  DALYREJS-STATUS = '00'                                          
               MOVE 0 TO APPL-RESULT                                           
           ELSE                                                                
               MOVE 12 TO APPL-RESULT                                          
           END-IF                                                              
           IF  APPL-AOK                                                        
               CONTINUE                                                        
           ELSE                                                                
               DISPLAY 'ERROR CLOSING REJECT FILE'                             
               MOVE DALYREJS-STATUS TO IO-STATUS                               
               PERFORM 9910-DISPLAY-IO-STATUS                                  
               PERFORM 9999-ABEND-PROGRAM                                      
           END-IF.                                                             
                                                                                
      *---------------------------------------------------------------*         
       9000-ACCTFILE-CLOSE.                                                    
           MOVE 8 TO APPL-RESULT.                                              
           CLOSE ACCOUNT-FILE                                                  
           IF  ACCTFILE-STATUS = '00'                                          
               MOVE 0 TO APPL-RESULT                                           
           ELSE                                                                
               MOVE 12 TO APPL-RESULT                                          
           END-IF                                                              
           IF  APPL-AOK                                                        
               CONTINUE                                                        
           ELSE                                                                
               DISPLAY 'ERROR CLOSING ACCOUNT FILE'                            
               MOVE ACCTFILE-STATUS TO IO-STATUS                               
               PERFORM 9910-DISPLAY-IO-STATUS                                  
               PERFORM 9999-ABEND-PROGRAM                                      
           END-IF.                                                             
                                                                                
      *---------------------------------------------------------------*         
       9000-TCATBALF-CLOSE.                                                    
           MOVE 8 TO APPL-RESULT.                                              
           CLOSE TCATBAL-FILE                                                  
           IF  TCATBALF-STATUS = '00'                                          
               MOVE 0 TO APPL-RESULT                                           
           ELSE                                                                
               MOVE 12 TO APPL-RESULT                                          
           END-IF                                                              
           IF  APPL-AOK                                                        
               CONTINUE                                                        
           ELSE                                                                
               DISPLAY 'ERROR CLOSING TRANSACTION BALANCE FILE'                
               MOVE TCATBALF-STATUS TO IO-STATUS                               
               PERFORM 9910-DISPLAY-IO-STATUS                                  
               PERFORM 9999-ABEND-PROGRAM                                      
           END-IF.                                                             
                                                                                
      *---------------------------------------------------------------*         
       9910-DISPLAY-IO-STATUS.                                                 
           DISPLAY 'FILE STATUS IS:' IO-STATUS.                                
           EXIT.                                                               
      *---------------------------------------------------------------*         
       9999-ABEND-PROGRAM.                                                     
           DISPLAY 'ABENDING PROGRAM.'                                         
           MOVE 12 TO RETURN-CODE                                              
           GOBACK.                                                             
