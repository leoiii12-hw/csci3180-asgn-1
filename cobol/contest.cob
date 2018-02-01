      ******************************************************************
      *> /*
      *>  * CSCI3180 Principles of Programming Languages
      *>  *
      *>  * --- Declaration ---
      *>  *
      *>  * I declare that the assignment here submitted is original except for source
      *>  * material explicitly acknowledged. I also acknowledge that I am aware of
      *>  * University policy and regulations on honesty in academic work, and of the
      *>  * disciplinary guidelines and procedures applicable to breaches of such policy
      *>  * and regulations, as contained in the website
      *>  * http://www.cuhk.edu.hk/policy/academichonesty/
      *>  *
      *>  * Assignment 1
      *>  * Name : Choi Man Kin
      *>  * Student ID : 1155077469
      *>  * Email Addr : mkchoi6@cse.cuhk.edu.hk
      *>  */
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            *>  INPUT
            SELECT T-FILE ASSIGN TO 'teams.txt'
              ORGANIZATION IS LINE SEQUENTIAL.
            SELECT SR-FILE ASSIGN TO 'submission-records.txt'
              ORGANIZATION IS LINE SEQUENTIAL.
            
            *>  OUTPUT
            SELECT NEW-LINE-FILE ASSIGN TO 'reportcob.txt'
              ORGANIZATION IS SEQUENTIAL.

            SELECT NEW-FILE ASSIGN TO 'reportcob.txt'
              ORGANIZATION IS SEQUENTIAL.
            SELECT CONTEST-HEADER-FILE ASSIGN TO 'reportcob.txt'
              ORGANIZATION IS SEQUENTIAL.
            SELECT CONTEST-TSR-FILE ASSIGN TO 'reportcob.txt'
              ORGANIZATION IS SEQUENTIAL.
            SELECT TEAM-NAME-FILE ASSIGN TO 'reportcob.txt'
              ORGANIZATION IS SEQUENTIAL.
            SELECT PROBLEM-ID-FILE ASSIGN TO 'reportcob.txt'
              ORGANIZATION IS SEQUENTIAL.
            SELECT PROBLEM-SCORE-FILE ASSIGN TO 'reportcob.txt'
              ORGANIZATION IS SEQUENTIAL.
            SELECT PFS-FILE ASSIGN TO 'reportcob.txt'
              ORGANIZATION IS SEQUENTIAL.
            

       DATA DIVISION.
       FILE SECTION.

       *>  INPUT
       FD T-FILE.
       01 TEAMS.
            02 TEAM-NAME PIC X(15).
       FD SR-FILE.
       01 SUBMISSION-RECORDS.
            04 TEAM-NAME PIC X(15).
            04 PROBLEM-ID PIC 9(1).
            04 OUTCOME PIC X(19).
            04 SCORE PIC 9(3).

       *>  OUTPUT
       FD NEW-LINE-FILE.
       01 NLF-NEW-LINE.
            02 CARRIAGE-RETURN PIC X.
            02 LINE-FEED PIC X.

       FD CONTEST-HEADER-FILE.
       01 CF-HEADER.
            02 CF-HEADER-DATA PIC X(33).
       FD CONTEST-TSR-FILE.
       01 TSRF-HEADER.
            02 TSRF-HEADER-DATA PIC X(17).
       FD TEAM-NAME-FILE.
       01 TNF-TEAM-NAME.
            02 TNF-TEAM-NAME-DATA PIC X(15).
       FD PROBLEM-ID-FILE.      
       01 PIF-PROBLEM.
            03 PIF-LEFT-QUOTE PIC X(1).
            03 PIF-PROBLEM-ID PIC X(1).
            03 PIF-RIGHT-QUOTE PIC X(1).
       FD PROBLEM-SCORE-FILE.      
       01 PSF-SCORE.
            02 PSF-SCORE-DATA PIC X(3).
            02 PSF-SPACE PIC X(1).
       FD PFS-FILE.      
       01 PFSF-SCORE.
            02 PFSF-T PIC X(2) VALUE SPACE.
            02 PFSF-SCORE-DATA PIC X(4) VALUE SPACE.
            

       WORKING-STORAGE SECTION.
       01 WS-TEAM.
            02 T-TEAM-NAME PIC X(15).
       01 WS-SUBMISSION-RECORD.
            04 SR-TEAM-NAME PIC X(15).
            04 SR-PROBLEM-ID PIC 9(1).
            04 SR-OUTCOME PIC X(19).
            04 SR-SCORE PIC 9(3).
       01 WS-SUBMISSION-RECORD-PROCESSED PIC 9(1).
       01 WS-SUBMISSION-RECORD-ENDED PIC 9(1). 

       01 WS-TEAM-TABLE. 
            05 WS-PROBLEM OCCURS 10 TIMES INDEXED BY I.
                  10 WS-PROBLEM-MIN-SCORE PIC 9(3).
                  10 WS-PROBLEM-MAX-SCORE PIC 9(3).
                  10 WS-PROBLEM-BASE-SCORE PIC 9(3).
                  10 WS-PROBLEM-NUM-OF-SUBMISSIONS PIC 9(3).
                  10 WS-PROBLEM-TOTAL-SCORE PIC 9(3).

       01 WS-PROCESSING-PROBLEM-ID PIC 9(2). 
       01 WS-PROCESSING-I PIC 9(2). 

       01 WS-PROBLEM-B-SCORE PIC 9(3)V9(2).
       01 WS-PROBLEM-DECAY PIC 9(3)V9(2).
       01 WS-PROBLEM-R-SCORE PIC 9(3)V9(2).   
       01 WS-PROBLEM-FINAL-SCORE PIC 9(3).
       
       01 WS-ALL-PROBLEMS-SCORE PIC 9(4).

       01 ONE_NUMBER_STRING PIC 9(1).
       01 TWO_STRING PIC X(2).
       01 THREE_STRING PIC X(3).
       01 FOUR_STRING PIC X(4).
       
       PROCEDURE DIVISION.
       MAIN-PROC.
            PERFORM CREATE-OUTPUT-PROC.
            PERFORM DISPLAY-HEADER-PROC.

            OPEN INPUT T-FILE.
            OPEN INPUT SR-FILE.

            *> The first record is empty
            MOVE 1 TO WS-SUBMISSION-RECORD-PROCESSED.
            
            GO TO TEAM-PROC.
       CREATE-OUTPUT-PROC.
            OPEN OUTPUT NEW-FILE.
            CLOSE NEW-FILE.
       RESET-ALL-VARIABLES-PROC.
            *> DISPLAY "RESET-ALL-VARIABLES-PROC".

            MOVE 0 TO WS-PROCESSING-PROBLEM-ID.

            PERFORM RESET-TEAM-VARIABLES-PROC.

            MOVE 0 TO WS-PROBLEM-FINAL-SCORE.
            MOVE 0 TO WS-ALL-PROBLEMS-SCORE.
       RESET-TEAM-VARIABLES-PROC.
            SET I TO 1.

            MOVE 100 TO WS-PROBLEM-MIN-SCORE(1).
            MOVE 100 TO WS-PROBLEM-MIN-SCORE(2).
            MOVE 100 TO WS-PROBLEM-MIN-SCORE(3).
            MOVE 100 TO WS-PROBLEM-MIN-SCORE(4).
            MOVE 100 TO WS-PROBLEM-MIN-SCORE(5).
            MOVE 100 TO WS-PROBLEM-MIN-SCORE(6).
            MOVE 100 TO WS-PROBLEM-MIN-SCORE(7).
            MOVE 100 TO WS-PROBLEM-MIN-SCORE(8).
            MOVE 100 TO WS-PROBLEM-MIN-SCORE(9).
            MOVE 100 TO WS-PROBLEM-MIN-SCORE(10).

            MOVE 0 TO WS-PROBLEM-MAX-SCORE(1).
            MOVE 0 TO WS-PROBLEM-MAX-SCORE(2).
            MOVE 0 TO WS-PROBLEM-MAX-SCORE(3).
            MOVE 0 TO WS-PROBLEM-MAX-SCORE(4).
            MOVE 0 TO WS-PROBLEM-MAX-SCORE(5).
            MOVE 0 TO WS-PROBLEM-MAX-SCORE(6).
            MOVE 0 TO WS-PROBLEM-MAX-SCORE(7).
            MOVE 0 TO WS-PROBLEM-MAX-SCORE(8).
            MOVE 0 TO WS-PROBLEM-MAX-SCORE(9).
            MOVE 0 TO WS-PROBLEM-MAX-SCORE(10).
            
            MOVE 0 TO WS-PROBLEM-BASE-SCORE(1).
            MOVE 0 TO WS-PROBLEM-BASE-SCORE(2).
            MOVE 0 TO WS-PROBLEM-BASE-SCORE(3).
            MOVE 0 TO WS-PROBLEM-BASE-SCORE(4).
            MOVE 0 TO WS-PROBLEM-BASE-SCORE(5).
            MOVE 0 TO WS-PROBLEM-BASE-SCORE(6).
            MOVE 0 TO WS-PROBLEM-BASE-SCORE(7).
            MOVE 0 TO WS-PROBLEM-BASE-SCORE(8).
            MOVE 0 TO WS-PROBLEM-BASE-SCORE(9).
            MOVE 0 TO WS-PROBLEM-BASE-SCORE(10).

            MOVE 0 TO WS-PROBLEM-NUM-OF-SUBMISSIONS(1).
            MOVE 0 TO WS-PROBLEM-NUM-OF-SUBMISSIONS(2).
            MOVE 0 TO WS-PROBLEM-NUM-OF-SUBMISSIONS(3).
            MOVE 0 TO WS-PROBLEM-NUM-OF-SUBMISSIONS(4).
            MOVE 0 TO WS-PROBLEM-NUM-OF-SUBMISSIONS(5).
            MOVE 0 TO WS-PROBLEM-NUM-OF-SUBMISSIONS(6).
            MOVE 0 TO WS-PROBLEM-NUM-OF-SUBMISSIONS(7).
            MOVE 0 TO WS-PROBLEM-NUM-OF-SUBMISSIONS(8).
            MOVE 0 TO WS-PROBLEM-NUM-OF-SUBMISSIONS(9).
            MOVE 0 TO WS-PROBLEM-NUM-OF-SUBMISSIONS(10).

            MOVE 0 TO WS-PROBLEM-TOTAL-SCORE(1).
            MOVE 0 TO WS-PROBLEM-TOTAL-SCORE(2).
            MOVE 0 TO WS-PROBLEM-TOTAL-SCORE(3).
            MOVE 0 TO WS-PROBLEM-TOTAL-SCORE(4).
            MOVE 0 TO WS-PROBLEM-TOTAL-SCORE(5).
            MOVE 0 TO WS-PROBLEM-TOTAL-SCORE(6).
            MOVE 0 TO WS-PROBLEM-TOTAL-SCORE(7).
            MOVE 0 TO WS-PROBLEM-TOTAL-SCORE(8).
            MOVE 0 TO WS-PROBLEM-TOTAL-SCORE(9).
            MOVE 0 TO WS-PROBLEM-TOTAL-SCORE(10).
       END-PROC.
            *> DISPLAY "END-PROC".

            CLOSE T-FILE.
            CLOSE SR-FILE.     

            STOP RUN.
       TEAM-PROC.
            *> DISPLAY "TEAM-PROC".

            PERFORM RESET-ALL-VARIABLES-PROC.

            READ T-FILE INTO WS-TEAM
                  AT END GO TO END-PROC
            END-READ.

            PERFORM DISPLAY-TEAM-NAME-PROC.

            PERFORM RESET-TEAM-VARIABLES-PROC.

            GO TO SCAN-RECORDS-LOOP-PROC.
       SCAN-RECORDS-LOOP-PROC.
            *> DISPLAY "SCAN-RECORDS-LOOP-PROC".

            IF WS-SUBMISSION-RECORD-PROCESSED = 0 THEN
                  IF T-TEAM-NAME = SR-TEAM-NAME THEN
                        PERFORM SCAN-RECORDS-ACTION-PROC
                        MOVE 1 TO WS-SUBMISSION-RECORD-PROCESSED
                  END-IF
            END-IF.

            IF WS-SUBMISSION-RECORD-PROCESSED = 1 THEN
                  IF WS-SUBMISSION-RECORD-ENDED <> 1 THEN
                        READ SR-FILE INTO WS-SUBMISSION-RECORD
                              AT END
                                    MOVE 1 TO WS-SUBMISSION-RECORD-ENDED
                                    GO TO CAL-PROBLEM-SCORE-LOOP-PROC
                        END-READ
                  END-IF
                  MOVE 0 TO WS-SUBMISSION-RECORD-PROCESSED
            END-IF.

            IF T-TEAM-NAME = SR-TEAM-NAME THEN
                  PERFORM SCAN-RECORDS-ACTION-PROC
                  MOVE 1 TO WS-SUBMISSION-RECORD-PROCESSED
            END-IF.

            IF T-TEAM-NAME <> SR-TEAM-NAME THEN
                  GO TO CAL-PROBLEM-SCORE-LOOP-PROC
            END-IF.

            GO TO SCAN-RECORDS-LOOP-PROC.
       SCAN-RECORDS-ACTION-PROC.
            *> DISPLAY "SCAN-RECORDS-ACTION-PROC".

            COMPUTE WS-PROCESSING-I = SR-PROBLEM-ID + 1.
            SET I TO WS-PROCESSING-I.

            MOVE SR-SCORE TO WS-PROBLEM-BASE-SCORE(I).            
            IF SR-SCORE < WS-PROBLEM-MIN-SCORE(I) THEN
                  MOVE SR-SCORE TO WS-PROBLEM-MIN-SCORE(I)
            END-IF.
            IF SR-SCORE > WS-PROBLEM-MAX-SCORE(I) THEN
                  MOVE SR-SCORE TO WS-PROBLEM-MAX-SCORE(I)
            END-IF.

            ADD 1 TO WS-PROBLEM-NUM-OF-SUBMISSIONS(I)
                  GIVING WS-PROBLEM-NUM-OF-SUBMISSIONS(I).

            ADD SR-SCORE TO WS-PROBLEM-TOTAL-SCORE(I)
                  GIVING WS-PROBLEM-TOTAL-SCORE(I).
            
            *> DISPLAY
            *>       WS-PROBLEM-BASE-SCORE, " ",
            *>       WS-PROBLEM-NUM-OF-SUBMISSIONS, " ",
            *>       WS-PROBLEM-TOTAL-SCORE.
       CAL-PROBLEM-SCORE-LOOP-PROC.
            *> DISPLAY "CAL-PROBLEM-SCORE-LOOP-PROC".

            *> DISPLAY
            *>       "*",
            *>       WS-PROBLEM-BASE-SCORE(1), " ",
            *>       WS-PROBLEM-NUM-OF-SUBMISSIONS(1), " ",
            *>       WS-PROBLEM-TOTAL-SCORE(1), " ",
            *>       WS-PROBLEM-MAX-SCORE(1), " "
            *>       WS-PROBLEM-MIN-SCORE(1), " "
            *>       WS-PROBLEM-FINAL-SCORE,
            *>       "*" NO ADVANCING.

            IF WS-PROCESSING-PROBLEM-ID = 10 THEN
                  PERFORM DISPLAY-TEAM-SCORE-PROC
                  GO TO TEAM-PROC
            END-IF.

            MOVE 0 TO WS-PROBLEM-B-SCORE.
            MOVE 0 TO WS-PROBLEM-DECAY.
            MOVE 0 TO WS-PROBLEM-R-SCORE.

            MOVE 0 TO WS-PROBLEM-FINAL-SCORE.            
            
            COMPUTE WS-PROCESSING-I = WS-PROCESSING-PROBLEM-ID + 1.
            SET I TO WS-PROCESSING-I.

            IF (WS-PROBLEM-MAX-SCORE(I) > 0) THEN
                  MOVE WS-PROBLEM-BASE-SCORE(I) TO WS-PROBLEM-B-SCORE

                  IF (WS-PROBLEM-BASE-SCORE(I) = 100) THEN
                        MOVE 1.0 TO WS-PROBLEM-DECAY
                  END-IF
                  IF (WS-PROBLEM-BASE-SCORE(I) < 100) THEN
                        COMPUTE WS-PROBLEM-DECAY =
                              1.0 / WS-PROBLEM-NUM-OF-SUBMISSIONS(I)
                  END-IF

                  IF (WS-PROBLEM-MAX-SCORE(I) <= 30) THEN
                        MOVE 0 TO WS-PROBLEM-R-SCORE
                  END-IF
                  IF (WS-PROBLEM-MAX-SCORE(I) > 30) THEN
                        COMPUTE WS-PROBLEM-R-SCORE =
                              100 -
                              WS-PROBLEM-MAX-SCORE(I) +
                              WS-PROBLEM-MIN-SCORE(I)
                  END-IF

                  COMPUTE WS-PROBLEM-FINAL-SCORE =
                        0.6*WS-PROBLEM-B-SCORE*WS-PROBLEM-DECAY+
                        0.3*WS-PROBLEM-TOTAL-SCORE(I)/
                        WS-PROBLEM-NUM-OF-SUBMISSIONS(I)+
                        0.1*WS-PROBLEM-R-SCORE
            END-IF.

            PERFORM DISPLAY-PROBLEM-ID-PROC.
            PERFORM DISPLAY-PROBLEM-SCORE-PROC.

            *> ADD TO TOTAL
            ADD WS-PROBLEM-FINAL-SCORE TO WS-ALL-PROBLEMS-SCORE
                  GIVING WS-ALL-PROBLEMS-SCORE.

            ADD 1 TO WS-PROCESSING-PROBLEM-ID
                  GIVING WS-PROCESSING-PROBLEM-ID.

            GO TO CAL-PROBLEM-SCORE-LOOP-PROC.
       DISPLAY-HEADER-PROC.
            OPEN EXTEND CONTEST-HEADER-FILE.
            MOVE "2018 CUHK CSE Programming Contest" TO CF-HEADER-DATA
            WRITE CF-HEADER.
            CLOSE CONTEST-HEADER-FILE.

            PERFORM DISPLAY-NEW-LINE-PROC.

            OPEN EXTEND CONTEST-TSR-FILE.
            MOVE "Team Score Report" TO TSRF-HEADER-DATA
            WRITE TSRF-HEADER.
            CLOSE CONTEST-TSR-FILE.

            PERFORM DISPLAY-NEW-LINE-PROC.
            PERFORM DISPLAY-NEW-LINE-PROC.
       DISPLAY-TEAM-NAME-PROC.
            *> DISPLAY T-TEAM-NAME NO ADVANCING.

            OPEN EXTEND TEAM-NAME-FILE.
            MOVE T-TEAM-NAME TO TNF-TEAM-NAME-DATA.
            WRITE TNF-TEAM-NAME.
            CLOSE TEAM-NAME-FILE.
       DISPLAY-PROBLEM-ID-PROC.
            MOVE WS-PROCESSING-PROBLEM-ID TO ONE_NUMBER_STRING.

            *> DISPLAY "(", ONE_NUMBER_STRING, ")" NO ADVANCING.

            OPEN EXTEND PROBLEM-ID-FILE.
            MOVE "(" TO PIF-LEFT-QUOTE.
            MOVE ONE_NUMBER_STRING TO PIF-PROBLEM-ID.
            MOVE ")" TO PIF-RIGHT-QUOTE.
            WRITE PIF-PROBLEM.
            CLOSE PROBLEM-ID-FILE.
       DISPLAY-PROBLEM-SCORE-PROC.
            IF WS-PROBLEM-FINAL-SCORE = 0 THEN
                  *> DISPLAY "  0 " NO ADVANCING
                  
                  OPEN EXTEND PROBLEM-SCORE-FILE
                  MOVE "  0" TO PSF-SCORE-DATA
                  MOVE " " TO PSF-SPACE
                  WRITE PSF-SCORE
                  CLOSE PROBLEM-SCORE-FILE
            END-IF.

            IF WS-PROBLEM-FINAL-SCORE > 0 THEN
                  MOVE WS-PROBLEM-FINAL-SCORE TO THREE_STRING
                  INSPECT THREE_STRING REPLACING LEADING "0" BY " "

                  *> DISPLAY THREE_STRING, " " NO ADVANCING

                  OPEN EXTEND PROBLEM-SCORE-FILE
                  MOVE THREE_STRING TO PSF-SCORE-DATA
                  MOVE " " TO PSF-SPACE
                  WRITE PSF-SCORE
                  CLOSE PROBLEM-SCORE-FILE
            END-IF.
       DISPLAY-TEAM-SCORE-PROC.
            *> DISPLAY "T:" NO ADVANCING.

            IF WS-ALL-PROBLEMS-SCORE = 0 THEN
                  *> DISPLAY "   0"
                  
                  OPEN EXTEND PFS-FILE
                  MOVE "T:" TO PFSF-T
                  MOVE "   0" TO PFSF-SCORE-DATA       
                  WRITE PFSF-SCORE
                  CLOSE PFS-FILE

                  PERFORM DISPLAY-NEW-LINE-PROC
            END-IF.
            IF WS-ALL-PROBLEMS-SCORE > 0 THEN
                  MOVE WS-ALL-PROBLEMS-SCORE TO FOUR_STRING
                  INSPECT FOUR_STRING REPLACING LEADING "0" BY " "

                  *> DISPLAY FOUR_STRING, " "
                  
                  OPEN EXTEND PFS-FILE
                  MOVE "T:" TO PFSF-T
                  MOVE FOUR_STRING TO PFSF-SCORE-DATA
                  WRITE PFSF-SCORE
                  CLOSE PFS-FILE

                  PERFORM DISPLAY-NEW-LINE-PROC          
            END-IF.
       DISPLAY-NEW-LINE-PROC.
            OPEN EXTEND NEW-LINE-FILE.
            MOVE X'0D' TO CARRIAGE-RETURN.
            MOVE X'0A' TO LINE-FEED.
            WRITE NLF-NEW-LINE.
            CLOSE NEW-LINE-FILE.
       END PROGRAM YOUR-PROGRAM-NAME.
