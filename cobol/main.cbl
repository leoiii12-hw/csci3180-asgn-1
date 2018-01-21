      ******************************************************************
      * Author: Choi Man Kin
      * Date: 17 Jan 2018
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT T-FILE ASSIGN TO 'teams.txt'
              ORGANIZATION IS LINE SEQUENTIAL.
            SELECT SR-FILE ASSIGN TO 'submission-records.txt'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD T-FILE.
       01 TEAMS.
            02 TEAM-NAME PIC X(15).
       FD SR-FILE.
       01 SUBMISSION-RECORDS.
            04 TEAM-NAME PIC X(15).
            04 PROBLEM-ID PIC 9(1).
            04 OUTCOME PIC X(19).
            04 SCORE PIC 9(3).

       WORKING-STORAGE SECTION.
       01 WS-TEAM.
            02 T-TEAM-NAME PIC X(15).
       01 WS-SUBMISSION-RECORD.
            04 SR-TEAM-NAME PIC X(15).
            04 SR-PROBLEM-ID PIC 9(1).
            04 SR-OUTCOME PIC X(19).
            04 SR-SCORE PIC 9(3).
      
       01 WS-PROCESSING-PROBLEM-ID PIC 9(2).

       01 WS-PROBLEM-MIN-SCORE PIC 9(3).
       01 WS-PROBLEM-MAX-SCORE PIC 9(3).
       01 WS-PROBLEM-BASE-SCORE PIC 9(3).
       01 WS-PROBLEM-NUM-OF-SUBMISSIONS PIC 9(3).
       01 WS-PROBLEM-TOTAL-SCORE PIC 9(3).

       01 WS-PROBLEM-FINAL-SCORE PIC 9(3).
       01 WS-ALL-PROBLEMS-SCORE PIC 9(4).

       01 ONE_NUMBER_STRING PIC 9(1).
       01 TWO_STRING PIC X(2).
       01 THREE_STRING PIC X(3).
       01 FOUR_STRING PIC X(4).
       
       PROCEDURE DIVISION.
       MAIN-PROC.
            OPEN INPUT T-FILE.
            OPEN INPUT SR-FILE.
            
            GO TO TEAM-PROC.
       RESET-ALL-VARIABLES-PROC.
            *> DISPLAY "RESET-ALL-VARIABLES-PROC".
            MOVE 0 TO WS-PROCESSING-PROBLEM-ID.

            PERFORM RESET-PROBLEM-VARIABLES-PROC.

            MOVE 0 TO WS-PROBLEM-FINAL-SCORE.
            MOVE 0 TO WS-ALL-PROBLEMS-SCORE.
       RESET-PROBLEM-VARIABLES-PROC.
            MOVE 100 TO WS-PROBLEM-MIN-SCORE.
            MOVE 0 TO WS-PROBLEM-MAX-SCORE.
            MOVE 0 TO WS-PROBLEM-BASE-SCORE.
            MOVE 0 TO WS-PROBLEM-NUM-OF-SUBMISSIONS.
            MOVE 0 TO WS-PROBLEM-TOTAL-SCORE.
       END-PROC.
            *> DISPLAY "END-PROC".

            CLOSE T-FILE.
            CLOSE SR-FILE.            

            STOP RUN.
       TEAM-PROC.
            *> DISPLAY "TEAM-PROC".

            READ T-FILE INTO WS-TEAM
                  AT END GO TO END-PROC
            END-READ.

            PERFORM RESET-ALL-VARIABLES-PROC.

            DISPLAY T-TEAM-NAME NO ADVANCING.
            GO TO SCAN-RECORDS-PROC.
       SCAN-RECORDS-PROC.
            *> DISPLAY "SCAN-RECORDS-PROC".
       
            *> RESET SR-FILE
            CLOSE SR-FILE.
            OPEN INPUT SR-FILE.

            MOVE WS-PROCESSING-PROBLEM-ID TO ONE_NUMBER_STRING
            DISPLAY "(", ONE_NUMBER_STRING, ")" NO ADVANCING

            PERFORM RESET-PROBLEM-VARIABLES-PROC.

            GO TO SCAN-RECORDS-LOOP-PROC.
       SCAN-RECORDS-LOOP-PROC.
            *> DISPLAY "SCAN-RECORDS-LOOP-PROC".
       
            READ SR-FILE INTO WS-SUBMISSION-RECORD
                  AT END GO TO PROBLEM-POST-PROC
            END-READ.

            IF T-TEAM-NAME = SR-TEAM-NAME THEN
                  IF SR-PROBLEM-ID = WS-PROCESSING-PROBLEM-ID THEN
                        PERFORM SCAN-RECORDS-ACTION-PROC
                  END-IF
            END-IF.

            GO TO SCAN-RECORDS-LOOP-PROC.
       SCAN-RECORDS-ACTION-PROC.
            *> DISPLAY "SCAN-RECORDS-ACTION-PROC".

            MOVE SR-SCORE TO WS-PROBLEM-BASE-SCORE.            
            IF SR-SCORE < WS-PROBLEM-MIN-SCORE THEN
                  MOVE SR-SCORE TO WS-PROBLEM-MIN-SCORE
            END-IF.
            IF SR-SCORE > WS-PROBLEM-MAX-SCORE THEN
                  MOVE SR-SCORE TO WS-PROBLEM-MAX-SCORE
            END-IF.

            ADD 1 TO WS-PROBLEM-NUM-OF-SUBMISSIONS
                  GIVING WS-PROBLEM-NUM-OF-SUBMISSIONS.

            ADD SR-SCORE TO WS-PROBLEM-TOTAL-SCORE
                  GIVING WS-PROBLEM-TOTAL-SCORE.

            *> DISPLAY
            *>       WS-PROBLEM-BASE-SCORE, " ",
            *>       WS-PROBLEM-NUM-OF-SUBMISSIONS, " ",
            *>       WS-PROBLEM-TOTAL-SCORE.
       PROBLEM-POST-PROC.
            *> DISPLAY "PROBLEM-POST-PROC".

            ADD 1 TO WS-PROCESSING-PROBLEM-ID
                  GIVING WS-PROCESSING-PROBLEM-ID.

            PERFORM SCORE-PRINTING-PROC.

            IF WS-PROCESSING-PROBLEM-ID = 10 THEN
                  PERFORM TEAM-TOTAL-SCORE-PROC
                  GO TO TEAM-PROC
            END-IF.

            GO TO SCAN-RECORDS-PROC.
       SCORE-PRINTING-PROC.
            *> DISPLAY "SCORE-PRINTING-PROC".

            MOVE 0 TO WS-PROBLEM-FINAL-SCORE.
            
            *> DISPLAY
            *>       "*",
            *>       WS-PROBLEM-BASE-SCORE, " ",
            *>       WS-PROBLEM-NUM-OF-SUBMISSIONS, " ",
            *>       WS-PROBLEM-TOTAL-SCORE, " ",
            *>       WS-PROBLEM-MAX-SCORE, " "
            *>       WS-PROBLEM-MIN-SCORE, " "
            *>       WS-PROBLEM-FINAL-SCORE,
            *>       "*" NO ADVANCING.
            
            *> base_score
            COMPUTE WS-PROBLEM-FINAL-SCORE =
                        WS-PROBLEM-FINAL-SCORE +
                        0.6 *
                        WS-PROBLEM-BASE-SCORE
            IF WS-PROBLEM-BASE-SCORE < 100 THEN
                  COMPUTE WS-PROBLEM-FINAL-SCORE =
                        WS-PROBLEM-FINAL-SCORE / 
                        WS-PROBLEM-NUM-OF-SUBMISSIONS
            END-IF

            *> average_score
            COMPUTE WS-PROBLEM-FINAL-SCORE =
                  WS-PROBLEM-FINAL-SCORE +
                  0.3 * 
                  WS-PROBLEM-TOTAL-SCORE / 
                  WS-PROBLEM-NUM-OF-SUBMISSIONS.

            *> robutness_score
            IF WS-PROBLEM-MAX-SCORE > 30 THEN
                  COMPUTE WS-PROBLEM-FINAL-SCORE = 
                        WS-PROBLEM-FINAL-SCORE +
                        0.1 *
                        (100 - 
                        WS-PROBLEM-MAX-SCORE + 
                        WS-PROBLEM-MIN-SCORE)
            END-IF.

            *> ADD TO TOTAL
            ADD WS-PROBLEM-FINAL-SCORE TO WS-ALL-PROBLEMS-SCORE
                  GIVING WS-ALL-PROBLEMS-SCORE.

            *> DISPLAY
            IF WS-PROBLEM-FINAL-SCORE = 0 THEN
                  DISPLAY "  0 " NO ADVANCING
            END-IF.
            IF WS-PROBLEM-FINAL-SCORE > 0 THEN
                  MOVE WS-PROBLEM-FINAL-SCORE TO THREE_STRING
                  INSPECT THREE_STRING REPLACING LEADING "0" BY " "
                  DISPLAY THREE_STRING, " " NO ADVANCING
            END-IF.
       TEAM-TOTAL-SCORE-PROC.
            *> DISPLAY "TEAM-TOTAL-SCORE-PROC".

            DISPLAY "T:" NO ADVANCING.

            IF WS-ALL-PROBLEMS-SCORE = 0 THEN
                  DISPLAY "   0"
            END-IF.
            IF WS-ALL-PROBLEMS-SCORE > 0 THEN
                  MOVE WS-ALL-PROBLEMS-SCORE TO FOUR_STRING
                  INSPECT FOUR_STRING REPLACING LEADING "0" BY " "
                  DISPLAY FOUR_STRING, " "
            END-IF.
            
       END PROGRAM YOUR-PROGRAM-NAME.
