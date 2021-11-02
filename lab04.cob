       IDENTIFICATION DIVISION.
       PROGRAM-ID. TOTAL5.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
         SELECT COURSE-FILE ASSIGN TO 'DA-S-COURSE'.
         SELECT PRINT-FILE ASSIGN TO 'UR-S-PRINT'.
       DATA DIVISION.
       FILE SECTION.
       FD COURSE-FILE
         RECORDING MODE IS F
         LABEL RECORDS ARE STANDARD.
       01 EMP-REC PIC X(80).
       FD PRINT-FILE
         RECORDING MODE IS F
         LABEL RECORDS ARE STANDARD.
       01 PRINT-REC PIC X(132).
       WORKING-STORAGE SECTION.
       01 MISC.
         03 EOF         PIC X    VALUE 'N'.
           88 END-OF-DATA        VALUE 'Y'.
         03 LINE-CT     PIC 99   VALUE 0.
         03 PAGE-CT     PIC 9999 VALUE '0001'.
         03 C-TAKEN     PIC 9999 VALUE '0000'.
         03 LIMITTOT    PIC 9999 VALUE '0000'.
         03 OPENTOT     PIC 9999 VALUE '0000'.
         03 TAKENTOT    PIC 9999 VALUE '0000'.
      ******************************************************************
      * DESCRIPTION OF INPUT DATA LAYOUT ***
      ******************************************************************
       01 COURSE-DATA.
         03 C-COURSE.
           05 C-ABB     PIC XXX.
           05 C-NUMB    PIC XXXX.
           05 C-SEC     PIC XXX.
         03 C-TITLE     PIC X(20).
         03 C-SEATS-REMAINING   PIC S999.
         03 C-CLASSLIMIT        PIC 999.
         03 FILLER      PIC XXX.
         03 C-STARTING-TIME.
           05 C-STARTING-HOUR   PIC 99.
           05 C-STARTING-MIN    PIC 99.
               03 FILLER      PIC XX.
         03 C-DAYS      PIC ZZZZZ9.
         03 C-LOCATION.
           05 C-BUILDING        PIC XX.
           05 C-ROOM    PIC XXX.
         03 FILLER      PIC X(24).
      ******************************************************************
      * *** DESCRIPTION OF HEADING PRINT LINES *** ***
      ******************************************************************
       01 HEADING1.
         03 FILLER      PIC X(30)       VALUE SPACES.
         03 FILLER      PIC X(27)   VALUE 'EASTERN ILLINOIS UNIVERSITY'.
         03 FILLER      PIC X(20)       VALUE SPACES.
         03 FILLER      PIC X(4)        VALUE 'PAGE'.
         03 HL-PAGE-CT  PIC ZZZ9.
       01 HEADING2.
         03 FILLER      PIC X(39)       VALUE SPACES.
         03 FILLER      PIC X(14)       VALUE 'COURSE LISTING'.
         03 FILLER      PIC X(29)       VALUE SPACES.
       01 HEADING3.
         03 FILLER      PIC X(10)       VALUE SPACES.
         03 FILLER      PIC X(5)        VALUE 'CLASS'.
         03 FILLER      PIC X(11)       VALUE SPACES.
         03 FILLER      PIC X(8)        VALUE 'LOCATION'.
         03 FILLER      PIC X(8)        VALUE SPACES.
         03 FILLER      PIC X(4)        VALUE 'DAYS'.
         03 FILLER      PIC X(11)       VALUE SPACES.
         03 FILLER      PIC X(4)        VALUE 'TIME'.
         03 FILLER      PIC X(10)       VALUE SPACES.
         03 FILLER      PIC X(5)        VALUE 'CLASS'.
         03 FILLER      PIC X(7)        VALUE SPACES.
         03 FILLER      PIC XXXX        VALUE 'OPEN'.
         03 FILLER      PIC X(7)        VALUE SPACES.
         03 FILLER      PIC X(5)        VALUE 'TAKEN'.
       01 HEADING4.
         03 FILLER      PIC X(71)       VALUE SPACES.
         03 FILLER      PIC X(5)        VALUE 'LIMIT'.
         03 FILLER      PIC X(7)        VALUE SPACES.
         03 FILLER      PIC X(5)        VALUE 'SEATS'.
      ******************************************************************
      * DESCRIPTION OF PRINT DATA LAYOUT ***
      ******************************************************************
       01 PRINT-DATA.
         03 FILLER      PIC X(10)       VALUE SPACES.
         03 PABB        PIC XXX.
         03 FILLER      PIC X           VALUE SPACES.
         03 PNUMB       PIC XXXX.
         03 FILLER      PIC X           VALUE SPACES.
         03 PSEC        PIC XXX.
         03 FILLER      PIC X(4)        VALUE SPACES.
         03 PBUILDING   PIC XX.
         03 FILLER      PIC X           VALUE SPACES.
         03 PROOM       PIC XXX.
         03 FILLER      PIC X(10)       VALUE SPACES.
         03 PDAYS       PIC X(5).
         03 FILLER      PIC X(10)        VALUE SPACES.
         03 PSTARTING-HOUR      PIC Z9.
         03 FILLER      PIC X           VALUE ':'.
         03 PSTARTING-MIN       PIC 99.
         03 FILLER      PIC X(8)        VALUE SPACES.
         03 PCLASSLIMIT PIC ZZ9.
         03 FILLER      PIC X(9)        VALUE SPACES.
         03 PSEATS-REMAINING    PIC ZZ9-.
         03 FILLER      PIC X(8)        VALUE SPACES.
         03 PTAKEN      PIC ZZ9.
      *****************************************************************
      * FINAL PAGE
      *****************************************************************
       01 PRINT-FINAL.
         03 FILLER      PIC X(10)       VALUE SPACES.
         03 FILLER      PIC X(11)       VALUE 'GRAND TOTAL'.
         03 FILLER      PIC X(48)       VALUE SPACES.
         03 PLIMITTOT   PIC ZZZ9.
         03 FILLER      PIC X(8)        VALUE SPACES.
         03 POPENTOT    PIC ZZZ9.
         03 FILLER      PIC X(8)        VALUE SPACES.
         03 PTAKENTOT   PIC ZZZ9.

       PROCEDURE DIVISION.
       000-MAINLINE.
           OPEN INPUT COURSE-FILE
             OUTPUT PRINT-FILE.
           PERFORM 800-READ-COURSE-FILE.
           PERFORM 225-COURSE-HEADINGS.
           PERFORM 100-PROCESS-LOOP
             UNTIL END-OF-DATA.
           PERFORM 400-PRINT-FINAL.
           CLOSE COURSE-FILE
             PRINT-FILE.
           STOP RUN.
      *****************************************************************
      * CALCULATIONS
      ******************************************************************
       300-CALC-TAKEN.
          SUBTRACT C-SEATS-REMAINING FROM C-CLASSLIMIT
            GIVING C-TAKEN.
       310-CALC-TOTAL-LIMIT.
          ADD C-CLASSLIMIT TO LIMITTOT.
       320-CALC-TOTAL-OPEN.
          ADD C-SEATS-REMAINING TO OPENTOT.
       330-CALC-TOTAL-TAKEN.
          ADD C-TAKEN TO TAKENTOT.
      ******************************************************************
      * PRINT EACH CLASS ***
      ******************************************************************
       100-PROCESS-LOOP.
          PERFORM 300-CALC-TAKEN.
          PERFORM 310-CALC-TOTAL-LIMIT.
          PERFORM 320-CALC-TOTAL-OPEN.
          PERFORM 330-CALC-TOTAL-TAKEN.
          IF LINE-CT > 45
            THEN
            PERFORM 225-COURSE-HEADINGS.
          MOVE C-ABB TO PABB.
          MOVE C-NUMB TO PNUMB.
          MOVE C-SEC TO PSEC.
          MOVE C-BUILDING TO PBUILDING.
          MOVE C-ROOM TO PROOM.
          MOVE C-DAYS TO PDAYS.
          INSPECT PDAYS REPLACING ALL ' ' BY '-'.
          MOVE C-STARTING-HOUR TO PSTARTING-HOUR.
          MOVE C-STARTING-MIN TO PSTARTING-MIN.
          MOVE C-SEATS-REMAINING TO PSEATS-REMAINING.
          MOVE C-CLASSLIMIT TO PCLASSLIMIT.
          MOVE C-TAKEN TO PTAKEN.
          WRITE PRINT-REC FROM PRINT-DATA
            AFTER ADVANCING 1 LINE.
          ADD 1 TO LINE-CT.
          PERFORM 800-READ-COURSE-FILE.
     ******************************************************************
      * PRINTS HEADING LINE ***
      ******************************************************************
       225-COURSE-HEADINGS.
         MOVE PAGE-CT TO HL-PAGE-CT.
         WRITE PRINT-REC FROM HEADING1
           AFTER ADVANCING PAGE.
         WRITE PRINT-REC FROM HEADING2
           AFTER ADVANCING 1.
         WRITE PRINT-REC FROM HEADING3
           AFTER ADVANCING 1.
         WRITE PRINT-REC FROM HEADING4
           AFTER ADVANCING 1.
         MOVE SPACES TO PRINT-REC.
         WRITE PRINT-REC
           AFTER ADVANCING 1.
         MOVE 0 TO LINE-CT.
           ADD 1 TO PAGE-CT.
      ******************************************************************
      * PRINTS FINAL PAGE
      ******************************************************************
       400-PRINT-FINAL.
          MOVE LIMITTOT TO PLIMITTOT.
          MOVE OPENTOT TO POPENTOT.
          MOVE TAKENTOT TO PTAKENTOT.
            WRITE PRINT-REC FROM PRINT-FINAL
              AFTER ADVANCING 1 LINE.
      ******************************************************************
      * READS THE DATA FILE ***
      ******************************************************************
       800-READ-COURSE-FILE.
         READ COURSE-FILE INTO COURSE-DATA
           AT END MOVE 'Y' TO EOF.
