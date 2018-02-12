       identification division.
       program-id. CBLAMB00.
	   AUTHOR.    Aleesia Beary
	   DATE-WRITTEN. 11/27/2017

************************************************************************
***********
************************************************************************

       environment division.
	   INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-MASTER
			   ASSIGN TO 'C:\COBOL\STDNTMST.DAT'
	           ORGANIZATION IS LINE SEQUENTIAL.
		   SELECT PRTOUT
			   ASSIGN TO 'C:\COBOL\STDTRPT.PRT'
			   ORGANIZATION IS RECORD SEQUENTIAL.

       data division.
	   FILE SECTION.


	   FD  STUDENT-MASTER
		   LABEL RECORD IS STANDARD
		   DATA RECORD IS I-REC
	       RECORD CONTAINS 49 CHARACTERS.
		   01 I-REC.
			   05 I-ID                    PIC X(7).
			   05 I-NAME.
				   10 I-LNAME             PIC X(15).
				   10 I-FNAME             PIC X(15).
				   10 I-INIT              PIC X.
			   05 I-GPA                   PIC 9V99.
		       05 I-EX-STRT-SAL           PIC 9(6)V99.
               

	   FD  PRTOUT
		   LABEL RECORD IS OMITTED
		   RECORD CONTAINS 132 CHARACTERS
		   DATA RECORD IS PRTLINE
		   LINAGE IS 60 WITH FOOTING AT 56.
		   01 PRTLINE                     PIC X(132).

       working-storage section.
       01  MISC.
           05  EOF                   PIC X        VALUE 'F'.
           05  CURRENT-DATE-AND-TIME.
               10  CURRENT-YEAR      PIC X(4).
               10  CURRENT-MONTH     PIC XX.
               10  CURRENT-DAY       PIC XX.
               10  CURRENT-TIME      PIC X(11).
           05  C-PCTR                PIC 99       VALUE 0.
           05  C-STUCTR              PIC 99       VALUE 0.
           05  SALARY-TAX            PIC 9(9)V99  VALUE 0.
           05  FINAL-SAL             PIC 9(7)V99  VALUE 0.
       01  HEADING1.
           05  FILLER               PIC X(6)     VALUE 'DATE: '.
           05  H1-DATE.
               10  H1-MONTH         PIC 99.
               10  FILLER           PIC X        VALUE '/'.
               10  H1-DAY           PIC 99.
               10  FILLER           PIC X        VALUE '/'.
               10  H1-YEAR          PIC 9999.
           05  FILLER               PIC X(35)    VALUE SPACES.
           05  FILLER               PIC X(29)
                   VALUE 'WILSON S COBOL STUDENT ROSTER'.
           05  FILLER               PIC X(44)    VALUE SPACES.
           05  FILLER               PIC X(6)     VALUE 'PAGES: '.
           05  HI-PAGE              PIC Z9.
       01  COL-HEADING1.
           05  FILLER               PIC X(119)   VALUE SPACES.
           05  FILLER               PIC X(11)    VALUE 'ANTICIPATED'.
           05  FILLER               PIC X(2)     VALUE SPACES.
       01  COL-HEADING2.
           05  FILLER               PIC X(2)     VALUE SPACES.
           05  FILLER               PIC XX       VALUE 'ID'.
           05  FILLER               PIC X(23)    VALUE SPACES.
           05  FILLER               PIC X(9)     VALUE 'LAST NAME'.
           05  FILLER               PIC X(26)    VALUE SPACES.
           05  FILLER               PIC X(10)    VALUE 'FIRST NAME'.
           05  FILLER               PIC X(26)    VALUE SPACES.
           05  FILLER               PIC X(3)     VALUE 'GPA'.
           05  FILLER               PIC X(16)    VALUE SPACES.
           05  FILLER               PIC X(15)
                   VALUE 'STARTING SALARY'.
       01  DETAIL-LINE.
           05  O-ID                 PIC X(7).
           05  FILLER               PIC X(20)    VALUE SPACES.
           05  O-LAST-NAME          PIC X(15).
           05  FILLER               PIC X(20)    VALUE SPACES.
           05  O-FIRST-NAME         PIC X(15).
           05  FILLER               PIC X(20)    VALUE SPACES.
           05  O-GPA                PIC Z.99.
           05  FILLER               PIC X(18)    VALUE SPACES.
           05  O-SALARY             PIC $ZZZ,ZZZ.99.
       01  TOTAL-LINE.
           05  FILLER               PIC X(54)    VALUE SPACES.
           05  FILLER               PIC X(15)    VALUE 'STUDENT COUNT:'.
           05  TOT-STUDENT-COUNT    PIC ZZ9.

       procedure division.
       L1-MAIN.
           PERFORM L2-INIT.
           PERFORM L2-MAINLINE
               UNTIL EOF = 'T'.
           PERFORM L2-CLOSING.
           STOP RUN.

       L2-INIT.
           MOVE FUNCTION current-date      TO CURRENT-DATE-AND-TIME.
           MOVE CURRENT-DAY                TO H1-DAY.
           MOVE CURRENT-MONTH              TO H1-MONTH.
           MOVE CURRENT-YEAR               TO H1-YEAR.
           OPEN INPUT STUDENT-MASTER.
           OPEN OUTPUT PRTOUT.
           PERFORM L4-HEADINGS.
           PERFORM L3-READ.

       L2-MAINLINE.
           PERFORM L3-CALCS.
           PERFORM L3-MOVES.
           PERFORM L3-READ.

       L2-CLOSING.
           MOVE C-STUCTR TO TOT-STUDENT-COUNT.
           WRITE PRTLINE FROM TOTAL-LINE
               AFTER ADVANCING 3 lines.
           CLOSE STUDENT-MASTER.
           CLOSE PRTOUT.

       L3-READ.
           READ STUDENT-MASTER
               AT end
                   MOVE 'T' TO EOF.

       L3-CALCS.
           ADD 1 TO C-STUCTR.
           COMPUTE SALARY-TAX = I-EX-STRT-SAL * .25.
           COMPUTE FINAL-SAL = I-EX-STRT-SAL - SALARY-TAX.

       L3-MOVES.
           MOVE I-ID           TO O-ID.
           MOVE I-LNAME        TO O-LAST-NAME.
           MOVE I-FNAME        TO O-FIRST-NAME.
           MOVE I-GPA          TO O-GPA.
           MOVE FINAL-SAL      TO O-SALARY.
           WRITE PRTLINE FROM DETAIL-LINE
               AFTER ADVANCING 2 lines
                   AT eop
                       PERFORM L4-HEADINGS.

       L4-HEADINGS.
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO HI-PAGE.
           WRITE PRTLINE FROM HEADING1
               AFTER ADVANCING page.
           WRITE PRTLINE FROM COL-HEADING1
               AFTER ADVANCING 2 lines. 
           WRITE PRTLINE FROM COL-HEADING2
               AFTER ADVANCING 1 LINE.
           
       end program CBLAMB00.
