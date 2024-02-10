       IDENTIFICATION DIVISION.
       PROGRAM-ID. BBC-ADMIN.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *>      FILE HANDLING
           SELECT CustomerFile ASSIGN TO "D:\cobol programs\account.txt"
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS PIN.

       DATA DIVISION.
       FILE SECTION.
      *>      VARIABLES FOR FILE HANDLING
       FD CustomerFile.
       01 CUST-DATA.
           02 PIN PIC 9(4).
           02 BALANCE PIC 9(6)V9(9).
           02 FIRST-NAME PIC X(15).
           02 LAST-NAME PIC X(15).

       WORKING-STORAGE SECTION.
      *>       VARIABLES USED FOR DISPLAY
       01 WSCUSTDATASTDATA.
           02 WSPIN PIC 9999.
           02 WSBALANCE PIC 9(6)V99.
           02 WSFIRST-NAME PIC Z(15).
           02 WSLAST-NAME PIC Z(15).
      *>      USE TO AUTO GENERATED PIN
       01 WSGEN-PIN PIC 9(4) VALUE 0.
      *>  VARIABLES USED FOR UPDATING ADD-ACCOUNT BALANCE
       01 TEMP PIC 9(6).
       01 DISPBAL PIC -ZZZ,ZZZ.ZZ.
      *>  VARIABLES USE FOR SELECTION AND PAUSE
       01 CHOICE PIC 9.
       01 PAUSE PIC Z.

       PROCEDURE DIVISION.
           OPEN I-O CustomerFile.
      *>      MAIN PARAGRAPH
       MAIN.
           PERFORM DESIGN-BOX.
           DISPLAY "BBC ADMIN" AT 1154.
           DISPLAY "1 - ADD ACCOUNT" AT 1345.
           DISPLAY "2 - DISPLAY ACCOUNT " AT 1445.
           DISPLAY "3 - ADD BALANCE ON ACCOUNT" AT 1545.
           DISPLAY "4 - DEDUCT BALANCE ON ACCOUNT" AT 1645.
           DISPLAY "5 - EXIT " AT 1745.
           DISPLAY "Enter choice: " AT 1951.
           ACCEPT CHOICE AT 1965.

      *>      SELECTION
           IF CHOICE = 1
      *>          ADD OR CREATE AN ACCOUNT
               PERFORM ADD-ACCOUNT
           ELSE IF CHOICE = 2
      *>          DISPLAY A SPECIFIC ACCOUNT INFORMATION
               PERFORM DISPLAY-ACC-DATA
           ELSE IF CHOICE = 3
      *>          ADD BALANCE ON ACCOUNT
               PERFORM ADD-ACC-BALANCE
           ELSE IF CHOICE = 4
      *>          DEDUCT BALANCE ON ACOUNT
               PERFORM DEDUCT-ACC-BALANCE
           ELSE IF CHOICE = 5
      *>          EXIT
               CLOSE CustomerFile
               STOP RUN
           ELSE
      *>          INPUT VALIDATION
               DISPLAY "Invalid input! please retry..."AT 1942
               ACCEPT PAUSE AT 1977
               DISPLAY " " ERASE SCREEN
               PERFORM MAIN
           END-IF.

       DESIGN-BOX.
      *>      UPPER AND LOWER DESIGN
           DISPLAY "--------------------------------------" AT 0940.
           DISPLAY "--------------------------------------" AT 2240.
           DISPLAY "------------------------------------------" AT 0838.
           DISPLAY "------------------------------------------" AT 2338.
           DISPLAY "----------------------------------------------"
           AT 0736.
           DISPLAY "----------------------------------------------"
           AT 2436.
           DISPLAY "--------------------------------------------------"
           AT 0634.
           DISPLAY "--------------------------------------------------"
           AT 2534.
         *>    LEFT  SIDE DESIGN
           DISPLAY "--" AT 1037.
           DISPLAY "----" AT 1135.
           DISPLAY "------" AT 1233.
           DISPLAY "--------" AT 1331.
           DISPLAY "-----------" AT 1428.
           DISPLAY "--------------" AT 1525.
           DISPLAY "--------------" AT 1625.
           DISPLAY "-----------" AT 1728.
           DISPLAY "--------" AT 1831.
           DISPLAY "------" AT 1933.
           DISPLAY "----" AT 2035.
           DISPLAY "--" AT 2137.

      *>    RIGHT  SIDE DESIGN
           DISPLAY "--" AT 1079.
           DISPLAY "----" AT 1179.
           DISPLAY "------" AT 1279.
           DISPLAY "--------" AT 1379.
           DISPLAY "-----------" AT 1479.
           DISPLAY "--------------" AT 1579.
           DISPLAY "--------------" AT 1679.
           DISPLAY "-----------" AT 1779.
           DISPLAY "--------" AT 1879.
           DISPLAY "------" AT 1979.
           DISPLAY "----" AT 2079.
           DISPLAY "--" AT 2179.
           EXIT.

      *>      PARAGRAPH FOR ACCOUNT CREATION
       ADD-ACCOUNT.
           DISPLAY " " ERASE SCREEN.
           PERFORM DESIGN-BOX.
           DISPLAY "ADD NEW ACCOUNT" AT 1150.
           DISPLAY "Enter the following:" AT 1350.
      *>      INPUTS
           DISPLAY "First name:" AT 1450.
           ACCEPT FIRST-NAME AT 1463.
           DISPLAY "Last name:" AT 1550.
           ACCEPT LAST-NAME AT 1563.
           DISPLAY "Initial balance:" AT 1650.
           DISPLAY "PHP " AT 1755.
           ACCEPT BALANCE AT 1763.

      *>       FOR GENERATION OF PIN
           COMPUTE WSGEN-PIN = FUNCTION RANDOM * (999 + 1) + 999.

           DISPLAY "ACCOUNT GENERATED PIN: " AT 1850
           DISPLAY WSGEN-PIN AT 1963.
           MOVE WSGEN-PIN TO PIN.

           ACCEPT PAUSE AT 1973.
           DISPLAY " " ERASE SCREEN.
      *>   WRITE ON FILE
               WRITE CUST-DATA
           END-WRITE.
           PERFORM MAIN.

      *>      PARAGRAPH FOR DISPLAYING ACCOUNT DATA
       DISPLAY-ACC-DATA.
           DISPLAY " " ERASE SCREEN.
           PERFORM DESIGN-BOX.
           DISPLAY "DISPLAY ACCOUNT DATA" AT 1150.
           DISPLAY "Enter account pin: " AT 1346.
           ACCEPT PIN AT 1364.
           READ CustomerFile
           INVALID KEY
      *>          VALIDATION OF INPUT
               DISPLAY "Account not found. Please retry..." AT 1542
               ACCEPT PAUSE AT 1577
               DISPLAY " " ERASE SCREEN
               PERFORM MAIN
           END-READ.

           MOVE BALANCE TO WSBALANCE
           MOVE WSBALANCE TO DISPBAL
      *>              DISPLAYING DETAILS
              DISPLAY "ACCOUNT DETAILS" AT 1550.
              DISPLAY "Name: " AT 1645.
              DISPLAY FIRST-NAME AT 1652.
              DISPLAY LAST-NAME AT 1664.
              DISPLAY "Balance: " AT 1745.
              DISPLAY DISPBAL AT 1755.
              DISPLAY "PIN: " AT 1845.
              DISPLAY PIN AT 1852.
              ACCEPT PAUSE AT 1856.

           DISPLAY " " ERASE SCREEN.
           PERFORM MAIN.

      *>          PARAGRAPH FOR ADDING BALANCE ON ACCOUNT
       ADD-ACC-BALANCE.
           DISPLAY " " ERASE SCREEN.
           PERFORM DESIGN-BOX.
           DISPLAY "ADD ACCOUNT BALANCE" AT 1150.
           DISPLAY "Enter pin: " AT 1348.
           ACCEPT PIN AT 1363.

           READ CustomerFile
           INVALID KEY
      *>      VALIDATION OF INPUT
               DISPLAY "Account not found. Please retry..." AT 1542
               ACCEPT PAUSE AT 1577
               DISPLAY " " ERASE SCREEN
               PERFORM MAIN
           END-READ.

              DISPLAY "Name: " AT 1445.
              DISPLAY FIRST-NAME AT 1452.
              DISPLAY LAST-NAME AT 1464.
              DISPLAY "Balance: " AT 1545.

      *>         TO DISPLAY BALANCE
              MOVE BALANCE TO WSBALANCE.
              MOVE WSBALANCE TO DISPBAL.

              DISPLAY DISPBAL AT 1560.

      *>          AMOUNT TO ADD
               DISPLAY "Enter amount: " AT 1747.
               ACCEPT TEMP AT 1765.

      *>          PROCESS FOR UPDATING BALANCE
               MOVE DISPBAL TO WSBALANCE.
               COMPUTE WSBALANCE = WSBALANCE + TEMP.
               MOVE WSBALANCE TO BALANCE.
               MOVE BALANCE TO DISPBAL.

               DISPLAY "Balance: " AT 1545.
               DISPLAY DISPBAL AT 1560.
               ACCEPT PAUSE AT 1775.
               DISPLAY " " ERASE SCREEN.

               REWRITE CUST-DATA
               END-REWRITE.
               PERFORM MAIN.

      *>          PARAGRAPH FOR DEDUCTING BALANCE ON ACCOUNT
       DEDUCT-ACC-BALANCE.
             DISPLAY " " ERASE SCREEN.
           PERFORM DESIGN-BOX.
           DISPLAY "ADD ACCOUNT BALANCE" AT 1150.
           DISPLAY "Enter pin: " AT 1348.
           ACCEPT PIN AT 1363.

           READ CustomerFile
           INVALID KEY
               DISPLAY "Account not found. Please retry..." AT 1542
               ACCEPT PAUSE AT 1577
               DISPLAY " " ERASE SCREEN
               PERFORM MAIN
           END-READ.

              DISPLAY "Name: " AT 1445.
              DISPLAY FIRST-NAME AT 1452.
              DISPLAY LAST-NAME AT 1464.
              DISPLAY "Balance: " AT 1545.

      *>         TO DISPLAY BALANCE
              MOVE BALANCE TO WSBALANCE.
              MOVE WSBALANCE TO DISPBAL.

              DISPLAY DISPBAL AT 1560.

      *>          AMOUNT TO DEDUCT
               DISPLAY "Enter amount: " AT 1747.
               ACCEPT TEMP AT 1765.

      *>          VALIDTION OF INPUT
               IF TEMP > BALANCE
               DISPLAY "Invalid amount! please retry..." AT 1942
               ACCEPT PAUSE AT 1977
               DISPLAY " " ERASE SCREEN
                   PERFORM DEDUCT-ACC-BALANCE
               END-IF.

      *>          PROCESS FOR UPDATING BALANCE
               MOVE DISPBAL TO WSBALANCE.
               COMPUTE WSBALANCE = WSBALANCE - TEMP.
               MOVE WSBALANCE TO BALANCE.
               MOVE BALANCE TO DISPBAL.

               DISPLAY "Balance: " AT 1545.
               DISPLAY DISPBAL AT 1560.
               ACCEPT PAUSE AT 1775.
               DISPLAY " " ERASE SCREEN.

               REWRITE CUST-DATA
               END-REWRITE.
               PERFORM MAIN.
      *>       PARAGRAPH FOR DESIGN




           CLOSE CustomerFile.
           STOP RUN.
