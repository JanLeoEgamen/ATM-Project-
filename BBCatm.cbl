      ******************************************************************
      * Author: GROUP 7
      * Date: JANUARY
      * Purpose: PROJECT IN COMPUTER PROGRAMMING 3
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BBC-ATM.
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

      *     BALANCE VARIABLES
       01 PHP-BALANCE PIC 9(6)V9(9).
       01 USD-BALANCE PIC 9(6)V9(9).
       01 JPY-BALANCE PIC 9(6)V9(9).
       01 INR-BALANCE PIC 9(6)V9(9).

      *     VARIABLES FOR DISPLAYING BALANCES
       01 PHP-DISPLAY-BAL PIC -Z,ZZZ,ZZ9.99.
       01 USD-DISPLAY-BAL PIC -Z,ZZZ,ZZ9.99.
       01 JPY-DISPLAY-BAL PIC -Z,ZZZ,ZZ9.99.
       01 INR-DISPLAY-BAL PIC -Z,ZZZ,ZZ9.99.

      *     VARIABLES FOR SELECTION.
       01 MENU-CHOICE PIC 9.
       01 SUBMENU-CHOICE PIC 9.
       01 TEMP PIC X.

      *     VARIABLES FOR WIDTHRAW
       01 WIDTHRAW-AMOUNT PIC 9(5).

       PROCEDURE DIVISION.
      *     MAIN PARAGRAPH
       OPEN I-O CustomerFile.
       MAIN.
      *     FOR PIN
           PERFORM DESIGN-BOX.
           DISPLAY "Welcome to Big Black Card!" AT 1446.
           DISPLAY "Enter PIN: " AT 1751.

           ACCEPT PIN AT 1762 NO-ECHO.
           DISPLAY " " ERASE SCREEN.

          READ CustomerFile
           INVALID KEY
      *>          VALIDATION OF INPUT
               DISPLAY "Account not found. Please retry..." AT 1545
               ACCEPT TEMP AT 1580
               DISPLAY " " ERASE SCREEN
               PERFORM MAIN
           END-READ.

      *     PARAGRAPH FOR MAIN MENU
       BBC-MAINMENU.
           PERFORM DESIGN-BOX.
           DISPLAY "MAIN MENU" AT 1154.
           DISPLAY "1 - Check Balance" AT 1350.
           DISPLAY "2 - Withdraw"AT 1450.
           DISPLAY "3 - Exit "AT 1550.
           DISPLAY "Enter choice: "AT 1751.
           ACCEPT MENU-CHOICE AT 1765.

      *    CONDITION STATEMENT FOR MAIN MENU
           IF MENU-CHOICE = 1
      *        BALANCE
               DISPLAY " " ERASE SCREEN
               PERFORM BALANCE-CONVERISON

           ELSE IF MENU-CHOICE = 2
      *        WITHDRAW
               DISPLAY " " ERASE SCREEN
               PERFORM WITHDRAW

           ELSE IF MENU-CHOICE = 3
      *         EXIT
               DISPLAY " " ERASE SCREEN
               DISPLAY "THANK YOU FOR USING OUR SERVICE!" AT 1345
               DISPLAY "MAS MAGANDA MAG CASH OUT PAG MALAKI" AT 1544
               DISPLAY "YOUR ONE AND ONLY BBC - BIG BLACK CARD" AT 1742
               ACCEPT TEMP AT 1785
               CLOSE CustomerFile
               STOP RUN
           ELSE
      *        EXCEPTION HANDLING
               DISPLAY "Invalid choice! please try again..." AT 1950
               ACCEPT TEMP AT 1985
               DISPLAY " " ERASE SCREEN
               PERFORM BBC-MAINMENU
           END-IF.

      *       Computation for conversion of currency
       BALANCE-CONVERISON.
            MOVE BALANCE TO PHP-BALANCE
            COMPUTE USD-BALANCE = PHP-BALANCE * 0.01775228.
            COMPUTE JPY-BALANCE = PHP-BALANCE * 2.6221674.
            COMPUTE INR-BALANCE = PHP-BALANCE * 1.4754944.
      *    STORING THE VALUE FOR DISPLAYING
            MOVE USD-BALANCE TO USD-DISPLAY-BAL.
            MOVE INR-BALANCE TO INR-DISPLAY-BAL.
            MOVE JPY-BALANCE TO JPY-DISPLAY-BAL.
            MOVE PHP-BALANCE TO PHP-DISPLAY-BAL.
            PERFORM BALANCE-DISPLAY.

      *     PARAGRAPH FOR UPDATING BALANCE
       UPDATE-BALANCE.
           MOVE BALANCE TO PHP-BALANCE
           COMPUTE USD-BALANCE = PHP-BALANCE * 0.01775228.
           COMPUTE JPY-BALANCE = PHP-BALANCE * 2.6221674.
           COMPUTE INR-BALANCE = PHP-BALANCE * 1.4754944.

           MOVE USD-BALANCE TO USD-DISPLAY-BAL.
           MOVE INR-BALANCE TO INR-DISPLAY-BAL.
           MOVE JPY-BALANCE TO JPY-DISPLAY-BAL.
           MOVE PHP-BALANCE TO PHP-DISPLAY-BAL.

      *    PARAGRAPH FOR DISPLAY OF BALANCE
       BALANCE-DISPLAY.
           PERFORM DESIGN-BOX.
           DISPLAY "BALANCE" AT 1154.
           DISPLAY "Account Balance:   PHP" AT 1340
           DISPLAY PHP-DISPLAY-BAL AT 1362.
           DISPLAY "In other currency: " AT 1540.
           DISPLAY "USD"  AT 1659.
           DISPLAY USD-DISPLAY-BAL AT 1662.
           DISPLAY "INR" AT 1759.
           DISPLAY INR-DISPLAY-BAL AT 1762.
           DISPLAY "JPY" AT 1859.
           DISPLAY JPY-DISPLAY-BAL AT 1862.

           DISPLAY "Press Enter to continue..." AT 2040.
           ACCEPT TEMP AT 2066.

           DISPLAY " " ERASE SCREEN.
           PERFORM BBC-MAINMENU.

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



      *     PARAGRAPH FOR WITHDRAWAL
       WITHDRAW.
           PERFORM DESIGN-BOX.
           PERFORM UPDATE-BALANCE.
           DISPLAY "WITHRAWAL" AT 1154.
           DISPLAY "Account Balance: PHP" AT 1340
           DISPLAY PHP-DISPLAY-BAL AT 1362.
           DISPLAY "Enter amount: " AT 1540.
           ACCEPT WIDTHRAW-AMOUNT AT 1555.

      *        EXCEPTION HANDLING
           IF WIDTHRAW-AMOUNT > PHP-BALANCE
               DISPLAY "Invalid amount! please try again..." AT 1740
               ACCEPT TEMP AT 1774
               DISPLAY " " ERASE SCREEN
               PERFORM WITHDRAW
           ELSE
               COMPUTE PHP-BALANCE = PHP-BALANCE - WIDTHRAW-AMOUNT
               MOVE PHP-BALANCE TO BALANCE
               REWRITE CUST-DATA
               END-REWRITE
               PERFORM UPDATE-BALANCE
               DISPLAY "Account Balance: PHP" AT 1340
               DISPLAY PHP-DISPLAY-BAL AT 1362
               DISPLAY "Successful transaction! Press enter.." AT 1740
               ACCEPT TEMP AT 1777
               DISPLAY " " ERASE SCREEN
               PERFORM BBC-MAINMENU
           END-IF.
           CLOSE CustomerFile.
            STOP RUN.
       END PROGRAM BBC-ATM.
