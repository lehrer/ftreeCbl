       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAMILYTREE.
       AUTHOR. GPT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TREEFILE ASSIGN TO "familytree.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT EXPORTFILE ASSIGN TO "export.csv"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TEMPFILE ASSIGN TO "temp.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD TREEFILE.
       01 PERSON-RECORD.
           05 ID                 PIC 9(5).
           05 NAME               PIC X(50).
           05 BIRTHDATE          PIC X(10).
           05 ALTNAME            PIC X(50).
           05 DEATHDATE          PIC X(10).
           05 BURIALPLACE        PIC X(50).
           05 RESIDENCE          PIC X(50).
           05 SPOUSE-ID          PIC 9(5).
           05 FATHER-ID          PIC 9(5).
           05 MOTHER-ID          PIC 9(5).

       FD EXPORTFILE.
       01 EXPORT-RECORD          PIC X(300).

       FD TEMPFILE.
       01 TEMP-RECORD.
           05 T-ID               PIC 9(5).
           05 T-NAME             PIC X(50).
           05 T-BIRTHDATE        PIC X(10).
           05 T-ALTNAME          PIC X(50).
           05 T-DEATHDATE        PIC X(10).
           05 T-BURIALPLACE      PIC X(50).
           05 T-RESIDENCE        PIC X(50).
           05 T-SPOUSE-ID        PIC 9(5).
           05 T-FATHER-ID        PIC 9(5).
           05 T-MOTHER-ID        PIC 9(5).

       WORKING-STORAGE SECTION.
       01 WS-MENU-OPTION         PIC 9.
       01 WS-END-FLAG            PIC X VALUE 'N'.
       01 WS-ID-COUNTER          PIC 9(5) VALUE 1.
       01 WS-LINE                PIC X(300).

       01 TEMP-PERSON.
           05 TP-ID              PIC 9(5).
           05 TP-NAME            PIC X(50).
           05 TP-BIRTH           PIC X(10).
           05 TP-ALTNAME         PIC X(50).
           05 TP-DEATH           PIC X(10).
           05 TP-BURIAL          PIC X(50).
           05 TP-RESIDENCE       PIC X(50).
           05 TP-SPOUSE-ID       PIC 9(5).
           05 TP-FATHER-ID       PIC 9(5).
           05 TP-MOTHER-ID       PIC 9(5).

       01 EOF-TREEFILE PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-LOOP.
           PERFORM LOAD-TREE
           PERFORM UNTIL WS-END-FLAG = 'Y'
               DISPLAY "\nFAMILY TREE MENU:"
               DISPLAY "1. Add Person"
               DISPLAY "2. View All Records"
               DISPLAY "3. Edit Person"
               DISPLAY "4. Save Tree"
               DISPLAY "5. View Person with Family"
               DISPLAY "6. Exit"
               DISPLAY "7. Export Tree to CSV"
               ACCEPT WS-MENU-OPTION
               EVALUATE WS-MENU-OPTION
                   WHEN 1
                       PERFORM ADD-PERSON
                   WHEN 2
                       PERFORM VIEW-ALL
                   WHEN 3
                       PERFORM EDIT-PERSON
                   WHEN 4
                       DISPLAY "Tree saved automatically on adding."
                   WHEN 5
                       PERFORM VIEW-FAMILY
                   WHEN 6
                       MOVE 'Y' TO WS-END-FLAG
                   WHEN 7
                       PERFORM EXPORT-TREE
                   WHEN OTHER
                       DISPLAY "Invalid option."
               END-EVALUATE
           END-PERFORM
           STOP RUN.

       ADD-PERSON.
           * Existing logic remains unchanged...
           [existing ADD-PERSON logic here]

       VIEW-ALL.
           * Existing logic remains unchanged...
           [existing VIEW-ALL logic here]

       LOAD-TREE.
           * Existing logic remains unchanged...
           [existing LOAD-TREE logic here]

       EDIT-PERSON.
           DISPLAY "Enter ID of person to edit:"
           ACCEPT TP-ID
           OPEN INPUT TREEFILE
           OPEN OUTPUT TEMPFILE
           MOVE 'N' TO EOF-TREEFILE
           PERFORM UNTIL EOF-TREEFILE = 'Y'
               READ TREEFILE INTO PERSON-RECORD
                   AT END
                       MOVE 'Y' TO EOF-TREEFILE
                   NOT AT END
                       IF ID = TP-ID
                           DISPLAY "Enter new name (leave blank to keep
      -                            "current):"
                           ACCEPT TP-NAME
                           IF TP-NAME = SPACES
                               MOVE NAME TO TP-NAME
                           END-IF
                           DISPLAY "Enter new birth date (YYYY-MM-DD):"
                           ACCEPT TP-BIRTH
                           IF TP-BIRTH = SPACES
                               MOVE BIRTHDATE TO TP-BIRTH
                           END-IF
                           DISPLAY "Enter new alternate name:"
                           ACCEPT TP-ALTNAME
                           IF TP-ALTNAME = SPACES
                               MOVE ALTNAME TO TP-ALTNAME
                           END-IF
                           DISPLAY "Enter new death date (YYYY-MM-DD):"
                           ACCEPT TP-DEATH
                           IF TP-DEATH = SPACES
                               MOVE DEATHDATE TO TP-DEATH
                           END-IF
                           DISPLAY "Enter new burial place:"
                           ACCEPT TP-BURIAL
                           IF TP-BURIAL = SPACES
                               MOVE BURIALPLACE TO TP-BURIAL
                           END-IF
                           DISPLAY "Enter new residence:"
                           ACCEPT TP-RESIDENCE
                           IF TP-RESIDENCE = SPACES
                               MOVE RESIDENCE TO TP-RESIDENCE
                           END-IF
                           DISPLAY "Enter new spouse ID (or 0):"
                           ACCEPT TP-SPOUSE-ID
                           DISPLAY "Enter new father ID (or 0):"
                           ACCEPT TP-FATHER-ID
                           DISPLAY "Enter new mother ID (or 0):"
                           ACCEPT TP-MOTHER-ID

                           MOVE TP-ID         TO T-ID
                           MOVE TP-NAME       TO T-NAME
                           MOVE TP-BIRTH      TO T-BIRTHDATE
                           MOVE TP-ALTNAME    TO T-ALTNAME
                           MOVE TP-DEATH      TO T-DEATHDATE
                           MOVE TP-BURIAL     TO T-BURIALPLACE
                           MOVE TP-RESIDENCE  TO T-RESIDENCE
                           MOVE TP-SPOUSE-ID  TO T-SPOUSE-ID
                           MOVE TP-FATHER-ID  TO T-FATHER-ID
                           MOVE TP-MOTHER-ID  TO T-MOTHER-ID
                       ELSE
                           MOVE ID            TO T-ID
                           MOVE NAME          TO T-NAME
                           MOVE BIRTHDATE     TO T-BIRTHDATE
                           MOVE ALTNAME       TO T-ALTNAME
                           MOVE DEATHDATE     TO T-DEATHDATE
                           MOVE BURIALPLACE   TO T-BURIALPLACE
                           MOVE RESIDENCE     TO T-RESIDENCE
                           MOVE SPOUSE-ID     TO T-SPOUSE-ID
                           MOVE FATHER-ID     TO T-FATHER-ID
                           MOVE MOTHER-ID     TO T-MOTHER-ID
                       END-IF
                       WRITE TEMP-RECORD
               END-READ
           END-PERFORM
           CLOSE TREEFILE
           CLOSE TEMPFILE
           CALL "CBL_DELETE_FILE" USING "familytree.dat"
           CALL "CBL_RENAME_FILE" USING "temp.dat" "familytree.dat"
           DISPLAY "Person updated successfully."
           .

       VIEW-FAMILY.
           * Existing logic remains unchanged...
           [existing VIEW-FAMILY logic here]

       DISPLAY-FAMILY-MEMBER USING BY VALUE TP-ID, RELATION.
           * Existing logic remains unchanged...
           [existing DISPLAY-FAMILY-MEMBER logic here]

       EXPORT-TREE.
           DISPLAY "Exporting tree to export.csv..."
           OPEN INPUT TREEFILE
           OPEN OUTPUT EXPORTFILE
           MOVE "ID,Name,Birthdate,AltName,DeathDate,BurialPlace,Residen
      -    "ce,SpouseID,FatherID,MotherID" TO EXPORT-RECORD
           WRITE EXPORT-RECORD
           MOVE 'N' TO EOF-TREEFILE
           PERFORM UNTIL EOF-TREEFILE = 'Y'
               READ TREEFILE INTO PERSON-RECORD
                   AT END
                       MOVE 'Y' TO EOF-TREEFILE
                   NOT AT END
                       STRING
                           ID DELIMITED BY SIZE ","
                           NAME DELIMITED BY SIZE ","
                           BIRTHDATE DELIMITED BY SIZE ","
                           ALTNAME DELIMITED BY SIZE ","
                           DEATHDATE DELIMITED BY SIZE ","
                           BURIALPLACE DELIMITED BY SIZE ","
                           RESIDENCE DELIMITED BY SIZE ","
                           SPOUSE-ID DELIMITED BY SIZE ","
                           FATHER-ID DELIMITED BY SIZE ","
                           MOTHER-ID DELIMITED BY SIZE
                           INTO EXPORT-RECORD
                       END-STRING
                       WRITE EXPORT-RECORD
               END-READ
           END-PERFORM
           CLOSE TREEFILE
           CLOSE EXPORTFILE
           DISPLAY "Export complete."
           .
