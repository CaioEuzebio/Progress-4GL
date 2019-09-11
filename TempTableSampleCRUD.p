/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cAction AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE tt-usuario
    FIELD nome      AS CHARACTER 
    FIELD sobrenome AS CHARACTER
    FIELD cpf AS CHARACTER FORMAT "999-999-999-99"
    INDEX idxNomeSobreNome IS PRIMARY nome sobrenome
    INDEX idxcpf IS UNIQUE cpf.



/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brUsuario

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-usuario

/* Definitions for BROWSE brUsuario                                     */
&Scoped-define FIELDS-IN-QUERY-brUsuario tt-usuario.nome tt-usuario.sobrenome tt-usuario.cpf   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brUsuario   
&Scoped-define SELF-NAME brUsuario
&Scoped-define QUERY-STRING-brUsuario FOR EACH tt-usuario
&Scoped-define OPEN-QUERY-brUsuario OPEN QUERY {&SELF-NAME} FOR EACH tt-usuario.
&Scoped-define TABLES-IN-QUERY-brUsuario tt-usuario
&Scoped-define FIRST-TABLE-IN-QUERY-brUsuario tt-usuario


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brUsuario}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTCADAST BTLIST BTALTERAR brUsuario BTAPAGAR 
&Scoped-Define DISPLAYED-OBJECTS FILLNOME FILLSOBRENOME FILLCPF 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTALTERAR 
     LABEL "Alterar" 
     SIZE 20 BY 1.14.

DEFINE BUTTON BTAPAGAR 
     LABEL "Apagar" 
     SIZE 20 BY 1.

DEFINE BUTTON BTCADAST 
     LABEL "Cadastrar" 
     SIZE 20 BY 1.14.

DEFINE BUTTON btCancel 
     LABEL "Cancelar" 
     SIZE 20 BY .95.

DEFINE BUTTON BTLIST 
     LABEL "Listar" 
     SIZE 20 BY 1.14.

DEFINE BUTTON btSave 
     LABEL "Salvar" 
     SIZE 20 BY 1.14.

DEFINE VARIABLE FILLCPF AS CHARACTER FORMAT "X(3).X(3).X(3)-X(2)":U 
     LABEL "CPF" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FILLNOME AS CHARACTER FORMAT "X(25)":U 
     LABEL "Nome" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FILLSOBRENOME AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sobrenome" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

/* Query definitions                                                    */
DEFINE QUERY brUsuario FOR 
      tt-usuario SCROLLING.

/* Browse definitions                                                   */
DEFINE BROWSE brUsuario
  QUERY brUsuario DISPLAY
      tt-usuario.nome
     tt-usuario.sobrenome
     tt-usuario.cpf
    WITH NO-ROW-MARKERS SEPARATORS SIZE 41 BY 5.48 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BTCADAST AT ROW 2 COL 11 WIDGET-ID 2
     FILLNOME AT ROW 2 COL 45 COLON-ALIGNED WIDGET-ID 10
     BTLIST AT ROW 4 COL 11 WIDGET-ID 4
     FILLSOBRENOME AT ROW 4 COL 45 COLON-ALIGNED WIDGET-ID 14
     BTALTERAR AT ROW 6 COL 11 WIDGET-ID 6
     FILLCPF AT ROW 6 COL 45 COLON-ALIGNED WIDGET-ID 12
     brUsuario AT ROW 7.9 COL 36 WIDGET-ID 200
     BTAPAGAR AT ROW 8 COL 11 WIDGET-ID 8
     btSave AT ROW 10.29 COL 11 WIDGET-ID 18
     btCancel AT ROW 12.43 COL 11 WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */

/* *************************  Create Window  ************************** */

IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 17.76
         WIDTH              = 92.8
         MAX-HEIGHT         = 17.76
         MAX-WIDTH          = 92.8
         VIRTUAL-HEIGHT     = 17.76
         VIRTUAL-WIDTH      = 92.8
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Size-to-Fit                                               */
/* BROWSE-TAB brUsuario FILLCPF DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:SCROLLABLE       = FALSE.

/* SETTINGS FOR BUTTON btCancel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btSave IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILLCPF IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILLNOME IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILLSOBRENOME IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.



/* Setting information for Queries and Browse Widgets fields            */

/* Query rebuild information for BROWSE brUsuario
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-usuario.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brUsuario */





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.


ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.


&Scoped-define SELF-NAME BTALTERAR
ON CHOOSE OF BTALTERAR IN FRAME DEFAULT-FRAME /* Alterar */
DO:
  IF NOT AVAILABLE tt-usuario THEN RETURN NO-APPLY.

  ASSIGN fillnome = tt-usuario.nome
         fillsobrenome = tt-usuario.sobrenome
         fillcpf = tt-usuario.cpf.
  DO WITH FRAME {&FRAME-NAME}:
     DISPLAY fillnome fillsobrenome fillcpf .
     ENABLE fillNome fillSobrenome btSave btCancel.
     DISABLE btCadast btList btAlterar btApagar.
  END.
  ASSIGN cAction = 'u'.
END.


&Scoped-define SELF-NAME BTAPAGAR
ON CHOOSE OF BTAPAGAR IN FRAME DEFAULT-FRAME /* Apagar */
DO:
  IF AVAILABLE tt-usuario THEN DO:
     MESSAGE "Deletar?" 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lAns AS LOGICAL.
     IF lAns THEN do:
        DELETE tt-usuario.
        {&open-query-{&browse-name}}
     END.

  END.
END.


&Scoped-define SELF-NAME BTCADAST
ON CHOOSE OF BTCADAST IN FRAME DEFAULT-FRAME /* Cadastrar */
DO:
  DO WITH FRAME {&FRAME-NAME}:
     DISABLE btCadast btAlterar btList btApagar.
     ENABLE fillnome fillsobrenome fillcpf btSave btCancel.
     ASSIGN cAction = 'N'. /* New */
  END.
END.


&Scoped-define SELF-NAME btCancel
ON CHOOSE OF btCancel IN FRAME DEFAULT-FRAME /* Cancelar */
DO:
  RUN clearFields.
  ENABLE btCadast btList btAlterar btApagar WITH FRAME {&FRAME-NAME}.
  DISABLE btSave btCancel fillnome fillsobrenome fillcpf WITH FRAME {&FRAME-NAME}.
END.


&Scoped-define SELF-NAME BTLIST
ON CHOOSE OF BTLIST IN FRAME DEFAULT-FRAME /* Listar */
DO:
  {&open-query-{&browse-name}}
END.


&Scoped-define SELF-NAME btSave
ON CHOOSE OF btSave IN FRAME DEFAULT-FRAME /* Salvar */
DO:
  CASE cAction:
      WHEN 'N' THEN RUN newRecord.
      WHEN 'U' THEN RUN updateRecord.
      OTHERWISE 
         MESSAGE 'Opcao Invalida'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END CASE.
  ASSIGN cAction = ''.
  DO WITH FRAME {&FRAME-NAME}:
     DISABLE fillnome fillsobrenome fillcpf. 
     RUN clearFields.
  END.
  ENABLE btCadast btList btAlterar btApagar WITH FRAME {&FRAME-NAME}.
  DISABLE btSave btCancel WITH FRAME {&FRAME-NAME}.
END.


&Scoped-define BROWSE-NAME brUsuario
&UNDEFINE SELF-NAME



/* ***************************  Main Block  *************************** */


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.


/* **********************  Internal Procedures  *********************** */

PROCEDURE clearFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN fillnome = '' fillsobrenome = '' fillcpf = ''.
DISP fillnome fillsobrenome fillcpf WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY FILLNOME FILLSOBRENOME FILLCPF 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BTCADAST BTLIST BTALTERAR brUsuario BTAPAGAR 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

PROCEDURE newRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   ASSIGN fillnome fillsobrenome fillcpf .
END.
FIND tt-usuario WHERE tt-usuario.cpf = fillcpf NO-ERROR.
IF AVAILABLE tt-usuario THEN
   MESSAGE 'Ja existe'
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
ELSE DO:
   CREATE tt-usuario.
   ASSIGN tt-usuario.nome      = fillnome
          tt-usuario.sobrenome = fillsobrenome
          tt-usuario.cpf       = fillcpf.
   {&open-query-{&browse-name}}
END.
END PROCEDURE.

PROCEDURE updateRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   ASSIGN fillnome fillsobrenome fillcpf .
END.
FIND tt-usuario WHERE tt-usuario.cpf = fillcpf NO-ERROR.
IF AVAILABLE tt-usuario THEN DO:
   ASSIGN tt-usuario.nome      = fillnome
          tt-usuario.sobrenome = fillsobrenome
          tt-usuario.cpf       = fillcpf.
   {&open-query-{&browse-name}}
END.
ELSE 
   MESSAGE 'Nao encontrado'
       VIEW-AS ALERT-BOX INFO BUTTONS OK.


END PROCEDURE.
