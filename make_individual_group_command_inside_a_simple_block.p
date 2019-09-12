DEF VAR action AS CHAR FORMAT "x(20)".
ASSIGN action="TurnOn".

IF action = "TurnOn" THEN DO:
  MESSAGE "Device is On" SKIP
    "Device Will turn Off" VIEW-AS ALERT-BOX.
  ASSIGN action="TurnOff".
  MESSAGE "Device is " SAKIP
    action VIEW-AS ALERT-BOX.
END.
    
