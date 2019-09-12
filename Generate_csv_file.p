DEF VAR csv-file AS char.
ASSIGN csv-file = "c:\temp\caio\report.csv".

OUTPUT TO VALUE(csv-file)NO-CONVERT.
FOR EACH products WHERE products.partnumber-prod >= 1 AND
                        products.partnumber-prod <= 999999 NO-LOCK USE-INDEX productcode.
   PUT products.partnumber-prod
       ";"
       products.descriítion
       ";"
       products.qtyavailable SKIP
END.
OUTPUT CLOSE.

DOS SILENT START VALUE(csv-file)
