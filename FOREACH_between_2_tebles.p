FOR EACH nota-fiscal WHERE nota-fiscal.dt-emis-nota >= 03/01/2010 AND
                           nota-fiscal.dt-emis-nota <= 03/30/2010
                           NO-LOCK
                           USE-INDEX nfftrm-20,

EACH it-nota-fisc WHERE it-nota-fisc.cod-estabel = nota-fiscal.cod-estabel AND
    it-nota-fisc.serie = nota-fiscal.serie AND
    it-nota-fisc.nr-nota-fis = nota-fiscal.nr-nota-fis
    USE-INDEX ch-nota-item.
DISP nota-fiscal.nr-nota-fis
  it-nota-fisc.it-codigo.
END.
