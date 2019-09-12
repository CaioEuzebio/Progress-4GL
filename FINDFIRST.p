FIND FISRT product WHERE product.productiid = 123 NO-LOCK NO-ERROR.
IF AVAIL produc THEN MESSAGE "Product Found" VIEW-AS ALERT-BOX.
IF NOT AVAIL product THEN MESSAGE "Product not found" VIEW-AS ALERT-BOX.
