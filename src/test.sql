/* TEST PM Picklist */
(SELECT OrderEntry.ProjID OrderEntryProjID,
    OrderEntry.ItemID OrderEntryItemID,
    OrderEntry.Memo OrderEntryMemo,
    OrderEntry.Unit OrderEntryUnit,
    OrderEntry.DocID OrderEntryDocID,
    OrderEntry.DocNO OrderEntryDocNO,
    OrderEntry.DocParID OrderEntryDocParID,
    PO.ItemID POItemID,
    PO.ItemDesc POItemDesc,
    PO.SourceDocID POSourceDocID,
    PO.Unit POUnit,
    PO.DocID PODocID,
    PO.QTY POQTY,
    PO.Price POPrice
FROM (SELECT
   "so:Order Entry"."Transaction detail Attributes"."ITEMDESC" ItemDesc,
   "so:Order Entry"."Transaction detail Attributes"."ITEMID" ItemID,
   "so:Order Entry"."Transaction detail Attributes"."MEMO" Memo,
   "so:Order Entry"."Transaction detail Attributes"."PROJECTID" ProjID,
   "so:Order Entry"."Transaction detail Attributes"."PROJECTNAME" ProjName,
   "so:Order Entry"."Transaction detail Attributes"."UNIT" Unit,
   "so:Order Entry"."Transaction"."DOCID" DocID,
   "so:Order Entry"."Transaction"."DOCNO" DocNO,
   "so:Order Entry"."Transaction"."DOCPARID" DocParID,
   "so:Order Entry"."Transaction"."WHENCREATED" WhenCreated,
   "so:Order Entry"."Warehouse"."WAREHOUSEID" WhareHouseID,
   "so:Order Entry"."Transaction detail Measures"."PRICE_CONVERTED" Price,
   "so:Order Entry"."Transaction detail Measures"."UIQTY" QTY
FROM "so:Order Entry"
WHERE
("so:Order Entry"."Transaction"."DOCPARID" IN ('Inventory Order Shipper'))) OrderEntry
INNER JOIN (
SELECT
   "po:Purchase Order"."Transaction detail Attributes"."ITEMDESC" ItemDesc,
   "po:Purchase Order"."Transaction detail Attributes"."ITEMID" ItemID,
   "po:Purchase Order"."Transaction detail Attributes"."PROJECTID" ProjID,
   "po:Purchase Order"."Transaction detail Attributes"."SOURCEDOCID" SourceDocID,
   "po:Purchase Order"."Transaction detail Attributes"."UNIT" Unit,
   "po:Purchase Order"."Transaction"."DOCID" DocID,
   "po:Purchase Order"."Transaction detail Measures"."UIPRICE" Price,
   "po:Purchase Order"."Transaction detail Measures"."UIQTY" QTY
FROM "po:Purchase Order") PO
ON OrderEntry.ProjID = PO.ProjID) M
