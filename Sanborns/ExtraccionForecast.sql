-- 4 - BOLSAS
-- 401 - BOLSAS PIEL (401004)
-- 402 - BOLSAS SINTETICO (402004)
-- 67 - FOTOGRAFIA 
-- 6703 - CAMARAS REFLEX  (67030067)
-- 6702 - CAMARAS DIGITALES (67020067)


SELECT * FROM S_Td_Articulo  
WHERE 1 = 1
	AND Id_Linea = 402004
	AND Id_Clase IN (88880040204, 99990040204)


SELECT 
	B.Id_Linea,
	A.Id_Fecha,
	SUM(Num_UVenta) AS Unidades
FROM S_Th_Ventas A, S_Td_Articulo B
WHERE 1 = 1
	AND A.Id_Fecha >= '20160101' AND A.Id_Fecha <= '20171231'
	AND A.Id_Articulo = B.Id_Articulo
	AND B.Id_Linea IN (401004, 402004)
	AND B.Id_Clase IN (
		99990040104,
		88880040104,
		88880040204,
		99990040204
		)
GROUP BY
	Id_Linea,
	Id_Fecha
ORDER BY 
	Id_Fecha
