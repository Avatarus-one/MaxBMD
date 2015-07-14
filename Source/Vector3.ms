struct Vector3
(
	x, y, z,
	
	fn ToMaxScriptPos = 
	(
		return [x,y,z]
		
		--return [x, -z, y] -- flip order
	),
	
	fn ToMaxScriptPosFlip = 
	(
		return [x, -z, y] -- flip order
	),
	
	fn IsZero = (return (x != 0 AND y != 0 AND z != 0)),
	
	fn  setXYZFlip aX aY aZ =
	(
		x = aX
		y = -aZ
		z = aY	
	),
	
	fn setXYZ aX aY aZ =
	(
		x = aX
		y = aY
		z = aZ
		
		
		--y = -aZ
		--z = aY
		
		-- left hand
		-- y,z,x // top ok. needs z rotate
		-- z,x,y
		-- x,z,y
		-- z,y,x // same as orig?
		-- y,z,x,    z,x,y,    
		--x = -aY
		--y = -aX
		--z= -aY
		
		
		if (x == undefined) then
			messageBox "X Undefined"
		if (y == undefined) then
			messageBox "Y Undefined"
		if (z == undefined) then
			messageBox "Z Undefined"
			
	)
)