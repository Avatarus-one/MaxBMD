struct Matrix44
(
	--_00, _01, _02, _03,
	--_10, _11, _12, _13,
	--_20, _21, _22, _23,
	--_30, _31, _32, _33,
	m,
	
	

	fn Equals b =
	(
		for i = 1 to 4 do
		(
			for j = 1 to 4 do
			(
				if m[i][j] != b.m[i][j] then
					return false
			)
		)
  		return true
	),
	
	fn SetValues v00 v01 v02 v03 v10 v11 v12 v13 v20 v21 v22 v23 v30 v31 v32 v33 =
	(
		m = #()
		m[1] = #(v00, v01, v02, v03)
		m[2] = #(v10, v11, v12, v13)
		m[3] = #(v20, v21, v22, v23)
		m[4] = #(v30, v31, v32, v33)
	),
	
	fn GetIdentity =
	(
		mat = Matrix44()
		mat.m = #()
		mat.m[1] = #(1.0, 0.0, 0.0, 0.0)
		mat.m[2] = #(0.0, 1.0, 0.0, 0.0)
		mat.m[3] = #(0.0, 0.0, 1.0, 0.0)
		mat.m[4] = #(0.0, 0.0, 0.0, 1.0)
		return mat
	),
	
	fn LoadIdentity =
	(
		m = #()
		m[1] = #(1.0, 0.0, 0.0, 0.0)
	    m[2] = #(0.0, 1.0, 0.0, 0.0)
		m[3] = #(0.0, 0.0, 1.0, 0.0)
		m[4] = #(0.0, 0.0, 0.0, 1.0)
	),
	
	fn LoadZero =
	(
		m = #()
		m[1] = #(0.0, 0.0, 0.0, 0.0)
	    m[2] = #(0.0, 0.0, 0.0, 0.0)
		m[3] = #(0.0, 0.0, 0.0, 0.0)
		m[4] = #(0.0, 0.0, 0.0, 0.0)
	),

	fn Multiply b = 
	(
  		ret = Matrix44()
		ret.LoadIdentity()
		for i = 1 to 4 do
		(
			for j = 1 to 4 do
			(
				ret.m[i][j] = m[i][1]*b.m[1][j] + m[i][2]*b.m[2][j] + m[i][3]*b.m[3][j] + m[i][4]*b.m[4][j]
			)
		)
  		return ret
	),
	
	-- returns point
	-- see drawBmd.cpp
	fn MultiplyVector v =
	(
		x = m[1][1] * v.x + m[1][2]*v.y + m[1][3] * v.z + m[1][4]
		y = m[2][1] * v.x + m[2][2]*v.y + m[2][3] * v.z + m[2][4]
		z = m[3][1] * v.x + m[3][2]*v.y + m[3][3] * v.z + m[3][4]
		
		v = Vector3()
		v.setXYZ x y z
		
		return v
	),
	
	fn LoadTranslateRM tx ty tz =
	(
	  LoadIdentity()
	  m[4][1] = tx
	  m[4][2] = ty
	  m[4][3] = tz
	),
	
	fn LoadTranslateLM  tx ty tz =
	(
	  LoadIdentity()
	  m[1][4] = tx
	  m[2][4] = ty
	  m[3][4] = tz
	),
	
	fn LoadRotateXRM rad =
	(
	  LoadIdentity()
	  m[2][2] =  Math.RadCos rad
	  m[3][2] = -1 * (Math.RadSin rad)
	  m[2][3] =  Math.RadSin rad
	  m[3][3] =  Math.RadCos rad
	),
	
	fn LoadRotateXLM rad =
	(
	  LoadIdentity()
	  m[2][2] =  Math.RadCos rad
	  m[2][3] = -1 * (Math.RadSin rad) -- -1 * sin rad same as -sin 1.5707
	  m[3][2] =  Math.RadSin rad
	  m[3][3] =  Math.RadCos rad
	),
	
	fn LoadRotateYRM rad =
	(
	   LoadIdentity()
	   m[1][1] =  Math.RadCos rad
	   m[3][1] =  Math.RadSin rad
	   m[1][3] = -1 * (Math.RadSin rad)
	   m[3][3] =  Math.RadCos rad
	),
	
	fn LoadRotateYLM rad =
	(
	  LoadIdentity()
	  m[1][1] =  Math.RadCos rad
	  m[1][3] =  Math.RadSin rad
	  m[3][1] = -1 * (Math.RadSin rad)
	  m[3][3] =  Math.RadCos rad
  
  
	),
	
	fn loadRotateZRM rad =
	(
	  loadIdentity()
	  m[1][1] =  Math.RadCos rad
	  m[2][1] = -1 * (Math.RadSin rad)
	  m[1][2] =  Math.RadSin rad
	  m[2][2] =  Math.RadCos rad
	),

	fn loadRotateZLM rad =
	(
	
	  LoadIdentity()
	  m[1][1] =  Math.RadCos rad
	  m[1][2] = -1 * (Math.RadSin rad)
	  m[2][1] =  Math.RadSin rad
	  m[2][2] =  Math.RadCos rad
	)

)
