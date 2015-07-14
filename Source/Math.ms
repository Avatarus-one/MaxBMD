struct MathSingleton
(
	fn RadCos rad =
	(
		deg = rad * 180.0 / PI 
		return (cos deg)
	),
	
	fn RadSin rad =
	(
		deg = rad * 180.0 / PI
		return (sin deg)
	),
	
	fn Maximum a b =
	(
	  if (a > b) then
	  	return a
	  else 
	    return b
	)
)

Math = MathSingleton()