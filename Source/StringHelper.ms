struct _StringHelper
(
	fn GetLastIndex srcStr matchChar =
	(
		i = srcStr.count
		for index = 1 to srcStr.count do
		(
			if srcStr[i] == matchChar then
				return i
			i -= 1
		)
		return -1
	),
	
	fn StringReplace srcStr matchStr replaceStr =
	(
		res = srcStr 
		i = findString res matchStr
		while i != undefined do
		(
			res = replace res i matchStr.count replaceStr 
			i = findString res matchStr
		)
		return res
	),
	
	
	fn ReplaceChar srcStr matchChar replaceChar =
	(
		res = srcStr 
		for i = 1 to res .count do
		(
			if res [i] == matchChar then
				res [i] = replaceChar 
		)
		return res 
	)
)

StringHelper = _StringHelper()


