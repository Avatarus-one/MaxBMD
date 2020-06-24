macroScript BMDImporterUI2 category:"MaxBMD" tooltip:"BMD Importer" Icon:#("Maxscript", 2) ButtonText:"MaxBMD"
(
	
if (heapSize < 20000000) then
	heapSize = 20000000 -- allow ~ 20 MB instead of just 7.5 MB. Prevents "Runtime Error: Out of scripter memory"



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
struct ReturnValue
(
	srcPos = 0, dstPos = 0 -- int 
)

struct BinaryReader
(
_f, -- binary file stream
_size,
_tempFileName,
	
fn Close =
(
	fclose _f
	
	if (_tempFileName != undefined) then
		deleteFile _tempFileName
),

fn DecodeYaz0 src srcSize dst uncompressedSize =
(
	local r = ReturnValue() -- current read/write positions
  
	local validBitCount = 0 -- u32 number of valid bits left in "code" byte
	local currCodeByte -- u8
	
	while (r.dstPos < uncompressedSize) do
	(
		-- read new "code" byte if the current one is used up
		if (validBitCount == 0) then
		(
			if (r.srcPos >= srcSize) then
				return r
			currCodeByte = src[r.srcPos + 1] 
			r.srcPos += 1
			validBitCount = 8
		)
		
		if ((bit.and currCodeByte 0x80) != 0) then
		(
			  -- straight copy
			  if (r.srcPos >= srcSize) then
				return r
			  dst[r.dstPos + 1] = src[r.srcPos + 1]
			  r.dstPos += 1
			  r.srcPos += 1
		)
		else
		(
			  -- RLE part
			  if(r.srcPos >= srcSize - 1) then
					return r
			
			  byte1 = src[r.srcPos + 1] -- u8 
			  byte2 = src[r.srcPos + 2] -- u8 

			  r.srcPos += 2

			  
			  dist = bit.or (bit.shift (bit.and byte1 0xF) 8) byte2 -- u32  ((byte1 & 0xF) << 8) | byte2;
			
			
			
			  copySource = r.dstPos - (dist + 1) -- u32 copySource = r.dstPos - (dist + 1);

			if (copySource < 0) then
			(
				messageBox ("copySource < 0 ??? " + (r.dstPos as string ) +":"+ (dist as string ) +":"+ (copySource as string))
				throw "ERROR"
			)
			
			  numBytes = bit.shift byte1 -4 -- u32  byte1 >> 4
			  if(numBytes == 0) then
			  (
					if(r.srcPos >= srcSize) then
					  return r
					numBytes = src[r.srcPos + 1] + 0x12
					r.srcPos += 1
			  )
			  else
				numBytes += 2

			  -- copy run
			  for i = 1 to numBytes do
			  (
					if(r.dstPos >= uncompressedSize) then
					  return r
					dst[r.dstPos + 1] = dst[copySource + 1]
					copySource += 1
					r.dstPos += 1
			  )
		  )
		
		
		 -- use next bit from "code" byte
		currCodeByte = bit.shift currCodeByte 1 -- currCodeByte <<= 1
		validBitCount -= 1
	)
	
	return r
),

-- get a string with a fixed size
fn ReadFixedLengthString len = 
(
	local strRead = ""
	for i = 1 to len do
		strRead += (bit.intAsChar (ReadByte _f ))

	return strRead
),

-- 4 bytes, unsigned int
fn ReadDWORD = 
(
	local w1 = ReadByte _f #unsigned
	local w2 = ReadByte _f #unsigned
	local w3 = ReadByte _f #unsigned
	local w4 = ReadByte _f #unsigned
	
	local d = bit.or (bit.shift w1 24) (bit.shift w2 16) 
  	d = bit.or d (bit.shift w3 8)
  	d = bit.or d w4
  	return d
),

fn Open srcPath = 
(
	_f = fopen srcPath  "rb"
	--fseek _f 0 #seek_end
	--_size = ftell _f
	fseek _f 0 #seek_set
	
	if (_f == undefined) then
	(
		messageBox ("Unable to open file " + srcPath)
		throw ("Unable to open file " + srcPath )
	)
	
	tag = ReadFixedLengthString 4
	fseek _f 0 #seek_set
	
	if (tag != "Yaz0") then
		return undefined -- not compressed, return file directly
	


  -- yaz0-compressed file - uncompress to a temporary file,
  -- return the uncompressed file

	uncompressedSize = ReadDWord()
	fseek _f 0 #seek_end
	compressedSize = (ftell _f) - 16 -- 16 byte header
	fseek _f 16 #seek_set -- seek to start of data
	
	srcData = #()
	--srcData[compressedSize] = 0 -- Pre-initialize array size
	
	dstData = #()
	--dstData[uncompressedSize] = 0 -- Pre-initialize array size
	
	for i = 1 to compressedSize do
		srcData[i] = ReadByte _f #unsigned

	fclose _f
	
for i = 1 to compressedSize do
(
	if (	srcData[i]  < 0) then
	(
		messageBox ("srcData[i]  < 0 ??? " + (srcData[i]  as string))
					throw "ERROR"
	)
)
	
	r = decodeYaz0 srcData compressedSize dstData uncompressedSize
					 
  --write decompressed data to a temporary file and
  --return handle to this file
  _tempFileName = (getFilenamePath srcPath) + (getFilenameFile srcPath) + ".tempYaz0"
	
   _f = fopen _tempFileName  "wb" -- creates file if not found


	for i = 1 to r.dstPos do
		WriteByte _f dstData[i] #unsigned
	
	fclose _f
	
	-- open temp file for reading
	_f = fopen _tempFileName  "rb"
	fseek _f 0 #seek_set
),

-- TODO: Dosn't work
fn EOF = 
(
	throw "NYI"
	return (ftell _f) >= _size
),

-- seek with an offset to the current position
fn SeekCur offset = 
(
	fseek _f offset #seek_cur
),

-- seek to an absolute position
fn SeekSet position = 
(
	fseek _f position #seek_set
),

fn Position = (return ftell _f),

fn GetByte = 
(
	return ReadByte _f #unsigned
),



-- read a null terminated string from an absolute offset
fn ReadString offset = 
(
	local t = ftell _f
	fseek _f offset #seek_set
	
	local strRead = ""
	local ic = ReadByte _f
	local c = (bit.intAsChar ic)
	strRead += c
	while (ic != 0) do
	(
		ic = ReadByte _f
		c = (bit.intAsChar ic)
		strRead += c
	)
	
	fseek _f t #seek_set
	
	return strRead
),



-- 2 bytes, unsigned short
fn ReadWORD =
(
  local w1 = ReadByte _f #unsigned
  local w2 = ReadByte _f #unsigned
  
  local w = bit.or (bit.shift w1 8) w2 -- w = (w1 << 8) | w2;
  
  if w < 0 then
  (
		messageBox "ReadWORD should be unsigned"
		throw "GetSHORT should be unsigned"
  )
	
  return w
),


-- returns string[]
fn ReadStringTable pos =
(
	oldPos = Position()
	SeekSet pos 
	count = ReadWORD() -- orig = 35 read = 35. current pos = ok?
	unknown1 = ReadWORD() -- skip pad bytes
	result = #()
	
	for i = 1 to count do
	(
		unknown = ReadWORD()
		stringOffset = ReadWORD()
		s = ReadString (pos + stringOffset)
		result[i] = s
	)
	SeekSet oldPos 
	return result
),

-- 2 bytes, signed short. Using two's complement
fn GetSHORT = 
(
	signedValue = ReadWORD() -- unsigned
	negativeSign = bit.get signedValue 16
	if (negativeSign) then
	(
		-- flip bits and add 1
		-- signedValue = bit.not signedValue // Dosn't work?
		signedValue = bit.xor signedValue 65535 -- 65535 == all 1's
		signedValue += 1
		signedValue = signedValue * -1
	)
		
	return signedValue
)
,

-- must reverse value for read. big endian [could also write / read from another file]
-- http://java.sun.com/j2se/1.3/docs/api/java/lang/Float.html#intBitsToFloat(int)
fn GetFloat =
(
	bits = ReadDWORD()
	s = -1
	if (bit.shift bits -31) == 0 then
		s = 1
		
	e = bit.and (bit.shift bits -23) 0xff 

 	m = 0
	if (e == 0) then
		m = bit.shift (bit.and bits 0x7fffff) 1
	else
		m = bit.or (bit.and bits 0x7fffff) 0x800000
		
	fx = s * m * (pow 2 (e-150))
	return fx 
)

)
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



-- only used during file load
struct Inf1Header
(
  tag, -- char[4] 'INF1'
  sizeOfSection, -- u32 
  unknown1, -- u16 
  pad, -- u16  0xffff
  unknown2, -- u32 
  vertexCount, -- u32 number of coords in VTX1 section
  offsetToEntries, -- u32 offset relative to Inf1Header start
  
  fn LoadData br =
  (
	tag = br.ReadFixedLengthString 4
	sizeOfSection = br.ReadDWORD()
    unknown1 = br.ReadWORD()
	pad = br.ReadWORD()
	unknown2 =  br.ReadDWORD()
  	vertexCount =  br.ReadDWORD()
	offsetToEntries =  br.ReadDWORD()

  )  
)

-- only used during file load
-- This stores the scene graph of the file
struct Inf1Entry
(
  -- 0x10: Joint
  -- 0x11: Material
  -- 0x12: Shape (ie. Batch)
  -- 0x01: Hierarchy down (insert node), new child
  -- 0x02: Hierarchy up, close child
  -- 0x00: Terminator
  type, -- u16 

  -- Index into Joint, Material or Shape table
  -- always zero for types 0, 1 and 2
  index, -- u16 
  
  fn LoadData br =
  (
	type = br.ReadWORD()
    index = br.ReadWORD()
  )
)

struct Inf1
(
  numVertices, -- int no idea what's this good for ;-)
  scenegraph, -- std::vector<Inf1Entry> scenegraph;
  
  fn LoadData br =
  (
  	local inf1Offset = br.Position()
	
  	scenegraph = #() -- vector<Inf1Entry>
	local header = Inf1Header()
	header.LoadData br
	numVertices = header.vertexCount
	
	-- read scene graph
	br.SeekSet (inf1Offset + header.offsetToEntries)
	 
	entry = Inf1Entry()
	entry.LoadData br
	 
	local i = 1
	while entry.type != 0 do
	(
		scenegraph[i] = entry 
		
		entry = Inf1Entry()
		entry.LoadData br
	
		i += 1
	)

  )
)


struct VertColor
(
  -- unsigned char 
  r, g, b, a,

  -- all params are floats, must cast to char
  fn SetRGBA ri gi bi ai =
  (
  	-- TODO
    --r = (unsigned char)(ri + .5f);
    --g = (unsigned char)(gi + .5f);
    --b = (unsigned char)(bi + .5f);
    --a = (unsigned char)(ai + .5f);
  )
)

struct TexCoord
(
  s, t, -- float 

  fn SetST si ti =
  (
    s = si
    t = ti
  )
)

struct ArrayFormat
(
  -- see ogc/gx.h for a more complete list of these values:
  arrayType, -- u32 9: coords, a: normal, b: color, d: tex0 (gx.h: "Attribute")
  componentCount, -- u32 meaning depends on dataType (gx.h: "CompCount")
  dataType, -- u32 3: s16, 4: float, 5: rgba8 (gx.h: "CompType")

  -- values i've seem for this: 7, e, 8, b, 0
  ---> number of mantissa bits for fixed point numbers!
  -- (position of decimal point)
  decimalPoint, -- u8 
  unknown3, -- u8 seems to be always 0xff
  unknown4, -- u16 seems to be always 0xffff
  
  fn LoadData br =
  (
  	arrayType = br.ReadDWORD()
	componentCount = br.ReadDWORD()
	dataType = br.ReadDWORD()
	decimalPoint = br.GetByte()
	unknown3 = br.GetByte()
	unknown4 = br.ReadWORD()
  )
)

struct Vtx1Header
(
  tag, -- char[4] 'VTX1'
  sizeOfSection, -- u32 
  arrayFormatOffset, -- u32 for each offsets[i] != 0, an ArrayFormat
                    -- is stored for that offset
                    -- offset relative to Vtx1Header start

  /*
    content is described by ArrayFormat - for each offset != 0,
    an ArrayFormat struct is stored at Vtx1Header.arrayFormatOffset
  */
  offsets = #(), -- u32[13]  offsets relative to Vtx1Header start
  
  fn LoadData br =
  (
  	tag = br.ReadFixedLengthString 4
	sizeOfSection = br.ReadDWORD()
	arrayFormatOffset = br.ReadDWORD()
	for i=1 to 13 do
	(
		offsets[i] = br.ReadDWORD()
	)
  ),
  
  fn GetLength offsetIndex =
  (
  	startOffset = offsets[offsetIndex]
	for i = (offsetIndex + 1) to 13 do --  for(int i = k + 1; i < 13; ++i)
	(
		if (offsets[i] != 0) then
			return offsets[i] - startOffset
	)
	
	return sizeOfSection  - startOffset
  )
  
)

struct Vtx1
(
  positions = #(), -- std::vector<Vector3f> 
  normals = #(), -- std::vector<Vector3f>
  colors = #(), -- std::vector<Color> colors[2] 
  texCoords = #(), -- std::vector<TexCoord> texCoords[8]  
  
  -- pass in floats. Round up?
  fn GetColor ri gi bi ai = 
  (
  	r = (ri + 0.5) as Integer
    g = (gi + 0.5) as Integer
    b = (bi + 0.5) as Integer
    a = (ai + 0.5) as Integer
	return color r g b a
  ),
  
  --void readVertexArray(Vtx1& arrays, const bmd::ArrayFormat& af, int length,
   --                  FILE* f, long offset)
  fn ReadVertexArray af length br offset =
  (
  	br.SeekSet offset
	
	----------------------------------------------------------------------
  	-- convert array to float (so we have n + m cases, not n*m)
	data = #() -- vector<float>
	bytesRead = 0 -- int
	
	 -- print ("af.dataType=" + (af.dataType as string) + ": af.arrayType=" + (af.arrayType as string)  )
	  
	if (af.dataType == 3) then
	(
		tmp = #() -- size = length/2
		count = length/2
		scale = pow 0.5 af.decimalPoint
		for i = 1 to count do
		(
			tmp[i] = br.GetSHORT() -- TODO: test
			data[i] = tmp[i] * scale
		)
		--throw "TODO: testing"
		--messageBox "3"
	) 
	else if (af.dataType == 4) then -- f32
	(
		count = length/4
		for i = 1 to count do
		(
			data[i] = br.GetFloat() -- TODO: test
		)
		--throw "TODO: testing2"
		--print (format "ZZZ % %" length count )
	) 
	else if (af.dataType == 5) then -- rgb(a)
	(
		tmp = #() -- size = length
		for i = 1 to length do
		(
			data[i] = br.GetByte()
		)
		--messageBox "Vtx1: af.dataType == 5. NYI"
	)
	else 
	(
		messageBox "vtx1: unknown array data type %" af.dataType
	)

--print "DATA: "
--print data

	----------------------------------------------------------------------
    -- stuff floats into appropriate vertex array
	if (af.arrayType == 9) then -- positions
	(
	  if(af.componentCount == 0) then -- xy [Needs testing]
      (
	    positions = #()
		posCount = data.count / 2
		k = 1
		for j = 1 to posCount do
		(
			pos = Vector3()
			pos.setXYZ data[k] data[k + 1] 0
			positions [j] = pos
			k += 2
		)
		
		messagebox (format "Vtx1: DT % %. Needs testings" af.dataType af.componentCount)
      )
	  else if(af.componentCount == 1) then -- xyz
      (
	  	positions = #()
		posCount = data.count / 3
		k = 1
		for j = 1 to posCount do
		(
			pos = Vector3()
			pos.setXYZ data[k] data[k + 1] data[k + 2]
			
			-- pos.setXYZFlip data[k] data[k + 1] data[k + 2]
			
			-- [v.x, -v.z, v.y] -- flip order
			
			positions[j] = pos
			k += 3
		)
		
		--
	    --print (format "LEN %. COUNT %" length (data.count / 3))
		--print positions 
		
		--messagebox (format "DT % %" af.dataType af.componentCount)
      )
      else
	  (
	    messageBox "vtx1: unsupported componentCount for positions array" 
	 
	  	--messageBox (format "vtx1: unsupported componentCount for positions array: %" af.componentCount)
	  )
	)
	else if (af.arrayType == 0xa) then -- normals TODO: Test [0xa=10]
	(
	  if af.componentCount == 0 then -- xyz
      (
        normalsCount = data.count / 3.0
		normals = #() -- arrays.normals.resize(data.size()/3);
		
		k = 1
		for j = 1 to normalsCount do
		(
		    normals[j] = Vector3()
		    normals[j].setXYZ data[k] data[k + 1] data[k + 2]
			k += 3
		) 
        --for(int j = 0, k = 0; j < arrays.normals.size(); ++j, k += 3)
        --  arrays.normals[j].setXYZ(data[k], data[k + 1], data[k + 2]);
      )
      else
	      throw "Warning: vtx1: unsupported componentCount for normals array"

	)
	else if (af.arrayType == 0xb OR af.arrayType == 0xc) then -- color0 or color1
	(
	  index = af.arrayType - 0xb;
      if (af.componentCount == 0) then -- rgb
      (
		colors = #()
		-- colors[data.count / 3] = 0 --initialize???
		colorCount = data.count / 3
		colors[index + 1] = #()
		k = 1
		for j = 1 to colorCount  do
		(
			colors[index + 1][j] = GetColor data[k] data[k + 1] data[k + 2] 255
			k += 3
		)
      )
      else if (af.componentCount == 1) then -- rgba
      (
	  	colors = #()
		colors[index + 1] = #()
		colorCount = data.count / 4
		k = 1
		for j = 1 to colorCount  do
		(
			colors[index + 1][j] = GetColor data[k] data[k + 1] data[k + 2] data[k + 3]
			k += 4
		)
      )
      else
        messageBox "vtx1: unsupported componentCount for colors array"
		  
	)
    -- texcoords 0 - 7 [13]
    else if (af.arrayType == 0xd OR
            af.arrayType == 0xe OR
            af.arrayType == 0xf OR
            af.arrayType == 0x10 OR
            af.arrayType == 0x11 OR
            af.arrayType == 0x12 OR
            af.arrayType == 0x13 OR
            af.arrayType == 0x14) then 
    (
	  -- std::vector<TexCoord> texCoords[8] texCoords
	  index = (af.arrayType - 0xd) + 1
      
      if (af.componentCount == 0) then --s
      (
        texCoords[index] = #() -- texCoords[index].resize(data.size());
		
		for j=1 to data.count do
		(
		   texCoords[index][j] = TexCoord()
		   texCoords[index][j].SetST data[j] 0
		)
		
        --for(int j = 0; j < arrays.texCoords[index].size(); ++j)
        --  arrays.texCoords[index][j].setST(data[j], 0);
      )
      else if (af.componentCount == 1) then -- st
      (
	    texCount = data.count/2
        texCoords[index] = #() -- arrays.texCoords[index].resize(data.size()/2);
		
		k = 1
		for j=1 to texCount  do
		(
		   texCoords[index][j] = TexCoord()
		   texCoords[index][j].SetST data[k] data[k + 1]
		   k += 2
        )
		
        --for(int j = 0, k = 0; j < arrays.texCoords[index].size(); ++j, k += 2)
       --   arrays.texCoords[index][j].setST(data[k], data[k + 1]);
      )
      else
	  		throw "WARNING: vtx1: unsupported componentCount for texcoords array "
    )
	
  ),
				 
  fn LoadData br =
  (
    vtx1Offset = br.Position()
  
  	local header = Vtx1Header()
	header.LoadData br
	
	--messageBox "x"
	numArrays = 0
	for i = 1 to 13 do
	(
		if header.offsets[i] != 0 then
			numArrays += 1
	)

	-- read vertex array format descriptions
	local formats = #() -- vector<bmd::ArrayFormat> 
	for i = 1 to numArrays do
	(
		af = ArrayFormat()
		af.LoadData br
		formats[i] = af
	)
	
	
	-- read arrays
	br.SeekSet (vtx1Offset + header.arrayFormatOffset)
	
	j = 1
	for i = 1 to 13 do
	(
	  if (header.offsets[i] != 0) then
	  (
	  	f = formats[j]
		len = header.GetLength i
		  
		--print ("Vert " + (i as string) + ":" + (len as string)  )
		ReadVertexArray f len br (vtx1Offset + header.offsets[i])
		
		j += 1
	  )
	)
	
  )
  
)

struct ShpIndex
(
  matrixIndex, -- u16 -- can be undefined
  posIndex, -- u16 
  normalIndex, -- u16 
  colorIndex = #(), -- u16[2]
  texCoordIndex = #() -- u16[8]
)

---------------------------------------------------------------------------------------------------------------------

struct ShpPrimitive
(
  type, -- u8 
  points = #() -- vector<ShpIndex>
)

---------------------------------------------------------------------------------------------------------------------

struct ShpPacket
(
  primitives = #(), -- std::vector<ShpPrimitive>
  matrixTable = #(), -- std::vector<u16> maps attribute matrix index to draw array index
  
-- Shp1BatchAttrib[] attribs 
-- Packet& dst
fn LoadPacketPrimitives attribs dataSize br =
(
	local done = false
	local readBytes = 0
	local primIndex = 1
	
	while (not done) do
	(
		type = br.GetByte()
		readBytes += 1
		
		if (type == 0 OR readBytes >= dataSize) then
		(
			done = true
		)
		else
		(
			curPrimative = ShpPrimitive()
			curPrimative.type = type 
			primitives[primIndex] = curPrimative
			primIndex += 1
			
			count = br.ReadWord()

			readBytes  += 2
			curPrimative.points = #() --  primative.points.resize(count)
  

			for j = 1 to count do
			(
				curPoint = ShpIndex()
				
				for k = 1 to attribs.count do
				(
					val = 0
					
					-- get value
					if attribs[k].dataType == 1 then -- s8
					(
						val = br.GetByte()
						readBytes += 1
					)
					else if attribs[k].dataType == 3 then -- s16
					(
						val = br.ReadWORD()
            					readBytes += 2
					)
					else 
					(
						messageBox "X shp1: got invalid data type in packet. should never happen because dumpBatch() should check this before calling dumpPacket()"
						throw "ERROR"
					)
					
					

			        -- set appropriate index
			        if attribs[k].attrib == 0 then
					(
						curPoint.matrixIndex = val -- can be undefined
						
					)
					else if attribs[k].attrib == 9 then
					(
						curPoint.posIndex = val
						
					)
					else if attribs[k].attrib == 0xa then
					(
						curPoint.normalIndex = val
						
					)
					else if attribs[k].attrib == 0xb OR attribs[k].attrib == 0xc then
					(
						curPoint.colorIndex[(attribs[k].attrib - 0xb) + 1] = val
					)
					else if attribs[k].attrib == 0xd OR
					        attribs[k].attrib == 0xe OR
					        attribs[k].attrib == 0xf OR
					        attribs[k].attrib == 0x10 OR
					        attribs[k].attrib == 0x11 OR
					        attribs[k].attrib == 0x12 OR
					        attribs[k].attrib == 0x13 OR
					        attribs[k].attrib == 0x14 then 
					(
						curPoint.texCoordIndex[(attribs[k].attrib - 0xd) + 1] = val
					)
					else
					(
						
						 -- messageBox "WARNING shp1: got invalid attrib in packet. should never happen because dumpBatch() should check this before calling dumpPacket()"
						 --print curPrimative
						-- throw "shp1: got invalid attrib in packet. should never happen because dumpBatch() should check this before calling dumpPacket()"
						
						 -- ignore unknown types, it's enough to warn() in dumpBatch
					)
				) -- end for k = 1 to attribs.count do
				
				curPrimative.points[j] = curPoint
			) -- for j = 1 to count do


		)  -- end else (type == 0 || readBytes >= dataSize) then
	) -- end while not done do
	
	
) -- end function
  
)

---------------------------------------------------------------------------------------------------------------------

struct ShpAttributes
(
  hasMatrixIndices, hasPositions, hasNormals, -- bool 
  hasColors = #(), -- bool[2] 
  hasTexCoords = #() -- bool[8]; 
)

---------------------------------------------------------------------------------------------------------------------

-- Used in dumpBatch
struct ShpBatch
(
  attribs, -- ShpAttributes
  packets -- std::vector<ShpPacket>
)

---------------------------------------------------------------------------------------------------------------------

-- same as ShpBatch?
struct Shp1HeaderBatch
(
  unknown, -- u16 seems to be always 0x00ff ("matrix type, unk")
  packetCount, -- u16 number of packets belonging to this batch
  --attribs used for the strips in this batch. relative to
  --Shp1Header.offsetToBatchAttribs
  --Read StripTypes until you encounter an 0x000000ff/0x00000000,
  --for all these types indices are included. If, for example,
  --a Batch has types (9, 3), (a, 3), (0xff, 0), then for this batch two shorts (= 3)
  --are stored per vertex: position index and normal index
  offsetToAttribs, --u16 
  firstMatrixData, --u16 index to first matrix data (packetCount consecutive indices)
  firstPacketLocation, --u16 index to first packet location (packetCount consecutive indices)
  unknown3, --u16 0xffff
  unknown4 = #(), --float[7]  great... (seems to match the last 7 floats of joint info sometimes)
  --(one unknown float, 6 floats bounding box?)
  
  fn LoadData br =
  (
	   unknown = br.ReadWORD()
	   packetCount = br.ReadWORD()
	   offsetToAttribs = br.ReadWORD()
	   firstMatrixData = br.ReadWORD()
	   firstPacketLocation = br.ReadWORD()
	   unknown3 = br.ReadWORD()
	   for j = 1 to 7 do
		   unknown4[j] = br.GetFloat()
  )
)

struct Shp1Header
(
  tag, -- char[4]
  sizeOfSection, -- u32 
  batchCount, -- u16 number of batches 
  pad, -- u16 ??
  offsetToBatches, -- u32 should be 0x2c (batch info starts here)
  offsetUnknown, -- u32 ??
  zero, -- u32 ??
  offsetToBatchAttribs, -- u32 batch vertex attrib start

  --The matrixTable is an array of u16, which maps from the matrix data indices
  --to Drw1Data arrays indices. If a batch contains multiple packets, for the
  --2nd, 3rd, ... packet this array may contain 0xffff values, which means that
  --the corresponding index from the previous packet should be used.
  offsetToMatrixTable, -- u32 
  offsetData, -- u32 start of the actual primitive data
  offsetToMatrixData, -- u32 
  offsetToPacketLocations, -- u32 offset to packet start/length info
  
  --(all offsets relative to Shp1Header start)
  
  fn LoadData br =
  (
  	tag = br.ReadFixedLengthString 4
	sizeOfSection = br.ReadDWORD()
    batchCount = br.ReadWORD()
	pad = br.ReadWORD()
	offsetToBatches = br.ReadDWORD()
	offsetUnknown = br.ReadDWORD()
	zero = br.ReadDWORD()
	offsetToBatchAttribs = br.ReadDWORD()
	offsetToMatrixTable = br.ReadDWORD()
	
	offsetData = br.ReadDWORD()
	offsetToMatrixData = br.ReadDWORD()
	offsetToPacketLocations = br.ReadDWORD()
  )
)

---------------------------------------------------------------------------------------------------------------------

struct Shp1BatchAttrib
(
  attrib, --u32 cf. ArrayFormat.arrayType
  dataType, --u32 cf. ArrayFormat.dataType (always bytes or shorts...)
  
  fn LoadData br =
  (
  	attrib = br.ReadDWORD()
	dataType= br.ReadDWORD()
  )
)

-----------------------------------------

--for every packet a PacketLocation struct is stored at
--Shp1Header.offsetToPacketLocation + Batch.firstPacketLocation*sizeof(PacketLocation).
--This struct stores where the primitive data for this packet is stored in the
--data block.
struct Shp1PacketLocation
(
  size, --u32 size in bytes of packet
  offset, --u32 relative to Shp1Header.offsetData
  
  fn LoadData br =
  (
  	size = br.ReadDWORD()
	offset = br.ReadDWORD()
  )
)


---------------------------------------------------------------------------------------------------------------------

struct Shp1Primitive
(
  primitiveType, --u8 see above
  numVertices --u16 that many vertices included in this primitive - for
                   --each vertex indices are stored according to batch type
)

---------------------------------------------------------------------------------------------------------------------

--for every packet a MatrixData struct is stored at
--Shp1Header.offsetToMatrixData + Batch.firstMatrixData*sizeof(MatrixData).
--This struct stores which part of the MatrixTable belongs to this packet
--(the matrix table is stored at Shp1Header.offsetToMatrixTable)
struct Shp1MatrixData --from yaz0r's source (animation stuff)
(
  unknown1, --u16 
  count, --u16 count many consecutive indices into matrixTable
  firstIndex, --u32 first index into matrix table
  
  fn StructSize = (return 8),
  
  fn LoadData br =
  (
  	unknown1 = br.ReadWORD() -- TODO: figure this out...
	count = br.ReadWORD()
	firstIndex = br.ReadDWORD()
  )
)

---------------------------------------------------------------------------------------------------------------------


struct Shp1
(
  batches = #(), -- std::vector<ShpBatch> 

-- return Shp1BatchAttrib[]
fn GetBatchAttribs br offset =
(
	origPos = br.Position()
	br.SeekSet offset
	batchAttribs = #() -- of type Shp1BatchAttrib
	attrib = Shp1BatchAttrib()
	attrib.LoadData br
	
	i = 1
	while (attrib.attrib != 0xff) do
	(
		batchAttribs[i] = attrib
		attrib = Shp1BatchAttrib()
		attrib.LoadData br
		i += 1
	)
	
	br.SeekSet origPos 
	
	return batchAttribs 
),



  -- TODO: unknown data is missing, ...
  -- void dumpBatch(const bmd::Batch& batch, const bmd::Shp1Header& h, FILE* f, long baseOffset, Batch& dst)
fn dumpBatch br batchSrc header baseOffset dst =
(
  	-- read and interpret batch vertex attribs
	
	attribs = GetBatchAttribs br (baseOffset + header.offsetToBatchAttribs + batchSrc.offsetToAttribs)
	
	dst.attribs.hasMatrixIndices = false
	dst.attribs.hasPositions = false
	dst.attribs.hasNormals = false
	
	for i=1 to 2 do 
		dst.attribs.hasColors[i] = false
		
	for i=1 to 8 do 
		dst.attribs.hasTexCoords[i] = false
	
	for i=1 to attribs.count do 
	(
		if attribs[i].dataType != 1 AND attribs[i].dataType != 3 then
		(
			--print "Warning: shp1, dumpBatch(): unknown attrib data type %d, skipping batch"
			messageBox "Warning: shp1, dumpBatch(): unknown attrib data type %d, skipping batch"
			return undefined
		)

		if attribs[i].attrib == 0 then
			dst.attribs.hasMatrixIndices = true
		else if attribs[i].attrib == 9 then
			dst.attribs.hasPositions = true
		else if attribs[i].attrib == 0xa then
			dst.attribs.hasNormals = true
		else if attribs[i].attrib == 0xb OR attribs[i].attrib == 0xc then
			dst.attribs.hasColors[(attribs[i].attrib - 0xb) + 1] = true
		else if (attribs[i].attrib == 0xd OR
		        attribs[i].attrib == 0xe OR
		        attribs[i].attrib == 0xf OR
		        attribs[i].attrib == 0x10 OR
		        attribs[i].attrib == 0x11 OR
		        attribs[i].attrib == 0x12 OR
		        attribs[i].attrib == 0x13 OR
		        attribs[i].attrib == 0x14) then 
		(
			dst.attribs.hasTexCoords[(attribs[i].attrib - 0xd) + 1] = true
		)
		else
		(
			print "Warning: shp1, dumpBatch(): unknown attrib %d in batch, it might not display correctly"
	        -- return; //it's enough to warn
		)
	) -- end for i=1 to attribs.count do 
		
	-- read packets
	dst.packets = #() -- dst.packets.resize(batch.packetCount);
	
	
	for i=1 to batchSrc.packetCount do
	(
		br.SeekSet (baseOffset + header.offsetToPacketLocations + (batchSrc.firstPacketLocation + (i-1))*8) -- sizeof(packetLocation) = 8 bytes
		packetLoc = Shp1PacketLocation()
		packetLoc.LoadData br
		
	    -- read packet's primitives
		dstPacket = ShpPacket()
		br.SeekSet (baseOffset + header.offsetData + packetLoc.offset)
		dstPacket.LoadPacketPrimitives attribs packetLoc.size br
		dst.packets[i] = dstPacket 
		
		
	    -- read matrix data for current packet
		matrixData = Shp1MatrixData()
		br.SeekSet  (baseOffset + header.offsetToMatrixData + (batchSrc.firstMatrixData + (i-1))*matrixData.StructSize())
		matrixData.LoadData br
		
		--print (matrixData as string)
		
	    -- read packet's matrix table
	    --dstPacket.matrixTable.resize(matrixData.count);
		dstPacket.matrixTable = #()
		br.SeekSet (baseOffset + header.offsetToMatrixTable + 2*matrixData.firstIndex)

		for j = 1 to matrixData.count do
			dstPacket.matrixTable[j] = br.ReadWORD()
		
		
		--print (dstPacket.matrixTable.count as string) -- matrixTable
	
		
		--print (dstPacket.matrixTable[1] as string)
	) -- end for i=1 to batchSrc.packetCount do
), -- end fn dumpBatch 

  fn LoadData br =
  (
	  -- print ("0: " + (br.Position() as string))
		 
	  shp1Offset = br.Position()
	  header = Shp1Header()
	  header.LoadData br
	 
	  -- print ("1: " + (br.Position() as string))
		
	  -- read batches
	  br.SeekSet (header.offsetToBatches + shp1Offset)
	  batches = #() --.resize(h.batchCount);
	  
	  -- print  (header.batchCount as string)  = 1 on face
	  for i = 1 to header.batchCount do
	  (
		 -- print ("2: " + (br.Position() as string))
		  
	  	d = Shp1HeaderBatch()
		d.LoadData br
		
		--print ("3: " + (br.Position() as string))
		  
		  
		-- TODO: check code
		dstBatch = ShpBatch()
		dstBatch.attribs = ShpAttributes()
		batches[i] = dstBatch 
		  
	    --Batch& dstBatch = dst.batches[i]; dst = this
	    curPos = br.Position()
	    dumpBatch br d header shp1Offset dstBatch
		  
		--  print ("4: " + (br.Position() as string))
		br.SeekSet curPos
		  
	
	 )
	  
  )
)








struct Jnt1Header
(
  tag, -- char[4] 'JNT1'
  sizeOfSection, -- u32 
  count, -- u16 number of joints
  pad, -- u16 padding u16 (?)

  jntEntryOffset, -- u32 joints are stored at this place
                  -- offset relative to Jnt1Header start

  unknownOffset, -- u32 there are count u16's stored at this point,
                     -- always the numbers 0 to count - 1 (in that order).
                     -- perhaps an index-to-stringtable-index map?
                     -- offset relative to Jnt1Header start
  stringTableOffset, -- u32 names of joints
  
  fn LoadData br =
  (
  	tag = br.ReadFixedLengthString 4
  	sizeOfSection = br.ReadDWORD()
    count = br.ReadWORD()
    pad = br.ReadWORD()
    jntEntryOffset = br.ReadDWORD()
    unknownOffset = br.ReadDWORD()
    stringTableOffset = br.ReadDWORD()
  )
)


struct JntEntry
(
  unknown, -- u16 no idea how this works...always 0, 1 or 2
           --"matrix type" according to yaz0r - whatever this means ;-)
  pad, -- u16 always 0x00ff in mario, but not in zelda

  sx, sy, sz, -- float scale
  rx, ry, rz, -- s16 -32768 = -180 deg, 32767 = 180 deg
  pad2, -- u16 always 0xffff
  tx, ty, tz, -- float translation

  unknown2, -- float 
  bbMin = #(), -- float[3] bounding box (?)
  bbMax = #(), -- float[3] bounding box (?)
  
  fn LoadData br =
  (
	  -- don't flip values (has rotation problems)
    unknown = br.ReadWORD()
	pad = br.ReadWORD()
	sx = br.GetFloat()
	sy = br.GetFloat()  -- flip
	sz = br.GetFloat()  -- flip
	  
	rx = br.GetSHORT() 
	ry = br.GetSHORT()
	rz = br.GetSHORT()
	  
	pad2 = br.ReadWORD()
	  
	tx = br.GetFloat()
	ty = br.GetFloat() -- flip
	tz = br.GetFloat()  -- flip
	
	unknown2 = br.GetFloat()
	
	bbMin = #()
	for j = 1 to 3 do
		bbMin[j] =  br.GetFloat()
	
	bbMax = #()
	for j = 1 to 3 do
		bbMax[j] =  br.GetFloat()
	
	if unknown < 0 OR unknown > 2 then
	(
		msg = "jnt1: unknown of " + (unknown as String) + " joint not in [0, 2]"
		throw msg
  	)
  )
)

struct JntFrame
(
  sx, sy, sz, -- float  scale
  rx, ry, rz, -- float rotation (in degree)
  t, -- Vector3f  //translation
  name, -- string
--_unknown ,
--_pad,
--_unknown2,
---bbMin ,
--bbMax,
  --TODO: u16 unknown
  --bounding box, float unknown2
  
  fn InitFromJntEntry e =
  (
	    sx = e.sx
	    sy = e.sy
	    sz = e.sz
		
	    rx = (e.rx/(32768 as Float))*180
	    ry = (e.ry/(32768 as Float))*180
	    rz = (e.rz/(32768 as Float))*180
		
		t = Vector3()
	    t.setXYZ e.tx e.ty e.tz
		
		_bbMin = e.bbMin 
		_bbMax= e.bbMax
  )
)


struct Jnt1
(
  frames = #(), -- std::vector<Frame> 

  -- the Frames have to be converted to matrices
  -- to be usable by gl. isMatrixValid stores
  -- if a matrix represents a frame of if the
  -- frame has changed since the matrix was
  -- built (in animations for example)
  matrices = #(), -- std::vector<Matrix44f> 
  isMatrixValid = #(), -- std::vector<bool>  //TODO: use this

  --TODO: unknown array
  
  fn LoadData br =
  (
  	jnt1Offset = br.Position()
	
	header = Jnt1Header()
	header.LoadData br
	
	stringTable = br.ReadStringTable (jnt1Offset + header.stringTableOffset)
	
	if stringtable.count != header.count then
	(
		messageBox "jnt1: number of strings doesn't match number of joints"
		throw "jnt1: number of strings doesn't match number of joints"
	)
	
	-- read joints
	br.SeekSet (jnt1Offset + header.jntEntryOffset)
	frames = #() -- frames.resize(h.count);
	matrices = #() -- matrices.resize(h.count);
	isMatrixValid = #() -- isMatrixValid.resize(h.count);
	
	for i = 1 to header.count do
	(
		e = JntEntry()
		e.LoadData br
		f = JntFrame()
		f.InitFromJntEntry e
	
  		if (i-1) < stringtable.count then -- should always be true
	      f.name = stringtable[i];
		else
			throw (format "(i-1) < stringtable.count % % " i stringtable.count)
		
		frames[i] = f
	)
  )
   
)
	


struct Evp1Header
(
  tag, -- char[4]  'EVP1'
  sizeOfSection, -- u32 
  count, -- u16 
  pad, -- u16 

  --0 - count many bytes, each byte describes how many bones belong to this index
  --1 - sum over all bytes in 0 many shorts (index into some joint stuff? into matrix table?)
  --2 - bone weights table (as many floats as shorts in 1)
  --3 - matrix table (matrix is 3x4 float array)
  offsets= #(), -- u32[4]
  
  fn LoadData br =
  (
    tag = br.ReadFixedLengthString 4
	sizeOfSection = br.ReadDWORD()
	count = br.ReadWORD()
	pad = br.ReadWORD()
	
	for i=1 to 4 do
	(
		offsets[i] = br.ReadDWORD()
	)
  )
  
)


struct MultiMatrix
(
  weights = #(), -- std::vector<float> ;
  indices = #() -- std::vector<u16> indices; //indices into Evp1.matrices (?)
)


struct Evp1
(
  weightedIndices = #(), -- std::vector<MultiMatrix> ;
  matrices = #(), -- std::vector<Matrix44f> ;
  
  fn LoadData br =
  (
    evp1Offset = br.Position()

    header = Evp1Header()
	header.LoadData br

	  -- read counts array
	  br.SeekSet (evp1Offset + header.offsets[1])
	  counts = #() -- vector<int> counts(h.count);
	  sum = 0
	  
	  for i = 1 to header.count do
	  (
	    v = br.GetByte() -- u8 v; fread(&v, 1, 1, f);
		sum += v
		counts[i] = v
	  )

	  weightedIndices = #() --  dst.weightedIndices.resize(h.count);
	  
	  -- read indices of weighted matrices
	  br.SeekSet (evp1Offset + header.offsets[2])
	  numMatrices = 0
	  for i = 1 to header.count do
	  (
	    weightedIndices[i] = MultiMatrix()
	    weightedIndices[i].indices = #() -- weightedIndices[i].indices.resize(counts[i]);
	    
		for j = 1 to counts[i] do
		(
		  d = br.ReadWORD()-- index to array (starts at one)
		  weightedIndices[i].indices[j] = d  
		  numMatrices = (Math.Maximum numMatrices (d+1))
		)
	  )
	
	  -- read weights of weighted matrices
	  br.SeekSet (evp1Offset + header.offsets[3])
	  
	  for i = 1 to header.count do
	  (
	    weightedIndices[i].weights = #() -- .resize(counts[i]);
	    for j = 1 to counts[i] do --(int j = 0; j < counts[i]; ++j)
	    (
		 -- error if f1 = br.GetFloat() used? can print value but assign = undefined
		  local fz = br.GetFloat()
		  weightedIndices[i].weights[j] = fz
	    )
	  )
	
	  -- read matrices
	  matrices = #() -- .resize(numMatrices);
	  br.SeekSet (evp1Offset + header.offsets[4])
	  for i = 1 to numMatrices do
	  (
	    matrices[i] = Matrix44()
		matrices[i].LoadIdentity()
		
		for j = 1 to 3 do
		(
		  for k = 1 to 4 do
		  (
		    matrices[i].m[j][k] = br.GetFloat()
		  )
		)
	  )
  )
)

-- loads correctly: count=113, offsetToW=20, offsetToD=134 [20 + 113 = 133 (nextBit = offsetToD)]
struct Drw1Header
(
  tag, -- char[4]
  sizeOfSection, -- u32 
  count, -- u16  
  pad, -- u16 

  --stores for each matrix if it's weighted (normal (0)/skinned (1) matrix types)
  offsetToIsWeighted, -- u32 

  --for normal (0) matrices, this is an index into the global matrix
  --table (which stores a matrix for every joint). for skinned
  --matrices (1), I'm not yet totally sure how this works (but it's
  --probably an offset into the Evp1-array)
  offsetToData, -- u32 
  
  fn LoadData br =
  (
  	tag = br.ReadFixedLengthString 4
	sizeOfSection = br.ReadDWORD()
	count = br.ReadWORD()
	pad = br.ReadWORD()
	offsetToIsWeighted = br.ReadDWORD()
	offsetToData = br.ReadDWORD()
  )
)

struct Drw1
(
  isWeighted = #(), -- std::vector<bool> isWeighted;
  data = #(), -- std::vector<u16> data;
  
  fn LoadData br =
  (
    drw1Offset = br.Position()
	
  	header = Drw1Header()
	header.LoadData br
	
	-- read bool array
	isWeighted = #() -- isWeighted.resize(h.count);
	br.SeekSet (drw1Offset + header.offsetToIsWeighted)
	for i = 1 to header.count do
	(
		v = br.GetByte() -- u8 v; fread(&v, 1, 1, f);
		
		if v == 0 then
      		isWeighted[i] = false
    	else if v == 1 then
      		isWeighted[i] = true
    	else
      		throw ("drw1: unexpected value in isWeighted array: " + (v as string))
	)
	
	-- read data array
	data = #() -- dst.data.resize(h.count);
	br.SeekSet (drw1Offset + header.offsetToData) 
	for i = 1 to header.count do
	(
	  data[i] = br.ReadWORD()
	)
	
  )
)

struct BckKey
(
  time, -- float 
  value, -- float 
  tangent -- float  //??
)

struct BckJointAnim
(
  scalesX = #(), -- std::vector<Key>
  scalesY = #(), -- std::vector<Key>
  scalesZ = #(), -- std::vector<Key>

  rotationsX = #(), -- std::vector<Key>
  rotationsY = #(), -- std::vector<Key>
  rotationsZ = #(), -- std::vector<Key>

  translationsX = #(), -- std::vector<Key>
  translationsY = #(), -- std::vector<Key>
  translationsZ = #() -- std::vector<Key>
)


------------------------------------

struct BckAnk1Header
(
  tag, -- char[4] 'ANK1'
  sizeOfSection, -- u32 

  -- 0 - play once, 2 - loop
  loopFlags, -- u8 

  angleMultiplier, -- u8 all angles have to multiplied by pow(2, angleMultiplyer)

  animationLength, -- u16 in time units

  numJoints, -- u16 that many animated joints at offsetToJoints
  scaleCount, --u16  that many floats at offsetToScales
  rotCount, -- u16 that many s16s at offsetToRots
  transCount, -- u16 that many floats at offsetToTrans

  offsetToJoints, -- u32 
  offsetToScales, -- u32 
  offsetToRots, -- u32 
  offsetToTrans, -- u32 
  
  fn LoadData br = 
  (
  	  tag = br.ReadFixedLengthString 4
  	  sizeOfSection = br.ReadDWORD()
	  loopFlags = br.GetByte()
	  angleMultiplier = br.GetByte()
	  animationLength = br.ReadWORD()
	  numJoints = br.ReadWORD()
	  scaleCount = br.ReadWORD()
	  rotCount = br.ReadWORD()
	  transCount = br.ReadWORD()
	  offsetToJoints = br.ReadDWORD()
	  offsetToScales = br.ReadDWORD()
	  offsetToRots = br.ReadDWORD()
	  offsetToTrans = br.ReadDWORD()
	
  )
)


-- TODO: the following two structs have really silly names, rename them
struct BckAnimIndex
(
  count, -- u16 
  index, -- u16 
  zero, -- u16 always 0?? -> no (biawatermill01.bck) TODO: find out what it means
  
  fn LoadData br =
  (
     count = br.GetSHORT()
	 index = br.GetSHORT()
	 zero = br.GetSHORT()
  )
)

struct BckAnimComponent
(
  s = BckAnimIndex(), -- AnimIndex scale
  r = BckAnimIndex(), -- AnimIndex rotation
  t = BckAnimIndex(), -- AnimIndex translation
  
  fn LoadData br =
  (
     s.LoadData br
	 r.LoadData br
	 t.LoadData br
  )
)

struct BckAnimatedJoint
(

  --if count > 1, count*3 floats/shorts stored at index (time, value, unk [interpolation info, e.g. tangent??])?
  --for shorts, time is a "real" short, no fixedpoint
  x = BckAnimComponent(), -- AnimComponent 
  y = BckAnimComponent(), -- AnimComponent 
  z = BckAnimComponent(), -- AnimComponent 
  
  fn LoadData br =
  (
	 x.LoadData br
	 y.LoadData br
	 z.LoadData br
  )
)

-----------------------------------------------

struct Bck
(
  anims = #(), -- std::vector<JointAnim>
  animationLength, -- int 
  currAnimTime, -- float 
  
  -- ConvRotation(vector<Key>& rots, float scale)
  fn ConvRotation rots scale =
  (
     for j = 1 to rots.count do
	 (
	     rots[j].value *= scale
		 rots[j].tangent *= scale
	 )
	 
	 return rots
  ),
  
  -- void readComp(vector<Key>& dst, const vector<T>& src, const bck::AnimIndex& index)
  fn ReadComp dst src index =
  (
     dst = #() -- dst.resize(index.count);
	 
	  -- violated by biawatermill01.bck
	  if index.zero != 0 then
	  (
	    -- throw "bck: zero field %d instead of zero" -- ignore for now?
	    --TODO: biawatermill01.bck doesn't work, so the "zero"
	    --value is obviously something important
	  )
	  
	  if index.count <= 0 then
	  (
	    print "Warning: readComp(): count is <= 0"
	  )
	  else if index.count == 1 then
	  (
	    dst[1] = BckKey()
	    dst[1].time = 0
	    dst[1].value = src[index.index + 1]
	    dst[1].tangent = 0
	  )
	  else
	  (
	    for j = 0 to (index.count - 1) do -- (int j = 0; j < index.count; ++j)
	    (
		  dst[j + 1] = BckKey()
	      dst[j + 1].time = src[(index.index + 3*j) + 1]
	      dst[j + 1].value = src[(index.index + 3*j + 1) + 1]
	      dst[j + 1].tangent = src[(index.index + 3*j + 2) + 1]
	    )
	  )
  
  	return dst
  ),
  
  fn LoadAnk1 br =
  (

      i = 0
	  ank1Offset = br.Position()
	  
	  -- read header
      h = BckAnk1Header()
	  h.LoadData br
	  currAnimTime = 0.0
	  animationLength = h.animationLength
	  
	  -- read scale floats:
	  br.SeekSet (ank1Offset + h.offsetToScales)
	  scales = #() -- vector<f32> scales(h.scaleCount);
	  for i=1 to h.scaleCount do
	      append scales (br.GetFloat())

	  -- read rotation s16s:
	  br.SeekSet (ank1Offset + h.offsetToRots)
	  rotations = #()
	  for i=1 to h.rotCount do
	       append rotations (br.GetSHORT())
		   
	  -- read translation floats:
	  br.SeekSet (ank1Offset + h.offsetToTrans)
	  translations= #() 
	  for i=1 to h.transCount do
	      append translations (br.GetFloat()) 

	  -- read joints
	  rotScale = (pow (2 as float) h.angleMultiplier) * 180 / (32768 as float)
	  br.SeekSet (ank1Offset + h.offsetToJoints)
	  
	  anims = #() -- bck.anims.resize(h.numJoints);
	  for i = 1 to h.numJoints do
	  (
	    joint = BckAnimatedJoint()
		joint.LoadData br 
		
		anims[i] = BckJointAnim()

	    anims[i].scalesX = ReadComp anims[i].scalesX scales joint.x.s
	    anims[i].scalesY = ReadComp anims[i].scalesY scales joint.y.s
	    anims[i].scalesZ = ReadComp anims[i].scalesZ scales joint.z.s
	
	    anims[i].rotationsX = ReadComp anims[i].rotationsX rotations joint.x.r
	    anims[i].rotationsX = ConvRotation anims[i].rotationsX rotScale
	    anims[i].rotationsY = ReadComp anims[i].rotationsY rotations joint.y.r
	    anims[i].rotationsY = ConvRotation anims[i].rotationsY rotScale
	    anims[i].rotationsZ = ReadComp anims[i].rotationsZ rotations joint.z.r
	    anims[i].rotationsZ = ConvRotation anims[i].rotationsZ rotScale
	
	    anims[i].translationsX = ReadComp anims[i].translationsX translations joint.x.t
	    anims[i].translationsY = ReadComp anims[i].translationsY translations joint.y.t
	    anims[i].translationsZ = ReadComp anims[i].translationsZ translations joint.z.t
	  )
	  
  ),
  
  fn LoadBck filePath =
  (
      local br = BinaryReader()
	  br.Open filePath 
	  br.SeekSet 0x20 
	  local size = 0
	  local i = 0
	  do
	  (
		 
		br.SeekCur size
	    local pos = br.Position()
	    local tag = br.ReadFixedLengthString 4
		size = br.ReadDWORD()

		if(size < 8) then
			size = 8 -- prevent endless loop on corrupt data

		br.SeekSet pos
		
	    --if tag == "ANK1" then
		if tag == "ANK1" then
	      LoadAnk1 br
	    else
		(
	      messageBox ("readBck(): Unsupported section " + tag)
			throw  ("readBck(): Unsupported section " + tag)
		)
		
	    br.SeekSet pos

		i += 1
	  ) while i < 1
	  
		br.Close()
  ),
  
  
  fn Interpolate v1 d1 v2 d2 t = -- t in [0,1]
  (
  	   --cubic interpolation
	   -- float values
	  a = 2*(v1 - v2) + d1 + d2
	  b = -3*v1 + 3*v2 - 2*d1 - d2
	  c = d1
	  d = v1
	  --TODO: yoshi_walk.bck has strange-looking legs...not sure if
  	 --the following line is to blame, though
  	 return ((a*t + b)*t + c)*t + d
  ),
  
  fn GetAnimValue keys t =
  (
	  if keys.count == 0 then
	    return 0.0
	
	  if keys.count == 1 then
	    return keys[1].value
	
	  --messageBox (keys as string)
	 -- throw "E"
	  i = 2
	  while keys[i].time < t do
	    i += 1
	
	  time = (t - keys[i - 1].time)/(keys[i].time - keys[i - 1].time) -- scale to [0, 1]
	  return interpolate keys[i - 1].value keys[i - 1].tangent keys[i].value keys[i].tangent time
  ),
  
-- the caller has to ensure that jnt1.frames and bck.anims contain
--the same number of elements
fn AnimateJnt jnt deltaTime =
(
  -- update time
  currAnimTime += deltaTime --*16 -- convert from seconds to ticks (dunno if this is right this way...TODO)
  currAnimTime = mod currAnimTime animationLength -- loop?

  -- update joints
  for i = 1 to jnt.frames.count do
  (
    jnt.frames[i].sx = getAnimValue anims[i].scalesX currAnimTime
    jnt.frames[i].sy = getAnimValue anims[i].scalesY currAnimTime
    jnt.frames[i].sz = getAnimValue anims[i].scalesZ currAnimTime

    --TODO: use quaternion interpolation for rotations?
    jnt.frames[i].rx = getAnimValue anims[i].rotationsX currAnimTime
    jnt.frames[i].ry = getAnimValue anims[i].rotationsY currAnimTime
    jnt.frames[i].rz = getAnimValue anims[i].rotationsZ currAnimTime

    jnt.frames[i].t.x = getAnimValue anims[i].translationsX currAnimTime
    jnt.frames[i].t.y = getAnimValue anims[i].translationsY currAnimTime
    jnt.frames[i].t.z = getAnimValue anims[i].translationsZ currAnimTime
  )
),

-- IMPORTANT: scale values are absolute and not related to the parent
-- e.g Bone A (scale=200%), Bone B (Scale=200%), Bone C (Scale=100%). Bone A is the parent of Bone B and Bone B is the parent of Bone C
--  need to remove the parent scaling. e.g Bone C shouldn't change in size but in 3DS max it will equal 400% (2 * 2 * 1 * 100)


fn GetParentBoneScale currBone frameTime  = 
(
	local parentScale = Point3 1 1 1
	at time (frameTime)
	(
		local parentBone = currBone.parent
		while (parentBone != undefined) do
		(
			parentScale.x *= (parentBone.scale.controller.x_scale / 100)
			parentScale.y *= (parentBone.scale.controller.y_scale / 100)
			parentScale.z *= (parentBone.scale.controller.z_scale / 100)
	
			parentBone = parentBone.parent
		)
	)

	--return 1
	return parentScale
),

-- CalcScale anims[i].scalesX parentBoneIndexs 8
fn _CalcParentScale boneIndex keys parentBoneIndexs frame =
(
	-- if keys.count == 0 then
	--    return 1.0 -- identity
	 if (boneIndex <= 0 ) then
		return 1 -- identity
	
	local val = getAnimValue keys frame -- absolute value
	if (val < 0.1 ) then
		throw "E"
	if (val > 10) then 
		throw "Max"
	val = 1 / val
	
	
	if (parentBoneIndexs[boneIndex] > 0) then
		return (_CalcParentScale parentBoneIndexs[boneIndex] keys parentBoneIndexs frame) * val
	else
		return val
),

-- only calc on first parent. ignore boneIndex
fn CalcParentScale boneIndex keys parentBoneIndexs frame =
(

	return 1
	-- if (boneIndex <= 0 ) then
	--	return 1 -- identity
	
	local val = getAnimValue keys frame -- absolute value
	if (val < 0.0000001 ) then
		throw "E"
	if (val > 100000) then 
		throw "Max"
	val = 1 / val
	return val
	
	--return _CalcParentScale parentBoneIndexs[boneIndex] keys parentBoneIndexs frame
),

-- gets total x scale (excluding self)
-- bck file stores the absolute scale at that point and should ignore all parent bone scaling
-- e.g. bone a (200%) -> bone b (200%) -> bone C (200%).
-- bone a (1 * 2 * 100), bone b ((1 / 2 (bone a scale)) * 2 * 100 = 50 %), bone c (1/2 * 1/2 * 100 = 25%)
-- however, the parent bone is already scaled based on all items before it so only the parents scale is required. e.g. bone c (1/2 * 100 = 50) because bone b is already at 50%, total scale = 50%*50%=25%
-- WARNING: skewed bones?
fn CalcParentXScale anims parentBoneIndex parentBoneIndexs frame =
(
	--	return 1
	if (parentBoneIndexs[parentBoneIndex] <= 0 ) then -- root bone
		return 1 -- identity
	--return  (getAnimValue anims[parentBoneIndex].scalesX frame)
	
	local val = 1 / (getAnimValue anims[parentBoneIndex].scalesX frame) -- absolute value
	return val
	--return (CalcParentXScale anims parentBoneIndexs[parentBoneIndex] parentBoneIndexs frame) * val
),

fn CalcParentYScale anims parentBoneIndex parentBoneIndexs frame =
(
	--return 1
	if (parentBoneIndexs[parentBoneIndex] <= 0 ) then -- root bone
		return 1 -- identity
	--return  (getAnimValue anims[parentBoneIndex].scalesY frame)
	local val = 1 / (getAnimValue anims[parentBoneIndex].scalesY frame) -- absolute value
	return val
	--return (CalcParentYScale anims parentBoneIndexs[parentBoneIndex] parentBoneIndexs frame) * val
),

fn CalcParentZScale anims parentBoneIndex parentBoneIndexs frame =
(
	--return 1
	if (parentBoneIndexs[parentBoneIndex] <= 0 ) then -- root bone
		return 1 -- identity
	--return  (getAnimValue anims[parentBoneIndex].scalesZ frame)
	local val = 1 / (getAnimValue anims[parentBoneIndex].scalesZ frame) -- absolute value
	return val
	--return (CalcParentZScale anims parentBoneIndexs[parentBoneIndex] parentBoneIndexs frame) * val
),

fn GetPositionBone curBone =
(
	local dummyBone = (getNodeByName (curBone.name + "_dummy"))
	if dummyBone == undefined then
		return curBone
	else
		return dummyBone
),	

fn ValidateScale curBone scaleValue =
(
		--if (scaleValue != 1 and curBone.children.count > 1) then
		--	throw (curBone.name + " unable to scale ( " +(scaleValue as string)+" ) bones with more than one child bone")
),


-- could use timeOffset to load all animations one after another
-- parentBoneIndexs array of ints. e.g. parentBoneIndexs[2] = 1 (2nd bones parent is the first bone)
fn AnimateBoneFrames timeOffset bones frameScale rootBoneOffset exportType refBoneRequiresDummyList includeScaling =
(
	
	if (exportType == #CHARACTER AND animationLength > 0) then
		timeOffset = 0

	--alert (bones.count as string)
	rootBoneOffset = [0,0,0] 
	animate on
	(  
	  
		for i = 1 to bones.count do
		(  
			
	
			bone = bones[i]
			anim = anims[i]
			
			-- animated bones require scaling helper
			if (anim.scalesX.count > 1 OR anim.scalesY.count > 1 OR anim.scalesZ.count > 1) then
				refBoneRequiresDummyList[i] =true -- bone.name
				
			if (anim.translationsX.count > 1 OR anim.translationsY.count > 1 OR anim.translationsZ.count > 1) then
				bone.boneEnable=false -- allow moving bone without affecting parent bone
				
				--messageBox (anim.scalesX as string) -- only one value if position not animated. value = 0
				--messageBox (anim.translationsY as string) -- only one value if scale not animated. value = 1
			
			for j = 1 to anim.rotationsX.count do
			(
				rot = anim.rotationsX[j]
				at time ((rot.time * frameScale) + timeOffset) (bone.rotation.controller.x_rotation = rot.value )
			)
			
			for j = 1 to anim.rotationsY.count do
			(
				rot = anim.rotationsY[j]
				at time ((rot.time * frameScale) + timeOffset) (bone.rotation.controller.y_rotation = rot.value )
			)
			
			for j = 1 to anim.rotationsZ.count do
			(
				rot = anim.rotationsZ[j]
				at time ((rot.time * frameScale) + timeOffset) (bone.rotation.controller.Z_rotation = rot.value )
			)
			

			for j = 1 to anim.translationsX.count do
			(
				t  = anim.translationsX[j]
				at time ((t.time * frameScale) + timeOffset) ((GetPositionBone bone).position.controller.x_position = t.value - rootBoneOffset.x)
			)
				
			for j = 1 to anim.translationsY.count do
			(
				t  = anim.translationsY[j]
				at time ((t.time * frameScale) + timeOffset) ((GetPositionBone bone).position.controller.y_position = t.value - rootBoneOffset.y)
			)
				
			for j = 1 to anim.translationsZ.count do
			(
				t  = anim.translationsZ[j]
				at time ((t.time * frameScale) + timeOffset) ((GetPositionBone bone).position.controller.z_position = t.value - rootBoneOffset.z )
			)

			if (includeScaling) then
			(
				for j = 1 to anim.scalesX.count do
				(
					local s  = anim.scalesX[j]
					at time ((s.time * frameScale) + timeOffset) 
					(
						ValidateScale bone s.value
						in coordsys local (bone.scale.controller.x_scale =  s.value * 100)
					)
				)
				for j = 1 to anim.scalesY.count do
				(
					local s  = anim.scalesY[j]
					at time ((s.time * frameScale) + timeOffset) 
					(
						ValidateScale bone s.value
						in coordsys local (bone.scale.controller.y_scale =  s.value * 100)
					)
				)
				for j = 1 to anim.scalesZ.count do
				(
					local s  = anim.scalesZ[j]
					at time ((s.time * frameScale) + timeOffset) 
					(
						in coordsys local 
						(
							ValidateScale bone s.value
							in coordsys local (bone.scale.controller.z_scale =  s.value * 100)
						)
					)
				)
			)
			
			rootBoneOffset = [0,0,0] -- only the root bone has an offset. bones[1]
		) -- for i = 1 to bones.count do
	
		  -- IMPORTANT: set all transforms for the last frame. prevents errors when frames loaded on after another animation
		if (animationLength > 0) then
		(
			local endFrame = timeOffset + animationLength
			for i = 1 to bones.count do
			(  
				local bone = bones[i]
				local anim = anims[i]

				addNewKey bone.rotation.controller endFrame
				addNewKey bone.position.controller endFrame
				addNewKey bone.scale.controller endFrame
				-- only seems to create a new keyframe if the value changes (+ 0.0000000000000001)
				local delta = 0.0000001
				at time (endFrame) (bone.rotation.controller.x_rotation = (anim.rotationsX[anim.rotationsX.count]).value + delta) --
				at time (endFrame) (bone.rotation.controller.y_rotation = (anim.rotationsY[anim.rotationsY.count]).value + delta) --+ delta
				at time (endFrame) (bone.rotation.controller.z_rotation = (anim.rotationsZ[anim.rotationsZ.count]).value + delta) --+delta

				at time (endFrame) 
				(
					local posBone = GetPositionBone bone
					posBone.position.controller.x_position = (anim.translationsX[anim.translationsX.count]).value  + delta
					posBone.position.controller.y_position = (anim.translationsY[anim.translationsY.count]).value  + delta
					posBone.position.controller.z_position = (anim.translationsZ[anim.translationsZ.count]).value  + delta
					
					if (includeScaling) then
					(
							in coordsys local (bone.scale.controller.x_scale =  ((anim.scalesX[anim.scalesX.count]).value * 100) + delta)
							in coordsys local (bone.scale.controller.y_scale =  ((anim.scalesY[anim.scalesY.count]).value * 100) + delta)
							in coordsys local (bone.scale.controller.Z_scale =  ((anim.scalesZ[anim.scalesZ.count]).value * 100) + delta)
					)
				)
			) -- for i = 1 to bones.count do
		) -- if (animationLength > 0) then*/
	
		
		if (exportType == #CHARACTER AND animationLength > 0) then
		(
			if (animationLength > 0) then
				animationRange = interval 0 (animationLength * frameScale)
			else
				animationRange = interval 0 1
		)

	) -- animate on

),

	
		
-- deltaTime in ticks
fn AnimateBones bones deltaTime =
(
  -- update time
  currAnimTime += deltaTime -- *16 -- convert from seconds to ticks (dunno if this is right this way...TODO)
  currAnimTime = mod currAnimTime animationLength -- loop?

  -- update joints
  for i = 1 to bones.count do
  (
  	bone = bones[i]
	
    --TODO: use quaternion interpolation for rotations?
    rx = getAnimValue anims[i].rotationsX currAnimTime
    ry = getAnimValue anims[i].rotationsY currAnimTime
    rz = getAnimValue anims[i].rotationsZ currAnimTime
	bone.rotation.controller.x_rotation = rx
	bone.rotation.controller.y_rotation = ry
	bone.rotation.controller.z_rotation = rz
	
    tx = getAnimValue anims[i].translationsX currAnimTime
    ty = getAnimValue anims[i].translationsY currAnimTime
    tz = getAnimValue anims[i].translationsZ currAnimTime
	bone.position.controller.x_position = tx
	bone.position.controller.y_position = ty
	bone.position.controller.z_position = tz
	
	sx = getAnimValue anims[i].scalesX currAnimTime
    sy = getAnimValue anims[i].scalesY currAnimTime
    sz = getAnimValue anims[i].scalesZ currAnimTime
	bone.scale.controller.x_scale = sx * 100
	bone.scale.controller.y_scale = sy * 100
	bone.scale.controller.z_scale = sz * 100
  )
),

-- from stdplugs/stdscripts/CharacterPluginObject.ms
fn resetAnim fromAnim =
(
	if fromAnim.controller != undefined then
		deleteKeys fromAnim.controller #allKeys			
	
	-- delete anims from custom attributes
	for k=1 to (custAttributes.count fromAnim) do 
	(
		local ca = custattributes.get fromAnim k 
		if (ca != undefined) do
		(
			local saNames = getSubAnimNames ca

			for s=1 to saNames.count do 
			(
				if (ca[s].controller != undefined) do (
					deleteKeys ca[s].controller #allKeys
				)	
			)
		)
	)
	
	for j=1 to fromAnim.numSubs do
	(
		resetAnim fromAnim[j]
	)
),

-- from stdplugs/stdscripts/CharacterPluginObject.ms
fn resetNodeAnim node = 
(
	resetAnim node.controller
	resetAnim node.baseObject
	for m in node.modifiers do (
		resetAnim m
	)
),

fn DeleteAllKeys bones =
(
	if (animationLength > 0) then
	(
			-- move all the keys back by one frame (extra frame was required to prevent export / import bug)
			for i = 1 to bones.count do
			( 
				local b = bones[i]
				selectKeys b.rotation.controller  (interval 1 (animationLength+5))
				moveKeys  b.rotation.controller (-animationLength+1) #selection
				deleteKeys  b.rotation.controller #selection
				
				selectKeys b.position.controller  (interval 1 (animationLength+5))
				moveKeys  b.position.controller (-animationLength+1) #selection
				deleteKeys  b.position.controller #selection
				
				selectKeys b.scale.controller  (interval 1 (animationLength+5))
				moveKeys  b.scale.controller (-animationLength+1) #selection
				deleteKeys  b.scale.controller #selection
				
				--moveKeys  b -animationLength
			)
	)
)

)



struct Mat3Header
(
   tag, -- char[4] 'MAT3'
  sizeOfSection, -- u32 
   count, -- u16
   pad, -- u16

  /*
    0 - MatInit array
    1 - array of shorts with 0, 1, 2, ..., count (nearly...sometimes small deviations) -> index into offset[0]
    2 - string table
    3 - divisible by Mat3Header.count - so the Mat3Entries are stored here
    15 - index to texture table?
  */
  offsets = #(), -- u32 [30]
	
	fn LoadData br =
	(
		tag = br.ReadFixedLengthString 4
		sizeOfSection = br.ReadDWORD()
		count = br.ReadWORD()
		pad = br.ReadWORD()
		
		for i = 1 to 30 do
			offsets[i] = br.ReadDWORD()
	)
)

struct MatInit
(
   unknown1 = #(), -- u8 [132]
   texStages = #(), -- u16[8]
  unknown2 = #(), -- u8 [332 - 132 - 8*2]
	
	fn LoadData br =
	(
		for i = 1 to 132 do
			unknown1[i] = br.GetByte()
		
		for i = 1 to 8 do
			texStages[i] = br.ReadWORD()
		
		u2Count = 332 - 132 - 8*2
		for i = 1 to u2Count do
			unknown2[i] = br.GetByte()
	)
)



struct MatEntry
(
	/*
	  //(0 - possible values: 1, 4, 253 [1: draw on tree down, 4 on up??])
  //     - related to transparency sorting
  //1 - index into cullModes
  //2 - index into numChans
  //3 - index into texgen counts
  //4 - index into tev counts
  //5 - index into matData6 (?)
  //6 - index into zModes? (quite sure)
  //7 - index into matData7 (?)
  //(still missing stuff: isDirect, zCompLoc,
  //enable/disable blend alphatest depthtest, ...)*/
  unk = #(), -- u8[8];

  -- 0, 1 - index into color1 (e.g. map_delfino3.bmd)
  -- 6, 7 - index into color2 (e.g. mo.bdl)
  -- 2, 3, 4, 5 - index into chanControls
  chanControls = #(), -- u16[8];

  color1 = #(), --u16[2];
  -- chanControls = #(), -- u16[4];
  color2 = #(), --u16[2]; //not in MAT2 block

  lightList = #(), --lights u16[8]; //all 0xffff most of the time, not in MAT2 block

  texGenInfo = #(), --u16[8];
   texGenInfo2 = #(), --u16[8];

   texMatrices = #(), --u16[10]; //direct index
   dttMatrices = #(), --u16[20]; //?? (I have no idea what dtt matrices do...)

   texStages = #(), --u16[8]; //indices into textureTable

  --constColor (GX_TEV_KCSEL_K0-3)
   color3 = #(), --u16[4]; //direct index

   constColorSel = #(), --u8[16]; //0x0c most of the time (const color sel, GX_TEV_KCSEL_*)
   constAlphaSel = #(), --u8[16]; //0x1c most of the time (const alpha sel, GX_TEV_KASEL_*)

   tevOrderInfo = #(), --u16[16]; //direct index

  --this is to be loaded into
  --GX_CC_CPREV - GX_CC_A2??
   colorS10 = #(), --u16[4]; //direct index


  --these two always contained the same data in all files
  --I've seen...
   tevStageInfo = #(), --u16[16]; //direct index
   tevSwapModeInfo = #(), --u16[16]; //direct index

   tevSwapModeTable = #(), --u16[4];
  
  
   unknown6 = #(), --u16[12]; //vf_118 has a float in here (but only in one block...)
  --f32 unknown6[6];

  --0 - fog index (vf_117.bdl)
  --1 - alphaComp (vf_117.bdl, yoshi.bmd)
  --2 - blendInfo (cl.bdl)
  --3 - nbt scale?
   indices2 = #(), --u16[4];
	
	fn LoadData br isMat2 =
	(
		for i = 1 to 8 do unk[i] = br.GetByte()
		for i = 1 to 2 do color1[i] = br.ReadWORD()
		for i = 1 to 4 do chanControls[i] = br.ReadWORD()
		 
  
		--these two fields are only in mat3 headers, not in mat2
		if (not isMat2) then
			for i = 1 to 2 do color2[i] = br.ReadWORD()
		else
			throw "isMat2 header NYI"-- memset(init.color2, 0xff, 2*2);

		if (not isMat2) then
			for i = 1 to 8 do lightList[i] = br.ReadWORD()
		else
			throw "isMat2 header NYI"-- memset(init.lights, 0xff, 8*2);
		
		for i = 1 to 8 do texGenInfo[i] = br.ReadWORD()
		for i = 1 to 8 do texGenInfo2[i] = br.ReadWORD()
		for i = 1 to 10 do texMatrices[i] = br.ReadWORD()
		for i = 1 to 20 do dttMatrices[i] = br.ReadWORD()
		for i = 1 to 8 do texStages[i] = br.ReadWORD() 
		for i = 1 to 4 do color3[i] = br.ReadWORD()
		for i = 1 to 16 do constColorSel[i] = br.GetByte()
		for i = 1 to 16 do constAlphaSel[i] = br.GetByte()
		for i = 1 to 16 do tevOrderInfo[i] = br.ReadWORD()
		for i = 1 to 4 do colorS10[i] = br.ReadWORD()
		for i = 1 to 16 do tevStageInfo[i] = br.ReadWORD()
		for i = 1 to 16 do tevSwapModeInfo[i] = br.ReadWORD()
		for i = 1 to 4 do tevSwapModeTable[i] = br.ReadWORD()
		for i = 1 to 12 do unknown6[i] = br.ReadWORD()
		for i = 1 to 4 do indices2[i] = br.ReadWORD()
  )
  
)

struct Mat3
(
  -- temporary, maps mat index to tex index
  texTable = #(), -- std::vector<int> 
	stringtable,
	indexToMatIndex = #(),
	-- _texStageIndexToTextureIndex, -- used by btp
	materials = #(), -- MatEntry array
	texStageIndexToTextureIndex = #(),
	
	fn LoadData br =
	(
		-- "Mat3 section support is very incomplete"
		
		mat3Offset = br.Position()

	  -- read header
		h = Mat3Header()
		h.LoadData br
		 
		local isMat2 = (h.tag == "MAT2")

		stringtable = br.ReadStringTable (mat3Offset + h.offsets[3]) -- readStringtable(mat3Offset + h.offsets[2], f, stringtable);

		if h.count != stringtable.count then
			throw "mat3: number of strings (%d) doesn't match number of elements (%d)"

		  -- compute max length of each subsection
		  lengths = #() -- vector<int> lengths(30);
		  
		   for i = 1 to 30 do
		  (
				len = 0
				if h.offsets[i] != 0 then
				(
					next = h.sizeOfSection
				
					  for j = i + 1 to 30 do
					  (
							if h.offsets[j] != 0 then
							(
							  next = h.offsets[j]
							  break
							)
							
					  )
					  len = next - h.offsets[i]
				)
				
				lengths[i] = len
				if (i == 3) then
				(
				  -- assert(length%h.count == 0); //violated by luigi's mansion files
				  -- assert(length/h.count == 312); //violated by quite a few files
				)
		  )
		
		  ------------------
		  br.SeekSet (mat3Offset + h.offsets[1]) -- offset[0] (MatEntries)
		  materials = #() -- vector<int> indexToInitData(h.count); ' indexToMatIndex
		  for i = 1 to h.count do
		  (
				local m = MatEntry()
				m.LoadData br isMat2
				append materials  m
		  )
		  
		  ------------------
		  br.SeekSet (mat3Offset + h.offsets[2]) -- offset[1] (indirection table from indices to init data indices)
		  maxIndex = 0
		  indexToInitData = #() -- vector<int> indexToInitData(h.count); ' indexToMatIndex
		  for i = 1 to h.count do
		  (
				bla = br.ReadWORD()
				if (bla > maxIndex) then
					maxIndex = bla
				indexToInitData[i] = bla
			  indexToMatIndex[i] = bla
		  )
		  --indexToMatIndex = indexToInitData
		  
		  br.SeekSet (mat3Offset + h.offsets[1])

		  initData = #() -- vector<bmd::MatInit> initData(maxIndex + 1)
		  
		  for i = 1 to (maxIndex + 1) do -- for(i = 0; i <= maxIndex; ++i)
		  (
				init = MatInit()
				init.LoadData br
			  initData[i] = init
		  )

		  -- read texTable
		br.SeekSet (mat3Offset + h.offsets[16]) --  fseek(f, mat3Offset + h.offsets[15], SEEK_SET);
		   
		   texLength = lengths[16]
		  local tempTexTable = #() -- vector<int> texTable(texLength/2);  texStageIndexToTextureIndex
		   maxTexIndex = 0;
			  
		  for i = 1 to (texLength/2) do
		  (
			index = br.ReadWORD()
			texTable[i] = index
			texStageIndexToTextureIndex[i] = index
			--if (index > maxTexIndex) then
		  )
		-- messageBox (texTable as string)
		  /*
		for i = 1 to tempTexTable.count do
		  (
				index = br.ReadWORD()
			  texTable[i] = tempTexTable[i]
			  
			 -- if (index > maxTexIndex) then
			--	maxTexIndex = index
		  )*/
		  
		 /*
		  messageBox (initData[1].texStages.count as string)
		  for i = 1 to h.count do
		  (
				stage =  initData[indexToInitData[i] + 1].texStages[1] 
				if (stage != 0xffff) then
				  texTable[i] = texTable[stage + 1]
				else
				  texTable[i] = 0xffff
		  )*/
  
	)
)

struct ImageHeader
(
  data, -- Image* 
  name, -- std::string 

  /*
    from gx.h:
    0: clamp to edge
    1: repeat
    2: mirror
  */
   wrapS, wrapT -- u8

  --TODO: unknow fields
)

------------------------------------------------------------------------------------------------

struct Image
(
   format, -- int
   width, height, -- int

  mipmaps = #(), --std::vector<u8*>  points into imageData
  sizes = #(), -- std::vector<int> image data size for each mipmap
  imageData = #(), -- std::vector<u8> 

	/*
  //NOTE: palettized images are converted
  //to non-palettized images during load time,
  //i4 and i4a4 are converted to i8 and i8a8
  //(i8a8 is then converted to a8i8 for opengl),
  //r5g5b5a3 and r5g6b5 to rgba8 (actually, to agbr8,
  //that is rgba8 backwards - for opengl. rgba8
  //is converted to agbr8 as well).
  //(that is, only formats 1, 3, 6 and 14 are
  //used after conversion)
*/
  --TODO: gl image conversions (rgba -> abgr, ia -> ai
  --somewhere else?)

  --TODO: this is temporary and belongs somewhere else:
  texId -- unsigned int 			
)



------------------------------------------------------------------------------------------------

-- header format for 'bmd3' files, seems to be slightly different for 'jpa1'
struct Tex1Header
(
  tag, --char [4]  'TEX1'
   sizeOfSection, -- u32
   numImages, -- u16
   unknown, -- u16 padding, usually 0xffff
   textureHeaderOffset, -- u32numImages bti image headers are stored here (see bti spec)
                           --note: several image headers may point to same image data
                           --offset relative to Tex1Header start

   stringTableOffset, -- u32stores one filename for each image (TODO: details on stringtables)
                           --offset relative to Tex1Header start  
	
	fn LoadData br =
	(
		tag = br.ReadFixedLengthString 4
		sizeOfSection = br.ReadDWORD()
		numImages = br.ReadWORD()
		unknown = br.ReadWORD()
		textureHeaderOffset = br.ReadDWORD()
		stringTableOffset = br.ReadDWORD()
	)
	
)

------------------------------------------------------------------------------------------------

struct TextureHeader
(
   format, -- u8data format - seems to match tpl's format (see yagcd)
   unknown, -- u8
  width, -- u16 
   height, -- u16

  /*
    from gx.h:
    0: clamp to edge
    1: repeat
    2: mirror
  */
   wrapS, -- u8
  wrapT, -- u8 

 unknown3, --   u8
  paletteFormat, -- u8 palette format - matches tpl palette format (-> yagcd)
   paletteNumEntries, -- u16
   paletteOffset, -- u32 palette data


   unknown5, -- u32
   unknown6, -- u16 prolly two u8s, first is 5 or 1, second 1 most of the time
   unknown7, -- u16 0 most of the time, sometimes 0x10, 0x18, 0x20, 0x28
   mipmapCount, -- u8
   unknown8, -- u8
   unknown9, -- u16

   dataOffset, -- u32 image data

  --some of the unknown data could be render state?
  --(lod bias, min/mag filter, clamp s/t, ...)
  fn LoadData br = 
  (
	     format = br.GetByte()
	   unknown = br.GetByte()
	  width = br.ReadWORD()
	   height = br.ReadWORD()
	   wrapS = br.GetByte()
	  wrapT = br.GetByte()
	 unknown3 = br.GetByte()
	  paletteFormat = br.GetByte()
	   paletteNumEntries = br.ReadWORD()
	   paletteOffset = br.ReadDWORD()
	   unknown5 = br.ReadDWORD()
	   unknown6 = br.ReadWORD()
	   unknown7 = br.ReadWORD()
	   mipmapCount = br.GetByte()
	   unknown8 = br.GetByte()
	   unknown9 = br.ReadWORD()
	   dataOffset = br.ReadDWORD()
  )

)


------------------------------------------------------------------------------------------------

struct Tex1
(
  --imageHeaders = #(), -- std::vector<ImageHeader> 
texHeaders = #(),
stringtable,
  --because several image headers might point to the
  --same image data, this data is stored
  --separately to save some memory
  --(this way only about 1/6 of the memory required
  --otherwise is used)
 -- images = #(), -- std::vector<Image > 
	
	fn LoadData br =
	(
		tex1Offset = br.Position()
		
		  -- read textureblock header
		  h = Tex1Header()
		h.LoadData br

		  -- read stringtable
		stringtable = br.ReadStringTable (tex1Offset + h.stringTableOffset) -- readStringtable(tex1Offset + h.stringTableOffset, f, stringtable);
		
		  if(stringtable.count != h.numImages) then
			throw "tex1: number of strings doesn't match number of images"
			
		  -- read all image headers before loading the actual image
		  -- data, because several headers can refer to the same data
		br.SeekSet (tex1Offset + h.textureHeaderOffset)
		 
		texHeaders = #()
		imageOffsets = #()
		for i = 1 to h.numImages do
		(
			texHeader = TextureHeader()
			texHeader.LoadData br
			texHeaders[i] = texHeader
		)
		
	)
)


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




struct FrameNode
(
    name,
	startPoint, -- end point = first child
	children = #(), -- FrameNode
	 parentFrameNode,
	 effP,
	f,
	 _bone,
	_eulerController, -- used for animations

	_dummyHelper,
	
	--_dummyHelperRequired = false,
	
	fn RemoveDummyHelper =
	(
		if (_bone != undefined AND _dummyHelper != undefined) then
		(
			_bone.parent = _dummyHelper.parent
			delete _dummyHelper
			_dummyHelper = undefined
		)
	),
	
	-- required for character assembly
	fn _GetAllNodes nodes =
	(
		if (_bone != undefined) then
		(
			append nodes _bone 
			if (_dummyHelper != undefined) then
				append nodes _dummyHelper -- required 
		)
		
		for child in children do
		   child._GetAllNodes nodes
	),
	
	fn GetAllNodes =
	(
		local retNodes = #()
		_GetAllNodes(retNodes)
		return retNodes
	),
	
	-- parent scale has no effect on child bone (prevents skewing)
	-- can move child bone without auto scaling parent bone (face animation)
	-- should only scale when the parent bone only has one child?
	fn FixBones boneThickness =
	(
		/*
		local parent = _bone
		
		if (parent != undefined) then
		(
			local x = XForm()
			addModifier parent x 
			x.gizmo.scale = [1, 1, 1] -- Gizmo
			parent.boneScaleType = #none -- don't squash bones
		)
		for childFrame in children do
		(
			local child = childFrame._bone
			-- IMPORTANT: only use local scale on XForm object
			-- Works with IK
			-- NOTE: only updates position on x scale. e.g. body / arms: arms don't move outward when chest scaled on y axis
			if (parent != undefined) then
			(
				local d = distance child.pos  parent.pos
				paramWire.connect parent.modifiers[#XForm].Gizmo.controller[#Scale] child.pos.controller[#X_Position] ((d as string) + " * (Scale.x)")
			)
			childFrame.FixBones boneThickness
		)*/
	
		--local parentBone = _bone
		for child in children do
		(
			if (_bone != undefined) then
			(
				local childBone = child ._bone

				local d  = dummy boxsize:[boneThickness, boneThickness, boneThickness] name: (childBone.name +"_dummy")
				--local d = point size:boneThickness name: (childBone.name +"_dummy")

				
				d.transform = _bone.transform -- not rotation
				d.position = childBone.position -- end points should match parent direction? Watch out for multi child bones e.g. back -> 2 shoulders (back shouldn't scale)
				
				-- in coordsys world (d.position.x = child.position.x) -- only move along x axis?
				d.parent = _bone
				childBone.parent = d
				--in coordsys parent (child.position = [0,0,0] ) -- using dummy position instead
				
				
				paramWire.connect _bone.transform.controller[#Scale] d.transform.controller[#Scale] "1/Scale"
				freeze d -- don't hide or .x export won't work
				--hide d
				child._dummyHelper = d
			--)
			)
			child.FixBones boneThickness
		)
	),

	-- private
	 fn _PrintTree depth =
	(
	   if (name != undefined) then
	   		print (depth + name)
		
		for child in children do
		   child._PrintTree (depth + "--")
	),
	
	 -- used for testing
	 fn PrintTree =
	(
		_PrintTree ""
	),
	

	
	-- private
	fn _CreateBones parentBone boneThickness createdBonesTable postfixName parentTransform  =
	(
	     local bone = undefined
		 local endPoint = undefined
		 
	     if (parentFrameNode != undefined) then
		 (
			   if (children.count > 0 ) then
			 (
	      		endPoint = children[1].startPoint
			 )
			 
			 else
			(
				-- create an end point bone (same direction as parent bone with a length of boneThickness)
			    local start = startPoint 
				if (parentBone != undefined) then		-- THIS FIXES IMPORT OF MODELS WITH ONE BONE
					dir  = normalize (parentBone.position - start) 
				else
					dir  = [0,0,0]
				dir  *= (-1 * boneThickness)
				dir += startPoint 
				endPoint = [dir.x, dir.y, dir.z]
				--endPoint = [dir.x, -dir.z, dir.y] -- using orig cords
			)
		   		
			
			 
		   if (parentBone == undefined) then
			(
				endPoint = [startPoint.x, startPoint.y + boneThickness, startPoint.z] 
			)
			
			--startPoint = [0,0,0]
			--endPoint = [10, 0, 0]
			
			 bone = bonesys.createbone startPoint endPoint z_axis
		--	freeze bone -- don't hide or .x export won't work
			_bone = bone
			
			--_bone.boneFreezeLength=false -- prevent scale errors on animations. e.g. talking animations scale head?
			--_bone.boneAutoAlign=false
			
			_bone.scale.controller = ScaleXYZ ()
			
			
			mTransform = matrix3 1 -- identity
			mt = transMatrix [f.t.x, f.t.y, f.t.z]
			mx = rotateXMatrix f.rx
			my = rotateYMatrix f.ry
			mz = rotateZMatrix f.rz
			
			if (parentBone != undefined) then
			(
				mTransform =  (mx * my * mz * mt) * parentTransform
				
				bone.transform = mTransform
				bone.parent = parentBone
			)
			else
			(
				mTransform =  (mx * my * mz * mt) 
				bone.transform = mTransform
			)
			
			  bone.name = name + postfixName 
			  bone.width = boneThickness
			  bone.height = boneThickness
			  append createdBonesTable bone
	 
		 )		 
		 
		 for child in children do
		   child._CreateBones bone boneThickness createdBonesTable postfixName  mTransform 
	),
	
	
	-- private: NYI
	fn _FixBoneLength =
	(
		for childBone in children do
		(
		
			if (_bone != undefined AND _bone.parent != undefined) then
			(
				mt = transMatrix [f.t.x, f.t.y, f.t.z]
				mx = rotateXMatrix f.rx
				my = rotateYMatrix f.ry
				mz = rotateZMatrix f.rz
				
				parentVec = normalize ((in coordsys world _bone.position) - (in coordsys world _bone.parent.position))
			
				mTransform =  (mx * my * mz * mt) 
				
				boneVec2 = normalize ((normalize _bone.dir) * mTransform)
				
				print (((parentVec as string) + ":" + (boneVec2 as string)))
			)
			
			childBone._FixBoneLength()
		)
	),
	
	fn _ToArray items = 
	(
		 
		for child in children do
		(
			append items child
		   child._ToArray items
		)
	),
	
	fn ToArray =
	(
		local items = #()
		_ToArray items
		return items
	),
	
	fn _CreateParentBoneIndexs items itemIndex parentIndex depth =
	(
		
		append items parentIndex

		
		parentIndex = itemIndex[1] 
		--if (name != undefined) then -- first item is undefined
		--	print (depth + name + ":" + (parentIndex as string) + ":" + (itemIndex[1] as string))
		
		itemIndex[1] = itemIndex[1] + 1
		for child in children do
		   child._CreateParentBoneIndexs items itemIndex parentIndex (depth + "--")
		
		
	),
	
	fn CreateParentBoneIndexs =
	(
		local items = #()
		local itemIndex = #() -- only contains one value. Pass by reference?
		itemIndex[1] = 0
		_CreateParentBoneIndexs items itemIndex 0 "--"
		
		deleteItem  items 1 -- first item not used
		return items
	),
	
	-- returns new bones array
	fn CreateBones boneThickness postfixName  =
	(
		with animate off
		(
			if postfixName == undefined then
				postfixName = ""
		   createdBonesTable = #()
			
			mTransform = matrix3 1 -- identity
				
			_CreateBones undefined boneThickness createdBonesTable postfixName mTransform 
			
			
		)
		
		return createdBonesTable 
	),
	
	fn _RemapBones boneSet =
	(
		_bone = getNodeByName name
		if (_bone != undefined) then
			_dummyHelper = getNodeByName(_bone.name +"_dummy")
		
		append boneSet _bone 
		for child in children do
		   child._RemapBones boneSet
	),
	
	-- used on hold, fetch. Bone references lost.
	fn RemapBones =
	(
		local boneSet = #()
		 for child in children do
		   child._RemapBones boneSet
		 
		 return boneSet
	),
	
		
	fn ResetControllers =
	(
		  if (_bone != undefined ) then
			(
				if (_bone.parent != undefined ) then 
				(
					m = Euler_XYZ()
					/*
					m.x_rotation = f.rx
					m.y_rotation = f.ry
					m.z_rotation = f.rz
					*/
					_bone.scale.controller = ScaleXYZ () -- resets animations
					
					_eulerController = m
					_bone.rotation.controller = m 	

					pos = Position_XYZ()
					/*pos.x_position = f.t.x 
					pos.y_position = f.t.y
					pos.z_position = f.t.z*/
					
					_bone.position.controller = pos
				)
			)
			
		 for child in children do
		   child.ResetControllers()
	)
	
	
)

-- from ui/macroscripts/Macro_BoneAdjustmentsTools. on ReassignRoot_btn pressed do
fn removeIKsolvers a =
(
   if ( (not IsProperty a "pos") or (not IsProperty a "rotation") ) do
   (
	 HDIKSys.RemoveChain a
   )
)

fn getEndPoint a =
(
	if ( classOf(a) == BoneGeometry ) then
	(
	   [a.length,0,0] * a.objectTransform
	)
	else
	(
	   (a.transform).translation
	)
)

fn copyBoneHeightWidth destination source =
(
	if ( (source != undefined) and (classOf(source) == BoneGeometry) ) do
	(
		destination.width   = source.width
		destination.height  = source.height
	)
)

fn ReassignRoot currentBone =
(
	-- messageBox (currentBone as string)
		undo "Reassign Root" on
		(			
			with redraw off
			(
				with animate off
				(
					local deleteBoneArr = #()
					-- local currentBone   = selection[1]
					local selBone       = undefined
					local chlBone       = undefined
					local parentBone    = currentBone.parent
					local prevBone      = undefined
					local newBone       = undefined
					local newBoneArr    = #()
					local endBone       = undefined
					local revReset
					local exPrevBone    = undefined
					local i
						
					fn isPosSame a b =
					(
						local posTol = 5
						v1=a
						v2=b
						vi=0
						
						if ((v1.x) <= (v2.x  + posTol)) and ((v1.x) >= (v2.x  - posTol)) then vi +=1
						if ((v1.y) <= (v2.y  + posTol)) and ((v1.y) >= (v2.y  - posTol)) then vi +=1
						if ((v1.z) <= (v2.z  + posTol)) and ((v1.z) >= (v2.z  - posTol)) then vi +=1
						
						if vi > 1 then true else false
					)
				
					append deleteBoneArr currentBone

					removeIKsolvers currentBone
		
					if currentBone.children.count > 0 then
					(
						chlBone = currentBone.children
						revReset = true
					)


					if (classOf(currentBone) == BoneGeometry) and (currentBone.length == 10) and (currentBone.children.count == 0) then 
					(
						currentBone = parentBone
						parentBone = currentBone.parent
						append deleteBoneArr currentBone
					)

					if (parentBone != undefined) then
					(
						do   -- bone creation loop
						(
					        removeIKsolvers currentBone

						    if ( classOf(currentBone) == BoneGeometry ) then
							(
								newBone = boneSys.createBone (getEndPoint currentBone) currentBone.transform.translation currentBone.dir
								copyBoneHeightWidth newBone currentBone
								newBone.name = currentBone.name
								newBone.wirecolor=currentBone.wirecolor
								newBone.parent = prevBone
								newBone.resetBoneStretch()
								
 								if (parentBone.children.count > 1) and (parentBone.parent != undefined) then
								(
									parentBone.children.parent =  newBone
								)
								
								if (newBone.children == 0) and (newBone.length == 10) then
								(
									delete newBone
								)
								
								if chlBone != undefined then
								(
									chlBone.parent=newBone
								)
								
								if prevBone == undefined then
								(
									selBone = newbone
								)				
				

								prevBone = newBone
								currentBone = parentBone
								parentBone = currentBone.parent
								
								if ( classOf(currentBone) == BoneGeometry ) do append deleteBoneArr currentBone
								append newBoneArr newBone
							)
							else
							(
 								if (parentBone.children.count > 1) and (parentBone.parent != undefined) then
								(
								  local siblings = #()
								  for  b in parentBone.children do
								  (
								    if b != currentBone then
									(
									  append siblings b
									)
								  )
								  for i = 1 to siblings.count do
								  (
									(siblings[i]).parent = currentBone
								  )
								)

								if chlBone != undefined then
								(
									chlBone.parent=currentBone
								)

								if prevBone == undefined then
								(
									selBone = currentBone
								)	

								exPrevBone  = prevBone
								prevBone    = currentBone
								currentBone = parentBone
								parentBone  = currentBone.parent
								prevBone.parent = exPrevBone
								if ( classOf(currentBone) == BoneGeometry ) do append deleteBoneArr currentBone
							)
						
						) while (parentBone != undefined) -- bone creation loop

				        --removeIKsolvers currentBone

						if currentBone.children.count > 1 then
						(
							if ( classOf(currentBone) == BoneGeometry ) then
							(
						        local chlVar = #()

								for b in currentBone.children do
								(
					                --removeIKsolvers b
									append chlVar b
									b.parent = undefined
								)

								newBone = boneSys.createBone (getEndPoint currentBone) currentBone.transform.translation currentBone.dir
								copyBoneHeightWidth newBone currentBone
								newBone.name = currentBone.name
								newBone.wirecolor=currentBone.wirecolor
								newBone.parent = prevBone		
								
								chlVar.parent=newBone
								
								newBone.realignBoneToChild()
								newBone.resetBoneStretch()
								append newBoneArr newBone
							)
							else
							(
								currentBone.parent = prevBone		
								append newBoneArr currentBone
							)
						)
						else
						(
							if ( classOf(currentBone) == BoneGeometry ) then
							(
								newBone = boneSys.createBone (getEndPoint currentBone) currentBone.transform.translation currentBone.dir
								copyBoneHeightWidth newBone currentBone
								newBone.name = currentBone.name
								newBone.wirecolor=currentBone.wirecolor
								newBone.parent = prevBone
								append newBoneArr newBone
								
								parentBone = newBone
								
								newBone=BoneSys.createBone parentBone.transform.translation (parentBone.transform.translation+6) parentBone.dir
								copyBoneHeightWidth newBone parentBone
								newBone.rotation=parentBone.rotation
								newBone.pos=parentBone.transform.translation
								in coordSys Local move newBone [parentBone.length,0,0]
								newBone.parent=parentBone
								newBone.width=parentBone.width
								newBone.height=parentBone.height
								newBone.taper=90
								newBone.length=(parentBone.width+parentBone.height)/2
								newBone.wirecolor=parentBone.wirecolor
							)
							else
							(
								currentBone.parent = prevBone
							)
						)					
						
						for b in deleteBoneArr do
						(
						  if not isDeleted b do delete b
						)
						
						if (revReset != true) then
						(
							for i=1 to newBoneArr.count do 
							(
								(newBoneArr[i]).resetBoneStretch()
							)
						)
						else
						(
							for i=newBoneArr.count to 2 by -1 do 
							(
								(newBoneArr[i]).resetBoneStretch()
							)
						)
						
						
						select selBone
					)
				)
			)	
		)	
	)
	
--include "BinaryReader.ms"



struct TptHeader
(
	tag, -- char[4]; //'TPT1'
	sizeOfSection, -- u32 
	unk, -- u8  loop type????
	pad, -- u8 
	unk2, -- u16  (shortsPerMaterialAnim - no...sometimes 1 less (?))
	numMaterialAnims, -- u16 
	numShorts, -- u16 (should be product of previous shorts)

	offsetToMatAnims, -- u32  - for each materialAnim: u16 numSorts, u16 firstShort, u32 unk
	offsetToShorts, -- u32 (shorts are texture indices)
	offsetToIndexTable, -- u32 stores for every material to which mat3 index it belongs
	offsetToStringTable, -- u32 
	
	fn LoadData br = 
	(
		tag = br.ReadFixedLengthString 4
		sizeOfSection = br.ReadDWORD()
		unk = br.GetByte()
		pad = br.GetByte()
		unk2 = br.ReadWORD()
		numMaterialAnims = br.ReadWORD()
		numShorts = br.ReadWORD()
		offsetToMatAnims = br.ReadDWORD()
		offsetToShorts = br.ReadDWORD()
		offsetToIndexTable = br.ReadDWORD()
		offsetToStringTable= br.ReadDWORD()
	)

)


struct MatAnim
(
	count, -- u16
	firstIndex, --  u16
	unknown, -- u32 
	
	fn LoadData br = 
	(
		count = br.ReadWORD()
		firstIndex = br.ReadWORD()
		unknown = br.ReadDWORD()
	)
)

struct BtpAnim
(
	materialIndex,
	materialName,
	keyFrameIndexTable
)

struct Btp
(
	anims = #(),
	
	fn LoadData  br =
	(
		
		local tpt1Offset = br.Position()
		--size_t i;

		-- read header
		local header = TptHeader()
		header.LoadData(br)

		-- read stringtable
		local stringtable = br.ReadStringTable(tpt1Offset + header.offsetToStringTable)
		if (stringtable.count != header.numMaterialAnims) then
			throw ("Btp:LoadTPT1: number of strings ("+(stringtable.count as string)+") doesn't match number of animated materials (" +(header.numMaterialAnims as string) + ")")

		--read matAnimIndexToMat3Index table
		local matAnimIndexToMat3Index = #() -- (h.numMaterialAnims);
		br.SeekSet (tpt1Offset + header.offsetToIndexTable) 
		for i = 1 to header.numMaterialAnims do
			matAnimIndexToMat3Index[i] = br.ReadWORD()

-- messagebox (matAnimIndexToMat3Index as string)
		--read shorts table
		local shorts = #() -- (h.numShorts);
		br.SeekSet(tpt1Offset + header.offsetToShorts) 
		for i = 1 to header.numShorts do
			shorts[i] = br.ReadWORD()

		--read animations
		-- btp.anims.resize(h.numMaterialAnims);
		br.SeekSet (tpt1Offset + header.offsetToMatAnims) 
		for i = 1 to header.numMaterialAnims do
		(
			--messageBox stringtable
			
			local mAnim = MatAnim()
			mAnim.LoadData(br)
			--anims[i] = anim
			
			if (mAnim.unknown != 0x00ffffff) then
			  throw ("btp: "+(mAnim.unknown as string)+" instead of 0x00ffffff for mat anim nr "+(i as string))
			
			--anims[i].indexToMat3Table = matAnimIndexToMat3Index[i]
			--btp.anims[i].indices.resize(anim.count)
			--messageBox (matAnimIndexToMat3Index as string)
			local animaiton = ""
			for c in shorts do
				animaiton = animaiton + (c as string) + " "
			
			local anim = BtpAnim()
			anim.materialIndex = i
			anim.materialName = stringtable[i]
			anim.keyFrameIndexTable = shorts
			anims[i] = anim
			--print animaiton
			--copy(shorts.begin() + anim.firstIndex, shorts.begin() + anim.firstIndex + anim.count,
			 -- btp.anims[i].indices.begin());
	   )
	),
	
	fn LoadBTP filePath =
	(
		local br = BinaryReader()
		br.Open filePath 
		br.SeekSet 0x20 

		-- local size = 0
		-- local tag -- char[4];
		-- local t = 0

		--do 
		--(
			-- br.SeekCur size
			local pos = br.Position()
			local tag = br.ReadFixedLengthString 4
		
			local size = br.ReadDWORD()

			if(size < 8) then
				size = 8 -- prevent endless loop on corrupt data

			br.SeekSet pos
			
			--if(feof(f)) then -- need to check how to test in maxscript. Use fseek  end, get pos, compare to current position ????
			--	break
			
			if tag == "TPT1" then
			  LoadData br
			else
			(
			  messageBox ("readBck(): Unsupported section " + tag)
				throw  ("readBck(): Unsupported section " + tag)
			)

			br.SeekSet pos 

			
		--) while not EOF br._f
		
		br.Close()
	)
		


)



struct BModel
(
    _boneThickness = 10,
	inf,
	vtx,
	shp,
	jnt,
	evp,
	drw,
	_mat1, 
	tex,
	_bmdViewPathExe,
	_bones = #(),
	_iconSize = 100,
	_currMaterialIndex = 1,
	_currMaterial,
	_texturePath,
	_texturePrefix,
	_bckPaths = #(),
	_bmdFilePath,
	_bmdDir, 
	_bmdFileName,  
	_createBones = true,
	_loadAnimations = true,
	
	vertices = #(),
	faces = #(),
	tverts = #(),
	tFaces = #(),
	normals = #(),
	vcFaces = #(), -- vertex color
	vertexMultiMatrixEntry = #(),
	_materialIDS = #(),
	_subMaterials = #(),
	
	_parentBoneIndexs = #(),
	_allowTextureMirror = false, -- doesn't work on characters? required for stages?
	_forceCreateBones = false,
	
	_exportType=#XFILE, -- #XFILE, #CHARACTER
	_runExtractTexturesCmd = true,
	_includeScaling = false,
	_reverseFaces = true, -- required for .x export (render eyes before face)
	
	fn SetBmdViewExePath value =
	(
		_bmdViewPathExe = value

		if (getFiles (_bmdViewPathExe + "BmdView.exe")).count == 0 then
		(
			MessageBox (_bmdViewPathExe + "BmdView.exe not found. Place the BmdView.exe file included in the zip file into the given path.")
			throw "ERROR"
		)
		
		/*
		if (findString _bmdViewPathExe " ") != undefined then
		(
			try
			(
				HiddenDOSCommand "cmd dir"
			)
			catch 
			(
				MessageBox "DosCommand does not support a BmdView path that contains spaces. Move the BmdView.exe file to a path without spaces and update the code int ImportUI.ms (search for 'UPDATE BMDVIEW.EXE PATH')"
				throw
			)
		) */
	),

	-- HiddenDOSCommand cmd startpath:scriptPath
	-- already defined in maxscript 2008
	fn TryHiddenDOSCommand cmd startpath =
	(
		--print "###################"
		--print cmd
		--print startpath
		try
		(
			HiddenDOSCommand cmd startpath:startpath
		)
		catch 
		(
			-- Uncomment the line below for if the startpath contains spaces 
			-- startpath = "C:\\" -- and place BmdView.exe in "C:\\" directory
			if (findString startpath " ") != undefined  then
			(
				msg = "The startpath contains spaces (unable to run DosCommand). Place \"BmdView.exe\" in a path without spaces an update the startpath value in the \"BModel.ms\" file"
				messageBox msg 
				throw
			)
			DosCommand (startpath + cmd)
		)
	),

	fn ReverseArray inputArray =
	(
		local i = 0
		local rev = #()
		i = inputArray.count
		while i > 0  do
		(
			append rev inputArray[i]
			i -= 1
		)
		-- inputArray = rev doesn't work
		return rev
	),
	
	fn BuildSingleMesh =
	(		
		
		-----------------------------------------------------------------
		-- mesh
		if (_reverseFaces) then -- 
		(
			faces = ReverseArray faces
			_materialIDS = ReverseArray _materialIDS
			tFaces = ReverseArray tFaces
			vcFaces = ReverseArray vcFaces
		)
		
		-- TODO: should never have undefined materials
		for i = 1 to _materialIDS.count do
		(
			if (_materialIDS[i] == undefined) then
			(
				_materialIDS[i] = 0 -- not found index
			)
		)
		
		-- FIX: Fill missing material IDs
		if (_materialIDS.count > 0) then
		(
			for i=_materialIDS.count+1 to faces.count do
			(
				_materialIDS[i] = 0
			)
			
			
			modelMesh = mesh vertices: vertices faces:faces tverts:tverts materialIDS:_materialIDS
			modelMesh.name = getFilenameFile _bmdFilePath
			
			update modelMesh
			ClassOf modelMesh

			-- tvert faces
			--Set texcoord faces 
			buildTVFaces modelMesh  false
			for i = 1 to tFaces.count do
			(
				if (tFaces[i] != undefined) then -- TODO: should never have undefined texture faces
					setTVFace modelMesh i tFaces[i]
			)
		)
		else
		(
			_materialIDS.count = faces.count
			for i = 1 to _materialIDS.count do
			(
				_materialIDS[i] = 0 -- not found index
			)
			
			
			modelMesh = mesh vertices: vertices faces:faces tverts:tverts materialIDS:_materialIDS
			modelMesh.name = getFilenameFile _bmdFilePath
			
			update modelMesh
			ClassOf modelMesh

			-- tvert faces
			--Set texcoord faces 
			--buildTVFaces modelMesh  false	-- FIX: Do not set texture faces when there are no textures on model.
			for i = 1 to tFaces.count do
			(
				if (tFaces[i] != undefined) then -- TODO: should never have undefined texture faces
					setTVFace modelMesh i tFaces[i]
			)
		)
		
		
		-- set normals [no effect?]
		if (normals.count != vertices.count) then
		(
			--messageBox "Invalid normals?"	-- FIX: IGNORE INVALID NORMALS TO ALLOW IMPORT SOME MODELS
			--throw "Invalid normals?"
		)
		/*for i = 1 to vertices.count  do	-- FIX: DO NOT IMPORT NORMALS, THEY ARE BROKEN ON NEW MAX VERSIONS
		(
			if (normals[i] != undefined) then
				setNormal modelMesh i normals[i] 
		)*/

		update modelMesh 
		

		if (vtx.colors.count != 0 AND vtx.colors[1].count != 0) then -- has colors?
		(
			--if (vtx.colors.count > 1) then
			--	throw ("vtx.colors.count = " + (vtx.colors[1].count as string))
			
			--if (vtx.colors[1].count > 1) then
			--	throw ("vtx.colors[1].count = " + (vtx.colors[1].count as string))
			setNumCPVVerts modelMesh vtx.colors[1].count

			
			for i = 1 to vtx.colors[1].count do
				setVertColor  modelMesh i vtx.colors[1][i]
			
			buildVCFaces modelMesh false

			for i = 1 to vcFaces.count do
			(
				if (vcFaces[i] != undefined) then -- check not needed?
				(
					--messageBox "Vertex color error"
					--throw "Vertex color error"
					setVCFace modelMesh i vcFaces[i][1] vcFaces[i][2] vcFaces[i][3]
				)
				
				
			)
			
			modelMesh.showVertexColors = true -- display vertex shading 
		)
		

		update modelMesh 
		
		-----------------------------------------------------------------
		-- skin
		if (_createBones) then
		(
			update modelMesh 
			max modify mode
			select modelMesh 
			subObjectLevel = 0
			local newskin = Skin()
			addModifier modelMesh newskin
			local mysk = modelMesh.modifiers[#Skin]
			subobjectLevel = 1
			modPanel.setCurrentObject mysk 
			subobjectLevel = 1
		
			for bone in _bones do
			(
				skinOps.addBone mysk bone 0
			)
		
			if (vertexMultiMatrixEntry.count != vertices.count) then
			(
				messageBox "Invalid skin"
				throw "E"
			)
			
			ClassOf modelMesh -- http://forums.cgsociety.org/archive/index.php/t-456395.html

			for i=1 to vertices.count do
			(
				-- Don't use setVertexWeights. Has issues with existing bone weights (mainly root bone)
				skinOps.ReplaceVertexWeights mysk  i vertexMultiMatrixEntry[i].indices vertexMultiMatrixEntry[i].weights 
			)
			
			update modelMesh 
			
			subObjectLevel = 0
			deselect modelMesh 
		)	
		
		cmat = multimaterial numsubs:_subMaterials.count
		cmat.name = getFilenameFile _bmdFilePath
		for i = 1 to _subMaterials.count do
			cmat[i] = _subMaterials[i]

		modelMesh.material = cmat
		
		meditMaterials[1]  = cmat
		
		-- freeze model by default
		--freeze  modelMesh	-- DO NOT FREEZE MODEL BY DEFAULT
		modelMesh.showFrozenInGray = off
		
		return modelMesh
	),
	
fn LoadModel filePath =
(
	-- load model
	local br = BinaryReader()
	br.Open filePath 
	_bmdFilePath = filePath 
	_bmdDir = getFilenamePath _bmdFilePath 
	_bmdFileName = getFilenameFile  _bmdFilePath 
	_bmdDir += _bmdFileName + "\\"
	makeDir _bmdDir
	
	_texturePath = _bmdDir + "Textures\\" 

	br.SeekSet 0x20 
	
	local iSize = 0 
	local strTag = "" -- 4 characters
	local iTell = 0
	
	inf = Inf1()
	vtx = Vtx1()
	shp = Shp1()
	jnt = Jnt1()
	evp = Evp1()
	drw = Drw1()
	_mat1 = Mat3()
	tex = Tex1()
	
	do
	(
		br.SeekCur (iSize)
		local streamPos = br.Position()
		strTag = br.ReadFixedLengthString 4
		iSize = br.ReadDWORD()
		
		-- print (strTag + ":" + (streamPos as string))
		
		br.SeekSet streamPos 
		if strTag == "INF1" then
		(
			inf.LoadData br
		)
		else if strTag == "VTX1" then
		(
			vtx.LoadData br
		)
		else if strTag == "SHP1" then
		(
			shp.LoadData br
			-- print (shp as string)
		)
		else if strTag == "JNT1" then
		(
			jnt.LoadData br
		)
		else if strTag == "EVP1" then
		(
			evp.LoadData br
		)
		else if strTag == "DRW1" then
		(
			drw.LoadData br
		)
		else if strTag == "MAT3" then
		(
			_mat1.LoadData br
		)
		else if strTag == "TEX1" then
		(
			tex.LoadData br
		)
		
		br.SeekSet streamPos 
	)
	while strTag != "TEX1" -- not br.EOF() --
	
	br.Close()
),

fn DrawVerts =
(
	delete $*
	for vec in vtx.positions do
	(
		p = Point pos:[vec.x, vec.y, vec.z] cross:on Box:off 
		 print vec 
    )
),

fn Mad r m f =
(
		
	for j = 1 to 3 do
	  (
	    for k = 1 to 4 do
		(
		   r.m[j][k] += f * m.m[j][k]
		)
	  )
	  
  return r
),
	
fn LocalMatrix i = -- returns Matrix44f 
(
  --s =  Matrix44f()
  --s.LoadScale jnt.frames[i].sx jnt.frames[i].sy jnt.frames[i].sz

  --TODO: I don't know which of these two return values are the right ones
  --(if it's the first, then what is scale used for at all?)

  --looks wrong in certain circumstances...
  return jnt.matrices[i] -- this looks better with vf_064l.bdl (from zelda)
  --return bm.jnt1.matrices[i]*s -- this looks a bit better with mario's bottle_in animation
),

--newVertIndex = 1,
faceIndex = 1,
fn DrawBatch index def =
(
	currBatch = shp.batches[index]
	if (not currBatch.attribs.hasPositions) then
		throw "found batch without positions"
		
	
	--local firstTextCoordIndex = 1

	vertIndex = 1
	i = 1
	
	  matrixTable = #() -- there should NEVER be more than 20 matrices per packet imo...even 10 sound like a lot...
	  isMatrixWeighted = #() -- pos?
	  multiMatrixTable = #() -- should be same count as matrixTable 
	  maxWeightIndices = 0
	  matrixTable = #()
	  multiMatrixTable = #()

	--print (vtx.texCoords.count as string)
	if (vtx.texCoords[1] != undefined) then
	(
		for i = 1 to vtx.texCoords[1].count do
		(
			tvert = vtx.texCoords[1][i]
			tverts[i] = [tvert.s, -tvert.t+1, 0] -- flip uv v element
		)
	)

	  for currPacket in currBatch.packets do
	  (
		    for n = 1 to currPacket.matrixTable.count do
			(
			         index = currPacket.matrixTable[n]
					 if index != 0xffff then -- //this means keep old entry
				     (
						   	if drw.isWeighted[index + 1] then
						   	(
						          --TODO: the EVP1 data should probably be used here,
						          --figure out how this works (most files look ok
						          --without this, but models/ji.bdl is for example
						          --broken this way)
						          --matrixTable[n] = def;
						
						          --the following _does_ the right thing...it looks
						          --ok for all files, but i don't understand why :-P
						          --(and this code is slow as hell, so TODO: fix this)
						
						          --NO idea if this is right this way...
								  m = Matrix44()
								  m.LoadZero()
						         
								  mm = evp.weightedIndices[drw.data[index + 1]+1] -- get MultiMatrix
								  singleMultiMatrixEntry = MultiMatrix()
							
								  for r = 1 to mm.weights.count do
								  (
									singleMultiMatrixEntry.weights[r] = mm.weights[r]
									singleMultiMatrixEntry.indices[r] = mm.indices[r] + 1-- (drw.data[mm.indices[r]+ 1] + 1) -- bone index
								  
									--  sm1 = evp.matrices[mm.indices[r] + 1] -- const Matrix44f
									--  messageBox (mm.indices as string)
									--if (mm.indices[r] != 0) then
									-- (
										sm1 = evp.matrices[mm.indices[r]+1] -- const Matrix44f
										sm2 = LocalMatrix (mm.indices[r] + 1)
										sm3 = sm2.Multiply sm1
									  /*
									  	   sm1 = evp.matrices[mm.indices[r]] -- const Matrix44f
										sm2 = LocalMatrix mm.indices[r] 
										sm3 = sm2.Multiply sm1*/
									--  )
									--  else
									--	sm3 = (LocalMatrix mm.indices[r] )
									
						             Mad m sm3 mm.weights[r]
								  )
									
								  multiMatrixTable[n] = singleMultiMatrixEntry 
								  m.m[4][4] = 1
						          matrixTable[n] = m
						          isMatrixWeighted[n] = true
					        )
					        else
					        (
						          matrixTable[n] = jnt.matrices[drw.data[index + 1] + 1]
						          isMatrixWeighted[n] = false
								  
								  singleMultiMatrixEntry = MultiMatrix()
								  singleMultiMatrixEntry.weights = #(1)
								  singleMultiMatrixEntry.indices = #(drw.data[index + 1] + 1) -- bone index

								  multiMatrixTable[n] = singleMultiMatrixEntry 
					        ) -- end if drw.isWeighted[index] then
				
					) -- end if index != 0xffff then -- //this means keep old entry
			) -- end for index in currPacket.matrixTable do
		  
		  
			--if no matrix index is given per vertex, 0 is the default.
		    --otherwise, mat is overwritten later.
			mat = matrixTable[1]				  
            multiMat = multiMatrixTable[1] 
			
			for currPrimitive in currPacket.primitives do
			(	
				for m = 1 to currPrimitive.points.count do
				(
				    posIndex = currPrimitive.points[m].posIndex + 1
					
			        -- TODO: texcoords 1-7, color1
			        if currBatch.attribs.hasMatrixIndices then
					(
			            mat = matrixTable[(currPrimitive.points[m].matrixIndex/3) + 1]
						
						if (mod currPrimitive.points[m].matrixIndex 3) != 0 then
							messageBox (("WARNING: if (mod currPrimitive.points[m].matrixIndex 3) != 0 then ") + (currPrimitive.points[m].matrixIndex as string))
					   multiMat = multiMatrixTable[(currPrimitive.points[m].matrixIndex/3) + 1]

  					)

        			if currBatch.attribs.hasNormals then
					(
						normal = vtx.normals[(currPrimitive.points[m].normalIndex) + 1]
          					normals[posIndex] = normal.ToMaxScriptPos()
					)
			     
					vertexMultiMatrixEntry[posIndex] = multiMat 
					newPos = mat.MultiplyVector vtx.positions[posIndex]
					
					vertices[posIndex] = [newPos.x, -newPos.z, newPos.y] -- flip order
	            )

				if currPrimitive.type == 0x98 then -- strip
				(
					for m = 1 to (currPrimitive.points.count - 2) do
					(
						posIndex1 = currPrimitive.points[m].posIndex + 1
						posIndex2 = currPrimitive.points[m + 1].posIndex + 1
						posIndex3 = currPrimitive.points[m + 2].posIndex + 1
						
						if (mod m 2) == 0 then -- even
							faces[faceIndex] = [posIndex1, posIndex2, posIndex3]
						else 
							faces[faceIndex] = [posIndex3, posIndex2, posIndex1] -- reverse
						
						if currBatch.attribs.hasTexCoords[1] then
						(
						   	t1Index = currPrimitive.points[m].texCoordIndex[1] + 1
							t2Index = currPrimitive.points[m + 1].texCoordIndex[1] + 1
							t3Index = currPrimitive.points[m + 2].texCoordIndex[1] + 1
						 
						  	if (mod m 2) == 0 then -- even
								tFaces[faceIndex] = [t1Index , t2Index , t3Index ]
							else 
								tFaces[faceIndex] = [t3Index , t2Index , t1Index ] -- reverse
							
							_materialIDS[faceIndex] = _currMaterialIndex - 1
						)
						
						
						
						-- vertex colors
						if currBatch.attribs.hasColors[1] then
						(
						   	c1Index = currPrimitive.points[m].colorIndex[1] + 1
							c2Index = currPrimitive.points[m + 1].colorIndex[1] + 1
							c3Index = currPrimitive.points[m + 2].colorIndex[1] + 1
							
							if (mod m 2) == 0 then -- even
								vcFaces[faceIndex] = [c1Index , c2Index , c3Index ]
							else 
								vcFaces[faceIndex] = [c3Index , c2Index , c1Index ] -- reverse
						)
						else
							vcFaces[faceIndex] = undefined
					
						faceIndex += 1 
					)
				) -- GL_TRIANGLE_STRIP
				else if currPrimitive.type == 0xa0 then 
				(
					messageBox "NYI: fan"
				) -- GL_TRIANGLE_FAN
				else
				(
					messageBox "unknown primitive type"
				) -- end if currPrimitive.type == 0x98 then -- strip
	
		   )-- end for currPrimitive in currPacket.primitives do
	
	  ) -- end for currPacket in currBatch.packets do
),

fn FrameMatrix f =
(
	t = Matrix44()
	rx = Matrix44()
	ry = Matrix44()
	rz = Matrix44()
	s = Matrix44()

  t.LoadTranslateLM f.t.x f.t.y f.t.z
  rx.LoadRotateXLM ((f.rx/(360 as float))*2*PI)
  ry.LoadRotateYLM ((f.ry/(360 as float)) *2*PI)
  rz.LoadRotateZLM ((f.rz/(360 as float))*2*PI)
  
  res = Matrix44()
  res.LoadIdentity()
  res = t.Multiply(rz.Multiply (ry.Multiply rx))
  return res 	
),

-- create frame nodes and setup jnt.matrices
fn CreateFrameNodes j d parentMatrix parentFrameNode =
(
	local b1 = false
    local effP = parentMatrix 
	i = j

  fNode = parentFrameNode 
  
	while i < inf.scenegraph.count do
	(
	   n = inf.scenegraph[i + 1]
	
	    if(n.type != 1 AND b1) then
	    (
	      b1 = false;
	      effP = parentMatrix   -- prevents fixed chain
		  fNode = parentFrameNode
	    )
	
		if n.type == 0x10 then
	   	(
			 --joint
			f = jnt.frames[n.index + 1] -- arrays start at index 1 in maxscript
			effP = effP.Multiply (FrameMatrix f)
	        jnt.matrices[n.index + 1] = effP -- effP.Multiply(FrameMatrix(f))
			
			fNode = FrameNode()
			fNode.f = f
			
			fNode.startPoint = (parentMatrix.MultiplyVector f.t).ToMaxScriptPos()
			
			fNode.parentFrameNode = parentFrameNode
			fNode.effP = effP
			--fNode.name = _bmdFileName + "_" + f.name	-- FIX: DO NOT ADD FILENAME PREFIX TO BONES
			fNode.name = f.name
			
			append parentFrameNode.children fNode
		   b1 = true
		)
		 else if n.type == 1 then 
		 (	
		 	i += CreateFrameNodes (i+1) (d+1) effP fNode -- note: i and j start at 1 instead of 0
		 )
		else if n.type == 2 then
		(
		   return i - j + 1 -- note: i and j start at 1 instead of 0
		)

	   i += 1
	)
	
	return -1
),

fn CreateCharacter rootFrameNode = 
(
	local nodes = rootFrameNode.GetAllNodes()
	local chr = assemblyMgr.assemble  nodes  name:(uniqueName "Character") classDesc:CharacterAssembly
	chr.name = _bmdFileName + "_Character" 
	chr.assemblyBBoxDisplay = false
	chr.iconSize = _iconSize
	chr.wirecolor = (colorMan.getColor #chr_color)*255
	
	local groupHead = undefined
	for n in nodes do 
	(
		if isGroupHead n then 
			groupHead = n
	)

	for bone in _bones do
		bone.setSkinPose()
	
	chr.displayRes = 1 -- hide bones
	assemblyMgr.Open chr
	
	return chr
),


fn DrawScenegraph j d parentMatrix =
(
	local b1 = false
    local effP = parentMatrix--.Copy()
	i = j
	while i < inf.scenegraph.count do
	(
		
	   n = inf.scenegraph[i + 1]
		
		--print (n.type as string)
	
	    if(n.type != 1 AND b1) then
	    (
	      b1 = false;
	      effP = parentMatrix--.Copy() -- prevents fixed chain
	    )
	
		if n.type == 0x10 then --joint
	   	(
		    effP = jnt.matrices[n.index + 1] -- setup during CreateBones 
		    b1 = true
		)
		 else if (n.type == 0x11) then
		(
			local matName = _mat1.stringtable[n.Index + 1]
			local mat = _mat1.materials[_mat1.indexToMatIndex[n.Index + 1] + 1]
			local stage =  mat.texStages[1]
			local textureName = ""
			local v2 = _mat1.texStageIndexToTextureIndex[stage + 1] -- undefined if stage = 0xffff
			if (stage != 0xffff) then
			(
				v2 += 1 -- v2 used latter. value is undefined if stage == 0xffff
				textureName = tex.stringtable[v2]
			)
			
			--textureName = matName
			local fileName = _texturePath + _texturePrefix + textureName + ".tga"
			bmpFound = (getFiles fileName).count == 1 
			
			-- messageBox fileName
			_currMaterial= StandardMaterial()
			_currMaterial.diffusemap = bitmapTexture filename:fileName
			_currMaterial.diffusemap.coords.blur = 0.01
			
			--gc()
			local bmp = undefined
			local hasAlpha = false
			
			if (bmpFound) then
			(
				 bmp = _currMaterial.diffusemap.bitmap 
				alp = 0
				-- TODO: find faster alpha check
				for r=0 to bmp.height-1 do -- for each row in the bitmap
				( 
					local pixels=getpixels bmp [0,r] bmp.width -- read in the column of pixels
				
					for c=1 to bmp.width do -- loop through each pixel
					( 
						local p=pixels[c] -- get the pixel
						if (p.alpha != 255) then
						(
							alp  = p.alpha
							hasAlpha = true 
							exit -- break
						)
					)
					
					if (hasAlpha ) then
						exit -- break
				)
			)
			else
			(
				-- make it easier to see invalid textures
				_currMaterial.ambient = red
				_currMaterial.Diffuse = red
			)

			if (hasAlpha) then
			(
				_currMaterial.twoSided = true -- anything with alpha is always two sided?
				_currMaterial.opacityMap = bitmapTexture filename:fileName
				_currMaterial.opacityMap.monoOutput = 1
				_currMaterial.opacityMap.coords.blur = 0.01
			)
			
			showTextureMap _currMaterial true -- display texture in view
			_currMaterial.name = matName
			
			_subMaterials[_currMaterialIndex] = _currMaterial
			
			-- display in material editor?
			-- meditMaterials[_currMaterialIndex + 1] = _currMaterial
			_currMaterialIndex += 1
			
			-- messageBox (matName + (tex.texHeaders[v2].wrapS as string) + "+" + (tex.texHeaders[v2].wrapT as string))
			-- NOTE: check ash.bmd for case when wrapS=2 and wrap=2. u_offset = 0.5 and V_offset = 0.5
			if (bmpFound) then
			(
				if (tex.texHeaders[v2].wrapS == 0) then -- clamp to edge? Needs testing. Cannot use .U_Mirror = false and .U_Tile = false. If WrapS == 0 then has Alpha?
				(
				)
				else if (tex.texHeaders[v2].wrapS == 1) then -- repeat (default)
				(
				)
				else if (tex.texHeaders[v2].wrapS == 2) then
				(
					_currMaterial.name += "_U" -- add suffix to let the modeler know where mirror should be used
					if (_allowTextureMirror) then
					(
						_currMaterial.diffusemap.coords.U_Mirror = true
						_currMaterial.diffusemap.coords.U_Tile = false
						_currMaterial.diffusemap.coords.u_offset = 0.5
						_currMaterial.diffusemap.coords.U_Tiling = 0.5
						
						if (hasAlpha) then
						(
							_currMaterial.opacityMap.coords.U_Mirror = true
							_currMaterial.opacityMap.coords.U_Tile = false
							_currMaterial.opacityMap.coords.u_offset = 0.5
							_currMaterial.opacityMap.coords.U_Tiling = 0.5
						)
					)
				)
				else
					throw ("Unknown wrapS " + (tex.texHeaders[v2].wrapS as string))

				
	
				if (tex.texHeaders[v2].wrapT == 0) then -- clamp to edge? Needs testing
				(
				)
				else if (tex.texHeaders[v2].wrapT == 1) then -- repeat (default)
				(
				)
				else if (tex.texHeaders[v2].wrapT == 2) then
				(
					_currMaterial.name += "_V" -- add suffix to let the modeler know where mirror should be used
					if (_allowTextureMirror) then
					(
						_currMaterial.diffusemap.coords.V_Mirror = true
						_currMaterial.diffusemap.coords.V_Tile = false
						_currMaterial.diffusemap.coords.V_offset = 0.5
						_currMaterial.diffusemap.coords.V_Tiling = 0.5
						
						if (hasAlpha) then
						(	
							_currMaterial.opacityMap.coords.V_Mirror = true
							_currMaterial.opacityMap.coords.V_Tile = false
							_currMaterial.opacityMap.coords.V_offset = 0.5
							_currMaterial.opacityMap.coords.V_Tiling = 0.5
						)
					)
				)
				else
					throw ("Unknown wrapT " + (tex.texHeaders[v2].wrapS as string))
			)
		)
	  
		 else if (n.type == 0x12) then -- type = 18
		 (
		 	   DrawBatch (n.index + 1) effP 
		 )
		 else if n.type == 1 then 
		 (
		 	i += DrawScenegraph (i+1) (d+1) effP -- note: i and j start at 1 instead of 0
		 )
		else if n.type == 2 then
		(
		   return i - j + 1 -- note: i and j start at 1 instead of 0
		)

	   i += 1
	)
	
	return -1
),

-- TODO: use matrix math instead
/*
fn RotateAroundWorld obj rotation =
(
	
	local origParent =  obj.parent
	local d = dummy()
	obj.parent = d
	rotate d  rotation
	--delete d
	--if (origParent != undefined) then
	--	obj.parent = origParent
	
),
*/

fn DrawScene =
(
	delete $*
	local m= Matrix44()
	_frameMatrix = m.GetIdentity()

	rootFrameNode = FrameNode()
	identity = m.GetIdentity()
	CreateFrameNodes 0 0 identity rootFrameNode 
	
	-- FIX: Force create bone option allows to generate bones independently of their count
	if ((rootFrameNode.children.count == 1 AND rootFrameNode.children[1].children.count == 0) AND not _forceCreateBones) then
		_createBones = false  
	
	local origWorldBonePos = undefined
	
	if (_createBones) then
	(
		_bones = rootFrameNode.CreateBones _boneThickness ""
		
		if (_includeScaling) then -- scaling cases IK and number of bones issue
			rootFrameNode.FixBones _boneThickness
		
		_parentBoneIndexs = rootFrameNode.CreateParentBoneIndexs()
		origWorldBonePos = _bones[1].position
		
		-- easier than recalculating all bone transforms
		local d = point()
		_bones[1].parent = d
		rotate d  (EulerAngles 90 0 0)
	)

	i = m.GetIdentity()
	
	
	-----------------------------------
	-- reverse items
	/*
	local revList = #()
	local i = inf.scenegraph.count
	while i > 0 do
	(
		append revList (inf.scenegraph[i])
		i -= 1
	)
	inf.scenegraph = revList*/
	-----------------------------------
	
	
	DrawScenegraph 0 0 i 
	modelMesh = BuildSingleMesh()
	
	local chr = undefined
	local characterPos = undefined
	
	if (_createBones AND _exportType==#CHARACTER) then
	(
		chr = CreateCharacter(rootFrameNode)
	    --RotateAroundWorld  chr (EulerAngles 90 0 0)
		
		-- Rotate Character assembly upwards and swap hierarchy for Point and Character
		_bones[1].parent = undefined
		chr.parent = undefined
		rotate chr (EulerAngles 90 0 0)
		_bones[1].parent = d
		d.parent = chr
	)
	
	--RotateAroundWorld modelMesh (EulerAngles 90 0 0) -- e.g. stage, object

	if (_createBones) then
		dirCreated = makeDir (_bmdDir + "\\Animations")

	local bckFiles = #()
	local saveMaxName = _bmdDir + _bmdFileName + ".max" -- .chr?
	local errMsg = ""
	max tool zoomextents all
		
	for bone in _bones do
		bone.setSkinPose()
	
	fileProperties.addProperty  #custom "exportAnimation" false
	
	--_createBones = true
	if (_createBones AND _loadAnimations) then
	(
		fileProperties.addProperty  #custom "exportAnimation" true
		
		local _onlyExportAnimations = false
		if (_onlyExportAnimations) then
		(
			-- remove mesh and create fake skinning mesh (required for panda export)
			delete modelMesh
			fakeMesh = mesh vertices: #([10, 0, 0], [0, 0, 0], [0, 10, 0]) faces: #([3,2,1])
			update fakeMesh 
			max modify mode
			select fakeMesh 
			subObjectLevel = 0
			local newskin = Skin()
			addModifier fakeMesh newskin
			local mysk = fakeMesh.modifiers[#Skin]
			subobjectLevel = 1
			modPanel.setCurrentObject mysk 
			subobjectLevel = 1
			skinOps.addBone mysk (getNodeByName "RootBone") 0
			ClassOf fakeMesh -- http://forums.cgsociety.org/archive/index.php/t-456395.html
			update fakeMesh 
			subObjectLevel = 0
			deselect fakeMesh 
		)
	
	
		local kwXPortAnimationName = ""
		local animationCount = 1 -- default pose at frame 1

		if (_exportType!=#XFILE) then
			saveMaxFile saveMaxName 
		
		if (_exportType==#FBX) then
		(
			FBXExporterSetParam "Animation" true
		)
		
		local startFrame = 0
		
		local refBoneRequiresDummyList = #() -- remove dummy objects if not required
		for i = 1 to _bones.count do
			refBoneRequiresDummyList[i] = false
		
		for bckPath in _bckPaths do
		(
			
			bckFiles = getFiles (_bmdDir + bckPath)
			
			for f in bckFiles do
			(
				local bckFileName = getFilenameFile f
				
				
				local saveMaxAnimName = _bmdDir  + bckFileName + ".max" -- .chr?
				
				local b = Bck()
				b.LoadBck f 
				
				if (b.anims.count != _bones.count) then
					errMsg += bckFileName + "\n"
				else
				(
					local endFrame = undefined
					
					b.AnimateBoneFrames startFrame _bones 1 [origWorldBonePos.x,origWorldBonePos.y,origWorldBonePos.z] _exportType refBoneRequiresDummyList _includeScaling
					
				
					local numberOfFrames = b.animationLength
					if ( b.animationLength <= 0) then
						numberOfFrames= 1
					
					endFrame = startFrame + b.animationLength

					if (_exportType==#XFILE) then
					(
						kwXPortAnimationName += bckFileName + "," + (startFrame as string) + "," + (numberOfFrames as string) + ",1;"
						startFrame = endFrame + 1 
						animationCount += 1
					)
					else if (_exportType==#CHARACTER) then
					(
						local savePath = _bmdDir + "Animations\\" + bckFileName + ".anm"
						
						saveNodes _bones savePath
						--b.DeleteAllKeys _bones
						--rootFrameNode.ResetControllers() -- removes animations?
						--b.resetNodeAnim _bones[1]
						loadMaxFile saveMaxName	-- TODO: should only need to reset keys / clear animations
						_bones = rootFrameNode.RemapBones()
						
					--	rootFrameNode.ResetControllers() -- removes animations?
					)
					else	-- FBX export
					(
						animationRange = interval 0 numberOfFrames
						
						select $Point001...*
						
						local savePath = _bmdDir + "Animations\\" + bckFileName + ".fbx"
						exportFile savePath #noPrompt selectedOnly:true using:FBXEXP
						
						loadMaxFile saveMaxName
						_bones = rootFrameNode.RemapBones()
					)
				)
			) 
		)
		
		local frameItems = rootFrameNode.ToArray()
		if (frameItems.count != _bones.count) then
			throw ("number of frameItems ("+(frameItems.count as string)+") must match number of bones (" +(_bones.count as string)+ ")")
		--messageBox ((frameItems.count as string) + ":" + (_bones.count as string))
		for i = 1 to frameItems.count do
		(
			if (not refBoneRequiresDummyList[i]) then
			(
				-- bone doesn't require helpers
				frameItems[i].RemoveDummyHelper()
			)
		)
		/*
		for item in frameItems do
			print item
		
		for item in refBoneRequiresDummyList do
			print item
		*/
	
		--messageBox (refBoneRequiresDummyList as string)
		if (_exportType==#XFILE) then
		(
			kwXPortAnimationName = (animationCount as string) + ";"+ kwXPortAnimationName
			--messageBox kwXPortAnimationName
			fileProperties.addProperty  #custom "allAnimations" kwXPortAnimationName
			animationRange = interval 0 startFrame
			--messageBox (_bmdDir + _bmdFileName + ".x" )
		)
	)
	else
	(
		saveMaxFile saveMaxName
	)
	
	if (_exportType==#XFILE) then
	(	
		exportFile (_bmdDir + _bmdFileName + ".x" ) #noPrompt -- selectedOnly:true
		saveMaxFile saveMaxName 
	)
	else
	(
	    loadMaxFile saveMaxName	
		animationRange = interval 0 100 -- not required
	)
	
	

),

fn ExtractImages =
(
	imageType = ".tga"

	bmdViewExe = _bmdViewPathExe + "BmdView.exe"
	bmdPath = (getFilenamePath _bmdFilePath) + (getFilenameFile _bmdFilePath) + "\\"
	makeDir bmdPath 
	makeDir _texturePath 

	-- if no tga files are found then extract them
	tgaFiles = getFiles (_texturePath + "*.tga")

	-- cannot use shellLaunch because it doesn't wait for a return value
	-- don't use DOSCommand. Doesn't support spaces in full exe path. e.g. C:Program files\
	-- if using version before 2008 then use DOSCommand and set BmdView.exe into a known path
	if tgaFiles.count == 0 then
		TryHiddenDOSCommand ("BmdView.exe \"" + _bmdFilePath+ "\" \""+_texturePath+ "\\\"" + " DDS") _bmdViewPathExe

	classof tgaFiles
	ddsFiles = getFiles (_texturePath + "*.dds")
	
	-- create tga file and delete dds file
	for f in getFiles (_texturePath + "*.dds")  do 
	(
		TryHiddenDOSCommand ("readdxt.exe \""+_texturePath+ "\\" + (getFilenameFile f) + ".dds" + "\"") _bmdViewPathExe
		
		-- remove 00 suffix
		oldname = _texturePath+ "\\" + (getFilenameFile f) + "00.tga"
		newname = _texturePath+ "\\" + (getFilenameFile f) + ".tga"
		renameFile oldname newname
		
		deleteFile f
		
		/*local img = openBitMap f
		saveFileName = _texturePath + (getFilenameFile  f) + ".tga"
		local destImg = copy img
		destImg.filename = saveFileName
		save destImg -- cannot save img directly (requires copy to remove dds format)
		deleteFile f*/
	)
	
	-- TODO: need to update BmdView.exe to process all file formats like BmdView2
	errorMessage = "Error generating dds / tga image file(s).\nUse BmdView2 to export the missing tga file(s) then delete the *.ERROR file(s) and run the importer again\n\n"
	errorFiles = getFiles (_texturePath + "*.ERROR")
	for f in errorFiles  do 
	(
		errorMessage += f + "\n"
	)
	
	if (errorFiles.count != 0) then
	(
		messageBox errorMessage
		return false
	)
	
	return true
),

	
fn CreateBTPDataFile =
(
	
	local bckFiles = getFiles (_bmdDir + "..\\..\\btp\\*.btp")
	--messageBox (bckFiles as string)

	local fBTP = createFile (_bmdDir + "TextureAnimations.xml")
	
	format  "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" to:fBTP
	format  "<TextureAnimation>\n" to:fBTP
	format  "	<Textures>" to:fBTP
	
	local firstLoop = true
	for texName in tex.stringtable do
	(
		if (firstLoop) then
			firstLoop = false
		else
			format  "#" to:fBTP
		format "%" texName to:fBTP
	)
	format "</Textures>\n" to:fBTP
	
	format  "	<Materials>" to:fBTP
	
	local firstLoop = true
	for matName in _mat1.stringtable do
	(
		if (firstLoop) then
			firstLoop = false
		else
			format  "#" to:fBTP
		format "%" matName to:fBTP
	)
	format "</Materials>\n" to:fBTP
	
	format  "	<Animations>\n" to:fBTP
	for bckFile in bckFiles do
	(
		local textureAnim = Btp()
		textureAnim.LoadBTP(bckFile)
		
		format  "		<Animation>\n" to:fBTP
		format  "			<Name>%</Name>\n" (getFilenameFile bckFile) to:fBTP
		firstLoop = true
		for anim in textureAnim.anims do
		(
			format  "			<Material>\n" to:fBTP
			format  "				<MaterialIndex>%</MaterialIndex>\n" (anim.materialIndex) to:fBTP
			-- format  "				<Name>%</Name>\n" (anim.materialName) to:fBTP
			
			local animaitonKeys = ""
			for key in anim.keyFrameIndexTable do
			(
				if (firstLoop) then
					firstLoop = false
				else
					animaitonKeys = animaitonKeys + "#"
				animaitonKeys = animaitonKeys + (key as string) 
			)
			
			format  "				<KeyFrames>%</KeyFrames>\n" animaitonKeys to:fBTP
			format  "			</Material>\n"to:fBTP
			--messageBox (anim.animationName + ":" + animaitonKeys)
		)
		format  "		</Animation>\n" to:fBTP
		
	)
	format  "	</Animations>\n" to:fBTP
	format  "</TextureAnimation>" to:fBTP
	
	close fBTP
),
	
fn Import filename boneThickness allowTextureMirror forceCreateBones loadAnimations exportTextures exportType includeScaling =
(
	if (exportTextures) then
		_texturePrefix = ""
	else 
		_texturePrefix = "_"
	
	append _bckPaths  "..\\..\\bck\\*.bck"
	append _bckPaths  "..\\..\\bcks\\*.bck"
	append _bckPaths  "..\\..\\scrn\\*.bck"
	--_createBones = false
	_includeScaling = includeScaling
	--_exportType=#XFILE
	_exportType=exportType 
	
	_allowTextureMirror = allowTextureMirror
	_forceCreateBones = forceCreateBones
	_loadAnimations = loadAnimations
	_boneThickness = boneThickness
	LoadModel filename
	
	bmdPath = (getFilenamePath _bmdFilePath) + (getFilenameFile _bmdFilePath) + "\\"
	makeDir bmdPath 
	makeDir _texturePath 
	
	if (not exportTextures OR (exportTextures AND ExtractImages())) then
		DrawScene()

	CreateBTPDataFile()

)

)
		


	-- note that the mactoscript itself defines a scope

	rollout MaxBMDUI_Rollout "BMD Importer"
	(
		edittext txtFilePath "BMD File: " enabled:false text:"" width:250 align:#left
		button btnBrowse "Browse..." width:100 align:#center
		spinner spnBoneThickness "Bone Thickness:" range:[0,100,5] align:#left fieldwidth:50
		checkBox chkTextureMirror "Allow mirrored textures " checked:true width:250 align:#left
		checkBox chkForceBones "Force create bones " checked:false width:250 align:#left
		checkBox chkSaveAnim "Save Animations " checked:true width:250 align:#left
		checkBox chkExportTextures "Export textures" checked:true width:250 align:#left
		checkBox chkIncludeScaling "Include scaling" checked:false width:250 align:#left
		radiobuttons radExportType labels:#("character export (modeling)", "fbx export", ".X export (games)")
		button btnImport "Import" width:100 align:#center
		label lblNotes "" width:250 height:200 align:#left -- IMPORTANT: must unhide all items before exporting .x file.
		
 		on btnBrowse pressed do
		(
			local f = getOpenFileName types:"BMD (*.bmd)"
			if f != undefined then
			(
				fileType = getFilenameType f
				if fileType  == ".bmd" OR fileType == ".BMD" OR fileType  == ".bdl" OR fileType == ".BDL" then
					txtFilePath.text = f
				else
					messageBox "Only BMD or BDL files are supported"
			)
		)

		on btnImport pressed do
		(
			if (txtFilePath.text == undefined OR txtFilePath.text == "") then
				messageBox "You must select a BMD file to import"
			else
			(
				Undo off 
				(
					btnImport.enabled = false
					bmd = BModel()
					bmd.SetBmdViewExePath (getDir #maxroot)

					-- UPDATE BMDVIEW.EXE PATH
					-- bmd.SetBmdViewExePath "C:\\" -- if Max version before 2008 and the path contains spaces the path will have to be manually updated
					local exportType = case radExportType.state of (
														1: #CHARACTER
														2: #FBX
														3: #XFILE
													)

					bmd.Import txtFilePath.text spnBoneThickness.value chkTextureMirror.checked chkForceBones.checked chkSaveAnim.checked chkExportTextures.checked exportType chkIncludeScaling.checked
					btnImport.enabled = true
				)
				
				Undo on (radExportType)
				
				DestroyDialog MaxBMDUI_Rollout
			)
		)
	)
	
	createDialog MaxBMDUI_Rollout 300 260

)