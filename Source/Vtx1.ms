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