

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
  chanControls = #(), -- u16[4];
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