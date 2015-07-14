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

