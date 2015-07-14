
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





