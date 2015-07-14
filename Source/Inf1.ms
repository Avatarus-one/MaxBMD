

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

