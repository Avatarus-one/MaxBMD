
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