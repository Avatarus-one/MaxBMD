

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
	

