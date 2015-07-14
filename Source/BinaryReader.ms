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