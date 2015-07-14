
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

