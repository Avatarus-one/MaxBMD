
if (heapSize < 20000000) then
	heapSize = 20000000 -- allow ~ 20 MB instead of just 7.5 MB. Prevents "Runtime Error: Out of scripter memory"



include "Math.ms"
include "BinaryReader.ms"
include "Vector3.ms"
include "Matrix44.ms"
include "Inf1.ms"
include "Vtx1.ms"
include "Shp1.ms"
include "Jnt1.ms"
include "Evp1.ms"
include "Drw1.ms"
include "Bck.ms"
include "Mat3.ms"
include "Tex1.ms"
include "StringHelper.ms"
include "FrameNode.ms"
include "ReassignRoot.ms"
include "Btp.ms"

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
		for i = 1 to vertices.count  do
		(
			if (normals[i] != undefined) then
				setNormal modelMesh i normals[i] 
		)

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
			-- NOTE: check ash.bmd for case when wrapS=2 and wrap=2. u_offset = 0.5 and V_offset = -0.5 [note negative on v]
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
					/*
					_currMaterial.diffusemap.coords.V_Mirror = false
					_currMaterial.diffusemap.coords.V_Tile = true
					
					if (hasAlpha) then
					(	
						_currMaterial.opacityMap.coords.V_Mirror = false
						_currMaterial.opacityMap.coords.V_Tile = true
					)*/
				)
				else if (tex.texHeaders[v2].wrapT == 2) then
				(
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
	
	--if ((rootFrameNode.children.count == 1 AND rootFrameNode.children[1].children.count == 0)) then	-- FIX: THIS FORCES TO CREATE BONES EVEN IF THERE ARE ONLY ONE BONE. 
	--	_createBones = false   
	
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
	
	if (_createBones AND _exportType!=#XFILE) then
	(
		chr = CreateCharacter(rootFrameNode)
	    --RotateAroundWorld  chr (EulerAngles 90 0 0)
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
		
		local startFrame = 1
		
		local refBoneRequiresDummyList = #() -- remove dummy objects if not required
		for i = 1 to _bones.count do
			refBoneRequiresDummyList[i] = false
		
		for bckPath in _bckPaths do
		(
			
			bckFiles = getFiles (_bmdDir + bckPath)
			
			for f in bckFiles do
			(
				local bckFileName = getFilenameFile f
				local savePath = _bmdDir + "Animations\\" + bckFileName + ".anm"
				
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
					else
					(
						saveNodes _bones savePath
						--b.DeleteAllKeys _bones
						--rootFrameNode.ResetControllers() -- removes animations?
						--b.resetNodeAnim _bones[1]
						loadMaxFile saveMaxName	-- TODO: should only need to reset keys / clear animations
						_bones = rootFrameNode.RemapBones()
						
					--	rootFrameNode.ResetControllers() -- removes animations?
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

	-- if no tga files are found then extract the 
	tgaFiles = getFiles (_texturePath + "*.tga")

	-- cannot use shellLaunch because it doesn't wait for a return value
	-- don't use DOSCommand. Doesn't support spaces in full exe path. e.g. C:Program files\
	-- if using version before 2008 then use DOSCommand and set BmdView.exe into a known path
	if tgaFiles.count == 0 then
		TryHiddenDOSCommand ("BmdView.exe \"" + _bmdFilePath+ "\" \""+_texturePath+ "\\\"") _bmdViewPathExe

	classof tgaFiles
	ddsFiles = getFiles (_texturePath + "*.dds")
	
	-- create tga file and delete dds file
	for f in getFiles (_texturePath + "*.dds")  do 
	(
		local img = openBitMap f
		saveFileName = _texturePath + (getFilenameFile  f) + ".tga"
		local destImg = copy img
		destImg.filename = saveFileName
		save destImg -- cannot save img directly (requires copy to remove dds format)
		deleteFile f
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
	
fn Import filename boneThickness loadAnimations exportTextures exportType includeScaling =
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
	
	_allowTextureMirror = true -- doesn't work on characters? required for stages?
	
	_loadAnimations =loadAnimations
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
		
