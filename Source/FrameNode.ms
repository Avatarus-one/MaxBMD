
struct FrameNode
(
    name,
	startPoint, -- end point = first child
	children = #(), -- FrameNode
	 parentFrameNode,
	 effP,
	f,
	 _bone,
	_eulerController, -- used for animations

	_dummyHelper,
	
	--_dummyHelperRequired = false,
	
	fn RemoveDummyHelper =
	(
		if (_bone != undefined AND _dummyHelper != undefined) then
		(
			_bone.parent = _dummyHelper.parent
			delete _dummyHelper
			_dummyHelper = undefined
		)
	),
	
	-- required for character assembly
	fn _GetAllNodes nodes =
	(
		if (_bone != undefined) then
		(
			append nodes _bone 
			if (_dummyHelper != undefined) then
				append nodes _dummyHelper -- required 
		)
		
		for child in children do
		   child._GetAllNodes nodes
	),
	
	fn GetAllNodes =
	(
		local retNodes = #()
		_GetAllNodes(retNodes)
		return retNodes
	),
	
	-- parent scale has no effect on child bone (prevents skewing)
	-- can move child bone without auto scaling parent bone (face animation)
	-- should only scale when the parent bone only has one child?
	fn FixBones boneThickness =
	(
		/*
		local parent = _bone
		
		if (parent != undefined) then
		(
			local x = XForm()
			addModifier parent x 
			x.gizmo.scale = [1, 1, 1] -- Gizmo
			parent.boneScaleType = #none -- don't squash bones
		)
		for childFrame in children do
		(
			local child = childFrame._bone
			-- IMPORTANT: only use local scale on XForm object
			-- Works with IK
			-- NOTE: only updates position on x scale. e.g. body / arms: arms don't move outward when chest scaled on y axis
			if (parent != undefined) then
			(
				local d = distance child.pos  parent.pos
				paramWire.connect parent.modifiers[#XForm].Gizmo.controller[#Scale] child.pos.controller[#X_Position] ((d as string) + " * (Scale.x)")
			)
			childFrame.FixBones boneThickness
		)*/
	
		--local parentBone = _bone
		for child in children do
		(
			if (_bone != undefined) then
			(
				local childBone = child ._bone

				local d  = dummy boxsize:[boneThickness, boneThickness, boneThickness] name: (childBone.name +"_dummy")
				--local d = point size:boneThickness name: (childBone.name +"_dummy")

				
				d.transform = _bone.transform -- not rotation
				d.position = childBone.position -- end points should match parent direction? Watch out for multi child bones e.g. back -> 2 shoulders (back shouldn't scale)
				
				-- in coordsys world (d.position.x = child.position.x) -- only move along x axis?
				d.parent = _bone
				childBone.parent = d
				--in coordsys parent (child.position = [0,0,0] ) -- using dummy position instead
				
				
				paramWire.connect _bone.transform.controller[#Scale] d.transform.controller[#Scale] "1/Scale"
				freeze d -- don't hide or .x export won't work
				--hide d
				child._dummyHelper = d
			--)
			)
			child.FixBones boneThickness
		)
	),

	-- private
	 fn _PrintTree depth =
	(
	   if (name != undefined) then
	   		print (depth + name)
		
		for child in children do
		   child._PrintTree (depth + "--")
	),
	
	 -- used for testing
	 fn PrintTree =
	(
		_PrintTree ""
	),
	

	
	-- private
	fn _CreateBones parentBone boneThickness createdBonesTable postfixName parentTransform  =
	(
	     local bone = undefined
		 local endPoint = undefined
		 
	     if (parentFrameNode != undefined) then
		 (
			   if (children.count > 0 ) then
			 (
	      		endPoint = children[1].startPoint
			 )
			 
			 else
			(
				-- create an end point bone (same direction as parent bone with a length of boneThickness)
			    local start = startPoint 
				if (parentBone != undefined) then		-- THIS FIXES IMPORT OF MODELS WITH ONE BONE
					dir  = normalize (parentBone.position - start) 
				else
					dir  = [0,0,0] 
				dir  *= (-1 * boneThickness)
				dir += startPoint 
				endPoint = [dir.x, dir.y, dir.z]
				--endPoint = [dir.x, -dir.z, dir.y] -- using orig cords
			)
		   		
			
			 
		   if (parentBone == undefined) then
			(
				endPoint = [startPoint.x, startPoint.y + boneThickness, startPoint.z] 
			)
			
			--startPoint = [0,0,0]
			--endPoint = [10, 0, 0]
			
			 bone = bonesys.createbone startPoint endPoint z_axis
		--	freeze bone -- don't hide or .x export won't work
			_bone = bone
			
			--_bone.boneFreezeLength=false -- prevent scale errors on animations. e.g. talking animations scale head?
			--_bone.boneAutoAlign=false
			
			_bone.scale.controller = ScaleXYZ ()
			
			
			mTransform = matrix3 1 -- identity
			mt = transMatrix [f.t.x, f.t.y, f.t.z]
			mx = rotateXMatrix f.rx
			my = rotateYMatrix f.ry
			mz = rotateZMatrix f.rz
			
			if (parentBone != undefined) then
			(
				mTransform =  (mx * my * mz * mt) * parentTransform
				
				bone.transform = mTransform
				bone.parent = parentBone
			)
			else
			(
				mTransform =  (mx * my * mz * mt) 
				bone.transform = mTransform
			)
			
			  bone.name = name + postfixName 
			  bone.width = boneThickness
			  bone.height = boneThickness
			  append createdBonesTable bone
	 
		 )		 
		 
		 for child in children do
		   child._CreateBones bone boneThickness createdBonesTable postfixName  mTransform 
	),
	
	
	-- private: NYI
	fn _FixBoneLength =
	(
		for childBone in children do
		(
		
			if (_bone != undefined AND _bone.parent != undefined) then
			(
				mt = transMatrix [f.t.x, f.t.y, f.t.z]
				mx = rotateXMatrix f.rx
				my = rotateYMatrix f.ry
				mz = rotateZMatrix f.rz
				
				parentVec = normalize ((in coordsys world _bone.position) - (in coordsys world _bone.parent.position))
			
				mTransform =  (mx * my * mz * mt) 
				
				boneVec2 = normalize ((normalize _bone.dir) * mTransform)
				
				print (((parentVec as string) + ":" + (boneVec2 as string)))
			)
			
			childBone._FixBoneLength()
		)
	),
	
	fn _ToArray items = 
	(
		 
		for child in children do
		(
			append items child
		   child._ToArray items
		)
	),
	
	fn ToArray =
	(
		local items = #()
		_ToArray items
		return items
	),
	
	fn _CreateParentBoneIndexs items itemIndex parentIndex depth =
	(
		
		append items parentIndex

		
		parentIndex = itemIndex[1] 
		--if (name != undefined) then -- first item is undefined
		--	print (depth + name + ":" + (parentIndex as string) + ":" + (itemIndex[1] as string))
		
		itemIndex[1] = itemIndex[1] + 1
		for child in children do
		   child._CreateParentBoneIndexs items itemIndex parentIndex (depth + "--")
		
		
	),
	
	fn CreateParentBoneIndexs =
	(
		local items = #()
		local itemIndex = #() -- only contains one value. Pass by reference?
		itemIndex[1] = 0
		_CreateParentBoneIndexs items itemIndex 0 "--"
		
		deleteItem  items 1 -- first item not used
		return items
	),
	
	-- returns new bones array
	fn CreateBones boneThickness postfixName  =
	(
		with animate off
		(
			if postfixName == undefined then
				postfixName = ""
		   createdBonesTable = #()
			
			mTransform = matrix3 1 -- identity
				
			_CreateBones undefined boneThickness createdBonesTable postfixName mTransform 
			
			
		)
		
		return createdBonesTable 
	),
	
	fn _RemapBones boneSet =
	(
		_bone = getNodeByName name
		if (_bone != undefined) then
			_dummyHelper = getNodeByName(_bone.name +"_dummy")
		
		append boneSet _bone 
		for child in children do
		   child._RemapBones boneSet
	),
	
	-- used on hold, fetch. Bone references lost.
	fn RemapBones =
	(
		local boneSet = #()
		 for child in children do
		   child._RemapBones boneSet
		 
		 return boneSet
	),
	
		
	fn ResetControllers =
	(
		  if (_bone != undefined ) then
			(
				if (_bone.parent != undefined ) then 
				(
					m = Euler_XYZ()
					/*
					m.x_rotation = f.rx
					m.y_rotation = f.ry
					m.z_rotation = f.rz
					*/
					_bone.scale.controller = ScaleXYZ () -- resets animations
					
					_eulerController = m
					_bone.rotation.controller = m 	

					pos = Position_XYZ()
					/*pos.x_position = f.t.x 
					pos.y_position = f.t.y
					pos.z_position = f.t.z*/
					
					_bone.position.controller = pos
				)
			)
			
		 for child in children do
		   child.ResetControllers()
	)
	
	
)
