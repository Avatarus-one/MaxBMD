
struct BckKey
(
  time, -- float 
  value, -- float 
  tangent -- float  //??
)

struct BckJointAnim
(
  scalesX = #(), -- std::vector<Key>
  scalesY = #(), -- std::vector<Key>
  scalesZ = #(), -- std::vector<Key>

  rotationsX = #(), -- std::vector<Key>
  rotationsY = #(), -- std::vector<Key>
  rotationsZ = #(), -- std::vector<Key>

  translationsX = #(), -- std::vector<Key>
  translationsY = #(), -- std::vector<Key>
  translationsZ = #() -- std::vector<Key>
)


------------------------------------

struct BckAnk1Header
(
  tag, -- char[4] 'ANK1'
  sizeOfSection, -- u32 

  -- 0 - play once, 2 - loop
  loopFlags, -- u8 

  angleMultiplier, -- u8 all angles have to multiplied by pow(2, angleMultiplyer)

  animationLength, -- u16 in time units

  numJoints, -- u16 that many animated joints at offsetToJoints
  scaleCount, --u16  that many floats at offsetToScales
  rotCount, -- u16 that many s16s at offsetToRots
  transCount, -- u16 that many floats at offsetToTrans

  offsetToJoints, -- u32 
  offsetToScales, -- u32 
  offsetToRots, -- u32 
  offsetToTrans, -- u32 
  
  fn LoadData br = 
  (
  	  tag = br.ReadFixedLengthString 4
  	  sizeOfSection = br.ReadDWORD()
	  loopFlags = br.GetByte()
	  angleMultiplier = br.GetByte()
	  animationLength = br.ReadWORD()
	  numJoints = br.ReadWORD()
	  scaleCount = br.ReadWORD()
	  rotCount = br.ReadWORD()
	  transCount = br.ReadWORD()
	  offsetToJoints = br.ReadDWORD()
	  offsetToScales = br.ReadDWORD()
	  offsetToRots = br.ReadDWORD()
	  offsetToTrans = br.ReadDWORD()
	
  )
)


-- TODO: the following two structs have really silly names, rename them
struct BckAnimIndex
(
  count, -- u16 
  index, -- u16 
  zero, -- u16 always 0?? -> no (biawatermill01.bck) TODO: find out what it means
  
  fn LoadData br =
  (
     count = br.GetSHORT()
	 index = br.GetSHORT()
	 zero = br.GetSHORT()
  )
)

struct BckAnimComponent
(
  s = BckAnimIndex(), -- AnimIndex scale
  r = BckAnimIndex(), -- AnimIndex rotation
  t = BckAnimIndex(), -- AnimIndex translation
  
  fn LoadData br =
  (
     s.LoadData br
	 r.LoadData br
	 t.LoadData br
  )
)

struct BckAnimatedJoint
(

  --if count > 1, count*3 floats/shorts stored at index (time, value, unk [interpolation info, e.g. tangent??])?
  --for shorts, time is a "real" short, no fixedpoint
  x = BckAnimComponent(), -- AnimComponent 
  y = BckAnimComponent(), -- AnimComponent 
  z = BckAnimComponent(), -- AnimComponent 
  
  fn LoadData br =
  (
	 x.LoadData br
	 y.LoadData br
	 z.LoadData br
  )
)

-----------------------------------------------

struct Bck
(
  anims = #(), -- std::vector<JointAnim>
  animationLength, -- int 
  currAnimTime, -- float 
  
  -- ConvRotation(vector<Key>& rots, float scale)
  fn ConvRotation rots scale =
  (
     for j = 1 to rots.count do
	 (
	     rots[j].value *= scale
		 rots[j].tangent *= scale
	 )
	 
	 return rots
  ),
  
  -- void readComp(vector<Key>& dst, const vector<T>& src, const bck::AnimIndex& index)
  fn ReadComp dst src index =
  (
     dst = #() -- dst.resize(index.count);
	 
	  -- violated by biawatermill01.bck
	  if index.zero != 0 then
	  (
	    -- throw "bck: zero field %d instead of zero" -- ignore for now?
	    --TODO: biawatermill01.bck doesn't work, so the "zero"
	    --value is obviously something important
	  )
	  
	  if index.count <= 0 then
	  (
	    print "Warning: readComp(): count is <= 0"
	  )
	  else if index.count == 1 then
	  (
	    dst[1] = BckKey()
	    dst[1].time = 0
	    dst[1].value = src[index.index + 1]
	    dst[1].tangent = 0
	  )
	  else
	  (
	    for j = 0 to (index.count - 1) do -- (int j = 0; j < index.count; ++j)
	    (
		  dst[j + 1] = BckKey()
	      dst[j + 1].time = src[(index.index + 3*j) + 1]
	      dst[j + 1].value = src[(index.index + 3*j + 1) + 1]
	      dst[j + 1].tangent = src[(index.index + 3*j + 2) + 1]
	    )
	  )
  
  	return dst
  ),
  
  fn LoadAnk1 br =
  (

      i = 0
	  ank1Offset = br.Position()
	  
	  -- read header
      h = BckAnk1Header()
	  h.LoadData br
	  currAnimTime = 0.0
	  animationLength = h.animationLength
	  
	  -- read scale floats:
	  br.SeekSet (ank1Offset + h.offsetToScales)
	  scales = #() -- vector<f32> scales(h.scaleCount);
	  for i=1 to h.scaleCount do
	      append scales (br.GetFloat())

	  -- read rotation s16s:
	  br.SeekSet (ank1Offset + h.offsetToRots)
	  rotations = #()
	  for i=1 to h.rotCount do
	       append rotations (br.GetSHORT())
		   
	  -- read translation floats:
	  br.SeekSet (ank1Offset + h.offsetToTrans)
	  translations= #() 
	  for i=1 to h.transCount do
	      append translations (br.GetFloat()) 

	  -- read joints
	  rotScale = (pow (2 as float) h.angleMultiplier) * 180 / (32768 as float)
	  br.SeekSet (ank1Offset + h.offsetToJoints)
	  
	  anims = #() -- bck.anims.resize(h.numJoints);
	  for i = 1 to h.numJoints do
	  (
	    joint = BckAnimatedJoint()
		joint.LoadData br 
		
		anims[i] = BckJointAnim()

	    anims[i].scalesX = ReadComp anims[i].scalesX scales joint.x.s
	    anims[i].scalesY = ReadComp anims[i].scalesY scales joint.y.s
	    anims[i].scalesZ = ReadComp anims[i].scalesZ scales joint.z.s
	
	    anims[i].rotationsX = ReadComp anims[i].rotationsX rotations joint.x.r
	    anims[i].rotationsX = ConvRotation anims[i].rotationsX rotScale
	    anims[i].rotationsY = ReadComp anims[i].rotationsY rotations joint.y.r
	    anims[i].rotationsY = ConvRotation anims[i].rotationsY rotScale
	    anims[i].rotationsZ = ReadComp anims[i].rotationsZ rotations joint.z.r
	    anims[i].rotationsZ = ConvRotation anims[i].rotationsZ rotScale
	
	    anims[i].translationsX = ReadComp anims[i].translationsX translations joint.x.t
	    anims[i].translationsY = ReadComp anims[i].translationsY translations joint.y.t
	    anims[i].translationsZ = ReadComp anims[i].translationsZ translations joint.z.t
	  )
	  
  ),
  
  fn LoadBck filePath =
  (
      local br = BinaryReader()
	  br.Open filePath 
	  br.SeekSet 0x20 
	  local size = 0
	  local i = 0
	  do
	  (
		 
		br.SeekCur size
	    local pos = br.Position()
	    local tag = br.ReadFixedLengthString 4
		size = br.ReadDWORD()

		if(size < 8) then
			size = 8 -- prevent endless loop on corrupt data

		br.SeekSet pos
		
	    --if tag == "ANK1" then
		if tag == "ANK1" then
	      LoadAnk1 br
	    else
		(
	      messageBox ("readBck(): Unsupported section " + tag)
			throw  ("readBck(): Unsupported section " + tag)
		)
		
	    br.SeekSet pos

		i += 1
	  ) while i < 1
	  
		br.Close()
  ),
  
  
  fn Interpolate v1 d1 v2 d2 t = -- t in [0,1]
  (
  	   --cubic interpolation
	   -- float values
	  a = 2*(v1 - v2) + d1 + d2
	  b = -3*v1 + 3*v2 - 2*d1 - d2
	  c = d1
	  d = v1
	  --TODO: yoshi_walk.bck has strange-looking legs...not sure if
  	 --the following line is to blame, though
  	 return ((a*t + b)*t + c)*t + d
  ),
  
  fn GetAnimValue keys t =
  (
	  if keys.count == 0 then
	    return 0.0
	
	  if keys.count == 1 then
	    return keys[1].value
	
	  --messageBox (keys as string)
	 -- throw "E"
	  i = 2
	  while keys[i].time < t do
	    i += 1
	
	  time = (t - keys[i - 1].time)/(keys[i].time - keys[i - 1].time) -- scale to [0, 1]
	  return interpolate keys[i - 1].value keys[i - 1].tangent keys[i].value keys[i].tangent time
  ),
  
-- the caller has to ensure that jnt1.frames and bck.anims contain
--the same number of elements
fn AnimateJnt jnt deltaTime =
(
  -- update time
  currAnimTime += deltaTime --*16 -- convert from seconds to ticks (dunno if this is right this way...TODO)
  currAnimTime = mod currAnimTime animationLength -- loop?

  -- update joints
  for i = 1 to jnt.frames.count do
  (
    jnt.frames[i].sx = getAnimValue anims[i].scalesX currAnimTime
    jnt.frames[i].sy = getAnimValue anims[i].scalesY currAnimTime
    jnt.frames[i].sz = getAnimValue anims[i].scalesZ currAnimTime

    --TODO: use quaternion interpolation for rotations?
    jnt.frames[i].rx = getAnimValue anims[i].rotationsX currAnimTime
    jnt.frames[i].ry = getAnimValue anims[i].rotationsY currAnimTime
    jnt.frames[i].rz = getAnimValue anims[i].rotationsZ currAnimTime

    jnt.frames[i].t.x = getAnimValue anims[i].translationsX currAnimTime
    jnt.frames[i].t.y = getAnimValue anims[i].translationsY currAnimTime
    jnt.frames[i].t.z = getAnimValue anims[i].translationsZ currAnimTime
  )
),

-- IMPORTANT: scale values are absolute and not related to the parent
-- e.g Bone A (scale=200%), Bone B (Scale=200%), Bone C (Scale=100%). Bone A is the parent of Bone B and Bone B is the parent of Bone C
--  need to remove the parent scaling. e.g Bone C shouldn't change in size but in 3DS max it will equal 400% (2 * 2 * 1 * 100)


fn GetParentBoneScale currBone frameTime  = 
(
	local parentScale = Point3 1 1 1
	at time (frameTime)
	(
		local parentBone = currBone.parent
		while (parentBone != undefined) do
		(
			parentScale.x *= (parentBone.scale.controller.x_scale / 100)
			parentScale.y *= (parentBone.scale.controller.y_scale / 100)
			parentScale.z *= (parentBone.scale.controller.z_scale / 100)
	
			parentBone = parentBone.parent
		)
	)

	--return 1
	return parentScale
),

-- CalcScale anims[i].scalesX parentBoneIndexs 8
fn _CalcParentScale boneIndex keys parentBoneIndexs frame =
(
	-- if keys.count == 0 then
	--    return 1.0 -- identity
	 if (boneIndex <= 0 ) then
		return 1 -- identity
	
	local val = getAnimValue keys frame -- absolute value
	if (val < 0.1 ) then
		throw "E"
	if (val > 10) then 
		throw "Max"
	val = 1 / val
	
	
	if (parentBoneIndexs[boneIndex] > 0) then
		return (_CalcParentScale parentBoneIndexs[boneIndex] keys parentBoneIndexs frame) * val
	else
		return val
),

-- only calc on first parent. ignore boneIndex
fn CalcParentScale boneIndex keys parentBoneIndexs frame =
(

	return 1
	-- if (boneIndex <= 0 ) then
	--	return 1 -- identity
	
	local val = getAnimValue keys frame -- absolute value
	if (val < 0.0000001 ) then
		throw "E"
	if (val > 100000) then 
		throw "Max"
	val = 1 / val
	return val
	
	--return _CalcParentScale parentBoneIndexs[boneIndex] keys parentBoneIndexs frame
),

-- gets total x scale (excluding self)
-- bck file stores the absolute scale at that point and should ignore all parent bone scaling
-- e.g. bone a (200%) -> bone b (200%) -> bone C (200%).
-- bone a (1 * 2 * 100), bone b ((1 / 2 (bone a scale)) * 2 * 100 = 50 %), bone c (1/2 * 1/2 * 100 = 25%)
-- however, the parent bone is already scaled based on all items before it so only the parents scale is required. e.g. bone c (1/2 * 100 = 50) because bone b is already at 50%, total scale = 50%*50%=25%
-- WARNING: skewed bones?
fn CalcParentXScale anims parentBoneIndex parentBoneIndexs frame =
(
	--	return 1
	if (parentBoneIndexs[parentBoneIndex] <= 0 ) then -- root bone
		return 1 -- identity
	--return  (getAnimValue anims[parentBoneIndex].scalesX frame)
	
	local val = 1 / (getAnimValue anims[parentBoneIndex].scalesX frame) -- absolute value
	return val
	--return (CalcParentXScale anims parentBoneIndexs[parentBoneIndex] parentBoneIndexs frame) * val
),

fn CalcParentYScale anims parentBoneIndex parentBoneIndexs frame =
(
	--return 1
	if (parentBoneIndexs[parentBoneIndex] <= 0 ) then -- root bone
		return 1 -- identity
	--return  (getAnimValue anims[parentBoneIndex].scalesY frame)
	local val = 1 / (getAnimValue anims[parentBoneIndex].scalesY frame) -- absolute value
	return val
	--return (CalcParentYScale anims parentBoneIndexs[parentBoneIndex] parentBoneIndexs frame) * val
),

fn CalcParentZScale anims parentBoneIndex parentBoneIndexs frame =
(
	--return 1
	if (parentBoneIndexs[parentBoneIndex] <= 0 ) then -- root bone
		return 1 -- identity
	--return  (getAnimValue anims[parentBoneIndex].scalesZ frame)
	local val = 1 / (getAnimValue anims[parentBoneIndex].scalesZ frame) -- absolute value
	return val
	--return (CalcParentZScale anims parentBoneIndexs[parentBoneIndex] parentBoneIndexs frame) * val
),

fn GetPositionBone curBone =
(
	local dummyBone = (getNodeByName (curBone.name + "_dummy"))
	if dummyBone == undefined then
		return curBone
	else
		return dummyBone
),	

fn ValidateScale curBone scaleValue =
(
		--if (scaleValue != 1 and curBone.children.count > 1) then
		--	throw (curBone.name + " unable to scale ( " +(scaleValue as string)+" ) bones with more than one child bone")
),


-- could use timeOffset to load all animations one after another
-- parentBoneIndexs array of ints. e.g. parentBoneIndexs[2] = 1 (2nd bones parent is the first bone)
fn AnimateBoneFrames timeOffset bones frameScale rootBoneOffset exportType refBoneRequiresDummyList includeScaling =
(
	
	if (exportType == #CHARACTER AND animationLength > 0) then
		timeOffset = 0

	--alert (bones.count as string)
	rootBoneOffset = [0,0,0] 
	animate on
	(  
	  
		for i = 1 to bones.count do
		(  
			
	
			bone = bones[i]
			anim = anims[i]
			
			-- animated bones require scaling helper
			if (anim.scalesX.count > 1 OR anim.scalesY.count > 1 OR anim.scalesZ.count > 1) then
				refBoneRequiresDummyList[i] =true -- bone.name
				
			if (anim.translationsX.count > 1 OR anim.translationsY.count > 1 OR anim.translationsZ.count > 1) then
				bone.boneEnable=false -- allow moving bone without affecting parent bone
				
				--messageBox (anim.scalesX as string) -- only one value if position not animated. value = 0
				--messageBox (anim.translationsY as string) -- only one value if scale not animated. value = 1
			
			for j = 1 to anim.rotationsX.count do
			(
				rot = anim.rotationsX[j]
				at time ((rot.time * frameScale) + timeOffset) (bone.rotation.controller.x_rotation = rot.value )
			)
			
			for j = 1 to anim.rotationsY.count do
			(
				rot = anim.rotationsY[j]
				at time ((rot.time * frameScale) + timeOffset) (bone.rotation.controller.y_rotation = rot.value )
			)
			
			for j = 1 to anim.rotationsZ.count do
			(
				rot = anim.rotationsZ[j]
				at time ((rot.time * frameScale) + timeOffset) (bone.rotation.controller.Z_rotation = rot.value )
			)
			

			for j = 1 to anim.translationsX.count do
			(
				t  = anim.translationsX[j]
				at time ((t.time * frameScale) + timeOffset) ((GetPositionBone bone).position.controller.x_position = t.value - rootBoneOffset.x)
			)
				
			for j = 1 to anim.translationsY.count do
			(
				t  = anim.translationsY[j]
				at time ((t.time * frameScale) + timeOffset) ((GetPositionBone bone).position.controller.y_position = t.value - rootBoneOffset.y)
			)
				
			for j = 1 to anim.translationsZ.count do
			(
				t  = anim.translationsZ[j]
				at time ((t.time * frameScale) + timeOffset) ((GetPositionBone bone).position.controller.z_position = t.value - rootBoneOffset.z )
			)

			if (includeScaling) then
			(
				for j = 1 to anim.scalesX.count do
				(
					local s  = anim.scalesX[j]
					at time ((s.time * frameScale) + timeOffset) 
					(
						ValidateScale bone s.value
						in coordsys local (bone.scale.controller.x_scale =  s.value * 100)
					)
				)
				for j = 1 to anim.scalesY.count do
				(
					local s  = anim.scalesY[j]
					at time ((s.time * frameScale) + timeOffset) 
					(
						ValidateScale bone s.value
						in coordsys local (bone.scale.controller.y_scale =  s.value * 100)
					)
				)
				for j = 1 to anim.scalesZ.count do
				(
					local s  = anim.scalesZ[j]
					at time ((s.time * frameScale) + timeOffset) 
					(
						in coordsys local 
						(
							ValidateScale bone s.value
							in coordsys local (bone.scale.controller.z_scale =  s.value * 100)
						)
					)
				)
			)
			
			rootBoneOffset = [0,0,0] -- only the root bone has an offset. bones[1]
		) -- for i = 1 to bones.count do
	
		  -- IMPORTANT: set all transforms for the last frame. prevents errors when frames loaded on after another animation
		if (animationLength > 0) then
		(
			local endFrame = timeOffset + animationLength
			for i = 1 to bones.count do
			(  
				local bone = bones[i]
				local anim = anims[i]

				addNewKey bone.rotation.controller endFrame
				addNewKey bone.position.controller endFrame
				addNewKey bone.scale.controller endFrame
				-- only seems to create a new keyframe if the value changes (+ 0.0000000000000001)
				local delta = 0.0000001
				at time (endFrame) (bone.rotation.controller.x_rotation = (anim.rotationsX[anim.rotationsX.count]).value + delta) --
				at time (endFrame) (bone.rotation.controller.y_rotation = (anim.rotationsY[anim.rotationsY.count]).value + delta) --+ delta
				at time (endFrame) (bone.rotation.controller.z_rotation = (anim.rotationsZ[anim.rotationsZ.count]).value + delta) --+delta

				at time (endFrame) 
				(
					local posBone = GetPositionBone bone
					posBone.position.controller.x_position = (anim.translationsX[anim.translationsX.count]).value  + delta
					posBone.position.controller.y_position = (anim.translationsY[anim.translationsY.count]).value  + delta
					posBone.position.controller.z_position = (anim.translationsZ[anim.translationsZ.count]).value  + delta
					
					if (includeScaling) then
					(
							in coordsys local (bone.scale.controller.x_scale =  ((anim.scalesX[anim.scalesX.count]).value * 100) + delta)
							in coordsys local (bone.scale.controller.y_scale =  ((anim.scalesY[anim.scalesY.count]).value * 100) + delta)
							in coordsys local (bone.scale.controller.Z_scale =  ((anim.scalesZ[anim.scalesZ.count]).value * 100) + delta)
					)
				)
			) -- for i = 1 to bones.count do
		) -- if (animationLength > 0) then*/
	
		
		if (exportType == #CHARACTER AND animationLength > 0) then
		(
			if (animationLength > 0) then
				animationRange = interval 0 (animationLength * frameScale)
			else
				animationRange = interval 0 1
		)

	) -- animate on

),

	
		
-- deltaTime in ticks
fn AnimateBones bones deltaTime =
(
  -- update time
  currAnimTime += deltaTime -- *16 -- convert from seconds to ticks (dunno if this is right this way...TODO)
  currAnimTime = mod currAnimTime animationLength -- loop?

  -- update joints
  for i = 1 to bones.count do
  (
  	bone = bones[i]
	
    --TODO: use quaternion interpolation for rotations?
    rx = getAnimValue anims[i].rotationsX currAnimTime
    ry = getAnimValue anims[i].rotationsY currAnimTime
    rz = getAnimValue anims[i].rotationsZ currAnimTime
	bone.rotation.controller.x_rotation = rx
	bone.rotation.controller.y_rotation = ry
	bone.rotation.controller.z_rotation = rz
	
    tx = getAnimValue anims[i].translationsX currAnimTime
    ty = getAnimValue anims[i].translationsY currAnimTime
    tz = getAnimValue anims[i].translationsZ currAnimTime
	bone.position.controller.x_position = tx
	bone.position.controller.y_position = ty
	bone.position.controller.z_position = tz
	
	sx = getAnimValue anims[i].scalesX currAnimTime
    sy = getAnimValue anims[i].scalesY currAnimTime
    sz = getAnimValue anims[i].scalesZ currAnimTime
	bone.scale.controller.x_scale = sx * 100
	bone.scale.controller.y_scale = sy * 100
	bone.scale.controller.z_scale = sz * 100
  )
),

-- from stdplugs/stdscripts/CharacterPluginObject.ms
fn resetAnim fromAnim =
(
	if fromAnim.controller != undefined then
		deleteKeys fromAnim.controller #allKeys			
	
	-- delete anims from custom attributes
	for k=1 to (custAttributes.count fromAnim) do 
	(
		local ca = custattributes.get fromAnim k 
		if (ca != undefined) do
		(
			local saNames = getSubAnimNames ca

			for s=1 to saNames.count do 
			(
				if (ca[s].controller != undefined) do (
					deleteKeys ca[s].controller #allKeys
				)	
			)
		)
	)
	
	for j=1 to fromAnim.numSubs do
	(
		resetAnim fromAnim[j]
	)
),

-- from stdplugs/stdscripts/CharacterPluginObject.ms
fn resetNodeAnim node = 
(
	resetAnim node.controller
	resetAnim node.baseObject
	for m in node.modifiers do (
		resetAnim m
	)
),

fn DeleteAllKeys bones =
(
	if (animationLength > 0) then
	(
			-- move all the keys back by one frame (extra frame was required to prevent export / import bug)
			for i = 1 to bones.count do
			( 
				local b = bones[i]
				selectKeys b.rotation.controller  (interval 1 (animationLength+5))
				moveKeys  b.rotation.controller (-animationLength+1) #selection
				deleteKeys  b.rotation.controller #selection
				
				selectKeys b.position.controller  (interval 1 (animationLength+5))
				moveKeys  b.position.controller (-animationLength+1) #selection
				deleteKeys  b.position.controller #selection
				
				selectKeys b.scale.controller  (interval 1 (animationLength+5))
				moveKeys  b.scale.controller (-animationLength+1) #selection
				deleteKeys  b.scale.controller #selection
				
				--moveKeys  b -animationLength
			)
	)
)

)
