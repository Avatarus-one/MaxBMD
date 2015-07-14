-- from ui/macroscripts/Macro_BoneAdjustmentsTools. on ReassignRoot_btn pressed do
fn removeIKsolvers a =
(
   if ( (not IsProperty a "pos") or (not IsProperty a "rotation") ) do
   (
	 HDIKSys.RemoveChain a
   )
)

fn getEndPoint a =
(
	if ( classOf(a) == BoneGeometry ) then
	(
	   [a.length,0,0] * a.objectTransform
	)
	else
	(
	   (a.transform).translation
	)
)

fn copyBoneHeightWidth destination source =
(
	if ( (source != undefined) and (classOf(source) == BoneGeometry) ) do
	(
		destination.width   = source.width
		destination.height  = source.height
	)
)

fn ReassignRoot currentBone =
(
	-- messageBox (currentBone as string)
		undo "Reassign Root" on
		(			
			with redraw off
			(
				with animate off
				(
					local deleteBoneArr = #()
					-- local currentBone   = selection[1]
					local selBone       = undefined
					local chlBone       = undefined
					local parentBone    = currentBone.parent
					local prevBone      = undefined
					local newBone       = undefined
					local newBoneArr    = #()
					local endBone       = undefined
					local revReset
					local exPrevBone    = undefined
					local i
						
					fn isPosSame a b =
					(
						local posTol = 5
						v1=a
						v2=b
						vi=0
						
						if ((v1.x) <= (v2.x  + posTol)) and ((v1.x) >= (v2.x  - posTol)) then vi +=1
						if ((v1.y) <= (v2.y  + posTol)) and ((v1.y) >= (v2.y  - posTol)) then vi +=1
						if ((v1.z) <= (v2.z  + posTol)) and ((v1.z) >= (v2.z  - posTol)) then vi +=1
						
						if vi > 1 then true else false
					)
				
					append deleteBoneArr currentBone

					removeIKsolvers currentBone
		
					if currentBone.children.count > 0 then
					(
						chlBone = currentBone.children
						revReset = true
					)


					if (classOf(currentBone) == BoneGeometry) and (currentBone.length == 10) and (currentBone.children.count == 0) then 
					(
						currentBone = parentBone
						parentBone = currentBone.parent
						append deleteBoneArr currentBone
					)

					if (parentBone != undefined) then
					(
						do   -- bone creation loop
						(
					        removeIKsolvers currentBone

						    if ( classOf(currentBone) == BoneGeometry ) then
							(
								newBone = boneSys.createBone (getEndPoint currentBone) currentBone.transform.translation currentBone.dir
								copyBoneHeightWidth newBone currentBone
								newBone.name = currentBone.name
								newBone.wirecolor=currentBone.wirecolor
								newBone.parent = prevBone
								newBone.resetBoneStretch()
								
 								if (parentBone.children.count > 1) and (parentBone.parent != undefined) then
								(
									parentBone.children.parent =  newBone
								)
								
								if (newBone.children == 0) and (newBone.length == 10) then
								(
									delete newBone
								)
								
								if chlBone != undefined then
								(
									chlBone.parent=newBone
								)
								
								if prevBone == undefined then
								(
									selBone = newbone
								)				
				

								prevBone = newBone
								currentBone = parentBone
								parentBone = currentBone.parent
								
								if ( classOf(currentBone) == BoneGeometry ) do append deleteBoneArr currentBone
								append newBoneArr newBone
							)
							else
							(
 								if (parentBone.children.count > 1) and (parentBone.parent != undefined) then
								(
								  local siblings = #()
								  for  b in parentBone.children do
								  (
								    if b != currentBone then
									(
									  append siblings b
									)
								  )
								  for i = 1 to siblings.count do
								  (
									(siblings[i]).parent = currentBone
								  )
								)

								if chlBone != undefined then
								(
									chlBone.parent=currentBone
								)

								if prevBone == undefined then
								(
									selBone = currentBone
								)	

								exPrevBone  = prevBone
								prevBone    = currentBone
								currentBone = parentBone
								parentBone  = currentBone.parent
								prevBone.parent = exPrevBone
								if ( classOf(currentBone) == BoneGeometry ) do append deleteBoneArr currentBone
							)
						
						) while (parentBone != undefined) -- bone creation loop

				        --removeIKsolvers currentBone

						if currentBone.children.count > 1 then
						(
							if ( classOf(currentBone) == BoneGeometry ) then
							(
						        local chlVar = #()

								for b in currentBone.children do
								(
					                --removeIKsolvers b
									append chlVar b
									b.parent = undefined
								)

								newBone = boneSys.createBone (getEndPoint currentBone) currentBone.transform.translation currentBone.dir
								copyBoneHeightWidth newBone currentBone
								newBone.name = currentBone.name
								newBone.wirecolor=currentBone.wirecolor
								newBone.parent = prevBone		
								
								chlVar.parent=newBone
								
								newBone.realignBoneToChild()
								newBone.resetBoneStretch()
								append newBoneArr newBone
							)
							else
							(
								currentBone.parent = prevBone		
								append newBoneArr currentBone
							)
						)
						else
						(
							if ( classOf(currentBone) == BoneGeometry ) then
							(
								newBone = boneSys.createBone (getEndPoint currentBone) currentBone.transform.translation currentBone.dir
								copyBoneHeightWidth newBone currentBone
								newBone.name = currentBone.name
								newBone.wirecolor=currentBone.wirecolor
								newBone.parent = prevBone
								append newBoneArr newBone
								
								parentBone = newBone
								
								newBone=BoneSys.createBone parentBone.transform.translation (parentBone.transform.translation+6) parentBone.dir
								copyBoneHeightWidth newBone parentBone
								newBone.rotation=parentBone.rotation
								newBone.pos=parentBone.transform.translation
								in coordSys Local move newBone [parentBone.length,0,0]
								newBone.parent=parentBone
								newBone.width=parentBone.width
								newBone.height=parentBone.height
								newBone.taper=90
								newBone.length=(parentBone.width+parentBone.height)/2
								newBone.wirecolor=parentBone.wirecolor
							)
							else
							(
								currentBone.parent = prevBone
							)
						)					
						
						for b in deleteBoneArr do
						(
						  if not isDeleted b do delete b
						)
						
						if (revReset != true) then
						(
							for i=1 to newBoneArr.count do 
							(
								(newBoneArr[i]).resetBoneStretch()
							)
						)
						else
						(
							for i=newBoneArr.count to 2 by -1 do 
							(
								(newBoneArr[i]).resetBoneStretch()
							)
						)
						
						
						select selBone
					)
				)
			)	
		)	
	)
	