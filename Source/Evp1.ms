struct Evp1Header
(
  tag, -- char[4]  'EVP1'
  sizeOfSection, -- u32 
  count, -- u16 
  pad, -- u16 

  --0 - count many bytes, each byte describes how many bones belong to this index
  --1 - sum over all bytes in 0 many shorts (index into some joint stuff? into matrix table?)
  --2 - bone weights table (as many floats as shorts in 1)
  --3 - matrix table (matrix is 3x4 float array)
  offsets= #(), -- u32[4]
  
  fn LoadData br =
  (
    tag = br.ReadFixedLengthString 4
	sizeOfSection = br.ReadDWORD()
	count = br.ReadWORD()
	pad = br.ReadWORD()
	
	for i=1 to 4 do
	(
		offsets[i] = br.ReadDWORD()
	)
  )
  
)


struct MultiMatrix
(
  weights = #(), -- std::vector<float> ;
  indices = #() -- std::vector<u16> indices; //indices into Evp1.matrices (?)
)


struct Evp1
(
  weightedIndices = #(), -- std::vector<MultiMatrix> ;
  matrices = #(), -- std::vector<Matrix44f> ;
  
  fn LoadData br =
  (
    evp1Offset = br.Position()

    header = Evp1Header()
	header.LoadData br

	  -- read counts array
	  br.SeekSet (evp1Offset + header.offsets[1])
	  counts = #() -- vector<int> counts(h.count);
	  sum = 0
	  
	  for i = 1 to header.count do
	  (
	    v = br.GetByte() -- u8 v; fread(&v, 1, 1, f);
		sum += v
		counts[i] = v
	  )

	  weightedIndices = #() --  dst.weightedIndices.resize(h.count);
	  
	  -- read indices of weighted matrices
	  br.SeekSet (evp1Offset + header.offsets[2])
	  numMatrices = 0
	  for i = 1 to header.count do
	  (
	    weightedIndices[i] = MultiMatrix()
	    weightedIndices[i].indices = #() -- weightedIndices[i].indices.resize(counts[i]);
	    
		for j = 1 to counts[i] do
		(
		  d = br.ReadWORD()-- index to array (starts at one)
		  weightedIndices[i].indices[j] = d  
		  numMatrices = (Math.Maximum numMatrices (d+1))
		)
	  )
	
	  -- read weights of weighted matrices
	  br.SeekSet (evp1Offset + header.offsets[3])
	  
	  for i = 1 to header.count do
	  (
	    weightedIndices[i].weights = #() -- .resize(counts[i]);
	    for j = 1 to counts[i] do --(int j = 0; j < counts[i]; ++j)
	    (
		 -- error if f1 = br.GetFloat() used? can print value but assign = undefined
		  local fz = br.GetFloat()
		  weightedIndices[i].weights[j] = fz
	    )
	  )
	
	  -- read matrices
	  matrices = #() -- .resize(numMatrices);
	  br.SeekSet (evp1Offset + header.offsets[4])
	  for i = 1 to numMatrices do
	  (
	    matrices[i] = Matrix44()
		matrices[i].LoadIdentity()
		
		for j = 1 to 3 do
		(
		  for k = 1 to 4 do
		  (
		    matrices[i].m[j][k] = br.GetFloat()
		  )
		)
	  )
  )
)