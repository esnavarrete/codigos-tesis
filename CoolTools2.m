(* ::Package:: *)

BeginPackage["CoolTools`"]
Needs["Carlos`"]
Needs["MaTeX`"]
GetDataBruto::usage="Get pure states whos image is a given target state, polarized in the
z direction GetDataBruto[{SwapProbability_, MaxError_, SampleSize_, rz_}]"
randomKets::usage ="randomKets[dim,m] returns a list of m random kets of dimension 'dim'. The ket components are generated with the standard normal distribution."
ketsToDensity::usage ="Takes a list of kets and returns their corresponding density matrices by taking the dyadic product."
densityMatrixToPoint::usage ="Takes a list of density matrices and creates their generalized Bloch vectors over the specified basis."
nQubitBasis::usage"Given an integer n, gives the SU(2^n) generators (athough not properly normalized) 
by taking the tensor product between the elements of the pauli matrices and the identity matrix."
swapGate::usage ="It's the SWAP gate for a two qubit system."
fuzzy::usage ="Applies a fuzzy map over a density matrix with a probability p of permutation."
partialTraceB::usage ="Traces out the second element in a two qubit system."
partialTraceA::usage="Traces out the first element in a two qubit system."
coarseGraining2::usage="Given a probability p of 'mismeasurement', applies the Coarse Graining map over a two qubit system density matrix."
coarseGrainingBr::"Applies the coarse graining map from Silva et al."
nearVectorQ::usage="nearVectorQ[{vector,obj},norm,eps] returns True whenever norm[vector-obj]<eps."
selectNearVectors::usage="selectNearVectors[vectors,obj,norm,eps] returns all elements from vectros such as nearVectorQ is True."
traceNorm::usage="Returns the trace norm of a given matrix."
traceDistance::usage="Applies traceNorm to the difference of two matrices and divides it by two."
getDistances::usage="Given a list of density matrices, getDistances[dms,obj,norm] returns a list of norm-induced metrics between 
the elements of dms and obj."
getDistancesAfterCG::usage="Given a list of two qubit system density matrices, getDistancesAfterCG[dms,obj,norm,p] returns a list of 
norm-induced metrics between obj and the image of each element of dms under a Coarse Graining map."
fidelity::usage="Computes the fidelity between two states."
selectNearAfterCG::usage="Similar to selectNearVectors, but selects states such as their image under coarseGraining2 is near the target."
gellMannBasis::usage="Use it as gellMannBasis[1] or Use it as gellMannBasis[2] to obtain the Gell-Mann basis for one and two qubits."
evolutionOperator::usage="Computes evolution operator"
randomSmallEvolution::usage="Creates an evolution operator by evaluating the taylor series of exp(-iH) 
were H is a hermitian operator of shape {n,n} generated from the gaussian unitary ensemble.\n 
It is defined as:\n 
randomSmallEvolution[n_Integer,epsilon_?((0<#<1)&)]:=MatrixExp[-I*epsilon*RandomVariate[GaussianUnitaryMatrixDistribution[n]]]"
thermalMinimizationStep::usage="minimizationStep[{initialdm, tarjetdm},\[Epsilon],T,norm] applies a random evolution operator U=randomSmallEvolution[n,\[Epsilon]] 
to initialdm so that if the U.initialdm.Superscript[U,*] is closer to tarjetdm it takes that new value. 
If it's not, it can take that new value with probability T."
thermalMinimizationList::usage="distanceMinimizationList[{initialdm, tarjetdm},\[Delta],\[Epsilon],T,norm] returns a list of density operators obtained by minimizing 
norm(initialdm-tarjetdm). This is done by applying minimizationStep[{initialdm, tarjetdm},\[Epsilon],T,norm] recursively."
thermalMinimization::usage="distanceMinimization[{initialdm, tarjetdm},\[Delta],\[Epsilon],T,norm] returns a density operator obtained by minimizing 
norm(initialdm-tarjetdm). This is done by applying minimizationStep[{initialdm, tarjetdm},\[Epsilon],T,norm] recursively."
randomHermitianOld::usage="randomHermitianOld[n] creates a hermitian matrix of shape {n,n}.\n 
It is defined as:\n 
randomHermitian[n_Integer]:=With[{a=Table[RandomComplex[{-1-I,1+I}],{n},{n}]},(a+ConjugateTranspose[a])/2]"
randomPerturbationOld::usage="randomPerturbationOld[n,\[Epsilon]] creates a perturbation Hamiltonian of the form H=1+\[Epsilon]W, 
where 1 is the nxn identity matrix and W is a random hermitian operator of shape {n,n} generated using the function randomHermitianOld.\n 
It is defined as:\n 
randomPerturbation[n_Integer,epsilon_?((0<#<1)&)]:=IdentityMatrix[n]+epsilon*randomHermitian[n]"
randomSmallEvolutionOld::usage="randomSmallEvolutionOld[n,\[Epsilon]] it's a deprecated implementation of the smallEvolution function. 
It creates an evolution operator by evaluating the taylor series of exp(-iH) were H is a perturbation hamiltonian of shape {n,n} 
(not generated via the gaussian unitary ensemble).\n 
It is defined as:\n 
randomSmallEvolution[n_Integer,epsilon_?((0<#<1)&)]:=MatrixExp[-I*randomPerturbation[n,epsilon]]"
thermalMinimizationStepOld::usage="minimizationStepOld[{initialdm, tarjetdm},\[Epsilon],T,norm] applies a random evolution operator U=randomSmallEvolutionOld[n,\[Epsilon]] 
to initialdm so that if the U.initialdm.Superscript[U,*] is closer to tarjetdm it takes that new value. 
If it's not, it can take that new value with probability T."
thermalMinimizationListOld::usage="distanceMinimizationListOld[{initialdm, tarjetdm},\[Delta],\[Epsilon],T,norm] returns a list of density operators obtained by 
minimizing norm(initialdm-tarjetdm). This is done by applying minimizationStepOld[{initialdm, tarjetdm},\[Epsilon],T,norm] recursively."
thermalMinimizationOld::usage="distanceMinimizationOld[{initialdm, tarjetdm},\[Delta],\[Epsilon],T,norm] returns a density operator obtained by minimizing 
norm(initialdm-tarjetdm). This is done by applying minimizationStepOld[{initialdm, tarjetdm},\[Epsilon],T,norm] recursively."
carlosbasis::usage="Basis used to compute the evolution operator, created by Carlos."
evolutionOperator::usage="Evolution operator used to evolve and optimize the kets. It has the form exp[-I parameters.basis]"
lossFunction::usage="Loss function"
nGrad::usage="Numerical gradient. Just works for our loss function."
learningRateWithLineSearch::usage="Executes the Backtracking Line Search algorithm for the gradient descent."
descentStep::usage="Executes a single step of the gradient descent algorithm"
gradientDescent::usage="Repeated applications of descentStep to optimize states to a given target"
gradientDescentList::usage="Same as gradient descent, but it returns a list with all the parameters at each step"
gradientDescentMinLoss::usage="Same as gradient descent, but the stop condition is for the loss function"
gradientDescentMinLossList::usage="Same as gradientDescentMinLossList, but gives the complete list of parameters at each step"
visualizeBipartiteSystemWObj::usage="This is obsolete, we know where the target is. Please use visualizeBipartiteSystem instead."
visualizeBipartiteSystem::usage="Bipartite system visualization"
visualizeBipartiteSystemTwoSpheres::usage="Bipartite system visualization, but you can see the two spheres that emerge from the CG! It's pretty."
generatePureHaarStates::usage="gives out points that fall near a target state under the CG"
aTan2::usage="Properly defined arctan"
getAzimuthal::usage="Computes the azimuthal angle from a vector in cartesian coordinates"
MapMonitored::usage="Same as map but shows a progress bar"
middlePoints::usage="Returns the middle poitns of all the intervals of a given list of numbers."
partialtarcesBV::usage="Returns a list with the Bloch vectors corresponding to both partial traces of a given density matrix"
basisFromDirection::usage="Calculates a new orthonormal basis given a certain direction"
cartesianZChange::usage="ransforms the coordinates of a given set of vectors to a new reference system with a new direction and a new center"
cylindricalCoordinates::usage="Returns the cylindrical coordinates of a given point"
toCylindricalCoordinates::usage="Analogous to ToSphericalCoordinates, but you can also define a new center or a new Z direction"
tripleCylindricalPlot::usage="You know what this does, it's cool af"
tripleCylindricalPlot2::usage="The same as tripleCylindricalPlot"
noFillingHistogram::usage="Args:[hlist, axesLabels_List, color]. Generates a probability Histogram from the given histogram list 'hlist'. The generated histogram has no bottom color filling, just the edges."
tripleCylindricalPlotNoFilling::usage="Args:[sample, rz, colors_List, bins_List]. The same as tripleCylindricalPlot, but the Histograms have no bottom color fillings."
radialCylindricalDensity::usage="You know what this does, it's cool af"
azimuthalCylindricalDensity::usage="You know what this does, it's cool af"
zCylindricalDensity::usage="You know what this does, it's cool af"
\[Rho]estrella::usage="Calculate the density matrix composed of \!\(\*SubscriptBox[\(r\), \(i, j\)]\) elements, being the expected values of \[Rho] in the Pauli basis."
zCoordIntersetion\[Rho]a::usage="Calculate the height z of the circle where the Bloch sphere and the lower subsphere intersect."
volBipartiteSubSpaceTarget::usage="Calculate the volume of the subspace of pure states going to the target under the CGM."
element03AverageState::usage="Calculate the element \!\(\*SubscriptBox[\(A\), \(0, 3\)]\) where A is the average state density matrix of the set of states mapped to a target state projected in the z-direction, for a fixed p, in the \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)\[CircleTimes]\(\*SubscriptBox[\(\[Sigma]\), \(j\)]\) basis."
element30AverageState::usage="Calculate the element \!\(\*SubscriptBox[\(A\), \(3, 0\)]\) where A is the average state density matrix of the set of states mapped to a target state projected in the z-direction, for a fixed p, in the \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)\[CircleTimes]\(\*SubscriptBox[\(\[Sigma]\), \(j\)]\) basis."
element33AverageState::usage="Calculate the element \!\(\*SubscriptBox[\(A\), \(3, 3\)]\) where A is the average state density matrix of the set of states mapped to a target state projected in the z-direction, for a fixed p, in the \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)\[CircleTimes]\(\*SubscriptBox[\(\[Sigma]\), \(j\)]\) basis."
element22AverageState::usage="Calculate the element \!\(\*SubscriptBox[\(A\), \(2, 2\)]\) where A is the average state density matrix of the set of states mapped to a target state projected in the z-direction, for a fixed p, in the \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)\[CircleTimes]\(\*SubscriptBox[\(\[Sigma]\), \(j\)]\) basis."
element11AverageState::usage="Calculate the element \!\(\*SubscriptBox[\(A\), \(1, 1\)]\) where A is the average state density matrix of the set of states mapped to a target state projected in the z-direction, for a fixed p, in the \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)\[CircleTimes]\(\*SubscriptBox[\(\[Sigma]\), \(j\)]\) basis."
\[Rho]averageTwoQubits::usage="Calculate the average state density matrix of the set of states mapped to a target state projected in the z-direction, for a fixed p, in the \!\(\*SubscriptBox[\(\[Sigma]\), \(i\)]\)\[CircleTimes]\(\*SubscriptBox[\(\[Sigma]\), \(j\)]\) basis."
\[Rho]averageTwoQubitsbasiscomp::usage="Calculate the average state density matrix of the set of states mapped to a target state projected in the z-direction, for a fixed p, in the computational basis."
partialTrace23TripartiteSystem::usage="Function that calculate the partial double trace of a tripartite system, on particles 2 and 3. "
RadiusAsFunctionOfLagrange::usage="RadiusAsFunctionOfLagrange[\[Lambda],p] gives the radius of the Bloch vector of an effective state from the probability vector p used in the coarse graining map and Lagrange variable \[Lambda] obtained from the maximization of entropy."
LagrangeMulFromRadius::usage="RadiusAsFunctionOfLagrange[r,p] finds the lagrange multiplier corresponding to a radius r and probability vector p."
MaxEntSubStates::usage="MaxEntSubStates[\[Rho],p] generates a list of the states of the subsystems obtained from the maximum entropy assignment map. First argument is the effective state, second one is the probability vector used in the coarse graining map."
MaxEntN::usage="MaxEntN[\[Rho],p] gives de maximum entropy state compatible with the coarse graining map. First argument is the effective state, second one is the probability vector used in the coarse graining map."
CGNto1::usage="CGNto1[\[Rho],p] given an N particle system, calculates its N to 1 coarse graining. The preferential particle (the one being permutated) is the first one."
NQubitBasis::usage="Creates the N qubit SU basis by taking tensor products of the pauli matrices and the identity matrix."
listPlusP::usage="Generates the list of all possible subsets within the set of probabilities for the N-quibit CGM."
\[Xi]coeff::usage="Function that calculates the sign of the probability associated with the k-th reduced matrix."
\[CapitalSigma]0::usage="Take the first half of the elements generated by listPlusP[list]."
\[CapitalSigma]1::usage="Take the second half of the elements generated by listPlusP[list]."
\[CapitalSigma]01::usage="Generates sums of the elements of \[CapitalSigma]0 and \[CapitalSigma]1."
S\[Mu]::usage="part of the sum that gives rise to the volume of the subspace of pure states composed of N qubits that are brought to the target state by the CGM."
denom::usage="It's a function that calculates the denominator of the terms for the volume."
VolNqubits::usage="Volume of the subspace of pure states composed of N qubits that are brought to the target state by the CGM. 
Note that it gives the expression with P distinguished with subscripts ranging from 1 to N, to use the function properly, you must make the change from this notation to another 
without subscripts so that MAthematica can evaluate them properly, otherwise you will get wrong results."


Begin["`Private`"]

GetDataBruto[{SwapProbability_, MaxError_, SampleSize_, rz_}] := 
  ItppVectorToExpression /@ (ReadListUncomment[
     "!./cg -o get_states_bruto -s 0 --swap_probability " <> 
      ToString[SwapProbability] <> " --max_tolerance " <> 
      ToString[MaxError] <> " --sample_size " <> 
      ToString[SampleSize] <> " --r_z " <> ToString[rz], String]);
listP[lista_]:=Delete[Subsets[lista],{{1},{-1}}];
listPlusP[lista_]:=Delete[Total/@Subsets[lista],{{1},{-1}}];
\[Xi]coeff[G_,K_]:=If[(MemberQ[G,K]==True)||(MemberQ[{G},K]==True),1,-1];
\[CapitalSigma]0[lista_]:=Drop[listPlusP[lista],-2^(Length[lista]-1)+1];
\[CapitalSigma]1[lista_]:=Drop[listPlusP[lista],2^(Length[lista]-1)-1];
\[CapitalSigma]01[lista_,n_,m_]:=\[CapitalSigma]0[lista][[n]]-\[CapitalSigma]1[lista][[m]];
denom[lista_,n_]:=Times@@Rest[Total/@Subsets[\[Xi]coeff[Drop[listP[lista],-2^(Length[lista]-1)+1][[n]],lista[[#]]]*lista[[#]]&/@Range[Length[lista]]]];
S\[Mu][lista_,rz_,\[Mu]_]:=(((\[Mu]*\[CapitalSigma]01[lista,#,2^(Length[lista]-1)-#]+rz)^(2(2^(Length[lista]-1)-1)-1) HeavisideTheta[(-\[Mu]*\[CapitalSigma]01[lista,#,2^(Length[lista]-1)-#]-rz)])/(denom[lista,#])&/@Range[2^(Length[lista]-1)-1]);
VolNqubits[lista_,rz_]:=((2^Length[lista]-1)!/((2(2^(Length[lista]-1)-1)-1)!*2^(2(2^(Length[lista]-1)-1)-1)*\[Pi]^(Length[lista] (Length[lista]-1)/2) rz))(((1-rz)^(2(2^(Length[lista]-1)-1)-1)/(Times@@listPlusP[lista]))+Plus@@Join[S\[Mu][lista,rz,1],-S\[Mu][lista,rz,-1]]);
randomKets[dim_,m_]:= Table[Normalize[Complex @@@ RandomReal[NormalDistribution[],{dim,2}]],m];
ketsToDensity[{ket__List}]:=Map[Outer[Times,#,Conjugate[#]]&,{ket}];
densityMatrixToPoint[{rhos__List},{base__List}]:=Map[Rest[#]&,Chop[Transpose[Outer[Tr[#1 . #2]&,{base},{rhos},1]]]];
nQubitBasis[n_]:=Nest[Apply[KroneckerProduct,Tuples[{#,{IdentityMatrix[2],PauliMatrix[1],PauliMatrix[2],PauliMatrix[3]}}],{1}]&,{IdentityMatrix[2],PauliMatrix[1],PauliMatrix[2],PauliMatrix[3]},n-1]
swapGate={{1,0,0,0},{0,0,1,0},{0,1,0,0},{0,0,0,1}};
fuzzy[rho_List,p_Real?((0<=#<=1)&)]:=p*rho+(1-p)*swapGate . rho . ConjugateTranspose[swapGate];
partialTraceB[rho_?MatrixQ]:=With[{basis={{1,0},{0,1}}},Sum[KroneckerProduct[IdentityMatrix[2],{basis[[i]]}] . rho . KroneckerProduct[IdentityMatrix[2],Transpose[{basis[[i]]}]],{i,1,2}]]
partialTraceA[rho_?MatrixQ]:=With[{basis={{1,0},{0,1}}},Sum[KroneckerProduct[{basis[[i]]},IdentityMatrix[2]] . rho . KroneckerProduct[Transpose[{basis[[i]]}],IdentityMatrix[2]],{i,1,2}]]
coarseGraining2[rho_?MatrixQ,p_Real?((0<=#<=1)&)]:=partialTraceB[fuzzy[rho,p]]
coarseGrainingBr[rho_?((Dimensions[#]=={4,4} && MatrixQ[#])&)]:={{rho[[1,1]],(rho[[1,2]]+rho[[1,3]]+rho[[1,4]])/Sqrt[3]},{(rho[[2,1]]+rho[[3,1]]+rho[[4,1]])/Sqrt[3],rho[[2,2]]+rho[[3,3]]+rho[[4,4]]}}
nearVectorQ[{vector_List,objective_List} /;Dimensions[vector]==Dimensions[objective],norm_:Norm,epsilon_?(Positive[#]&)]:=norm[vector-objective]<epsilon
selectNearVectors[vectors_,objective_,norm_:Norm,epsilon_Positive:0.001]:=Select[vectors,nearVectorQ[#,objective,norm,epsilon]&]
traceNorm[dm_?MatrixQ]:=Chop[Tr[MatrixPower[ConjugateTranspose[(dm)] . (dm),0.5]]]
traceDistance[dm1_?MatrixQ,dm2_?MatrixQ]:=traceNorm[dm1-dm2]/2.
getDistances[dm_,objective_?MatrixQ,norm_:Norm]:=Map[norm[#-objective]&,dm]
getDistancesAfterCG[dm_,objective_?MatrixQ,norm_:Norm,p_]:=getDistances[Map[coarseGraining2[#,p]&,dm],objective,norm]
fidelity[rho1_?MatrixQ,rho2_?MatrixQ]:=With[{rho=MatrixPower[rho1,0.5]},Chop[ Tr[ MatrixPower[rho . rho2 . rho,0.5] ] ]^2.]
selectNearAfterCG[dmlist_List,tarjetdm_List,p_,epsilon_]:=Select[dmlist,(traceNorm[coarseGraining2[#,p]-tarjetdm]<epsilon)&]
NQubitBasis[n_?((#==1)&)]:=Table[PauliMatrix[i],{i,0,3}];
NQubitBasis[n_?((#!=1)&)]:=Flatten[Outer[KroneckerProduct,Sequence@@Table[Table[PauliMatrix[i],{i,0,3}],n],1],n-1];
gellMannBasis[n_?((#==1)&)]:={IdentityMatrix[2],
PauliMatrix[1],
PauliMatrix[2],
PauliMatrix[3]};
gellMannBasis[n_?((#==2)&)]:=Join[{IdentityMatrix[4]*Sqrt[2]},
Table[KroneckerProduct[PauliMatrix[k],IdentityMatrix[2]],{k,1,3}],
Table[KroneckerProduct[IdentityMatrix[2],PauliMatrix[k-3]],{k,4,6}],
Table[KroneckerProduct[PauliMatrix[1],PauliMatrix[k-6]],{k,7,9}],
Table[KroneckerProduct[PauliMatrix[2],PauliMatrix[k-9]],{k,10,12}],
Table[KroneckerProduct[PauliMatrix[3],PauliMatrix[k-12]],{k,13,15}]
]/Sqrt[2];
randomSmallEvolution[n_Integer,epsilon_?((0<#<1)&)]:=MatrixExp[-I*epsilon*RandomVariate[GaussianUnitaryMatrixDistribution[n]]]
thermalMinimizationStep[{initialstate_?MatrixQ,targetstate_?MatrixQ}/;Dimensions[initialstate]==Dimensions[targetstate],timestep_?((0<#<1)&),temperature_?((0<#<1)&),norm_:Norm]:=With[{evolution=randomSmallEvolution[Dimensions[initialstate][[1]],timestep]},With[{newstate=Chop[evolution . initialstate . ConjugateTranspose[evolution]]},If[norm[targetstate-newstate]<norm[targetstate-initialstate],newstate,RandomChoice[{temperature,(1-temperature)}->{newstate,initialstate}]]]]
thermalMinimizationList[{initialstate_?MatrixQ,targetstate_?MatrixQ}/;Dimensions[initialstate]==Dimensions[targetstate],tolerance_?((0<#<1)&),timestep_?((0<#<1)&),temperature_?((0<#<1)&),norm_:Norm]:=NestWhileList[thermalMinimizationStep[{#,targetstate},timestep,temperature,norm]&,initialstate,(norm[#-targetstate]>tolerance)&];
thermalMinimization[{initialstate_?MatrixQ,targetstate_?MatrixQ}/;Dimensions[initialstate]==Dimensions[targetstate],tolerance_?((0<#<1)&),timestep_?((0<#<1)&),temperature_?((0<#<1)&),norm_:Norm]:=NestWhile[thermalMinimizationStep[{#,targetstate},timestep,temperature,norm]&,initialstate,(norm[#-targetstate]>tolerance)&];
randomHermitianOld[n_Integer]:=With[{a=Table[RandomComplex[{-1-I,1+I}],{n},{n}]},(a+ConjugateTranspose[a])/2]
randomPerturbationOld[n_Integer,epsilon_?((0<#<1)&)]:=IdentityMatrix[n]+epsilon*randomHermitianOld[n]
randomSmallEvolutionOld[n_Integer,epsilon_?((0<#<1)&)]:=MatrixExp[-I*randomPerturbationOld[n,epsilon]]
thermalMinimizationStepOld[{initialstate_List,targetstate_List}/;Dimensions[initialstate]==Dimensions[targetstate],timestep_?((0<#<1)&),temperature_?((0<#<1)&),norm_:Norm]:=With[{evolution=randomSmallEvolutionOld[Dimensions[initialstate][[1]],timestep]},With[{newstate=Chop[evolution . initialstate . ConjugateTranspose[evolution]]},If[norm[targetstate-newstate]<norm[targetstate-initialstate],newstate,RandomChoice[{temperature,(1-temperature)}->{newstate,initialstate}]]]]
thermalMinimizationListOld[{initialstate_,targetstate_}/;Dimensions[initialstate]==Dimensions[targetstate],tolerance_?((0<#<1)&),timestep_?((0<#<1)&),temperature_?((0<#<1)&),norm_:Norm]:=NestWhileList[thermalMinimizationStepOld[{#,targetstate},timestep,temperature,norm]&,initialstate,(norm[#-targetstate]>tolerance)&];
thermalMinimizationOld[{initialstate_,targetstate_}/;Dimensions[initialstate]==Dimensions[targetstate],tolerance_?((0<#<1)&),timestep_?((0<#<1)&),temperature_?((0<#<1)&),norm_:Norm]:=NestWhile[thermalMinimizationStepOld[{#,targetstate},timestep,temperature,norm]&,initialstate,(norm[#-targetstate]>tolerance)&];
carlosbasis[n_]:=Flatten[Table[Module[{temp},temp=Table[0.,{k,n},{l,n}];
temp[[i,j]]=KroneckerDelta[i,j]+Sqrt[Sign[j-i]];
temp[[j,i]]=KroneckerDelta[i,j]+Sqrt[Sign[j-i]]Sign[j-i];
temp],{i,n},{j,n}],1]
evolutionOperator[parameters_?VectorQ]:=With[{basis=Normal[GellMann[Round[Sqrt[Length[parameters]+1]]]]},Chop[MatrixExp[-I(parameters . basis)]]]
evolutionOperator[parameters_?VectorQ,basis_List]:=Chop[MatrixExp[-I(parameters . basis)]]
lossFunction[param_?VectorQ,initialstate_?MatrixQ,targetstate_?MatrixQ,p_?((0.<#<1.)&)]:=Norm[targetstate-coarseGraining2[evolutionOperator[param] . initialstate . ConjugateTranspose[evolutionOperator[param]],p],"Frobenius"]
lossFunction[param_?VectorQ,initialstate_?MatrixQ,targetstate_?MatrixQ,p_?((0.<#<1.)&),basis_]:=Norm[targetstate-coarseGraining2[evolutionOperator[param,basis] . initialstate . ConjugateTranspose[evolutionOperator[param,basis]],p],"Frobenius"]
nGrad[param_?VectorQ,initialstate_?MatrixQ,targetstate_?MatrixQ,p_?((0.<#<1.)&),h_:0.01]:=Table[(lossFunction[ReplacePart[param,i->param[[i]]+h],initialstate,targetstate,p]-lossFunction[ReplacePart[param,i->param[[i]]-h],initialstate,targetstate,p])/(2*h),{i,Length[param]}]
nGrad[param_?VectorQ,initialstate_?MatrixQ,targetstate_?MatrixQ,p_?((0.<#<1.)&),h_:0.01,basis_]:=Table[(lossFunction[ReplacePart[param,i->param[[i]]+h],initialstate,targetstate,p,basis]-lossFunction[ReplacePart[param,i->param[[i]]-h],initialstate,targetstate,p,basis])/(2*h),{i,Length[param]}]

learningRateWithLineSearch[n_?VectorQ,initialstate_?MatrixQ,targetstate_?MatrixQ,p_?((0.<#<1.)&),h_,grad_,lrinit:(_?((0.<#<=1.)&)):1,\[Tau]_,c_]:=
NestWhile[(# \[Tau])&,lrinit,((lossFunction[n,initialstate,targetstate,p]-lossFunction[n-# grad,initialstate,targetstate,p])<# c*grad . grad)&]

learningRateWithLineSearch[n_?VectorQ,initialstate_?MatrixQ,targetstate_?MatrixQ,p_?((0.<#<1.)&),h_,basis_,grad_,lrinit:(_?((0.<#<=1.)&)):1,\[Tau]_,c_]:=
NestWhile[(# \[Tau])&,lrinit,((lossFunction[n,initialstate,targetstate,p,basis]-lossFunction[n-# grad,initialstate,targetstate,p,basis])<# c*grad . grad)&]

descentStep[n_,initialstate_,targetstate_,p_?((0.<#<1.)&),h_,lrinit_,\[Tau]_,c_]:=
With[{grad=nGrad[n,initialstate,targetstate,p,h]},
n-learningRateWithLineSearch[n,initialstate,targetstate,p,h,grad,lrinit,\[Tau],c]*grad
]

descentStep[n_,initialstate_,targetstate_,p_?((0.<#<1.)&),h_,basis_,lrinit_,\[Tau]_,c_]:=
With[{grad=nGrad[n,initialstate,targetstate,p,h,basis]},
n-learningRateWithLineSearch[n,initialstate,targetstate,p,h,basis,grad,lrinit,\[Tau],c]*grad
]

gradientDescent[initialstate_?MatrixQ,targetstate_?MatrixQ,p_?((0.<#<1.)&),tol_?((0.<#<1.)&),h_:0.01,maxiterations_Integer:200,lrinit:(_?((0.<#<=1.)&)):1,\[Tau]_:0.9,c_:0.5]:=
NestWhile[
descentStep[#,initialstate,targetstate,p,h,lrinit,\[Tau],c]&,
Table[0.,{i,Length[initialstate]^2}],
(Norm[nGrad[#,initialstate,targetstate,p,h]]>tol)&,
1,
maxiterations
]
gradientDescent[initialstate_?MatrixQ,targetstate_?MatrixQ,p_?((0.<#<1.)&),tol_?((0.<#<1.)&),h_:0.01,basis_,maxiterations_Integer:200,lrinit:(_?((0.<#<=1.)&)):1,\[Tau]_:0.9,c_:0.5]:=
NestWhile[
descentStep[#,initialstate,targetstate,p,h,basis,lrinit,\[Tau],c]&,
Table[0.,{i,Length[initialstate]^2}],
(Norm[nGrad[#,initialstate,targetstate,p,h,basis]]>tol)&,
1,
maxiterations
]

gradientDescentList[initialstate_?MatrixQ,targetstate_?MatrixQ,p_?((0.<#<1.)&),tol_?((0.<#<1.)&),h_:0.01,maxiterations_Integer:200,lrinit:(_?((0.<#<=1.)&)):1,\[Tau]_:0.9,c_:0.5]:=
NestWhileList[
descentStep[#,initialstate,targetstate,p,h,lrinit,\[Tau],c]&,
Table[0.,{i,Length[initialstate]^2}],
(Norm[nGrad[#,initialstate,targetstate,p,h]]>tol)&,
1,
maxiterations
]
gradientDescentList[initialstate_?MatrixQ,targetstate_?MatrixQ,p_?((0.<#<1.)&),tol_?((0.<#<1.)&),h_:0.01,basis_,maxiterations_Integer:200,lrinit:(_?((0.<#<=1.)&)):1,\[Tau]_:0.9,c_:0.5]:=
NestWhileList[
descentStep[#,initialstate,targetstate,p,h,basis,lrinit,\[Tau],c]&,
Table[0.,{i,Length[initialstate]^2}],
(Norm[nGrad[#,initialstate,targetstate,p,h,basis]]>tol)&,
1,
maxiterations
]

gradientDescentMinLoss[initialstate_?MatrixQ,targetstate_?MatrixQ,p_?((0.<#<1.)&),tol_?((0.<#<1.)&),h_:0.01,maxiterations_Integer:200,lrinit:(_?((0.<#<=1.)&)):1,\[Tau]_:0.9,c_:0.5]:=
NestWhile[
descentStep[#,initialstate,targetstate,p,h,lrinit,\[Tau],c]&,
Table[0.,{i,Length[initialstate]^2}],
(lossFunction[#,initialstate,targetstate,p]>tol)&,
1,
maxiterations
]
gradientDescentMinLoss[initialstate_?MatrixQ,targetstate_?MatrixQ,p_?((0.<#<1.)&),tol_?((0.<#<1.)&),h_:0.01,basis_,maxiterations_Integer:200,lrinit:(_?((0.<#<=1.)&)):1,\[Tau]_:0.9,c_:0.5]:=
NestWhile[
descentStep[#,initialstate,targetstate,p,h,basis,lrinit,\[Tau],c]&,
Table[0.,{i,Length[initialstate]^2}],
(lossFunction[#,initialstate,targetstate,p,basis]>tol)&,
1,
maxiterations
]

gradientDescentMinLossList[initialstate_?MatrixQ,targetstate_?MatrixQ,p_?((0.<#<1.)&),tol_?((0.<#<1.)&),h_:0.01,maxiterations_Integer:200,lrinit:(_?((0.<#<=1.)&)):1,\[Tau]_:0.9,c_:0.5]:=
NestWhileList[
descentStep[#,initialstate,targetstate,p,h,lrinit,\[Tau],c]&,
Table[0.,{i,Length[initialstate]^2}],
(lossFunction[#,initialstate,targetstate,p]>tol)&,
1,
maxiterations
]
gradientDescentMinLossList[initialstate_?MatrixQ,targetstate_?MatrixQ,p_?((0.<#<1.)&),tol_?((0.<#<1.)&),h_:0.01,basis_,maxiterations_Integer:200,lrinit:(_?((0.<#<=1.)&)):1,\[Tau]_:0.9,c_:0.5]:=
NestWhileList[
descentStep[#,initialstate,targetstate,p,h,basis,lrinit,\[Tau],c]&,
Table[0.,{i,Length[initialstate]^2}],
(lossFunction[#,initialstate,targetstate,p,basis]>tol)&,
1,
maxiterations
]
partialTrace23TripartiteSystem[matrix_?MatrixQ]:={{matrix[[1,1]]+matrix[[2,2]]+matrix[[3,3]]+matrix[[4,4]],matrix[[1,5]]+matrix[[2,6]]+matrix[[3,7]]+matrix[[4,8]]},
{matrix[[5,1]]+matrix[[6,2]]+matrix[[7,3]]+matrix[[8,4]],matrix[[5,5]]+matrix[[6,6]]+matrix[[7,7]]+matrix[[8,8]]}}
zCoordIntersetion\[Rho]a[p_,rz_]:=(rz^2+2p-1)/(2p rz);
volBipartiteSubSpaceTarget[p_,rz_]:=If[rz<=1-2p,(8 \[Pi]^2)/((1-p)(1-2 p)),(4 \[Pi]^2/(1-p)) ((1-rz)/(p rz))]
element03AverageState[p_,rz_]:=If[rz<=1-2p,(((3-2(3-p)p)rz)/(3(1-p(3-2p)))),((2(2-p)rz^2+(1-2p)rz+(1-2p))/(6(1-p)rz))]
element30AverageState[p_,rz_]:=If[rz<=1-2p,-(2p rz)/(3(1-2p)),((2(1+p)rz^2-(1-2p)rz-(1-2p))/(6p rz))]
element33AverageState[p_,rz_]:=If[rz<=1-2p,-(p)/(3(1-p)),((3rz^3-(1-2p)^2 rz^2-(1-2p)^2 rz-(1-2p)^2)/(12(1-p)p rz^2))]
element22AverageState[p_,rz_]:=If[rz<=1-2p,-(p)/(3(1-p)),((3rz^3-(2(1-2p)^2+3)rz^2+(1-2p)^2 rz+(1-2p)^2)/(24(1-p)p rz^2))]
element11AverageState[p_,rz_]:=element22AverageState[p,rz]
\[Rho]averageTwoQubits[p_Real?((0<=#<=(1/2))&),rz_Real?((0<=#<=1)&)]:={{1,0,0,element03AverageState[p,rz]},{0,element11AverageState[p,rz],0,0},
{0,0,element22AverageState[p,rz],0},{element30AverageState[p,rz],0,0,element33AverageState[p,rz]}};
\[Rho]averageTwoQubitsbasiscomp[p_Real?((0<=#<=(1/2))&),rz_Real?((0<=#<=1)&)]:=(1/4)(IdentityMatrix[4]+element11AverageState[p,rz]*KroneckerProduct[PauliMatrix[1],PauliMatrix[1]]
+element22AverageState[p,rz]*KroneckerProduct[PauliMatrix[2],PauliMatrix[2]]
+element33AverageState[p,rz]*KroneckerProduct[PauliMatrix[3],PauliMatrix[3]]
+element03AverageState[p,rz]*KroneckerProduct[PauliMatrix[0],PauliMatrix[3]]
+element30AverageState[p,rz]*KroneckerProduct[PauliMatrix[3],PauliMatrix[0]]);
\[Rho]estrella[\[Rho]_]:={ (* {{{*)
{1,
1/6 (-1+Sqrt[2])\[Rho][[1,2]]\[Rho][[1,4]]+\[Rho][[2,1]]\[Rho][[2,2]]+2(\[Rho][[3,1]])^2-2\[Rho][[3,1]]\[Rho][[3,2]]+3\[Rho][[3,1]]\[Rho][[3,4]]-\[Rho][[3,2]]\[Rho][[3,4]]
+(\[Rho][[3,4]])^2+1/6 (-1+Sqrt[2])\[Rho][[1,2]]\[Rho][[4,1]]+1/6 (-1+Sqrt[2])\[Rho][[1,4]]\[Rho][[4,2]]+1/6 (5+Sqrt[2])\[Rho][[4,1]]\[Rho][[4,2]]
-1/6 (-1+Sqrt[2]) \[Rho][[1,2]]\[Rho][[4,4]]-1/6 (-1+Sqrt[2]) \[Rho][[4,2]]\[Rho][[4,4]],
2\[Rho][[3,3]]\[Rho][[3,1]]+\[Rho][[3,3]]\[Rho][[3,4]]-\[Rho][[4,1]]\[Rho][[4,3]]-\[Rho][[2,1]]\[Rho][[2,3]],
\[Rho][[2,1]]\[Rho][[2,4]]+\[Rho][[4,1]]\[Rho][[4,4]]-(\[Rho][[3,4]])^2-2\[Rho][[3,1]]\[Rho][[3,4]]},
{(1/4)\[Rho][[2,2]]\[Rho][[2,1]]-(1/4)\[Rho][[3,3]]\[Rho][[2,1]]+\[Rho][[1,2]]\[Rho][[2,2]]-\[Rho][[1,3]]\[Rho][[2,3]]
+\[Rho][[1,4]]\[Rho][[2,4]]-(1/4)\[Rho][[2,2]]\[Rho][[2,4]]+(1/4)\[Rho][[2,4]]\[Rho][[3,3]],
\[Rho][[1,2]]\[Rho][[2,1]]+\[Rho][[3,4]]\[Rho][[4,3]]-\[Rho][[3,3]]\[Rho][[4,4]],
\[Rho][[3,2]]\[Rho][[4,4]]-\[Rho][[1,3]]\[Rho][[2,1]]-\[Rho][[3,4]]\[Rho][[4,2]]-\[Rho][[3,1]]\[Rho][[4,4]]-\[Rho][[3,4]]\[Rho][[4,4]],
(1/4)\[Rho][[3,3]]\[Rho][[2,1]]+(1/4)\[Rho][[2,2]]\[Rho][[2,4]]-\[Rho][[1,4]]\[Rho][[2,1]]
-(1/4)\[Rho][[2,2]]\[Rho][[2,1]]-(1/4)\[Rho][[2,4]]\[Rho][[3,3]]-\[Rho][[3,3]]\[Rho][[4,2]]
-\[Rho][[3,1]]\[Rho][[4,3]]+\[Rho][[3,2]]\[Rho][[4,3]]-\[Rho][[3,4]]\[Rho][[4,3]]},
{(1/2)\[Rho][[1,2]]\[Rho][[3,1]]+\[Rho][[1,4]]\[Rho][[3,1]]+(1/2)\[Rho][[1,3]]\[Rho][[3,3]]
-(1/2)\[Rho][[1,2]]\[Rho][[3,2]]+(1/2)\[Rho][[1,2]]\[Rho][[3,4]]+(1/2)\[Rho][[2,3]]\[Rho][[4,2]]-(1/2)\[Rho][[2,2]]\[Rho][[4,3]],
\[Rho][[1,3]]\[Rho][[3,3]]-(3/2)\[Rho][[1,2]]\[Rho][[3,1]]-2\[Rho][[1,4]]\[Rho][[3,1]]
-\[Rho][[1,4]]\[Rho][[3,4]]-\[Rho][[2,3]]\[Rho][[4,2]]+\[Rho][[2,2]]\[Rho][[4,3]]-2\[Rho][[2,4]]\[Rho][[4,3]]+2\[Rho][[2,3]]\[Rho][[4,4]],
2\[Rho][[1,3]]\[Rho][[3,1]]+\[Rho][[1,3]]\[Rho][[3,4]]+\[Rho][[2,4]]\[Rho][[4,2]]-\[Rho][[2,2]]\[Rho][[4,4]],
\[Rho][[2,2]]\[Rho][[4,3]]-2\[Rho][[1,4]]\[Rho][[3,1]]-\[Rho][[1,4]]\[Rho][[3,4]]-\[Rho][[2,3]]\[Rho][[4,2]]},
{\[Rho][[1,2]]\[Rho][[4,2]]-\[Rho][[1,3]]\[Rho][[4,3]]+\[Rho][[1,4]]\[Rho][[4,4]],
1/6 (-1+Sqrt[2])\[Rho][[1,2]]\[Rho][[1,3]]+1/6 (-1+Sqrt[2])\[Rho][[4,2]]\[Rho][[1,4]]+\[Rho][[2,4]]\[Rho][[3,3]]-\[Rho][[2,3]]\[Rho][[3,4]]+1/6 (5+Sqrt[2])\[Rho][[1,2]]\[Rho][[4,1]]+1/6 (-1+Sqrt[2])\[Rho][[4,2]]\[Rho][[4,1]]
-1/6 (-1+Sqrt[2])\[Rho][[1,2]]\[Rho][[4,4]]-1/6 (-1+Sqrt[2])\[Rho][[4,2]]\[Rho][[4,4]],
\[Rho][[2,2]]\[Rho][[3,4]]+\[Rho][[2,4]]\[Rho][[3,1]]-\[Rho][[2,4]]\[Rho][[3,2]]+\[Rho][[2,3]]\[Rho][[3,4]]-\[Rho][[1,3]]\[Rho][[4,1]],
\[Rho][[2,3]]\[Rho][[3,2]]-\[Rho][[2,2]]\[Rho][[3,3]]-\[Rho][[2,3]]\[Rho][[3,1]]-\[Rho][[2,3]]\[Rho][[3,4]]+\[Rho][[1,4]]\[Rho][[4,1]]}};
(* }}}*)
visualizeBipartiteSystemWObj[densityoperators_,objective_:IdentityMatrix[2]/2,range_:{{-1.,1.},{-1.,1.},{-1.,1.}}]:=Print["We know where the target is. This fuction is obsolete, please use visualizeBipartiteSystem instead."]

visualizeBipartiteSystem[densityoperators_, colors_:{Red, Green}, range_:{{-1.,1.},{-1.,1.},{-1.,1.}}]:=
With[{points=Map[densityMatrixToPoint[{partialTraceA[#],partialTraceB[#]},gellMannBasis[1]]&,densityoperators]},
	 Show[ListPointPlot3D[Transpose[points], BoxRatios->1, PlotStyle->{colors[[1]],colors[[2]],PointSize[0.01]},
	                      PlotRange->range, AxesLabel->MaTeX[{"x","y","z"}, Preamble->{"\\usepackage{newtxmath}"}], AxesStyle->Black],
		  Graphics3D[{ColorData[1][1],Thickness[0.001],Line/@points}],
          Graphics3D[{Opacity[0.2],GrayLevel[0.9],Sphere[]},BoxRatios->1,Axes->True, AxesStyle->20,TicksStyle->10]]
];

visualizeBipartiteSystemTwoSpheres[densityoperators_,rz_,p_,range_:{{-1.,1.},{-1.,1.},{-1.,1.}}]:=
With[
{points=Map[densityMatrixToPoint[{partialTraceA[#],partialTraceB[#]},gellMannBasis[1]]&,densityoperators]},
Show[
ListPointPlot3D[Transpose[points],BoxRatios->1,PlotStyle->{Red,Green,PointSize[0.01]},PlotRange->range],
Graphics3D[{ColorData[1][1],Thickness[0.001],Line/@points}],
Graphics3D[{Opacity[0.2`],GrayLevel[0.9`],Sphere[-{0,0,rz}*p/(1-2p),Norm[{0,0,rz}](1-p)/(1-2p)]},BoxRatios->1,Axes->True,AxesStyle->20,TicksStyle->10],
Graphics3D[{Opacity[0.2`],GrayLevel[0.9`],Sphere[{0,0,rz}*(1-p)/(1-2p),Norm[{0,0,rz}](p/(1-2p))]},BoxRatios->1,Axes->True,AxesStyle->20,TicksStyle->10]
]]

generatePureHaarStates[n_,batch_,targetstate_,eps_,p_]:=Module[{data={},totalstates=0,states},
While[totalstates < n,
states = selectNearAfterCG[ketsToDensity[randomKets[4,batch]],targetstate,p,eps];
(*Print[Length[data+1],"th batch. Number of states accepted: ",Length[states]];*)
AppendTo[data,states];
totalstates = totalstates + Length[states]];
Flatten[data,1]]

aTan2[y_Real?Positive,x_Real?Positive]:=ArcTan[y/x];
aTan2[y_Real?Positive,x_Real?Negative]:=ArcTan[y/x]+Pi;
aTan2[y_Real?Negative,x_Real?Negative]:=ArcTan[y/x]+Pi;
aTan2[y_Real?Negative,x_Real?Positive]:=ArcTan[y/x]+ 2 Pi;
aTan2[y_Real?Positive,x_Real?((#==0)&)]:=Pi/2;
aTan2[y_Real?Negative,x_Real?((#==0)&)]:=3 Pi/2;
aTan2[y_Real?((#==0)&),x_Real?Negative]:= Pi;
aTan2[y_Real?((#==0)&),x_Real?Positive]:= 0.;
aTan2[y_Real?((#==0)&),x_Real?((#==0)&)]:=0.;
getAzimuthal[{x_,y_,z_}]:=aTan2[y,x];
MapMonitored[f_, args_List] := Module[{x = 0}, Monitor[MapIndexed[(x = #2[[1]]; f[#1]) &, args], ProgressIndicator[x/Length[args]]]] 

middlePoints[values_]:=Table[(values[[i]]+values[[i+1]])/2,{i,Length[values]-1}];
(*Returns a list with the Bloch vectors corresponding to both partial traces of a given density matrix*)
partialtarcesBV[rho_?MatrixQ]:=densityMatrixToPoint[{partialTraceA[rho],partialTraceB[rho]},gellMannBasis[1]];
(*Calculates a new orthonormal basis given a certain direction*)
basisFromDirection[dir:{z1_,z2_,z3_}]:=Normalize/@{Cross[{0,z3,-z2}, dir],{0,z3,-z2},dir};
(*Transforms the coordinates of a given set of vectors to a new reference system with a new direction and a new center*)
cartesianZChange[veclist_,newdir_:{0.,0.,1.},newcenter_:{0.,0.,0.}]:=Map[(Inverse[Transpose[basisFromDirection[If[newdir=={0.,0.,0.},{0.,0.,1.},newdir]]]] . #)&,(#-newcenter)&/@veclist]
(*Returns the cylindrical coordinates of a given point*)
cylindricalCoordinates[vec:{x1_,x2_,x3_}]:={Norm[{x1,x2}],aTan2[x2,x1],x3}
(*Analogous to ToSphericalCoordinates, but you can also define a new center or a new Z direction*)
toCylindricalCoordinates[veclist_,newdir_:{0.,0.,1.},newcenter_:{0.,0.,0.}]:=Map[cylindricalCoordinates[#]&,cartesianZChange[veclist,newdir,newcenter]]

(*These functions do the same as the last three functions, but using cylindrical coordinates*)
radialCylindricalDensity[list_,{a_,b_,c_}]:=With[{data=HistogramList[list[[1]],{a,b,c}],voldiff=(2*Pi*(Max[list[[3]]]-Min[list[[3]]])*(#1^2-#2^2)/2)&},
					{middlePoints[data[[1]]],Normalize[Table[data[[2,i]]/voldiff[data[[1,i+1]],data[[1,i]]],{i,1,Length[data[[2]]]}]]}
					];
					
azimuthalCylindricalDensity[list_,{a_,b_,c_}]:=With[{data=HistogramList[list[[2]],{a,b,c}],voldiff=((Max[list[[3]]]-Min[list[[3]]])*(Max[list[[1]]]^2-Min[list[[1]]]^2)/2)*(#1-#2)&},
					{middlePoints[data[[1]]],Normalize[Table[data[[2,i]]/voldiff[data[[1,i+1]],data[[1,i]]],{i,1,Length[data[[2]]]}]]}
					];
					
zCylindricalDensity[list_,{a_,b_,c_}]:=With[{data=HistogramList[list[[3]],{a,b,c}],voldiff=(2*Pi*(#1-#2)*(Max[list[[1]]]^2-Min[list[[1]]]^2)/2)&},
					{middlePoints[data[[1]]],Normalize[Table[data[[2,i]]/voldiff[data[[1,i+1]],data[[1,i]]],{i,1,Length[data[[2]]]}]]}
					];

tripleCylindricalPlot[finalstates_List,targetstate_,rbins_Integer,tbins_Integer,zbins_Integer,colors_List:{Red,Green}, XrangeR_:Full,XrangeT_:Full,XrangeZ_:Full, Yrange_:{0.,1.}]:=
Module[{
finalpartialtracesCylindrical=With[{finalpartialtraces=Transpose[Map[partialtarcesBV[#]&,finalstates]],
								    newcenter=densityMatrixToPoint[{targetstate},gellMannBasis[1]][[1]]},
								    {toCylindricalCoordinates[finalpartialtraces[[1]],newcenter,newcenter]//Transpose,toCylindricalCoordinates[finalpartialtraces[[2]],newcenter,newcenter]//Transpose}],
rplot,
tplot,
zplot},
rplot=ListPlot[With[{joined=Join[finalpartialtracesCylindrical[[1,1]],finalpartialtracesCylindrical[[2,1]]]},Map[Transpose[radialCylindricalDensity[#,{Min[joined]-(Max[joined]-Min[joined])/rbins,Max[joined]+(Max[joined]-Min[joined])/rbins,(Max[joined]-Min[joined])/rbins}]]&,finalpartialtracesCylindrical]],
		PlotRange->{XrangeR,Yrange}, Joined -> True, InterpolationOrder -> 0, Filling->None, 
		Frame -> True, Axes -> False, FrameLabel->MaTeX[{"r","\\text{Densidad normalizada}"}], FrameStyle->Black,
		PlotStyle->colors, PlotTheme->"Scientific", GridLines->Automatic];

tplot=ListPlot[With[{joined=Join[finalpartialtracesCylindrical[[1,2]],finalpartialtracesCylindrical[[2,2]]]},Map[Transpose[azimuthalCylindricalDensity[#,{Min[joined]-(Max[joined]-Min[joined])/tbins,Max[joined]+(Max[joined]-Min[joined])/tbins,(Max[joined]-Min[joined])/tbins}]]&,finalpartialtracesCylindrical]],
		PlotRange->{XrangeT,Yrange}, Joined -> True, InterpolationOrder -> 0, Filling->None,
		Frame -> True, Axes -> False, FrameLabel->MaTeX[{"\\theta","\\text{Densidad normalizada}"}], FrameStyle->Black,
		PlotStyle->colors, PlotTheme->"Scientific", GridLines->Automatic];

zplot=ListPlot[With[{joined=Join[finalpartialtracesCylindrical[[1,3]],finalpartialtracesCylindrical[[2,3]]]},Map[Transpose[zCylindricalDensity[#,{Min[joined]-(Max[joined]-Min[joined])/zbins,Max[joined]+(Max[joined]-Min[joined])/zbins,(Max[joined]-Min[joined])/zbins}]]&,finalpartialtracesCylindrical]],
		PlotRange->{XrangeZ,Yrange}, Joined -> True, InterpolationOrder -> 0, Filling->None,
		Frame -> True, Axes -> False, FrameLabel->MaTeX[{"z","\\text{Densidad normalizada}"}], FrameStyle->Black,
		PlotStyle->colors, PlotTheme->"Scientific", GridLines->Automatic];
{rplot,tplot,zplot}
];

tripleCylindricalPlot2[sample_, rz_, colors_:{Red,Green}, Xranges_:{Full, Full, Full}, bins_:{Automatic,Automatic,Automatic}]:=
Module[{cylindricalBVs = toCylindricalCoordinates[#, {0,0,1}, {0,0,rz}]& /@ Transpose[partialtarcesBV[#]&/@sample],
		axesLabels = MaTeX[{"\\text{Densidad normalizada}", "r", "\\theta", "z"}, Preamble->{"\\usepackage{newtxmath}"}], 
		data},
	   data = Outer[Transpose[#2][[#1]]&, Range[3], cylindricalBVs, 1];
	   MapThread[Histogram[#1, #2, "Probability", PlotRange->{#3, Automatic}, 
	                       ChartStyle->colors, PlotTheme->"Scientific", FrameLabel->{#4, axesLabels[[1]]}, FrameStyle->Black, GridLines->Automatic]&,
	             {data, bins, Xranges, Rest[axesLabels]}]
];

noFillingHistogram[hlist_, axesLabels_, color_:RGBColor[1, 0.8, 0.5]]:=
Graphics[{FaceForm[None], EdgeForm[{color, Thickness[0.005]}],
           Polygon[Transpose[{Riffle[#,#]&@hlist[[1]], ArrayPad[Riffle[#,#]&@hlist[[2]],1]}]]},
         AspectRatio -> 1/GoldenRatio, Frame -> True,
         PlotRangePadding -> {{Scaled[0.02], Scaled[0.02]}, {0, Scaled[0.05]}}, 
         FrameTicksStyle->Directive[FontFamily->"Times"], FrameLabel->{axesLabels[[1]], axesLabels[[2]]}, FrameStyle->Black, 
         GridLines->Automatic, GridLinesStyle->Directive[Gray, Dotted]
];

tripleCylindricalPlotNoFilling[sample_, rz_, colors_:{Red,Green}, bins_:{Automatic, Automatic, Automatic}]:=
Module[{cylindricalBVs = toCylindricalCoordinates[#, {0,0,1}, {0,0,rz}]& /@ Transpose[partialtarcesBV[#]&/@sample],
		axesLabels = MaTeX[{"\\text{Fracci\[OAcute]n de estados}", "r", "\\theta", "z"}, Preamble->{"\\usepackage{newtxmath}"}], 
		data, hlists, auxFunction},
		auxFunction[bspec_, datapair_]:= (HistogramList[#, bspec, "Probability"]& /@ datapair);
		data = Outer[Transpose[#2][[#1]]&, Range[3], cylindricalBVs, 1];
		hlists = MapThread[auxFunction[#1, #2]&, {bins, data}];
		MapThread[Show[noFillingHistogram[#1[[1]], {#2, axesLabels[[1]]}, colors[[1]]], noFillingHistogram[#1[[2]], {#2, axesLabels[[1]]}, colors[[2]]]]&, 
				  {hlists, Rest[axesLabels]}]
];

RadiusAsFunctionOfLagrange[l_,p_]:=Sum[p[[k]]*Tanh[p[[k]]*l],{k,1,Length[p]}];

LagrangeMulFromRadius[radius_,pvec_]:=l/.FindRoot[RadiusAsFunctionOfLagrange[l,pvec]==radius,{l,0}];

MaxEntSubStates[rho_,p_]:=With[
	{vec=densityMatrixToPoint[{rho},NQubitBasis[1]][[1]]},
	lagmult=LagrangeMulFromRadius[Norm[vec],p];
	Table[(IdentityMatrix[2]+Tanh[p[[k]]*lagmult]*Normalize[vec] . Rest[NQubitBasis[1]])/2,{k,1,Length[p]}]]

MaxEntN[rho_?(Tr[(# . #)] != 1&),p_]:=Fold[KroneckerProduct,MaxEntSubStates[rho,p]];
MaxEntN[rho_?(Tr[(# . #)] == 1&),p_]:=Nest[KroneckerProduct[rho,#]&,rho,Length[p]-1];
CGNto1[rho_,p_]:=Sum[p[[k]]*PartialTrace[rho,2^(Length[p]-k)],{k,1,Length[p]}];



End[]
EndPackage[]
