(* ::Package:: *)

BeginPackage["Tesis`"]
Needs["CoolTools`"]
preimageMean::usage="Args: states_List. Takes the average state of the list of states. The states have to be density matrices."
numForm::usage="Args: matrix, n. Applies the function NumForm to the entries of matrix, not to matrix itself. n is the number of digits you want."
MatrixToLatex2::usage="Args: matrix. Returns a string with matrix formatted as Latex."
visualizeMonopartiteSystem::usage="Args: states_List, refState_List. Plots states and refState on the Bloch Sphere. All states must be monopartite."
distsToTarget::usage="Args: sample, targetstate, swapP. Calculates the Frobenius distances between 'targetstate' and the image of 'states' under CG with probability 'swapP'."
cleanSample::usage="Args: [sample, rz, swapP, error]. Returns a sample without the states that doesn't fall into the error region."
testDistribution::usage="Args: \[Beta], targetstate, state, swapP. Calculates the value of the probability distribution of our MH implementation corresponding to \[Beta] and the given target state, at the point 'state'."
metropolisHastingsSample::usage="Args: size, \[Beta], \[Delta], swapP, initialstate, targetstate. Runs our implementation of Metropolis-Hastings algorithm with parameters 'size', \[Beta] and \[Delta]; starting from 'initialstate' and trying to reach the region of 'targetstate' with CG swapP."
metropolisHastingsSampleGOOD::usage="Args:[size, \[Beta], \[Delta], swapP, initialstate, targetstate]. Runs out implementation of MH algorithm CORRECTLY."
metropolisHastingsSampleTest::usage="Args: size, initialstate, params. Same as metropolisHastingsSample, just gathers all the parameters \[Beta], \[Delta], swapP and targetstate into a simgle variable."
universalInitState::usage="The 2-qubit state used as initial state for all MH Great Tests so far."
initStateGenerator::usage="Args: \[Beta], \[Delta], swapP, targetstate, error. It runs MH algorithm until the error is less than 'error'. Then it returns the last state found. This state can then be used as initial state for MH."
distancesMatrix::usage="Args: brutalRef, sample. It computes the distances matrix needed to measure ergodicity of 'sample' wrt 'brutalRef' in all our ways."
minDistancesVector::usage="Args: brutalRef, sample. It gets the minimum values of each row of the distances matrix and arranges them in a vector."
ergodicityMeasure2::usage="Args: minVector. Our Second ergodicity measure (formally the first). It calculates the mean of the minVector components."
ergodicityMeasure4::usage="Args: minVector. Our fourth ergodicity measure (formally the second). It calculates the maximum component of the minVector (i.e. its infinity norm)"


Begin["`Private`"]
preimageMean[states_List]:= Apply[Plus, states]/Length[states];
numForm[matrix_, n_]:= Map[NumberForm[#, n]&, matrix, {2}];
MatrixToLatex2[matrix_]:=StringReplace["\\begin{pmatrix}\n"<>Fold[(#1<>"\\"<>"\\"<>#2)&, Map[Fold[(ToString[#1]<>"&"<>ToString[#2])&,#]&, matrix]]<>"\n\\end{pmatrix}", "I"->"i"];
visualizeMonopartiteSystem[states_List, refState_List]:= Show[Graphics3D[{PointSize[0.015], Blue, Point[densityMatrixToPoint[states, gellMannBasis[1]]]}], 
										Graphics3D[{PointSize[0.015], Red,Point[densityMatrixToPoint[refState, gellMannBasis[1]]]}],
										 Graphics3D[{Opacity[0.3], Sphere[]}]];
distsToTarget[sample_, targetstate_, swapP_]:= Map[Norm[coarseGraining2[#, swapP] - targetstate, "Frobenius"]&, sample];	
cleanSample[sample_, rz_, swapP_, error_]:= With[{targetstate = (IdentityMatrix[2] + rz PauliMatrix[3])/2},
												 Select[sample, Norm[coarseGraining2[#, swapP]-targetstate, "Frobenius"] <= error &]];
testDistribution[beta_,targetstate_,state_,swapP_]:= Exp[-beta*Norm[coarseGraining2[state, swapP]-targetstate, "Frobenius"]];
metropolisHastingsSample[size_,\[Beta]_,\[Delta]_,swapP_,initialstate_,targetstate_]:= Module[{n = 0, X = initialstate, Y, U, \[Alpha], statelist = {}},
	While[n < size,
		(*Y = ketsToDensity[randomKets[4,1]][[1]];*)
		U = randomSmallEvolution[4,\[Delta]];
		Y = U . X . ConjugateTranspose[U];
		\[Alpha] = Min[testDistribution[\[Beta],targetstate,Y,swapP]/testDistribution[\[Beta],targetstate,X,swapP],1];
		X = RandomChoice[{\[Alpha], 1 - \[Alpha]}->{Y,X}];
		If[X == Y, AppendTo[statelist,X];n++]
	];
	Return[statelist]
	];
	
metropolisHastingsSampleGOOD[size_,\[Beta]_,\[Delta]_,swapP_,initialstate_,targetstate_]:= Module[{n = 0, X = initialstate, Y, U, \[Alpha], statelist = {}, acceptances = 0},
	While[n < size,
		U = randomSmallEvolution[4,\[Delta]];
		Y = U . X . ConjugateTranspose[U];
		\[Alpha] = Min[testDistribution[\[Beta],targetstate,Y,swapP]/testDistribution[\[Beta],targetstate,X,swapP],1];
		X = RandomChoice[{\[Alpha], 1 - \[Alpha]}->{Y,X}];
		If[X == Y, acceptances++];
		AppendTo[statelist, X];
		n++
	];
	Return[{statelist, acceptances/size}]];	
	
metropolisHastingsSampleTest[size_, initialstate_, params_List]:= Module[{n = 0, X = initialstate, Y, U, \[Alpha], statelist = {}},
	While[n < size,
		(*Y = ketsToDensity[randomKets[4,1]][[1]];*)
		U = randomSmallEvolution[4,params[[2]]];
		Y = U . X . ConjugateTranspose[U];
		\[Alpha] = Min[testDistribution[params[[1]],params[[4]],Y,params[[3]]]/testDistribution[params[[1]],params[[4]],X,params[[3]]],1];
		X = RandomChoice[{\[Alpha], 1 - \[Alpha]}->{Y,X}];
		If[X == Y, AppendTo[statelist,X];n++]
	];
	Return[statelist]
	];
(*getParamsPositions[target_, swapP_]:= With[{targetstate=(IdentityMatrix[2]+target PauliMatrix[3])/2}, Position[allParams,{_,_,swapP,targetstate},1]];*)	
(*sampleErrorStdDev[sample_List, targetstate_, swapP_]:= StandardDeviation @ Map[distToTarget[#, targetstate, swapP]&, sample];	
sampleErrorMean[sample_List, targetstate_, swapP_]:= Mean @ Map[distToTarget[#, targetstate, swapP]&, sample];
sampleErrorMax[sample_List, targetstate_, swapP_]:= Max @ Map[distToTarget[#, targetstate, swapP]&, sample];*)

universalInitState = {{0.410917935196175 + 0.*I, 0.2272038918068731 - 0.20737674270874934*I, 
0.2105845415848582 - 0.2023568912321056*I, 
0.08863081936605553 + 0.23299802818153106*I}, 
{0.2272038918068731 + 0.20737674270874934*I, 0.23028131352676215 + 0.*I, 
0.21855882325494758 - 0.005611672613147364*I, 
-0.06858085917908074 + 0.17355784038938035*I},
{0.2105845415848582 + 0.2023568912321056*I, 
0.21855882325494758 + 0.005611672613147364*I, 0.20756981693414545 + 0.*I, 
-0.06931913586574238 + 0.16305163218798477*I}, 
{0.08863081936605553 - 0.23299802818153106*I, 
-0.06858085917908074 - 0.17355784038938035*I, 
-0.06931913586574238 - 0.16305163218798477*I, 0.15123093434291737 + 0.*I}};

initStateGenerator[\[Beta]_, \[Delta]_, swapP_, targetstate_, error_]:= Module[{X = universalInitState, \[Epsilon] = Norm[coarseGraining2[universalInitState, swapP]-targetstate,"Frobenius"], Y, U, \[Alpha]},
	While[\[Epsilon] > error,
		U = randomSmallEvolution[4,\[Delta]];
		Y = U . X . ConjugateTranspose[U];
		\[Alpha] = Min[testDistribution[\[Beta],targetstate,Y,swapP]/testDistribution[\[Beta],targetstate,X,swapP],1];
		X = RandomChoice[{\[Alpha], 1 - \[Alpha]}->{Y,X}];
		If[X == Y, \[Epsilon] = Norm[coarseGraining2[X, swapP]-targetstate,"Frobenius"]]
	];
	Return[X]
];
distancesMatrix[brutalRef_, sample_]:= Outer[Norm[#1 - #2, "Frobenius"]&, brutalRef, sample, 1];
minDistancesVector[brutalRef_, sample_]:= Min[#]& /@ distancesMatrix[brutalRef, sample];
ergodicityMeasure2[minVector_List]:= Mean[minVector];
ergodicityMeasure4[minVector_List]:= Norm[minVector, Infinity];


End[]
EndPackage[]
