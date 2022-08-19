#!/usr/bin/env wolframscript
(* ::Package:: *)

Get["CoolTools.m", Path->"."]
Get["usefulFunctions.wl", Path->"."]


brutalRefRp5Pp3 = Get["pureHaarStates_n=10000_p=0.3_rz=0.5.m", Path->"."][[1;;100]];
brutalRefRp5Pp3 = ketsToDensity[brutalRefRp5Pp3];


SetDirectory["/home/enavarrete/mh_samples"]


(*Matriz de distancias. (V\[EAcute]ase el cuaderno)*)
distancesMatrix[brutalRef_, sample_]:= Outer[Norm[#1 - #2, "Frobenius"]&, brutalRef, sample, 1]
(*M\[IAcute]nimos de cada rengl\[OAcute]n de la matriz de distancias:*)
minDistancesVector[brutalRef_, sample_]:= Min[#]& /@ distancesMatrix[brutalRef, sample];


minVecsForMHSample[enes_List, brutalRef_List, rz_, swapP_, \[Beta]_, \[Delta]_]:= 
				Map[minDistancesVector[brutalRef, 
					Get["MHsample_N="<>ToString[#]<>"_delta="<>ToString[\[Delta]]<>"_beta="<>ToString[\[Beta]]<>"_rz="<>ToString[rz]<>"_p="<>ToString[swapP]<>".wl"][[2]]]&
							,enes]


(*parametros necesarios*)
deltas = {0.01, 0.02, 0.03, 0.04, 0.05};
betas = {100, 250, 400, 500, 1000};
enes = {10000, 20000, 40000, 60000, 80000, 100000, 200000,300000,400000,500000};


minVecsRp5Pp3\[Beta]400 = Map[minVecsForMHSample[enes, brutalRefRp5Pp3, 0.5, 0.3, 400, #]&, deltas];
Print["MinVectors for beta=400 have been calculated."]


minVecsRp5Pp3\[Beta]500 = Map[minVecsForMHSample[enes, brutalRefRp5Pp3, 0.5, 0.3, 500, #]&, deltas];
Print["MinVectors for beta=500 have been calculated."]


minVecsRp5Pp3\[Beta]1000 = Append[(Map[minVecsForMHSample[enes, brutalRefRp5Pp3, 0.5, 0.3, 1000, #]&, Drop[deltas,-1]]), 
							minVecsForMHSample[Drop[enes, -1], brutalRefRp5Pp3, 0.5, 0.3, 1000, 0.05]];
Print["MinVectors for beta=1000 have been calculated."]


SetDirectory["/home/enavarrete/mh_samples/ergs_mh"]


Export["minVecsRp5Pp3_beta400.wl", minVecsRp5Pp3\[Beta]400]
Export["minVecsRp5Pp3_beta500.wl", minVecsRp5Pp3\[Beta]500]
Export["minVecsRp5Pp3_beta1000.wl", minVecsRp5Pp3\[Beta]1000]
