#!/usr/bin/env wolframscript
(* ::Package:: *)

Get["/home/enavarrete/libs/CoolTools.m"]
Get["/home/enavarrete/libs/usefulFunctions.wl"]


SetDirectory["/home/enavarrete/mh_samples_GOOD/distintas_Nbetasydeltas_Rp5Pp3"]


brutalRefRp5Pp3 = ketsToDensity[Get["/home/enavarrete/libs/pureHaarStates_n=10000_p=0.3_rz=0.5.m"][[1;;100]]];


(*parametros necesarios*)
deltas = {0.001, 0.005, 0.01, 0.03, 0.05};
betas = {100, 250, 400, 600, 750, 1000};
enes = {10000, 20000, 40000, 60000, 80000, 100000, 200000, 350000, 500000};


minVecsForMHSamples[enes_List, brutalRef_List, rz_, swapP_, \[Beta]_, \[Delta]_]:= 
Module[{minvecs = {}}, 
	   minvecs = Map[minDistancesVector[brutalRef, Get["samples/MHsample_N="<>ToString[#]<>"_delta="<>ToString[\[Delta]]<>"_beta="<>ToString[\[Beta]]<>"_rz="<>ToString[rz]<>"_p="<>ToString[swapP]<>".wl"][[2,1]]]&
		             , enes];
	   Export["minVecs_beta="<>ToString[\[Beta]]<>"_delta="<>ToString[\[Delta]]<>"_rz="<>ToString[rz]<>"_p="<>ToString[swapP]<>"_allN.wl", minvecs];
	   Print["Guardados los minvecs para: toda N y beta="<>ToString[\[Beta]]<>" delta="<>ToString[\[Delta]]<>" rz="<>ToString[rz]<>" p="<>ToString[swapP]]
]


ParallelTable[minVecsForMHSamples[enes, brutalRefRp5Pp3, 0.5, 0.3, i, j], {i, betas}, {j, deltas}, DistributedContexts->All]
