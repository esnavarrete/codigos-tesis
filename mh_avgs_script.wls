#!/usr/bin/env wolframscript
(* ::Package:: *)

Get["/home/enavarrete/libs/CoolTools.m"]
Get["/home/enavarrete/libs/usefulFunctions.wl"]


SetDirectory["/home/enavarrete/mh_samples_GOOD/distintas_Nbetasydeltas_Rp5Pp3"]


(*parametros necesarios*)
deltas = {0.001, 0.005, 0.01, 0.03, 0.05};
betas = {100, 250, 400, 600, 750, 1000};
enes = {10000, 20000, 40000, 60000, 80000, 100000, 200000,350000, 500000};


(*Devuelve la lista de edos promedio para todas las 'enes' correspondientes a cierta beta, delta, rz y swapP:*)
importSampleAndExportMeans[enes_, \[Beta]_, \[Delta]_, rz_, swapP_]:= Module[{avgs={}},
		avgs = Map[preimageMean[Get["samples/MHsample_N="<>ToString[#]<>"_delta="<>ToString[\[Delta]]<>"_beta="<>ToString[\[Beta]]<>"_rz="<>ToString[rz]<>"_p="<>ToString[swapP]<>".wl"][[2,1]]]&,enes];
		Export["avgs_delta="<>ToString[\[Delta]]<>"_beta="<>ToString[\[Beta]]<>"_rz=0.5_p=0.3_allN.wl", avgs];
	    Print["edos promedio con: delta="<>ToString[\[Delta]]<>" y beta="<>ToString[\[Beta]]<>" exportados."]
];


ParallelTable[importSampleAndExportMeans[enes, i, j, 0.5, 0.3], {i, betas}, {j, deltas}, DistributedContexts->All]
(*Outer[importSampleAndExportMeans[enes, #1, #2, 0.5, 0.3]&, betas, deltas]*)
