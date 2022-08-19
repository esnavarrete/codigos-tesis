#!/usr/bin/env wolframscript
(* ::Package:: *)

Get["CoolTools.m", Path->"."]
Get["usefulFunctions.wl", Path->"."]


SetDirectory["/home/enavarrete/mh_samples"]


(*parametros necesarios*)
deltas = {0.01, 0.02, 0.03, 0.04, 0.05};
betas = {100, 250, 400, 500, 1000};
enes = {10000, 20000, 40000, 60000, 80000, 100000, 200000,300000,400000,500000};


(*Devuelve la lista de edos promedio para todas las 'enes' correspondientes a cierta beta, delta, rz y swapP:*)
importSampleAndExportMeans[enes_, \[Beta]_, \[Delta]_, rz_, swapP_]:= Module[{avgs={}},
		avgs = Map[preimageMean[Get["MHsample_N="<>ToString[#]<>"_delta="<>ToString[\[Delta]]<>"_beta="<>ToString[\[Beta]]<>"_rz="<>ToString[rz]<>"_p="<>ToString[swapP]<>".wl"][[2]]]&,enes];
		Export["./averages/avgs_delta="<>ToString[\[Delta]]<>"_beta="<>ToString[\[Beta]]<>"_rz=0.5_p=0.3_allN.wl", avgs];
	    Print["edos promedio con: delta="<>ToString[\[Delta]]<>" y beta="<>ToString[\[Beta]]<>" exportados."]
];


Outer[importSampleAndExportMeans[enes, #1, #2, 0.5, 0.3]&, betas, deltas]
