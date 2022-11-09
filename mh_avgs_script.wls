#!/usr/bin/env wolframscript
(* ::Package:: *)

Get["/home/enavarrete/libs/CoolTools.m"]
Get["/home/enavarrete/libs/usefulFunctions.wl"]


SetDirectory["/home/enavarrete/mh_samples_GOOD/final_samples_3qubits"]


(*parametros necesarios*)
(*deltas = {0.001, 0.005, 0.01, 0.03, 0.05};
betas = {100, 250, 400, 600, 750, 1000};
enes = {10000, 20000, 40000, 60000, 80000, 100000, 200000,350000, 500000};*)
delta = 0.05;
beta = 100;
ene = 200000;
rzs = {0, 0.1, 0.5, 0.8};
pes = {"boltzmann", "pref_0.5", "pref_0.9"};


(*Devuelve la lista de edos promedio para todas las 'enes' correspondientes a cierta beta, delta, rz y swapP:*)
importSampleAndExportMeans[enes_, \[Beta]_, \[Delta]_, rz_, swapP_]:= Module[{avgs={}},
		avgs = Map[preimageMean[Get["samples/MHsample_N="<>ToString[#]<>"_delta="<>ToString[\[Delta]]<>"_beta="<>ToString[\[Beta]]<>"_rz="<>ToString[rz]<>"_p="<>ToString[swapP]<>".wl"][[2,1]]]&,enes];
		Export["avgs_delta="<>ToString[\[Delta]]<>"_beta="<>ToString[\[Beta]]<>"_rz="<>ToString[rz]<>"_p="<>ToString[swapP]<>"_allN.wl", avgs];
	    Print["edos promedio con: delta="<>ToString[\[Delta]]<>", beta="<>ToString[\[Beta]]<>", rz="<>ToString[rz]<>"y p="<>ToString[swapP]<>" exportados."]
];


ParallelTable[importSampleAndExportMeans[{ene}, beta, delta, i, j], {i, rzs}, {j, pes}, DistributedContexts->All]
