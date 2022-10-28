#!/usr/bin/env wolframscript
(* ::Package:: *)

SetDirectory["/home/enavarrete/mh_samples_GOOD/distintas_Nbetasydeltas_Rp5Pp3_DOS"]


betas = {100, 250, 400, 600, 750, 1000};
deltas = {0.005, 0.01, 0.03, 0.05, 0.07, 0.1};
enes = {10000, 20000, 40000, 60000, 80000, 100000, 200000, 350000, 500000, 1000000};


getAcceptanceRate[ene_, \[Delta]_, \[Beta]_, rz_, swapP_]:= 
	Get["samples/MHsample_N="<>ToString[ene]<>"_delta="<>ToString[\[Delta]]<>"_beta="<>ToString[\[Beta]]<>"_rz="<>ToString[rz]<>"_p="<>ToString[swapP]<>".wl"][[2,2]]


getExecTime[ene_, \[Delta]_, \[Beta]_, rz_, swapP_]:=
	Get["samples/MHsample_N="<>ToString[ene]<>"_delta="<>ToString[\[Delta]]<>"_beta="<>ToString[\[Beta]]<>"_rz="<>ToString[rz]<>"_p="<>ToString[swapP]<>".wl"][[1]]


getAcceptanceRatesFor\[Beta][\[Beta]_, deltas_, enes_, rz_, p_]:=
	Module[{accRates = {}},
			accRates = Outer[getAcceptanceRate[#1, #2, \[Beta], rz, p]&, enes, deltas];
			Export["accRates_beta="<>ToString[\[Beta]]<>"_allDeltas_allN_rz="<>ToString[rz]<>"_p="<>ToString[p]<>".wl", accRates];
			Print["Guardados los acceptance rates para: beta="<>ToString[\[Beta]]<>"_allDeltas_allN_rz="<>ToString[rz]<>"_p="<>ToString[p]]
	]


getExecTimesFor\[Beta]and\[Delta][enes_, \[Beta]_, \[Delta]_, rz_, p_]:=
	Module[{times = {}},
		    times = Map[getExecTime[#, \[Delta], \[Beta], rz, p]&, enes];
		    Export["execTimes_beta="<>ToString[\[Beta]]<>"_delta="<>ToString[\[Delta]]<>"_rz="<>ToString[rz]<>"_p="<>ToString[p]<>"_allN.wl", times];
		    Print["Guardados los tiempos de ej para: beta="<>ToString[\[Beta]]<>"_delta="<>ToString[\[Delta]]<>"_rz="<>ToString[rz]<>"_p="<>ToString[p]<>"_allN"]
	]


ParallelTable[getAcceptanceRatesFor\[Beta][i, deltas, enes, 0.5, 0.3], {i, betas}, DistributedContexts->All]


ParallelTable[getExecTimesFor\[Beta]and\[Delta][enes, j, k, 0.5, 0.3], {j, betas}, {k, deltas}, DistributedContexts->All]
