#!/usr/bin/env wolframscript
(* ::Package:: *)

Get["/home/enavarrete/libs/CoolTools.m"]
Get["/home/enavarrete/libs/usefulFunctions.wl"]


SetDirectory["/home/enavarrete/mh_samples_GOOD/gran_test_error"]


deltas = {0.001, 0.005, 0.01, 0.03, 0.05, 0.07, 0.1};
betas = {10, 50, 100, 250, 500, 750, 1000};
ene = 20000;
rzs = {0, 0.5, 0.8};
swapPs = {0.3, 0.5, 0.8};


runAndExportMH[N_, \[Beta]_, \[Delta]_, swapP_, rz_]:= With[{targetstate = (IdentityMatrix[2] + rz PauliMatrix[3])/2},
													With[{initialstate = universalInitState},
														sample = Timing[metropolisHastingsSampleGOOD[N, \[Beta], \[Delta], swapP, initialstate, targetstate]];
													    Export["MHsample_N="<>ToString[N]<>"_delta="<>ToString[\[Delta]]<>"_beta="<>ToString[\[Beta]]<>"_rz="<>ToString[rz]<>"_p="<>ToString[swapP]<>".wl", sample];
												        Print["Guardada la muestra con: N="<>ToString[N]<>"_delta="<>ToString[\[Delta]]<>"_beta="<>ToString[\[Beta]]<>"_rz="<>ToString[rz]<>"_p="<>ToString[swapP]]
													]
									 		 ]


ParallelTable[runAndExport[ene, i, j, k, l], {i,betas}, {j,deltas}, {k,swapPs}, {l,rzs}, DistributedContexts->All]


(*betasDeltasCombs = Flatten[Outer[{#1, #2}&, betas, deltas], 1];*)
(*Outer[runAndExportMH[#2, #1[[1]], #1[[2]], swapP, rz]&, betasDeltasCombs, enes, 1];*)
