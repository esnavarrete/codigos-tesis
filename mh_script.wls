#!/usr/bin/env wolframscript
(* ::Package:: *)

Get["/home/enavarrete/libs/CoolTools.m"]
Get["/home/enavarrete/libs/usefulFunctions.wl"]


SetDirectory["/home/enavarrete/mh_samples_GOOD/final_samples"]


(*deltas = {0.005, 0.01, 0.03, 0.05, 0.07, 0.1};
betas = {100, 250, 400, 600, 750, 1000};
enes = {10000, 20000, 40000, 60000, 80000, 100000, 200000, 350000, 500000, 1000000};*)
rzs = {0, 0.5, 0.8};
swapPs = {0.3, 0.5, 0.8};
delta = 0.05;
beta = 100;
ene = 200000;


runAndExportMH[N_, \[Beta]_, \[Delta]_, swapP_, rz_]:= 
With[{targetstate = (IdentityMatrix[2] + rz PauliMatrix[3])/2},
 	With[{initialstate = initStateGenerator[\[Beta], \[Delta], swapP, targetstate, 10^(0.472)/\[Beta]]},
		  sample = Timing[metropolisHastingsSampleGOOD[N, \[Beta], \[Delta], swapP, initialstate, targetstate]];
		  Export["MHsample_N="<>ToString[N]<>"_delta="<>ToString[\[Delta]]<>"_beta="<>ToString[\[Beta]]<>"_rz="<>ToString[rz]<>"_p="<>ToString[swapP]<>".wl", sample];
		  Print["Guardada la muestra con: N="<>ToString[N]<>"_delta="<>ToString[\[Delta]]<>"_beta="<>ToString[\[Beta]]<>"_rz="<>ToString[rz]<>"_p="<>ToString[swapP]]
		 ]]


ParallelTable[runAndExportMH[ene, beta, delta, i, j], {i, swapPs}, {j, rzs}, DistributedContexts->All]


(*betasDeltasCombs = Flatten[Outer[{#1, #2}&, betas, deltas], 1];*)
(*Outer[runAndExportMH[#2, #1[[1]], #1[[2]], swapP, rz]&, betasDeltasCombs, enes, 1];*)
