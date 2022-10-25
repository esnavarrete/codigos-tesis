#!/usr/bin/env wolframscript
(* ::Package:: *)

Get["/home/enavarrete/libs/CoolTools.m"]
Get["/home/enavarrete/libs/usefulFunctions.wl"]


SetDirectory["/home/enavarrete/mh_samples_GOOD/distintas_Nbetasydeltas_Rp5Pp3_DOS"]


deltas = {0.005, 0.01, 0.03, 0.05, 0.07, 0.1};
betas = {100, 250, 400, 600, 750, 1000};
enes = {10000, 20000, 40000, 60000, 80000, 100000, 200000, 350000, 500000};
rz = 0.5;
swapP = 0.3;


runAndExportMH[N_, \[Beta]_, \[Delta]_, swapP_, rz_]:= With[{targetstate = (IdentityMatrix[2] + rz PauliMatrix[3])/2},
													With[{initialstate = initStateGenerator[\[Beta], \[Delta], swapP, targetstate, 10^(0.472)/\[Beta]]},
														sample = Timing[metropolisHastingsSampleGOOD[N, \[Beta], \[Delta], swapP, initialstate, targetstate]];
													    Export["MHsample_N="<>ToString[N]<>"_delta="<>ToString[\[Delta]]<>"_beta="<>ToString[\[Beta]]<>"_rz="<>ToString[rz]<>"_p="<>ToString[swapP]<>".wl", sample];
												        Print["Guardada la muestra con: N="<>ToString[N]<>"_delta="<>ToString[\[Delta]]<>"_beta="<>ToString[\[Beta]]<>"_rz="<>ToString[rz]<>"_p="<>ToString[swapP]]
													]
									 		 ]


ParallelTable[runAndExportMH[enes[[8]], betas[[6]], j, swapP, rz], {j, deltas[[5;;]]}, DistributedContexts->All]


ParallelTable[runAndExportMH[enes[[9]], i, j, swapP, rz], {i, betas[[4;;5]]}, {j, deltas}, DistributedContexts->All]


ParallelTable[runAndExportMH[enes[[9]], betas[[6]], i, swapP, rz], {i, deltas[[1;;4]]}, DistributedContexts->All]


ParallelTable[runAndExportMH[enes[[9]], betas[[3]], i, swapP, rz], {i, deltas[[5;;]]}, DistributedContexts->All]


(*betasDeltasCombs = Flatten[Outer[{#1, #2}&, betas, deltas], 1];*)
(*Outer[runAndExportMH[#2, #1[[1]], #1[[2]], swapP, rz]&, betasDeltasCombs, enes, 1];*)
