#!/usr/bin/env wolframscript
(* ::Package:: *)

Get["/home/enavarrete/libs/CoolTools.m"]
Get["/home/enavarrete/libs/usefulFunctions.wl"]


SetDirectory["/home/enavarrete/mh_samples/distintasN_betasydeltas_Rp8Pp5"]


deltas = {0.01, 0.02, 0.03, 0.04, 0.05};
betas = {100, 250, 400, 500, 750, 1000};
(*betas = {100, 250};*)
rz = 0.8;
swapP = 0.5;
enes = {10000, 20000, 40000, 60000, 80000, 100000, 200000, 300000, 400000};
(*enes = {600000, 650000, 700000, 800000};*)


runAndExportMH[N_, \[Beta]_, \[Delta]_, swapP_, rz_]:= With[{targetstate = (IdentityMatrix[2] + rz PauliMatrix[3])/2},
													With[{initialstate = initStateGenerator[\[Beta], \[Delta], swapP, targetstate, E \[Beta]^(-0.925)]},
														sample = Timing[metropolisHastingsSample[N, \[Beta], \[Delta], swapP, initialstate, targetstate]];
													    Export["MHsample_N="<>ToString[N]<>"_delta="<>ToString[\[Delta]]<>"_beta="<>ToString[\[Beta]]<>"_rz="<>ToString[rz]<>"_p="<>ToString[swapP]<>".wl", sample];
												        Print["Guardada la muestra con: N="<>ToString[N]<>"_delta="<>ToString[\[Delta]]<>"_beta="<>ToString[\[Beta]]<>"_rz="<>ToString[rz]<>"_p="<>ToString[swapP]]
													]
									 		 ]


betasDeltasCombs = Flatten[Outer[{#1, #2}&, betas, deltas], 1];


Outer[runAndExportMH[#2, #1[[1]], #1[[2]], swapP, rz]&, betasDeltasCombs, enes, 1];
(*Ponerle ac\[AAcute] un PArallel Table en vez de Outer*)
