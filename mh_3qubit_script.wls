#!/usr/bin/env wolframscript
(* ::Package:: *)

Get["/home/enavarrete/libs/CoolTools2.m"]
Get["/home/enavarrete/libs/usefulFunctions.wl"]


SetDirectory["/home/enavarrete/mh_samples_GOOD/final_samples_3qubits"]


beta = 100;
delta = 0.05;
ene = 200000;
rzs = {0, 0.1, 0.5, 0.8};
ps = {1/3, 1/3, 1/3};


runAndExportMH3q[N_, \[Beta]_, \[Delta]_, pvec_, rz_]:= 
With[{targetstate = (IdentityMatrix[2] + rz PauliMatrix[3])/2},
 	With[{initialstate = initStateGeneratorNq[\[Beta], \[Delta], pvec, targetstate, E/\[Beta]]},
		  sample = Timing[metropolisHastingsSampleNq[N, \[Beta], \[Delta], pvec, initialstate, targetstate]];
		  Export["MHsample_N="<>ToString[N]<>"_delta="<>ToString[\[Delta]]<>"_beta="<>ToString[\[Beta]]<>"_rz="<>ToString[rz]<>"_p=boltzmann.wl", sample];
		  Print["Guardada la muestra con: N="<>ToString[N]<>"_delta="<>ToString[\[Delta]]<>"_beta="<>ToString[\[Beta]]<>"_rz="<>ToString[rz]<>"_p=boltzmann"]
		 ]]


ParallelTable[runAndExportMH3q[ene, beta, delta, ps, i], {i, rzs}, DistributedContexts->All]
