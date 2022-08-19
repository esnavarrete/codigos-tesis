#!/usr/bin/env wolframscript
(* ::Package:: *)

Get["CoolTools.m", Path->"."]
Get["usefulFunctions.wl", Path->"."]


Get["/media/storage/ciencia/investigacion/proyecto-ss/adanerick/codigo/CoolTools.m"]
Get["/media/storage/ciencia/investigacion/tesis/codigos/usefulFunctions.wl"]


(*SetDirectory["/home/enavarrete/mcs_samples/samplesRp8Pp8"]*)
SetDirectory["/media/storage/ciencia/investigacion/tesis/muestras_MCS/distintasEnes_Rp8Pp5"];


(*deltas = {0.001, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1};
taus = {0.1, 0.3, 0.5, 0.7, 0.9};
enes = {10000, 30000, 50000, 70000, 90000};*)
error = 0.01;
rz = 0.8;
swapP = 0.5;


(*funciones que se encargan del funcionamiento del algoritmo*)

minimizationStep[{initialstate_, targetstate_ , p_}, \[Delta]_?((0<#<1)&), \[Tau]_?((0<#<1)&)]:=
With[{U = randomSmallEvolution[4, \[Delta]]},
	With[{newstate = Chop[U . initialstate . ConjugateTranspose[U]]},
	If[Norm[targetstate-coarseGraining2[newstate,p], "Frobenius"]<Norm[targetstate-coarseGraining2[initialstate,p],"Frobenius"],
	    newstate,
		RandomChoice[{\[Tau], 1 - \[Tau]}->{newstate,initialstate}]]]];
		
(*Esta funci\[OAcute]n no devuelve un muestra completa, sino s\[OAcute]lo un estado perteneciente a la preimagen*)
simpleMCminimization[{initialstate_,targetstate_,p_}, \[Epsilon]_?((0<#<1)&), \[Delta]_?((0<#<1)&), \[Tau]_?((0<#<1)&)]:=
Module[{maxlimit = 3000},
		NestWhile[minimizationStep[{#,targetstate, p}, \[Delta], \[Tau]]&,
				 initialstate,
				 (Norm[targetstate - coarseGraining2[#,p], "Frobenius"] > \[Epsilon])&, 1, maxlimit]];
				 
simpleMonteCarloSample[size_, targetstate_, p_, \[Epsilon]_, \[Delta]_, \[Tau]_]:=
		simpleMCminimization[{#, targetstate, p}, \[Epsilon], \[Delta], \[Tau]]& /@ ketsToDensity[randomKets[4, size]];


(*esta funcion se encarga de la ejecucion:*)
runAndExportMCS[N_, \[Delta]_, \[Tau]_, \[Epsilon]_, swapP_, rz_, i_]:= With[{target = (IdentityMatrix[2] + rz PauliMatrix[3])/2},
													sample = Timing[simpleMonteCarloSample[N, target, swapP, \[Epsilon], \[Delta], \[Tau]]];
													Export["sampleMCS_n="<>ToString[N]<>"_err="<>ToString[\[Epsilon]]<>"_delta="<>ToString[\[Delta]]<>"_t="<>ToString[\[Tau]]<>"_rz="<>ToString[rz]<>"_p="<>ToString[swapP]<>"_"<>ToString[i]<>".wl", sample];
													Print["Muestra con: n="<>ToString[N]<>"_err="<>ToString[\[Epsilon]]<>"_delta="<>ToString[\[Delta]]<>"_t="<>ToString[\[Tau]]<>"_rz="<>ToString[rz]<>"_p="<>ToString[swapP]<>"_"<>ToString[i]<>" guardada"]
												]


(*deltasTausCombs = Flatten[Outer[{#1,#2}&, deltas, taus], 1];*)
(*Map[runAndExportMCS[#, 0.01, 0.3, error, swapP, rz]&, enes]*)

(*Outer[runAndExportMCS[#1, #2[[1]], #2[[2]], error, swapP, rz]&, enes, deltasTausCombs, 1]*)


Do[runAndExportMCS[10000, 0.01, 0.3, error, swapP, rz, i], {i,2,10}]



