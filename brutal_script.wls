#!/usr/bin/env wolframscript
(* ::Package:: *)

Get["./CoolTools.m"]


Get["/media/storage/ciencia/investigacion/proyecto-ss/adanerick/codigo/CoolTools.m"]


SetDirectory["/media/storage/ciencia/investigacion/tesis"]


sampleSize = 10;
rz = 0.8;
MaxError = 0.01;
swapP = 0.5;


brutalSample[rz_, sampleSize_, error_, swapP_]:= Module[{targetstate = (IdentityMatrix[2] + rz PauliMatrix[3])/2, sample = {}, n=0},
	While[n < sampleSize,
		\[Psi] = ketsToDensity[randomKets[4, 1]][[1]];
		distance = Norm[coarseGraining2[\[Psi], swapP] - targetstate, "Frobenius"];
		If[distance <= error,
			AppendTo[sample, \[Psi]];
			n++,
			0]
	];
	Return[sample]
]


(*Ac\[AAcute] empieza el ciclo que generar\[AAcute] muestras y las guardar\[AAcute] en archivos:*)
Do[
	sample = Timing[brutalSample[rz, sampleSize, MaxError, swapP]];
	Export["brutalsTEST_Rp8_Pp5_epsp01_N10_"<>ToString[i]<>".wl", sample];
	Print[i]
,{i,10}]
