#!/usr/bin/env wolframscript
(* ::Package:: *)

Get["/home/enavarrete/libs/CoolTools.m"]
Get["/home/enavarrete/libs/usefulFunctions.wl"]


SetDirectory["/home/enavarrete/brutals_3qubits"]


sampleSize = 10000;
rzs = {0, 0.1, 0.5, 0.8};
MaxError = 0.01;
pvecs = {{1/3, 1/3, 1/3}, {0.5, 0.25, 0.25}, {0.9, 0.05, 0.05}};


brutalSampleNto1[rz_, sampleSize_, error_, pvec_]:= Module[{targetstate = (IdentityMatrix[2] + rz PauliMatrix[3])/2, sample = {}, n=0, dim = Length[pvec]},
	While[n < sampleSize,
		\[Psi] = ketsToDensity[randomKets[dim, 1]][[1]];
		distance = Norm[CGNto1[\[Psi], pvec] - targetstate, "Frobenius"];
		If[distance <= error,
			AppendTo[sample, \[Psi]];
			n++]
	];
	Return[sample]
]


brutalsRunAndExport[rz_, size_, error_, pvec_]:= 
Module[{sample},
		sample = Timing[brutalSampleNto1[rz, size, error, pvec]];
		Export["brutals_N="<>ToString[size]<>"_error="<>ToString[error]<>"_rz="<>ToString[rz]<>"_p="<>ToString[pvec[[1]]]<>".wl", sample];
		Print["Preimagen brutal con: N="<>ToString[size]<>"_error="<>ToString[error]<>"_rz="<>ToString[rz]<>"_p="<>ToString[pvec[[1]]]<>"guardada."]]


ParallelTable[brutalsRunAndExport[i, sampleSize, MaxError, j], {i, rzs}, {j, pvecs}, DistributedContexts->All]
