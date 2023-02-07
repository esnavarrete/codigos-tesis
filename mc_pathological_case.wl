(* ::Package:: *)

(* ::Title:: *)
(*Analizando los casos patol\[OAcute]gicos del MC con diferentes errores*)


(* ::Text:: *)
(*Hay diversos casos patol\[OAcute]gicos . Los m\[AAcute]s importantes son con Subscript[r, z] = 0.8 y p = 0.3, 0.8, pues en estos casos los histogramas de puntos MCS tienen picos que parecen contrarios a los de los histogramas brutales. Por ahora, se tienen muchas muestras para p  = 0.8 y s\[OAcute]lo se analizar\[AAcute] este caso; no es de preocuparse, pues el caso con p = 0.3 es muy similar .*)


Needs["Quantum`"]
<<MaTeX`
Get["/media/storage/ciencia/investigacion/tesis/codigos-tesis/CoolTools2.m"]
Get["/media/storage/ciencia/investigacion/tesis/codigos-tesis/usefulFunctions.wl"]


SetDirectory["/media/storage/ciencia/investigacion/tesis/muestras_MCS"]


(* ::Section::Closed:: *)
(*Importando datos de mega-muestras*)


targets = Map[(IdentityMatrix[2] + # PauliMatrix[3])/2 &, {0, 0.5, 0.8}];


(*importando datos y juntando todos los puntos*)
mcsMegaSampleRp8Pp8 = Join[
	Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p01/sampleMCS_n="<>ToString[#]<>"_err=0.01_delta=0.01_t=0.3_rz=0.8_p=0.8.wl"][[2]]&, {10000, 30000, 50000}],1],
	Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p01/sampleMCS_n=10000_err=0.01_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[10]], 1],
	Get["initial_samples/sampleMCS_n=10000_err=0.01_paso=0.01_t=0.3_rz=0.8_p=0.8.m"][[2]]
];


brutalMegaSampleRp8Pp8 = Join[
	Flatten[Map[ketsToDensity[Get["../muestras_brutales_extra/samplesRp8Pp8/brutalSample_N=5000_err=0.01_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]]&, Range[30]],1],
	ketsToDensity[Get["../muestras_brutales_extra/brutalSamples_rz=0.8_N=7000_eps=0.01_allswapPs.m"][[3]]],
	ketsToDensity[Get["../muestras_brutales_extra/brutalSamples2_rz=0.8_N=7000_eps=0.01_allswapPs.m"][[3]]],
	ketsToDensity[Get["../muestras_brutales_extra/brutalSamples3_rz=0.8_N=10000_eps=0.01_allswapPs.m"][[3]]]
];


(*Quitamos los puntos "malos" de la muestra MCS*)
mcsMegaSampleRp8Pp8 = Select[mcsMegaSampleRp8Pp8, Norm[coarseGraining2[#,0.8]-targets[[3]],"Frobenius"] <= 0.01 &];


Clear[brutalMegaSampleRp8Pp8]


brutalMegaSampleRp8Pp8TIME = Total@Map[Get["../muestras_brutales_extra/samplesRp8Pp8/brutalSample_N=5000_err=0.01_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[1]]&, Range[6]]


(* ::Section:: *)
(*Comparando distintos valores del error*)


(* ::Text:: *)
(*Hemos visto que aunque se cambie N, los picos del MCS aparecen en el mismo lugar . Creo que los picos aparecen por que son la "orilla" de la regi\[OAcute]n de inter\[EAcute]s \[CapitalOmega] . As\[IAcute], si cambiamos el valor del error (ancho de \[CapitalOmega]), me imagino que cambiar\[AAcute] la posici\[OAcute]n de los picos ... Veamos .*)


errors = {0.0009, 0.001, 0.0015, 0.0017, 0.002, 0.003, 0.006, 0.008, 0.01, 0.02, 0.03, 0.06, 0.1, 0.2};
errors2 = {0.0005, 0.001, 0.002, 0.003, 0.006, 0.008, 0.02, 0.03, 0.06, 0.1, 0.2};
errors3 = Join[{0.0005}, Drop[errors, {9}]];
exactAvgRp8Pp8 = swapGate . \[Rho]averageTwoQubitsbasiscomp[0.2,0.8] . swapGate;
avgStateRp8Pp8BRUTAL = preimageMean[brutalMegaSampleRp8Pp8];


(* ::Subsection::Closed:: *)
(*Importando datos para esta secci\[OAcute]n*)


(*Para \[Delta] = 0.1*)
samplesDiffErrors\[Delta]p1 = {
	mcsSampleEp008\[Delta]p1 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p1/sampleMCS_n=6000_err=0.008_delta=0.1_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
	mcsSampleEp02\[Delta]p1 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p1/sampleMCS_n=6000_err=0.02_delta=0.1_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
	mcsSampleEp03\[Delta]p1 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p1/sampleMCS_n=6000_err=0.03_delta=0.1_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
	mcsSampleEp06\[Delta]p1 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p1/sampleMCS_n=6000_err=0.06_delta=0.1_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
	mcsSampleEp1\[Delta]p1 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p1/sampleMCS_n=6000_err=0.1_delta=0.1_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
	mcsSampleEp2\[Delta]p1 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p1/sampleMCS_n=6000_err=0.2_delta=0.1_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1]
};


Clear[mcsSampleEp008\[Delta]p1, mcsSampleEp02\[Delta]p1, mcsSampleEp03\[Delta]p1, mcsSampleEp06\[Delta]p1, mcsSampleEp1\[Delta]p1, mcsSampleEp2\[Delta]p1]


(*Ya tenemos la muestra con \[Epsilon] = 0.01. Importemos las dem\[AAcute]s:*)
(*todos traen delta = 0.01, Estas muestras se generanron con el maxlimit, las dem\[AAcute]s no*)
samplesDiffErrors\[Delta]p01 ={
 mcsSampleEp0009 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p01/sampleMCS_n=7000_err=0.0009_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp001 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p01/sampleMCS_n=10000_err=0.001_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp0015 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p01/sampleMCS_n=7000_err=0.0015_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp0017 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p01/sampleMCS_n=7000_err=0.0017_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp002 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p01/sampleMCS_n=7000_err=0.002_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp003 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p01/sampleMCS_n=10000_err=0.003_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp006 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p01/sampleMCS_n=10000_err=0.006_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp008 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p01/sampleMCS_n=10000_err=0.008_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp01 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p01/sampleMCS_n=10000_err=0.01_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]], 1],
 mcsSampleEp02 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p01/sampleMCS_n=10000_err=0.02_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp03 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p01/sampleMCS_n=10000_err=0.03_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp06 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p01/sampleMCS_n=10000_err=0.06_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp1 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p01/sampleMCS_n=10000_err=0.1_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp2 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p01/sampleMCS_n=10000_err=0.2_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1]
};


Clear[mcsSampleEp0009, mcsSampleEp001, mcsSampleEp0015, mcsSampleEp0017, mcsSampleEp002, mcsSampleEp003, mcsSampleEp006, mcsSampleEp008, mcsSampleEp01,
	  mcsSampleEp02, mcsSampleEp03, mcsSampleEp06, mcsSampleEp1, mcsSampleEp2]


(*Ac\[AAcute] van los que traen la delta m\[AAcute]s chica (\[Delta] = 0.005):*)
samplesDiffErrors\[Delta]p005 = {
 mcsSampleEp0005\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p005/sampleMCS_n=6000_err=0.0005_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp0009\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p005/sampleMCS_n=7000_err=0.0009_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp001\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p005/sampleMCS_n=7000_err=0.001_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp0015\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p005/sampleMCS_n=7000_err=0.0015_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp0017\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p005/sampleMCS_n=7000_err=0.0017_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp002\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p005/sampleMCS_n=7000_err=0.002_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp003\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p005/sampleMCS_n=6000_err=0.003_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp006\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p005/sampleMCS_n=6000_err=0.006_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp008\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p005/sampleMCS_n=6000_err=0.008_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp02\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p005/sampleMCS_n=6000_err=0.02_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp03\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p005/sampleMCS_n=6000_err=0.03_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp06\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p005/sampleMCS_n=6000_err=0.06_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp1\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p005/sampleMCS_n=6000_err=0.1_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp2\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p005/sampleMCS_n=6000_err=0.2_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1]
};


Clear[mcsSampleEp0005\[Delta]p005, mcsSampleEp0009\[Delta]p005, mcsSampleEp0015\[Delta]p005, mcsSampleEp0017\[Delta]p005, mcsSampleEp001\[Delta]p005, mcsSampleEp002\[Delta]p005, mcsSampleEp003\[Delta]p005,
	  mcsSampleEp006\[Delta]p005, mcsSampleEp008\[Delta]p005, mcsSampleEp02\[Delta]p005, mcsSampleEp03\[Delta]p005, mcsSampleEp06\[Delta]p005, mcsSampleEp1\[Delta]p005, mcsSampleEp2\[Delta]p005]


(*Ac\[AAcute] van con \[Delta] = 0.001 y s\[IAcute] hay con todos los errores (errors2)*)
samplesDiffErrors\[Delta]p001 = {
	mcsSampleEp0005\[Delta]p001 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p001/sampleMCS_n=6000_err=0.0005_delta=0.001_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
	mcsSampleEp001\[Delta]p001 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p001/sampleMCS_n=6000_err=0.001_delta=0.001_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
	mcsSampleEp002\[Delta]p001 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p001/sampleMCS_n=6000_err=0.002_delta=0.001_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
	mcsSampleEp003\[Delta]p001 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p001/sampleMCS_n=6000_err=0.003_delta=0.001_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
	mcsSampleEp006\[Delta]p001 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p001/sampleMCS_n=6000_err=0.006_delta=0.001_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
	mcsSampleEp008\[Delta]p001 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p001/sampleMCS_n=6000_err=0.008_delta=0.001_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
	mcsSampleEp02\[Delta]p001 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p001/sampleMCS_n=6000_err=0.02_delta=0.001_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
	mcsSampleEp03\[Delta]p001 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p001/sampleMCS_n=6000_err=0.03_delta=0.001_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
	mcsSampleEp06\[Delta]p001 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p001/sampleMCS_n=6000_err=0.06_delta=0.001_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
	mcsSampleEp1\[Delta]p001 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p001/sampleMCS_n=6000_err=0.1_delta=0.001_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
	mcsSampleEp2\[Delta]p001 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p001/sampleMCS_n=6000_err=0.2_delta=0.001_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1]
};


Clear[mcsSampleEp0005\[Delta]p001,mcsSampleEp001\[Delta]p001, mcsSampleEp002\[Delta]p001, mcsSampleEp003\[Delta]p001, mcsSampleEp006\[Delta]p001, mcsSampleEp008\[Delta]p001, mcsSampleEp02\[Delta]p001,
	  mcsSampleEp03\[Delta]p001, mcsSampleEp06\[Delta]p001, mcsSampleEp1\[Delta]p001, mcsSampleEp2\[Delta]p001]


(*con \[Delta] = 0.0005*)
samplesDiffErrors\[Delta]p0005 = {
	mcsSampleEp006\[Delta]p0005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p0005/sampleMCS_n=6000_err=0.006_delta=0.0005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[2]],1],
	mcsSampleEp008\[Delta]p0005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p0005/sampleMCS_n=6000_err=0.008_delta=0.0005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
	mcsSampleEp02\[Delta]p0005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p0005/sampleMCS_n=6000_err=0.02_delta=0.0005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
	mcsSampleEp03\[Delta]p0005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p0005/sampleMCS_n=6000_err=0.03_delta=0.0005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
	mcsSampleEp06\[Delta]p0005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p0005/sampleMCS_n=6000_err=0.06_delta=0.0005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
	mcsSampleEp1\[Delta]p0005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p0005/sampleMCS_n=6000_err=0.1_delta=0.0005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
	mcsSampleEp2\[Delta]p0005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p0005/sampleMCS_n=6000_err=0.2_delta=0.0005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1]
};


Clear[mcsSampleEp006\[Delta]p0005, mcsSampleEp008\[Delta]p0005, mcsSampleEp02\[Delta]p0005, mcsSampleEp03\[Delta]p0005, mcsSampleEp06\[Delta]p0005, mcsSampleEp1\[Delta]p0005, mcsSampleEp2\[Delta]p0005]


(* ::Subsection:: *)
(*Comportamiento de las fidelidades entre promedios en funci\[OAcute]n del error*)


(* ::Subsubsection:: *)
(*Gr\[AAcute]fica de las fidelidades*)


(* ::Text:: *)
(*Teniendo en cuenta c\[OAcute]mo las fidelidades pueden disminuir si una muestra con \[Delta] y \[Epsilon] incompatibles es ajustada, surge la pregunta: \[DownQuestion]c\[OAcute]mo se comportar\[AAcute] la fidelidad vs error si la delta s\[IAcute] se escoge apropiadamente? Veamos primero las fidelidades de todas las muestras con \[Delta] = 0.01 (en este caso, los errores m\[AAcute]s peque\[NTilde]os resultar\[AAcute]n incompatibles con esta delta):*)


(*fidsVSerrorData\[Delta]p01 = MapThread[{#1,#2}&, {errors, Map[1-fidelity[avgStateRp8Pp8BRUTAL, preimageMean[#]]&, samplesDiffErrors\[Delta]p01]}];*)
fidsVSerrorData\[Delta]p005 = MapThread[{#1,#2}&, {errors3, Map[1-fidelity[avgStateRp8Pp8BRUTAL, preimageMean[#]]&, samplesDiffErrors\[Delta]p005]}];
fidsVSerrorData\[Delta]p001 = MapThread[{#1,#2}&, {errors2, Map[1-fidelity[avgStateRp8Pp8BRUTAL, preimageMean[#]]&, samplesDiffErrors\[Delta]p001]}];
fidsVSerrorData\[Delta]p1 = MapThread[{#1,#2}&, {errors2[[6;;]], Map[1-fidelity[avgStateRp8Pp8BRUTAL, preimageMean[#]]&, samplesDiffErrors\[Delta]p1]}];
fidsVSerrorData\[Delta]p0005 = MapThread[{#1,#2}&, {errors2[[5;;]], Map[1-fidelity[avgStateRp8Pp8BRUTAL, preimageMean[#]]&, samplesDiffErrors\[Delta]p0005]}];


purgeSamplePp8[sample_, maxError_, minError_]:= Select[sample, minError <= Norm[coarseGraining2[#,0.8]-targets[[3]],"Frobenius"] <= maxError &];


samplesDiffErrors\[Delta]p01PURGED = MapThread[purgeSamplePp8[#1, #2]&, {samplesDiffErrors\[Delta]p01, errors}];
fidsVSerrorData\[Delta]p01PURGED = MapThread[{#1,#2}&, {errors, Map[1-fidelity[exactAvgRp8Pp8, preimageMean[#]]&, samplesDiffErrors\[Delta]p01PURGED]}];


(*Veamos todo junto*)
generalFidsPlot = ListLogLogPlot[{fidsVSerrorData\[Delta]p1, fidsVSerrorData\[Delta]p005, fidsVSerrorData\[Delta]p001, fidsVSerrorData\[Delta]p0005},
									PlotTheme->"Scientific",
									GridLines->Automatic,
									FrameLabel->MaTeX[{"\\text{Error}\, (\\varepsilon)", "1 - F(\\mathcal{A}_{\\text{brutal}},\, \\mathcal{A}_{\\text{MC}})"}],
									FrameStyle->Black,
									PlotLegends->PointLegend[{"0.1", "0.005", "0.001", "0.0005"}, LegendLabel->\[Delta], LabelStyle->Directive[Black, FontFamily->"Latin Modern Math"]]]


Export["fidelidadVSerror_Rp8Pp8.pdf", %37]


sampleErrorsEp008\[Delta]p1 = distsToTarget[samplesDiffErrors\[Delta]p1[[1]], targets[[3]], 0.8];
sampleErrorsEp008\[Delta]p0005 = distsToTarget[samplesDiffErrors\[Delta]p0005[[2]], targets[[3]], 0.8];


{ListPlot[sampleErrorsEp008\[Delta]p1, PlotRange->{0, 0.008}, 
		  PlotTheme->"Scientific", FrameLabel->MaTeX[{"\\ket{\\psi_i}", "d(\\mathcal{C}[\\ket{\\psi_i}], \\varrho_t)"}, Preamble->{"\\usepackage{physics, newtxmath}"}]],
 ListPlot[sampleErrorsEp008\[Delta]p0005, PlotRange->{0, 0.008}, 
          PlotTheme->"Scientific", FrameLabel->MaTeX[{"\\ket{\\psi_i}", "d(\\mathcal{C}[\\ket{\\psi_i}], \\varrho_t)"}, Preamble->{"\\usepackage{physics, newtxmath}"}]]}


Export["errorsPlot_Rp8Pp8_deltap0005.pdf", %52[[2]]]


cylPlotsEp008\[Delta]p1 = tripleCylindricalPlotNoFilling[samplesDiffErrors\[Delta]p1[[1]], 0.8, Darker[#,0.2]&/@{Red,Green}, {Full, {0,6.21},Full}, {20, Automatic, Automatic}];
cylPlotsEp008\[Delta]p0005 = tripleCylindricalPlotNoFilling[samplesDiffErrors\[Delta]p0005[[2]], 0.8, Darker[#,0.2]&/@{Red,Green}, {Full, {0,6.21},Full}, {20, Automatic, Automatic}];


cylPlotsBRUTAL = tripleCylindricalPlotNoFilling[brutalMegaSampleRp8Pp8, 0.8, {Hue[0.11,0.9,0.85], Hue[0.55,1,0.7]}, {Full, {0,6.21},Full}];


generalLegend = 
Grid[{{SwatchLegend[{Darker[Red,.2], Darker[Green, .2]}, 
					MaTeX[{"\\tr_A", "\\tr_B"}, Preamble->{"\\usepackage{physics, newtxmath}"}], 
					LegendLabel->MaTeX["\\text{Monte-Carlo}"], 
					LegendLayout->"Row"], , , ,,
		SwatchLegend[{Hue[0.11,0.9,0.85], Hue[0.55,1,0.7]}, 
	                 MaTeX[{"\\tr_A", "\\tr_B"}, Preamble->{"\\usepackage{physics, newtxmath}"}], 
	                 LegendLabel->MaTeX["\\text{Brutal}"], 
	                 LegendLayout->"Row"]}
}]


Grid[{{Item[MaTeX["\\delta_1 = 0.1"], Frame->False], SpanFromLeft, SpanFromLeft},
	  MapThread[Show[#1, #2]&, {cylPlotsBRUTAL, cylPlotsEp008\[Delta]p1}],
	  {Item[MaTeX["\\delta_2 = 0.0005"], Frame->False], SpanFromLeft, SpanFromLeft},
	  MapThread[Show[#1, #2]&, {cylPlotsBRUTAL, cylPlotsEp008\[Delta]p0005}],
	  {generalLegend, SpanFromLeft, SpanFromLeft}    
}, ItemStyle->ImageSizeMultipliers->0.7, Spacings->{1, 1}]


Directory[]


Export["cylPlots_Rp8Pp8_delta1and2.pdf", %52]


(* ::Subsubsection::Closed:: *)
(*Comportamiento de los picos con distintos errores*)


zAComps\[Delta]p0005 = Map[Map[partialtarcesBV[#][[1,3]]&, #]&, samplesDiffErrors\[Delta]p0005];


Histogram[{zAComps\[Delta]p0005[[1]]}~Join~zAComps\[Delta]p0005[[3;;4]], 100, "Probability", 
	ChartLegends->SwatchLegend[MaTeX[{errors2[[5]]}~Join~errors2[[7;;8]]], LegendLabel->MaTeX["\\varepsilon", Preamble->{"\\usepackage{newtxmath}"}]], 
	PlotTheme->"Scientific",
	FrameLabel->MaTeX[{"r_z^{(A)}", "\\text{Fracci\[OAcute]n de estados}"}, Preamble->{"\\usepackage{newtxmath}"}],
	GridLines->Automatic]


Export["distRZA_diffErrors_deltap0005.pdf", %41]


zAComps\[Delta]p001 = Map[Map[partialtarcesBV[#][[1,3]]&, #]&, samplesDiffErrors\[Delta]p001];


Histogram[{zAComps\[Delta]p001[[1]]}~Join~zAComps\[Delta]p001[[6;;8]], 100, "Probability", 
ChartLegends->SwatchLegend[MaTeX[{errors2[[1]]}~Join~errors2[[6;;8]]], LegendLabel->MaTeX["\\varepsilon", Preamble->{"\\usepackage{newtxmath}"}]], 
PlotTheme->"Scientific",
FrameLabel->MaTeX[{"r_z^{(A)}", "\\text{Fracci\[OAcute]n de estados}"}, Preamble->{"\\usepackage{newtxmath}"}],
GridLines->Automatic]


Export["distRZA_diffErrors_deltap001.pdf", %45]


(* ::Text:: *)
(*Veamos qu\[EAcute] pasa para r_ZB*)


zBComps\[Delta]p0005 = Map[Map[partialtarcesBV[#][[2,3]]&, #]&, samplesDiffErrors\[Delta]p0005];


Histogram[{zBComps\[Delta]p0005[[1]]}~Join~zBComps\[Delta]p0005[[3;;4]], 1000, "Probability", 
	ChartLegends->SwatchLegend[MaTeX[{errors2[[5]]}~Join~errors2[[7;;8]]], LegendLabel->MaTeX["\\varepsilon"]], 
	PlotTheme->"Scientific",
	FrameLabel->MaTeX[{"r_z^{(B)}", "\\text{Densidad normalizada}"}],
	GridLines->Automatic]


zBComps\[Delta]p001 = Map[Map[partialtarcesBV[#][[2,3]]&, #]&, samplesDiffErrors\[Delta]p001];


Histogram[{zBComps\[Delta]p001[[1]]}~Join~zBComps\[Delta]p001[[6;;8]], 5000, "Probability", 
ChartLegends->SwatchLegend[MaTeX[{errors2[[1]]}~Join~errors2[[6;;8]]], LegendLabel->MaTeX["\\varepsilon"]], 
PlotTheme->"Scientific",
FrameLabel->MaTeX[{"r_z^{(B)}", "\\text{Densidad normalizada}"}],
GridLines->Automatic]


Clear[zAComps\[Delta]p0005, zBComps\[Delta]p0005, zAComps\[Delta]p001, zBComps\[Delta]p001]


(* ::Subsection::Closed:: *)
(*Comportamiento de las fidelidades entre promedios en funci\[OAcute]n de N*)


Map[Length[#]&, samplesDiffErrors\[Delta]p005]


data = Map[Table[1-fidelity[preimageMean[#[[1;;i]]], exactAvgRp8Pp8],{i,5000,50000,5000}]&, samplesErr];
dataGood = Map[MapThread[{#1,#2}&, {Table[i,{i,5000,50000,5000}], #}]&, data];


data2 = Map[Table[1-fidelity[preimageMean[#[[1;;i]]], exactAvgRp8Pp8],{i,5000, 35000, 5000}]&, samplesDiffErrors\[Delta]p005];
dataGood2 = Map[MapThread[{#1,#2}&, {Table[i,{i, 5000, 35000,5000}], #}]&, data2];


ListLogLogPlot[dataGood, PlotTheme->"Detailed", FrameLabel->{"N", "1 - F"}, PlotLegends->errors]


ListLogPlot[dataGood2, PlotTheme->"Detailed", FrameLabel->{"Tama\[NTilde]o (N)", "1 - F(A,\!\(\*SubscriptBox[\(A\), \(mc\)]\))"}, PlotLegends->errors[[1;;5]], Joined->True]


(* ::Section::Closed:: *)
(*Errores de los estados en los bordes *)


(* ::Subsection:: *)
(*Bordes de las esferas *)


(* ::Subsubsection::Closed:: *)
(*Primer par de deltas: 0.0005 y 0.1*)


(*Se ocupan los datos de la secci\[OAcute]n "Comparando distintos valores del error"*)
A\[Delta]p0005BVs = partialtarcesBV[#][[1]]& /@ samplesDiffErrors\[Delta]p0005[[2]];
A\[Delta]p1BVs = partialtarcesBV[#][[1]]& /@ samplesDiffErrors\[Delta]p1[[1]];


zAComps\[Delta]p0005 = Map[#[[3]]&, A\[Delta]p0005BVs];
zAComps\[Delta]p1 = Map[#[[3]]&, A\[Delta]p1BVs];


Histogram[{zAComps\[Delta]p0005, zAComps\[Delta]p1}, 50, "Probability", ChartLegends->{"\[Delta] = 0.0005", "\[Delta] = 0.1"}, PlotTheme->"Scientific", FrameLabel->{"\!\(\*SuperscriptBox[SubscriptBox[\(r\), \(z\)], \(A\)]\)", "Densidad"},
			Epilog->{Line[{{0.07, 0}, {0.07, 0.13}}]}]


borderBVs\[Delta]p0005A = Select[A\[Delta]p0005BVs, #[[3]] <= 0.07 &];
borderBVs\[Delta]p1A = Select[A\[Delta]p1BVs, #[[3]] <= 0.09 &];


{Dimensions[borderBVs\[Delta]p0005A], Dimensions[borderBVs\[Delta]p1A]}


borderStates\[Delta]p0005 = Extract[samplesDiffErrors\[Delta]p0005[[2]], Position[A\[Delta]p0005BVs, #]][[1]]& /@ borderBVs\[Delta]p0005A;
borderStates\[Delta]p1 = Extract[samplesDiffErrors\[Delta]p1[[1]], Position[A\[Delta]p1BVs, #]][[1]]& /@ borderBVs\[Delta]p1A;


visualizeBipartiteSystem[borderStates\[Delta]p0005]


{ListPlot[{distsToTarget[borderStates\[Delta]p1, targets[[3]],0.8], sampleErrorsEp008\[Delta]p1[[1;;100]]}, PlotRange->{0,0.008}, PlotLabel->"\[Delta] = 0.1", PlotLegends->{"Border", "All"}], 
ListPlot[{distsToTarget[borderStates\[Delta]p0005, targets[[3]],0.8], sampleErrorsEp008\[Delta]p0005[[1;;7000]]}, PlotRange->Full, PlotLabel->"\[Delta] = 0.0005", PlotLegends->{"Border", "All"}]}


{Histogram[{distsToTarget[borderStates\[Delta]p0005, targets[[3]],0.8], sampleErrorsEp008\[Delta]p0005[[1;;600]]}, Automatic, "Probability",ChartLegends->{"Borde", "Todo"}, 
PlotLabel->"\[Delta] = 0.0005", PlotTheme->"Scientific"],
Histogram[{distsToTarget[borderStates\[Delta]p1, targets[[3]],0.8], sampleErrorsEp008\[Delta]p1[[1;;100]]}, Automatic, "Probability", ChartLegends->{"Borde", "Todo"},
PlotLabel->"\[Delta] = 0.1", PlotTheme->"Scientific"]}


{tripleCylindricalPlot[samplesDiffErrors\[Delta]p0005[[2]], targets[[3]], 100, 100, 100], tripleCylindricalPlot[borderStates\[Delta]p0005, targets[[3]], 100, 100, 100]}


sampleWithoutBorder = Delete[samplesDiffErrors\[Delta]p0005[[2]], Position[samplesDiffErrors\[Delta]p0005[[2]], #][[1]]& /@ borderStates\[Delta]p0005];


Grid[{tripleCylindricalPlot[samplesDiffErrors\[Delta]p0005[[2]], targets[[3]], 100, 100 ,100], tripleCylindricalPlot[sampleWithoutBorder, targets[[3]], 100, 100,100]}]


(* ::Subsubsection::Closed:: *)
(*Segundo par de deltas: 0.001 y 0.1*)


(*Esta nueva delta s\[OAcute]lo es 100 veces m\[AAcute]s chica que 0.1*)
A\[Delta]p001BVs = partialtarcesBV[#][[1]]& /@ samplesDiffErrors\[Delta]p001[[6]];


zAComps\[Delta]p001 = Map[#[[3]]&, A\[Delta]p001BVs];


Histogram[{zAComps\[Delta]p0005, zAComps\[Delta]p001, zAComps\[Delta]p1}, 1000, "Probability", ChartLegends->{"\[Delta] = 0.0005", "\[Delta] = 0.001", "\[Delta] = 0.1"}, PlotTheme->"Scientific"]


borderBVs\[Delta]p001A = Select[A\[Delta]p001BVs, #[[3]] <= 0.07 &];


{Dimensions[borderBVs\[Delta]p0005A], Dimensions[borderBVs\[Delta]p001A]}


borderStates\[Delta]p001 = Extract[samplesDiffErrors\[Delta]p001[[6]], Position[A\[Delta]p001BVs, #]][[1]]& /@ borderBVs\[Delta]p001A;


visualizeBipartiteSystem[borderStates\[Delta]p001]


Grid[{{ListPlot[distsToTarget[samplesDiffErrors\[Delta]p1[[1]], targets[[3]],0.8], PlotRange->{0,0.008}], 
	   ListPlot[distsToTarget[samplesDiffErrors\[Delta]p001[[6]],targets[[3]],0.8], PlotRange->{0,0.008}]}
}]


Grid[{{ListPlot[distsToTarget[#, targets[[3]],0.8]& /@ {samplesDiffErrors\[Delta]p1[[1, 1;;50]], borderStates\[Delta]p1}, PlotLegends->{"All", "Border"}, PlotLabel->"\[Delta] = 0.1", PlotRange->{0,0.008}], 
	   ListPlot[distsToTarget[#, targets[[3]], 0.8]& /@ {samplesDiffErrors\[Delta]p001[[6, 1;;800]], borderStates\[Delta]p001}, PlotLegends->{"All", "Border"}, PlotLabel->"\[Delta] = 0.001", PlotRange->{0,0.008}]}
}]


(* ::Subsection::Closed:: *)
(*Bordes de la regi\[OAcute]n aceptada*)


(* ::Text:: *)
(*Se entiende como "regi\[OAcute]n aceptada" a los estados de la preimagen cuyo error es cercano a \[Epsilon].*)


Grid[{{ListPlot[sampleErrorsEp008\[Delta]p0005, PlotRange->{0,0.008}], Histogram[sampleErrorsEp008\[Delta]p0005, Automatic, "Probability"]},
	  {ListPlot[sampleErrorsEp008\[Delta]p1, PlotRange->{0,0.008}], Histogram[sampleErrorsEp008\[Delta]p1, Automatic, "Probability", PlotRange->{{0, 0.008}, Automatic}]}
}, ItemSize->Full]


borderStates\[Delta]p0005 = purgeSamplePp8[samplesDiffErrors\[Delta]p0005[[2]], 0.008, 0.00795];
borderStates\[Delta]p1 = purgeSamplePp8[samplesDiffErrors\[Delta]p1[[1]], 0.008, 0.00795];


{Dimensions@borderStates\[Delta]p0005, Dimensions@borderStates\[Delta]p1}


Grid[{{ListPlot[distsToTarget[borderStates\[Delta]p0005, targets[[3]],0.8], PlotRange->{0,0.008}], Histogram[distsToTarget[borderStates\[Delta]p0005, targets[[3]],0.8], Automatic, "Probability"]},
	  {ListPlot[distsToTarget[borderStates\[Delta]p1, targets[[3]],0.8], PlotRange->{0,0.008}], Histogram[distsToTarget[borderStates\[Delta]p1, targets[[3]],0.8], Automatic, "Probability"]}
}, ItemSize->Full]


Grid[{tripleCylindricalPlot[borderStates\[Delta]p0005[[1;;600]], targets[[3]],50,50,50],
	  tripleCylindricalPlot[borderStates\[Delta]p1, targets[[3]], 50,50,50]}]


{visualizeBipartiteSystem[borderStates\[Delta]p0005[[1;;1000]]], visualizeBipartiteSystem[borderStates\[Delta]p1]}


(* ::Section::Closed:: *)
(*Generando trayectorias que caigan en los bordes*)


minimizationStep[{initialstate_, targetstate_ , p_}, \[Delta]_?((0<#<1)&), \[Tau]_?((0<#<1)&)]:=
With[{U = randomSmallEvolution[4, \[Delta]]},
	With[{newstate = Chop[U . initialstate . ConjugateTranspose[U]]},
	If[Norm[targetstate-coarseGraining2[newstate,p], "Frobenius"]<Norm[targetstate-coarseGraining2[initialstate,p],"Frobenius"],
	    newstate,
		RandomChoice[{\[Tau], 1 - \[Tau]}->{newstate,initialstate}]]]]


simpleMCMinimizationList[{initialstate_,targetstate_,p_}, \[Epsilon]_?((0<#<1)&), \[Delta]_?((0<#<1)&), \[Tau]_?((0<#<1)&)]:=
Module[{maxlimit = 3000},
		NestWhileList[minimizationStep[{#,targetstate, p}, \[Delta], \[Tau]]&,
				 initialstate,
				 (Norm[targetstate - coarseGraining2[#,p], "Frobenius"] > \[Epsilon])&, 1]]


simpleMonteCarloSampleList[size_, targetstate_, p_, \[Epsilon]_, \[Delta]_, \[Tau]_]:=
		simpleMCMinimizationList[{#, targetstate, p}, \[Epsilon], \[Delta], \[Tau]]& /@ ketsToDensity[randomKets[4, size]];


(*Sea el error 0.008*)
initialstate = initStateGenerator[200, 0.01, 0.8, targets[[3]], 0.01];


trajectory\[Delta]p0005 = simpleMCMinimizationList[{initialstate, targets[[3]], 0.8}, 0.008, 0.0005, 0.2];
trajectory\[Delta]p001 = simpleMCMinimizationList[{initialstate, targets[[3]], 0.8}, 0.008, 0.001, 0.2];


trajectory\[Delta]p01 = simpleMCMinimizationList[{initialstate, targets[[3]], 0.8}, 0.008, 0.01, 0.2];
trajectory\[Delta]p1 = simpleMCMinimizationList[{initialstate, targets[[3]], 0.8}, 0.008, 0.1, 0.2];


{Dimensions[trajectory\[Delta]p0005], Dimensions[trajectory\[Delta]p001], Dimensions[trajectory\[Delta]p01], Dimensions[trajectory\[Delta]p1]}


Clear[trajectory\[Delta]p0005, trajectory\[Delta]p001, trajectory\[Delta]p01, trajectory\[Delta]p05, trajectory\[Delta]p1]


Show[ListPlot[distsToTarget[#, targets[[3]], 0.8]& /@ {trajectory\[Delta]p0005[[4;;]], trajectory\[Delta]p001[[4;;]], trajectory\[Delta]p01[[60;;]], trajectory\[Delta]p1[[41305;;]]}, 
			  Joined->True,
			  Mesh->Full,
			  PlotRange->Automatic,
			  PlotLegends->LineLegend[MaTeX[{0.0005, 0.001, 0.01, 0.1}], LegendLabel->MaTeX["\\delta"]],
			  PlotTheme->"Scientific",
			  FrameLabel->MaTeX[{"\\ket{\\psi_i}", "d(\\mathcal{C}[\\ket{\\psi_i}], \\varrho_t)"}, Preamble->{"\\usepackage{physics, newtxmath}"}]], 
Plot[0.008, {x,0,35}, PlotStyle->Directive[Red, Dashed, Thickness[0.001]]]]


Export["trajectories_for_diff_deltas.pdf", %333]


Show[visualizeBipartiteSystem[{trajectory\[Delta]p0005[[-1]]}, {Pink, Yellow}], 
	 visualizeBipartiteSystem[{trajectory\[Delta]p08[[-1]]}],
ParametricPlot3D[{Cos[t], Sin[t], 0}, {t, 0, 2 Pi}]]


Show[ListPlot[{distsToTarget[trajectory\[Delta]p08[[2090;;]], targets[[3]], 0.8], distsToTarget[trajectory\[Delta]p0005, targets[[3]], 0.8]}, Joined->True], Plot[0.008, {x,0,2000}, PlotStyle->Directive[Red, Dashed]]]


(* ::Section::Closed:: *)
(*Tiempos de ejecuci\[OAcute]n para algunas deltas*)


(*Tiempos de ejecuci\[OAcute]n para preimagenes con 30,000 muestras*)
mcsEp008\[Delta]p1TIME = Total@Map[Get["distintasEnes_Rp8Pp8/delta_p1/sampleMCS_n=6000_err=0.008_delta=0.1_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[1]]&, Range[5]];
mcsEp008\[Delta]p0005TIME = Total@Map[Get["distintasEnes_Rp8Pp8/delta_p0005/sampleMCS_n=6000_err=0.008_delta=0.0005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[1]]&, Range[5]];


getSampleTime[error_, delta_]:= 
Total[Map[Get["distintasEnes_Rp8Pp8/delta_p0005/sampleMCS_n=6000_err="<>ToString[error]<>"_delta="<>ToString[delta]<>"_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[1]]&, Range[5]]]


mcsTIMES\[Delta]p1 = getSampleTime[#, 0.1]& /@ errors2[[6;;]];


mcsTIMES\[Delta]p0005 = getSampleTime[#, 0.0005]& /@ errors2[[6;;]]; (*cambiar la carpeta de las deltas en la fuci\[OAcute]n getSampleTime*)


ListLogLogPlot[{MapThread[{#1, #2}&, {errors2[[6;;]], mcsTIMES\[Delta]p0005}], MapThread[{#1, #2}&, {errors2[[6;;]], mcsTIMES\[Delta]p1}]},
		        PlotLegends->LineLegend[MaTeX@{"0.0005", "0.1"}, LegendLabel->MaTeX["\\delta"]],
		        FrameLabel->MaTeX@{"\\text{Error}\,\,\\varepsilon", "\\text{Tiempo de ejecuci\[OAcute]n}\,\,[s]"},
		        PlotTheme->"Scientific",
		        Joined->True,
		        Mesh->Full,
		        GridLines->Automatic]


Export["execTime_Rp8Pp8_N=30000_delta1and2.pdf", %1069]


fidelity[#, exactAvgRp8Pp8]& /@ {avgStateRp8Pp8BRUTAL, preimageMean[samplesDiffErrors\[Delta]p1[[1]]], preimageMean[samplesDiffErrors\[Delta]p0005[[2]]]}


fidelity[preimageMean[brutalMegaSampleRp8Pp8[[1;;30000]]], exactAvgRp8Pp8]
