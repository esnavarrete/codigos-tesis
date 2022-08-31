(* ::Package:: *)

(* ::Title:: *)
(*Analizando los casos patol\[OAcute]gicos del MC con diferentes errores*)


(* ::Text:: *)
(*Hay diversos casos patol\[OAcute]gicos . Los m\[AAcute]s importantes son con Subscript[r, z] = 0.8 y p = 0.3, 0.8, pues en estos casos los histogramas de puntos MCS tienen picos que parecen contrarios a los de los histogramas brutales. Por ahora, se tienen muchas muestras para p  = 0.8 y s\[OAcute]lo se analizar\[AAcute] este caso; no es de preocuparse, pues el caso con p = 0.3 es muy similar .*)


Needs["Quantum`"]
<<MaTeX`
Get["/media/storage/ciencia/investigacion/proyecto-ss/adanerick/codigo/CoolTools.m"]
Get["/media/storage/ciencia/investigacion/tesis/codigos-tesis/usefulFunctions.wl"]


SetDirectory["/media/storage/ciencia/investigacion/tesis/muestras_MCS"]


(* ::Section::Closed:: *)
(*Importando datos de mega-muestras*)


targets = Map[(IdentityMatrix[2] + # PauliMatrix[3])/2 &, {0, 0.5, 0.8}];


(*importando datos y juntando todos los puntos*)
mcsMegaSampleRp8Pp8 = Join[
	Flatten[Map[Get["distintasEnes_Rp8Pp8/sampleMCS_n="<>ToString[#]<>"_err=0.01_delta=0.01_t=0.3_rz=0.8_p=0.8.wl"][[2]]&, {10000, 30000, 50000}],1],
	Flatten[Map[Get["distintasEnes_Rp8Pp8/sampleMCS_n=10000_err=0.01_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[10]], 1],
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


(* ::Section:: *)
(*Errores de los elementos de las muestras *)


(* ::Section:: *)
(*Histogramas cil\[IAcute]ndricamente sim\[EAcute]tricos*)


(* ::Section:: *)
(*Histogramas cartesianos*)


(* ::Section:: *)
(*Comparando distintos valores del error*)


(* ::Text:: *)
(*Hemos visto que aunque se cambie N, los picos del MCS aparecen en el mismo lugar . Creo que los picos aparecen por que son la "orilla" de la regi\[OAcute]n de inter\[EAcute]s \[CapitalOmega] . As\[IAcute], si cambiamos el valor del error (ancho de \[CapitalOmega]), me imagino que cambiar\[AAcute] la posici\[OAcute]n de los picos ... Veamos .*)


errors = {0.0009, 0.001, 0.0015, 0.0017, 0.002, 0.003, 0.006, 0.008, 0.01, 0.02, 0.03, 0.06, 0.1, 0.2};
errors2 = {0.0005, 0.001, 0.002, 0.003, 0.006, 0.008, 0.02, 0.03, 0.06, 0.1, 0.2};
exactAvgRp8Pp8 = swapGate . \[Rho]averageTwoQubitsbasiscomp[0.2,0.8] . swapGate;


(* ::Subsection::Closed:: *)
(*Importando datos para esta secci\[OAcute]n*)


(*Ya tenemos la muestra con \[Epsilon] = 0.01. Importemos las dem\[AAcute]s:*)
(*todos traen delta = 0.01*)
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


(*Ac\[AAcute] van los que traen la delta m\[AAcute]s chica (\[Delta] = 0.005) s\[OAcute]lo hay con ciertos errores:*)
samplesDiffErrors\[Delta]p005 = {
 mcsSampleEp0009\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p005/sampleMCS_n=7000_err=0.0009_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp001\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p005/sampleMCS_n=7000_err=0.001_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp0015\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p005/sampleMCS_n=7000_err=0.0015_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp0017\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p005/sampleMCS_n=7000_err=0.0017_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp002\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/delta_p005/sampleMCS_n=7000_err=0.002_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1]
};


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


(* ::Subsection:: *)
(*Gr\[AAcute]ficas de histogramas y esferas*)


(* ::Subsection::Closed:: *)
(*Comportamiento de las fidelidades entre promedios en funci\[OAcute]n del error*)


(* ::Text:: *)
(*Teniendo en cuenta c\[OAcute]mo las fidelidades pueden disminuir si una muestra con \[Delta] y \[Epsilon] incompatibles es ajustada, surge la pregunta: \[DownQuestion]c\[OAcute]mo se comportar\[AAcute] la fidelidad vs error si la delta s\[IAcute] se escoge apropiadamente? Veamos primero las fidelidades de todas las muestras con \[Delta] = 0.01 (en este caso, los errores m\[AAcute]s peque\[NTilde]os resultar\[AAcute]n incompatibles con esta delta):*)


fidsVSerrorData\[Delta]p01 = MapThread[{#1,#2}&, {errors, Map[1-fidelity[exactAvgRp8Pp8, preimageMean[#]]&, samplesDiffErrors\[Delta]p01]}];


ListLogLogPlot[fidsVSerrorData\[Delta]p01, PlotTheme->"Detailed", FrameLabel->{"Error \[Epsilon]", "1 - F [A, \!\(\*SubscriptBox[\(A\), \(mcs\)]\)]"}, PlotLabel->"\[Delta] = 0.01"]


(* ::Text:: *)
(*La fidelidad va mejorando indefinidamente conforme el error va bajando, cosa que no esperar\[IAcute]amos del todo pues por la acumulaci\[OAcute]n de los bordes la fidelidad no deber\[IAcute]a mejorar tanto como se desee; deber\[IAcute]a haber un l\[IAcute]mite para esa mejora. Veamos ahora qu\[EAcute] pasa si para los puntos con \[Epsilon] < 0.003 ponemos las muestras con \[Delta]  = 0.005:*)


fidsVSerrorData\[Delta]p005 = MapThread[{#1,#2}&, {errors, Map[1-fidelity[exactAvgRp8Pp8, preimageMean[#]]&, Join[samplesDiffErrors\[Delta]p005, Drop[samplesDiffErrors\[Delta]p01, 5]]]}];


ListLogLogPlot[fidsVSerrorData\[Delta]p005, PlotTheme->"Detailed", FrameLabel->{"Error \[Epsilon]", "1 - F(A, \!\(\*SubscriptBox[\(A\), \(mcs\)]\))"}, PlotLabel->"\[Delta] = 0.005"]


(* ::Text:: *)
(*Las muestras con \[Delta] = 0.005 muestran peores fidelidades que, adem\[AAcute]s, no parecen mejorar indefinidamente, sino que encuentran algo como un comportamiento estable en valores mayores que 0.005 para 1-F. \[DownQuestion]Qu\[EAcute] pasar\[AAcute] si en lugar de usar las muestras con \[Delta] =0.005, usamos todas las muestras con \[Delta] = 0.01 pero "purgadas"? Veamos:*)


purgeSamplePp8[sample_, error_]:= Select[sample, Norm[coarseGraining2[#,0.8]-targets[[3]],"Frobenius"] <= error &];


samplesDiffErrors\[Delta]p01PURGED = MapThread[purgeSamplePp8[#1, #2]&, {samplesDiffErrors\[Delta]p01, errors}];
fidsVSerrorData\[Delta]p01PURGED = MapThread[{#1,#2}&, {errors, Map[1-fidelity[exactAvgRp8Pp8, preimageMean[#]]&, samplesDiffErrors\[Delta]p01PURGED]}];


ListLogLogPlot[fidsVSerrorData\[Delta]p01PURGED, PlotTheme->"Detailed", FrameLabel->{"Error \[Epsilon]", "1 - F(A, \!\(\*SubscriptBox[\(A\), \(mcs\)]\))"}, PlotLabel->"\[Delta] = 0.01 (purgados)"]


(* ::Text:: *)
(*Veamos ahora los valores de 1-F pero ahora s\[IAcute] con todas las muestras generadas con la misma delta de 0.001*)


fidsVSerrorData\[Delta]p001 = MapThread[{#1,#2}&, {errors2, Map[1-fidelity[exactAvgRp8Pp8, preimageMean[#]]&, samplesDiffErrors\[Delta]p001]}];


ListLogLogPlot[fidsVSerrorData\[Delta]p001, 
  			   PlotTheme->"Scientific",
				 FrameLabel->MaTeX[{"\\text{Error}\, \\epsilon", "1 - F(\\mathcal{A}, \\mathcal{A}_{\\text{MC}})"}], 
				 GridLines->Automatic,
				 FrameStyle->Black]


(*Veamos todo junto*)
generalFidsPlot = ListLogLogPlot[{fidsVSerrorData\[Delta]p005[[7;;14]], fidsVSerrorData\[Delta]p005[[1;;6]], fidsVSerrorData\[Delta]p001},
									PlotTheme->"Scientific",
									GridLines->Automatic,
									FrameLabel->MaTeX[{"\\text{Error}\, (\\epsilon)", "1 - F(\\mathcal{A}, \\mathcal{A}_{\\text{MC}})"}],
									FrameStyle->Black,
									PlotLegends->PointLegend[{"0.01", "0.005", "0.001"}, LegendLabel->\[Delta], LabelStyle->Directive[Black, FontFamily->"Latin Modern Math"]]]


Export["fidelityVSerror_MC.pdf", %81]


(* ::Subsection::Closed:: *)
(*Comportamiento de las fidelidades entre promedios en funci\[OAcute]n de N*)


Map[Length[#]&, samplesDiffErrors\[Delta]p005]


data = Map[Table[1-fidelity[preimageMean[#[[1;;i]]], exactAvgRp8Pp8],{i,5000,50000,5000}]&, samplesErr];
dataGood = Map[MapThread[{#1,#2}&, {Table[i,{i,5000,50000,5000}], #}]&, data];


data2 = Map[Table[1-fidelity[preimageMean[#[[1;;i]]], exactAvgRp8Pp8],{i,5000, 35000, 5000}]&, samplesDiffErrors\[Delta]p005];
dataGood2 = Map[MapThread[{#1,#2}&, {Table[i,{i, 5000, 35000,5000}], #}]&, data2];


ListLogLogPlot[dataGood, PlotTheme->"Detailed", FrameLabel->{"N", "1 - F"}, PlotLegends->errors]


ListLogPlot[dataGood2, PlotTheme->"Detailed", FrameLabel->{"Tama\[NTilde]o (N)", "1 - F(A,\!\(\*SubscriptBox[\(A\), \(mc\)]\))"}, PlotLegends->errors[[1;;5]], Joined->True]


(* ::Section:: *)
(*Promedio de Subscript[r, z]^(A,B) en funci\[OAcute]n de N y de \[Epsilon]*)
