(* ::Package:: *)

(* ::Title:: *)
(*Analizando los casos "patol\[OAcute]gicos" del MCS*)


(* ::Text:: *)
(*Hay diversos casos patol\[OAcute]gicos. Los m\[AAcute]s importantes son con Subscript[r, z] = 0.8 y p = 0.3, 0.8, pues en estos casos los histogramas de puntos MCS tienen picos que parecen contrarios a los de los histogramas brutales. Por ahora, se tienen muchas muestras para p  = 0.8 y s\[OAcute]lo se analizar\[AAcute] este caso; no es de preocuparse, pues el caso con p = 0.3 es muy similar.*)


Needs["Quantum`"]
Get["/media/storage/ciencia/investigacion/proyecto-ss/adanerick/codigo/CoolTools.m"]
Get["/media/storage/ciencia/investigacion/tesis/codigos-tesis/usefulFunctions.wl"]


SetDirectory["/media/storage/ciencia/investigacion/tesis/muestras_MCS"]


(* ::Section::Closed:: *)
(*Importando datos*)


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


Dimensions[brutalMegaSampleRp8Pp8]


Dimensions[mcsMegaSampleRp8Pp8]


mcsMegaSampleRp8Pp8[[1]]


mcsMegaSampleRp8Pp8 = Drop[mcsMegaSampleRp8Pp8, 25351];


199351-174000


(* ::Section::Closed:: *)
(*Error de los puntos de las muestras*)


(*Revisamos los errores de las muestras respecto al edo. target*)
brutalErrorsRp8Pp8 = Map[Norm[coarseGraining2[#,0.8] - targets[[3]], "Frobenius"]&, brutalMegaSampleRp8Pp8];
mcsErrorsRp8Pp8 = Map[Norm[coarseGraining2[#,0.8] - targets[[3]], "Frobenius"]&, mcsMegaSampleRp8Pp8];


Grid[{{ListPlot[mcsErrorsRp8Pp8, PlotLabel->"MCS (\!\(\*SubscriptBox[\(r\), \(z\)]\) = 0.8, p = 0.8)"], ListPlot[brutalErrorsRp8Pp8, PlotLabel->"Brutal (\!\(\*SubscriptBox[\(r\), \(z\)]\) = 0.8, p = 0.8)"]}}]


Show[Histogram[mcsErrorsRp8Pp8, Automatic, "PDF", ChartStyle->Blue], Histogram[brutalErrorsRp8Pp8, Automatic, "PDF", ChartStyle->Opacity[0.3]]]


Grid[{{Histogram[mcsErrorsRp8Pp8, PlotTheme->"Detailed", PlotRange->{{0,0.01},{0,35000}}, PlotLabel->"MCS", FrameLabel->{"Error", "Frecuencia"}, 
				 Epilog -> Inset[Framed[Grid[{
                                       {"Mean =", Mean[mcsErrorsRp8Pp8]},
                                       {"Max =", Max[mcsErrorsRp8Pp8]},
                                       {"Min =", Min[mcsErrorsRp8Pp8]}},
                                        Alignment -> {{Left}}], Background -> White],
                                       {Axis, Center},{Left, Center}]], 
	   Histogram[brutalErrorsRp8Pp8, PlotTheme->"Detailed",PlotRange->{{0,0.01},{0,35000}}, PlotLabel->"Brutal", FrameLabel->{"Error", "Frecuencia"},
	             Epilog -> Inset[Framed[Grid[{
                                       {"Mean =", Mean[brutalErrorsRp8Pp8]},
                                       {"Max =", Max[brutalErrorsRp8Pp8]},
                                       {"Min =", Min[brutalErrorsRp8Pp8]}},
                                        Alignment -> {{Left}}], Background -> White],
                                       {Axis, Center},{Left, Center}]]}}]


(* ::Section::Closed:: *)
(*Histogramas con simetr\[IAcute]a cil\[IAcute]ndrica*)


(*generando histogramas cil\[IAcute]ndr\[IAcute]cos de puntos*)
brutalMegaSampleCylPlots = tripleCylindricalPlot[brutalMegaSampleRp8Pp8, targets[[3]], 100,100,100, {Pink,Yellow}, {0.,1.1}, {-Pi, Pi}, {-0.9, 0.3}, {0., 0.65}];
(*mcsMegaSampleCylPlots = tripleCylindricalPlot[mcsMegaSampleRp8Pp8, targets[[3]], 100,100,100,{Red,Green}, {0.,1.1}, {-Pi, Pi}, {-0.9, 0.3}, {0., 0.65}];*)


(*spherePlotBrutals = visualizeBipartiteSystem[brutalMegaSampleRp8Pp8, {Pink, Yellow}];*)
spherePlotMCS = visualizeBipartiteSystem[mcsMegaSampleRp8Pp8[[1;;50000]]];


Grid[{Join[mcsMegaSampleCylPlots, {spherePlotMCS}],
	  Join[brutalMegaSampleCylPlots, {spherePlotBrutals}]}]


(*entre m\[AAcute]s puntos haya, el MCS no ve muy afectados sus picos. Pero y el brutal?*)
brutalMegaSampleCylPlots1 = tripleCylindricalPlot[brutalMegaSampleRp8Pp8[[1;;34800]], targets[[3]], 100,100,100, {Pink,Yellow}, {0,1.1},{-Pi,Pi},{-0.9,0.3},{0,0.4}];
brutalMegaSampleCylPlots2 = tripleCylindricalPlot[brutalMegaSampleRp8Pp8[[1;;69600]], targets[[3]], 100,100,100, {Pink,Yellow}, {0,1.1},{-Pi,Pi},{-0.9,0.3},{0,0.4}];
brutalMegaSampleCylPlots3 = tripleCylindricalPlot[brutalMegaSampleRp8Pp8[[1;;104400]], targets[[3]], 100,100,100, {Pink,Yellow}, {0,1.1},{-Pi,Pi},{-0.9,0.3},{0,0.4}];
brutalMegaSampleCylPlots4 = tripleCylindricalPlot[brutalMegaSampleRp8Pp8[[1;;139200]], targets[[3]], 100,100,100, {Pink,Yellow}, {0,1.1},{-Pi,Pi},{-0.9,0.3},{0,0.4}];


Grid[{Join[brutalMegaSampleCylPlots1, {Text["N = 34 800"]}],
	  Join[brutalMegaSampleCylPlots2, {Text["N = 69 600"]}],
	  Join[brutalMegaSampleCylPlots3, {Text["N = 104 400"]}],
	  Join[brutalMegaSampleCylPlots4, {Text["N = 139 200"]}],
	  Join[brutalMegaSampleCylPlots, {Text["N = 174 000"]}]
}]


(* ::Section::Closed:: *)
(*Histogramas cartesianos*)


(*Obtengamos los histogramas de las componenetes del vector de Bloch*)
bvsMCSsample = partialtarcesBV[#]& /@ mcsMegaSampleRp8Pp8;
bvsBRsample = partialtarcesBV[#]& /@ brutalMegaSampleRp8Pp8;


xACompsMCSsample = Map[#[[1,1]]&, bvsMCSsample];
xBCompsMCSsample = Map[#[[2,1]]&, bvsMCSsample];
xACompsBRsample = Map[#[[1,1]]&, bvsBRsample];
xBCompsBRsample = Map[#[[2,1]]&, bvsBRsample];


yACompsMCSsample = Map[#[[1,2]]&, bvsMCSsample];
yBCompsMCSsample = Map[#[[2,2]]&, bvsMCSsample];
yACompsBRsample = Map[#[[1,2]]&, bvsBRsample];
yBCompsBRsample = Map[#[[2,2]]&, bvsBRsample];


zACompsMCSsample = Map[#[[1,3]]&, bvsMCSsample];
zBCompsMCSsample = Map[#[[2,3]]&, bvsMCSsample];
zACompsBRsample = Map[#[[1,3]]&, bvsBRsample];
zBCompsBRsample = Map[#[[2,3]]&, bvsBRsample];


Grid[{{Histogram[zACompsMCSsample, 50, PlotTheme->"Detailed", PlotLabel->"Componentes \!\(\*SuperscriptBox[SubscriptBox[\(r\), \(z\)], \(A\)]\) para el MCS", FrameLabel->{"\!\(\*SuperscriptBox[SubscriptBox[\(r\), \(z\)], \(A\)]\)", "Frecuencia"}, 
				 Epilog -> Inset[Framed[Grid[{
                                       {"Mean =", Mean[zACompsMCSsample]},
                                       {"Max =", Max[zACompsMCSsample]},
                                       {"Min =", Min[zACompsMCSsample]}},
                                        Alignment -> {{Left}}], Background -> White],
                                       {Right, Center},{Right, Bottom}]], 
	   Histogram[zACompsBRsample, 50, PlotTheme->"Detailed", PlotLabel->"Componentes \!\(\*SuperscriptBox[SubscriptBox[\(r\), \(z\)], \(A\)]\) para el Brutal", FrameLabel->{"\!\(\*SuperscriptBox[SubscriptBox[\(r\), \(z\)], \(A\)]\)", "Frecuencia"},
	             Epilog -> Inset[Framed[Grid[{
                                       {"Mean =", Mean[zACompsBRsample]},
                                       {"Max =", Max[zACompsBRsample]},
                                       {"Min =", Min[zACompsBRsample]}},
                                        Alignment -> {{Left}}], Background -> White],
                                       {Axis, Top},{Left, Top}]]},
       {Histogram[zBCompsMCSsample, 50, PlotTheme->"Detailed", PlotLabel->"Componentes \!\(\*SuperscriptBox[SubscriptBox[\(r\), \(z\)], \(B\)]\) para el MCS", FrameLabel->{"\!\(\*SuperscriptBox[SubscriptBox[\(r\), \(z\)], \(B\)]\)", "Frecuencia"}, 
				 Epilog -> Inset[Framed[Grid[{
                                       {"Mean =", Mean[zBCompsMCSsample]},
                                       {"Max =", Max[zBCompsMCSsample]},
                                       {"Min =", Min[zBCompsMCSsample]}},
                                        Alignment -> {{Left}}], Background -> White],
                                       {Axis, Center},{Left, Bottom}]], 
	   Histogram[zBCompsBRsample, 50, PlotTheme->"Detailed", PlotLabel->"Componentes \!\(\*SuperscriptBox[SubscriptBox[\(r\), \(z\)], \(B\)]\) para el Brutal", FrameLabel->{"\!\(\*SuperscriptBox[SubscriptBox[\(r\), \(z\)], \(B\)]\)", "Frecuencia"},
	             Epilog -> Inset[Framed[Grid[{
                                       {"Mean =", Mean[zBCompsBRsample]},
                                       {"Max =", Max[zBCompsBRsample]},
                                       {"Min =", Min[zBCompsBRsample]}},
                                        Alignment -> {{Left}}], Background -> White],
                                       {Right, Center},{Right,Bottom}]]}
}]


Grid[{{Histogram[xACompsMCSsample, 50, PlotTheme->"Detailed", PlotLabel->"Componentes \!\(\*SuperscriptBox[SubscriptBox[\(r\), \(x\)], \(A\)]\) para el MCS", FrameLabel->{"\!\(\*SuperscriptBox[SubscriptBox[\(r\), \(x\)], \(A\)]\)", "Frecuencia"}, 
				 Epilog -> Inset[Framed[Grid[{
                                       {"Mean =", Mean[xACompsMCSsample]},
                                       {"Max =", Max[xACompsMCSsample]},
                                       {"Min =", Min[xACompsMCSsample]}},
                                        Alignment -> {{Left}}], Background -> White],
                                       {Right, Center},{Right, Bottom}]], 
	   Histogram[xACompsBRsample, 50, PlotTheme->"Detailed", PlotLabel->"Componentes \!\(\*SuperscriptBox[SubscriptBox[\(r\), \(x\)], \(A\)]\) para el Brutal", FrameLabel->{"\!\(\*SuperscriptBox[SubscriptBox[\(r\), \(x\)], \(A\)]\)", "Frecuencia"},
	             Epilog -> Inset[Framed[Grid[{
                                       {"Mean =", Mean[xACompsBRsample]},
                                       {"Max =", Max[xACompsBRsample]},
                                       {"Min =", Min[xACompsBRsample]}},
                                        Alignment -> {{Left}}], Background -> White],
                                       {Axis, Top},{Left, Top}]]},
       {Histogram[xBCompsMCSsample, 50, PlotTheme->"Detailed", PlotLabel->"Componentes \!\(\*SuperscriptBox[SubscriptBox[\(r\), \(x\)], \(B\)]\) para el MCS", FrameLabel->{"\!\(\*SuperscriptBox[SubscriptBox[\(r\), \(x\)], \(B\)]\)", "Frecuencia"}, 
				 Epilog -> Inset[Framed[Grid[{
                                       {"Mean =", Mean[xBCompsMCSsample]},
                                       {"Max =", Max[xBCompsMCSsample]},
                                       {"Min =", Min[xBCompsMCSsample]}},
                                        Alignment -> {{Left}}], Background -> White],
                                       {Axis, Center},{Left, Bottom}]], 
	   Histogram[xBCompsBRsample, 50, PlotTheme->"Detailed", PlotLabel->"Componentes \!\(\*SuperscriptBox[SubscriptBox[\(r\), \(x\)], \(B\)]\) para el Brutal", FrameLabel->{"\!\(\*SuperscriptBox[SubscriptBox[\(r\), \(x\)], \(B\)]\)", "Frecuencia"},
	             Epilog -> Inset[Framed[Grid[{
                                       {"Mean =", Mean[xBCompsBRsample]},
                                       {"Max =", Max[xBCompsBRsample]},
                                       {"Min =", Min[xBCompsBRsample]}},
                                        Alignment -> {{Left}}], Background -> White],
                                       {Right, Center},{Right,Bottom}]]}
}]


Grid[{{Histogram[yACompsMCSsample, 50, PlotTheme->"Detailed", PlotLabel->"Componentes \!\(\*SuperscriptBox[SubscriptBox[\(r\), \(y\)], \(A\)]\) para el MCS", FrameLabel->{"\!\(\*SuperscriptBox[SubscriptBox[\(r\), \(y\)], \(A\)]\)", "Frecuencia"}, 
				 Epilog -> Inset[Framed[Grid[{
                                       {"Mean =", Mean[yACompsMCSsample]},
                                       {"Max =", Max[yACompsMCSsample]},
                                       {"Min =", Min[yACompsMCSsample]}},
                                        Alignment -> {{Left}}], Background -> White],
                                       {Right, Center},{Right, Bottom}]], 
	   Histogram[yACompsBRsample, 50, PlotTheme->"Detailed", PlotLabel->"Componentes \!\(\*SuperscriptBox[SubscriptBox[\(r\), \(y\)], \(A\)]\) para el Brutal", FrameLabel->{"\!\(\*SuperscriptBox[SubscriptBox[\(r\), \(y\)], \(A\)]\)", "Frecuencia"},
	             Epilog -> Inset[Framed[Grid[{
                                       {"Mean =", Mean[yACompsBRsample]},
                                       {"Max =", Max[yACompsBRsample]},
                                       {"Min =", Min[yACompsBRsample]}},
                                        Alignment -> {{Left}}], Background -> White],
                                       {Axis, Top},{Left, Top}]]},
       {Histogram[yBCompsMCSsample, 50, PlotTheme->"Detailed", PlotLabel->"Componentes \!\(\*SuperscriptBox[SubscriptBox[\(r\), \(y\)], \(B\)]\) para el MCS", FrameLabel->{"\!\(\*SuperscriptBox[SubscriptBox[\(r\), \(y\)], \(B\)]\)", "Frecuencia"}, 
				 Epilog -> Inset[Framed[Grid[{
                                       {"Mean =", Mean[yBCompsMCSsample]},
                                       {"Max =", Max[yBCompsMCSsample]},
                                       {"Min =", Min[yBCompsMCSsample]}},
                                        Alignment -> {{Left}}], Background -> White],
                                       {Axis, Center},{Left, Bottom}]], 
	   Histogram[yBCompsBRsample, 50, PlotTheme->"Detailed", PlotLabel->"Componentes \!\(\*SuperscriptBox[SubscriptBox[\(r\), \(y\)], \(B\)]\) para el Brutal", FrameLabel->{"\!\(\*SuperscriptBox[SubscriptBox[\(r\), \(y\)], \(B\)]\)", "Frecuencia"},
	             Epilog -> Inset[Framed[Grid[{
                                       {"Mean =", Mean[yBCompsBRsample]},
                                       {"Max =", Max[yBCompsBRsample]},
                                       {"Min =", Min[yBCompsBRsample]}},
                                        Alignment -> {{Left}}], Background -> White],
                                       {Right, Center},{Right,Bottom}]]}
}]


(* ::Text:: *)
(*Los promedios de los histogramas coinciden con las componentes correspondientes del vector de Bloch de los estados promedio:*)


mcsAvgBVs = partialtarcesBV[preimageMean[mcsMegaSampleRp8Pp8]]


brutalAvgBVs = partialtarcesBV[preimageMean[brutalMegaSampleRp8Pp8]]


(* ::Text:: *)
(*Veamos cu\[AAcute]les son los vectores de Bloch del estado promedio exacto:*)


exactAvgBVs = partialtarcesBV[swapGate . \[Rho]averageTwoQubitsbasiscomp[0.2, 0.8] . swapGate]


(* ::Section:: *)
(*Comparando distintos valores de error*)


(* ::Text:: *)
(*Hemos visto que aunque se cambie N, los picos del MCS aparcen en el mismo lugar. Creo que los picos aparecen por que son la "orilla" de la regi\[OAcute]n de inter\[EAcute]s \[CapitalOmega]. As\[IAcute], si cambiamos el valor del error (ancho de \[CapitalOmega]), me imagino que cambiar\[AAcute] la posici\[OAcute]n de los picos... Veamos.*)


Directory[]


errors = {0.0009, 0.001, 0.0015, 0.0017, 0.002, 0.003, 0.006, 0.008, 0.01, 0.02, 0.03, 0.06, 0.1, 0.2}; 
exactAvgRp8Pp8 = swapGate . \[Rho]averageTwoQubitsbasiscomp[0.2,0.8] . swapGate;


(*Ya tenemos la muestra con \[Epsilon] = 0.01. Importemos las dem\[AAcute]s:*)
(*todos traen delta = 0.01*)
samplesDiffErrors\[Delta]p01 ={
 mcsSampleEp0009 = Flatten[Map[Get["distintasEnes_Rp8Pp8/sampleMCS_n=7000_err=0.0009_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp001 = Flatten[Map[Get["distintasEnes_Rp8Pp8/sampleMCS_n=10000_err=0.001_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp0015 = Flatten[Map[Get["distintasEnes_Rp8Pp8/sampleMCS_n=7000_err=0.0015_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp0017 = Flatten[Map[Get["distintasEnes_Rp8Pp8/sampleMCS_n=7000_err=0.0017_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp002 = Flatten[Map[Get["distintasEnes_Rp8Pp8/sampleMCS_n=7000_err=0.002_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp003 = Flatten[Map[Get["distintasEnes_Rp8Pp8/sampleMCS_n=10000_err=0.003_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp006 = Flatten[Map[Get["distintasEnes_Rp8Pp8/sampleMCS_n=10000_err=0.006_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp008 = Flatten[Map[Get["distintasEnes_Rp8Pp8/sampleMCS_n=10000_err=0.008_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp01 = Flatten[Map[Get["distintasEnes_Rp8Pp8/sampleMCS_n=10000_err=0.01_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]], 1],
 mcsSampleEp02 = Flatten[Map[Get["distintasEnes_Rp8Pp8/sampleMCS_n=10000_err=0.02_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp03 = Flatten[Map[Get["distintasEnes_Rp8Pp8/sampleMCS_n=10000_err=0.03_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp06 = Flatten[Map[Get["distintasEnes_Rp8Pp8/sampleMCS_n=10000_err=0.06_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp1 = Flatten[Map[Get["distintasEnes_Rp8Pp8/sampleMCS_n=10000_err=0.1_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp2 = Flatten[Map[Get["distintasEnes_Rp8Pp8/sampleMCS_n=10000_err=0.2_delta=0.01_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1]
};


(*Ac\[AAcute] van los que traen la delta m\[AAcute]s chica (\[Delta] = 0.005)*)
samplesDiffErrors\[Delta]p005 = {
 mcsSampleEp0009\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/sampleMCS_n=7000_err=0.0009_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp001\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/sampleMCS_n=7000_err=0.001_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp0015\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/sampleMCS_n=7000_err=0.0015_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp0017\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/sampleMCS_n=7000_err=0.0017_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1],
 mcsSampleEp002\[Delta]p005 = Flatten[Map[Get["distintasEnes_Rp8Pp8/sampleMCS_n=7000_err=0.002_delta=0.005_t=0.3_rz=0.8_p=0.8_"<>ToString[#]<>".wl"][[2]]&, Range[5]],1]
};


spherePlotMCSEp001 = visualizeBipartiteSystem[mcsSampleEp001];
spherePlotMCSEp003 = visualizeBipartiteSystem[mcsSampleEp003];
spherePlotMCSEp006 = visualizeBipartiteSystem[mcsSampleEp006];
spherePlotMCSEp008 = visualizeBipartiteSystem[mcsSampleEp008];
spherePlotMCSEp02 = visualizeBipartiteSystem[mcsSampleEp02];
spherePlotMCSEp03 = visualizeBipartiteSystem[mcsSampleEp03];
spherePlotMCSEp06 = visualizeBipartiteSystem[mcsSampleEp06];
spherePlotMCSEp1 = visualizeBipartiteSystem[mcsSampleEp1];
spherePlotMCSEp2 = visualizeBipartiteSystem[mcsSampleEp2];


cylPlotsMCSEp001 = tripleCylindricalPlot[mcsSampleEp001, targets[[3]], 100,100,100, {Red,Green},  {0.,1.1}, {-Pi, Pi}, {-1, 0.3}, {0., 0.65}];
cylPlotsMCSEp003 = tripleCylindricalPlot[mcsSampleEp003, targets[[3]], 100,100,100, {Red,Green},  {0.,1.1}, {-Pi, Pi}, {-1, 0.3}, {0., 0.65}];
cylPlotsMCSEp006 = tripleCylindricalPlot[mcsSampleEp006, targets[[3]], 100,100,100, {Red,Green},  {0.,1.1}, {-Pi, Pi}, {-1, 0.3}, {0., 0.65}];
cylPlotsMCSEp008 = tripleCylindricalPlot[mcsSampleEp008, targets[[3]], 100,100,100, {Red,Green},  {0.,1.1}, {-Pi, Pi}, {-1, 0.3}, {0., 0.65}];
cylPlotsMCSEp01 = tripleCylindricalPlot[mcsMegaSampleRp8Pp8[[1;;50000]], targets[[3]], 100,100,100, {Red,Green},  {0.,1.1}, {-Pi, Pi}, {-1, 0.3}, {0., 0.65}];
cylPlotsMCSEp02 = tripleCylindricalPlot[mcsSampleEp02, targets[[3]], 100,100,100, {Red,Green},  {0.,1.1}, {-Pi, Pi}, {-1, 0.3}, {0., 0.65}];
cylPlotsMCSEp03 = tripleCylindricalPlot[mcsSampleEp03, targets[[3]], 100,100,100, {Red,Green},  {0.,1.1}, {-Pi, Pi}, {-1, 0.3}, {0., 0.65}];
cylPlotsMCSEp06 = tripleCylindricalPlot[mcsSampleEp06, targets[[3]], 100,100,100, {Red,Green},  {0.,1.1}, {-Pi, Pi}, {-2, 0.3}, {0., 0.65}];
cylPlotsMCSEp1 = tripleCylindricalPlot[mcsSampleEp1, targets[[3]], 100,100,100, {Red,Green},  {0.,1.1}, {-Pi, Pi}, {-2, 0.3}, {0., 0.65}];
cylPlotsMCSEp2 = tripleCylindricalPlot[mcsSampleEp2, targets[[3]], 100,100,100, {Red,Green},  {0.,1.1}, {-Pi, Pi}, {-2, 0.3}, {0., 0.65}];


Grid[{Join[{Text["\[Epsilon] = 0.001"]}, cylPlotsMCSEp001, {spherePlotMCSEp001}],
	  Join[{Text["\[Epsilon] = 0.003"]}, cylPlotsMCSEp003, {spherePlotMCSEp003}],
	  Join[{Text["\[Epsilon] = 0.006"]}, cylPlotsMCSEp006, {spherePlotMCSEp006}],
	  Join[{Text["\[Epsilon] = 0.008"]}, cylPlotsMCSEp008, {spherePlotMCSEp008}],
	  Join[{Text["\[Epsilon] = 0.01"]}, cylPlotsMCSEp01, {spherePlotMCS}],
	  Join[{Text["\[Epsilon] = 0.02"]}, cylPlotsMCSEp02, {spherePlotMCSEp02}],
	  Join[{Text["\[Epsilon] = 0.03"]}, cylPlotsMCSEp03, {spherePlotMCSEp03}],
	  Join[{Text["\[Epsilon] = 0.06"]}, cylPlotsMCSEp06, {spherePlotMCSEp06}],
	  Join[{Text["\[Epsilon] = 0.1"]}, cylPlotsMCSEp1, {spherePlotMCSEp1}],
	  Join[{Text["\[Epsilon] = 0.2"]}, cylPlotsMCSEp2, {spherePlotMCSEp2}] 
}]


Show[{#[[3]],brutalMegaSampleCylPlots[[3]]}]&/@cylPlots


(* ::Text:: *)
(*Parece que, entre m\[AAcute]s grande sea el error, los picos radiales se mantienen en el mismo lugar, pero los verdes se reducen de tama\[NTilde]o y los rojos aumentan. Asimismo, entre m\[AAcute]s grande sea el error, los picos de Z parece que se mantienen del mismo tama\[NTilde]o (no estoy seguro) pero los rojos se deplazan a valores cada vez menores. *)


fidsVSerror = Map[1-fidelity[exactAvgRp8Pp8, preimageMean[#]]&, samplesDiffErrors\[Delta]p01];


fidsVSerrorData = MapThread[{#1,#2}&, {errors, fidsVSerror}];


ListLogLogPlot[fidsVSerrorData, PlotTheme->"Detailed", FrameLabel->{"Error \[Epsilon]", "1 - F [A, \!\(\*SubscriptBox[\(A\), \(mcs\)]\)]"}, PlotLabel->"\[Delta] = 0.01"]


(* ::Text:: *)
(*Veamos ahora qu\[EAcute] pasa si para los puntos con \[Epsilon] < 0.003 ponemos las muestras con \[Delta]  = 0.005:*)


fidsVSerrorData\[Delta]p005 = MapThread[{#1,#2}&, {errors, Map[1-fidelity[exactAvgRp8Pp8, preimageMean[#]]&, Join[samplesDiffErrors\[Delta]p005, Drop[samplesDiffErrors\[Delta]p01, 5]]]}];


ListLogLogPlot[fidsVSerrorData\[Delta]p005, PlotTheme->"Detailed", FrameLabel->{"Error \[Epsilon]", "1 - F(A, \!\(\*SubscriptBox[\(A\), \(mcs\)]\))"}, PlotLabel->"\[Delta] = 0.005"]


(* ::Text:: *)
(*Ahora veamos c\[OAcute]mo se comporta 1-F en funci\[OAcute]n de N, para los distintos valores del error*)


data = Map[Table[1-fidelity[preimageMean[#[[1;;i]]], exactAvgRp8Pp8],{i,5000,50000,5000}]&, samplesErr];


dataGood = Map[MapThread[{#1,#2}&, {Table[i,{i,5000,50000,5000}], #}]&, data];


ListLogLogPlot[dataGood, PlotTheme->"Detailed", FrameLabel->{"N", "1 - F"}, PlotLegends->errors]


(* ::Section::Closed:: *)
(*Promedio de Subscript[r, z]^(A,B) en funci\[OAcute]n de N y de \[Epsilon]*)


(* ::Text:: *)
(*Ya vimos que el promedio de Subscript[r, z]^(A,B) corresponde al correspondiente valor del estado promedio. Veamos c\[OAcute]mo cambia con el valor de \[Epsilon] y con el de N*)


(* ::Subsection:: *)
(*En funci\[OAcute]n de N*)


(* ::Text:: *)
(*Tomemos el caso con mayor cantidad de muestras: \[Epsilon] = 0.01*)


enes = Join[Table[10000 i, {i,19}], {199351}]


zACompsDistN = Map[Mean[Map[partialtarcesBV[#][[1,3]]&, mcsMegaSampleRp8Pp8[[1;;#]]]]&, enes];
zBCompsDistN = Map[Mean[Map[partialtarcesBV[#][[2,3]]&, mcsMegaSampleRp8Pp8[[1;;#]]]]&, enes];


(*Rearranging data*)
zACompsDistNdata = MapThread[{#1, #2}&, {enes, zACompsDistN}];
zBCompsDistNdata = MapThread[{#1, #2}&, {enes, zBCompsDistN}];


partialtarcesBV[swapGate . \[Rho]averageTwoQubitsbasiscomp[0.2,0.8] . swapGate]


Show[ListPlot[{zACompsDistNdata, zBCompsDistNdata}, PlotTheme->"Detailed", PlotLegends->{Mean[Subscript[r, z]^A], Mean[Subscript[r, z]^B]}, FrameLabel->{"N", "Promedio de \!\(\*SubscriptBox[\(r\), \(z\)]\)"}], 
	Plot[{0.475, 0.88125},{x,0,200000}]]


(* ::Text:: *)
(*Parece que no depende de N*)


(* ::Subsection:: *)
(*En funci\[OAcute]n del error \[Epsilon]*)


errors = {0.008, 0.01, 0.02, 0.03, 0.06, 0.1, 0.2};


zACompsDist\[Epsilon] = Map[Mean[Map[partialtarcesBV[#][[1,3]]&, #]]&, 
				   {mcsSampleEp008, mcsMegaSampleRp8Pp8[[1;;50000]], mcsSampleEp02, mcsSampleEp03, mcsSampleEp06, mcsSampleEp1, mcsSampleEp2}];
zBCompsDist\[Epsilon] = Map[Mean[Map[partialtarcesBV[#][[2,3]]&, #]]&, 
				   {mcsSampleEp008, mcsMegaSampleRp8Pp8[[1;;50000]], mcsSampleEp02, mcsSampleEp03, mcsSampleEp06, mcsSampleEp1, mcsSampleEp2}];


(*Re arranging data*)
zACompsDist\[Epsilon]data = MapThread[{#1, #2}&, {errors, zACompsDist\[Epsilon]}];
zBCompsDist\[Epsilon]data = MapThread[{#1, #2}&, {errors, zBCompsDist\[Epsilon]}];


Show[ListLogLogPlot[{zACompsDist\[Epsilon]data, zBCompsDist\[Epsilon]data}, PlotTheme->"Detailed", PlotLegends->{Mean[Subscript[r, z]^A], Mean[Subscript[r, z]^B]}, FrameLabel->{"Error \[Epsilon]", "Promedio de \!\(\*SubscriptBox[\(r\), \(z\)]\)"}], 
	LogLogPlot[{0.475, 0.88125},{x,0,0.2}]]


Show[visualizeBipartiteSystem[mcsSampleEp008[[1;;3000]]], visualizeBipartiteSystem[{preimageMean[mcsSampleEp008]}, {Pink,Yellow}], 
visualizeBipartiteSystem[{swapGate . \[Rho]averageTwoQubitsbasiscomp[0.2,0.8] . swapGate}, {Brown, White}]]
