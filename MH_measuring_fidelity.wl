(* ::Package:: *)

(* ::Title:: *)
(*Midiendo la fidelidad*)


(* ::Text:: *)
(*Hasta ahora hemos tratado de optimizar los valores de los par\[AAcute]metros \[Beta], \[Delta] y N de tal manera que arrojen el menor tiempo de c\[OAcute]mputo posible con la restricci\[OAcute]n sobre la ergodicidad. Pero, qu\[EAcute] pasa si en vez de la ergodicidad, usamos la fidelidad entre estado promedio del MH y el estado promedio exacto?*)


Get["/media/storage/ciencia/investigacion/tesis/codigos-tesis/CoolTools2.m"]
Get["/media/storage/ciencia/investigacion/tesis/codigos-tesis/usefulFunctions.wl"]
<<"MaTeX`"


(* ::Section::Closed:: *)
(*Preliminares*)


SetDirectory["/media/storage/ciencia/investigacion/tesis/mh_muestras_GOOD/distintas_Nbetasydeltas_Rp5Pp3_DOS/averages"]


deltas = {0.005, 0.01, 0.03, 0.05, 0.07, 0.1, 0.5, 1, 5, 10};
betas = {100, 250, 400, 600, 750, 1000};
enes = {10000, 20000, 40000, 60000, 80000, 100000, 200000, 350000, 500000, 1000000};


colors=(("DefaultPlotStyle"/.(Method /. 
     Charting`ResolvePlotTheme["Scientific" ,  ListLogLogPlot]))/. Directive[x_,__]:>x)


(*Funci\[OAcute]n para importar los edos promedio:*)
getAvgStatesForASingle\[Delta][\[Delta]_, betas_, rz_, p_]:= 
Map[Get["avgs_delta="<>ToString[\[Delta]]<>"_beta="<>ToString[#]<>"_rz="<>ToString[rz]<>"_p="<>ToString[p]<>"_allN.wl"]&, betas];


(*Importando los edos promedio para todas las betas:*)
avgStates\[Delta]1 = getAvgStatesForASingle\[Delta][deltas[[1]], betas, 0.5, 0.3];
avgStates\[Delta]2 = getAvgStatesForASingle\[Delta][deltas[[2]], betas, 0.5, 0.3];
avgStates\[Delta]3 = getAvgStatesForASingle\[Delta][deltas[[3]], betas, 0.5, 0.3];
avgStates\[Delta]4 = getAvgStatesForASingle\[Delta][deltas[[4]], betas, 0.5, 0.3];
(*avgStates\[Delta]5 = getAvgStatesForASingle\[Delta][deltas[[5]], betas, 0.5, 0.3];*)
avgStates\[Delta]6 = getAvgStatesForASingle\[Delta][deltas[[6]], betas, 0.5, 0.3];
avgStates\[Delta]7 = getAvgStatesForASingle\[Delta][deltas[[7]], betas, 0.5, 0.3];
(*avgStates\[Delta]8 = getAvgStatesForASingle\[Delta][deltas[[8]], betas, 0.5, 0.3];
avgStates\[Delta]9 = getAvgStatesForASingle\[Delta][deltas[[9]], betas, 0.5, 0.3];
avgStates\[Delta]10 = getAvgStatesForASingle\[Delta][deltas[[10]], betas, 0.5, 0.3];*)


(*Estado promedio brutal correspondiente a Subscript[r, z] =0.5 y p = 0.3, par\[AAcute]metros con los que se tienen todos los datos calculados hasta el momento*)
avgRp5Pp3BRUTAL = preimageMean[ketsToDensity[Get["/media/storage/ciencia/investigacion/adanerick/muestras_de_edos/pureBrutalStates/pureHaarStates_n=10000_p=0.3_rz=0.5.m"]]];


(*Calculando las fidelidades*)
fids\[Delta]1 = Map[1-fidelity[avgRp5Pp3BRUTAL, #]&, avgStates\[Delta]1, {2}];
fids\[Delta]2 = Map[1-fidelity[avgRp5Pp3BRUTAL, #]&, avgStates\[Delta]2, {2}];
fids\[Delta]3 = Map[1-fidelity[avgRp5Pp3BRUTAL, #]&, avgStates\[Delta]3, {2}];
fids\[Delta]4 = Map[1-fidelity[avgRp5Pp3BRUTAL, #]&, avgStates\[Delta]4, {2}];
(*fids\[Delta]5 = Map[1-fidelity[avgRp5Pp3BRUTAL, #]&, avgStates\[Delta]5, {2}];*)
fids\[Delta]6 = Map[1-fidelity[avgRp5Pp3BRUTAL, #]&, avgStates\[Delta]6, {2}];
fids\[Delta]7 = Map[1-fidelity[avgRp5Pp3BRUTAL, #]&, avgStates\[Delta]7, {2}];
(*fids\[Delta]8 = Map[1-fidelity[avgRp5Pp3BRUTAL, #]&, avgStates\[Delta]8, {2}];
fids\[Delta]9 = Map[1-fidelity[avgRp5Pp3BRUTAL, #]&, avgStates\[Delta]9, {2}];
fids\[Delta]10 = Map[1-fidelity[avgRp5Pp3BRUTAL, #]&, avgStates\[Delta]10, {2}];*)


(* ::Section:: *)
(*Fidelidad en funci\[OAcute]n de N*)


(*arranging data for fidelities*)
fidelityData\[Delta]1 = Map[MapThread[{#1, #2}&, {enes, #}]&, fids\[Delta]1];
fidelityData\[Delta]2 = Map[MapThread[{#1, #2}&, {enes, #}]&, fids\[Delta]2];
fidelityData\[Delta]3 = Map[MapThread[{#1, #2}&, {enes, #}]&, fids\[Delta]3];
fidelityData\[Delta]4 = Map[MapThread[{#1, #2}&, {enes, #}]&, fids\[Delta]4];
(*fidelityData\[Delta]5 = Map[MapThread[{#1, #2}&, {enes, #}]&, fids\[Delta]5];*)
fidelityData\[Delta]6 = Map[MapThread[{#1, #2}&, {enes, #}]&, fids\[Delta]6];
fidelityData\[Delta]7 = Map[MapThread[{#1, #2}&, {enes[[1;;9]], #}]&, fids\[Delta]7];
(*fidelityData\[Delta]8 = Map[MapThread[{#1, #2}&, {enes, #}]&, fids\[Delta]8];
fidelityData\[Delta]9 = Map[MapThread[{#1, #2}&, {enes, #}]&, fids\[Delta]9];
fidelityData\[Delta]10 = Map[MapThread[{#1, #2}&, {enes, #}]&, fids\[Delta]10];*)


allData = {fidelityData\[Delta]1, fidelityData\[Delta]2, fidelityData\[Delta]3 ,fidelityData\[Delta]4, fidelityData\[Delta]6, fidelityData\[Delta]7};


fidVSergLegend = LineLegend[colors, MaTeX[betas], LegendLabel->MaTeX["\\beta", Preamble->{"\\usepackage{newtxmath}"}]];


fidelityVSnPlots = MapThread[ListLogLogPlot[#1, PlotLabel->MaTeX["\\delta = "<>ToString[#2], Preamble->{"\\usepackage{newtxmath}"}], PlotRange->{0.0001, 1},
											    PlotTheme->"Scientific", Joined->True, Mesh->All, MeshStyle->PointSize[0.015], GridLines->Automatic,
											    FrameLabel->MaTeX[{"\\text{Iteraciones}\\,\\,(N)", "1-F(\\mathcal{A}_\\text{brutal}, \\mathcal{A}_\\text{MH})"}]]&, 
                             {allData, deltas[[1;;4]]~Join~deltas[[6;;7]]}];


Grid[{fidelityVSnPlots[[1;;2]]~Join~{fidVSergLegend},
	  fidelityVSnPlots[[3;;4]]~Join~{fidVSergLegend},
	  fidelityVSnPlots[[5;;]]~Join~{fidVSergLegend}
}, ItemStyle->ImageSizeMultipliers->1, Spacings->{1, 1}]


SetDirectory["/media/storage/ciencia/investigacion/tesis/mh_muestras_GOOD"]


Export["fidsVSn_Rp5Pp3.pdf", %94]


(* ::Section:: *)
(*Ergodicidad y fidelidad*)


SetDirectory["/media/storage/ciencia/investigacion/tesis/mh_muestras_GOOD/distintas_Nbetasydeltas_Rp5Pp3_DOS/minvecs"]


(*datos de ergodicidad*)
minVecs\[Delta]1 = Get["minVecs_beta="<>ToString[#]<>"_delta=0.005_rz=0.5_p=0.3_allN.wl"]& /@ betas;
minVecs\[Delta]2 = Get["minVecs_beta="<>ToString[#]<>"_delta=0.01_rz=0.5_p=0.3_allN.wl"]& /@ betas;
minVecs\[Delta]3 = Get["minVecs_beta="<>ToString[#]<>"_delta=0.03_rz=0.5_p=0.3_allN.wl"]& /@ betas;
minVecs\[Delta]4 = Get["minVecs_beta="<>ToString[#]<>"_delta=0.05_rz=0.5_p=0.3_allN.wl"]& /@ betas;
(*minVecs\[Delta]5 = Get["minVecs_beta="<>ToString[#]<>"_delta=0.07_rz=0.5_p=0.3_allN.wl"]& /@ betas;*)
minVecs\[Delta]6 = Get["minVecs_beta="<>ToString[#]<>"_delta=0.1_rz=0.5_p=0.3_allN.wl"]& /@ betas;
minVecs\[Delta]7 = Get["minVecs_beta="<>ToString[#]<>"_delta=0.5_rz=0.5_p=0.3_allN.wl"]& /@ betas;
(*minVecs\[Delta]8 = Get["minVecs_beta="<>ToString[#]<>"_delta=1_rz=0.5_p=0.3_allN.wl"]& /@ betas;
minVecs\[Delta]9 = Get["minVecs_beta="<>ToString[#]<>"_delta=5_rz=0.5_p=0.3_allN.wl"]& /@ betas;
minVecs\[Delta]10 = Get["minVecs_beta="<>ToString[#]<>"_delta=10_rz=0.5_p=0.3_allN.wl"]& /@ betas;*)


ergs\[Delta]1 = Map[Map[ergodicityMeasure2[#]&, #]&, minVecs\[Delta]1];
ergs\[Delta]2 = Map[Map[ergodicityMeasure2[#]&, #]&, minVecs\[Delta]2];
ergs\[Delta]3 = Map[Map[ergodicityMeasure2[#]&, #]&, minVecs\[Delta]3];
ergs\[Delta]4 = Map[Map[ergodicityMeasure2[#]&, #]&, minVecs\[Delta]4];
(*ergs\[Delta]5 = Map[Map[ergodicityMeasure2[#]&, #]&, minVecs\[Delta]5];*)
ergs\[Delta]6 = Map[Map[ergodicityMeasure2[#]&, #]&, minVecs\[Delta]6];
ergs\[Delta]7 = Map[Map[ergodicityMeasure2[#]&, #]&, minVecs\[Delta]7];
(*ergs\[Delta]8 = Map[Map[ergodicityMeasure2[#]&, #]&, minVecs\[Delta]8];
ergs\[Delta]9 = Map[Map[ergodicityMeasure2[#]&, #]&, minVecs\[Delta]9];
ergs\[Delta]10 = Map[Map[ergodicityMeasure2[#]&, #]&, minVecs\[Delta]10];*)


data\[Delta]1 = MapThread[MapThread[{#1, #2}&, {#1, #2[[1;;9]]}]&, {ergs\[Delta]1, fids\[Delta]1}];
data\[Delta]2 = MapThread[MapThread[{#1, #2}&, {#1, #2[[1;;9]]}]&, {ergs\[Delta]2, fids\[Delta]2}];
data\[Delta]3 = MapThread[MapThread[{#1, #2}&, {#1, #2[[1;;9]]}]&, {ergs\[Delta]3, fids\[Delta]3}];
data\[Delta]4 = MapThread[MapThread[{#1, #2}&, {#1, #2[[1;;9]]}]&, {ergs\[Delta]4, fids\[Delta]4}];
(*data\[Delta]5 = MapThread[MapThread[{#1, #2}&, {#1, #2[[1;;9]]}]&, {ergs\[Delta]5, fids\[Delta]5}];*)
data\[Delta]6 = MapThread[MapThread[{#1, #2}&, {#1, #2[[1;;9]]}]&, {ergs\[Delta]6, fids\[Delta]6}];
data\[Delta]7 = MapThread[MapThread[{#1, #2}&, {#1, #2[[1;;9]]}]&, {ergs\[Delta]7, fids\[Delta]7}];
(*data\[Delta]8 = MapThread[MapThread[{#1, #2}&, {#1, #2[[1;;9]]}]&, {ergs\[Delta]8, fids\[Delta]8}];
data\[Delta]9 = MapThread[MapThread[{#1, #2}&, {#1, #2[[1;;9]]}]&, {ergs\[Delta]9, fids\[Delta]9}];
data\[Delta]10 = MapThread[MapThread[{#1, #2}&, {#1, #2[[1;;9]]}]&, {ergs\[Delta]10, fids\[Delta]10}];*)


fidelityVSergPlots = MapThread[ListLogLogPlot[#1, PlotLabel->MaTeX["\\delta = "<>ToString[#2], Preamble->{"\\usepackage{newtxmath}"}], 
										  PlotLegends->PointLegend[MaTeX[betas], LegendLabel->MaTeX["\\beta", Preamble->{"\\usepackage{newtxmath}"}]], 
										  PlotTheme->"Scientific", GridLines->Automatic, PlotRange->Full, PlotStyle->PointSize[0.014],
										  FrameLabel->{MaTeX["\\text{Antiergodicidad}\\,\\,(\\tilde{E}_1)", Preamble->{"\\usepackage{newtxmath}"}], MaTeX["1-F(\\mathcal{A}_\\text{brutal}, \\mathcal{A}_\\text{MH})"]}]&,
		  {{data\[Delta]1, data\[Delta]2, data\[Delta]3, data\[Delta]4, data\[Delta]6, data\[Delta]7}, deltas[[1;;4]]~Join~deltas[[6;;7]]}];


Grid[{fidelityVSergPlots[[1;;2]],
	  fidelityVSergPlots[[3;;4]],
	  fidelityVSergPlots[[5;;6]]
}, ItemStyle->ImageSizeMultipliers->1, Spacings->{1, 1}]


SetDirectory["/media/storage/ciencia/investigacion/tesis/mh_muestras_GOOD"]


Export["fidVSerg_deltap03_Rp5Pp3.pdf", fidelityVSergPlots[[3]]]


(* ::Section::Closed:: *)
(*Fidelidad en funci\[OAcute]n de delta*)


avgStates\[Beta]1 = Map[Get["avgs_delta="<>ToString[#]<>"_beta=100_rz=0.5_p=0.3_allN.wl"]&, deltas];
avgStates\[Beta]2 = Map[Get["avgs_delta="<>ToString[#]<>"_beta=250_rz=0.5_p=0.3_allN.wl"]&, deltas];
avgStates\[Beta]3 = Map[Get["avgs_delta="<>ToString[#]<>"_beta=400_rz=0.5_p=0.3_allN.wl"]&, deltas];
avgStates\[Beta]4 = Map[Get["avgs_delta="<>ToString[#]<>"_beta=600_rz=0.5_p=0.3_allN.wl"]&, deltas];
avgStates\[Beta]5 = Map[Get["avgs_delta="<>ToString[#]<>"_beta=750_rz=0.5_p=0.3_allN.wl"]&, deltas];
avgStates\[Beta]6 = Map[Get["avgs_delta="<>ToString[#]<>"_beta=1000_rz=0.5_p=0.3_allN.wl"]&, deltas];


(*Calculando las fidelidades*)
fids\[Beta]1 = Map[1-fidelity[avgRp5Pp3BRUTAL, #]&, avgStates\[Beta]1, {2}]//Transpose;
fids\[Beta]2 = Map[1-fidelity[avgRp5Pp3BRUTAL, #]&, avgStates\[Beta]2, {2}]//Transpose;
fids\[Beta]3 = Map[1-fidelity[avgRp5Pp3BRUTAL, #]&, avgStates\[Beta]3, {2}]//Transpose;
fids\[Beta]4 = Map[1-fidelity[avgRp5Pp3BRUTAL, #]&, avgStates\[Beta]4, {2}]//Transpose;
fids\[Beta]5 = Map[1-fidelity[avgRp5Pp3BRUTAL, #]&, avgStates\[Beta]5, {2}]//Transpose;
fids\[Beta]6 = Map[1-fidelity[avgRp5Pp3BRUTAL, #]&, avgStates\[Beta]6, {2}]//Transpose;


(*arranging data for fidelities*)
fidelityData\[Beta]1 = Map[MapThread[{#1, #2}&, {deltas, #}]&, fids\[Beta]1];
fidelityData\[Beta]2 = Map[MapThread[{#1, #2}&, {deltas, #}]&, fids\[Beta]2];
fidelityData\[Beta]3 = Map[MapThread[{#1, #2}&, {deltas, #}]&, fids\[Beta]3];
fidelityData\[Beta]4 = Map[MapThread[{#1, #2}&, {deltas, #}]&, fids\[Beta]4];
fidelityData\[Beta]5 = Map[MapThread[{#1, #2}&, {deltas, #}]&, fids\[Beta]5];
fidelityData\[Beta]6 = Map[MapThread[{#1, #2}&, {deltas, #}]&, fids\[Beta]6];


fidelityVSdeltaPlots = MapThread[ListLogLogPlot[#1, PlotLabel->"\[Beta] = "<>ToString[#2], PlotTheme->"Scientific", GridLines->Automatic,
				                          Joined->True, Mesh->All, MeshStyle->PointSize[0.015], FrameLabel->{"\[Delta]", "1-F"}, PlotRange->All]&, 
		                        {{fidelityData\[Beta]1, fidelityData\[Beta]2, fidelityData\[Beta]3, fidelityData\[Beta]4, fidelityData\[Beta]5, fidelityData\[Beta]6}, betas}];


colors=(("DefaultPlotStyle"/.(Method /. 
     Charting`ResolvePlotTheme["Scientific" ,  ListLogLogPlot]))/. Directive[x_,__]:>x);


Grid[{fidelityVSdeltaPlots[[1;;2]]~Join~{LineLegend[colors, enes]},
	  fidelityVSdeltaPlots[[3;;4]],
	  fidelityVSdeltaPlots[[5;;]]
}, ItemStyle->ImageSizeMultipliers->1, Spacings->{1, 1}]


(* ::Section::Closed:: *)
(*Tasa de aceptaci\[OAcute]n vs fidelidad*)


SetDirectory["/media/storage/ciencia/investigacion/tesis/mh_muestras_GOOD/distintas_Nbetasydeltas_Rp5Pp3_DOS/accrates"]


accRates\[Beta]1 = N[Get["accRates_beta=100_allDeltas_allN_rz=0.5_p=0.3.wl"][[10]]];
accRates\[Beta]2 = N[Get["accRates_beta=250_allDeltas_allN_rz=0.5_p=0.3.wl"][[10]]];
accRates\[Beta]3 = N[Get["accRates_beta=400_allDeltas_allN_rz=0.5_p=0.3.wl"][[10]]];
accRates\[Beta]4 = N[Get["accRates_beta=600_allDeltas_allN_rz=0.5_p=0.3.wl"][[10]]];
accRates\[Beta]5 = N[Get["accRates_beta=750_allDeltas_allN_rz=0.5_p=0.3.wl"][[10]]];
accRates\[Beta]6 = N[Get["accRates_beta=1000_allDeltas_allN_rz=0.5_p=0.3.wl"][[10]]];


accRatesData\[Beta]1 = Map[MapThread[{#1, #2}&, {accRates\[Beta]1, #}]&, fids\[Beta]1];
accRatesData\[Beta]2 = Map[MapThread[{#1, #2}&, {accRates\[Beta]2, #}]&, fids\[Beta]2];
accRatesData\[Beta]3 = Map[MapThread[{#1, #2}&, {accRates\[Beta]3, #}]&, fids\[Beta]3];
accRatesData\[Beta]4 = Map[MapThread[{#1, #2}&, {accRates\[Beta]4, #}]&, fids\[Beta]4];
accRatesData\[Beta]5 = Map[MapThread[{#1, #2}&, {accRates\[Beta]5, #}]&, fids\[Beta]5];
accRatesData\[Beta]6 = Map[MapThread[{#1, #2}&, {accRates\[Beta]6, #}]&, fids\[Beta]6];


fidVSratePlots = MapThread[ListLogLogPlot[#1, PlotLabel->"\[Beta] = "<>ToString[#2], PlotTheme->"Scientific", GridLines->Automatic,
									FrameLabel->{"Tasa de aceptaci\[OAcute]n", "1-F"}, Joined->True, Mesh->All]&, 
		                  {{accRatesData\[Beta]1, accRatesData\[Beta]2, accRatesData\[Beta]3, accRatesData\[Beta]4, accRatesData\[Beta]5, accRatesData\[Beta]6}, betas}];


Grid[{fidVSratePlots[[1;;2]]~Join~{PointLegend[colors, enes]},
	  fidVSratePlots[[3;;4]],
	  fidVSratePlots[[5;;]]
}, ItemStyle->ImageSizeMultipliers->1, Spacings->{1, 1}]


(* ::Section::Closed:: *)
(*Fidelidades caso tripartito*)


(*Los brutales*)
SetDirectory["/media/storage/ciencia/investigacion/tesis/mh_muestras_GOOD/final_samples_3qubits/brutals"];
avgR0Pp33BRUTAL = preimageMean[Get["brutals_N=10000_error=0.01_rz=0_p=0.33.wl"][[2]]];
avgRp1Pp33BRUTAL = preimageMean[Get["brutals_N=10000_error=0.01_rz=0.1_p=0.33.wl"][[2]]];


(*Los MH*)
SetDirectory["/media/storage/ciencia/investigacion/tesis/mh_muestras_GOOD/final_samples_3qubits/averages"];
avgR0Pp33MH = Get["avgs_delta=0.05_beta=100_rz=0_p=boltzmann_allN.wl"][[1]];
avgRp1Pp33MH = Get["avgs_delta=0.05_beta=100_rz=0.1_p=boltzmann_allN.wl"][[1]];


fidelity[avgR0Pp33BRUTAL, avgR0Pp33MH]
fidelity[avgRp1Pp33BRUTAL, avgRp1Pp33MH]


MatrixForm /@ {Chop[avgR0Pp33BRUTAL], Chop[avgR0Pp33MH]}
