(* ::Package:: *)

(* ::Title:: *)
(*Midiendo la fidelidad*)


(* ::Text:: *)
(*Hasta ahora hemos tratado de optimizar los valores de los par\[AAcute]metros \[Beta], \[Delta] y N de tal manera que arrojen el menor tiempo de c\[OAcute]mputo posible con la restricci\[OAcute]n sobre la ergodicidad. Pero, qu\[EAcute] pasa si en vez de la ergodicidad, usamos la fidelidad entre estado promedio del MH y el estado promedio exacto?*)


Get["/media/storage/ciencia/investigacion/tesis/codigos-tesis/CoolTools2.m"]
Get["/media/storage/ciencia/investigacion/tesis/codigos-tesis/usefulFunctions.wl"]


(* ::Section::Closed:: *)
(*Preliminares*)


SetDirectory["/media/storage/ciencia/investigacion/tesis/mh_muestras_GOOD/distintas_Nbetasydeltas_Rp5Pp3_DOS/averages"]


deltas = {0.005, 0.01, 0.03, 0.05, 0.07, 0.1};
betas = {100, 250, 400, 600, 750, 1000};
enes = {10000, 20000, 40000, 60000, 80000, 100000, 200000, 350000, 500000, 1000000};


(*Funci\[OAcute]n para importar los edos promedio:*)
getAvgStatesForASingle\[Delta][\[Delta]_, betas_, rz_, p_]:= 
Map[Get["avgs_delta="<>ToString[\[Delta]]<>"_beta="<>ToString[#]<>"_rz="<>ToString[rz]<>"_p="<>ToString[p]<>"_allN.wl"]&, betas];


(*Importando los edos promedio para todas las betas:*)
avgStates\[Delta]1 = getAvgStatesForASingle\[Delta][deltas[[1]], betas, 0.5, 0.3];
avgStates\[Delta]2 = getAvgStatesForASingle\[Delta][deltas[[2]], betas, 0.5, 0.3];
avgStates\[Delta]3 = getAvgStatesForASingle\[Delta][deltas[[3]], betas, 0.5, 0.3];
avgStates\[Delta]4 = getAvgStatesForASingle\[Delta][deltas[[4]], betas, 0.5, 0.3];
avgStates\[Delta]5 = getAvgStatesForASingle\[Delta][deltas[[5]], betas, 0.5, 0.3];
avgStates\[Delta]6 = getAvgStatesForASingle\[Delta][deltas[[6]], betas, 0.5, 0.3];


(*Estado promedio brutal correspondiente a Subscript[r, z] =0.5 y p = 0.3, par\[AAcute]metros con los que se tienen todos los datos calculados hasta el momento*)
avgRp5Pp3BRUTAL = preimageMean[ketsToDensity[Get["/media/storage/ciencia/investigacion/adanerick/muestras_de_edos/pureBrutalStates/pureHaarStates_n=10000_p=0.3_rz=0.5.m"]]];


(*Calculando las fidelidades*)
fids\[Delta]1 = Map[1-fidelity[avgRp5Pp3BRUTAL, #]&, avgStates\[Delta]1, {2}];
fids\[Delta]2 = Map[1-fidelity[avgRp5Pp3BRUTAL, #]&, avgStates\[Delta]2, {2}];
fids\[Delta]3 = Map[1-fidelity[avgRp5Pp3BRUTAL, #]&, avgStates\[Delta]3, {2}];
fids\[Delta]4 = Map[1-fidelity[avgRp5Pp3BRUTAL, #]&, avgStates\[Delta]4, {2}];
fids\[Delta]5 = Map[1-fidelity[avgRp5Pp3BRUTAL, #]&, avgStates\[Delta]5, {2}];
fids\[Delta]6 = Map[1-fidelity[avgRp5Pp3BRUTAL, #]&, avgStates\[Delta]6, {2}];


(* ::Section:: *)
(*Fidelidad en funci\[OAcute]n de N*)


(*arranging data for fidelities*)
fidelityData\[Delta]1 = Map[MapThread[{#1, #2}&, {enes, #}]&, fids\[Delta]1];
fidelityData\[Delta]2 = Map[MapThread[{#1, #2}&, {enes, #}]&, fids\[Delta]2];
fidelityData\[Delta]3 = Map[MapThread[{#1, #2}&, {enes, #}]&, fids\[Delta]3];
fidelityData\[Delta]4 = Map[MapThread[{#1, #2}&, {enes, #}]&, fids\[Delta]4];
fidelityData\[Delta]5 = Map[MapThread[{#1, #2}&, {enes, #}]&, fids\[Delta]5];
fidelityData\[Delta]6 = Map[MapThread[{#1, #2}&, {enes, #}]&, fids\[Delta]6];


allData = {fidelityData\[Delta]1, fidelityData\[Delta]2, fidelityData\[Delta]3 ,fidelityData\[Delta]4, fidelityData\[Delta]5, fidelityData\[Delta]6};


fidelityVSnPlots = MapThread[Show[ListLogLogPlot[#1, PlotLabel->"\[Delta] = "<>ToString[#2], PlotLegends->betas, PlotTheme->"Scientific",
						                      FrameLabel->{"Iteraciones", "1-F"},
						                      Joined->True, Mesh->All, MeshStyle->PointSize[0.015], GridLines->Automatic], 
			                      LogPlot[0,{x,0,1000000}, PlotStyle->{Red, Dashed}]]&, 
                   {allData, deltas}];


Grid[{fidelityVSnPlots[[1;;2]],
	  fidelityVSnPlots[[3;;4]],
	  fidelityVSnPlots[[5;;]]
}, ItemStyle->ImageSizeMultipliers->1, Spacings->{1, 1}]


(* ::Section::Closed:: *)
(*Ergodicidad y fidelidad*)


SetDirectory["/media/storage/ciencia/investigacion/tesis/mh_muestras_GOOD/distintas_Nbetasydeltas_Rp5Pp3_DOS/minvecs"]


(*datos de ergodicidad*)
minVecs\[Delta]1 = Get["minVecs_beta="<>ToString[#]<>"_delta=0.005_rz=0.5_p=0.3_allN.wl"]& /@ betas;
minVecs\[Delta]2 = Get["minVecs_beta="<>ToString[#]<>"_delta=0.01_rz=0.5_p=0.3_allN.wl"]& /@ betas;
minVecs\[Delta]3 = Get["minVecs_beta="<>ToString[#]<>"_delta=0.03_rz=0.5_p=0.3_allN.wl"]& /@ betas;
minVecs\[Delta]4 = Get["minVecs_beta="<>ToString[#]<>"_delta=0.05_rz=0.5_p=0.3_allN.wl"]& /@ betas;
minVecs\[Delta]5 = Get["minVecs_beta="<>ToString[#]<>"_delta=0.07_rz=0.5_p=0.3_allN.wl"]& /@ betas;
minVecs\[Delta]6 = Get["minVecs_beta="<>ToString[#]<>"_delta=0.1_rz=0.5_p=0.3_allN.wl"]& /@ betas;


ergs\[Delta]1 = Map[Map[ergodicityMeasure2[#]&, #]&, minVecs\[Delta]1];
ergs\[Delta]2 = Map[Map[ergodicityMeasure2[#]&, #]&, minVecs\[Delta]2];
ergs\[Delta]3 = Map[Map[ergodicityMeasure2[#]&, #]&, minVecs\[Delta]3];
ergs\[Delta]4 = Map[Map[ergodicityMeasure2[#]&, #]&, minVecs\[Delta]4];
ergs\[Delta]5 = Map[Map[ergodicityMeasure2[#]&, #]&, minVecs\[Delta]5];
ergs\[Delta]6 = Map[Map[ergodicityMeasure2[#]&, #]&, minVecs\[Delta]6];


data\[Delta]1 = MapThread[MapThread[{#1, #2}&, {#1, #2[[1;;9]]}]&, {ergs\[Delta]1, fids\[Delta]1}];
data\[Delta]2 = MapThread[MapThread[{#1, #2}&, {#1, #2[[1;;9]]}]&, {ergs\[Delta]2, fids\[Delta]2}];
data\[Delta]3 = MapThread[MapThread[{#1, #2}&, {#1, #2[[1;;9]]}]&, {ergs\[Delta]3, fids\[Delta]3}];
data\[Delta]4 = MapThread[MapThread[{#1, #2}&, {#1, #2[[1;;9]]}]&, {ergs\[Delta]4, fids\[Delta]4}];
data\[Delta]5 = MapThread[MapThread[{#1, #2}&, {#1, #2[[1;;9]]}]&, {ergs\[Delta]5, fids\[Delta]5}];
data\[Delta]6 = MapThread[MapThread[{#1, #2}&, {#1, #2[[1;;9]]}]&, {ergs\[Delta]6, fids\[Delta]6}];


fidelityVSergPlots = MapThread[ListPlot[#1, PlotLabel->"\[Delta] = "<>ToString[#2], PlotLegends->betas, PlotTheme->"Scientific", 
										  FrameLabel->{"Ergodicidad", "1-F"}, GridLines->Automatic, PlotRange->{{0,1}, {0,0.6}}]&,
		  {{data\[Delta]1, data\[Delta]2, data\[Delta]3, data\[Delta]4, data\[Delta]5, data\[Delta]6}, deltas}];


Grid[{fidelityVSergPlots[[1;;2]],
	  fidelityVSergPlots[[3;;4]],
	  fidelityVSergPlots[[5;;]]
}, ItemStyle->ImageSizeMultipliers->1, Spacings->{1, 1}]


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
