(* ::Package:: *)

(* ::Title:: *)
(*Midiendo la fidelidad*)


(* ::Text:: *)
(*Hasta ahora hemos tratado de optimizar los valores de los par\[AAcute]metros \[Beta], \[Delta] y N de tal manera que arrojen el menor tiempo de c\[OAcute]mputo posible con la restricci\[OAcute]n sobre la ergodicidad. Pero, \[DownQuestion]qu\[EAcute] pasa si en vez de la ergodicidad, usamos la fidelidad entre estado promedio del MH y el estado promedio exacto?*)


Get["/media/storage/ciencia/investigacion/proyecto-ss/adanerick/codigo/CoolTools.m"]
Get["/media/storage/ciencia/investigacion/tesis/codigos/usefulFunctions.wl"]


(* ::Section::Closed:: *)
(*Preliminares*)


SetDirectory["/media/storage/ciencia/investigacion/tesis/muestras_MH/distintasN_betasydeltas_Rp5_Pp3/averages"]


deltas = {0.01, 0.02, 0.03, 0.04, 0.05};
betas = {100, 250, 400, 500, 1000};
enes = {10000, 20000, 40000, 60000, 80000, 100000, 200000,300000,400000,500000};


(*Funci\[OAcute]n para importar los edos promedio:*)
getAvgStatesForASingle\[Beta][deltas_, \[Beta]_, rz_, p_]:= 
Map[Get["avgs_delta="<>ToString[#]<>"_beta="<>ToString[\[Beta]]<>"_rz="<>ToString[rz]<>"_p="<>ToString[p]<>"_allN.wl"]&, deltas];


(*Importando los edos promedio para todas las betas:*)
avgStates\[Beta]100 = getAvgStatesForASingle\[Beta][deltas, 100, 0.5, 0.3];
avgStates\[Beta]250 = getAvgStatesForASingle\[Beta][deltas, 250, 0.5, 0.3];
avgStates\[Beta]400 = getAvgStatesForASingle\[Beta][deltas, 400, 0.5, 0.3];
avgStates\[Beta]500 = getAvgStatesForASingle\[Beta][deltas, 500, 0.5, 0.3];
avgStates\[Beta]1000 = getAvgStatesForASingle\[Beta][deltas, 1000, 0.5, 0.3];


(*Estado promedio exacto correspondiente a Subscript[r, z] =0.5 y p = 0.3, par\[AAcute]metros con los que se tienen todos los datos calculados hasta el momento*)
exactAvg = \[Rho]averageTwoQubitsbasiscomp[0.3, 0.5];
exactAvg//MatrixForm


(*Calculando las fidelidades*)
fids\[Beta]100 = Map[fidelity[exactAvg, #]&, avgStates\[Beta]100, {2}];
fids\[Beta]250 = Map[fidelity[exactAvg, #]&, avgStates\[Beta]250, {2}];
fids\[Beta]400 = Map[fidelity[exactAvg, #]&, avgStates\[Beta]400, {2}];
fids\[Beta]500 = Map[fidelity[exactAvg, #]&, avgStates\[Beta]500, {2}];
fids\[Beta]1000 = Map[fidelity[exactAvg, #]&, avgStates\[Beta]1000, {2}];


(* ::Section:: *)
(*Fidelidad en funci\[OAcute]n del n\[UAcute]mero de iteraciones*)


(*arranging data for fidelities*)
fidelityData\[Beta]100 = Map[MapThread[{#1, #2}&, {enes, #}]&, fids\[Beta]100];
fidelityData\[Beta]250 = Map[MapThread[{#1, #2}&, {enes, #}]&, fids\[Beta]250];
fidelityData\[Beta]400 = Map[MapThread[{#1, #2}&, {enes, #}]&, fids\[Beta]400];
fidelityData\[Beta]500 = Map[MapThread[{#1, #2}&, {enes, #}]&, fids\[Beta]500];
fidelityData\[Beta]1000 = Map[MapThread[{#1, #2}&, {enes, #}]&, fids\[Beta]1000];


(*the same, but for 1- fidelty*)
oneMinusFData\[Beta]100 = Map[MapAt[1 - # &, #, {All, 2}]&, fidelityData\[Beta]100];
oneMinusFData\[Beta]250 = Map[MapAt[1 - # &, #, {All, 2}]&, fidelityData\[Beta]250];
oneMinusFData\[Beta]400 = Map[MapAt[1 - # &, #, {All, 2}]&, fidelityData\[Beta]400];
oneMinusFData\[Beta]500 = Map[MapAt[1 - # &, #, {All, 2}]&, fidelityData\[Beta]500];
oneMinusFData\[Beta]1000 = Map[MapAt[1 - # &, #, {All, 2}]&, fidelityData\[Beta]1000];


allDataForNs = {fidelityData\[Beta]100, fidelityData\[Beta]250, fidelityData\[Beta]400, fidelityData\[Beta]500, fidelityData\[Beta]1000};
allDataForNs2 = {oneMinusFData\[Beta]100, oneMinusFData\[Beta]250, oneMinusFData\[Beta]400, oneMinusFData\[Beta]500, oneMinusFData\[Beta]1000};


MapThread[Show[ListLogLogPlot[#1, PlotLabel->"\[Beta] = "<>ToString[#2], PlotLegends->deltas, PlotTheme->"Detailed",
						 FrameLabel->{"Iteraciones", "F"},
						 Joined->True], 
			   LogLogPlot[1,{x,0,500000}, PlotStyle->{Red, Dashed}]]&, 
{allDataForNs, betas}]


MapThread[Show[ListLogLogPlot[#1, PlotLabel->"\[Beta] = "<>ToString[#2], PlotLegends->deltas, PlotTheme->"Detailed",
						 FrameLabel->{"Iteraciones", "1 - F"},
						 Joined->True], 
			   LogLogPlot[0,{x,0,500000}]]&, 
{allDataForNs2, betas}]


(* ::Section:: *)
(*Fidelidad en funci\[OAcute]n del tiempo de c\[OAcute]mputo*)


SetDirectory["/media/storage/ciencia/investigacion/tesis/muestras_MH/distintasN_betasydeltas_Rp5_Pp3/execution_times"]


(*getting execution time data:*)
exeTimesRp5Pp3\[Beta]100 = Get["exeTimes_Rp5Pp3_beta=100_alldeltas.wl"];
exeTimesRp5Pp3\[Beta]250 = Get["exeTimes_Rp5Pp3_beta=250_alldeltas.wl"];
exeTimesRp5Pp3\[Beta]400 = Get["exeTimes_Rp5Pp3_beta=400_alldeltas.wl"];
exeTimesRp5Pp3\[Beta]500 = Get["exeTimes_Rp5Pp3_beta=500_alldeltas.wl"];
exeTimesRp5Pp3\[Beta]1000 = Get["exeTimes_Rp5Pp3_beta=1000_alldeltas.wl"]; (*tiene pedos: la parte 5 s\[OAcute]lo tiene 8 datos. Por eso sale el error Part::partd*)


(*arranging data for fidelities:*)
fidelityDataWT\[Beta]100 = MapThread[MapThread[{#1, #2}&, {#1, #2}]&, {exeTimesRp5Pp3\[Beta]100, fids\[Beta]100}];
fidelityDataWT\[Beta]250 = MapThread[MapThread[{#1, #2}&, {#1, #2}]&, {exeTimesRp5Pp3\[Beta]250, fids\[Beta]250}];
fidelityDataWT\[Beta]400 = MapThread[MapThread[{#1, #2}&, {#1, #2}]&, {exeTimesRp5Pp3\[Beta]400, fids\[Beta]400}];
fidelityDataWT\[Beta]500 = MapThread[MapThread[{#1, #2}&, {#1, #2}]&, {exeTimesRp5Pp3\[Beta]500, fids\[Beta]500}];
fidelityDataWT\[Beta]1000 = MapThread[MapThread[{#1, #2}&, {#1, #2}]&, {exeTimesRp5Pp3\[Beta]1000, fids\[Beta]1000}];


(*the same, but for 1- fidelity*)
oneMinusFDataWT\[Beta]100 = Map[MapAt[1 - # &, #, {All, 2}]&, fidelityDataWT\[Beta]100];
oneMinusFDataWT\[Beta]250 = Map[MapAt[1 - # &, #, {All, 2}]&, fidelityDataWT\[Beta]250];
oneMinusFDataWT\[Beta]400 = Map[MapAt[1 - # &, #, {All, 2}]&, fidelityDataWT\[Beta]400];
oneMinusFDataWT\[Beta]500 = Map[MapAt[1 - # &, #, {All, 2}]&, fidelityDataWT\[Beta]500];
oneMinusFDataWT\[Beta]1000 = Map[MapAt[1 - # &, #, {All, 2}]&, fidelityDataWT\[Beta]1000];


allDataForTs = {fidelityDataWT\[Beta]100, fidelityDataWT\[Beta]250, fidelityDataWT\[Beta]400, fidelityDataWT\[Beta]500, fidelityDataWT\[Beta]1000};
allDataForTs2 = {oneMinusFDataWT\[Beta]100, oneMinusFDataWT\[Beta]250, oneMinusFDataWT\[Beta]400, oneMinusFDataWT\[Beta]500, oneMinusFDataWT\[Beta]1000};


MapThread[Show[ListLogLogPlot[#1, PlotLabel->"\[Beta] = "<>ToString[#2], PlotLegends->deltas, PlotTheme->"Detailed",
							 FrameLabel->{"t [s]", "F"},
							 Joined->True],
			  LogLogPlot[1, {x,0,10000}, PlotStyle->{Red, Dashed}]]&, 
{allDataForTs, betas}]


MapThread[Show[ListLogLogPlot[#1, PlotLabel->"\[Beta] = "<>ToString[#2], PlotLegends->deltas, PlotTheme->"Detailed",
							 FrameLabel->{"t [s]", "1 - F"},
							 Joined->True],
			  LogLogPlot[0, {x,0,10000}, PlotStyle->{Red, Dashed}]]&, 
{allDataForTs2, betas}]
