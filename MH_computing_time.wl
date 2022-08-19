(* ::Package:: *)

(* ::Title:: *)
(*Optimizando tiempos de c\[OAcute]mputo del MH*)


(* ::Text:: *)
(*En el archivo MH_measuring _ergodicity . nb se tomaron muestras de brutales para obtener una referencia de ergodicidad \[OAcute]ptima . Adem\[AAcute]s, se observ\[OAcute] el comportamiento de la ergodicidad de las muestras MH en funci\[OAcute]n del n\[UAcute]mero de iteraciones para distintos valores de \[Beta] y \[Delta] . Ahora, el objetivo es determinar qu\[EAcute] valores de \[Beta], \[Delta] y N hay que escoger para obtener la ergodicidad \[OAcute]ptima (Subscript[E, 1] <= 0.1) con el menor tiempo de c\[OAcute]mputo posible.*)


Get["/media/storage/ciencia/investigacion/proyecto-ss/adanerick/codigo/CoolTools.m"]
Get["/media/storage/ciencia/investigacion/tesis/codigos/usefulFunctions.wl"]


(* ::Section::Closed:: *)
(*Importando datos de tiempos de c\[OAcute]mputo*)


SetDirectory["/media/storage/ciencia/investigacion/tesis/muestras_MH/distintasN_betasydeltas_Rp5_Pp3/execution_times"]


deltas = {0.01, 0.02, 0.03, 0.04, 0.05};
betas = {100, 250, 400, 500, 1000};
enes = {10000, 20000, 40000, 60000, 80000, 100000, 200000, 300000, 400000, 500000};


exeTimesRp5Pp3\[Beta]100 = Get["exeTimes_Rp5Pp3_beta=100_alldeltas.wl"];
exeTimesRp5Pp3\[Beta]250 = Get["exeTimes_Rp5Pp3_beta=250_alldeltas.wl"];
exeTimesRp5Pp3\[Beta]400 = Get["exeTimes_Rp5Pp3_beta=400_alldeltas.wl"];
exeTimesRp5Pp3\[Beta]500 = Get["exeTimes_Rp5Pp3_beta=500_alldeltas.wl"];
exeTimesRp5Pp3\[Beta]1000 = Get["exeTimes_Rp5Pp3_beta=1000_alldeltas.wl"]; (*tiene pedos: la parte 5 s\[OAcute]lo tiene 8 datos. Por eso sale el error Part::partd*)


(* ::Section:: *)
(*Tiempo de c\[OAcute]mputo del MH en funci\[OAcute]n de N*)


(*Generaci\[OAcute]n de los datos a graficar:*)
timesDataRp5Pp3\[Beta]100 = Map[MapThread[{#1,#2}&, {enes, #}]&, exeTimesRp5Pp3\[Beta]100];
timesDataRp5Pp3\[Beta]250 = Map[MapThread[{#1,#2}&, {enes, #}]&, exeTimesRp5Pp3\[Beta]250];
timesDataRp5Pp3\[Beta]400 = Map[MapThread[{#1,#2}&, {enes, #}]&, exeTimesRp5Pp3\[Beta]400];
timesDataRp5Pp3\[Beta]500 = Map[MapThread[{#1,#2}&, {enes, #}]&, exeTimesRp5Pp3\[Beta]500];
timesDataRp5Pp3\[Beta]1000 = Map[MapThread[{#1,#2}&, {enes, #}]&, exeTimesRp5Pp3\[Beta]1000];
allDataForTime\[Delta]foreach\[Beta] = {timesDataRp5Pp3\[Beta]100, timesDataRp5Pp3\[Beta]250, timesDataRp5Pp3\[Beta]400, timesDataRp5Pp3\[Beta]500, timesDataRp5Pp3\[Beta]1000};


(*graficaci\[OAcute]n con distintas deltas, para cada beta:*)
MapThread[ListLogLogPlot[#1, PlotLabel->"\[Beta] = "<>ToString[#2], 
				   PlotTheme->"Detailed",
				   FrameLabel->{"Iterations", "t [seg]"},
				   PlotLegends->deltas]&, 
	     {allDataForTime\[Delta]foreach\[Beta], betas}]


(*Ora se grafican al reve\:015b: distintas betas, para cada delta*)
(*Reacomodando los datos:*)
exeTimes\[Delta]p01all\[Beta] = List[exeTimesRp5Pp3\[Beta]100[[1]], exeTimesRp5Pp3\[Beta]250[[1]], exeTimesRp5Pp3\[Beta]400[[1]], exeTimesRp5Pp3\[Beta]500[[1]],exeTimesRp5Pp3\[Beta]1000[[1]]];
exeTimes\[Delta]p02all\[Beta] = List[exeTimesRp5Pp3\[Beta]100[[2]], exeTimesRp5Pp3\[Beta]250[[2]], exeTimesRp5Pp3\[Beta]400[[2]], exeTimesRp5Pp3\[Beta]500[[2]],exeTimesRp5Pp3\[Beta]1000[[2]]];
exeTimes\[Delta]p03all\[Beta] = List[exeTimesRp5Pp3\[Beta]100[[3]], exeTimesRp5Pp3\[Beta]250[[3]], exeTimesRp5Pp3\[Beta]400[[3]], exeTimesRp5Pp3\[Beta]500[[3]],exeTimesRp5Pp3\[Beta]1000[[3]]];
exeTimes\[Delta]p04all\[Beta] = List[exeTimesRp5Pp3\[Beta]100[[4]], exeTimesRp5Pp3\[Beta]250[[4]], exeTimesRp5Pp3\[Beta]400[[4]], exeTimesRp5Pp3\[Beta]500[[4]],exeTimesRp5Pp3\[Beta]1000[[4]]];
exeTimes\[Delta]p05all\[Beta] = List[exeTimesRp5Pp3\[Beta]100[[5]], exeTimesRp5Pp3\[Beta]250[[5]], exeTimesRp5Pp3\[Beta]400[[5]], exeTimesRp5Pp3\[Beta]500[[5]],exeTimesRp5Pp3\[Beta]1000[[5]]];
(*Generando los datos a graficar*)
timesDataRp5Pp3\[Delta]p01 = Map[MapThread[{#1,#2}&, {enes, #}]&, exeTimes\[Delta]p01all\[Beta]];
timesDataRp5Pp3\[Delta]p02 = Map[MapThread[{#1,#2}&, {enes, #}]&, exeTimes\[Delta]p02all\[Beta]];
timesDataRp5Pp3\[Delta]p03 = Map[MapThread[{#1,#2}&, {enes, #}]&, exeTimes\[Delta]p03all\[Beta]];
timesDataRp5Pp3\[Delta]p04 = Map[MapThread[{#1,#2}&, {enes, #}]&, exeTimes\[Delta]p04all\[Beta]];
timesDataRp5Pp3\[Delta]p05 = Map[MapThread[{#1,#2}&, {enes, #}]&, exeTimes\[Delta]p05all\[Beta]];
allDataForTime\[Beta]foreach\[Delta] = {timesDataRp5Pp3\[Delta]p01, timesDataRp5Pp3\[Delta]p02, timesDataRp5Pp3\[Delta]p03, timesDataRp5Pp3\[Delta]p04, timesDataRp5Pp3\[Delta]p05};


(*graficaci\[OAcute]n con distintas betas, para cada delta:*)
MapThread[ListLogLogPlot[#1, PlotLabel->"\[Delta] = "<>ToString[#2], 
				   PlotTheme->"Detailed",
				   FrameLabel->{"Iterations", "t [seg]"},
				   PlotLegends->betas]&, 
	     {allDataForTime\[Beta]foreach\[Delta], deltas}]


(* ::Section:: *)
(*Optimizando el tiempo de c\[OAcute]mputo*)


(* ::Text:: *)
(*Seg\[UAcute]n los resultados de las ergodicidades, \[EAcute]stas no dependen de \[Beta] en forma relevante. Asumiremos pues, que la ergodicidad no depende de \[Beta], sino s\[OAcute]lo de \[Delta]. Con base en las gr\[AAcute]ficas de ergodicidad vs N, se determinaron los m\[IAcute]nimos valores de \[EAcute]sta \[UAcute]ltima que se necesitan con cada valor de \[Delta] para alcanzar la ergodicidad \[OAcute]ptima:*)


enes


(*Datos obtenidos por inspecci\[OAcute]n:*)
data\[Delta]Nmin = {{0.01, 305027}, {0.02, 95347}, {0.03, 54973}, {0.04, 25509}, {0.05, 23084}};


ListLogLogPlot[data\[Delta]Nmin, PlotTheme->"Detailed", FrameLabel->{"\[Delta]", "\!\(\*SubscriptBox[\(N\), \(min\)]\)"}, PlotLabel->"N\[UAcute]mero m\[IAcute]n. de iteraciones para alcanzar ergodicidad \[OAcute]ptima"]


(* ::Text:: *)
(*Sabemos entonces qu\[EAcute] valor de N escoger dada una \[Delta]. Uno pensar\[IAcute]a en escoger la \[Delta] m\[AAcute]s grande pues es la que necesita menos iteraciones, sin embargo, el tiempo que tardar\[AAcute] el algoritmo en hacerlas depender\[AAcute] del valor de \[Beta] que tengamos de antemano. Veamos pues, cu\[AAcute]nto tiempo toma generar las Subscript[N, min] iteraciones en funci\[OAcute]n de \[Beta].*)


timeDataFits\[Beta]100 = FindFit[Log[#],m x + b, {m, b}, {x}]& /@ timesDataRp5Pp3\[Beta]100;
timeDataFits\[Beta]250 = FindFit[Log[#],m x + b, {m, b}, {x}]& /@ timesDataRp5Pp3\[Beta]250;
timeDataFits\[Beta]400 = FindFit[Log[#],m x + b, {m, b}, {x}]& /@ timesDataRp5Pp3\[Beta]400;
timeDataFits\[Beta]500 = FindFit[Log[#],m x + b, {m, b}, {x}]& /@ timesDataRp5Pp3\[Beta]500;
timeDataFits\[Beta]1000 = FindFit[Log[#],m x + b, {m, b}, {x}]& /@ timesDataRp5Pp3\[Beta]1000;


(*para f\[AAcute]cil manejo:*)
Nmins = Map[#[[2]]&, data\[Delta]Nmin];


(*Con estos ajustes podemos calcular el Subscript[t, min] correspondiente a los Subscript[N, min] espec\[IAcute]ficos*)
tmins\[Beta]100 = MapThread[Exp[b] #2^m /. #1 &, {timeDataFits\[Beta]100, Nmins}];
tmins\[Beta]250 = MapThread[Exp[b] #2^m /. #1 &, {timeDataFits\[Beta]250, Nmins}];
tmins\[Beta]400 = MapThread[Exp[b] #2^m /. #1 &, {timeDataFits\[Beta]400, Nmins}];
tmins\[Beta]500 = MapThread[Exp[b] #2^m /. #1 &, {timeDataFits\[Beta]500, Nmins}];
tmins\[Beta]1000 = MapThread[Exp[b] #2^m /. #1 &, {timeDataFits\[Beta]1000, Nmins}];


(*Reorganizamos los datos*)
tminsData = Map[MapThread[{#1, #2}&, {betas, #}]&, Transpose[{tmins\[Beta]100, tmins\[Beta]250, tmins\[Beta]400, tmins\[Beta]500, tmins\[Beta]1000}]];


ListPlot[tminsData, PlotTheme->"Scientific", PlotLegends->PointLegend[deltas, LegendLabel->\[Delta]],
					FrameLabel->{"\[Beta]", "\!\(\*SubscriptBox[\(t\), \(min\)]\) [s]"},
					PlotLabel->"Tiempos de c\[OAcute]mputo m\[IAcute]nimos",
					GridLines->Automatic, 
					Joined->True]
