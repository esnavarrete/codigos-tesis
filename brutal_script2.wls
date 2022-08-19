#!/usr/bin/env wolframscript
(* ::Package:: *)

Get["CoolTools.m", Path->"."]
Get["Carlos.m", Path->"."]
Get["Quantum.m", Path->"."]


SetDirectory["/home/enavarrete/brutal_samples/samplesRp8P8"]


ItppVectorToExpression[vector_String] := 
 ToExpression /@ 
  StringSplit[
   StringReplace[
    StringTake[vector, {2, -2}], {"i" -> "I", "e+" :> "*^", 
     "e-" :> "*^-"}]]


SampleSize = 10000;
rz = 0.8;
MaxError = 0.01;
SwapProbability = 0.8;


brutalSampleCPP[swapP_, \[Epsilon]_, sampleSize_, rz_]:= ItppVectorToExpression /@ (RAW = 
     ReadListUncomment[
      "!/home/enavarrete/cg -o get_states_bruto -s 0 --swap_probability " <> 
       ToString[swapP] <> " --max_tolerance " <> 
       ToString[\[Epsilon]] <> " --sample_size " <> 
       ToString[sampleSize] <> " --r_z " <> ToString[rz], String]);


Do[
	sample = Timing[brutalSampleCPP[SwapProbability, MaxError, SampleSize, rz]];
	Export["brutals_N=10000_err=0.01_rz=0.8_p=0.8_"<>ToString[i]<>".wl", sample];
	Print["guardada la muestra"<>ToString[i]]
,{i,8}]
