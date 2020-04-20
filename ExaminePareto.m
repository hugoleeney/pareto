(* Wolfram Language package *)


PercentsBiggerThanMax[shape_:1.5, experiments_:10, trials_:10, nSamples_:40] := (
	percentsBiggerThanMax = Table[0, {i, experiments}];
	For[j = 1, j <= experiments, j++, 
		table1 = Table[RandomVariate[ParetoDistribution[1, shape]], {i, nSamples}];
		max = Max[table1];
		count = 0;
		For[i = 1, i <= trials, i++, 
			b = RandomVariate[ParetoDistribution[1, shape]];
			If[b > max, count++];
		];
		percentsBiggerThanMax[[j]] = N[count/trials];
	];
	percentsBiggerThanMax
)

PercentsBiggerThanMaxCont[shape_:1.5, experiments_:10, trials_:10] := (
	percentsBiggerThanMax = Table[0, {i, experiments}];
	For[j = 1, j <= experiments, j++,
		dist = ParetoDistribution[1, shape];
		max = RandomVariate[dist];
		
		count = 0;
		For[i = 1, i <= trials, i++, 
			b = RandomVariate[ParetoDistribution[1, shape]];
			If[b > max, count++; max=b];
		];
		percentsBiggerThanMax[[j]] = N[count/trials];
	];
	percentsBiggerThanMax
)

(* each experiment is scored by the average of all the ratios b/max that are > 1. If there are none we score 0 *)
AverageRatioBiggerThanMax[shape_:1.5, experiments_:10, trials_:10, nSamples_:40] := (
	averageRatioBiggerThanMax = Table[0, {i, experiments}];
	For[j = 1, j <= experiments, j++, 
		table1 = Table[RandomVariate[ParetoDistribution[1, shape]], {i, nSamples}];
		max = Max[table1];
		count = 0;
		total = 0;
		For[i = 1, i <= trials, i++, 
			b = RandomVariate[ParetoDistribution[1, shape]];
			If[b > max, count++; total = total + (b/max)];
		];
		If[count > 0, averageRatioBiggerThanMax[[j]] = N[(total/count), 0]]
	];
	averageRatioBiggerThanMax
)

(* each experiment is scored by the average of all the ratios b/max that are > 1. If there are none we score 0 *)
MedianRatioBiggerThanMax[shape_:1.5, experiments_:10, trials_:10, nSamples_:40] := (
	averageRatioBiggerThanMax = Table[0, {i, experiments}];
	For[j = 1, j <= experiments, j++, 
		table1 = Table[RandomVariate[ParetoDistribution[1, shape]], {i, nSamples}];
		max = Max[table1];
		count = 0;
		total = 0;
		ratios = Table[0, {i,trials}];
		For[i = 1, i <= trials, i++, 
			b = RandomVariate[ParetoDistribution[1, shape]];
			If[b > max, count++; total = total + (b/max); ratios[[i]] = b/max];
		];
		(*Print["ratios",ratios];
		Print["median",Median[ratios]];
		Print["count",count];*)
		If[count > 0, averageRatioBiggerThanMax[[j]] = Median[Select[ratios,#>0&]]]
	];
	averageRatioBiggerThanMax
)


DiffBetweenVarAndMaxCalc[shape_:1.5, experiments_:10, trials_:10, nSamples_:40] := (
	diffBetweenVarAndMax = Table[0, {i, experiments*trials}];
	For[j = 1, j <= experiments, j++, 
		table1 = Table[RandomVariate[ParetoDistribution[1, shape]], {i, nSamples}];
		max = Max[table1];
		For[i = 1, i <= trials, i++, 
			b = RandomVariate[ParetoDistribution[1, shape]];
			diffBetweenVarAndMax[[((j - 1)*trials) + i]] = b - max;
		];
	];
	diffBetweenVarAndMax
)

(***************************************************************)

PercentsBiggerThanMaxCont[shape_:1.5, experiments_:10, trials_:10] := (
	percentsBiggerThanMax = Table[0, {i, experiments}];
	For[j = 1, j <= experiments, j++,
		dist = ParetoDistribution[1, shape];
		max = RandomVariate[dist];
		
		count = 0;
		For[i = 1, i <= trials, i++, 
			b = RandomVariate[ParetoDistribution[1, shape]];
			If[b > max, count++; max=b];
		];
		percentsBiggerThanMax[[j]] = N[count/trials];
	];
	percentsBiggerThanMax
)


AverageRatioBiggerThanMaxCont[shape_:1.5, experiments_:10, trials_:10] := (
	averagePercentBiggerThanMax = Table[0, {i, experiments}];
	For[j = 1, j <= experiments, j++,
		dist = ParetoDistribution[1, shape];
		max = RandomVariate[dist];
		
		count = 0;
		total = 0;
		For[i = 1, i <= trials, i++, 
			b = RandomVariate[ParetoDistribution[1, shape]];
			If[b > max, count++; total = total + (b/max); max=b];
		];
		If[count > 0, averagePercentBiggerThanMax[[j]] = N[(total/count), 0]]
	];
	averagePercentBiggerThanMax
)

DiffBetweenVarAndMaxCont[shape_:1.5, experiments_:10, trials_:10] := (
	diffBetweenVarAndMax = Table[0, {i, experiments*trials}];
	For[j = 1, j <= experiments, j++,
		dist = ParetoDistribution[1, shape];
		max = RandomVariate[dist];
		For[i = 1, i <= trials, i++, 
			b = RandomVariate[ParetoDistribution[1, shape]];
			diffBetweenVarAndMax[[(j - 1)*trials + i]] = b - max;
			If[b > max, max=b];
		];
	];
	diffBetweenVarAndMax
)

(***************************************************************)

ForRange[start_, end_, inc_, shape_, experiments_]:=
	Table[
		f[shape, experiments, i], 
		{i,Range[start, end, inc]}
	]

ForRangeData[funToRun_, experiments_, start_, end_, inc_, shape_]:=(
	TransposeForH3D[
		ForRange[start, end, inc, shape, experiments]/.f->funToRun,
		experiments, start, end, inc
	]
)

ForRangeHistogram3D[funToRun_,experiments_, start_, end_, inc_, shape_, xbspec_:Automatic, chartElementFunction_:Automatic]:=(
	Histogram3D[
		ForRangeData[funToRun, experiments, start, end, inc, shape],
		{xbspec,Automatic}, chartElementFunction
	]
)

(*Range[1, Length[Range[start, end, inc]]]]]*)
TransposeForH3D[array_, experiments_, start_, end_, inc_]:=(
	Transpose@{
	  Apply[Join, array],
	  Apply[Join, 
	   Map[(Table[#, {j, experiments}]) &, 
	    Range[start, end, inc]]]
	  }
)


(* Functions to test nSample variable *)

ForRangeSamples[start_, end_, inc_, shape_, experiments_, trials_]:=
	Table[
		f[shape, experiments, trials, i], 
		{i,Range[start, end, inc]}
	]

ForRangeDataSamples[funToRun_, experiments_, start_, end_, inc_, shape_, trials_]:=(
	TransposeForH3D[
		ForRangeSamples[start, end, inc, shape, experiments, trials]/.f->funToRun,
		experiments, start, end, inc
	]
)

ForRangeSamplesHistogram3D[funToRun_,experiments_, start_, end_, inc_, shape_, trials_, xbspec_:Automatic, chartElementFunction_:Automatic]:=(
	Histogram3D[
		ForRangeDataSamples[funToRun, experiments, start, end, inc, shape, trials],
		{xbspec,Automatic}, chartElementFunction
	]
)

(* Functions to vary shape variable in single graph *)

ForRangeShape[start_, end_, inc_, experiments_, trials_, samples_]:=
	Table[
		f[i, experiments, trials, samples], 
		{i,Range[start, end, inc]}
	]

ForRangeDataShape[funToRun_, experiments_, start_, end_, inc_, trials_, samples_]:=(
	TransposeForH3D[
		ForRangeShape[start, end, inc, experiments, trials, samples]/.f->funToRun,
		experiments, start, end, inc
	]
)

ForRangeShapeHistogram3D[funToRun_,experiments_, start_, end_, inc_, trials_, samples_, xbspec_:Automatic, chartElementFunction_:Automatic]:=(
	Histogram3D[
		ForRangeDataShape[funToRun, experiments, start, end, inc, trials, samples],
		{xbspec,Automatic}, chartElementFunction
	]
)


(************************************************************************************)
(* Functions Below are structured around data for every trial of an experiment rather than summarised per experiment *)
(************************************************************************************)

TransposeTrialsForHistogram3D[array_, experiments_, start_, end_, inc_] := (
	Transpose@{
		Apply[Join, array],
		Apply[Join,
			Map[(Table[#, {j, experiments*#}]) &, Range[start, end, inc]]]
	}
)

ForRangeTrialsData[funToRun_, experiments_, start_, end_, inc_, shape_]:=(
	TransposeTrialsForHistogram3D[
		ForRange[start, end, inc, shape, experiments]/.f->funToRun,
		experiments, start, end, inc
	]
)

ForRangeTrialsHistogram3D[funToRun_, experiments_, start_, end_, inc_, shape_, xbspec_:Automatic, chartElementFunction_:Automatic]:=(
	Histogram3D[
		ForRangeTrialsData[funToRun, experiments, start, end, inc, shape],
		{xbspec,Automatic}, chartElementFunction
	]
)


TransposeFixedTrialsForHistogram3D[array_, experiments_, start_, end_, inc_, trials_] := (
	Transpose@{
		Apply[Join, array],
		Apply[Join,
			Map[(Table[#, {j, experiments*trials}]) &, Range[start, end, inc]]]
	}
)

(* Functions to test nSample variable *)

ForRangeSamplesOverTrials[start_, end_, inc_, shape_, experiments_, trials_]:=
	Table[
		f[shape, experiments, trials, i], 
		{i,Range[start, end, inc]}
	]

ForRangeSamplesOverTrialsData[funToRun_, experiments_, start_, end_, inc_, shape_, trials_]:=(
	TransposeFixedTrialsForHistogram3D[
		ForRangeSamplesOverTrials[start, end, inc, shape, experiments, trials]/.f->funToRun,
		experiments, start, end, inc, trials
	]
)

ForRangeSamplesOverTrialsHistogram3D[funToRun_, experiments_, start_, end_, inc_, shape_, trials_, xbspec_:Automatic, chartElementFunction_:Automatic]:=(
	Histogram3D[
		ForRangeSamplesOverTrialsData[funToRun, experiments, start, end, inc, shape, trials],
		{xbspec,Automatic}, chartElementFunction
	]
)

(* Functions to vary shape variable in single graph *)

ForRangeShapeOverTrials[start_, end_, inc_, experiments_, trials_, samples_]:=
	Table[
		f[i, experiments, trials, samples], 
		{i,Range[start, end, inc]}
	]

ForRangeDataShapeOverTrials[funToRun_, experiments_, start_, end_, inc_, trials_, samples_]:=(
	TransposeFixedTrialsForHistogram3D[
		ForRangeShapeOverTrials[start, end, inc, experiments, trials, samples]/.f->funToRun,
		experiments, start, end, inc, trials
	]
)

ForRangeShapeOverTrialsHistogram3D[funToRun_, experiments_, start_, end_, inc_, trials_, samples_, xbspec_:Automatic, chartElementFunction_:Automatic]:=(
	Histogram3D[
		ForRangeDataShapeOverTrials[funToRun, experiments, start, end, inc, trials, samples],
		{xbspec,Automatic}, chartElementFunction
	]
)