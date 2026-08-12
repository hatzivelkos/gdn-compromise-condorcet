ClearAll[
  SettingKey,
  GenerateProfileByModel,
  PerturbProfileAdjacentSwap,
  SingleInstanceMetrics,
  AggregateResults,
  RunExperimentSetting,
  RunExperimentGrid
];

SettingKey[setting_Association] := Hash[Normal[setting], "SHA256"];

Options[GenerateProfileByModel] = {
  "Model" -> "IC",
  "m" -> 5,
  "n" -> 25,
  "phi" -> 1.,
  "lambda" -> 0.75,
  "center1" -> Automatic,
  "center2" -> Automatic
};

(* Optional: message if unknown model *)
GenerateProfileByModel::model = "Unknown model `1`.";

GenerateProfileByModel[opts : OptionsPattern[]] := Module[
  {model = OptionValue["Model"], m = OptionValue["m"], n = OptionValue["n"],
   phi = OptionValue["phi"], lambda = OptionValue["lambda"],
   c1 = OptionValue["center1"], c2 = OptionValue["center2"]},

  Switch[
    model,

    "IC",
      GenerateICProfile[m, n],

    "Mallows",
      GenerateMallowsProfile[m, n, phi, c1],

    "Polarized",
      GeneratePolarizedMixtureProfile[m, n, phi, lambda, c1, c2],

    (*====================== NEW MODEL ======================*)
    (* Polarizacija između suprotnih preferencija (npr. ABCDE i EDCBA),
       uz šum phi i mixing prema IC kontroliran lambdom. *)
    "OppPrefs",
      GenerateOppPrefsMixtureProfile[m, n, phi, lambda, c1, c2],

    _,
      Message[GenerateProfileByModel::model, model]; $Failed
  ]
];

PerturbProfileAdjacentSwap[perm_?MatrixQ] := Module[
  {n = Length[perm], m = Length[First[perm]], perm2 = perm, j, tmp},
  If[m < 2, Return[Developer`ToPackedArray[perm2]]];
  Do[
    j = RandomInteger[{1, m - 1}];
    tmp = perm2[[v, j]];
    perm2[[v, j]] = perm2[[v, j + 1]];
    perm2[[v, j + 1]] = tmp;
    ,
    {v, 1, n}
  ];
  Developer`ToPackedArray[perm2]
];


(*====================== MidScore helper ======================*)

ClearAll[MidScoreValue];
MidScoreValue[m_Integer?Positive, cand_Integer?Positive] := Module[{mu, denom},
  mu = (m + 1)/2.0;
  denom = (m - 1)/2.0;
  If[denom == 0, 1.0, 1.0 - Abs[cand - mu]/denom]
];

ClearAll[MidScoreOfWinners];
MidScoreOfWinners[m_Integer?Positive, w_] := Module[{ws},
  ws = If[ListQ[w], w, {w}];
  Mean[MidScoreValue[m, #] & /@ ws]
];



Options[SingleInstanceMetrics] = {
  "q" -> 2/3,
  "p" -> 2,
  "TopKList" -> {2, 3},
  "NoiseModel" -> "AdjacentSwap",
  "MaxRank" -> Automatic,
  "TieBreak" -> "RandomSeeded"
};

SingleInstanceMetrics[perm_?MatrixQ, opts : OptionsPattern[]] := Module[
  {prof, q = OptionValue["q"], p = OptionValue["p"], ks = OptionValue["TopKList"],
   cw, cwExists, permNoise, profNoise, winners, winnersNoise, maxRank, tb, ruleNames},

  prof = MakeProfile[perm];
  If[!AssociationQ[prof], Return[Missing["BadProfile"]]];

  maxRank = OptionValue["MaxRank"];
  tb = OptionValue["TieBreak"];

  winners = WinnersAll[prof, q, p, "MaxRank" -> maxRank, "TieBreak" -> tb];

  cw = CondorcetWinner[prof];
  cwExists = cw =!= Missing["None"];

  permNoise = Switch[
    OptionValue["NoiseModel"],
    "AdjacentSwap", PerturbProfileAdjacentSwap[perm],
    _, PerturbProfileAdjacentSwap[perm]
  ];

  profNoise = MakeProfile[permNoise];
  If[!AssociationQ[profNoise], Return[Missing["BadNoisyProfile"]]];

  winnersNoise = WinnersAll[profNoise, q, p, "MaxRank" -> maxRank, "TieBreak" -> tb];

  ruleNames = Keys[winners];

  Association @ Table[
    With[{w = winners[rn], w2 = winnersNoise[rn]},
      rn -> <|
        "NeverFirst" -> Boole[TrueQ[NeverFirstQ[prof, w]]],
        "TopK" -> Association@Table[k -> TopKAcceptability[prof, w, k], {k, ks}],
        "CondorcetExists" -> Boole[cwExists],
        "CondorcetHit" -> If[cwExists, Boole[w === cw], 0],
        "MidScore" -> N@MidScoreOfWinners[prof["m"], w],
        "NoiseFlip" -> Boole[w2 =!= w]
      |>
    ],
    {rn, ruleNames}
  ]
];

Options[AggregateResults] = {"TopKList" -> {2, 3}};

AggregateResults[inst_List, opts : OptionsPattern[]] := Module[
  {rules, ks, nInst, cwCount, agg, safeMean},

  ks = OptionValue["TopKList"];
  nInst = Length[inst];
  If[nInst == 0, Return[Missing["NoInstances"]]];

  rules = Keys[First[inst]];

  (* helper: mean that tolerates Missing / empty lists *)
  safeMean[x_List] := Module[{y = DeleteMissing[x]},
    If[Length[y] == 0, Missing["NA"], N@Mean[y]]
  ];

  (* Condorcet existence is the same across rules; take it from Borda *)
  cwCount = Total @ inst[[All, "Borda", "CondorcetExists"]];

  agg = Association @ Table[
    Module[{neverFirst, noiseFlip, topk, ce},

      neverFirst = safeMean @ inst[[All, r, "NeverFirst"]];
      noiseFlip  = safeMean @ inst[[All, r, "NoiseFlip"]];
      midScore = safeMean @ inst[[All, r, "MidScore"]];

      topk = Association @ Table[
        k -> safeMean @ Lookup[inst[[All, r, "TopK"]], k, Missing["NA"]],
        {k, ks}
      ];

      ce = If[cwCount == 0,
        Missing["NA"],
        N[Total[inst[[All, r, "CondorcetHit"]]]/cwCount]
      ];

      r -> <|
        "N" -> nInst,
        "NeverFirstRate" -> neverFirst,
        "TopKAcceptability" -> topk,
        "CondorcetEfficiency" -> ce,
        "NoiseFlipRate" -> noiseFlip,
        "MidScore" -> midScore,
        "CondorcetExistRate" -> N[cwCount/nInst]
      |>
    ],
    {r, rules}
  ];

  agg
];

Options[RunExperimentSetting] = {
  "Model" -> "IC",
  "m" -> 5,
  "n" -> 25,
  "phi" -> 1.,
  "lambda" -> 0.75,
  "center1" -> Automatic,
  "center2" -> Automatic,
  "q" -> 2/3,
  "p" -> 2,
  "S" -> 200,
  "TopKList" -> {2, 3},
  "MaxRank" -> Automatic,
  "Parallel" -> True,
  "BaseSeed" -> 123456,
  "TieBreak" -> "RandomSeeded"
};

RunExperimentSetting[opts : OptionsPattern[]] := Module[
  {setting, skey, S, par, baseSeed, inst, modelOpts, metricOpts, ks, tb, maxRank},

  S = OptionValue["S"];
  par = TrueQ[OptionValue["Parallel"]];
  baseSeed = OptionValue["BaseSeed"];
  ks = OptionValue["TopKList"];
  tb = OptionValue["TieBreak"];
  maxRank = OptionValue["MaxRank"];

  setting = <|
     "Model" -> OptionValue["Model"],
     "m" -> OptionValue["m"],
     "n" -> OptionValue["n"],
     "phi" -> OptionValue["phi"],
     "q" -> OptionValue["q"],
     "p" -> OptionValue["p"],
     "S" -> S,
     "TopKList" -> ks,
     "MaxRank" -> maxRank
   |>;

  (*====================== UPDATED: lambda ulazi u key i za OppPrefs ======================*)
  If[MemberQ[{"Polarized", "OppPrefs"}, OptionValue["Model"]],
    setting["lambda"] = OptionValue["lambda"];
  ];

  skey = SettingKey[setting];

  modelOpts = FilterRules[
    {
      "Model" -> OptionValue["Model"],
      "m" -> OptionValue["m"],
      "n" -> OptionValue["n"],
      "phi" -> OptionValue["phi"],
      "lambda" -> OptionValue["lambda"],
      "center1" -> OptionValue["center1"],
      "center2" -> OptionValue["center2"]
    },
    Options[GenerateProfileByModel]
  ];

  metricOpts = FilterRules[
    {
      "q" -> OptionValue["q"],
      "p" -> OptionValue["p"],
      "TopKList" -> ks,
      "MaxRank" -> maxRank,
      "TieBreak" -> tb
    },
    Options[SingleInstanceMetrics]
  ];

  If[par,
    DistributeDefinitions[
      GenerateProfileByModel, PerturbProfileAdjacentSwap, SingleInstanceMetrics, AggregateResults, SettingKey, MidScoreValue,
MidScoreOfWinners,
      GenerateICProfile, MallowsSampleKendall, GenerateMallowsProfile,
      GeneratePolarizedMixtureProfile,
      (*====================== NEW: distribute new generator ======================*)
      GenerateOppPrefsMixtureProfile,
      MakeProfile, WinnersAll, WinnerBorda, WinnerCoombs, WinnerQRCompromise, WinnerSDM, WinnerFallback, WinnerPlurality,
      TieBreakPick, TieKeyDefault, ArgMaxCandidates, ArgMinCandidates,
      NeverFirstQ, TopKAcceptability, PairwiseMargins, CondorcetWinner
    ];
  ];

  inst = If[par,
    ParallelTable[
      BlockRandom[
        SeedRandom[Hash[{baseSeed, skey, rep}, "SHA256"]];
        With[{perm = GenerateProfileByModel[Sequence @@ modelOpts]},
          SingleInstanceMetrics[perm, Sequence @@ metricOpts]
        ]
      ],
      {rep, 1, S}
    ],
    Table[
      BlockRandom[
        SeedRandom[Hash[{baseSeed, skey, rep}, "SHA256"]];
        With[{perm = GenerateProfileByModel[Sequence @@ modelOpts]},
          SingleInstanceMetrics[perm, Sequence @@ metricOpts]
        ]
      ],
      {rep, 1, S}
    ]
  ];

  <|
    "Setting" -> setting,
    "RuleStats" -> AggregateResults[inst, "TopKList" -> ks]
  |>
];

Options[RunExperimentGrid] = {
  (* NOTE: default ostaje isti da ništa ne promijeni postojeći pipeline *)
  "Models" -> {"IC", "Mallows", "Polarized"},
  "mList" -> {3, 5, 7, 10},
  "nList" -> {25, 51, 101},
  "phiList" -> {0.6, 0.8, 1.},
  "lambdaList" -> {0.6, 0.7, 0.8, 0.9},
  "qList" -> {2/3},
  "pList" -> {2},
  "S" -> 200,
  "TopKList" -> {2, 3},
  "MaxRank" -> Automatic,
  "Parallel" -> True,
  "BaseSeed" -> 123456,
  "TieBreak" -> "RandomSeeded"
};

RunExperimentGrid[opts : OptionsPattern[]] := Module[
  {models, mList, nList, phiList, lambdaList, qList, pList, S, ks, maxRank, par, baseSeed, tb, settings, results},

  models = OptionValue["Models"];
  mList = OptionValue["mList"];
  nList = OptionValue["nList"];
  phiList = OptionValue["phiList"];
  lambdaList = OptionValue["lambdaList"];
  qList = OptionValue["qList"];
  pList = OptionValue["pList"];
  S = OptionValue["S"];
  ks = OptionValue["TopKList"];
  maxRank = OptionValue["MaxRank"];
  par = OptionValue["Parallel"];
  baseSeed = OptionValue["BaseSeed"];
  tb = OptionValue["TieBreak"];

  settings = Flatten@Table[
    Switch[
      model,

      "IC",
        Table[
          <|"Model" -> "IC", "m" -> m, "n" -> n, "S" -> S, "q" -> q, "p" -> p|>,
          {m, mList}, {n, nList}, {q, qList}, {p, pList}
        ],

      "Mallows",
        Table[
          <|"Model" -> "Mallows", "m" -> m, "n" -> n, "phi" -> phi, "S" -> S, "q" -> q, "p" -> p|>,
          {m, mList}, {n, nList}, {phi, phiList}, {q, qList}, {p, pList}
        ],

      "Polarized",
        Table[
          <|"Model" -> "Polarized", "m" -> m, "n" -> n, "phi" -> phi, "lambda" -> lambda, "S" -> S, "q" -> q, "p" -> p|>,
          {m, mList}, {n, nList}, {phi, phiList}, {lambda, lambdaList}, {q, qList}, {p, pList}
        ],

      (*====================== NEW SETTINGS GRID ======================*)
      "OppPrefs",
        Table[
          <|"Model" -> "OppPrefs", "m" -> m, "n" -> n, "phi" -> phi, "lambda" -> lambda, "S" -> S, "q" -> q, "p" -> p|>,
          {m, mList}, {n, nList}, {phi, phiList}, {lambda, lambdaList}, {q, qList}, {p, pList}
        ],

      _,
        {}
    ],
    {model, models}
  ];

  results = Table[
    RunExperimentSetting[
      "Model" -> Lookup[s, "Model", "IC"],
      "m" -> Lookup[s, "m", 5],
      "n" -> Lookup[s, "n", 25],
      "phi" -> Lookup[s, "phi", 1.],
      "lambda" -> Lookup[s, "lambda", 0.75],
      "q" -> Lookup[s, "q", 2/3],
      "p" -> Lookup[s, "p", 2],
      "S" -> Lookup[s, "S", S],
      "TopKList" -> ks,
      "MaxRank" -> maxRank,
      "Parallel" -> par,
      "BaseSeed" -> baseSeed,
      "TieBreak" -> tb
    ],
    {s, settings}
  ];

  results
];
