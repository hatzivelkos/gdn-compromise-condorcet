(*======================================================================
  05b_ExperimentRunnerGap.wl

  Separate experimental pipeline for correctly computing Compromise Gap.

  IMPORTANT:
  - Does NOT redefine the existing 05_ExperimentRunner.wl functions.
  - Gap is computed at the SINGLE-INSTANCE level:
        Gap(P,r) = 1 - Top1(P,r)/Top2(P,r),  if Top2 > 0,
                   0,                        otherwise.
  - Aggregate Gap is then Mean[Gap(P_s,r)] across replications.
  - Includes a separate ResultsToLongDatasetGap formatter.
======================================================================*)


ClearAll[
  SingleInstanceMetricsGap,
  AggregateResultsGap,
  RunExperimentSettingGap,
  RunExperimentGridGap,
  ResultsToLongDatasetGap
];


(*======================================================================
  1) SINGLE-INSTANCE METRICS
======================================================================*)

Options[SingleInstanceMetricsGap] = {
  "q" -> 2/3,
  "p" -> 2,
  "TopKList" -> {1, 2},
  "NoiseModel" -> "AdjacentSwap",
  "MaxRank" -> Automatic,
  "TieBreak" -> "RandomSeeded"
};


SingleInstanceMetricsGap[
   perm_?MatrixQ,
   opts : OptionsPattern[]
   ] :=
 Module[
  {
   prof,
   q = OptionValue["q"],
   p = OptionValue["p"],
   ks = OptionValue["TopKList"],
   cw,
   cwExists,
   permNoise,
   profNoise,
   winners,
   winnersNoise,
   maxRank,
   tb,
   ruleNames
   },

  (* Gap requires Top1 and Top2 *)
  If[
   !SubsetQ[ks, {1, 2}],
   Print[
    "ERROR in SingleInstanceMetricsGap: ",
    "TopKList must contain both 1 and 2."
    ];
   Return[Missing["TopKListMissing1or2"]]
   ];

  prof = MakeProfile[perm];

  If[
   !AssociationQ[prof],
   Return[Missing["BadProfile"]]
   ];

  maxRank = OptionValue["MaxRank"];
  tb = OptionValue["TieBreak"];

  winners =
   WinnersAll[
    prof,
    q,
    p,
    "MaxRank" -> maxRank,
    "TieBreak" -> tb
    ];

  cw = CondorcetWinner[prof];
  cwExists = cw =!= Missing["None"];

  permNoise =
   Switch[
    OptionValue["NoiseModel"],
    "AdjacentSwap",
    PerturbProfileAdjacentSwap[perm],
    _,
    PerturbProfileAdjacentSwap[perm]
    ];

  profNoise = MakeProfile[permNoise];

  If[
   !AssociationQ[profNoise],
   Return[Missing["BadNoisyProfile"]]
   ];

  winnersNoise =
   WinnersAll[
    profNoise,
    q,
    p,
    "MaxRank" -> maxRank,
    "TieBreak" -> tb
    ];

  ruleNames = Keys[winners];

  Association@Table[
    Module[
     {
      w = winners[rn],
      w2 = winnersNoise[rn],
      topk,
      top1,
      top2,
      gap
      },

     (* Compute all requested Top-k values once for this profile/rule *)
     topk =
      Association@Table[
        k -> TopKAcceptability[prof, w, k],
        {k, ks}
        ];

     top1 = Lookup[topk, 1, Missing["NoTop1"]];
     top2 = Lookup[topk, 2, Missing["NoTop2"]];

     gap =
      If[
       NumberQ[top1] && NumberQ[top2],
       If[
        top2 > 0,
        N[1.0 - top1/top2],
        0.0
        ],
       Missing["GapUnavailable"]
       ];

     rn ->
      <|
       "NeverFirst" ->
        Boole[TrueQ[NeverFirstQ[prof, w]]],

       "TopK" ->
        topk,

       (* Correct per-profile compromise gap *)
       "Gap" ->
        gap,

       "CondorcetExists" ->
        Boole[cwExists],

       "CondorcetHit" ->
        If[
         cwExists,
         Boole[w === cw],
         0
         ],

       "MidScore" ->
        N@MidScoreOfWinners[prof["m"], w],

       "NoiseFlip" ->
        Boole[w2 =!= w]
       |>
     ],
    {rn, ruleNames}
    ]
  ];


(*======================================================================
  2) AGGREGATION
======================================================================*)

Options[AggregateResultsGap] = {
  "TopKList" -> {1, 2}
  };


AggregateResultsGap[
   inst_List,
   opts : OptionsPattern[]
   ] :=
 Module[
  {
   rules,
   ks,
   nInst,
   cwCount,
   agg,
   safeMean,
   referenceRule
   },

  ks = OptionValue["TopKList"];
  nInst = Length[inst];

  If[
   nInst == 0,
   Return[Missing["NoInstances"]]
   ];

  rules = Keys[First[inst]];

  If[
   rules === {},
   Return[Missing["NoRules"]]
   ];

  (* Mean tolerant of Missing values *)
  safeMean[x_List] :=
   Module[
    {y = DeleteMissing[x]},
    If[
     Length[y] == 0,
     Missing["NA"],
     N@Mean[y]
     ]
    ];

  (* Condorcet existence is profile-dependent, not rule-dependent.
     Use the first available rule rather than assuming Borda. *)
  referenceRule = First[rules];

  cwCount =
   Total@
    inst[[All, referenceRule, "CondorcetExists"]];

  agg =
   Association@Table[
     Module[
      {
       neverFirst,
       noiseFlip,
       midScore,
       gap,
       topk,
       ce
       },

      neverFirst =
       safeMean@
        inst[[All, r, "NeverFirst"]];

      noiseFlip =
       safeMean@
        inst[[All, r, "NoiseFlip"]];

      midScore =
       safeMean@
        inst[[All, r, "MidScore"]];

      (* IMPORTANT:
         Mean of per-profile Gap values, NOT a ratio of mean Top1/Top2. *)
      gap =
       safeMean@
        inst[[All, r, "Gap"]];

      topk =
       Association@Table[
         k ->
          safeMean@
           Lookup[
            inst[[All, r, "TopK"]],
            k,
            Missing["NA"]
            ],
         {k, ks}
         ];

      ce =
       If[
        cwCount == 0,
        Missing["NA"],
        N[
         Total[
           inst[[All, r, "CondorcetHit"]]
           ]/cwCount
         ]
        ];

      r ->
       <|
        "N" ->
         nInst,

        "NeverFirstRate" ->
         neverFirst,

        "TopKAcceptability" ->
         topk,

        "Gap" ->
         gap,

        "CondorcetEfficiency" ->
         ce,

        "NoiseFlipRate" ->
         noiseFlip,

        "MidScore" ->
         midScore,

        "CondorcetExistRate" ->
         N[cwCount/nInst]
        |>
      ],
     {r, rules}
     ];

  agg
  ];


(*======================================================================
  3) RUN ONE EXPERIMENT SETTING
======================================================================*)

Options[RunExperimentSettingGap] = {
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
  "TopKList" -> {1, 2},
  "MaxRank" -> Automatic,
  "Parallel" -> True,
  "BaseSeed" -> 123456,
  "TieBreak" -> "RandomSeeded"
  };


RunExperimentSettingGap[
   opts : OptionsPattern[]
   ] :=
 Module[
  {
   setting,
   skey,
   S,
   par,
   baseSeed,
   inst,
   modelOpts,
   metricOpts,
   ks,
   tb,
   maxRank
   },

  S = OptionValue["S"];
  par = TrueQ[OptionValue["Parallel"]];
  baseSeed = OptionValue["BaseSeed"];
  ks = OptionValue["TopKList"];
  tb = OptionValue["TieBreak"];
  maxRank = OptionValue["MaxRank"];

  If[
   !SubsetQ[ks, {1, 2}],
   Print[
    "ERROR in RunExperimentSettingGap: ",
    "TopKList must contain both 1 and 2."
    ];
   Return[$Failed]
   ];

  setting =
   <|
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

  If[
   MemberQ[
    {"Polarized", "OppPrefs"},
    OptionValue["Model"]
    ],
   setting["lambda"] =
    OptionValue["lambda"];
   ];

  (* Reuse original deterministic setting-key function *)
  skey = SettingKey[setting];

  modelOpts =
   FilterRules[
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

  metricOpts =
   FilterRules[
    {
     "q" -> OptionValue["q"],
     "p" -> OptionValue["p"],
     "TopKList" -> ks,
     "MaxRank" -> maxRank,
     "TieBreak" -> tb
     },
    Options[SingleInstanceMetricsGap]
    ];

  If[
   par,

   DistributeDefinitions[
    SingleInstanceMetricsGap,
    AggregateResultsGap,

    SettingKey,
    GenerateProfileByModel,
    PerturbProfileAdjacentSwap,
    MidScoreValue,
    MidScoreOfWinners,

    GenerateICProfile,
    MallowsSampleKendall,
    GenerateMallowsProfile,
    GeneratePolarizedMixtureProfile,
    GenerateOppPrefsMixtureProfile,

    MakeProfile,

    WinnersAll,
    WinnerBorda,
    WinnerCoombs,
    WinnerQRCompromise,
    WinnerSDM,
    WinnerFallback,
    WinnerPlurality,

    TieBreakPick,
    TieKeyDefault,
    ArgMaxCandidates,
    ArgMinCandidates,

    NeverFirstQ,
    TopKAcceptability,
    PairwiseMargins,
    CondorcetWinner
    ];
   ];

  inst =
   If[
    par,

    ParallelTable[
     BlockRandom[
      SeedRandom[
       Hash[
        {baseSeed, skey, rep},
        "SHA256"
        ]
       ];

      With[
       {
        perm =
         GenerateProfileByModel[
          Sequence @@ modelOpts
          ]
        },

       SingleInstanceMetricsGap[
        perm,
        Sequence @@ metricOpts
        ]
       ]
      ],
     {rep, 1, S}
     ],

    Table[
     BlockRandom[
      SeedRandom[
       Hash[
        {baseSeed, skey, rep},
        "SHA256"
        ]
       ];

      With[
       {
        perm =
         GenerateProfileByModel[
          Sequence @@ modelOpts
          ]
        },

       SingleInstanceMetricsGap[
        perm,
        Sequence @@ metricOpts
        ]
       ]
      ],
     {rep, 1, S}
     ]
    ];

  <|
   "Setting" ->
    setting,

   "RuleStats" ->
    AggregateResultsGap[
     inst,
     "TopKList" -> ks
     ]
   |>
  ];


(*======================================================================
  4) RUN EXPERIMENT GRID
======================================================================*)

Options[RunExperimentGridGap] = {
  "Models" -> {"IC", "Mallows", "Polarized"},
  "mList" -> {3, 5, 7, 10},
  "nList" -> {25, 51, 101},
  "phiList" -> {0.6, 0.8, 1.},
  "lambdaList" -> {0.6, 0.7, 0.8, 0.9},
  "qList" -> {2/3},
  "pList" -> {2},
  "S" -> 200,
  "TopKList" -> {1, 2},
  "MaxRank" -> Automatic,
  "Parallel" -> True,
  "BaseSeed" -> 123456,
  "TieBreak" -> "RandomSeeded"
  };


RunExperimentGridGap[
   opts : OptionsPattern[]
   ] :=
 Module[
  {
   models,
   mList,
   nList,
   phiList,
   lambdaList,
   qList,
   pList,
   S,
   ks,
   maxRank,
   par,
   baseSeed,
   tb,
   settings,
   results
   },

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

  If[
   !SubsetQ[ks, {1, 2}],
   Print[
    "ERROR in RunExperimentGridGap: ",
    "TopKList must contain both 1 and 2."
    ];
   Return[$Failed]
   ];

  settings =
   Flatten@Table[
     Switch[
      model,

      "IC",
      Table[
       <|
        "Model" -> "IC",
        "m" -> m,
        "n" -> n,
        "S" -> S,
        "q" -> q,
        "p" -> p
        |>,
       {m, mList},
       {n, nList},
       {q, qList},
       {p, pList}
       ],

      "Mallows",
      Table[
       <|
        "Model" -> "Mallows",
        "m" -> m,
        "n" -> n,
        "phi" -> phi,
        "S" -> S,
        "q" -> q,
        "p" -> p
        |>,
       {m, mList},
       {n, nList},
       {phi, phiList},
       {q, qList},
       {p, pList}
       ],

      "Polarized",
      Table[
       <|
        "Model" -> "Polarized",
        "m" -> m,
        "n" -> n,
        "phi" -> phi,
        "lambda" -> lambda,
        "S" -> S,
        "q" -> q,
        "p" -> p
        |>,
       {m, mList},
       {n, nList},
       {phi, phiList},
       {lambda, lambdaList},
       {q, qList},
       {p, pList}
       ],

      "OppPrefs",
      Table[
       <|
        "Model" -> "OppPrefs",
        "m" -> m,
        "n" -> n,
        "phi" -> phi,
        "lambda" -> lambda,
        "S" -> S,
        "q" -> q,
        "p" -> p
        |>,
       {m, mList},
       {n, nList},
       {phi, phiList},
       {lambda, lambdaList},
       {q, qList},
       {p, pList}
       ],

      _,
      {}
      ],
     {model, models}
     ];

  results =
   Table[
    RunExperimentSettingGap[
     "Model" ->
      Lookup[s, "Model", "IC"],

     "m" ->
      Lookup[s, "m", 5],

     "n" ->
      Lookup[s, "n", 25],

     "phi" ->
      Lookup[s, "phi", 1.],

     "lambda" ->
      Lookup[s, "lambda", 0.75],

     "q" ->
      Lookup[s, "q", 2/3],

     "p" ->
      Lookup[s, "p", 2],

     "S" ->
      Lookup[s, "S", S],

     "TopKList" ->
      ks,

     "MaxRank" ->
      maxRank,

     "Parallel" ->
      par,

     "BaseSeed" ->
      baseSeed,

     "TieBreak" ->
      tb
     ],
    {s, settings}
    ];

  results
  ];


(*======================================================================
  5) LONG-DATA FORMATTER WITH GAP
======================================================================*)

ResultsToLongDatasetGap[
   results_List
   ] :=
 Module[
  {rows},

  rows =
   Flatten@Table[
     With[
      {
       set = res["Setting"],
       stats = res["RuleStats"]
       },

      Flatten@Table[
        With[
         {
          rname = rule,
          rstat = stats[rule]
          },

         Join[
          {
           Join[
            set,
            <|
             "Rule" -> rname,
             "Metric" -> "NeverFirstRate",
             "Value" -> rstat["NeverFirstRate"]
             |>
            ],

           Join[
            set,
            <|
             "Rule" -> rname,
             "Metric" -> "NoiseFlipRate",
             "Value" -> rstat["NoiseFlipRate"]
             |>
            ],

           Join[
            set,
            <|
             "Rule" -> rname,
             "Metric" -> "CondorcetExistRate",
             "Value" -> rstat["CondorcetExistRate"]
             |>
            ],

           Join[
            set,
            <|
             "Rule" -> rname,
             "Metric" -> "CondorcetEfficiency",
             "Value" -> rstat["CondorcetEfficiency"]
             |>
            ],

           Join[
            set,
            <|
             "Rule" -> rname,
             "Metric" -> "MidScore",
             "Value" ->
              Lookup[
               rstat,
               "MidScore",
               Missing["NA"]
               ]
             |>
            ],

           (* Correctly aggregated compromise gap *)
           Join[
            set,
            <|
             "Rule" -> rname,
             "Metric" -> "Gap",
             "Value" ->
              Lookup[
               rstat,
               "Gap",
               Missing["NA"]
               ]
             |>
            ]
           },

          Table[
           Join[
            set,
            <|
             "Rule" -> rname,
             "Metric" ->
              StringJoin[
               "Top",
               ToString[k],
               "Acceptability"
               ],
             "Value" ->
              rstat["TopKAcceptability"][k]
             |>
            ],
           {
            k,
            Keys[rstat["TopKAcceptability"]]
            }
           ]
          ]
         ],
        {
         rule,
         Keys[stats]
         }
        ]
      ],
     {
      res,
      results
      }
     ];

  Dataset[rows]
  ];