(*======================================================================
  05c_ExperimentRunnerGapPaired.wl

  Paired-profile CE / Compromise-Gap sweep for QR and SDM.

  PURPOSE
  -------
  For each fixed profile environment
      (Model, m, n, phi, lambda)
  generate ONE common Monte Carlo sample of S profiles.

  On every generated profile:
    - evaluate QR for every q in qList;
    - evaluate SDM for every p in pList.

  Thus all parameter values are compared on exactly the same profiles.

  IMPORTANT
  ---------
  1. Profile seeds DO NOT depend on p or q.
  2. Gap is computed at the single-profile level:
         Gap = 1 - Top1/Top2
     and only then averaged across replications.
  3. QR is swept only over q.
  4. SDM is swept only over p.
  5. Existing 05_ExperimentRunner.wl and
     05b_ExperimentRunnerGap.wl are not redefined.
======================================================================*)


ClearAll[
  PairedProfileKey,
  RuleMetricsGapPaired,
  SingleProfileSweepGapPaired,
  AggregateRuleParameterGapPaired,
  RunExperimentSettingGapPaired,
  RunExperimentGridGapPaired,
  ResultsToLongDatasetGapPaired
];


(*======================================================================
  1) PROFILE KEY

  Deliberately excludes q and p.
  Profiles therefore depend only on the preference environment.
======================================================================*)

PairedProfileKey[setting_Association] :=
  Hash[
    Normal@KeyDrop[
      setting,
      {"q", "p", "qList", "pList"}
    ],
    "SHA256"
  ];


(*======================================================================
  2) METRICS FOR ONE RULE WINNER ON ONE PROFILE
======================================================================*)

RuleMetricsGapPaired[
   prof_Association,
   winner_Integer,
   cw_
   ] :=
 Module[
  {
   top1,
   top2,
   gap,
   cwExists
   },

  top1 =
    TopKAcceptability[
      prof,
      winner,
      1
    ];

  top2 =
    TopKAcceptability[
      prof,
      winner,
      2
    ];

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

  cwExists =
    cw =!= Missing["None"];

  <|
    "Top1" -> N[top1],
    "Top2" -> N[top2],
    "Gap" -> gap,

    "CondorcetExists" ->
      Boole[cwExists],

    "CondorcetHit" ->
      If[
        cwExists,
        Boole[winner === cw],
        0
      ],

    "MidScore" ->
      N@MidScoreOfWinners[
        prof["m"],
        winner
      ]
  |>
];


(*======================================================================
  3) ONE PROFILE, COMPLETE q/p SWEEP

  QR:
      evaluate qList, keeping p fixed at pAnchor.

  SDM:
      evaluate pList, keeping q fixed at qAnchor.

  Since QR does not depend on p and SDM does not depend on q,
  this avoids the unnecessary q x p Cartesian product.

  Evaluation randomness is separated from profile-generation randomness.
======================================================================*)

Options[SingleProfileSweepGapPaired] = {
  "qList" -> {2/3},
  "pList" -> {2.},
  "qAnchor" -> 2/3,
  "pAnchor" -> 2.,
  "MaxRank" -> Automatic,
  "TieBreak" -> "RandomSeeded",
  "BaseSeed" -> 123456,
  "ProfileKey" -> 0,
  "Replication" -> 1
};


SingleProfileSweepGapPaired[
   perm_?MatrixQ,
   opts : OptionsPattern[]
   ] :=
 Module[
  {
   prof,
   cw,
   qList,
   pList,
   qAnchor,
   pAnchor,
   maxRank,
   tb,
   baseSeed,
   profileKey,
   rep,
   qrResults,
   sdmResults
   },

  prof = MakeProfile[perm];

  If[
    !AssociationQ[prof],
    Return[Missing["BadProfile"]]
  ];

  cw = CondorcetWinner[prof];

  qList = N@OptionValue["qList"];
  pList = N@OptionValue["pList"];

  qAnchor = N@OptionValue["qAnchor"];
  pAnchor = N@OptionValue["pAnchor"];

  maxRank = OptionValue["MaxRank"];
  tb = OptionValue["TieBreak"];

  baseSeed = OptionValue["BaseSeed"];
  profileKey = OptionValue["ProfileKey"];
  rep = OptionValue["Replication"];


  (*----------------------------------------------------------*)
  (* QR sweep                                                 *)
  (*----------------------------------------------------------*)

  qrResults =
    Association@Table[
      Module[
        {
         winners,
         w,
         evalSeed
        },

        evalSeed =
          Hash[
            {
             baseSeed,
             profileKey,
             rep,
             "QR",
             q
            },
            "SHA256"
          ];

        winners =
          BlockRandom[
            SeedRandom[evalSeed];

            WinnersAll[
              prof,
              q,
              pAnchor,
              "MaxRank" -> maxRank,
              "TieBreak" -> tb
            ]
          ];

        w = winners["QR"];

        q ->
          RuleMetricsGapPaired[
            prof,
            w,
            cw
          ]
      ],
      {q, qList}
    ];


  (*----------------------------------------------------------*)
  (* SDM sweep                                                *)
  (*----------------------------------------------------------*)

  sdmResults =
    Association@Table[
      Module[
        {
         winners,
         w,
         evalSeed
        },

        evalSeed =
          Hash[
            {
             baseSeed,
             profileKey,
             rep,
             "SDM",
             p
            },
            "SHA256"
          ];

        winners =
          BlockRandom[
            SeedRandom[evalSeed];

            WinnersAll[
              prof,
              qAnchor,
              p,
              "MaxRank" -> maxRank,
              "TieBreak" -> tb
            ]
          ];

        w = winners["SDM"];

        p ->
          RuleMetricsGapPaired[
            prof,
            w,
            cw
          ]
      ],
      {p, pList}
    ];


  <|
    "QR" -> qrResults,
    "SDM" -> sdmResults
  |>
];


(*======================================================================
  4) AGGREGATE ONE RULE / ONE PARAMETER VALUE
======================================================================*)

ClearAll[safeMeanGapPaired];

safeMeanGapPaired[x_List] :=
 Module[
  {y = DeleteMissing[x]},

  If[
    Length[y] == 0,
    Missing["NA"],
    N@Mean[y]
  ]
];


AggregateRuleParameterGapPaired[
   inst_List,
   rule_String,
   parameter_?NumericQ
   ] :=
 Module[
  {
   vals,
   nInst,
   cwCount,
   ce,
   gap,
   top1,
   top2,
   midScore
   },

  nInst = Length[inst];

  If[
    nInst == 0,
    Return[Missing["NoInstances"]]
  ];

  vals =
    Lookup[
      Lookup[inst, rule],
      parameter,
      Missing["ParameterAbsent"]
    ];

  vals = DeleteMissing[vals];

  If[
    vals === {},
    Return[Missing["NoParameterData"]]
  ];


  cwCount =
    Total[
      Lookup[
        vals,
        "CondorcetExists",
        0
      ]
    ];


  ce =
    If[
      cwCount == 0,

      Missing["NA"],

      N[
        Total[
          Lookup[
            vals,
            "CondorcetHit",
            0
          ]
        ] / cwCount
      ]
    ];


  gap =
    safeMeanGapPaired[
      Lookup[
        vals,
        "Gap",
        Missing["NA"]
      ]
    ];


  top1 =
    safeMeanGapPaired[
      Lookup[
        vals,
        "Top1",
        Missing["NA"]
      ]
    ];


  top2 =
    safeMeanGapPaired[
      Lookup[
        vals,
        "Top2",
        Missing["NA"]
      ]
    ];


  midScore =
    safeMeanGapPaired[
      Lookup[
        vals,
        "MidScore",
        Missing["NA"]
      ]
    ];


  <|
    "N" -> Length[vals],

    "Top1Acceptability" ->
      top1,

    "Top2Acceptability" ->
      top2,

    (* mean of PER-PROFILE Gap values *)
    "Gap" ->
      gap,

    "CondorcetEfficiency" ->
      ce,

    "CondorcetExistRate" ->
      N[cwCount/Length[vals]],

    "MidScore" ->
      midScore
  |>
];


(*======================================================================
  5) RUN ONE FIXED PREFERENCE ENVIRONMENT

  One environment =
      Model, m, n, phi, lambda

  The same S generated profiles are used for every q and p.
======================================================================*)

Options[RunExperimentSettingGapPaired] = {
  "Model" -> "Polarized",
  "m" -> 7,
  "n" -> 51,
  "phi" -> 0.15,
  "lambda" -> 0.5,
  "center1" -> Automatic,
  "center2" -> Automatic,

  "qList" -> {2/3},
  "pList" -> {2.},

  "qAnchor" -> 2/3,
  "pAnchor" -> 2.,

  "S" -> 200,

  "MaxRank" -> Automatic,
  "Parallel" -> True,
  "BaseSeed" -> 123456,
  "TieBreak" -> "RandomSeeded"
};


RunExperimentSettingGapPaired[
   opts : OptionsPattern[]
   ] :=
 Module[
  {
   baseSetting,
   profileKey,
   modelOpts,
   qList,
   pList,
   qAnchor,
   pAnchor,
   S,
   par,
   baseSeed,
   maxRank,
   tb,
   inst,
   qrStats,
   sdmStats,
   qrResults,
   sdmResults
   },

  qList = N@OptionValue["qList"];
  pList = N@OptionValue["pList"];

  qAnchor = N@OptionValue["qAnchor"];
  pAnchor = N@OptionValue["pAnchor"];

  S = OptionValue["S"];

  par =
    TrueQ[
      OptionValue["Parallel"]
    ];

  baseSeed =
    OptionValue["BaseSeed"];

  maxRank =
    OptionValue["MaxRank"];

  tb =
    OptionValue["TieBreak"];


  (*----------------------------------------------------------*)
  (* Base environment: NO q and NO p                          *)
  (*----------------------------------------------------------*)

  baseSetting =
    <|
      "Model" -> OptionValue["Model"],
      "m" -> OptionValue["m"],
      "n" -> OptionValue["n"],
      "phi" -> OptionValue["phi"],
      "S" -> S,
      "MaxRank" -> maxRank
    |>;


  If[
    MemberQ[
      {"Polarized", "OppPrefs"},
      OptionValue["Model"]
    ],

    baseSetting["lambda"] =
      OptionValue["lambda"];
  ];


  profileKey =
    PairedProfileKey[
      baseSetting
    ];


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


  (*----------------------------------------------------------*)
  (* Parallel dependencies                                    *)
  (*----------------------------------------------------------*)

  If[
    par,

    DistributeDefinitions[
      PairedProfileKey,
      RuleMetricsGapPaired,
      SingleProfileSweepGapPaired,
      safeMeanGapPaired,
      AggregateRuleParameterGapPaired,

      GenerateProfileByModel,
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

      TopKAcceptability,
      PairwiseMargins,
      CondorcetWinner
    ];
  ];


  (*----------------------------------------------------------*)
  (* COMMON PROFILE SAMPLE                                    *)
  (*                                                          *)
  (* profileSeed excludes q and p                             *)
  (*----------------------------------------------------------*)

  inst =
    If[
      par,

      ParallelTable[
        Module[
          {
           profileSeed,
           perm
          },

          profileSeed =
            Hash[
              {
               baseSeed,
               profileKey,
               rep
              },
              "SHA256"
            ];

          perm =
            BlockRandom[
              SeedRandom[
                profileSeed
              ];

              GenerateProfileByModel[
                Sequence @@ modelOpts
              ]
            ];


          SingleProfileSweepGapPaired[
            perm,

            "qList" -> qList,
            "pList" -> pList,

            "qAnchor" -> qAnchor,
            "pAnchor" -> pAnchor,

            "MaxRank" -> maxRank,
            "TieBreak" -> tb,

            "BaseSeed" -> baseSeed,
            "ProfileKey" -> profileKey,
            "Replication" -> rep
          ]
        ],
        {rep, 1, S}
      ],

      Table[
        Module[
          {
           profileSeed,
           perm
          },

          profileSeed =
            Hash[
              {
               baseSeed,
               profileKey,
               rep
              },
              "SHA256"
            ];

          perm =
            BlockRandom[
              SeedRandom[
                profileSeed
              ];

              GenerateProfileByModel[
                Sequence @@ modelOpts
              ]
            ];


          SingleProfileSweepGapPaired[
            perm,

            "qList" -> qList,
            "pList" -> pList,

            "qAnchor" -> qAnchor,
            "pAnchor" -> pAnchor,

            "MaxRank" -> maxRank,
            "TieBreak" -> tb,

            "BaseSeed" -> baseSeed,
            "ProfileKey" -> profileKey,
            "Replication" -> rep
          ]
        ],
        {rep, 1, S}
      ]
    ];


  (*----------------------------------------------------------*)
  (* Aggregate QR over q                                      *)
  (*----------------------------------------------------------*)

  qrStats =
    Association@Table[
      q ->
        AggregateRuleParameterGapPaired[
          inst,
          "QR",
          q
        ],
      {q, qList}
    ];


  (*----------------------------------------------------------*)
  (* Aggregate SDM over p                                     *)
  (*----------------------------------------------------------*)

  sdmStats =
    Association@Table[
      p ->
        AggregateRuleParameterGapPaired[
          inst,
          "SDM",
          p
        ],
      {p, pList}
    ];


  <|
    "Setting" ->
      baseSetting,

    "ProfileKey" ->
      profileKey,

    "QRStats" ->
      qrStats,

    "SDMStats" ->
      sdmStats
  |>
];


(*======================================================================
  6) RUN GRID OF PREFERENCE ENVIRONMENTS

  NOTE:
  q and p are NOT dimensions of this outer grid.

  For the current experiment:
      4 lambda values = only 4 outer settings,
  each based on one common sample of S profiles.
======================================================================*)

Options[RunExperimentGridGapPaired] = {
  "Models" -> {"Polarized"},
  "mList" -> {7},
  "nList" -> {51},
  "phiList" -> {0.15},
  "lambdaList" -> {0.2, 0.5, 0.8, 1.0},

  "qList" -> {2/3},
  "pList" -> {2.},

  "qAnchor" -> 2/3,
  "pAnchor" -> 2.,

  "S" -> 200,

  "MaxRank" -> Automatic,
  "Parallel" -> True,
  "BaseSeed" -> 123456,
  "TieBreak" -> "RandomSeeded"
};


RunExperimentGridGapPaired[
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
   qAnchor,
   pAnchor,
   S,
   maxRank,
   par,
   baseSeed,
   tb,
   settings,
   results
   },

  models =
    OptionValue["Models"];

  mList =
    OptionValue["mList"];

  nList =
    OptionValue["nList"];

  phiList =
    OptionValue["phiList"];

  lambdaList =
    OptionValue["lambdaList"];

  qList =
    N@OptionValue["qList"];

  pList =
    N@OptionValue["pList"];

  qAnchor =
    N@OptionValue["qAnchor"];

  pAnchor =
    N@OptionValue["pAnchor"];

  S =
    OptionValue["S"];

  maxRank =
    OptionValue["MaxRank"];

  par =
    OptionValue["Parallel"];

  baseSeed =
    OptionValue["BaseSeed"];

  tb =
    OptionValue["TieBreak"];


  settings =
    Flatten@Table[
      Switch[
        model,

        "Polarized",
        Table[
          <|
            "Model" -> "Polarized",
            "m" -> m,
            "n" -> n,
            "phi" -> phi,
            "lambda" -> lambda
          |>,
          {m, mList},
          {n, nList},
          {phi, phiList},
          {lambda, lambdaList}
        ],

        "OppPrefs",
        Table[
          <|
            "Model" -> "OppPrefs",
            "m" -> m,
            "n" -> n,
            "phi" -> phi,
            "lambda" -> lambda
          |>,
          {m, mList},
          {n, nList},
          {phi, phiList},
          {lambda, lambdaList}
        ],

        "Mallows",
        Table[
          <|
            "Model" -> "Mallows",
            "m" -> m,
            "n" -> n,
            "phi" -> phi
          |>,
          {m, mList},
          {n, nList},
          {phi, phiList}
        ],

        "IC",
        Table[
          <|
            "Model" -> "IC",
            "m" -> m,
            "n" -> n
          |>,
          {m, mList},
          {n, nList}
        ],

        _,
        {}
      ],
      {model, models}
    ];


  results =
    Table[
      RunExperimentSettingGapPaired[
        "Model" ->
          Lookup[s, "Model", "Polarized"],

        "m" ->
          Lookup[s, "m", 7],

        "n" ->
          Lookup[s, "n", 51],

        "phi" ->
          Lookup[s, "phi", 0.15],

        "lambda" ->
          Lookup[s, "lambda", 0.5],

        "qList" ->
          qList,

        "pList" ->
          pList,

        "qAnchor" ->
          qAnchor,

        "pAnchor" ->
          pAnchor,

        "S" ->
          S,

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
  7) LONG-DATA FORMATTER

  Produces one row per:
      environment x rule x parameter x metric

  For QR:
      q varies, p = Missing["NotApplicable"]

  For SDM:
      p varies, q = Missing["NotApplicable"]
======================================================================*)

ResultsToLongDatasetGapPaired[
   results_List
   ] :=
 Module[
  {
   rowsQR,
   rowsSDM,
   metricNames
   },


  metricNames = {
    "Top1Acceptability",
    "Top2Acceptability",
    "Gap",
    "CondorcetEfficiency",
    "CondorcetExistRate",
    "MidScore"
  };


  (*----------------------------------------------------------*)
  (* QR rows                                                  *)
  (*----------------------------------------------------------*)

  rowsQR =
    Flatten@Table[
      Module[
        {
         set = res["Setting"],
         stats = res["QRStats"]
        },

        Flatten@Table[
          Table[
            Join[
              set,

              <|
                "Rule" -> "QR",
                "q" -> q,
                "p" -> Missing["NotApplicable"],
                "Metric" -> metric,
                "Value" -> stats[q][metric]
              |>
            ],
            {metric, metricNames}
          ],
          {q, Keys[stats]}
        ]
      ],
      {res, results}
    ];


  (*----------------------------------------------------------*)
  (* SDM rows                                                 *)
  (*----------------------------------------------------------*)

  rowsSDM =
    Flatten@Table[
      Module[
        {
         set = res["Setting"],
         stats = res["SDMStats"]
        },

        Flatten@Table[
          Table[
            Join[
              set,

              <|
                "Rule" -> "SDM",
                "q" -> Missing["NotApplicable"],
                "p" -> p,
                "Metric" -> metric,
                "Value" -> stats[p][metric]
              |>
            ],
            {metric, metricNames}
          ],
          {p, Keys[stats]}
        ]
      ],
      {res, results}
    ];


  Dataset[
    Join[
      rowsQR,
      rowsSDM
    ]
  ]
];