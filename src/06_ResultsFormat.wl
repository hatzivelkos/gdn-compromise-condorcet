ClearAll[ResultsToLongDataset, ResultsToWideDataset];

ResultsToLongDataset[results_List] := Module[{rows},
  rows = Flatten @ Table[
    With[{set = res["Setting"], stats = res["RuleStats"]},
      Flatten @ Table[
        With[{rname = rule, rstat = stats[rule]},
          Join[
           {
           Join[set, <|"Rule" -> rname, "Metric" -> "NeverFirstRate", "Value" -> rstat["NeverFirstRate"]|>],
           Join[set, <|"Rule" -> rname, "Metric" -> "NoiseFlipRate", "Value" -> rstat["NoiseFlipRate"]|>],
           Join[set, <|"Rule" -> rname, "Metric" -> "CondorcetExistRate", "Value" -> rstat["CondorcetExistRate"]|>],
           Join[set, <|"Rule" -> rname, "Metric" -> "CondorcetEfficiency", "Value" -> rstat["CondorcetEfficiency"]|>],
           Join[set, <|"Rule" -> rname, "Metric" -> "MidScore", "Value" -> Lookup[rstat, "MidScore", Missing["NA"]]|>]
           },
            Table[
              Join[
                set,
                <|
                  "Rule" -> rname,
                  "Metric" -> ("Top" <> ToString[k] <> "Acceptability"),
                  "Value" -> rstat["TopKAcceptability"][k]
                |>
              ],
              {k, Keys @ rstat["TopKAcceptability"]}
            ]
          ]
        ],
        {rule, Keys[stats]}
      ]
    ],
    {res, results}
  ];
  Dataset[rows]
];

ResultsToWideDataset[results_List] := Module[{rows},
  rows = Flatten @ Table[
    With[{set = res["Setting"], stats = res["RuleStats"]},
      Table[
        With[{rname = rule, rstat = stats[rule]},
          Join[
            set,
            <|
              "Rule" -> rname,
              "NeverFirstRate" -> rstat["NeverFirstRate"],
              "NoiseFlipRate" -> rstat["NoiseFlipRate"],
              "CondorcetExistRate" -> rstat["CondorcetExistRate"],
              "CondorcetEfficiency" -> rstat["CondorcetEfficiency"],
              "MidScore" -> Lookup[rstat, "MidScore", Missing["NA"]]
            |>,
            Association @ Table[
              ("Top" <> ToString[k] <> "Acceptability") -> rstat["TopKAcceptability"][k],
              {k, Keys @ rstat["TopKAcceptability"]}
            ]
          ]
        ],
        {rule, Keys[stats]}
      ]
    ],
    {res, results}
  ];
  Dataset[rows]
];
