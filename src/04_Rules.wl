(* ::Package:: *)

ClearAll[
  TieKeyDefault,
  TieBreakPick,
  ArgMaxCandidates,
  ArgMinCandidates,
  WinnerBorda,
  WinnerPlurality,
  WinnerCoombs,
  WinnerQRCompromise,
  WinnerSDM,
  WinnerFallback,
  WinnersAll
];

(* ---------------- Tie-breaking ---------------- *)

Options[TieBreakPick] = {
  "TieBreak" -> "RandomSeeded",
  "TieKey" -> Automatic
};

TieKeyDefault[prof_Association] := Hash[prof["perm"], "SHA256"];

TieBreakPick[
  cands_List, prof_Association, ruleName_, stage_,
  opts : OptionsPattern[]
] := Module[
  {o, tb, baseKey, key},
  If[Length[cands] == 0, Return[Missing["NoCandidates"]]];
  If[Length[cands] == 1, Return[First[cands]]];

  o = FilterRules[{opts}, Options[TieBreakPick]];
  tb = ("TieBreak" /. o) /. Options[TieBreakPick];
  baseKey = ("TieKey" /. o) /. Options[TieBreakPick];
  If[baseKey === Automatic, baseKey = TieKeyDefault[prof]];

  Which[
    tb === "SmallestIndex",
      Min[cands],

    tb === "Random",
      RandomChoice[cands],

    tb === "RandomSeeded",
      key = {baseKey, ruleName, stage, Sort[cands]};
      BlockRandom[
        SeedRandom[Hash[key, "SHA256"]];
        RandomChoice[cands]
      ],

    True,
      Min[cands]
  ]
];

(* ---------------- Helpers ---------------- *)

ArgMaxCandidates[vals_List] := Module[{mx = Max[vals]},
  Flatten @ Position[vals, mx]
];

ArgMinCandidates[vals_List] := Module[{mn = Min[vals]},
  Flatten @ Position[vals, mn]
];

(* ---------------- Rules ---------------- *)

Options[WinnerBorda] = Options[TieBreakPick];

WinnerBorda[prof_Association, opts : OptionsPattern[]] := Module[
  {m = prof["m"], pos = prof["pos"], scores, cands},
  scores = Total[m - pos, {1}];
  cands = ArgMaxCandidates[scores];
  TieBreakPick[cands, prof, "Borda", "maxScore", opts]
];



Options[WinnerCoombs] = Options[TieBreakPick];

WinnerCoombs[prof_Association, opts : OptionsPattern[]] := Module[
  {m = prof["m"], n = prof["n"], pos = prof["pos"],
   remaining, round, firstCounts, lastCounts, top, bot,
   maj, winnerPos, elimCands, elim},

  remaining = Range[m];
  maj = Floor[n/2] + 1;

  For[round = 1, round <= m - 1, round++,
    firstCounts = ConstantArray[0, m];
    Do[
      top = remaining[[ First @ Ordering[pos[[v, remaining]], 1] ]];
      firstCounts[[top]]++;
      ,
      {v, 1, n}
    ];

    winnerPos = First[
      Flatten @ Position[firstCounts, _?(# >= maj &), 1, Heads -> False],
      Missing["None"]
    ];
    If[IntegerQ[winnerPos], Return[winnerPos]];

    lastCounts = ConstantArray[0, m];
    Do[
      bot = remaining[[ First @ Ordering[pos[[v, remaining]], -1] ]];
      lastCounts[[bot]]++;
      ,
      {v, 1, n}
    ];

    elimCands = Intersection[ArgMaxCandidates[lastCounts], remaining];
    elim = TieBreakPick[elimCands, prof, "Coombs", {"elim", round}, opts];
    remaining = DeleteCases[remaining, elim];
  ];

  First[remaining]
];

(* QR-style compromise (robust support computation):
   support[a] = #voters with pos[[v,a]] <= r
   We compute support by columns to avoid any non-evaluation issues.
*)


Options[WinnerQRCompromise] = Join[Options[TieBreakPick], {"MaxRank" -> Automatic}];

WinnerQRCompromise[prof_Association, q_?NumericQ, opts : OptionsPattern[]] := Module[
  {m = prof["m"], n = prof["n"], pos = prof["pos"],
   maxRank, qAbs, r, support, qual, best, bestSupport, tbOpts},

  If[!KeyExistsQ[prof, "pos"], Return[Missing["NoPos"]]];
  If[!MatrixQ[pos, IntegerQ], pos = Normal[pos]];

  maxRank = OptionValue["MaxRank"];
  If[maxRank === Automatic, maxRank = m];
  maxRank = Min[maxRank, m];

  (* q can be relative in (0,1] or absolute integer *)
  qAbs = If[q <= 1, Ceiling[q n], Round[q]];
  qAbs = Min[Max[qAbs, 1], n];

  tbOpts = Sequence @@ FilterRules[{opts}, Options[TieBreakPick]];

  For[r = 1, r <= maxRank, r++,
    (* robust column-wise support *)
     support = Table[
          Total[UnitStep[r - pos[[All, a]]]],
          {a, 1, m}
     ];
     support = Developer`ToPackedArray[support];

    qual = Select[Range[m], support[[#]] >= qAbs &];

    If[Length[qual] > 0,
      bestSupport = Max[support[[qual]]];
      best = Select[qual, support[[#]] == bestSupport &];
      Return @ TieBreakPick[best, prof, "QR", {"rank", r, "qAbs", qAbs}, tbOpts];
    ];
  ];

  (* fallback: best support at maxRank *)
   support = Table[
      Total[UnitStep[maxRank - pos[[All, a]]]],
      {a, 1, m}
   ];
   support = Developer`ToPackedArray[support];

  best = ArgMaxCandidates[support];
  TieBreakPick[best, prof, "QR", {"fallback", maxRank, "qAbs", qAbs}, tbOpts]
];


Options[WinnerSDM] = Options[TieBreakPick];

WinnerSDM[prof_Association, p_?NumericQ, opts : OptionsPattern[]] := Module[
  {pos = prof["pos"], div, cands},
  div = Total[Power[pos - 1, p], {1}];
  cands = ArgMinCandidates[div];
  TieBreakPick[cands, prof, "SDM", {"minDiv", p}, opts]
];



(* Fallback = QR with q=1, optionally limited by MaxRank *)
Options[WinnerFallback] = Join[Options[TieBreakPick], {"MaxRank" -> Automatic}];

WinnerFallback[prof_Association, opts : OptionsPattern[]] := Module[
  {maxRank = OptionValue["MaxRank"], tbOpts},
  tbOpts = Sequence @@ FilterRules[{opts}, Options[TieBreakPick]];
  WinnerQRCompromise[prof, 1, "MaxRank" -> maxRank, tbOpts]
];



(*  Plurality  *)
Options[WinnerPlurality] = Join[Options[TieBreakPick], {"MaxRank" -> Automatic}];

WinnerPlurality[prof_Association, opts : OptionsPattern[]] := Module[
  {m = prof["m"], pos = prof["pos"], firstCounts, cands, tbOpts},

  (* robust: osiguraj normalnu integer matricu *)
  If[!MatrixQ[pos, IntegerQ], pos = Normal[pos]];

  (* firstCounts[[a]] = broj glasača koji su kandidata a stavili na 1. mjesto *)
  firstCounts = Table[
    Count[pos[[All, a]], 1],
    {a, 1, m}
  ];

  cands = ArgMaxCandidates[firstCounts];

  tbOpts = Sequence @@ FilterRules[{opts}, Options[TieBreakPick]];
  TieBreakPick[cands, prof, "Plurality", "maxFirst", tbOpts]
];




(* ---------------- All winners ---------------- *)
Options[WinnersAll] = Join[Options[TieBreakPick], {"MaxRank" -> Automatic}];

WinnersAll[prof_Association, q_?NumericQ, p_?NumericQ, opts : OptionsPattern[]] := Module[
  {maxRank = OptionValue["MaxRank"], baseOpts},
  baseOpts = FilterRules[{opts}, Options[TieBreakPick]];
  <|
    "Borda" -> WinnerBorda[prof, Sequence @@ baseOpts],
    "Plurality" -> WinnerPlurality[prof, Sequence @@ baseOpts],  
    "Coombs" -> WinnerCoombs[prof, Sequence @@ baseOpts],
    "QR" -> WinnerQRCompromise[prof, q, "MaxRank" -> maxRank, Sequence @@ baseOpts],
    "SDM" -> WinnerSDM[prof, p, Sequence @@ baseOpts],
    "Fallback" -> WinnerFallback[prof, "MaxRank" -> maxRank, Sequence @@ baseOpts]
  |>
];

