ClearAll[NeverFirstQ, TopKAcceptability, PairwiseMargins, CondorcetWinner];

NeverFirstQ[prof_Association, winner_Integer] := Module[
  {m = prof["m"], pos = prof["pos"]},
  If[winner < 1 || winner > m, Return[Missing["BadWinnerIndex"]]];
  Count[pos[[All, winner]], 1] == 0
];

TopKAcceptability[prof_Association, winner_Integer, k_Integer?Positive] := Module[
  {n = prof["n"], m = prof["m"], pos = prof["pos"], kk},
  If[winner < 1 || winner > m, Return[Missing["BadWinnerIndex"]]];
  If[n <= 0, Return[Missing["BadN"]]];
  kk = Min[k, m];
  N[ Total[UnitStep[kk - pos[[All, winner]]]] / n ]
];

PairwiseMargins[prof_Association] := Module[
  {pos = prof["pos"], n = prof["n"], m = prof["m"], W},
  W = ConstantArray[0, {m, m}];
  Do[
    W += Boole[Outer[Less, pos[[v]], pos[[v]]]],
    {v, 1, n}
  ];
  Developer`ToPackedArray[W - Transpose[W]]
];

ClearAll[CondorcetWinner];

CondorcetWinner[prof_Association] := Module[
  {M = Normal @ PairwiseMargins[prof], m = prof["m"]},
  SelectFirst[
    Range[m],
    (Min[Delete[M[[#]], #]] > 0) &,
    Missing["None"]
  ]
];
