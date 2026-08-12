(* ::Package:: *)

ClearAll[MakeProfile];

MakeProfile[perm_?MatrixQ] := Module[
  {n, m, pos},

  n = Length[perm];
  m = Length[First[perm]];

  (* pos[[v,a]] = position (rank) of alternative a in perm[[v]] *)
  pos = Table[
    First@First@Position[perm[[v]], a],
    {v, 1, n}, {a, 1, m}
  ];

  <|
    "m" -> m,
    "n" -> n,
    "perm" -> Developer`ToPackedArray[perm],
    "pos" -> Developer`ToPackedArray[pos]
  |>
];
