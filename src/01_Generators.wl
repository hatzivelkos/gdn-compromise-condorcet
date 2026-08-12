ClearAll[GenerateICProfile, MallowsSampleKendall, 
   GenerateMallowsProfile, GeneratePolarizedMixtureProfile]; 

GenerateICProfile[m_Integer?Positive, n_Integer?Positive] := Module[{perm},
  perm = Table[RandomSample[Range[m]], {n}];  (* n rows, each a random permutation of 1..m *)
  Developer`ToPackedArray[perm]
];

MallowsSampleKendall[center_List, phi_?NumericQ] := 
   Module[{m = Length[center], inv, pi, i, weights, probs, k}, 
    If[phi <= 0 || phi > 1, Message[MallowsSampleKendall::phi, 
        phi]; Return[$Failed]; ]; 
     inv = Table[weights = phi^Range[0, i - 1]; 
        probs = weights/Total[weights]; 
        k = RandomChoice[probs -> Range[0, i - 1]]; k, 
       {i, 1, m}]; pi = {}; 
     Do[pi = Insert[pi, i, i - inv[[i]]]; , {i, 1, m}]; 
     center[[pi]]
]; 

GenerateMallowsProfile[m_Integer?Positive, 
    n_Integer?Positive, phi_?NumericQ, center_:Automatic] := 
   Module[{c}, c = If[center === Automatic, Range[m], center]; 
     Developer`ToPackedArray[Table[MallowsSampleKendall[c, phi], 
       {n}]]
]; 

ClearAll[GeneratePolarizedMixtureProfile];

Options[GeneratePolarizedMixtureProfile] = {
  "PoleType" -> "ExtremeSwap",   (* ili "Opposite" *)
  "ICStrength" -> 0.2            (* koliko je IC bliži 1 u odnosu na phi *)
};

GeneratePolarizedMixtureProfile[
   m_Integer?Positive, n_Integer?Positive,
   phi_?NumericQ, lambda_?NumericQ,
   center1_: Automatic, center2_: Automatic,
   opts : OptionsPattern[]
] := Module[
  {phiPol, lam, poleType, icStrength, phiIC,
   c1, c2, c0, mid, pickPole, sampleAround, perm},

  phiPol = Clip[phi, {0, 1}];
  lam = Clip[lambda, {0, 1}];
  poleType = OptionValue["PoleType"];
  icStrength = Clip[OptionValue["ICStrength"], {0, 1}];

  (* IC: uvijek puno bliže uniformnom od polar dijela *)
  phiIC = 1 - icStrength (1 - phiPol);
  phiIC = Clip[phiIC, {0, 1}];

  (* centar IC: slučajan po instanci (determinističan kroz BlockRandom u calleru) *)
  c0 = RandomSample[Range[m]];

  (* polovi *)
  {c1, c2} =
    Which[
      center1 =!= Automatic && center2 =!= Automatic,
        {center1, center2},

      poleType === "Opposite",
        {Range[m], Reverse@Range[m]},

      True, (* "ExtremeSwap" default *)
        mid = If[m >= 3, RandomSample[Range[2, m - 1]], {}];
        {Join[{1}, mid, {m}], Join[{m}, mid, {1}]}
    ];

  (* helper: uzorak iz Mallowsa oko centra *)
  sampleAround[ctr_, ph_] := MallowsSampleKendall[ctr, ph];

  perm = Table[
  If[RandomReal[] < lam,
    (* polar dio *)
    pickPole = If[RandomReal[] < 0.5, c1, c2];
    sampleAround[pickPole, phiPol]
    ,
    (* IC dio: RANDOM centar PO GLASAČU *)
    sampleAround[RandomSample[Range[m]], phiIC]
  ],
  {n}
];

  Developer`ToPackedArray[perm]
];


(*======================Opposite-preferences polarized generator======================*)

GenerateOppositePrefsRankings[
   m_Integer?Positive, n_Integer?Positive,
   phi_?NumericQ, lambda_?NumericQ,
   poles_: Automatic
] := Module[
  {pole1, pole2, nPol, nIC, n1, n2, polRankings, icRankings},

  pole1 = If[poles === Automatic, Range[m], poles[[1]]];
  pole2 = If[poles === Automatic, Reverse@Range[m], poles[[2]]];

  nPol = Round[Clip[lambda, {0, 1}] * n];
  nIC  = n - nPol;

  n1 = Ceiling[nPol/2];
  n2 = Floor[nPol/2];

  polRankings = Join[
    Table[MallowsSampleKendall[pole1, phi], {n1}],
    Table[MallowsSampleKendall[pole2, phi], {n2}]
  ];

  icRankings = Table[RandomSample[Range[m]], {nIC}];

  Developer`ToPackedArray @ RandomSample @ Join[polRankings, icRankings]
];

(* wrapper koji odgovara API-ju iz 05 *)

GenerateOppPrefsMixtureProfile[
   m_Integer?Positive, n_Integer?Positive,
   phi_?NumericQ, lambda_?NumericQ,
   center1_: Automatic, center2_: Automatic
] := Module[{p1, p2},
  p1 = If[center1 === Automatic, Range[m], center1];
  p2 = If[center2 === Automatic, Reverse@Range[m], center2];
  GenerateOppositePrefsRankings[m, n, phi, lambda, {p1, p2}]
];

