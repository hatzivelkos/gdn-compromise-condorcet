# Compromise and Condorcet Efficiency under Polarized Preferences

This repository contains the Wolfram Language code, simulation outputs, and reproduction pipeline for the computational results reported in the paper

**Compromise and Condorcet Efficiency under Polarized Preferences**

by Aleksandar Hatzivelkos.

The study compares six voting rules—Borda, Coombs, Fallback, Plurality, Quota Runoff (QR), and Sequentially Discounted Margins (SDM)—under several preference-profile models, with particular emphasis on polarized-candidate (PC) and opposite-preferences (OP) structures.

## Requirements

The code was developed and tested in **Wolfram Mathematica 14.2**.

No external Wolfram Language packages are required.

## Repository structure

```text
gdn-compromise-condorcet/
│
├── notebooks/
│   ├── 01_main_experiments.nb
│   ├── 02_ce_compromise_sweeps.nb
│   └── 03_reproduce_results.nb
│
├── src/
│   └── [Wolfram Language source modules]
│
├── output/
│   ├── tables/
│   └── figures/
│
├── README.md
└── LICENSE
```

The three notebooks have distinct roles:

- `01_main_experiments.nb` runs the main IC, Mallows, PC, and OP experiments and the robustness checks used in the paper.
- `02_ce_compromise_sweeps.nb` runs the paired OP parameter sweeps for QR and SDM used in the compromise–Condorcet analysis.
- `03_reproduce_results.nb` reads the exported result files and reproduces the figures and tables reported in the paper.

The implementation itself is contained in the modular Wolfram Language source files in `src/`.

## Reproducing the results

For a complete reproduction starting from the simulations:

1. Open and evaluate `notebooks/01_main_experiments.nb`.
2. Open and evaluate `notebooks/02_ce_compromise_sweeps.nb`.
3. Open and evaluate `notebooks/03_reproduce_results.nb`.

The first two notebooks generate the simulation results and export the resulting datasets to `output/tables/`.

The third notebook reads these exported datasets and reproduces the figures and tables reported in the paper.

If only the reported figures and tables are to be reproduced, the simulation notebooks do not need to be rerun. The exported simulation datasets are included in the repository and can be read directly by `03_reproduce_results.nb`.

## Reproducibility

The simulations use fixed parameter grids corresponding to those reported in the paper and deterministic seeding with base seed

123456

to make the computational experiments reproducible.

Whenever a voting rule produces a tie, one of the tied candidates is selected uniformly at random. Random tie-breaking is incorporated into the Monte Carlo procedure, and the reported quantities are averages across simulation replications.

For the QR and SDM parameter sweeps, alternative parameter values are evaluated on paired profile samples. Comparisons across parameter values within a given experimental setting are therefore based on the same generated preference profiles.

## Main outputs

The repository reproduces the principal computational results reported in the paper, including:

- baseline behavior under impartial culture (IC);
- Mallows central-winner robustness;
- Top-2 acceptability and compromise-gap results under polarized-candidate (PC) profiles;
- MidScore results under opposite-preferences (OP) profiles;
- Condorcet-efficiency comparisons under PC and OP polarization;
- QR and SDM compromise–Condorcet parameter trajectories; and
- the QR outcome-dispersion results reported in Table 1.

Generated figures are stored in `output/figures/`, while numerical result files and reproduced tables are stored in `output/tables/`.

## Citation

If you use the code or data from this repository, please cite the associated paper:

Aleksandar Hatzivelkos, *Compromise and Condorcet Efficiency under Polarized Preferences*.

Full publication details will be added after publication.

## License

The licensing terms for the repository are provided in the `LICENSE` file.