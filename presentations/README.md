# Presentations

In this folder the key presentations to understand the inner workings of the Density Matrix (DM) are included.

- 🟢 `DM`: A basic presentation of the tool (updated with ROC AUC, confusion matrix, probability diagnostics, and polar coordinate visualization). Study case: penguins.
- 🟢 `DM_hard`: Harder binary task (iris versicolor vs virginica). Streamlined workflow with split, fit (`dm_fit`), predict, metrics (balanced accuracy, accuracy, sensitivity, specificity, F1, kappa, MCC), ROC AUC, confusion matrix, and probability diagnostics; optional Cartesian variant (`use_polar = FALSE`).
- 🟡 `DM_vs_f_hat`: Head-to-head comparison of the DM pipeline vs a plain kernel density baseline on standardized raw 2D features (same split and bandwidth). Includes side-by-side metrics, ROC AUC, confusion matrices, and probability distributions.
  - 🟡 `DMnML`: A comparison with traditional ML approaches.
  - 🟡 `DMnPCA`: A comprehensive comparison with PCA (includes metrics, ROC curves, confusion matrices, and return_coords demo).
- 🟠 `Presenting_qml`: A presentation template for **showcasing** the qml package.
- 🔴 `DM_fine_tuning`:

## Color keys

- 🟢 Green: Ready or working except for visualization issues.
- 🟡 Yellow: Needs review or minor fixes.
- 🟠 Orange: Under development.
- 🔴 Red: Placeholder.
