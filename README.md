# Project Title: Quantifying In-Air Receiver Movement in NFL Tracking Data (2023 Big Data Bowl)

**Team:**
* Eleanor Brothers
* Kalynn Willis
* Jasmine Peck

## 1. Project Overview

This project analyzes NFL Next Gen Stats tracking data from the 2026/2023 Big Data Bowl to understand how offensive players—specifically targeted receivers—move while the ball is in the air during passing plays.

The goal is to develop a receiver movement metric that captures a player’s ability to create or maintain separation during ball flight. This metric will allow us to:

* Evaluate receiver skill independent of quarterback accuracy
* Quantify “late separation” behaviors
* Compare players and teams
* Link movement to completion probability
* Build interpretable analytics that could be used by coaches, scouts, and fans

Our broader aim is to develop this as a strong candidate for a Big Data Bowl–style submission and to meet the course requirements for Project 3.

## 2. Core Research Question

**How well do receivers create or maintain separation from defenders while the ball is in the air, and how does this vary across players, teams, and situations?**

We focus specifically on the targeted receiver and their spatial relationship to the nearest defender.

We will quantify this as:

**In-Air Separation Added (IASA)**

$$
IASA = d_{catch} - d_{throw}
$$

Where:
* $d_{throw}$ = separation between targeted receiver and nearest defender at the throw frame
* $d_{catch}$ = separation at the final in-air frame (≈ catch or ball arrival)

**Interpretation:**
* **Positive IASA:** receiver gained separation while ball was in flight
* **Negative IASA:** defender closed in / covered better during flight

This metric is intuitive, football-grounded, and computationally feasible.

## 3. Data Sources

We will use the Big Data Bowl 2026 (using 2023 season) tracking data, specifically:

### Input Files
`input_2023_wXX.csv`
Contains data from snap → throw.

Key columns include:
* player IDs & roles (`nfl_id`, `player_role`)
* player attributes (speed, acceleration, direction, orientation)
* raw coordinates (`x`, `y`)
* frame info (`game_id`, `play_id`, `frame_id`)
* pre-throw movement
* ball landing location (`ball_land_x`, `ball_land_y`)

### Output Files
`output_2023_wXX.csv`
Contains data from throw → ball arrival (in-air frames only).

Key column:
* `player_to_predict` identifies players with in-air trajectories available (e.g., targeted receiver, certain defenders).

## 4. Planned Methods

### (A) Data Assembly
* Load all `input_*` and `output_*` files for selected weeks (start with Week 1).
* Join input and output frames by `game_id` + `play_id` + `nfl_id`.
* Identify the targeted receiver using `player_role == "Targeted Receiver"`.
* Identify nearest defender at each frame.

### (B) Feature Engineering
For each play:
1. Compute separation at throw frame
2. Compute separation during each in-air frame
3. Extract separation at final in-air frame
4. Compute IASA

Also compute optional secondary metrics:
* Path Efficiency
* Change in angle to ball
* WR/DB relative speed
* Air time of throw
* Depth and direction of target
* Generate a tidy dataset with one row per play.

### (C) Exploratory Data Analysis (EDA)
* Distribution of IASA
* Relationship between IASA and:
    * depth of target
    * air time
    * field location
    * outcome (completion / YAC where available)
* Player-level summaries
* Team-level summaries

### (D) Statistical/ML Modeling
We will incorporate machine learning methods consistent with course expectations.

**Hierarchical Mixed-Effects Model**
Estimate “true” receiver ability to generate in-air separation:

$$
IASA_{play} = \alpha + \beta X + u_{receiver} + u_{team} + \epsilon
$$

**Predictive Model**
Using pre-throw features to predict IASA or completion:
* XGBoost / Random Forest
* Penalized regression (LASSO/Ridge)
* Or simple RNN for sequential positions (optional stretch goal)

Outputs:
* Feature importance
* Partial dependence plots
* Model performance metrics

### (E) Visualization
* Static plots of WR/DB separation
* Play-level animations (optional)
* Receiver rankings
* Team-level heatmaps
* Broadcast-style graphics (if time)

## 5. Deliverables (aligned with course project requirements)

### Executive Summary (≤ 500 words)
* Non-technical explanation of IASA
* High-level results
* Why the metric matters to coaches/teams
* No figures, code, or jargon

### Technical Report
* Data description
* Methodology
* Reproducible code (R or Python)
* Tables, charts, visualizations
* Model descriptions
* Limitations + future extensions

### Presentation (8–10 minutes)
* Motivation
* Data
* Approach
* Main findings
* Future directions

## 6. File Structure

```
project/
│
├── data/
│   ├── input_2023_w01.csv
│   ├── output_2023_w01.csv
│   └── (other weeks)
│
├── src/
│   ├── 00_load_data.R
│   ├── 01_engineer_features.R
│   ├── 02_compute_IASA.R
│   ├── 03_models.R
│   └── 04_visualizations.R
│
├── report/
│   ├── executive_summary.qmd
│   └── technical_report.qmd
│
└── README.md
```

## 7. Immediate Next Steps
1. Load Week 1 input + output data into R.
2. Identify:
    * a play
    * its targeted receiver
    * throw frame & in-air frames
3. Compute IASA for 5–10 plays to verify the pipeline works.
4. Expand to all Week 1.
5. Build first plots (histogram of IASA, scatter vs. depth).
6. Once this is stable, we’ll scale to more weeks and start modeling.

