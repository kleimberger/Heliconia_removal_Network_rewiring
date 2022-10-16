# Plant-hummingbird pollination networks exhibit minimal rewiring after experimental removal of a locally abundant plant species

Analyses for [network rewiring manuscript](https://www.biorxiv.org/content/10.1101/2022.10.05.511021v1.abstract) (third chapter of dissertation)

**Question** 

How do interactions between plants and hummingbirds rearrange after a plant species is lost from the community? Do hummingbirds expand their diet to feed on new plant species? 

**Approach**

I temporarily removed the inflorescences of a locally common flowering plant, *Heliconia tortuosa*, from forested study sites surrounding the Las Cruces Biological Station in southern Costa Rica. This experiment followed a Before-After-Control-Impact design.

I gathered data on plant-hummingbird interactions using two methods:

1.	Trail cameras positioned at flowering plants

2.	Pollen grains from captured hummingbirds
    - I swabbed the feathers of hummingbirds captured in mist nets. Using a microscope, plants can be identified based distinctive characteristics of pollen grains.

To understand if hummingbirds expanded their diet to feed on new plant species, I calculated several metrics of ecological specialization. Most of these metrics were calculated from interaction networks (network-level metric: H2’, species-level metrics: d’, Species Specificity Index). I also examined one non-network metric, the number of pollen species per individual hummingbird.

To investigate rearrangement of interactions, I calculated a measure of interaction turnover (loss/gain of interactions).

To determine whether *Heliconia* removal affected these response variables, I conducted statistical analyses generalized linear mixed models (beta and Poisson regression).

**Results**

Limited evidence that hummingbirds changed their foraging behavior to cope with removal of a locally common resource.

**Repository organization**

```bash

code
   |-- 01_filtering_summarizing_camera_data.Rmd
   |-- 02_filtering_summarizing_pollen_data.Rmd
   |-- 03_summarizing_video_effort_and_camera_methods.Rmd
   |-- 04_creating_networks_calculating_network_metrics.Rmd
   |-- 05_creating_metanetworks.Rmd
   |-- 06_analyzing_sampling_completeness_for_experiment.Rmd
   |-- 07_analyzing_network_metrics_for_experiment.Rmd
   |-- 08_analyzing_network_metrics_for_sampling_method.Rmd
   |-- 09_making_toy_networks_for_turnover_figure.Rmd
   |-- helper_functions
   |   |-- Build_and_visualize_networks_for_experiment.R
   |   |-- Calculate_basic_summary_stats.R
   |   |-- Extract_data_from_safely_and_quietly_lists.R
   |   |-- Modeling_helper_functions.R
   |   |-- Plotting_helper_functions.R
   |   |-- Summarize_camera_data.R
   |   |-- Transform_proportion_for_betareg.R
   |-- knitted_markdown_files
   |   |-- 01_filtering_summarizing_camera_data.html
   |   |-- 02_filtering_summarizing_pollen_data.html
   |   |-- 03_summarizing_video_effort_and_camera_methods.html
   |   |-- 04_creating_networks_calculating_network_metrics.html
   |   |-- 05_creating_metanetworks.html
   |   |-- 06_analyzing_sampling_completeness_for_experiment.html
   |   |-- 07_analyzing_network_metrics_for_experiment.html
   |   |-- 08_analyzing_network_metrics_for_sampling_method.html
   |   |-- 09_making_toy_networks_for_turnover_figure.html

```
