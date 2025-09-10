# Dimensional Traits vs. Personality Types
## Sharing an interactive dataviz showing why cramming the complexity of personality into a few types just doesn’t work.

For a guest lecture I’m giving on personality in the business world, I built a simple interactive dataviz that clearly demonstrates just how futile (and a bit naïve) it is to try to squeeze the huge variability of personality characteristics into a handful of types like… [insert your favorite typology 😉].

The dataviz uses a sample of 10k Big Five profiles from [Johnson’s IPIP-NEO-300 dataset](https://osf.io/wxvth/files/osfstorage). With a 3D scatter plot, profile line chart, UMAP dimensionality reduction, and the HDBSCAN clustering algorithm, it shows that even when groups of people with similar profiles do appear—and are large enough to matter (say, at least 1% of the population)—they still show pretty high within-group variability. On top of that, a big chunk (actually, the majority) of profiles can’t be assigned to any stable cluster at all, ending up as “noise” points in HDBSCAN’s terms.

If you find it useful for your teaching or training, check out the dataviz [here](https://sanofi-people-analytics.shinyapps.io/personality-mapping/). If you want to download the data and code to run it locally, you will find it in this GitHub repo.
