# GCoM
Code in R for validation of GCoM energy activity, emissions and mitigation policies, and comparison against EDGAR

Technical validation of energy activity, emissions and mitigation policies
A quality-harnessing computational procedure is developed, aiming at enhancing the internal coherence and contents reliability of the data sets that are published in https://data.jrc.ec.europa.eu/collection/id-00354. 

R code: GCoM_validation_emissions_policies
Part I. Energy
Part II. Emissions
Part III. Energy supply
Part IV. Policies
Part V. Energy and emissions inventories for publication

Comparison with EDGAR (v7)
The EDGAR database is used to validate the GHG emissions data available in the GCoM. Each one follows a different methodology, being there some disagreement in terms of spatial/geographical coverage, emissions sources, emissions allocation or the type of emissions considered. Therefore, controlling for the divergences between both approaches, the comparison between the GCoM and EDGAR data is performed on direct CO_2 emissions, taking only the buildings (IPCC codes 1A4 and 1A5) and road transport (IPCC code 1a3b) sectors. Focus is placed on a sample of European cities (enabling the geographical matching between the GCoM cities identified by a LAU code, and the EDGAR grid-based emissions), taking the GCoM annual inventories for every year  after 1999, for all of the available cities having a LAU code and population above 50,000 inhabitants, and matching them with the corresponding geographical polygon of the EDGAR grid.  

To develop this analysis, emissions are extracted from the EDGAR database for each one of the cities (polygons) included in this study, combining different LAUs if needed to have the most complete match based on the LAU(s) and the EDGAR gridded data.  Greater uncertainty should be associated to small areas, as such a match is more difficult to implement given the resolution of the EDGAR grid. Hence, the sample of cities to compare is composed of medium to big cities, although there are some small polygons in the sample, where 54 cities have surface of less than 100 km2.

In total, there are 322 different cities. For emissions in the buildings and road transport sectors, there are 587 and 582 observations, respectively, which include different yearly inventories for the same city having a match in both GCoM and EDGAR datasets.
