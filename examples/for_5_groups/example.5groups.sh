#!/bin/bash
PipiDABS                     \
--projectName                ART_Methylome     \
--author                     YongPeng           \
--samplesInformation         example.samples_information.5groups.txt     \
--colorShape                 example.colorShape.5groups.txt               \
--outDir                     1000_Twins_AllPups/C-onlyX_5groups            \
--lowestCoverage             6      \
--numberCoveredSamples       5       \
--RefGenomes                 hg38     \
--fileFormat                 bismarkCoverage     \
--cytosineContext            CpG     \
--qvalue                     0.05     \
--differenceOfMethylation    0.05      \
--windowSize                 100        \
--stepSize                   20          \
--mergeDistance              102          \
--minBases                   2             \
--numCores                   16             \
--SpecificRegions                            \
--minBasesSpecificRegions    3                \
--topPercentageCoverage      1

