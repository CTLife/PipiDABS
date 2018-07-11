#!/bin/bash
PipiDABS      \
--projectName                ART_Methylome     \
--author                     YongPeng           \
--samplesInformation         example.samples_information.10groups.txt     \
--colorShape                 example.colorShape.10groups.txt               \
--outDir                     1000_Twins_AllPups/A-rmXY            \
--lowestCoverage             6       \
--numberCoveredSamples       5        \
--RefGenomes                 hg38      \
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

