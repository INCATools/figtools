# figtools
Scala toolkit for analyzing figshare figures

## Installation Instructions

Make sure you have the java JDK and [sbt](https://www.scala-sbt.org/) installed.

Next, in the top-level `figtools` directory, run:

```
sbt updatePrependScript package
```

Now in the `figtools/bin`, directory, run the `figtools` script.

The first time you run the script, it will download all of its dependencies to
your `~/.ivy2` directory. Then it will display the help message:

```
FigTools 0.1.0
Usage: figtools [options] [command] [command-options]

Available commands: analyze, download, download-all, get, list, search

Description: Recursively analyze and segment a directory full of publication images.
Command: analyze
Usage: figtools analyze <List of FigShare IDs to analyze (optional)>
  --dir  <DIR>
        Directory in which to analyze image files
  --pdf-export-resolution  <DPI>
        Resolution to use when exporting PDFs to images
  --edge-detector  <MODULE>
        Edge detector to use. Possible values: susan imagej

Description: Download figures from figtools into the current directory
Command: download
Usage: figtools download <List of FigShare IDs>
  --url  <URL>
        URL of FigShare API
  --out-dir  <DIRECTORY>
        Output directory

Description: Download *ALL* figures from figtools into the current directory.
Command: download-all
Usage: figtools download-all
  --url  <URL>
        URL of FigShare API
  --out-dir  <DIRECTORY>
        Output directory

Description: Get metadata for FigShare IDs
Command: get
Usage: figtools get <List of FigShare IDs>
  --url  <URL>
        URL of FigShare API

Description: List FigShare IDs
Command: list
Usage: figtools list
  --url  <URL>
        URL of FigShare API

Description: Search for FigShare IDs
Command: search
Usage: figtools search <List of search terms>
  --url  <URL>
        URL of FigShare API
```
