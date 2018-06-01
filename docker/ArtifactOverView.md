# Getting Started Guide

The ICFP18 artifact for paper #42 is bundled as a Docker image that contains a working installation of the Haskell Platform, GHCJS, and the Haskell Automated Assessment Platform (HAAP) that supported the grading and feedback of students of the Informatics Laboratories I course described in the experience report.

* [Dropbox]()
* 

As a generic platform, HAAP is modeled as a collection of plugins and scripting libraries that automate the assessment process.
A concrete HAAP program is then the analog of a batch script that generates feedback in various forms.

Since we will provide sample HAAP scripts that highlight HAAP's visual feedback capabilities, and the Docker container has no X window system, we recommend binding a directory in the host system, say ~/Desktop, to a path inside the container, say /Desktop, by running the container with the arguments:

#Step-by-Step Instructions

We provide a series of incrementally more complex HAAP scripts that highlight HAAP's visual feedback capabilities.

