This repository contains code to build cosponsorship networks from bills passed in the [lower][ca] and [upper][se] chambers of the Romanian Parliament. 

- [interactive demo](http://briatte.org/parlamentul/)
- [static plots](http://briatte.org/parlamentul/plots.html)

[ca]: http://www.cdep.ro/
[se]: http://www.senat.ro/

# HOWTO

Replicate by running `make.r` in R.

The `build.r` script then assembles the edge lists and plots the networks, with the help of a few routines coded into `functions.r`. Adjust the `plot`, `gexf` and `mode` parameters to skip the plots or to change the node placement algorithm.

# DATA

## Bills

- `page` -- the index page from which the bill was scraped
- `url` -- bill URL
- `ref` -- reference of the form "L1/03.02.2009" (includes the full date for most bills)
- `name` -- short title
- `status` -- when the bill passed, the reference of the law
- `authors` -- bill sponsor
- `n_au` -- total number of sponsors

## Sponsors

The sponsors data has one row per sponsor-legislature.

- `legislature` -- legislature of activity
- `url` -- profile URL
- `name` -- sponsor name
- `sex` -- gender (F/M), imputed from birth information ("aleasă")
- `born` -- year of birth
- `party` -- political party, abbreviated
- `party_dummy` -- numeric dummy to indicate the presence of multiple political parties
- `mdts_ca` -- number of mandates in the Camera (see `nyears`)
- `mdts_se` -- number of mandates in the Senate (see `nyears`)
- `nyears` -- number of past mandates in the same chamber
- `constituency` -- constituency, stored as the string to its Wikipedia English entry
- `photo` -- photo URL, simplified to filename
- `type` -- either "Deputat" or "Senator"
