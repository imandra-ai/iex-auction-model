# A Model of the IEX Auction Process

This repository contains a model of the IEX Opening/Closing Auction Process, developed by [Aesthetic Integration](https://www.imandra.ai). For further details please refer to [this article](https://medium.com/p/13139fb74098).

This model is primarily based on [IEX's Auction Specification](https://iextrading.com/docs/IEX%20Auction%20Process%20Specification.pdf), also taking information from IEX's other [public documentation](https://iextrading.com/trading/documents/) where necessary. It is intended to model the clearing price calculation and order matching logic for the Opening and Closing auctions. Aspects which are not modelled include: continuous trading, timing constraints (e.g. the Lock-In and Lock-Out behaviours), behaviours specific to Halt/Volatility/IPO auctions.

**DISCLAIMER**: This model was not developed in collaboration with IEX. The information presented here is based on of our understanding of the information disclosed in IEX’s public documentation and may not reflect the intended meaning of that documentation, or the actual behaviour of IEX. In places we discovered that certain behaviours were ambiguous or unspecified. In these cases we made a choice for how the system should behave. Again, this may not reflect the actual or intended behaviour of the IEX auction logic. Any claims made here apply to the model that we have developed, and not to IEX's production system. Any misunderstandings or mistakes are entirely ours.

## Imandra
The model is build using Aesthetic Integration's automated reasoning engine, known as Imandra. The model is encoded using the Imandra Modelling Language (IML) - a subset of the OCaml programming language. Imandra comes equipped with powerful tools for analysing models written in IML, using state of the art techniques from formal verification and artificial intelligence.

## The model
Some of the important aspects of the model:

* order.ml - defines the order datatype. Orders are either market, limit or pegged orders. In the case of limit orders, we make a distinction between orders which originate from the Continuous Book (`ContLimit`) and orders which originate from the Auction Book (`AuctLimit`). Note that market orders always originate from the Auction Book and pegged orders always originate from the Continuous Book.

* priority.ml - defines the priority ordering on orders. At a high-level this ordering can be described as price/display/time priority, but there is some slight subtlety in the way that IEX's smart orders (DPeg and Primary Peg orders) are handled.

* clearing_price.ml - describes how the clearing price is calculated.

* auction.ml - describes the overall auction process. The auction returns the final clearing price and a list of fills which result from the auction.

## Running the model
Since IML is a subset of OCaml, all the source files can be compiled/interpreted using the regular OCaml compiler/REPL. This repository is also setup to use [jbuilder](https://github.com/janestreet/jbuilder). Checking the proofs and using Imandra's reasoning engine to investigate the model requires access to Imandra - please see [here](https://m.imandra.ai/) for the latest information on how to access Imandra.

### Using jbuilder
You can build the project by running `jbuilder build` from the root of the repository. To experiment with the model, you can run `jbuilder utop model` to get a REPL with the model in a module named `Iex_model`. To run the examples run `jbuilder utop examples`. This will run the examples and give you a REPL with the model in a module named `Iex_model` and the examples in a module named `Iex_model_examples`.

### Using Imandra
To load the model in Imandra simply load the file `load_model.iml`, and to check the proofs simply load the file `check_proofs.iml`. These scripts are currently setup to be loaded one directory up from the repository root, i.e. loaded by `:load iex-auction-model/load_model.iml`. This is convenient when mounting the repository inside a Docker container, but this behaviour can be modified by changing the `load_path` variables in the two scripts.
