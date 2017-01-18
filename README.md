Polyphase Filter Bank [![Build Status](https://travis-ci.org/ucb-art/pfb.svg?branch=master)](https://travis-ci.org/ucb-art/pfb)
=======================


# Overview

This project contains an implementation of a [Casper](https://casper.berkeley.edu/wiki/The_Polyphase_Filter_Bank_Technique)-style PFB.
It consists of a bank of Finite Impulse Response (FIR) filters to shape the input time-series data in such a way that the spectral channels are properly windowed, reducing spectral leakage between frequency bins. 
The weights for these filters are programmable at either compile time (maps to a ROM) or run time (maps to a RAM).

# Usage

## Building

Build the dependencies by typing `make libs`.
To build the Verilog and IP-Xact output, type `make verilog`.
Results are placed in a `generated-src` directory.

## Testing

To test the block, type `make test`.
This runs the block tester in the `src/test/scala` directory.
It currently just tests the DC bin until unscrambling and futher testing capabilities are added.

## Configuring

In `src/main/scala` there is a `Config.scala` file.
In the `DspConfig` class are a bunch of parameters, like `NumTaps` and `FractionalBits`.
Set these to your desired values, then rebuild and retest the design.

TODO: changing between FixedPoint and DspReal


# Specifications

## Interfaces

The PFB uses the DSP streaming interface (a subset of AXI4-Stream) on both the data input and data output.
When using a RAM to set the window coefficients, this RAM exists in the SCR file and is accessible through the AXI4 control interface.
If the coefficients are hard-coded, no SCR file exists.

## Implementation

