Polyphase Filter Bank [![Build Status](https://travis-ci.org/ucb-art/pfb.svg?branch=master)](https://travis-ci.org/ucb-art/pfb)
=======================


# Overview

This project contains an implementation of a [Casper](https://casper.berkeley.edu/wiki/The_Polyphase_Filter_Bank_Technique)-style PFB.
It consists of a bank of Finite Impulse Response (FIR) filters to shape the input time-series data in such a way that the spectral channels are properly windowed, reducing spectral leakage between frequency bins. 
The weights for these filters are programmable at either compile time (maps to a ROM) or run time (maps to a RAM).

# Usage

## GitHub Pages

See [here](https://ucb-art.github.io/pfb/latest/api/) for the GitHub pages scaladoc.

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
You can choose the windowing function by setting it when creating the PFBConfig in the PFBKey parameter.
Current choices are a Sinc + Hamming window or a Blackman Harris window.
Set these parameters to your desired values, then rebuild and retest the design.

TODO: changing between FixedPoint and DspReal


# Specifications

## Interfaces

The PFB uses the DSP streaming interface (a subset of AXI4-Stream) on both the data input and data output.
When using a RAM to set the window coefficients, this RAM exists in the SCR file and is accessible through the AXI4 control interface.
If the coefficients are hard-coded, no SCR file exists.

## Signaling

### Bits

It is expected that the bits inputs contain time-series data time-multiplexed on the inputs, such that on the first cycle are values x[0], x[1], …, x[p-1], then the next cycle contains x[p], x[p+1], … and this continues until the input is x[n-p], x[n-p+1], …, x[n-1].

### Valid

The PFB delays the input valid by a value equal to the cycle time (output window size divided by the number of input lanes) times the number of taps.
When valid is low, the PFB creates zeros at its input.
Internal counters continue to count, flushing out extant data.
The shift register delaying the valid signal is set to all 0s during reset.

### Sync

The PFB effectively defines when a data set begins since it bucketizes streaming data into windows.
An internal counter constantly keeps track of the location within a data set, and it determines the output sync signal.
A high input sync signal will reset this counter.
The shift register delaying the sync signal is set to all 0s during reset.

## Implementation

The PFB is implemented as described in the [Casper](https://casper.berkeley.edu/wiki/The_Polyphase_Filter_Bank_Technique) website.

In Chisel, the design is split into lanes. Each lane implements a FIR filter in transposed form, using Chisel `Mems` for delays, which may be mapped to SRAMs.
Currently, only constant coefficients are allowed.
