Polyphase Filter Bank [![Build Status](https://travis-ci.org/ucb-art/pfb.svg?branch=master)](https://travis-ci.org/ucb-art/pfb)
=======================


# Overview

This project contains an implementation of a [Casper](https://casper.berkeley.edu/wiki/The_Polyphase_Filter_Bank_Technique)-style PFB.
It consists of a bank of Finite Impulse Response (FIR) filters to shape the input time-series data in such a way that the spectral channels are properly windowed, reducing spectral leakage between frequency bins. 
The weights for these filters are programmable at compile time (maps to a ROM).

# Usage

## GitHub Pages

See [here](https://ucb-art.github.io/pfb/latest/api/) for the GitHub pages scaladoc.

## Setup

Clone the repository and update the depenedencies:

```
git clone git@github.com:ucb-art/pfb.git
git submodule update --init
cd dsp-framework
./update.bash no_hwacha
cd ..
```

See the [dsp-framework README](https://github.com/ucb-art/dsp-framework/blob/master/README.md) for more details on this infrastructure.
Build the dependencies by typing `make libs`.

## Building

The build flow generates FIRRTL, then generates Verilog, then runs the TSMC memory compiler to generate memories.
Memories are black boxes in the Verilog by default.
IP-Xact is created with the FIRRTL.
The build targets for each of these are firrtl, verilog, and mems, respectively.
Depedencies are handled automatically, so to build the Verilog and memories, just type `make mems`.
Results are placed in a `generated-src` directory.

## Testing

To test the block, type `make test`.
This runs the block tester in the `src/test/scala` directory.

## Configuring

In `src/main/scala` there is a `Config.scala` file.
A few default configurations are defined for you, called DefaultStandaloneXPFBConfig, where X is either Real or FixedPoint.
These generate a small PFB with default parameters.
To run them, type `make verilog CONFIG=DefaultStandaloneXPFBConfig`, replacing X with Real or FixedPoint.
The default make target is the default FixedPoint configuration.

The suggested way to create a custom configuration is to modify CustomStandalonePFBConfig, which defines values for all possible parameters.
Then run `make verilog CONFIG=CustomStandalonePFBConfig` to generate the Verilog.
Define your own window here, or create the function in Windows.scala and refer to it here.
Current choices are a Sinc + Hamming window or a Blackman Harris window.

# Specifications

## Interfaces

The PFB uses the [DSP streaming interface](https://github.com/ucb-art/rocket-dsp-utils/blob/master/doc/stream.md) (a subset of AXI4-Stream) on both the data input and data output.
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

In Chisel, the design is split into lanes. 
Each lane implements a FIR filter in transposed form, using Chisel `Mems` for delays, which may be mapped to SRAMs.
Currently, only constant coefficients are allowed.
