- improve overflow?
- add more windows and compare them?
- handle ready signals on data interface?

- investigate memory configurations and sharing
- investigate different bitwidth configurations (what if output < input? scaling, truncation, rounding, etc)

Low priority:
- get DspComplex version working
- move convenient functions (input packing, output unpacking, convert) to central location
  - move CustomFunctions to rocket-dsp-utils
