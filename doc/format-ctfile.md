---
title: Connection table format
---

## Specification

@Note [Reference](https://www.daylight.com/meetings/mug05/Kappler/ctfile.pdf)

The molfile is identified by the extension ``mol`` and the structure data format
is identified by ``sdf``.
Both V2000 and V3000 connection tables can be read.

## Example

Caffeine molecule in mol format:

```text

          11262021073D

 24  0  0     0  0            999 V2000
    1.0732    0.0488   -0.0757 C   0  0  0  0  0  0  0  0  0  0  0  0
    2.5137    0.0126   -0.0758 N   0  0  0  0  0  0  0  0  0  0  0  0
    3.3520    1.0959   -0.0753 C   0  0  0  0  0  0  0  0  0  0  0  0
    4.6190    0.7303   -0.0755 N   0  0  0  0  0  0  0  0  0  0  0  0
    4.5791   -0.6314   -0.0753 C   0  0  0  0  0  0  0  0  0  0  0  0
    3.3013   -1.1026   -0.0752 C   0  0  0  0  0  0  0  0  0  0  0  0
    2.9807   -2.4869   -0.0738 C   0  0  0  0  0  0  0  0  0  0  0  0
    1.8253   -2.9004   -0.0758 O   0  0  0  0  0  0  0  0  0  0  0  0
    4.1144   -3.3043   -0.0694 N   0  0  0  0  0  0  0  0  0  0  0  0
    5.4517   -2.8562   -0.0723 C   0  0  0  0  0  0  0  0  0  0  0  0
    6.3893   -3.6597   -0.0723 O   0  0  0  0  0  0  0  0  0  0  0  0
    5.6624   -1.4768   -0.0749 N   0  0  0  0  0  0  0  0  0  0  0  0
    7.0095   -0.9365   -0.0752 C   0  0  0  0  0  0  0  0  0  0  0  0
    3.9206   -4.7409   -0.0616 C   0  0  0  0  0  0  0  0  0  0  0  0
    0.7340    1.0879   -0.0750 H   0  0  0  0  0  0  0  0  0  0  0  0
    0.7124   -0.4570    0.8233 H   0  0  0  0  0  0  0  0  0  0  0  0
    0.7124   -0.4558   -0.9755 H   0  0  0  0  0  0  0  0  0  0  0  0
    2.9930    2.1176   -0.0748 H   0  0  0  0  0  0  0  0  0  0  0  0
    7.7653   -1.7263   -0.0759 H   0  0  0  0  0  0  0  0  0  0  0  0
    7.1486   -0.3218    0.8197 H   0  0  0  0  0  0  0  0  0  0  0  0
    7.1480   -0.3208   -0.9695 H   0  0  0  0  0  0  0  0  0  0  0  0
    2.8650   -5.0232   -0.0583 H   0  0  0  0  0  0  0  0  0  0  0  0
    4.4023   -5.1592    0.8284 H   0  0  0  0  0  0  0  0  0  0  0  0
    4.4002   -5.1693   -0.9478 H   0  0  0  0  0  0  0  0  0  0  0  0
M  END
```

## Extensions

No extension implemented to the original format.

## Missing Features

The following features are currently not supported:

- Not all modifiers are supported for the connection table
- SDF key-value pair annotations are dropped
- continuation lines in V3000 format are not supported

@Note Feel free to contribute support for missing features
      or bring missing features to our attention by opening an issue.
