---
title: Turbomole's coordinate data group
---

## Specification

@Note [Reference](https://www.turbomole.org/wp-content/uploads/2019/11/Turbomole_Manual_7-4-1.pdf)

The Turbomole format mainly builds around the ``control`` file.
The ``control`` file contains several data groups which are delimited by
their identifier, groups are either present in the ``control`` file or
references from the ``control`` file.
This format is defined by the geometry related information from the
``control`` file, mainly the:

- ``coord`` data group
- ``lattice`` data group
- ``cell`` data group
- ``periodic`` data group
- ``eht`` data group

For simplicity file references are not allowed and all data groups should be
in the same file. The data groups are not required to be in any particular order.

A group is started by a ``$`` symbol and accept modifiers. It is terminated by
another group or the ``end`` group which stops the scanning for further groups:

```text
$group1 [modifier]...
[entries]...
$group2 [modifier]...
[entries]...
$end
```

The ``coord`` data group contains the cartesian coordinates of all atoms and
their element symbols at the end of each line.
Atomic coordinates can either be specified in Bohr, by default or with the ``bohr``
modifier on the ``coord`` data group, in Ångström with the modifer ``angs`` or
as fractions of the lattice vectors with the modifier ``frac``.
Fractional coordinates can only be present for periodicities greater than zero.

The periodicity of the system is specified as modifier to the ``periodic`` data
group, the group itself is empty.

The lattice parameters can either be specified in the ``lattice`` or the ``cell``
data group, which requre different amounts of entries depending on the systems
periodicity. Both data groups are either given in atomic units (Bohr) or in
Ångström with the ``angs`` modifier.

For 3D periodic systems three lines with each three reals are required in the
``lattice`` data group. For a 2D periodic system two lines with each two reals
are required and the aperiodic direction is the z-axis.
Finally, for 1D periodic systems one real is required, giving the translation
vector in the x-direction.
The periodic directions are fixed in this format.

Similarly, the ``cell`` data groups allows for six, three, and one entries for
3D, 2D, and 1D periodic systems, respectively. The cell parameters are given
as the length of the lattice vectors and their angles, with the angles given
in degrees.

Charge and spin can be given in the ``eht`` data group with

```text
$eht charge=<int> unpaired=<int>
```

The format is identified by ``coord`` or ``tmol`` extension or by using ``coord``
as basename.

## Example

Caffeine molecule in Turbomole's coord format

```text
$coord
    2.02799694102955E+00    9.23131009712288E-02   -1.43108928076789E-01      C
    4.75010903288289E+00    2.37349549273006E-02   -1.43241208876543E-01      N
    6.33434168525178E+00    2.07098820094962E+00   -1.42353037792480E-01      C
    8.72860526354322E+00    1.38002889206282E+00   -1.42655393906204E-01      N
    8.65318631042630E+00   -1.19324840281009E+00   -1.42315243278265E-01      C
    6.23857038622984E+00   -2.08353597966915E+00   -1.42182962478511E-01      C
    5.63266763158490E+00   -4.69950217834841E+00   -1.39405065683676E-01      C
    3.44931633987275E+00   -5.48092265700988E+00   -1.43184517105220E-01      O
    7.77508746440172E+00   -6.24427735787637E+00   -1.31071375299170E-01      N
    1.03022932464460E+01   -5.39739678059374E+00   -1.36721655174379E-01      C
    1.20741000728661E+01   -6.91573469742800E+00   -1.36664963403056E-01      O
    1.07003828646773E+01   -2.79078472418281E+00   -1.41483763965525E-01      N
    1.32459756778874E+01   -1.76969033362408E+00   -1.42182962478511E-01      C
    7.40891531342536E+00   -8.95905731397191E+00   -1.16369309269361E-01      C
    1.38702087719268E+00    2.05575701172080E+00   -1.41786120079249E-01      H
    1.34622169909711E+00   -8.63566855308744E-01    1.55590566396441E+00      H
    1.34624059635422E+00   -8.61336978970033E-01   -1.84340853360131E+00      H
    5.65596794959872E+00    4.00172095964572E+00   -1.41313688651556E-01      H
    1.46743059591176E+01   -3.26230908353472E+00   -1.43449078704728E-01      H
    1.35089688050556E+01   -6.08151528240755E-01    1.54898926786298E+00      H
    1.35077971751149E+01   -6.06148418987336E-01   -1.83214576836511E+00      H
    5.41408305861986E+00   -9.49239460132319E+00   -1.10227700709351E-01      H
    8.31919618830440E+00   -9.74947288701666E+00    1.56539208703248E+00      H
    8.31511438076913E+00   -9.76854021943835E+00   -1.79108202867002E+00      H
$end
```

Ammonia molecular crystal:

```text
$coord
    4.15467326939489E+00    3.33328828180759E+00    1.66323354579962E+00      H
    3.33328828180759E+00    1.66323354579962E+00    4.15467326939489E+00      H
    1.66323354579962E+00    4.15467326939489E+00    3.33328828180759E+00      H
    9.14844767316819E+00    3.06025119596830E+00    9.33489353886275E+00      H
    8.23222919393441E+00    4.72395843553239E+00    6.86439107965696E+00      H
    6.65103940814062E+00    2.17993870408119E+00    7.71770302696940E+00      H
    7.71770302696940E+00    6.65103940814062E+00    2.17993870408119E+00      H
    9.33489353886275E+00    9.14844767316819E+00    3.06025119596830E+00      H
    6.86439107965696E+00    8.23222919393441E+00    4.72395843553239E+00      H
    4.72395843553239E+00    6.86439107965696E+00    8.23222919393441E+00      H
    2.17993870408119E+00    7.71770302696940E+00    6.65103940814062E+00      H
    3.06025119596830E+00    9.33489353886275E+00    9.14844767316819E+00      H
    2.59764186558897E+00    2.59764186558897E+00    2.59764186558897E+00      N
    7.55541554326270E+00    3.76254957116838E+00    8.43506486387956E+00      N
    8.43506486387956E+00    7.55541554326270E+00    3.76254957116838E+00      N
    3.76254957116838E+00    8.43506486387956E+00    7.55541554326270E+00      N
$periodic 3
$lattice
    9.47387528935762    0.00000000000000    0.00000000000000
    0.00000000000000    9.47387528935762    0.00000000000000
    0.00000000000000    0.00000000000000    9.47387528935762
$end
```

## Extensions

The original format does only allow for the ``periodic`` or ``eht`` group to
appear in the ``control`` file, to make the format self-contained, all groups
must appear in the same file.

The ``coord`` group only supports the ``frac`` modifier in Turbomole, but this
reader also allows ``angs`` and ``bohr``.

## Missing Features

The following features are currently not supported:

- Preserving information about frozen atoms from ``coord`` data group

@Note Feel free to contribute support for missing features
      or bring missing features to our attention by opening an issue.
