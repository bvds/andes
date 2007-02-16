// list of units in terms of fundamental dimensions.
//First term is string representing best way to write this set of units.
//
//name             Value     m  kg   s    C   K  pfx?    wordy name
//
punit( "",           1.,     0,  0,  0,   0,  0,  0), // dimensionless
punit( "m",          1.,     1,  0,  0,   0,  0,  1), // meters
punit( "kg",         1.,     0,  1,  0,   0,  0,  0), // kilogram
punit( "s",          1.,     0,  0,  1,   0,  0,  1), // seconds, sec
punit( "C",          1.,     0,  0,  0,   1,  0,  1), // Coulombs
punit( "K",          1.,     0,  0,  0,   0,  1,  1), // degrees Kelvin
punit( "g",        0.001,    0,  1,  0,   0,  0,  1), // grams
punit( "N",          1.,     1,  1,  -2,  0,  0,  1), // Newtons
punit( "J",          1.,     2,  1,  -2,  0,  0,  1), // Joules
punit( "V",          1.,     2,  1,  -2,  -1, 0,  1), // Volt
punit( "A",          1.,     0,  0,  -1,  1,  0,  1), // amps, Amperes
punit( "T",          1.,     0,  1,  -1,  -1, 0,  1), // tesla
punit( "Wb",         1.,     2,  1,  -1,  -1, 0,  1), // webers
punit( "$W",         1.,     2,  1,  -1,  -2, 0,  1), // ohm (as capital omega)
punit( "ohm",        1.,     2,  1,  -1,  -2, 0,  1), // ohm
punit( "Hz",         1.,     0,  0,  -1,  0,  0,  1), // Hertz
punit( "Pa",         1.,     -1, 1,  -2,  0,  0,  1), // Pascal
punit( "F",          1.,     -2, -1,  2,  2,  0,  1), // Farad
punit( "H",          1.,     2,  1,  0,   -2, 0,  1), // Henry
punit( "W",          1.,     2,  1,  -3,  0,  0,  1), // Watt
punit( "m/s",        1.,     1,  0,  -1,  0,  0,  0), // velocity
punit( "m/s^2",      1.,     1,  0,  -2,  0,  0,  0), // acceleration
punit( "N.m",        1.,     2,  1,  -2,  0,  0,  0), // torque (but = J)
punit( "J.s",        1.,     2,  1,  -1,  0,  0,  0), // angular momentum
punit( "kg.m^2",     1.,     2,  1,  0,   0,  0,  0), // moment of inertia
punit( "N/m",        1.,     0,  1,  -2,  0,  0,  0), // spring constant
punit( "N.s/m^2",    1.,     -1, 1,  -1,  0,  0,  0), // viscosity
punit( "N/m^2",      1.,     -1, 1,  -2,  0,  0,  0), // Young/shear/bulk mod
punit( "deg", .0174532925199433, 0, 0, 0, 0,  0,  1), // degree
punit( "rad",        1.,     0,  0,  0,   0,  0,  1), // radians
punit( "rev", 6.2831853071796, 0, 0, 0,   0,  0,  0), // revolutions
punit( "rad/s^2",    1.,     0,  0, -2,   0,  0,  1), // angular acceleration
punit( "lb",         4.448,  1,  1,  -2,  0,  0,  0), // pounds
punit( "day",      86400.,   0,  0,  1,   0,  0,  0), // day
punit( "hr",         3600.,  0,  0,  1,   0,  0,  0), // hour
punit( "h",          3600.,  0,  0,  1,   0,  0,  0), // hour
punit( "min",        60.,    0,  0,  1,   0,  0,  0), // minute
punit( "yr",     31556952.,   0,  0,  1,   0,  0,  0), // year
punit( "liter",    0.001,    3,  0,  0,   0,  0,  1), // liter
punit( "ft",       0.3048,   1,  0,  0,   0,  0,  0), // foot
punit( "in",       0.0254,   1,  0,  0,   0,  0,  0), // inch (SI is in.)
punit( "mi",       1609.,    1,  0,  0,   0,  0,  0), // mile
punit( "slug",     14.59,    0,  1,  0,   0,  0,  0), // slug, 1 lb.s^2/ft
punit( "gal",      0.003786, 3,  0,  0,   0,  0,  0), // gallon
punit( "u",   1.6605402E-27, 0,  1,  0,   0,  0,  0), // atomic mass unit
punit( "eV", 1.60217733E-19, 2,  1,  -2,  0,  0,  1), // electron volt
punit( "dyne",   1.0E-5,     1,  1,  -2,  0,  0,  1), // dyne
punit( "erg",    1.0E-7,     2,  1,  -2,  0,  0,  1), // erg
punit( "cal",     4.186,     2,  1,  -2,  0,  0,  1), // calorie
punit( "lbs",        4.448,  1,  1,  -2,  0,  0,  0), // pounds
punit( "ozW",        0.278,  1,  1,  -2,  0,  0,  0), // ounce of weight
punit( "ozVUS",  2.9578E-5,  3,  0,  0,   0,  0,  0), // US ounce of volume
punit( "knot",  0.5144444,   1,  0,  -1,  0,  0,  0), // knot (not Admiralty) 
punit( "psi", 6894.4137888276, -1, 1, -2, 0,  0,  0), // pounds/sq in 
punit( "rpm", 0.0166666666666666666, 0, 0, -1, 0,  0,  0), // revs/min
punit( "blocks", 80.45,      1,  0,  0,   0,  0,  0), // NYC block
punit( "block",  80.45,      1,  0,  0,   0,  0,  0), // NYC block
punit( "dB",        1.0,     0,  0,  0,   0,  0,  0),  // decibels
punit( "NIL",        1.,     0,  0,  0,   0,  0,  0) // LISP KLUDGE
