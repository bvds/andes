// list of constants, mathematical and physical
// First term is string as appears in canoneqf 
//
// BvdS:  Such quantitites should not be defined in the solver
//        since they still must be defined in the physics rules
//        anyway and mapped over to these constants.
// BvdS:  The mapping is performed in Help/State.cl
//
//      name             value       m  kg   s    C   K 
//
physc( "$P",    3.14159265358979323, 0,  0,  0,   0,  0 ), //  $\Pi$ math
physc( "Pi",    3.14159265358979323, 0,  0,  0,   0,  0 ), //  $\Pi$ math
physc( "eps0",    8.854187817E-12,   -3, -1, 2,   2,  0 ), //  coulombs law
physc( "kCoulumb", 8.98755178799791E+9, 3, 1, -2 ,-2, 0 ), //  1/4*pi*eps0
physc( "mu0",  1.2566370614359173E-6, 1, 1,  0,  -2,  0 ), //  fr spc permtvty
