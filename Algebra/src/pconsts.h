// list of constants, mathematical and physical
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
// Modifications by Brett van de Sande, 2005-2008
//
//  This file is part of the Andes Solver.
//
//  The Andes Solver is free software: you can redistribute it and/or modify
//  it under the terms of the GNU Lesser General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  The Andes Solver is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU Lesser General Public License for more details.
//
//  You should have received a copy of the GNU Lesser General Public License
//  along with the Andes Solver.  If not, see <http://www.gnu.org/licenses/>.


// list of constants, mathematical and physical
// First term is string as appears in canoneqf 
//
// See KB/constants.cl
//
//      name             value       m kg  s  C  K 
//
physc("$P",     3.14159265358979323, 0, 0, 0, 0, 0), // pi
physc("Pi",     3.14159265358979323, 0, 0, 0, 0, 0), // pi
physc("$e0",       8.854187817E-12,-3,-1, 2, 2, 0), // pemittivity of vacuum
physc("eps0",       8.854187817E-12,-3,-1, 2, 2, 0), // pemittivity of vacuum
physc("kelec",  8.98755178799791E+9, 3, 1,-2,-2, 0), // 1/4*pi*eps0
physc("$m0",  1.2566370614359173E-6, 1, 1, 0,-2, 0), // permeability of vacuum
physc("mu0",  1.2566370614359173E-6, 1, 1, 0,-2, 0), // permeability of vacuum
physc("kmag",                1.0E-7, 1, 1, 0,-2, 0), // mu0/(4*pi)
physc("c",                299792458, 1, 0,-1, 0, 0), // speed of light
physc("hbar",        1.05457168E-34, 2, 1,-1, 0, 0), // hbar
physc("G",               6.6742E-11, 3,-1,-2, 0, 0), // gravitational constant
physc("Iref",               1.0E-12, 0, 1,-3, 0, 0), // reference intensity dB
