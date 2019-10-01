#############################################################################
##
#W    init.g               Package singular            Willem de Graaf
#W                                                     Marco Costantini
##
#Y    Copyright (C) 2003 Willem de Graaf and Marco Costantini
#Y    Copyright (C) 2004, 2005, 2006 Marco Costantini
##

ReadPackage( "singular", "gap/singular.gd" );
ReadPackage( "singular", "gap/singular.g" );


# The following implication was added in GAP 4.11, and results in row modules
# over polynomial rings to be printed more nicely; in order to get tests that
# behave the same in GAP 4.11 as in older GAP versions, we replicate this
# implication here. This does nothing in GAP >= 4.11.
InstallTrueMethod( IsFreeLeftModule, IsRowModule );
