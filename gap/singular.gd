#############################################################################
##
#W    singular.gd          Package singular            Willem de Graaf
#W                                                     Marco Costantini
##
#Y    Copyright (C) 2003 Willem de Graaf and Marco Costantini
#Y    Copyright (C) 2004, 2005, 2006 Marco Costantini
##


############################################################################# 
##
#A  TermOrdering( <PolynomialRing> ) 
##
##  The term or monomial ordering of the polynomial ring
##

DeclareAttribute( "TermOrdering", IsPolynomialRing, "mutable" );


#############################################################################
##
#A  IndeterminateNumbers( <PolynomialRing> )
##
##  This gives the mapping between the indeterminates in Gap and in Singular
##

DeclareAttribute( "IndeterminateNumbers", IsPolynomialRing );


#############################################################################
##
#A  SingularIdentifier( <Object> )
##
##  The following attribute record whether an object has been sent to
##  Singular, and if so, by what identifier it is known to Singular.
##

DeclareAttribute( "SingularIdentifier", IsObject, "mutable" );


#############################################################################
##
#I InfoSingular
##
## The InfoClass for package singular
##

DeclareInfoClass( "InfoSingular" );

# InfoLevel( InfoSingular );

# set the default level to 1
SetInfoLevel( InfoSingular, 1);


#############################################################################
#E

