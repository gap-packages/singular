DeclareAttribute( "TermOrdering", IsPolynomialRing, "mutable" );
DeclareAttribute( "IndeterminateNumbers", IsPolynomialRing );

#
# The following attribute record whether an object has been sent to
# Singular, and if so, by what identifier it is known to Singular.
#
DeclareAttribute( "SingularIdentifier", IsObject, "mutable" );


if not CompareVersionNumbers( VERSION, "4.4" ) and 
    # something else may have already defined GroebnerBasis ...
    not IsBound( GroebnerBasis )  then
    DeclareAttribute( "GroebnerBasis", IsMagma );
fi;

DeclareInfoClass( "InfoSingular" );
# InfoLevel( InfoSingular );
SetInfoLevel( InfoSingular, 1);

