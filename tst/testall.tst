# Start
gap> START_TEST( "Test of the singular package" );
gap> SetInfoLevel( InfoSingular, 1 );
gap> StartSingular();

#
# Examples from README file
#
gap> R:=PolynomialRing( Rationals, ["x", "y", "z"] : new );
Rationals[x,y,z]
gap> gen:=GeneratorsOfLeftOperatorRingWithOne(R);
[ x, y, z ]
gap> x:=gen[1];
x
gap> y:=gen[2];
y
gap> z:=gen[3];
z
gap> pol1:=-3*x*z^3+x^3+x*y*z;
-3*x*z^3+x^3+x*y*z
gap> pol2:=-3*x^2*z^3+x^4+x^2*y*z-3*x*z^3+x^3+x*y*z;
-3*x^2*z^3+x^4+x^2*y*z-3*x*z^3+x^3+x*y*z
gap> pol3:=x*y+x*z+x+y+z;
x*y+x*z+x+y+z
gap> I:=Ideal( R, [ pol1, pol2, pol3] );
<two-sided ideal in Rationals[x,y,z], (3 generators)>
gap> SingularSetBaseRing( R );
gap> J:=SingularInterface( "jacob", [ pol1 ], "ideal" );
<two-sided ideal in Rationals[x,y,z], (3 generators)>
gap> GeneratorsOfTwoSidedIdeal( J );
[ -3*z^3+3*x^2+y*z, x*z, -9*x*z^2+x*y ]
gap> SingularInterface( "dim", [ I ], "int" );
2
gap> std:=SingularInterface( "std", [ I ], "ideal" );
<two-sided ideal in Rationals[x,y,z], (3 generators)>
gap> GeneratorsOfTwoSidedIdeal( std );
[ x*y+x*z+x+y+z, 3*y*z^3+3*z^4-y^2*z-y*z^2+x^2-x-y-z, 
  3*x*z^3-x^3+x*z^2+x*z+y*z+z^2 ]
gap> GroebnerBasis( I );
[ x*y+x*z+x+y+z, 3*y*z^3+3*z^4-y^2*z-y*z^2+x^2-x-y-z, 
  3*x*z^3-x^3+x*z^2+x*z+y*z+z^2 ]
gap> HasTrivialGroebnerBasis( I );
false
gap> SingularLibrary( "general.lib");
gap> GcdUsingSingular( pol1, pol2, pol3 );
1
gap> FactorsUsingSingularNC( pol1 );
[ -1, x, 3*z^3-x^2-y*z ]
gap> FactorsUsingSingular( pol2 );
[ -1, x, x+1, 3*z^3-x^2-y*z ]

#
# Examples from manual
#
gap> SetInfoLevel( InfoSingular, 2 );
gap> G:= SymmetricGroup( 3 );
Sym( [ 1 .. 3 ] )
gap> R:= PolynomialRing( GF(2), 3 );
GF(2)[x_1,x_2,x_3]
gap> gens:=GeneratorsOfInvariantRing( R, G );
#I  running SingularInterface( "invariant_ring", [ "matrix", "matrix" ], "list" )...
#I  done SingularInterface.
[ x_1+x_2+x_3, x_1*x_2+x_1*x_3+x_2*x_3, x_1*x_2*x_3 ]
gap> I:= Ideal( R, gens );
<two-sided ideal in GF(2)[x_1,x_2,x_3], (3 generators)>
gap> GB:=GroebnerBasis( I );
#I  running GroebnerBasis...
#I  done GroebnerBasis.
[ x_1+x_2+x_3, x_2^2+x_2*x_3+x_3^2, x_3^3 ]
gap> R:= PolynomialRing( Rationals, ["x","y","z"] : old );
Rationals[x,y,z]
gap> i:= IndeterminatesOfPolynomialRing(R);
[ x, y, z ]
gap> x:= i[1];
x
gap> y:= i[2];
y
gap> z:= i[3];
z
gap> r:= [ x*y*z -x^2*z, x^2*y*z-x*y^2*z-x*y*z^2, x*y-x*z-y*z];
[ -x^2*z+x*y*z, x^2*y*z-x*y^2*z-x*y*z^2, x*y-x*z-y*z ]
gap> I:= Ideal( R, r );
<two-sided ideal in Rationals[x,y,z], (3 generators)>
gap> GroebnerBasis( I );
#I  running GroebnerBasis...
#I  done GroebnerBasis.
[ x*y-x*z-y*z, x^2*z-x*z^2-y*z^2, x*z^3+y*z^3, -x*z^3+y^2*z^2-y*z^3 ]
gap> R:= PolynomialRing( Rationals, 3 );
Rationals[x,y,z]
gap> i:= IndeterminatesOfPolynomialRing( R );
[ x, y, z ]
gap> pols:= [i[1]+i[2]+i[3], i[1]*i[2]+i[1]*i[3]+i[2]*i[3], i[1]*i[2]*i[3]];
[ x+y+z, x*y+x*z+y*z, x*y*z ]
gap> o:= MonomialLexOrdering();
MonomialLexOrdering()
gap> GBASIS:= GAPGBASIS;
rec( GroebnerBasis := function( elms, order ) ... end, 
  name := "naive GAP version of Buchberger's algorithm" )
gap> gg:=GroebnerBasis( pols, o );   # This is the internal GAP method.
[ x+y+z, x*y+x*z+y*z, x*y*z, -y^2-y*z-z^2, z^3 ]
gap> GBASIS:= SINGULARGBASIS;
rec( GroebnerBasis := function( pols, O ) ... end, 
  name := "singular interface for GroebnerBasis" )
gap> gs:=GroebnerBasis( pols, o );
#I  running GroebnerBasis...
#I  done GroebnerBasis.
[ z^3, y^2+y*z+z^2, x+y+z ]
gap> (gg[1]=gs[1] and -gg[4]=gs[2] and gg[5]=gs[3]) or (gg[5]=gs[1] and -gg[4]=gs[2] and gg[1]=gs[3]);
true
gap> I:=Ideal(R, gs);
<two-sided ideal in Rationals[x,y,z], (3 generators)>
gap> SingularInterface("reduce", [ gg[2], I ], "poly" );
#I  running SingularInterface( "reduce", [ "poly", "ideal" ], "poly" )...
#I  done SingularInterface.
0
gap> SingularInterface("reduce", [ gg[3], I ], "poly" );
#I  running SingularInterface( "reduce", [ "poly", "ideal" ], "poly" )...
#I  done SingularInterface.
0
gap> gs[2] in I;
#I  running GroebnerBasis...
#I  done GroebnerBasis.
true
gap> R:= PolynomialRing( GaussianRationals, ["x","y","z"] : old );
GaussianRationals[x,y,z]
gap> i:= IndeterminatesOfPolynomialRing(R);
[ x, y, z ]
gap> x:= i[1];
x
gap> y:= i[2];
y
gap> z:= i[3];
z
gap> f:= (x*y-z)*(x*y*z+y^2*z+x^2*z);
x^3*y*z+x^2*y^2*z+x*y^3*z-x^2*z^2-x*y*z^2-y^2*z^2
gap> g:= (x*y-z)*(x*y*z^2+x*y^2*z+x^2*y*z);
x^3*y^2*z+x^2*y^3*z+x^2*y^2*z^2-x^2*y*z^2-x*y^2*z^2-x*y*z^3
gap> I:= Ideal( R, [f,g] );
<two-sided ideal in GaussianRationals[x,y,z], (2 generators)>
gap> HasTrivialGroebnerBasis( I );
#I  running HasTrivialGroebnerBasis...
#I  done HasTrivialGroebnerBasis.
false
gap> R:= PolynomialRing( Rationals, ["x","y","z"] : old );
Rationals[x,y,z]
gap> i:= IndeterminatesOfPolynomialRing(R);
[ x, y, z ]
gap> x:= i[1];
x
gap> y:= i[2];
y
gap> z:= i[3];
z
gap> f:= (x*y-z)*(x*y*z+y^2*z+x^2*z);
x^3*y*z+x^2*y^2*z+x*y^3*z-x^2*z^2-x*y*z^2-y^2*z^2
gap> g:= (x*y-z)*(x*y*z^2+x*y^2*z+x^2*y*z);
x^3*y^2*z+x^2*y^3*z+x^2*y^2*z^2-x^2*y*z^2-x*y^2*z^2-x*y*z^3
gap> SingularSetBaseRing( R );
gap> GcdUsingSingular( f, g );
-x*y*z+z^2
gap> f:= (x*y-z)*(x*y*z+y^2*z+x^2*z);
x^3*y*z+x^2*y^2*z+x*y^3*z-x^2*z^2-x*y*z^2-y^2*z^2
gap> SingularSetBaseRing( R );
gap> FactorsUsingSingularNC( f );
#I  running SingularInterface( "factorize", [ "poly" ], "list" )...
#I  done SingularInterface.
[ -1, x^2+x*y+y^2, -x*y+z, z ]
gap> m:=[[1,1,1],[0,1,1],[0,0,1]]*One(GF(3));
[ [ Z(3)^0, Z(3)^0, Z(3)^0 ], [ 0*Z(3), Z(3)^0, Z(3)^0 ], 
  [ 0*Z(3), 0*Z(3), Z(3)^0 ] ]
gap> G:= Group( [m] );
Group(
[ 
  [ [ Z(3)^0, Z(3)^0, Z(3)^0 ], [ 0*Z(3), Z(3)^0, Z(3)^0 ], 
      [ 0*Z(3), 0*Z(3), Z(3)^0 ] ] ])
gap> R:= PolynomialRing( GF(3), 3 );
GF(3)[x_1,x_2,x_3]
gap> GeneratorsOfInvariantRing( R, G );
#I  running SingularInterface( "invariant_ring", [ "matrix" ], "list" )...
#I  done SingularInterface.
[ x_3, x_1*x_3+x_2^2+x_2*x_3, x_1^3+x_1^2*x_3-x_1*x_2^2-x_1*x_2*x_3 ]
gap> R:= PolynomialRing( Rationals, ["x","y","z"] : old );
Rationals[x,y,z]
gap> i:= IndeterminatesOfPolynomialRing(R);
[ x, y, z ]
gap> x:= i[1];
x
gap> y:= i[2];
y
gap> z:= i[3];
z
gap> f:= (x*y-z)*(x*y*z+y^2*z+x^2*z);
x^3*y*z+x^2*y^2*z+x*y^3*z-x^2*z^2-x*y*z^2-y^2*z^2
gap> g:= (x*y-z)*(x*y*z^2+x*y^2*z+x^2*y*z);
x^3*y^2*z+x^2*y^3*z+x^2*y^2*z^2-x^2*y*z^2-x*y^2*z^2-x*y*z^3
gap> I:= Ideal( R, [f,g] );
<two-sided ideal in Rationals[x,y,z], (2 generators)>
gap> SingularLibrary("primdec.lib");
gap> pd:=SingularInterface("primdecGTZ", [ I ], "def" );;
#I  running SingularInterface( "primdecGTZ", [ "ideal" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "list"
gap> Length(pd);
4
gap> List(pd,x->List(x,GeneratorsOfTwoSidedIdeal));
[ [ [ x*y-z ], [ x*y-z ] ], [ [ z ], [ z ] ], 
  [ [ y^2+y*z+z^2, x+y+z ], [ y^2+y*z+z^2, x+y+z ] ], 
  [ [ y^3, x*y, x^2+y^2 ], [ y, x ] ] ]

#
# ParseSingProcToGapFunction
#
gap> SingCommandInStreamOutStream( "", "proc a1 () { return ( 35 ) };" );
""
gap> SingCommandInStreamOutStream( "", "proc a2 (e2) { return ( a1 ) };" );
""
gap> SingCommandInStreamOutStream( "", "proc a3 (e3) { return ( a2 ) };" );
""
gap> f := SingularInterface( "a3", [ 1 ], "proc" );
#I  running SingularInterface( "a3", [ "int" ], "proc" )...
#I  done SingularInterface.
function( e2 ) ... end
gap> g := f( 10 );
#I  running SingularInterface( "GAP_proc", [ "int" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "proc"
function(  ) ... end
gap> g(  );
#I  running SingularInterface( "GAP_proc", "...", "def" )...
#I  done SingularInterface.
#I  Singular output of type "int"
35
gap> SingCommandInStreamOutStream( "",
>  "proc asd ( t ) { return (t+1 ) ;return();};" );
""
gap> SingCommandInStreamOutStream( "",
>  "proc asd2 (){return (asd ) ;return(); };" );
""
gap> f := SingularInterface( "asd2", [  ], "proc" );
#I  running SingularInterface( "asd2", "...", "proc" )...
#I  done SingularInterface.
function( t ) ... end
gap> f( 19 );
#I  running SingularInterface( "GAP_proc", [ "int" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "int"
20

#
# Check the conversion of numbers and polynomials for the various
# CoefficientsRing
#
gap> SingularCommand("proc id_func (a){return (a);}", ""); # id_func = identity
""
gap> R:=PolynomialRing( GF(32003), ["x"] : old );;
gap> SingularSetBaseRing( R );
gap> SingularInterface("id_func", [Z(32003)], "def");
#I  running SingularInterface( "id_func", [ "number" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "number"
Z(32003)
gap> One(SingularBaseRing);
Z(32003)^0
gap> SingularInterface("id_func", [last], "def");
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
Z(32003)^0

#
gap> x:=Indeterminate(Rationals);;
gap> F1:=AlgebraicExtension(Rationals, x^5+4*x+1);
<algebraic extension over the Rationals of degree 5>
gap> e1:=RootOfDefiningPolynomial(F1);
a
gap> x:=Indeterminate(GF(5));;
gap> F2:=AlgebraicExtension(GF(5), x^5+4*x+1);;
gap> e2:=RootOfDefiningPolynomial(F2);
a

#
gap> field_element := [ 
> [ Rationals, 23 ],
> [ GF(2), Z(2) ],
> [ GF(7), Z(7)^2 ],
> [ GF(32), Z(32)^3 ],
> [ GF(25), Z(25)^4 ],
> [ CF(3), -E(3) ],
> [ CF(25), Random(CF(25)) ],
> [ CF(25), E(5) ],
> [ F1, e1^2 + e1 ],
> [ F2, e2^3+One(F2) ]
> ];;

#
gap> for i in field_element do
> 
>     R := PolynomialRing( i[1], 2 );
>     SingularSetBaseRing( R );
> 
>     # test of numbers
>     u1:=i[2];
>     u2:=SingularInterface( "id_func", [u1], "def" );
>     if u1<>u2 then Error( "wrong conversion of number!\n"); fi;
>     u1:=Zero(i[1]);
>     u2:=SingularInterface( "id_func", [u1], "def" );
>     if u1<>u2 then Error( "wrong conversion of number!\n"); fi;
>     u1:=One(i[1]);
>     u2:=SingularInterface( "id_func", [u1], "def" );
>     if u1<>u2 then Error( "wrong conversion of number!\n"); fi;
> 
>     # test of polynomials
>     gen := GeneratorsOfLeftOperatorRingWithOne(R);
>     p1 := (i[2]*gen[1]+i[2]*gen[2])^4;
>     p2 := SingularInterface( "id_func", [p1], "def" );
>     if p1<>p2 then Error( "wrong conversion of polynomial!\n"); fi;
>     p1:=Zero(R);
>     p2:=SingularInterface( "id_func", [p1], "def" );
>     if p1<>p2 then Error( "wrong conversion of zero polynomial!\n"); fi;
>     p1:=One(R);
>     p2:=SingularInterface( "id_func", [p1], "def" );
>     if p1<>p2 then Error( "wrong conversion of zero polynomial!\n"); fi;
> 
> od;
#I  running SingularInterface( "id_func", [ "int" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "int"
#I  running SingularInterface( "id_func", [ "int" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "int"
#I  running SingularInterface( "id_func", [ "int" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "int"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "number" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "number"
#I  running SingularInterface( "id_func", [ "number" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "number"
#I  running SingularInterface( "id_func", [ "number" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "number"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "number" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "number"
#I  running SingularInterface( "id_func", [ "number" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "number"
#I  running SingularInterface( "id_func", [ "number" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "number"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "number" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "number"
#I  running SingularInterface( "id_func", [ "number" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "number"
#I  running SingularInterface( "id_func", [ "number" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "number"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "number" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "number"
#I  running SingularInterface( "id_func", [ "number" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "number"
#I  running SingularInterface( "id_func", [ "number" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "number"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "number" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "number"
#I  running SingularInterface( "id_func", [ "int" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "int"
#I  running SingularInterface( "id_func", [ "int" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "int"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "number" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "number"
#I  running SingularInterface( "id_func", [ "int" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "int"
#I  running SingularInterface( "id_func", [ "int" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "int"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "number" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "number"
#I  running SingularInterface( "id_func", [ "int" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "int"
#I  running SingularInterface( "id_func", [ "int" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "int"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "number" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "number"
#I  running SingularInterface( "id_func", [ "number" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "number"
#I  running SingularInterface( "id_func", [ "number" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "number"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "number" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "number"
#I  running SingularInterface( "id_func", [ "number" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "number"
#I  running SingularInterface( "id_func", [ "number" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "number"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"
#I  running SingularInterface( "id_func", [ "poly" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "poly"

#
# test the string conversion
gap> s1:=[32..126];;
gap> s2:=List("$'@", INT_CHAR);;
gap> s1:=Difference(s1,s2);;
gap> s3:=List(s1,CHAR_INT);;
gap> ConvertToStringRep(s3);
gap> s4:=SingularInterface("id_func", [s3], "def");
#I  running SingularInterface( "id_func", [ "string" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "string"
" !\"#%&()*+,-./0123456789:;<=>?ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklm\
nopqrstuvwxyz{|}~"
gap> s3=s4;
true

#
# Check the conversion of various types of object
#
gap> x:=X(GF(5),"x" : new );;
gap> y:=X(GF(5),"y",[x] : new );;
gap> ring:=PolynomialRing(GF(5),[x,y]);;
gap> SingularSetBaseRing(ring);

#
gap> ideal:=Ideal(ring,[x*y, x+y]);
<two-sided ideal in GF(5)[x,y], (2 generators)>
gap> int:=123456;
123456
gap> intmat:=[[12,34],[56,78]];
[ [ 12, 34 ], [ 56, 78 ] ]
gap> intvec:=[98765,4321];
[ 98765, 4321 ]
gap> map:=AlgebraGeneralMappingByImages(ring,ring,[x,y],[x+y,x-y]);
[ x, y ] -> [ x+y, x-y ]
gap> matrix:=[[x+y, x*y],[x+y+x*y,x^2+y^2]];
[ [ x+y, x*y ], [ x*y+x+y, x^2+y^2 ] ]
gap> module:=LeftModuleByGenerators( ring, [[x+y+x*y,x-y], [x+x^2+x^3,y+x^2+y^3]]);
<free left module over PolynomialRing( GF(5), ["x", "y"] ), with 2 generators>
gap> number:=Z(5)^3;
Z(5)^3
gap> poly:=x^3+y^3+Z(5)^3*x^2+Z(5)^3*x*y+y^2+Z(5)^3*y;
x^3+y^3+Z(5)^3*x^2+Z(5)^3*x*y+y^2+Z(5)^3*y
gap> proc:=function() end;
function(  ) ... end
gap> string:="ciao";
"ciao"
gap> vector:=[x^3+y^3,+(5)^3*x^2+Z(5)^3,x*y+y^2+Z(5)^3*y];
[ x^3+y^3, Z(5)^3, x*y+y^2+Z(5)^3*y ]

#
gap> list1 := [ ideal, int, intmat, intvec, map, matrix, module, number,
> poly, proc, ring, string, vector];
[ <two-sided ideal in GF(5)[x,y], (2 generators)>, 123456, 
  [ [ 12, 34 ], [ 56, 78 ] ], [ 98765, 4321 ], [ x, y ] -> [ x+y, x-y ], 
  [ [ x+y, x*y ], [ x*y+x+y, x^2+y^2 ] ], 
  <free left module over PolynomialRing( GF(5), ["x", "y"] ), with 
    2 generators>, Z(5)^3, x^3+y^3+Z(5)^3*x^2+Z(5)^3*x*y+y^2+Z(5)^3*y, 
  function(  ) ... end, GF(5)[x,y], "ciao", 
  [ x^3+y^3, Z(5)^3, x*y+y^2+Z(5)^3*y ] ]
gap> List( list1, SingularType);
[ "ideal", "int", "intmat", "intvec", "map", "matrix", "module", "number", 
  "poly", "proc", "ring", "string", "vector" ]

#
gap> list2 := [ ideal, int, intmat, intvec, matrix, module, number,
> poly, string, vector];
[ <two-sided ideal in GF(5)[x,y], (2 generators)>, 123456, 
  [ [ 12, 34 ], [ 56, 78 ] ], [ 98765, 4321 ], 
  [ [ x+y, x*y ], [ x*y+x+y, x^2+y^2 ] ], 
  <free left module over PolynomialRing( GF(5), ["x", "y"] ), with 
    2 generators>, Z(5)^3, x^3+y^3+Z(5)^3*x^2+Z(5)^3*x*y+y^2+Z(5)^3*y, 
  "ciao", [ x^3+y^3, Z(5)^3, x*y+y^2+Z(5)^3*y ] ]
gap> list3 := SingularInterface( "id_func", [list2], "def" );
#I  running SingularInterface( "id_func", [ "list" ], "def" )...
#I  done SingularInterface.
#I  Singular output of type "list"
#I  running SingularInterface( "matrix", "...", "matrix" )...
#I  done SingularInterface.
#I  running SingularInterface( "matrix", "...", "matrix" )...
#I  done SingularInterface.
[ <two-sided ideal in GF(5)[x,y], (2 generators)>, 123456, 
  [ [ 12, 34 ], [ 56, 78 ] ], [ 98765, 4321 ], 
  [ [ x+y, x*y ], [ x*y+x+y, x^2+y^2 ] ], 
  <free left module over PolynomialRing( GF(5), ["x", "y"] ), with 
    2 generators>, Z(5)^3, x^3+y^3+Z(5)^3*x^2+Z(5)^3*x*y+y^2+Z(5)^3*y, 
  "ciao", [ x^3+y^3, Z(5)^3, x*y+y^2+Z(5)^3*y ] ]

#
gap> for i in [2,3,4,5,7,8,9,10] do
>     if list2[i] <> list3[i] then
>        Error( "wrong conversion!\n");
>     fi;
> od;

#
# test of GapInterface
#
gap> cc:=ConvertGapObjToSingObj(matrix);;
gap> SingularCommand(Concatenation( "matrix mm = ", cc),"print(mm)");
"x_1+x_2,        x_1*x_2,   \nx_1*x_2+x_1+x_2,x_1^2+x_2^2"
gap> SingularCommand(Concatenation( "matrix mm = ", cc), "print(mm, \"%l\")");
"matrix(ideal(x_1+x_2,x_1*x_2,x_1*x_2+x_1+x_2,x_1^2+x_2^2),2,2)"

#
gap> GapInterface(TransposedMat, ["mm"], "mm2");
#I  Singular output of type "matrix"
gap> SingularCommand("","transpose(mm2)==mm");
"1"

#
# this should be at the end of the test file:
#
gap> [ SingularNr.Process, SingularNr.Input, SingularNr.Output ];
[ 0, 1136, 1136 ]
gap> SingularBaseRing;
GF(5)[x,y]
gap> CoefficientsRing( SingularBaseRing );
GF(5)
gap> SingWriteAndReadUntilDone("1;GAP_Apostrophe();2;GAP_Apostrophe();");
"1\n'\n2\n'\n> @\n> "
gap> SingularInterface( "string", "GAP_Done", "string" );
#I  running SingularInterface( "string", "...", "string" )...
#I  done SingularInterface.
"\n return ( \"@\" ) \n\n;return();\n\n"
gap> CloseSingular();
gap> SetInfoLevel( InfoSingular, 1 );

#
gap> STOP_TEST( "test", 10000 );
