#############################################################################
##
#W    init.g               Gap package `singular'             
##
##

# Announce the package version and try to find `Singular' on the system.
# If it is there, set up a process called `Sing_Proc'.

DeclarePackage( "singular", "4.04.15", true );
DeclarePackageDocumentation( "singular", "doc" );

# Read the files...

# Singular interface
ReadPkg( "singular", "gap/singular.gd" );
ReadPkg( "singular", "gap/singular.g" );



