 
##############################################################################
##############################################################################

## PART 1. Global variables, some of them mirroring Singular globals ##


# <--- This line is for debug: this allow to do 
# ReadPkg("singular/gap/singular.g");
# as much as needed. Simply ignore it.
if not IsBound( Sing_Proc ) then  


# Here in this file must be added a line with the full path
# to the Singular executable file on your system (without the '#'),
# sing_exec := "";
# in place of the following example;

sing_exec := "/home/graaf/Singular/2-0-3/ix86-Linux/Singular";

# The directory separator is always '/' even under DOS/Windows or MacOS. 

# If the Singular executable is the the system $PATH, it should be non 
# necessary adding this line, because the interface should be able to
# find the executable.
# You can get this path, from within Singular, with the command 
# system("Singular" );


# sing_exec_options a list of command-line options (given as strings) that 
# will be passed to Singular at its startup. "-t" is necessary for proper 
# working, but others can be added. See the documentation of Singular, 
# paragraph "3.1.6 Command line option".

sing_exec_options := [ "-t" ];


# Temporary directory for i/o with Singular 
# You may set it like the following if you prefer to have the temporary 
# files in a specific directory. If you don't specify it, The interface 
# will set up one.
# SingularTempDirectory := Directory( "/tmp" );
SingularTempDirectory := "";



##############################################################################
# Global variables, some of them mirroring Singular globals


# The variables sing_exec and SingularTempDirectory need to be checked...
SingularExecutableAndTempDirAreOk := false;

# The limitations of Singular: see the documentation of Singular,
# paragraph "6.1 Limitations".

SingularLimitations := rec(
# the maximal characteristic of a prime field:
max_char_prime_field := 2147483629,
# the maximal size of a finite non prime field:
max_size_nonprime_field := 2^15,
# the maximal exponent of a ring variable:
max_exp_ring_var := 65536,
# the biggest integer (of type "int"):
max_int := 2147483647 );

## You can tell the interface which is the biggest number in Singular of 
## type "int" (it depends also on your hardware and on the version of 
## Singular). If you omit this, the interface will try to autodetermine.
## For safety, you can choose the smallest one.
#
#SingularLimitations.max_int := 2147483647; # on a 32-bit machine
## SingularLimitations.max_int := 9223372036854775807; # on a 64-bit 
## machine, with a new version of Singular
#
### try to autodetect
##if  not IsBound( OBJLEN ) and OBJLEN = 8  then # Gap 4.3
##if  not IsBound( GAPInfo.BytesPerVariable ) and GAPInfo.BytesPerVariable 
## = 8  then # Gap 4.4
##    SingularLimitations.max_int := 9223372036854775807;
##else 
##    SingularLimitations.max_int := 2147483647;
##fi;


# the Singular process
Sing_Proc := fail; # not yet started

# Singular version, an integer, as in the output of the Singular command
# system("version");
# The interface will ask Singular for it.
SingularVersion := 0; # not yet determined;

SingularNr := rec(  );
# How many times did Gap (re)start Singular (as InputOutputLocalProcess)?
SingularNr.Session := 0;
# How many times did Gap (re)start Singular (as Process)?
SingularNr.Process := 0;
# How many times did Gap send input to Singular (in this session)?
SingularNr.Input := 0;
# How many times has Gap received output from Singular (in this session)?
SingularNr.Output := 0;

# This is used to generate unique names in Singular
SingularArgument := 1;

# The Libraries loaded in Singular
SingularLoadedLibraries := "";

# The Base Ring in Singular; the provided default should match the default 
# of Singular.
SingularBaseRing := PolynomialRing( GF( 32003 ), [ "x", "y", "z" ] );

ParseGapRingToSingRing := function (  ) end; # it will be defined later...
SingularSetBaseRing := function (  ) end; # it will be defined later...

# The next boolean is true if the ground field ( i.e.  
# UnderlyingField(SingularBaseRing); ) is GF(p^n) with n>1, or if it 
# is an extension of the Rationals.
IsNonPrimeGroundField:= false;
  
# The SingularBaseRing will be called GAP_ring in Singular,
# ideals will be called GAP_ideal_1, GAP_ideal_2, etc in Singular.
# If SingularNames.ideals = n, then the ideals GAP_ideal_1, ..., 
# GAP_ideal_n have been sent to Singular or received from Singular.
# It will be checked that the ideal names are valid for the current 
# SingularBaseRing of Singular.
# If SingularNames.ideals <= SingularNamesThisRing.ideals, then the
# name is out of date.

SingularNames:= rec( ideals:= 0 );

SingularNamesThisRing := ShallowCopy( SingularNames );


# <--- for debug, see above
fi; 


# This function checks whether the SingularIdentifier of an ideal is valid
# for the current session of Singular or it is the old SingularIdentifier
# of a previous SingularBaseRing

HasValidSingularIdentifier := function ( obj )

    local  n, s;

    if not HasSingularIdentifier( obj )  then
        return false;
    fi;

    s := SingularIdentifier( obj );
    n := Int( s{[ 11 .. Length( s ) ]} );
    if n > SingularNamesThisRing.ideals  then
        return true;
    else
        return false;
    fi;

end;



# The following code is for compatibility with older Gap versions

if not CompareVersionNumbers( VERSION, "4.4" ) then 

    if not IsBound( IsPolynomialRingIdeal)  then
        DeclareSynonym( "IsPolynomialRingIdeal",
         IsRing and IsRationalFunctionCollection and HasLeftActingRingOfIdeal
          and HasRightActingRingOfIdeal );
    fi;

    if not IsBound( IsMonomialOrdering)  then
        IsMonomialOrdering := ReturnFalse;
    fi;

    if not CompareVersionNumbers( VERSION, "4.3" )  then

        InstallOtherMethod( PositionSublist, "for an empty list", true,
         [ IsEmpty, IsList ], 0, ReturnFail );

        InstallOtherMethod( PositionSublist, "for an empty list", true,
         [ IsEmpty, IsList, IsInt ], 0, ReturnFail );

        if not IsBound( NormalizedWhitespace)  then
            NormalizedWhitespace := x -> ReplacedString( x, "\n", " ");
        fi;

        if not IsBound( EvalString)  then
            EvalString := function ( expr )
                  local  tmp;
                  tmp := Concatenation( "return ", expr, ";" );
                  return ReadAsFunction( InputTextString( tmp ) )(  );
              end;
        fi;

        JoinStringsWithSeparator := function ( arg )
              local  str, sep, res, i;
              str := List( arg[1], String );
              if Length( str ) = 0  then
                  return "";
              fi;
              if Length( arg ) > 1  then
                  sep := arg[2];
              else
                  sep := ",";
              fi;
              res := ShallowCopy( str[1] );
              for i  in [ 2 .. Length( str ) ]  do
                  Append( res, sep );
                  Append( res, str[i] );
              od;
              return res;
          end;

        if not CompareVersionNumbers( VERSION, "4.2" )  then

            if not IsBound( UnderlyingField)  then

                DeclareAttribute( "UnderlyingField", IsVectorSpace );

                InstallMethod( UnderlyingField, "for vector space", true,
                 [ IsVectorSpace ], 0, function ( V )
                      return LeftActingDomain( V );
                  end );

            fi;

        fi;

    fi;

fi;




##############################################################################
##############################################################################

## PART 2. Singular interface at low level ##




CheckSingularExecutableAndTempDir := function (  )
    local i;

    # check the Singular executable, and if needed try to autodetermine, 
    # or print an appropriate error message 

    if IsBound( sing_exec ) and IsString( sing_exec ) then
         if IsDirectoryPath( sing_exec ) = true  then
            sing_exec := Filename( Directory( sing_exec ), "Singular" );
        elif not IsExecutableFile( sing_exec ) = true and 
             not "/" in sing_exec  then
            sing_exec := Filename( DirectoriesSystemPrograms(  ), sing_exec );
        fi;
   fi;

    if not IsBound( sing_exec ) or not IsString( sing_exec ) or not 
           IsExecutableFile( sing_exec ) = true  or
           IsDirectoryPath( sing_exec ) = true  then
        sing_exec := Filename( DirectoriesSystemPrograms(  ), "Singular" );
        if sing_exec <> fail then 
            Info( InfoSingular, 2, "found Singular executable ", sing_exec );
        fi;
    fi;

    while not IsBound( sing_exec) or not IsString( sing_exec ) or not 
          IsExecutableFile( sing_exec ) = true  do
        Print( "  Type 'sing_exec:=\"<path>\"; return;' where <path>\n" );
        Print( "  is the path of the Singular executable on your system.\n" );
        if IsBound( sing_exec)  then
            if not IsString( sing_exec )  then
                Print( "  'sing_exec' must be a string.\n" );
            else
                Print( "'", sing_exec, "' is not an executable file.\n" );
            fi;
        fi;
        Error( "Singular executable not found!\n" );
    od;


    # check the Singuar command line options

    # sing_exec_options must be a dense list of strings
    if not (IsList( sing_exec_options ) and IsDenseList( sing_exec_options ) )
         then
        Error( "sing_exec_options must be a (dense) list\n" );
    fi;
    if not ForAll( sing_exec_options, IsString )  then
        Error( "All the components of sing_exec_options must be strings\n" );
    fi;

    # some options are necessary
    for i  in [ "-t" ]  do
        if not i in sing_exec_options  then
            Error( "Singuar command line option ", i, " is necessary\n" );
        fi;
    od;

    # some options are not supported
    for i  in [ "-h", "--help" ]  do
        if i in sing_exec_options  then
            Error( "Singuar command line option ", i, " not allowed\n" );
        fi;
    od;


    # check the temporary directory that will be used for i/o with Singular

    if not IsBound( SingularTempDirectory![1] ) or not 
           IsDirectoryPath( SingularTempDirectory![1] ) = true or not 
#           IsReadableFile( SingularTempDirectory![1] ) = true or not 
           IsWritableFile( SingularTempDirectory![1] ) = true or not
           IsExecutableFile( SingularTempDirectory![1] ) = true  then
        SingularTempDirectory := DirectoryTemporary( "Sing" );

        if SingularTempDirectory = fail  then
            Error( "Cannot create a temporary directory.\n" );
        fi;

        Info( InfoSingular, 2, "using temporary ", SingularTempDirectory );

    fi;

    SingularExecutableAndTempDirAreOk := true;

end;



# A function for closing Singular

CloseSingular := function (  )
    if IsStream( Sing_Proc )  then
        if not IsClosedStream( Sing_Proc )  then
            # WriteLine( Sing_Proc, ";quit;" );
            CloseStream( Sing_Proc );
        else
            Info( InfoSingular, 2, "Singular already closed." );
        fi;
    fi;
    # after closing Singular, the names become out of date.
    SingularNamesThisRing := ShallowCopy( SingularNames );
end;


# Kill Singular when Gap terminates
InstallAtExit( CloseSingular );



# The low level function for i/o with Singular. This function splits the 
# string with the Singular input into several lines, sends each of them to 
# Singular, waiting for the Singular prompt "> " or ". " at end of 
# output, relative to that line, before sending the next line. 
# This is necessary because some versions of Singular ignore the input 
# that is received before giving the prompt.
# After that, this function calls "GAP_Done ();" (to have a '@' in the 
# output, to be sure that Singular finished), waits to receive the 
# prompt "@\n> ", and then returns all the output of Singular.
# (The char before "> ", ". " or "@\n> " depends on the hardware
# or on the operating system, and on the sing_exec_options "-t".)

SingWriteAndReadUntilDone := function ( string )

    local read_blocking, read_non_blocking, read, out, OUT, s, i;

    read_blocking := ReadAll;

    read_non_blocking := function ( stream )
        local  sl, outl;
        outl := "";
        repeat
            sl := READ_IOSTREAM_NOWAIT( stream![1], 1 );
            if sl <> fail  then
                Append( outl, sl );
            fi;
        until sl = fail;
        return outl;
    end;

    # choose exactly one of the following lines:

    # read := read_non_blocking;
    read := read_blocking;

    # read_blocking: Gap blocks while Singular is running, resulting in a 
    # faster execution; Gap cannot be interrupted by <ctrl>-C in case of 
    # interface error. Suggested for normal use.
    
    # read_non_blocking: Gap keeps running while Singular is running, 
    # resulting in a slower execution; Gap can be interrupted by 
    # <ctrl>-C in case of interface error. Suggested for debugging.
    # Requires Gap version at least 4.3.

    if '$' in string  then
        # a '$' would close Singular...
        Print("Discarding the '$' in the Singular input\n");
        string := ReplacedString( string, "$", "");
    fi;

    string := SplitString( string, '\n' );
    out := "";
    OUT := "";

    for i  in [ 1 .. Length( string ) ]  do
        if Length( string[i] ) > 4000  then    # max ~4050
            Error( "Command line too long, please report\n" );
        fi;

        WriteLine( Sing_Proc, string[i] );

        SingularNr.Input := SingularNr.Input + 1;
        Info( InfoSingular, 3, "input ", SingularNr.Input, ": ", string[i] );

        repeat
            s := read( Sing_Proc );
            Append( out, s );
        until PositionSublist( out, "> ", Length( out ) - 2 ) <> fail
          or PositionSublist( out, ". ", Length( out ) - 2 ) <> fail;

        SingularNr.Output := SingularNr.Output + 1;
        Info( InfoSingular, 3, "output ", SingularNr.Output, ": ", out );

        Append( OUT, out );
        out := "";
    od;

    WriteLine( Sing_Proc, ";GAP_Done ();" );

    SingularNr.Input := SingularNr.Input + 1;
    Info( InfoSingular, 3, "input ", SingularNr.Input, ": ", ";GAP_Done ();" );


    repeat
        s := read( Sing_Proc );
        Append( out, s );

    until PositionSublist( out, "@\n> ", Length( out ) - 4 ) <> fail;

#   with a very old version of Singular replace the previous line with the 
#   following ones

#    until PositionSublist( out, "@\n" ) <> fail and
#          PositionSublist( out, "> ", Length( out ) - 2 ) <> fail;

    Append( OUT, out ); # is this needed?

#        # attempt to trap the Singular errors
#        pos := PositionSublist( OUT, "? error occurred in STDIN line " );
#        if pos <> fail  then
#             Error( "singular error" );
#        fi;

    SingularNr.Output := SingularNr.Output + 1;
    Info( InfoSingular, 3, "output ", SingularNr.Output, ": ", out );

    return OUT;
end;




StartSingular := function (  )

    local  file_in, out, s;


    # is there a previous Singular running?

    if IsStream( Sing_Proc ) and not IsClosedStream( Sing_Proc )  then
        CloseSingular(  );
    fi;


    CheckSingularExecutableAndTempDir(  );


    # We also provide Singular with a function for producing a '@'; this
    # enables us to let Singular write a '@' without putting one in the
    # input; the latter strategy proved to be confusing with the new 
    # version of Singular. (Another possibility would be to send to Singular
    # LIB("general.lib"); proc GAP_Done () { return ( ASCII(64) ) }; .)

    # perhaps would be better using a file in DirectoriesPackageLibrary

    file_in := Filename( SingularTempDirectory, "sing.in" );

    PrintTo( file_in, "proc GAP_Done () { return ( \"@\" ) };\n",
                      "proc GAP_Apostrophe () { return ( \"'\" ) };\n", 
                      "GAP_Done();\n" );


    # this starts Singular, attaches it to the process `Sing_Proc', and 
    # reads <file_in> with the commands given above

    Sing_Proc := InputOutputLocalProcess( SingularTempDirectory, 
       sing_exec, Concatenation( sing_exec_options, [ file_in ] ) );


    SingularNr.Session := SingularNr.Session + 1;

    SingularNr.Input := 0;
    SingularNr.Output := 0;


    # We get the Singular banner and discard any output.
    out := ReadAll( Sing_Proc );
    if out = fail  then
        Error( "Singular didn't start!\n",
               "Is correct the value of sing_exec ( ", sing_exec, " )?\n",
               "Does Singular work, when called as a standalone program?\n");
    fi;

    while PositionSublist( out, "@\n> ", Length( out ) - 4 ) = fail do

#   with a very old version of Singular replace the previous line with the
#   following ones

#    while PositionSublist( out, "@\n" ) <> fail and 
#          PositionSublist( out, "> ", Length( out ) - 2 ) = fail do
        s := ReadAll( Sing_Proc );
        Append( out, s );
    od;

#    SingularNr.Output:= SingularNr.Output + 1;
    Info(InfoSingular, 3, "output ", SingularNr.Output, ":\n", out);


    # Now we check that Singular is working, and test the interface
    out := SingWriteAndReadUntilDone( "" );


    # ask Singular, to determine its version
    out := SingWriteAndReadUntilDone( "system(\"version\");" );
    SingularVersion := Int( Filtered( out, IsDigitChar ) );
    # SingularVersion := SingularInterface( "system", [ "version" ], "int" );
    Info( InfoSingular, 1, "Started Singular (version ", SingularVersion, 
          ")" );


    # set the base ring in Singular according to the SingularBaseRing in Gap.
    SingularSetBaseRing( SingularBaseRing );


end;




# Under construction, not yet ready, and probably no more needed:
# two functions to try to reset Singular in case something goes wrong;
# they shouldn't be necessary
resetSing := function (  )

    local wait, arrives, out, OUT, ok, r, r1, r2, s;

    wait := false;
    arrives := true;
    out := "";
    OUT := "";
    ok := false;
    WriteLine( Sing_Proc, "}}};\n;" );
    repeat
        r := String( Random( [ 1000 .. 9999 ] ) );
        r1 := Concatenation( "GAP_Apostrophe ();", r, ";GAP_Apostrophe ();\n");
        r2 := Concatenation( "\n'\n", r, "\n'\n> " );
        WriteLine( Sing_Proc, r1 );
        while PositionSublist( out, "\n> " ) = fail  do
            repeat
                s := READ_IOSTREAM_NOWAIT( Sing_Proc![1], 1 );
                if s <> fail  then
                    Append( out, s );
                    wait := true;
                    arrives := true;
                else
                    if arrives = true  then
                        wait := false;
                    fi;
                    arrives := false;
                fi;
            until not arrives;
            if not wait and PositionSublist( out, "\n> " ) = fail  then
                WriteLine( Sing_Proc, "}}}\"}}};\n;" );
            fi;
            wait := true;
        od;
        if PositionSublist( out, r ) <> fail  then
            ok := true;
        fi;
        Append( OUT, out );
        out := "";
    until ok;

    Print( "Length ", Length( OUT ), "\n" );
    SingularNr.Output := SingularNr.Output + 1;
    Info(InfoSingular, 3, "output ", SingularNr.Output, ":\n", OUT);

    return true;
end;



doneSing := function (  )
    return WriteLine( Sing_Proc, "GAP_Done ();" );
end;





##############################################################################
##############################################################################

## PART 3. Singular interface at medium level ##



# this function writes a Gap string to a file (that will be read by Singular)
# without the '\' at the end of the lines: the '\' confuses Singular

AppendStringToFile := function ( file, s )
    local  otf;
    otf := OutputTextFile( file, true );
    SetPrintFormattingStatus( otf, false );
    AppendTo( otf, s );
    CloseStream( otf );
end;



# This function reads a file (written by Singular), and returns it as a 
# string to Gap, without the "\n", that confuse Gap.
ReadStringFromFile := function ( file )
    local  itf, r;
    itf := InputTextFile( file );
    r := ReadAll( itf );
    CloseStream( itf );
    return NormalizedWhitespace( r );
end;



WithoutEndingSemicolon := function ( string )
    local  i;
    i := Length( string );
    while i > 0  do
        if string[i] = ' '  then
            i := i - 1;
        elif string[i] = ';'  then
            string[i] := ' ';
        else
            break;
        fi;
    od;
    return string;
end;



# This function is under construction...
EscapeCharsInString := function ( string )
    string := ReplacedString( string, "\n", "\\\n" );
    string := ReplacedString( string, "\"", "\\\"" );
    string := ReplacedString( string, "'", "\\'" );
    string := ReplacedString( string, "\\", "\\\\" );
    string := ReplacedString( string, "\b", "\\\b" );
    string := ReplacedString( string, "\r", "\\\r" );
    string := ReplacedString( string, "\c", "\\\c" );
    return string;
end;



# In the following functions, 'precommand' is used, for instance, to send
# the SingularBaseRing, then only the output of 'command' will be returned.
# 'command' must be a single command, but 'precommand' may be a
# comma-separated list of commands

# "Stream" and "File", in the name of the following functions, refers only 
# to the way of sending the mathematical data, all these functions use the
# stream for low-level communications.

SingCommandInStreamOutStream := function ( precommand, command )

    local  singcom, out, pos1, pos2;

    if not IsStream( Sing_Proc ) or IsClosedStream( Sing_Proc )  then
        StartSingular(  );
    fi;

    # test the input
    if '@' in precommand or '@' in command  then
        Print( "Please do not use '@' in the commands \n" );
        return fail;
    fi;
    if ''' in precommand or ''' in command  then
        Print( "Please do not use ''' in the commands \n" );
        return fail;
    fi;

    # prepare the input to Singular, asking for an output between two '''
    singcom := Concatenation( precommand, ";\nGAP_Apostrophe();",
                              command, ";GAP_Apostrophe();" );    

    # send it, and get the output of Singular
    out := SingWriteAndReadUntilDone( singcom );

    pos1 := PositionSublist( out, "'\n" );
    if pos1 = fail  then
        Error( "Output of Singular only partially retrieved\n" );
    fi;

    pos2 := PositionSublist( out, "\n'\n", pos1 );
    if pos2 = fail  then
        Error( "Output of Singular only partially retrieved\n" );
    fi;

    # return the output, without the ''' and the "\n", 
    return out{[ pos1 + 2 .. pos2 - 1 ]};

end;



SingCommandInFileOutStream := function ( precommand, command )

    local file_in, out, pos1, pos2;

    if not IsStream( Sing_Proc ) or IsClosedStream( Sing_Proc )  then
        StartSingular();
    fi;

    # test the input
    if '@' in precommand or '@' in command  then
        Print( "Please do not use '@' in the commands \n" );
        return fail;
    fi;
    if ''' in precommand or ''' in command  then
        Print( "Please do not use ''' in the commands \n" );
        return fail;
    fi;

    # the input file
    file_in:= Filename( SingularTempDirectory, "sing.in" );

    # to be safe
    RemoveFile( file_in );

    # write the input for Singular in 'file_in'
    AppendStringToFile( file_in, Concatenation( precommand,
           ";\nGAP_Apostrophe();", command, ";GAP_Apostrophe();" ) );

    # tell Singular to read and execute 'file_in', and get the output
    out := SingWriteAndReadUntilDone( "< \"sing.in\";" );

    pos1 := PositionSublist( out, "'\n" );
    if pos1 = fail  then
        Error( "Output of Singular only partially retrieved\n" );
    fi;

    pos2 := PositionSublist( out, "\n'\n", pos1 );
    if pos2 = fail  then
        Error( "Output of Singular only partially retrieved\n" );
    fi;

    # the output, without the ''' and the "\n"
    out := out{[ pos1 + 2 .. pos2 - 1 ]};

    if InfoLevel( InfoSingular ) < 3 then
        RemoveFile( file_in );
    fi;

    return out;
end;



SingCommandInFileOutFile := function ( precommand, command )

    local file_in, file_out, out;


    if not IsStream( Sing_Proc ) or IsClosedStream( Sing_Proc )  then
        StartSingular();
    fi;

    # test the input
    if '@' in precommand or '@' in command  then
        Print( "Please do not use '@' in the commands \n" );
        return fail;
    fi;

    # the input and output files
    file_in:= Filename( SingularTempDirectory, "sing.in" );
    file_out:= Filename( SingularTempDirectory, "sing.out" );

    # to be safe
    RemoveFile( file_in );
    RemoveFile( file_out );

    # write the input for Singular in 'file_in'
    AppendStringToFile( file_in, precommand );
    AppendStringToFile( file_in, ";\n" );

    if command <> ""  then
        AppendStringToFile( file_in, "write( \"sing.out\", " );
        AppendStringToFile( file_in, WithoutEndingSemicolon( command ) );
        AppendStringToFile( file_in, " );\n" );
    fi;

    # tell Singular to read and execute 'file_in', and get the output
    out := SingWriteAndReadUntilDone( "< \"sing.in\";" );

    if command <> ""  then
        if not IsExistingFile( file_out ) then
            Error( "Singular didn't write the output to the file\n" );
        fi;

        out := ReadStringFromFile( file_out );
    fi;

    if InfoLevel( InfoSingular ) < 3 then
        RemoveFile( file_in );
        RemoveFile( file_out );
    fi;

    if command <> ""  then
        return out;
    else
        return "";
    fi;

end;



SingCommandInStreamOutFile := function ( precommand, command )

    local file_out, out, singcom;

    if not IsStream( Sing_Proc ) or IsClosedStream( Sing_Proc )  then
        StartSingular();
    fi;

    # test the input
    if '@' in precommand or '@' in command  then
        Print( "Please do not use '@' in the commands \n" );
        return fail;
    fi;

    # the output file
    file_out:= Filename( SingularTempDirectory, "sing.out" );

    # to be safe
    RemoveFile( file_out );

    # send the input to Singular, asking to write it in file_out

    out := SingWriteAndReadUntilDone( precommand );

    if command <> ""  then

        singcom := "write( \"sing.out\", ";
        Append( singcom, WithoutEndingSemicolon( command ) );
        Append( singcom, " );\n" );

        out := SingWriteAndReadUntilDone( singcom );

        if not IsExistingFile( file_out ) then
            Error( "Singular didn't write the output to the file\n" );
        fi;

        out := ReadStringFromFile( file_out );


        if InfoLevel( InfoSingular ) < 3 then
            RemoveFile( file_out );
        fi;

        return out;

    else
        return "";
    fi;

end;



# The following function doesn't use InputOutputLocalProcess,
# so it can be used under Windows with Gap version < 4.4

SingCommandUsingProcess := function ( precommand, command )

    local  _in, out, _out, opt, file_in, file_out;


    if not SingularExecutableAndTempDirAreOk  then
        CheckSingularExecutableAndTempDir(  );
    fi;

    # the input and output files
    file_in:= Filename( SingularTempDirectory, "sing.in" );
    file_out:= Filename( SingularTempDirectory, "sing.out" );

    # to be safe
    RemoveFile( file_in );
    RemoveFile( file_out );

    # write the input for Singular in 'file_in'
    AppendStringToFile( file_in, SingularLoadedLibraries );
    AppendStringToFile( file_in, ParseGapRingToSingRing( SingularBaseRing ) );
    AppendStringToFile( file_in, precommand );
    AppendStringToFile( file_in, ";\n" );

    if command <> ""  then
        AppendStringToFile( file_in, "write( \"sing.out\", " );
        AppendStringToFile( file_in, WithoutEndingSemicolon( command ) );
        AppendStringToFile( file_in, " );\n" );
    fi;


    _in := InputTextNone(  );
    _out := OutputTextNone(  );
    opt := Concatenation( "--execute=", "< \"sing.in\";", ";quit;" );

    Process( SingularTempDirectory, sing_exec, _in, _out, 
             Concatenation( sing_exec_options, [ opt ] ) );

    CloseStream( _in );
    CloseStream( _out );

    if command <> ""  then
        if not IsExistingFile( file_out ) then
            Error( "Singular didn't write the output to the file\n" );
        fi;

        out := ReadStringFromFile( file_out );

    fi;

    if InfoLevel( InfoSingular ) < 3 then
        RemoveFile( file_in );
        RemoveFile( file_out );
    fi;

    SingularNr.Process := SingularNr.Process + 1;

    if command <> ""  then

        if not CompareVersionNumbers( VERSION, "4.2" ) and 
        # the output of "Process" contains an extra space at the end
           Length( out ) > 0 and out{[ Length( out ) ]} = " "  then
            out := out{[ 1 .. Length( out ) - 1 ]};
        fi;

        return out;
    else
        return "";
    fi;

end;


if ARCH_IS_UNIX(  ) and CompareVersionNumbers( VERSION, "4.2" ) or
# comment out the next line with a 4.4 pre-release!
   CompareVersionNumbers( VERSION, "4.4" )
 then

    # "InputOutputLocalProcess" is available
    # choose one
    SingularCommand := SingCommandInStreamOutStream;
    #SingularCommand := SingCommandInFileOutStream;
    #SingularCommand := SingCommandInFileOutFile;
    #SingularCommand := SingCommandInStreamOutFile;
    #SingularCommand := SingCommandUsingProcess; # not recommended!

else

    # "InputOutputLocalProcess" doesn't work yet, "Process" will be used
    SingCommandInStreamOutStream := ReturnFail;
    HasValidSingularIdentifier := ReturnFalse;
    SingularCommand := SingCommandUsingProcess;
#    SingularVersion := Int( SingularCommand( "", "system(\"version\");" ) );

fi;



##############################################################################
##############################################################################

## PART 4. Parsing Gap --> Singular ##

# Some functions to convert Gap objects into strings that represent
# Singular objects.



# This function determines the Singular type of a Gap object

SingularType := function ( obj )

    local IsSingInt, IsSingPoly;

    # this function tells whether a Gap object corresponds to a Singular
    # object of type "int"
    IsSingInt := function ( n )
        if IsInt( n )  then
            return - SingularLimitations.max_int <= n and 
                   n <= SingularLimitations.max_int;
        elif IsBool( n )  then
            return n = true or n = false;
        else
            return false;
        fi;
    end;


    # this function tells whether a Gap object corresponds to a Singular
    # object of type "poly"
    IsSingPoly := p -> HasIsPolynomial( p ) and IsPolynomial( p ) and 
                      p in SingularBaseRing;


    # ideal
    # IsIdeal doesn't work! IsPolynomialRingIdeal could be used.
    # if IsIdeal( obj )  then 
    if HasLeftActingRingOfIdeal( obj ) and HasGeneratorsOfTwoSidedIdeal( obj )
        and ForAll( GeneratorsOfTwoSidedIdeal( obj ), IsPolynomial )  then
        return "ideal";

    # int
    elif IsSingInt( obj )  then
        return "int";

    # intmat
    elif IsMatrix( obj ) and ForAll( obj, x -> ForAll( x, IsSingInt ) ) then
        return "intmat";

    # intvec
    elif IsRowVector( obj ) and ForAll( obj, IsSingInt )  then
        return "intvec";

    # matrix 
    elif IsMatrix( obj ) and ForAll( obj, x -> ForAll( x, y -> 
                                          IsSingPoly( y ) ) )  then
        return "matrix";

    # number
    elif obj in CoefficientsRing( SingularBaseRing )  then
        return "number";

    # poly
    elif IsSingPoly( obj )  then
        return "poly";


    # ring
    elif HasIsRing( obj ) and IsRing( obj )  and #IsPolynomialRing
         HasIndeterminatesOfPolynomialRing( obj )  then
        return "ring";

    # string
    elif IsString( obj )  then
        return "string";

    # vector
    elif IsRowVector( obj ) and ForAll( obj, y -> IsSingPoly( y ) )  then
        return "vector";


    # list (to be done after intmat, intvec, matrix, string, vector)
    elif IsList( obj ) and IsDenseList( obj ) and 
         ForAll( obj, y -> SingularType(y) <> fail ) then
        return "list";


    else
        return fail;
    fi;

end;



ParseToSingInt := function ( n )
    if IsInt( n )  then
        return String( n );
    elif IsBool( n ) and n = true  then
        return "1";
    elif IsBool( n ) and n = false  then
        return "0";
    else
        Error( "The object ", n, " is not a Singular \"int\".\n" );
    fi;
end;



ParseGapNumberToSingNumber := function ( n )

    local  eroo, str, i;

    if IsNonPrimeGroundField  then

        if Characteristic( SingularBaseRing ) = 0  then
            if IsRat( n )  then
                return String( n );
            fi;
            eroo := ExtRepOfObj( n );
            str := "( ";
            for i  in [ 1 .. Length( eroo ) ]  do
                Append( str, String( eroo[i] ) );
                Append( str, "*q^" );
                Append( str, String( i - 1 ) );
                if i < Length( eroo )  then
                    Append( str, "+" );
                fi;
            od;
            Append( str, " )" );
            return str;

        else

            return Concatenation( "( q^", String( LogFFE( n, 
        PrimitiveRoot( CoefficientsRing( SingularBaseRing ) ) ) ), " )" );

        fi;

    else

        if Characteristic( SingularBaseRing ) = 0  then
            return String( n );
        else
            return String( IntFFE( n ) );
        fi;

    fi;

end;



ParseGapPolyToSingPoly:= function ( pol )
    
    # pol is a GAP polynomial, we parse it into a string representing
    # a Singular polynomial.
    
    local   varnums,  str,  mons,  k,  mon,  m,  len;
    
    if not pol in SingularBaseRing  then
        Error( "The polynomial ", pol, " is not in the Singular Base Ring ",
               SingularBaseRing, "\n" );
    fi;

    if IsZero( pol )  then
        return "0";
    fi;

    varnums:= IndeterminateNumbers( SingularBaseRing );
    str:= "";
    mons:= ExtRepPolynomialRatFun( pol );
    k:= 1;
    
    len:= 0;
    
    while k <= Length( mons ) do
        
        # after 1000 chars we append a "\n", to avoid too long lines
        if Length( str )-len >= 1000 then 
            Append( str, "\n" ); 
            len:= Length( str );
        fi;
        
        if k > 1 then Add( str, '+' ); fi;
        
        Append( str, ParseGapNumberToSingNumber( mons[k+1] ) );
        
        mon:= mons[k];
        m:= 1;
        while m <= Length( mon ) do
            Append( str, "*x_" );
            Append( str, String( Position( varnums, mon[m] ) ) );
            Append( str, "^" );
            if mon[m + 1] >= SingularLimitations.max_exp_ring_var  then
                Error( "Singular supports only exponents of a ring ",
                       "variables smaller than ", 
                       SingularLimitations.max_exp_ring_var, "\n" );
            fi;
            Append( str, String( mon[m+1] ) );
            m:=m+2;
        od;
        k:= k+2;
    od;
    
    return str;
end;




ParseGapOrderingToSingOrdering := function( tor )

    # A TermOrdering of a ring R is either a string ( "lp", "dp", "Dp" ),
    # meaning that the corresponding term ordering in Singular is chosen,
    # or a list of the form (e.g.) [ "dp", 3, "lp", 2 ], meaning that 
    # the first three indets are ordered by dp, the remaining two by lp.
    # If a weighted ordering is specified ( "wp", "Wp" ), then the next 
    # element in the list is not an integer, but the weight vector.
    
    local  to, i, j, name;

    if IsString( tor )  then
        return tor;

    elif IsList( tor )  then
        to := "(";
        for i  in [ 1, 3 .. Length( tor ) - 1 ]  do
            if i <> 1  then
                Append( to, ", " );
            fi;
            Append( to, tor[i] );
            Append( to, "(" );
            if not tor[i] in [ "wp", "Wp" ]  then
                Append( to, String( tor[i + 1] ) );
            else
                for j  in [ 1 .. Length( tor[i + 1] ) ]  do
                    if j <> 1  then
                        Append( to, "," );
                    fi;
                    Append( to, String( tor[i + 1][j] ) );
                od;
            fi;
            Append( to, ")" );
        od;
        Append( to, ")" );


    elif IsMonomialOrdering( tor )  then
        name := Name( tor );
        if name = "MonomialLexOrdering()"  then
            to := "lp";
        elif name = "MonomialGrevlexOrdering()"  then
            to := "dp";
        elif name = "MonomialGrlexOrdering()"  then
            to := "Dp";
        elif name = "EliminationOrdering()"  then
            Error( "Ordering EliminationOrdering not yet supported" );
        else
            Error( "Ordering not recognized" );
        fi;

    elif IsFunction( tor )  then
        if tor = MonomialTotalDegreeLess  then
        # but now the name is MonomialExtGrlexLess
            to := "dp";
        else
            Error( "term ordering ", tor, " not implemented\n" );
        fi;

    else
        Error( "The ring does not have a valid term ordering" );
    fi;

    return to;
end;




ParseGapRingToSingRing := function ( R )

    local varnums, str, F, minpol, f, ef, i, ipr, mcf;

    F:= CoefficientsRing( R );


# Check that the field is supported by Singular

    if Characteristic( F ) > 0  then
        if IsPrimeField( F )  then

            if SingularVersion <= 2003  then
                if Characteristic( F ) > 32003 and Characteristic( F ) <= 
                  SingularLimitations.max_char_prime_field  then
                    Error( "only prime fields of char <= 32003 are ",
                     "supported by your version of \nSingular: upgrade it ",
                     "to use prime fields of char <= ",
                     SingularLimitations.max_char_prime_field, ". \n" );
                elif Characteristic( F ) > 
                  SingularLimitations.max_char_prime_field  then
                    Error( "only prime fields of char <= 32003 are ",
                     "supported by your version of \nSingular (or prime ",
                     "fields of char <= ", 
                     SingularLimitations.max_char_prime_field,
                     " by the latest version.)\n" );
                fi;
            else
                if Characteristic( F ) > 
                  SingularLimitations.max_char_prime_field  then
                    Error( "only prime fields of char <= ",
                     SingularLimitations.max_char_prime_field,
                     " are supported by Singular \n" );
                fi;
            fi;

        else

            if Size( F ) > SingularLimitations.max_size_nonprime_field  then
                Error( "Singular supports finite but non-prime fields ",
                 "only if \nof size <= ",
                 SingularLimitations.max_size_nonprime_field, ". \n" );
            fi;

        fi;
    else

        if not (HasIsCyclotomicField( F ) and IsCyclotomicField( F ) or 
    IsAlgebraicExtension( F ) and LeftActingDomain( F ) = Rationals)  then

           Error( "in Characteristic 0, only CyclotomicField's (including ",
             "Rationals) and\nAlgebraicExtension's of Rationals are ",
             "supported by the Singular interface \nand by Singular.\n" );

        fi;

    fi;


# In Singular, a ring declaration is of the form
# ring name = (coefficient_field), (names_of_ring_variables), (ordering);
# possibly followed by a 
# minpoly = (poly);



    str := "ring GAP_ring = ";

    
# Calculating "(coefficient_field)" and minpoly

    if IsPrimeField( F )  then

        IsNonPrimeGroundField := false;
        Append( str, String( Characteristic( F ) ) );
        minpol := "";

    else

        IsNonPrimeGroundField := true;
            
        Append( str, "( " );
        Append( str, String( Characteristic( F ) ) );
        Append( str, ", q )" );

        # Compute a string representing the minimum polynomial of a
        # primitive element of F.

        if Characteristic( F ) > 0  then
            f:= MinimalPolynomial( PrimeField(F), PrimitiveRoot(F), 1 );
        elif HasIsCyclotomicField( F ) and IsCyclotomicField( F )  then
            f:= MinimalPolynomial( PrimeField(F), PrimitiveElement(F), 1 );
        else
            f:= DefiningPolynomial( F );
        fi;
        ef:= ExtRepPolynomialRatFun( f );
        minpol:= " minpoly = ";
        for i in [1,3..Length(ef)-1] do
            if i<>1 then Append( minpol, "+" ); fi;
            if Characteristic( F ) > 0  then
                Append( minpol, String( IntFFE( ef[i+1] ) ) );
            else
                Append( minpol, String( ef[i+1] ) );
            fi;
            if ef[i] <> [] then
                Append( minpol, "*q^" );
                Append( minpol, String( ef[i][2] ) );
            fi;
        od;
        Append( minpol, ";" );
    fi;
    

# Calculating ", (names_of_ring_variables),"

    ipr := ShallowCopy( IndeterminatesOfPolynomialRing( R ) );

    if HasTermOrdering( R ) and IsMonomialOrdering( TermOrdering( R ) )  then
        mcf := MonomialComparisonFunction( TermOrdering( R ) );
        Sort( ipr, mcf );
        ipr := Reversed( ipr );
    fi;

    varnums := List( ipr, x -> ExtRepPolynomialRatFun( x )[1][1] );
    SetIndeterminateNumbers( R, varnums );

    Append( str, ", (" );
    
    for i in [1..Length(varnums)] do
        Append( str, "x_" );
        Append( str, String( i ) );
        if i<>Length(varnums) then Append( str, "," ); fi;
    od;

    Append( str, ")," ); 
    
    
# Calculating "(ordering);"

    if HasTermOrdering( R ) then
        Append( str, ParseGapOrderingToSingOrdering( TermOrdering( R ) ) );
    else
        # the default "dp" is used
        Append( str, "dp" );
    fi;
    
    Append( str, "; " );


# done

    Append( str, minpol );
    Append( str, "\n" );

    return str;
end;



ParseGapIdealToSingIdeal:= function ( I )

    local   idealno,  name,  str,  pols,  k;    
    
    if LeftActingRingOfIdeal( I ) <> SingularBaseRing  then
        Error( "Any ideal sent to Singular must be an ideal of ",
               "SingularBaseRing" );
    fi;
    
    idealno:= SingularNames.ideals+1;
    SingularNames.ideals:= idealno;
    name:= "GAP_ideal_"; 
    Append( name, String( idealno ) );
    
    SetSingularIdentifier( I, name );
    
    str:= "ideal GAP_ideal_";
    Append( str, String( idealno ) );
    Append( str, " = \n" );

    pols:= GeneratorsOfTwoSidedIdeal( I );
    for k in [1..Length(pols)] do
        Append( str, ParseGapPolyToSingPoly( pols[k] ) );
        if k < Length( pols ) then 
            Append( str, ",\n" ); 
        else 
            Append( str, ";\n" );
        fi;
    od;    
    
    return str;
end;



ConvertGapObjToSingObj := function ( obj )

    local headers, precommand, type, dim, singsubobj, i, j;


    # Usually the interface determines the type, but this can be 
    # overridden specifying it like the following example:
    # rec( Object := [ 1, 2 ], SingularType := "list" );
    # otherwise [ 1, 2 ] will be of type "intvec".

    if IsRecord( obj ) and IsBound( obj.SingularType ) and 
       IsBound( obj.Object )  then
        type := obj.SingularType;
        obj := obj.Object;
    else
        type := SingularType( obj );
    fi;


    headers := "";
    precommand := "";


    # ideal
    if type = "ideal"  then
        if LeftActingRingOfIdeal( obj ) <> SingularBaseRing then
            SingularSetBaseRing( LeftActingRingOfIdeal( obj ) );
        fi;
        Append( headers, "setring( GAP_ring );\n" );
        if not HasValidSingularIdentifier( obj )  then
            Append( headers, ParseGapIdealToSingIdeal( obj ) );
        fi;
        Append( precommand, SingularIdentifier( obj ) );

    # int
    elif type = "int"  then
        Append( precommand, ParseToSingInt( obj ) );

    # intmat
    elif type = "intmat"  then
        dim := DimensionsMat( obj );
        Append( headers, "intmat GAP_intmat" );
        Append( headers, String( SingularArgument ) );
        Append( headers, String( [ dim[1] ] ) );
        Append( headers, String( [ dim[2] ] ) );
        Append( headers, " = " );
        for i  in [ 1 .. dim[1] ]  do
            # to avoid too long lines, and for clarity
            Append( headers, "\n" );
            for j  in [ 1 .. dim[2] ]  do
                Append( headers, ParseToSingInt( obj[i][j] ) );
                if not (i = dim[1] and j = dim[2])  then
                    Append( headers, "," );
                fi;
                # to avoid too long lines
                if j mod 50 = 0 then
                    Append( headers, "\n" );
                fi;
            od;
        od;
        Append( headers, ";\n" );
        Append( precommand, "GAP_intmat" );
        Append( precommand, String( SingularArgument ) );
        SingularArgument := SingularArgument + 1;

    # intvec
    elif type = "intvec"  then
        dim := Length( obj );
        Append( headers, "intvec GAP_intvec" );
        Append( headers, String( SingularArgument ) );
        Append( headers, " = " );
        for i  in [ 1 .. dim ]  do
            Append( headers, ParseToSingInt( obj[i] ) );
            if not (i = dim)  then
                Append( headers, "," );
            fi;
            # to avoid too long lines
            if i mod 50 = 0 then
                Append( headers, "\n" );
            fi;
        od;
        Append( headers, ";\n" );
        Append( precommand, "GAP_intvec" );
        Append( precommand, String( SingularArgument ) );
        SingularArgument := SingularArgument + 1;

    # list
    elif type = "list"  then
        dim := Length( obj );
        Append( precommand, "list( " );
        for i  in [ 1 .. dim ]  do
            singsubobj := ConvertGapObjToSingObj( obj[i] );
            Append( headers, singsubobj[1] );
            Append( precommand, singsubobj[2] );
            if i < dim  then
                Append( precommand, ", " );
            fi;
            # to avoid too long lines
            if i mod 50 = 0 then
                Append( headers, "\n" );
            fi;
        od;
        Append( precommand, " )" );

    # matrix 
    elif type = "matrix"  then
        dim := DimensionsMat( obj );
        Append( headers, "matrix GAP_matrix" );
        Append( headers, String( SingularArgument ) );
        Append( headers, String( [ dim[1] ] ) );
        Append( headers, String( [ dim[2] ] ) );
        Append( headers, " = " );
        for i  in [ 1 .. dim[1] ]  do
            # to avoid too long lines, and for clarity
            Append( headers, "\n" );
            for j in [ 1 .. dim[2] ]  do
                Append( headers, ParseGapPolyToSingPoly( obj[i][j] ) );
                if not (i = dim[1] and j = dim[2])  then
                    Append( headers, "," );
                fi;
                # to avoid too long lines
                if j mod 50 = 0 then
                    Append( headers, "\n" );
                fi;
            od;
        od;
        Append( headers, ";\n" );
        Append( precommand, "GAP_matrix" );
        Append( precommand, String( SingularArgument ) );
        SingularArgument := SingularArgument + 1;


    # number
    elif type = "number"  then
        Append( precommand, ParseGapNumberToSingNumber( obj ) );

    # poly
    elif type = "poly"  then
        Append( precommand, ParseGapPolyToSingPoly( obj ) );


    # ring
    elif type = "ring"  then
        if obj <> SingularBaseRing then
            SingularSetBaseRing( obj );
        fi;
        Append( headers, "setring( GAP_ring );\n" );
        Append( precommand, "GAP_ring" );

    # string
    elif type = "string"  then
        # the next two lines are necessary when the string is sent via a 
        # stream
        obj := ReplacedString( obj, "\\", "\\\\" );
        obj := ReplacedString( obj, "\"", "\\\"" );
#        obj := EscapeCharsInString( obj );
        Append( precommand, "\"" );
        Append( precommand, obj );
        Append( precommand, "\"" );

    # vector
    elif type = "vector"  then
        dim := Length( obj );
        Append( headers, "vector GAP_vector" );
        Append( headers, String( SingularArgument ) );
        Append( headers, " = " );
        Append( headers, "[" );
        for i  in [ 1 .. dim ]  do
            Append( headers, ParseGapPolyToSingPoly( obj[i] ) );
            if not (i = dim)  then
                Append( headers, "," );
            fi;
            # to avoid too long lines
            if i mod 50 = 0 then
                Append( headers, "\n" );
            fi;
        od;
        Append( headers, "]" );
        Append( headers, ";\n" );
        Append( precommand, "GAP_vector" );
        Append( precommand, String( SingularArgument ) );
        SingularArgument := SingularArgument + 1;


    else
       Print( "Sorry: Singular, or the interface to Singular, or the ",
              "current \nSingularBaseRing, do not support the object " );
       Print( obj, ".\n(Your code to support it will be welcome!)\n" );
        return fail;
    fi;

    return [ headers, precommand ];
end;




##############################################################################
##############################################################################

## PART 5. Parsing Singular --> Gap ##

# Some functions to convert strings that represent Singular
# objects into Gap objects




ParseSingNumberToGapNumber:= function ( str )
    
    # We note that (at least for now) the
    # primitive elements in Singular are always called `q'.
    # That is, non-prime fields...

    # here `str' is a string representing a finite field element
    # of a non-prime field in Singular. This is just a polynomial
    # in `q' over the integers. So this function more or less copies the
    # parse function for polynomials, only each time for `q' substituting
    # the primitive root of the ground field.
        
    local   F,  len,  k,  coef,  cf,  exp,  res;

    F := CoefficientsRing( SingularBaseRing );
    res:= Zero( F );

    len:= Length( str );
    k:= 1;

    while k <= len do
        
        # we parse the coefficient of the monomial, and we first get
        # the sign of that coefficient.
    
        if str[k] = '-' then 
            coef:= "-"; 
            k:= k+1;
        elif str[k]='+' then 
            coef:=""; 
            k:=k+1;
        else 
            coef:="";
        fi;
        
        # now we get the coefficient itself

        while k <= len and (str[k] in CHARS_DIGITS or str[k] = '/')  do
            Add( coef, str[k] );
            k:=k+1;      
        od;
        
        if k <= len and str[k] = '*' then k:= k+1; fi;
        
        # if the coefficient is 1, then nothing has been done in the
        # previous loop...
        
        if coef = "" then coef:= "1"; fi;
        if coef = "-" then coef:= "-1"; fi;
        
        cf:= Rat( coef );
        
        # note that if the monomial only consists of a coefficient
        # (i.e., constant monomial), then we will not enter the next
        # loop, and a [] will be added to mons, just as it should.
        
        exp:= 0;
        if k <= len and str[k] = 'q' then
     
            k:= k+1;

            # Now we get the exponent:
            
            if k <= len and str[k] = '^' then
                exp:= "";
                k:= k+1;
                while k <= len and str[k] in CHARS_DIGITS do
                    Add( exp, str[k] );
                    k:= k+1;
                od;
                exp:= Int( exp );
            else
                exp:= 1;
            fi;
        fi;

        if Characteristic( F ) > 0 then
            res:= res + cf*PrimitiveRoot( F )^exp;
        elif HasIsCyclotomicField( F ) and IsCyclotomicField( F )  then
            res:= res + cf*PrimitiveElement( F )^exp;
        else
            res:= res + cf*RootOfDefiningPolynomial( F )^exp;
        fi;
                              
    od;

    return res;
end;



ParseSingPolyToGapPoly:= function ( str )
    
    # here `str' is a string representing a polynomial in Singular
    # format. We parse it into a GAP polynomial.
    # so a substring of the form `x_21' in `st' means the 21st element
    # from `IndeterminateNumbers( SingularBaseRing )'. 
        
    local   len,  mons,  cfs,  k,  mon,  coef,  ind,  exp,  
            erep, pos, fam;

    mons:= [ ];
    cfs:= [ ];

    len:= Length( str );
    k:= 1;
    
    while k <= len do
        
        mon:= [ ];

        # we parse the coefficient of the monomial, and we first get
        # the sign of that coefficient.

        if str[k] = '-' then 
            coef:= "-"; 
            k:= k+1;
        elif str[k]='+' then 
            coef:=""; 
            k:=k+1;
        else 
            coef:="";
        fi;

        # now we get the coefficient itself

        while k <= len and str[k] <> 'x' do
            if str[k] <> '*' then 
                Add( coef, str[k] ); 
            fi;  
            k:=k+1;      
        od;

        # we parse the coefficient, this is different if the ground 
        # field is not prime.

        if IsNonPrimeGroundField then
        
            pos:= Position( coef, '(' );
            if pos <> fail then
                # it is a polynomial in q; get rid of the (), and parse
                RemoveElmList( coef, pos );
                pos:= Position( coef, ')' );
                RemoveElmList( coef, pos );
                Add( cfs, ParseSingNumberToGapNumber( coef ) );
            else
                # it is a prime field element, written as a rational;
                if coef = "" then coef:= "1"; fi;
                if coef = "-" then coef:= "-1"; fi;
#                Add( cfs, ParseSingNumberToGapNumber( coef ) );
                Add( cfs, Rat( coef )*
                          One( CoefficientsRing( SingularBaseRing ) ) );
            fi;

        else

            # if the coefficient is 1, then nothing has been done in the
            # previous loop...

            if coef = "" then coef:= "1"; fi;
            if coef = "-" then coef:= "-1"; fi;

            Add( cfs, Rat( coef )*
                      One( CoefficientsRing( SingularBaseRing ) ) );
        fi;
            
####################################################################
#
#    OLD code, which works for the old (1.x.x ?) version of Singular:
#            
#            
#            # first we look for the opening '('
#            # (only not there is the coefficient is 1)
#            while k <= len and str[k] <> 'x' and str[k] <> '(' do
#                k:= k+1;
#            od;
#            if k <= len and str[k] = 'x' then
#                coef:= "q^0";
#            else
#                k:= k+1;
#                # now we look for the closing ); everything in between 
#                # belongs to the coefficient.
#                while str[k] <> ')' do
#                    Add( coef, str[k] );
#                    k:= k+1;
#                od;
#                k:= k+1;
#                if k <= len and str[k] = '*' then 
#                    k:= k+1;
#                fi; 
#            fi;
#            
#            Add( cfs, ParseSingNumberToGapNumber( coef ) );
######################################################################
            

        # note that if the monomial only consists of a coefficient
        # (i.e., constant monomial), then we will not enter the next
        # loop, and a [] will be added to mons, just as it should.
     
        while k <= len and not str[k] in ['-','+'] do
            
            # At this point we always have str[k] = 'x'.
            # We parse this piece of monomial and add it to mon.
            # Here str = x_!!, where !! is an index, so if we increase k
            # by 2 we jump to the index.
            
            k:=k+2;
            ind:= "";
            while k <= len and str[k] in CHARS_DIGITS  do 
                Add( ind, str[k] );
                k:=k+1;
            od;
            
            # Now we get the exponent:
            
            if k <= len and str[k] = '^' then
                exp:= "";
                k:= k+1;
                while k <= len and str[k] in CHARS_DIGITS do
                    Add( exp, str[k] );
                    k:= k+1;
                od;
                exp:= Int( exp );
            else
                exp:= 1;
            fi;
            
            Add( mon, IndeterminateNumbers( SingularBaseRing )[Int(ind)] );
            Add( mon, exp );

            if k <= len and str[k]='*' then k:= k+1; fi;
        od;
        
        Add( mons, mon );
    od;

    fam:= ElementsFamily( FamilyObj( SingularBaseRing ) );
    
    SortParallel( mons, cfs, fam!.zippedSum[1] );
    
    # merge mons and cfs...
    
    erep:= [ ];
    for k in [1..Length(mons)] do
        Add( erep, mons[k] );
        Add( erep, cfs[k] );
    od;

    return PolynomialByExtRep( fam, erep );

end;



ParseSingProcToGapFunction := function ( string )

    local length, k, parameters, done, pos, pos2, precommand, func;

    length := Length( string );
    if length = 0  then
        return ( function (  ) return; end );
    fi;

    # determine in <string> what are the parameters or arguments, and 
    # what is the body of the Singular function
    k := 1;
    parameters := " ";
    done := false;

    repeat
        while string[k] = ' '  do
            k := k + 1;
        od;

        if length > k + 11 and string{[ k .. k + 9 ]} = "parameter "  then
            pos := Position( string, ';' );
            Append( parameters, string{[ k + 10 .. pos - 1 ]} );
            Append( parameters, "," );
            string := string{[ pos + 1 .. length ]};
            length := Length( string );
            k := 1;
        else
            done := true;
        fi;

    until done;
    parameters{[ Length( parameters ) ]} := " ";

# remove Singular comments:
# // comment delimiter. Comment extends to end of line. 
# These should not harm
# /* comment delimiter. Starts a comment which ends with */. 
# */ comment delimiter. Ends a comment which starts with /*. 

    pos := PositionSublist( string, "//" );
    while pos <> fail  do
        pos2 := PositionSublist( string, "\n", pos );
        string := Concatenation( string{[ 1 .. pos - 1 ]}, " \n",
           string{[ pos2 + 1 .. Length( string ) ]} );
        pos := PositionSublist( string, "//" );
    od;

    string := NormalizedWhitespace( string );

    # the next two lines are necessary when the string is sent via a
    # stream
    string := ReplacedString( string, "\"", "\\\"" );
    string := ReplacedString( string, "\\", "\\\\" );
#    string := EscapeCharsInString( string );


    # the definition of the Singular function
    precommand := Concatenation( "proc GAP_proc (", parameters, ") {", 
                                 string, "};" );

 if parameters <> " " then
    # the '#' of Singular correspond to the <arg> of Gap (but this may 
    # give strange effect when there are both named and unnamed arguments)

    parameters := ReplacedString( parameters, "#", "arg" );

    # change the parameters like "def i, list arg" into "i, arg"
    parameters := SplitString( parameters, "," );
    parameters := List( parameters, x -> SplitString( x, " " ) );
    parameters := List( parameters, x ->Filtered( x, y -> not 
                                                     IsEmptyString(y)));
    parameters := List( parameters, x -> x[Length( x )] );
    parameters := JoinStringsWithSeparator( parameters, ", " );
 fi;

    # the definition of the Gap function
    func := Concatenation( 
       "function (", parameters, ") \n",
       "    SingularCommand( \"\", \"", precommand, "\" );\n",
       "    return SingularInterface( \"GAP_proc\", [", parameters,
       "] , \"def\" );\n",
       "end;\n" );

    return EvalString( func );

end;



# This function converts the string <obj> (that represent a Singular 
# object of type <type_output>) into a Gap object. It may be necessary to 
# ask Singular for more informations about this object: <singname> is the 
# name in Singular of this object.

ConvertSingObjToGapObj := function ( obj, type_output, singname )

    local command, ideal, idealno, name, list, nrows, ncols, r, length,
         type, string, i;

    # def
    if type_output = "def"  then
    # in this case ask Singular for the type
        command := Concatenation( "typeof( ", singname, " );" );
        type_output := SingCommandInStreamOutStream( "", command );
        Info( InfoSingular, 1, "Singular type \"", type_output, "\"" );        
    fi;

    # ideal
    if type_output = "ideal"  then 
        ideal := Ideal( SingularBaseRing, List( SplitString( obj, ',' ), 
                                      ParseSingPolyToGapPoly ) );

        if SingularCommand <> SingCommandUsingProcess  then

            # set the SingularIdentifier of the returned ideal
            idealno:= SingularNames.ideals+1;
            SingularNames.ideals:= idealno;
            name:= "GAP_ideal_"; Append( name, String( idealno ) );

            SetSingularIdentifier( ideal, name );

            command:= "ideal GAP_ideal_";
            Append( command, String( idealno ) );
            Append( command, " = " );
            Append( command, singname );
            SingCommandInStreamOutStream( command, "" );

        fi;

        return ideal;


    # int
    elif type_output = "int"  then
        return Int( obj );

    # intmat
    elif type_output = "intmat"  then
        list:= List( SplitString( obj, ',' ,' '), Int );
        command := Concatenation( "nrows( ", singname, " );" );
        nrows := Int( SingCommandInStreamOutStream( "", command ) );
        command := Concatenation( "ncols( ", singname, " );" );
        ncols := Int( SingCommandInStreamOutStream( "", command ) );
        return List( [ 1 .. nrows ], x ->
                     list{[ (x - 1) * ncols + 1 .. x * ncols ]} );

    # intvec
    elif type_output = "intvec"  then
        return List( SplitString( obj, ',' ), Int );

    # link
    elif type_output = "link"  then
        r := rec( object := "link" );
        command := Concatenation( "status( ", singname, ", \"name\" );" );
        r.name :=SingCommandInStreamOutStream( "", command );
        command := Concatenation( "status( ", singname, ", \"mode\" );" );
        r.mode :=SingCommandInStreamOutStream( "", command );
        command := Concatenation( "status( ", singname, ", \"type\" );" );
        r.type :=SingCommandInStreamOutStream( "", command );
        return r;

    # list
    elif type_output = "list"  then
        list := [  ];
        command := Concatenation( "size( ", singname, " );" );
        length := Int( SingCommandInStreamOutStream( "", command ) );
        for i  in [ 1 .. length ]  do
            name := Concatenation( singname, "[", String( i ), "]" );
            command := Concatenation( "typeof( ", name, " );" );
            type := SingCommandInStreamOutStream( "", command );
            command := Concatenation( "string( ", name, " );" );
            string := SingularCommand( "", command );
            Add( list, ConvertSingObjToGapObj( string, type, name ) );
        od;
        return list;


    # matrix
    elif type_output = "matrix"  then
        list:= List( SplitString( obj, ',', ' ' ), ParseSingPolyToGapPoly );
        command := Concatenation( "nrows( ", singname, " );" );
        nrows := Int( SingCommandInStreamOutStream( "", command ) );
        command := Concatenation( "ncols( ", singname, " );" );
        ncols := Int( SingCommandInStreamOutStream( "", command ) );
        return List( [ 1 .. nrows ], x ->
                     list{[ (x - 1) * ncols + 1 .. x * ncols ]} );


    # number
    elif type_output = "number"  then
        if # Characteristic( SingularBaseRing ) > 0 or 
           IsNonPrimeGroundField  then
            return ParseSingNumberToGapNumber( obj );
        else 
            return Rat( obj ) * One( CoefficientsRing( SingularBaseRing ) );
        fi;

    # poly
    elif type_output = "poly"  then
        return ParseSingPolyToGapPoly( obj );

    # proc
    elif type_output = "proc"  then
        return ParseSingProcToGapFunction( obj );
       
    # string
    elif type_output = "string"  then
        return obj;


    # ?unknown type?, none 
    elif type_output = "?unknown type?" or type_output = "none"  
         or type_output = ""  then
        if Length( obj ) > 0 then 
            Print( "Type \"", type_output, "\", returned as string\n" );
        else
            Print( "No output from Singular\n");
        fi;
        return obj;

    else
        Print( "Not yet implemented conversion from Singular type \"");
        Print( type_output, "\", returned as string\n" );
        Print( "(Your code to convert it will be welcome!)\n");        
        return obj;
    fi;

end;




##############################################################################
##############################################################################

## PART 6. The general high level interface to Singular ##




SingularInterface := function ( singcom, arguments, type_output )

    local sing_types, headers, precommand, length, singobj, out, i, 
          unsupported;

    # some Singular functions are unsupported:
    unsupported := [ "exit", "pause", "setring", "quit" ];
    # others may be added

    # trap them
    if singcom in unsupported then 
        Print( "Singular function ", singcom, 
               " is not supported by the interface\n" );

        if singcom in [ "exit", "quit" ]  then
            Print( "use CloseSingular instead\n");
        elif singcom = "setring" then 
            Print( "use SingularSetBaseRing instead\n");
        fi;

        return fail;
    fi;


    sing_types := [ "def", "ideal", "int", "intmat", "intvec", "link", 
        "list", "map", "matrix", "module", "number", "poly", "proc", "qring", 
        "resolution", "ring", "string", "vector", "" ];


    if not type_output in sing_types  then
        Print( "Type ", type_output, " not supported by Singular\n" );
        return fail;
    fi;

    # Singular may require that a SingularBaseRing is sent
    headers := "";

    # parsing singcom
    precommand := "";
    if type_output <> "" then 
        Append( precommand, type_output );
        Append( precommand, " GAP_" );
        Append( precommand, type_output );
        Append( precommand, " = " );
    fi;

    Append( precommand, singcom );
    Append( precommand, "( " );

    # parsing the arguments
    if IsString( arguments )  then
# are needed the following two lines? (or the other one?)
#        arguments := ReplacedString( arguments, "\\", "\\\\" );
#        arguments := ReplacedString( arguments, "\"", "\\\"" );
##        arguments := EscapeCharsInString( arguments );

        Append( precommand, arguments );

    else

        SingularArgument := 1;
        length := Length( arguments );
        for i  in [ 1 .. length ]  do

            singobj := ConvertGapObjToSingObj( arguments[i] );
            Append( headers, singobj[1] );
            Append( precommand, singobj[2] );

            if i < length  then
                Append( precommand, ", " );
            fi;

        od;

    fi;

    # end of the command for Singular
    Append( precommand, " );\n" );



    # send the commands to singular and get the output
    out := SingularCommand( Concatenation( headers, precommand, 
                   "string GAP_output = string ( GAP_", type_output, ");" ), 
               "GAP_output" );


    if SingularCommand = SingCommandUsingProcess and type_output in 
       [ "def", "intmat", "link", "list", "matrix", "proc" ]  then

        Print( "Sorry, type ", type_output, " is supported only with ",
               "Unix and Gap version >= 4.2,\nor Windows and Gap version ", 
               ">= 4.4. Output returned as a string.\n" );
        type_output := "string";

    fi;


    return ConvertSingObjToGapObj( out, type_output,
                                   Concatenation( "GAP_", type_output ) );


end;




GapInterface := function ( func, arg, out )

    local  i, length, sing_obj, gap_obj, gap_arg, type_output;

    length := Length( arg );
    gap_arg := [  ];

    for i  in [ 1 .. length ]  do
        sing_obj := SingularCommand(
           Concatenation( "def GAP_arg = ", arg[i], "; " ), 
           "string(GAP_arg)" );
        gap_obj := ConvertSingObjToGapObj( sing_obj, "def", "GAP_arg" );
        Add( gap_arg, gap_obj );
    od;

    gap_obj := CallFuncList( func, gap_arg );

    type_output := SingularType( gap_obj );
    if type_output = fail  then
        Error( "object ", gap_obj, "not supported\n" );
    fi;
    sing_obj := ConvertGapObjToSingObj( gap_obj );

    SingularCommand( Concatenation( sing_obj[1], "; ",
       type_output, " ", out, " = ", sing_obj[2], ";" ), "" );

end;




##############################################################################
##############################################################################

## PART 7. High level interface to some functions of Singular ##


# Groebner basis methods.....
# "GroebnerBasis" calculates a GB via the "groebner" command of Singular;

InstallOtherMethod( GroebnerBasis,
        "for an ideal in a poly ring", true,
        [ IsMagma ], 0, 
        
        function ( I )

    local input, out;

    
    Info( InfoSingular, 2, "running GroebnerBasis..." );


    # preparing the input for Singular
    input := "";
    if LeftActingRingOfIdeal( I ) <> SingularBaseRing then
        SingularSetBaseRing( LeftActingRingOfIdeal( I ) );
    fi;

    if not HasValidSingularIdentifier( I )  then
        Append( input, ParseGapIdealToSingIdeal( I ) );
    fi;
    Append( input, "ideal GAP_groebner = groebner( " );
    Append( input, SingularIdentifier( I ) );
    Append( input, " );\n" );


    out := SingularCommand( input, "string (GAP_groebner)" );


    Info( InfoSingular, 2, "done GroebnerBasis." );

    return List( SplitString( out, ',' ), ParseSingPolyToGapPoly );
      
end );
    


# something like the following could be used in Singular:
# LIB "general.lib";
# watchdog(1048576, "GAP_groebner==1");



HasTrivialGroebnerBasis:= function ( I )

    local input, out;

    Info( InfoSingular, 2, "running HasTrivialGroebnerBasis..." );


    # preparing the input for Singular
    input := "";
    if LeftActingRingOfIdeal( I ) <> SingularBaseRing then
        SingularSetBaseRing( LeftActingRingOfIdeal( I ) );
    fi;

    if not HasValidSingularIdentifier( I )  then
        Append( input, ParseGapIdealToSingIdeal( I ) );
    fi;
    Append( input, "ideal GAP_groebner = groebner( " );
    Append( input, SingularIdentifier( I ) );
#    to terminate in a reasonable time the following line can be used...
#    Append( input, ", 60);\n" );
    Append( input, " );\n" );


    out := SingularCommand( input, "GAP_groebner==1" );
    

    Info( InfoSingular, 2, "done HasTrivialGroebnerBasis." );

    if out = "0" then 
        return false;
    elif out = "1" then
        return true;
    else
        Error( " in the Singular interface, please report\n" );
    fi;

end;



# Function that displays the help of Singular

SingularHelp := function ( topic )
    local  browser, precommand, out;

    browser := SingularInterface( "system", [ "--browser" ], "string" );
    if browser in [ "info", "builtin", "lynx", "emacs" ]  then
        Error( "The browser ", browser,
         " is not supported by the interface." );
    elif browser = "dummy"  then 
        Print( "Singular says: ",
               "\"? No functioning help browser available.\"\n" );
    fi;

    out := SingularCommand( "", Concatenation( "help ", topic, ";" ) );
    Info( InfoSingular, 1, out );

end;




# Function that loads a Singular library

SingularLibrary := function ( lib )
    if Length( lib ) > 0 and PositionSublist( lib, ".lib" ) = fail  then
        Append( lib, ".lib" );
    fi;
    lib := Concatenation( "LIB \"", lib, "\";" );
    SingCommandInStreamOutStream( lib, "" );
    # maybe if ... then
    Append( SingularLoadedLibraries, lib );
end;



SingularSetBaseRing := function ( R )
    SingularBaseRing := R;
    SingCommandInStreamOutStream( ParseGapRingToSingRing( R ), "" );
    SingularNamesThisRing := ShallowCopy( SingularNames );
end;



# to be improved
GcdUsingSingular := function ( arg )

    local  i;

    if Length( arg ) = 1 and IsList( arg[1] )  then
        arg := arg[1];
    fi;

    SingularCommand( Concatenation( "poly GAP_gcd = ",
       ParseGapPolyToSingPoly( arg[1] ) ), "" );

    for i  in [ 2 .. Length( arg ) ]  do

        # calculate gcd( gcd( arg[1]..arg[i-1] ), arg[i] ) ...
        if SingularCommand(
         Concatenation( "poly GAP_gcd_ = gcd( GAP_gcd, ",
           ParseGapPolyToSingPoly( arg[i] ), " );\n",
           "poly GAP_gcd = GAP_gcd_;" ),
         
        # ... and ask soon whether it is trivial
            "GAP_gcd == 1" ) = "1"  then
            return One( SingularBaseRing );
        fi;

    od;

    return ParseSingPolyToGapPoly( SingularCommand( "", 
                                       "string( GAP_gcd )" ) );
end;



# to be improved, and to be done at lower level
FactorsUsingSingularNC := function ( poly )

    local list, g, ind, res, i;

    list := SingularInterface( "factorize", [ poly ], "list" );

    g := GeneratorsOfTwoSidedIdeal( list[1] );
    ind := list[2];

    res := [  ];
    for i  in [ 1 .. Length( ind ) ]  do
        Append( res, List( [ 1 .. ind[i] ], x -> g[i] ) );
    od;

    return res;

end;




FactorsUsingSingular := function ( poly )

    local res;

    if IsNonPrimeGroundField and SingularVersion < 2004  then
        Info( InfoSingular, 1, "Your version of Singular has a bug and ",
         "the result may be wrong." );
        Info( InfoSingular, 1, "Singular version at least 2-0-4 is ",
         "recommended." );
    fi;

    res := FactorsUsingSingularNC( poly );

    if Product( res ) <> poly then 
       Print ( "Bug (probably in Singular)!  The result, ", res,
               ", is wrong\n" );
       return fail;
    fi;
 
    return res;

end;



GeneratorsOfInvariantRing:= function( R, G )

    local   g,  n,  F;

    if IsMatrixGroup(G) then
        g:= GeneratorsOfGroup( G );
        if Length(g[1]) > Length( IndeterminatesOfPolynomialRing(R) ) then
            Error("<G> does not act on <R>");
        fi;
    elif IsPermGroup(G) then
        n:= Maximum(MovedPoints(G));
        F:= LeftActingDomain( R );
        g:= List( GeneratorsOfGroup( G ), x ->
                  TransposedMat(PermutationMat(x,n,F))  );
        if Maximum(MovedPoints(G)) >
           Length( IndeterminatesOfPolynomialRing(R) ) then
            Error("<G> does not act on <R>");
        fi;
    else
        Error("<G> must be a matrix or permutation group");
    fi;

    SingularLibrary( "finvar.lib" );

    if R <> SingularBaseRing then
        SingularSetBaseRing( R );
    fi;

    g:= g*One(R);
    return SingularInterface( "invariant_ring", g, "list" )[1][1];
end;





##############################################################################
##############################################################################

## PART 8. Some final technical stuff ##




    
    
SINGULARGBASIS := rec(
  name := "singular interface for GroebnerBasis",
  GroebnerBasis := function ( pols, O )

        local  ipr, mcf, R, I;


        if IsPolynomialRingIdeal( pols )  then
            R := LeftActingRingOfIdeal( pols );
            pols := GeneratorsOfTwoSidedIdeal( pols );
        else
            R := DefaultRing( pols );
        fi;

        if IsMonomialOrdering( O )  then
            ipr := ShallowCopy( IndeterminatesOfPolynomialRing( R ) );
            mcf := MonomialComparisonFunction( O );
            Sort( ipr, mcf );
            ipr := Reversed( ipr );
            R := PolynomialRing( LeftActingDomain( R ), ipr );
        fi;

        SetTermOrdering( R, O );

        I := Ideal( R, pols );
        return GroebnerBasis( I );

    end );



# Make the method provided by this package the default method for 
# calculating the Groebner Bases.

# GBASIS:= SINGULARGBASIS;



# This functions collects all the informations that are useful for a 
# report about the Singular Interface

SingularReportInformations := function (  )

    local  string, s, uname, _in, _out;

    string := "";

    s := Concatenation( "Gap_Version := \"", VERSION, "\";\n" );
    Print( s );
    Append( string, s );

    s := Concatenation( "Gap_Architecture := \"", GAP_ARCHITECTURE, 
         "\";\n" );
    Print( s );
    Append( string, s );

  if IsBound(GAPInfo) then

    s := Concatenation( "Gap_BytesPerVariable := ",
       String( GAPInfo.BytesPerVariable ), ";\n" );
    Print( s );
    Append( string, s );

  else

  fi;

    if ARCH_IS_UNIX(  )  then
        s := "";
        _in := InputTextNone(  );
        _out := OutputTextString( s, true );
        uname := Filename( DirectoriesSystemPrograms(  ), "uname" );
        # "var" instead of "uname" under Windows, to be implemented

        Process( DirectoryCurrent(  ), uname, _in, _out, [ "-mrs" ] );

        CloseStream( _in );
        CloseStream( _out );

        s := Concatenation( "uname := \"", NormalizedWhitespace( s ), 
           "\";\n" );
        Print( s );
        Append( string, s );
    fi;

    s := Concatenation( "Singular_Version: := ",
       SingularInterface( "string", "system(\"version\")", "string" ), 
       ";\n" ) ;
    Print( s );
    Append( string, s );

    s := Concatenation( "Singular_Name: := \"",
       String( SingularInterface( "system", [ "Singular" ], "string" ) ),
       "\";\n" );
    Print( s );
    Append( string, s );

    Print( "\n" );

    return string;
end;



# Ask Singular to get SingularVersion.

if SingularCommand = SingCommandUsingProcess  then
    SingularVersion := Int( SingularCommand( "", "system(\"version\");" ) );
fi;
