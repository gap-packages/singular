#############################################################################
##
##  PackageInfo.g file for the package Singular
##  Marco Costantini and Willem de Graaf
##

SetPackageInfo( rec(
PackageName := "singular",
Subtitle := "The GAP interface to Singular",

# on a new release, change the version/date twice here, in init.g and in 
# doc/singular.xml

Version := "4.04.15",
Date := "15/04/2004",
ArchiveURL := Concatenation([
 "http://www-math.science.unitn.it/~costanti/gap_code/singular/singular-", 
  ~.Version]),
ArchiveFormats := ".tar.gz",

Persons := [
  rec(
  LastName := "Costantini",
  FirstNames := "Marco",
  IsAuthor := true,
  IsMaintainer := true,
  Email := "costanti@science.unitn.it",
  WWWHome := "http://www-math.science.unitn.it/~costanti/",
  PostalAddress := Concatenation( [
                     "Marco Costantini\n",
                     "Dipartimento di Matematica\n",
                     "Università degli Studi di Trento\n",
                     "I-38050 Povo (Trento)\n",
                     "Italy" ] ),
  Place := "Trento",
  Institution := "Department of Mathematics, University of Trento"
  ),

  rec(
  LastName := "de Graaf",
  FirstNames := "Willem",
  IsAuthor := true,
  IsMaintainer := true,
  Email := "quagroup@hetnet.nl",
  WWWHome := "http://www-circa.mcs.st-and.ac.uk/~wdg/",
  Place := "Utrecht",
  Institution := "Mathematisch Instituut Universiteit Utrecht"
  )
],

Status := "deposited",
#CommunicatedBy := "",
#AcceptDate := "",

README_URL := "http://www-math.science.unitn.it/~costanti/gap_code/singular/README",
PackageInfoURL :=
 "http://www-math.science.unitn.it/~costanti/gap_code/singular/PackageInfo.g",

AbstractHTML :=
  "The <span class=\"pkgname\">singular</span> package provides an interface \
   from <span class=\"pkgname\">GAP</span> to the computer algebra system \
   <span class=\"pkgname\">Singular</span>.",

PackageWWWHome := "http://www-math.science.unitn.it/~costanti/#singular",

PackageDoc := rec(
  BookName  := "singular",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "The GAP interface to Singular",
  Autoload  := true
),

Dependencies := rec(
  GAP := "4", # recommended >=4.2 under unix or >=4.4 under windows
  NeededOtherPackages := [  ],
  SuggestedOtherPackages := [  ],
  ExternalConditions := [ ["Requires the computer algebra system Singular",
                           "http://www.singular.uni-kl.de/"] ]
),
AvailabilityTest := ReturnTrue,
Autoload := false,
# the banner
BannerString := "The GAP interface to Singular\n",

#TestFile := "tst/testall.g",
Keywords := ["Groebner bases"]

));

