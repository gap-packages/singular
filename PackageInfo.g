#############################################################################
##
#W    PackageInfo.g        Package singular            Willem de Graaf
#W                                                     Marco Costantini
##
#Y    Copyright (C) 2003 Willem de Graaf and Marco Costantini
#Y    Copyright (C) 2004, 2005, 2006 Marco Costantini
##


SetPackageInfo( rec(
PackageName := "singular",
Subtitle := "A GAP interface to Singular",
Version := "2024.06.03",
Date := Concatenation( ~.Version{[ 9, 10 ]}, "/", ~.Version{[ 6, 7 ]}, "/", ~.Version{[ 1 .. 4 ]} ),
License := "GPL-2.0-or-later",

Persons := [
  rec(
  LastName := "Costantini",
  FirstNames := "Marco",
  IsAuthor := true,
  IsMaintainer := false,
  #Email := "costanti@science.unitn.it",
  #WWWHome := "http://www-math.science.unitn.it/~costanti/",
  #Place := "Trento",
  #Institution := "Department of Mathematics, University of Trento"
  ),

  rec(
  LastName := "de Graaf",
  FirstNames := "Willem Adriaan",
  IsAuthor := true,
  IsMaintainer := false,
  Email := "degraaf@science.unitn.it",
  WWWHome := "https://www.science.unitn.it/~degraaf/",
  PostalAddress := Concatenation( [
                     "Willem de Graaf\n",
                     "Dipartimento di Matematica\n",
                     "Università degli Studi di Trento\n",
                     "I-38050 Povo (Trento)\n",
                     "Italy" ] ),
  Place := "Trento",
  Institution := "Department of Mathematics, University of Trento"
  ),

  rec(
    LastName      := "GAP Team",
    FirstNames    := "The",
    IsAuthor      := false,
    IsMaintainer  := true,
    Email         := "support@gap-system.org",
  ),
],

Status := "deposited",
#CommunicatedBy := "",
#AcceptDate := "",

PackageWWWHome  := "https://gap-packages.github.io/singular/",
README_URL      := Concatenation( ~.PackageWWWHome, "README.md" ),
PackageInfoURL  := Concatenation( ~.PackageWWWHome, "PackageInfo.g" ),
SourceRepository := rec(
    Type := "git",
    URL := "https://github.com/gap-packages/singular",
),
IssueTrackerURL := Concatenation( ~.SourceRepository.URL, "/issues" ),
ArchiveURL      := Concatenation( ~.SourceRepository.URL,
                                 "/releases/download/v", ~.Version,
                                 "/singular-", ~.Version ),
ArchiveFormats := ".tar.gz",

AbstractHTML :=
  "The <span class=\"pkgname\">singular</span> package provides an interface \
   from <span class=\"pkgname\">GAP</span> to the computer algebra system \
   <span class=\"pkgname\">Singular</span>.",

PackageDoc := rec(
  BookName  := "singular",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0_mj.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "A GAP interface to Singular",
  Autoload  := true
),

Dependencies := rec(
  GAP := ">=4.8",
  NeededOtherPackages := [  ],
  SuggestedOtherPackages := [  ],
  ExternalConditions := [ ["Requires the computer algebra system Singular",
                           "https://www.singular.uni-kl.de/"] ]
),
AvailabilityTest := ReturnTrue,
Autoload := false,

TestFile := "tst/testall.g",
Keywords := [ "Interface to Singular", "Groebner bases" ]

));
