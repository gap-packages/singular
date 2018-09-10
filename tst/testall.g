LoadPackage("singular");
TestDirectory(DirectoriesPackageLibrary("singular", "tst"), rec(exitGAP := true));
FORCE_QUIT_GAP(1);
