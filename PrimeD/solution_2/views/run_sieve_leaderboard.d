alias %s = SieveRT_%s;
runSingleThreaded!%s(IsFaithful.yes, "base", %s);
runMultiThreaded!(%s, st)(IsFaithful.yes, "base", %s);