alias %s = SieveRTB1_%s;
runSingleThreaded!%s(IsFaithful.yes, "base", %s);
runMultiThreaded!(%s, dt)(IsFaithful.yes, "base", %s);
runMultiThreaded!(%s, st)(IsFaithful.yes, "base", %s);