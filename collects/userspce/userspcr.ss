(compound-unit/sig (import [params : plt:userspace:params^])
  (link [core : mzlib:core-flat^ ((require-library "coreflatr.ss"))]
	[mred : mred^ (mred@)])
  (export (open core)
	  (open mred)))
