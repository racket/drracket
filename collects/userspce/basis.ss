(compound-unit/sig
  (import [import : plt:basis-import^]
	  [params : plt:userspace:params^]
	  [zodiac : zodiac:system^]
	  [zodiac:interface : drscheme:interface^]
	  [aries : plt:aries^]
	  [mzlib:print-convert : mzlib:print-convert^]
	  [mzlib:pretty-print : mzlib:pretty-print^]
	  [mzlib:function : mzlib:function^])
  (link
   [init-params : plt:init-params^ ((require-relative-library "init-paramr.ss")
				    import
				    init-namespace
				    params
				    zodiac
				    zodiac:interface
				    aries
				    mzlib:print-convert
				    mzlib:pretty-print
				    mzlib:function)]
   [init-namespace : plt:init-namespace^ ((require-relative-library "init-namespacer.ss")
					  import
					  init-params
					  mzlib:function)])
  (export
   (open init-params)
   (open init-namespace)))

