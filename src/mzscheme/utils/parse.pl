
# Copyright (c) 1995 Matthew Flatt
# Parsing for xctocc and xctotex

$key_include = '@INCLUDE ';
$key_boolean = '@BOOLEAN ';
$key_classbase = '@CLASSBASE ';
$key_interface = '@INTERFACE ';
$key_classid = '@CLASSID ';
$key_global = '@GLOBAL ';
$key_common = '@COMMON ';
$key_header = '@HEADER ';
$key_end = '@END ';
$key_stop = '@STOP ';
$key_creator = '@CREATOR ';
$key_creatorx = '@CREATORX ';
$key_macro = '@MACRO ';
$key_var = '@VAR ';
$key_set = '@SET ';
$key_define = '@DEFINE ';
$key_ifdefine = '@IFDEFINE ';
$key_ivar = '@IVAR ';
$key_constant = '@CONSTANT ';
$key_suffix = '@CLASSSUFFIX ';
$key_test = '@TEST ';
$key_setmark = '@SETMARK ';
$key_idfield = '@IDFIELD ';
$key_startsymbols = '@BEGINSYMBOLS ';
$key_endsymbols = '@ENDSYMBOLS ';
$key_sym = '@SYM ';

sub ResetObjParams 
{
    @functions = ();
    @funcnames = ();
    @vars = ();
    @ivars = ();
    @creators = ();
    @distinct_creators = ();
    @constants = ();
    %justoneok = ();
    %justonemin = ();
    %justonemax = ();
    $globalname = '';
    $classid = '';
    $global = 0;
    $common = 0;
    $implementor = "";
    $interfacestring = "";
}

&ResetObjParams();

sub ReadFile {
    @openfiles = ();
    $stop = 0;
    $linenum = 0;
    $testfile = 0;
    $classsuffix = '';
    $idfield = '';
    $bool = 'boolean';
    $cursymset = '';
    %macros = ();
    %sets = ();
    %marks = ();
    @syms = ();
    $symsetkind = "";
    $symsetomit = "";
    $marks{'V'} = 'V';
    $marks{'H'} = 'H';
    $marks{'v'} = 'v';
    $marks{'m'} = 'm';
    $ifzero = 0;
    while(!$stop)
    {
	$_ = <IN>;
	$linenum += 1;
	if ($_ eq undef) {
	    if ($#openfiles >= 0) {
		close(IN);
		$oldin = pop(@openfiles);
		$p = index($oldin, ":");
		$linenum = substr($oldin, 0, $p);
		$oldin = substr($oldin, $p + 1); 
		$p = index($oldin, ":");
		$pos = substr($oldin, 0, $p);
		$thisfile = substr($oldin, $p + 1); 
		open(IN, "$thisfile");
		if ($pos > 0) {
		    seek(IN, $pos, 0);
		} else {
		    seek(IN, 0, 2);
		}
	    } else {
		$stop = 1;
	    }
	} elsif ($ifzero) {
           if (/^\#endif/) {
	       $ifzero -= 1;
	   } elsif (/^\#if/) {
	       $ifzero += 1;
	   }
        } elsif (/^\@/) {
	    chop;
	    $_ = $_ . " ";
	    if (&StartsWithKey($_, $key_include)) {
		$_ = &SkipKey($_, $key_include);
		$incfile = &Wash($_);
		$oldin = $linenum . ":" . tell(IN) . ":" . $thisfile;
		if (!open(IN2, $incfile)) {
		    print STDERR "Couldn't open \"${incfile}\"\n";
		} else {
		    $thisfile = $incfile;
		    $linenum = 0;
		    push(@openfiles, $oldin);
		    close(IN);
		    open(IN, "<&IN2");
		}
	    } elsif (&StartsWithKey($_, $key_boolean)) {
		$bool = &Wash(&SkipKey($_, $key_boolean));
	    } elsif (&StartsWithKey($_, $key_test)) {
		$testfile = 1;
	    } elsif (&StartsWithKey($_, $key_classbase)) {
		&ResetObjParams();
		$_ = &Wash(&SkipKey($_, $key_classbase));
		$pos = index($_, ' ');
		$base = substr($_, 0, $pos);
		$classstring = substr($_, $pos);
		$base = &Wash($base);
		($classstring,$parentstring) = &SplitColon($classstring);
		$classstring = '"' . &Unquote(&Wash($classstring))
		    . &Unquote($classsuffix) . '"';
		$parentstring = &Wash($parentstring);
		if ($parentstring ne '') {
		    $parentstring = '"' . &Unquote($parentstring)
			. &Unquote($classsuffix) . '"';
		}
		$oldclass = $base;
		$newclass = 'os_' . $base;
	    } elsif (&StartsWithKey($_, $key_interface)) {
		$_ = &Wash(&SkipKey($_, $key_classbase));
		$interfacestring =$_;
	    } elsif (&StartsWithKey($_, $key_global)) {
		&ResetObjParams();
		$global = 1;
		$globalname = &Wash(&SkipKey($_, $key_global));
	    } elsif (&StartsWithKey($_, $key_common)) {
		&ResetObjParams();
		$common = 1;
	    } elsif (&StartsWithKey($_, $key_idfield)) {
		$idfield = &Wash(&SkipKey($_, $key_idfield));
	    } elsif (&StartsWithKey($_, $key_classid)) {
		$classid = &Wash(&SkipKey($_, $key_classid));
	    } elsif (&StartsWithKey($_, $key_header)) {
		&PrintHeader();
	    } elsif (&StartsWithKey($_, $key_end)) {
		&DoPrintClass();
	    } elsif (&StartsWithKey($_, $key_stop)) {
		$stop = 1;
	    } elsif (&StartsWithKey($_, $key_creator)) {
		$creator = &SkipKey($_, $key_creator);
		@creators = (@creators, $creator);
		@distinct_creators = (@distinct_creators, $creator);
	    } elsif (&StartsWithKey($_, $key_creatorx)) {
		$creator = &SkipKey($_, $key_creatorx);
		@creators = (@creators, $creator);
	    } elsif (&StartsWithKey($_, $key_macro)) {
		$s = &Wash(&SkipKey($_, $key_macro));
		$eqpos = index($s, '=');
		$parenpos = index($s, '[');
		if ($parenpos >= $[ && $parenpos < $eqpos) {
		    $macro = substr($s, 0, $parenpos);
		} else {
		    $macro = substr($s, 0, $eqpos);
		}
		$macro = &Wash($macro);
		$macros{$macro} = $s;
	    } elsif (&StartsWithKey($_, $key_var)) {
		@vars = (@vars, &Wash(substr($_, 4)));
	    } elsif (&StartsWithKey($_, $key_set)) {
		($var, $val) = split(/=/, &SkipKey($_, $key_set));
		$var = &Wash($var);
		$val = &Wash($val);
		$sets{$var} = $val;
	    } elsif (&StartsWithKey($_, $key_define)) {
		($var, $val) = split(/=/, &SkipKey($_, $key_define));
		$var = &Wash($var);
		$val = &Wash($val);
		&DefineVar($var, $val);
	    } elsif (&StartsWithKey($_, $key_ifdefine)) {
		($var, $val) = split(/=/, &SkipKey($_, $key_ifdefine), 2);
		$var = &Wash($var);
		($test, $val1, $val2) = &SplitColon($val);
		if ($sets{&Wash($test)} > 0) {
		    $val = $val1;
		} else {
		    $val = $val2;
		}
		$val = &Wash($val);
		&DefineVar($var, $val);
	    } elsif (&StartsWithKey($_, $key_ivar)) {
		@ivars = (@ivars, &Wash(&SkipKey($_, $key_ivar)));
	    } elsif (&StartsWithKey($_, $key_constant)) {
		@constants = (@constants, &Wash(&SkipKey($_, $key_constant)));
	    } elsif (&StartsWithKey($_, $key_suffix)) {
		$classsuffix = &Wash(&SkipKey($_, $key_suffix));
	    } elsif (&StartsWithKey($_, $key_setmark)) {
		($mark, $val) = split(/=/, &SkipKey($_, $key_setmark), 2);
		$mark = &Wash($mark);
		$marks{$mark} = &Wash($val);
	    } elsif (&StartsWithKey($_, $key_startsymbols)) {
		($name, $kind, $omit) = split(/>/, &SkipKey($_, $key_startsymbols), 3);
		$name = &Wash($name);
		@syms = ();
		$cursymset = $name;
		$symsetkind = $kind;
		$symsetomit = $omit;
	    } elsif (&StartsWithKey($_, $key_sym)) {
		($name, $val) = split(/:/, &SkipKey($_, $key_sym), 2);
		$name = &Wash($name);
		$val = &Wash($val);
		@syms = (@syms, "$name,$val");
	    } elsif (&StartsWithKey($_, $key_endsymbols)) {
		&PrintSymSet($cursymset, $symsetkind, $symsetomit, @syms);
	    } elsif (substr($_, 1, 1) ne ' ') {
		print STDERR 
		    "syntax error at line $linenum of \"$thisfile\".\n"
		} else {
		    $function = &Wash(substr($_, 1));
		    $mark = substr($function, 0, 1);
		    if ($mark ne '"') {
			$mark = $marks{$mark};
			substr($function, 0, 1) = $mark;
		    }
		    @functions = ($function, @functions);
		    &ReadFields($function);
		    @funcnames = ($func, @funcnames);
		}
	} elsif (/^\#if 0/) {
	    $ifzero = 1;
	} else {
	    &IgnoreLine($_);
	}
    }

    # Finish up the file
    while(<IN>) { &IgnoreLine($_); }
}

sub StartsWithKey
{
    return (index($_[0], $_[1]) == $[);
}

sub SkipKey
{
    return substr($_[0], length($_[1]));
}

sub Wash 
{
    $_[0] =~ /^ *(.*[^ ]) *$/;
    return $1;
}

sub SplitColon
{
    local($s) = @_;
    local($balance, @ans, $a, $c);
    
    $balance = 0;
    $a = '';
    @ans = ();
    while ($s ne '') {
	$c = substr($s, 0, 1);
	if (!$balance && $c eq ':') {
	    @ans = (@ans, $a);
	    $a = '';
	} else {
	    $a = $a . $c;
	}

	if ($c eq '"') {
	    $balance = !$balance;
	} 
	$s = substr($s, 1);
    }
    @ans = (@ans, $a) if ($a ne '');

    return @ans;
}

sub ReadFields {

    ($s) = @_;

    $virtual = $virtualonly = $hidden = $delegate = $externalmethod = 0;

    $mark = substr($s, 0, 1);
    if ($mark ne '"') {
	substr($s, 0, 1) = '';
    }

    if ($mark eq 'V') {
	$virtual = $virtualonly = 1;
    } elsif ($mark eq 'v') {
	$virtual = 1;
    } elsif ($mark eq 'H') {
	$virtual = $virtualonly = $hidden = 1;
    } elsif ($mark eq 'd') {
	$virtual = 1;
    } elsif ($mark eq 'm') {
	$externalmethod = 1;
    }

    ($s, $casename) = split(/<>/, $s);
    if ($casename eq "") {
	($s, $onlyif) = split(/##/, $s);
    } else {
	($casename, $onlyif) = split(/##/, $casename);
    }

    $casename = &Wash($casename);

    ($fname, $s, $methodpostmacros, $gluepostmacros, $exception, $vexception, $implementor) 
	= &SplitColon($s);
    
    $s = &Wash($s);

    $p = index($s, ' ');
    $returntype = substr($s, 0, $p);
    $s = substr($s, $p + 1);

    $p = index($s, '(');
    $func = substr($s, 0, $p);
    $s = substr($s, $p + 1);

    $p = rindex($s, ')');
    $s = substr($s, 0, $p);

    @defvals = ();
    @bundles = ();
    @unbundles = ();
    @typechecks = ();
    @typeids = ();
    @schemes = ();
    @schemeparams = ();
    @spideytypes = ();
    @pushables = ();
    @paramtypes = split(/,/, $s);
    $numschemes = 0;
    foreach $paramtype (@paramtypes) {
	($paramtype, $bundle, $unbundle, $typecheck, $typeid, $spideytype, $pushable) 
	    = split('/', $paramtype);

	($paramtype, $defval) = split(/=/, $paramtype);
	$paramtype = &Wash($paramtype);
	if (substr($paramtype, 0, 1) eq '-') {
	    $paramtype = substr($paramtype, 1);
	    $scheme = 0;
	} else {
	    $scheme = 1;
	}
	$defval = &Wash($defval);
	$bundle = &Wash($bundle);
	$unbundle = &Wash($unbundle);
	$typecheck = &Wash($typecheck);
	$typeid = &Wash($typeid);
	$spideytype = &Wash($spideytype);
	$pushable = &Wash($pushable);

	@defvals = (@defvals, $defval); 
	@bundles = (@bundles, $bundle);
	@unbundles = (@unbundles, $unbundle);
	@typechecks = (@typechecks, $typecheck);
	@typeids = (@typeids, $typeid);
	@spideytypes = (@spideytypes, $spideytype);
	@pushables = (@pushables, $pushable);
	@schemes = (@schemes, $scheme);
	if ($scheme) {
	    @schemeparams = (@schemeparams, $paramtype);
	    $numschemes += 1;
	}
    }

    ($returntype, $returnbundle, $returnunbundle, $returnspideytype) = split('/', $returntype);

    $func = &Wash($func);
    $fname = &Wash($fname);
    $returntype = &Wash($returntype);
    $returnbundle = &Wash($returnbundle);
    $returnunbundle = &Wash($returnunbundle);

    ($methpre, $methprecall, $methpostcall, $methpost) 
	= split('/', $methodpostmacros);
    ($gluepre, $glueprecall, $gluepostcall, $gluepost) 
	= split('/', $gluepostmacros);

    $methpre = &Wash($methpre);
    $methprecall = &Wash($methprecall);
    $methpost = &Wash($methpost);
    $methpostcall = &Wash($methpostcall);

    $gluepre = &Wash($gluepre);
    $glueprecall = &Wash($glueprecall);
    $gluepost = &Wash($gluepost);
    $gluepostcall = &Wash($gluepostcall);

    if ($virtualonly && ($vexception ne '')) {
	$exception = &Wash($vexception);
    } else {
	$exception = &Wash($exception);
    }
    if ($exception eq '') {
	$exception = 'SUPER';
    }

    if ($classstring eq '') {
	$method = $fname;
    } else {
	if ($interfacestring eq '') {
	    $sourcestring = $classstring;
	} else {
	    $sourcestring = substr($interfacestring, 0, length($interfacestring) - 1) . '<%>"';
	}
	$method = '"' . substr($fname, 1, length($fname) - 2) . " in "
	    . substr($sourcestring, 1, length($sourcestring) - 2) . '"';
    }
}

sub ReadIvarFields
{
    local($s) = @_;

    $readonly = 0;

    ($s, $onlyif) = split(/##/, $s);

    if (substr($s, 0, 1) eq 'r') {
	$readonly = 1;
    }

    substr($s, 0, 1) = '' if $readonly;
    
    ($iname, $itype) = &SplitColon($s);
    ($itype, $ivarname) = split(/ /, &Wash($itype));

    $iname = &Wash($iname);
    $getname = substr($iname, 1);
    substr($getname, -1) = '';
    $setname = "\"set-" . $getname . "\"";
    $getname = "\"get-" . $getname . "\"";

    $ivartype = &Wash($itype);
    $ivarname = &Wash($ivarname);

    $method = 'et-' . substr($iname, 1, length($iname) - 2) 
	    . " in " . substr($classstring, 1, length($classstring) - 2) . '"';

    $longsetname = '"s' . $method;
    $longgetname = '"g' . $method;
    $method = $longsetname;
}

sub ReadConstFields
{
    local($s) = @_;

    ($s, $onlyif) = split(/##/, $s);

    ($const, $ctype) = &SplitColon($s);
    ($ctype, $cname) = split(/ /, &Wash($ctype));

    $const = &Wash($const);
    $ctype = &Wash($ctype);
    $cname = &Wash($cname);
}

sub DefineReplace
{
    local($val) = @_;

    while ($val =~ /<([^>]*)>/ ) {
	$subval = $sets{$1};
	$val =~ s/<[^>]*>/$subval/;
    }

    return $val;
}

sub DefineVar
{
    local($var, $val) = @_;

    $val = &DefineReplace($val);
    &PrintDefine("#define $var $val\n");
}

sub Unquote
{
    return substr($_[0], 1, length($_[0]) - 2);
}

sub Sprintfify
{
    $_[0] =~ s/%/%%/g;
}

sub ApplyMacro
{
    local($name, $var, $var2) = @_;

    if (($name eq '') || ($name eq undef)) {
	return "";
    }

    local($pos, @args, @argnames, $arg);
    $pos = index($name, '[');
    if ($pos >= $[) {
	$arg = substr($name, $pos + 1);
	$name = substr($name, 0, $pos);
	$pos = index($arg, ']');
	$arg = substr($arg, 0, $pos);
	@args = split(/\./, $arg);
    } else {
	@args = ();
    }

    $macro = $macros{$name};

    if ($macro eq undef) {
	print STDERR "Unknown macro $name in $func.\n";
	return "";
    }

    $pos = index($macro, '=');

    $m = substr($macro, $pos + 1);
    $macro = substr($macro, 0, $pos);
    $pos = index($macro, '[');
    if ($pos >= $[) {
	$arg = substr($macro, $pos + 1);
	$pos = index($arg, ']');
	$arg = substr($arg, 0, $pos);
	@argnames = split(/\./, $arg);
    } else {
	@argnames = ();
    }
    
    $m =~ s/{x}/$var/g;
    $m =~ s/{s}/$var2/g;

    if ($#argnames != $#args) {
	print STDERR "Bad parameter ("
	    . ($#args + 1)
	    . " for "
	    . ($#argnames + 1)
	    . ") count to macro $name in $func.\n";
	return "";
    }

    foreach $name (@argnames) {
	$arg = shift(@args);
	$name = &Wash($name);
	$arg = &Wash($arg);
	$m =~ s/<${name}>/$arg/g;
    }

    $m = &Wash($m);

    while (substr($m, 0, 2) eq '$$') {
	if (substr($m, 2, 1) eq '>') {
	    $m = &DefineReplace(substr($m, 3));
	} else {
	    $m = &ApplyMacro(substr($m, 2), "", "");
	}
    }

    return $m;
}

sub ApplyMacros
{
    local($macrolist, $var, $var2) = @_;

    @macros = split(/\|/, $macrolist);
    $str = '';
    foreach $macro (@macros) {
	$str = $str . &ApplyMacro(&Wash($macro), $var, $var2);
    }

    return $str;
}
    
1;
