
sub Desymbol
{
    local($t, $e) = @_;

    $e = substr($t, -1);
    if ($e eq '*') {
	substr($t, -1) = '';
	return &Desymbol($t) . "P";
    }
    if ($e eq '&') {
	substr($t, -1) = '';
	return &Desymbol($t) . "A";
    }
    if ($e eq '?') {
	substr($t, -1) = '';
	return &Desymbol($t) . "Q";
    }
    if ($e eq '!') {
	substr($t, -1) = '';
	return &Desymbol($t) . "PT";
    }
    if ($e eq '^') {
	substr($t, -1) = '';
	return &Desymbol($t) . "NP";
    }
    if ($e eq '%') {
	substr($t, -1) = '';
	return &Desymbol($t) . "AD";
    }
    if ($e eq ']') {
	$badflag = 1;
	return "XXX";
    }

    return $t;
}

sub MakeCommonName
{
    $commonname = "wxsC_" . &Desymbol($returntype);

    foreach $p (@paramtypes) {
	$commonname = $commonname . "_" . &Desymbol($p);
    }

    return $commonname;
}

1;
