SCM gh_eval_str(char* s )
{
  return scheme_eval_string(s, scheme_env);
}

SCM gh_eval_file(char* file )
{
  return scheme_load(file);
}

void gh_new_procedure(char* name , SCM_FN fn , int required , int optional , int restp )
{
  scheme_add_global(name, gh_make_subr(fn, required, optional, restp, NULL), scheme_env);
}

SCM gh_bool(int x )
{
  return x ? scheme_true : scheme_false;
}

SCM gh_long2scm(long x )
{
  return scheme_make_integer_value(x);
}

SCM gh_ulong2scm(unsigned long x )
{
  return scheme_make_integer_value_from_unsigned(x);
}

SCM gh_double2scm(double x )
{
  return scheme_make_double(x);
}

SCM gh_char2scm(char x )
{
  return scheme_make_char(x);
}

SCM gh_str2scm(char* x , int len )
{
  return scheme_make_sized_string(x, len, 1);
}

SCM gh_str02scm(char* x )
{
  return scheme_make_string(x);
}

void gh_set_substr(char* x , SCM dst , int start , int len )
{
  if (start + len >= SCHEME_STRTAG_VAL(dst)) { scheme_signal_error("gh_set_substr: out-of-range"); } {memcpy(SCHEME_STR_VAL(dst) + start, x, len); };
}

SCM gh_symbol2scm(char* x )
{
  return scheme_intern_symbol(x);
}

int gh_scm2bool(SCM x )
{
  return SCHEME_TRUEP(x);
}

int gh_scm2char(SCM x )
{
  return SCHEME_CHAR_VAL(x);
}

char* gh_scm2newstr(SCM x , int* len )
{
  return (*len = SCHEME_STRTAG_VAL(x), strdup_sized(SCHEME_STR_VAL(x), *len));
}

void gh_get_substr(SCM s , char* x , int start , int len )
{
  if (start + len >= SCHEME_STRTAG_VAL(s)) { scheme_signal_error("gh_get_substr: out-of-range"); } {memcpy(x, SCHEME_STR_VAL(s) + start, len); };
}

char* gh_symbol2newstr(SCM x , int* len )
{
  return (*len = SCHEME_SYM_LEN(x), strdup_sized(SCHEME_STR_VAL(x), *len));
}

int gh_boolean_p(SCM x )
{
  return SCHEME_BOOLP(x);
}

int gh_symbol_p(SCM x )
{
  return SCHEME_SYMBOLP(x);
}

int gh_char_p(SCM x )
{
  return SCHEME_CHARP(x);
}

int gh_vector_p(SCM x )
{
  return SCHEME_VECTORP(x);
}

int gh_pair_p(SCM x )
{
  return SCHEME_PAIRP(x);
}

int gh_procedure_p(SCM x )
{
  return SCHEME_PROCP(x);
}

int gh_string_p(SCM x )
{
  return SCHEME_STRINGP(x);
}

int gh_exact_p(SCM x )
{
  return scheme_is_exact(x);
}

int gh_inexact_p(SCM x )
{
  return scheme_is_inexact(x);
}

int gh_eq_p(SCM x , SCM y )
{
  return scheme_eq(x, y);
}

int gh_eqv_p(SCM x , SCM y )
{
  return scheme_eqv(x, y);
}

int gh_equal_p(SCM x , SCM y )
{
  return scheme_equal(x, y);
}

void gh_define(char* name , SCM v )
{
  scheme_add_global(name, v, scheme_env);
}

SCM gh_cons(SCM car , SCM cdr )
{
  return scheme_make_pair(car, cdr);
}

int gh_ilength(SCM ls )
{
  return scheme_proper_list_length(ls);
}

void gh_set_car(SCM pr , SCM val )
{
  SCHEME_CAR(pr) = val;
}

void gh_set_cdr(SCM pr , SCM val )
{
  SCHEME_CDR(pr) = val;
}

SCM gh_caaaar(SCM pr )
{
  return SCHEME_CAR(SCHEME_CAR(SCHEME_CAR(SCHEME_CAR(pr))));
}

SCM gh_caaadr(SCM pr )
{
  return SCHEME_CAR(SCHEME_CAR(SCHEME_CAR(SCHEME_CDR(pr))));
}

SCM gh_caadar(SCM pr )
{
  return SCHEME_CAR(SCHEME_CAR(SCHEME_CDR(SCHEME_CAR(pr))));
}

SCM gh_caaddr(SCM pr )
{
  return SCHEME_CAR(SCHEME_CAR(SCHEME_CDR(SCHEME_CDR(pr))));
}

SCM gh_caaar(SCM pr )
{
  return SCHEME_CAR(SCHEME_CAR(SCHEME_CAR(pr)));
}

SCM gh_caadr(SCM pr )
{
  return SCHEME_CAR(SCHEME_CAR(SCHEME_CDR(pr)));
}

SCM gh_cadaar(SCM pr )
{
  return SCHEME_CAR(SCHEME_CDR(SCHEME_CAR(SCHEME_CAR(pr))));
}

SCM gh_cadadr(SCM pr )
{
  return SCHEME_CAR(SCHEME_CDR(SCHEME_CAR(SCHEME_CDR(pr))));
}

SCM gh_caddar(SCM pr )
{
  return SCHEME_CAR(SCHEME_CDR(SCHEME_CDR(SCHEME_CAR(pr))));
}

SCM gh_cadddr(SCM pr )
{
  return SCHEME_CAR(SCHEME_CDR(SCHEME_CDR(SCHEME_CDR(pr))));
}

SCM gh_cadar(SCM pr )
{
  return SCHEME_CAR(SCHEME_CDR(SCHEME_CAR(pr)));
}

SCM gh_caddr(SCM pr )
{
  return SCHEME_CAR(SCHEME_CDR(SCHEME_CDR(pr)));
}

SCM gh_caar(SCM pr )
{
  return SCHEME_CAR(SCHEME_CAR(pr));
}

SCM gh_cadr(SCM pr )
{
  return SCHEME_CAR(SCHEME_CDR(pr));
}

SCM gh_cdaaar(SCM pr )
{
  return SCHEME_CDR(SCHEME_CAR(SCHEME_CAR(SCHEME_CAR(pr))));
}

SCM gh_cdaadr(SCM pr )
{
  return SCHEME_CDR(SCHEME_CAR(SCHEME_CAR(SCHEME_CDR(pr))));
}

SCM gh_cdadar(SCM pr )
{
  return SCHEME_CDR(SCHEME_CAR(SCHEME_CDR(SCHEME_CAR(pr))));
}

SCM gh_cdaddr(SCM pr )
{
  return SCHEME_CDR(SCHEME_CAR(SCHEME_CDR(SCHEME_CDR(pr))));
}

SCM gh_cdaar(SCM pr )
{
  return SCHEME_CDR(SCHEME_CAR(SCHEME_CAR(pr)));
}

SCM gh_cdadr(SCM pr )
{
  return SCHEME_CDR(SCHEME_CAR(SCHEME_CDR(pr)));
}

SCM gh_cddaar(SCM pr )
{
  return SCHEME_CDR(SCHEME_CDR(SCHEME_CAR(SCHEME_CAR(pr))));
}

SCM gh_cddadr(SCM pr )
{
  return SCHEME_CDR(SCHEME_CDR(SCHEME_CAR(SCHEME_CDR(pr))));
}

SCM gh_cdddar(SCM pr )
{
  return SCHEME_CDR(SCHEME_CDR(SCHEME_CDR(SCHEME_CAR(pr))));
}

SCM gh_cddddr(SCM pr )
{
  return SCHEME_CDR(SCHEME_CDR(SCHEME_CDR(SCHEME_CDR(pr))));
}

SCM gh_cddar(SCM pr )
{
  return SCHEME_CDR(SCHEME_CDR(SCHEME_CAR(pr)));
}

SCM gh_cdddr(SCM pr )
{
  return SCHEME_CDR(SCHEME_CDR(SCHEME_CDR(pr)));
}

SCM gh_cdar(SCM pr )
{
  return SCHEME_CDR(SCHEME_CAR(pr));
}

SCM gh_cddr(SCM pr )
{
  return SCHEME_CDR(SCHEME_CDR(pr));
}

SCM gh_car(SCM pr )
{
  return SCHEME_CAR(pr);
}

SCM gh_cdr(SCM pr )
{
  return SCHEME_CDR(pr);
}

SCM gh_vector(int len , SCM fill )
{
  return scheme_make_vector(len, fill);
}

SCM gh_vref(SCM v , int i )
{
  return SCHEME_VEC_ELS(v)[i];
}

SCM gh_vset(SCM v , int i , SCM x )
{
  return SCHEME_VEC_ELS(v)[i] = x;
}

int gh_vector_length(SCM v )
{
  return SCHEME_VEC_SIZE(v);
}

SCM gh_apply(SCM rator , SCM rands )
{
  return scheme_apply_to_list(rator, rands);
}

SCM gh_call0(SCM proc )
{
  return _scheme_apply(proc, 0, NULL);
}

int lgh_obj_length(SCM obj )
{
  return -1;
}

void gh_new_argv_procedure(char* name , SCM_ARGV_FN fn , int required , int optional )
{
  scheme_add_global(name, scheme_make_prim_w_arity(fn, name, required, optional), scheme_env);
}

SCM gh_make_argv_subr(SCM_ARGV_FN fn , int required , int optional , char* doc )
{
  return scheme_make_prim_w_arity(fn, NULL, required, optional);
}

SCM gh_apply_argv(SCM rator , int argc , SCM* argv )
{
  return _scheme_apply(rator, argc, argv);
}

void gh_lock(SCM v )
{
  scheme_dont_gc_ptr(v);
}

void gh_unlock(SCM v )
{
  scheme_gc_ptr_ok(v);
}

