/* stubs. */

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#include <dirent.h>
#include <errno.h>

CAMLprim value db_readdir(value vd)
{
	CAMLparam0();
	CAMLlocal1(pair);
	DIR *d;
	struct dirent *e;
	d = DIR_Val(vd);
	if (d == (DIR *) NULL)
		unix_error(EBADF, "readdir", Nothing);
	e = readdir((DIR *) d);
	if (e == (struct dirent *) NULL)
		raise_end_of_file();

	pair = caml_alloc(2, 0);
	Store_field(pair, 0, copy_string(e->d_name));
	Store_field(pair, 1, copy_int64(e->d_ino));
	CAMLreturn(pair);
}
