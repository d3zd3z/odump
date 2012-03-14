/* stubs. */

#define _FILE_OFFSET_BITS 64
#define _GNU_SOURCE

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

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

/* Add a pair list to the result.  'head' should already be a root'. */
static value add_property(value head, char *key, char *val)
{
	CAMLlocal2(pair, cons);
	pair = caml_alloc(2, 0);
	Store_field(pair, 0, copy_string(key));
	Store_field(pair, 1, copy_string(val));
	cons = caml_alloc(2, 0);
	Store_field(cons, 0, pair);
	Store_field(cons, 1, head);
	return cons;
}

static value add_field(value head, char *key, long long val)
{
	char tmp[40];
	sprintf(tmp, "%lld", val);
	return add_property(head, key, tmp);
}

CAMLprim value db_lstat(value path)
{
	CAMLparam1(path);
	CAMLlocal2(list, pair);
	int result;
	struct stat sbuf;
	result = lstat(String_val(path), &sbuf);
	if (result == -1)
		uerror("lstat", path);

	/* Determine the file kind. */
	char *v;
        if (S_ISREG(sbuf.st_mode))
                v = "REG";
        else if (S_ISDIR(sbuf.st_mode))
                v = "DIR";
        else if (S_ISCHR(sbuf.st_mode))
                v = "CHR";
        else if (S_ISBLK(sbuf.st_mode))
                v = "BLK";
        else if (S_ISFIFO(sbuf.st_mode))
                v = "FIFO";
        else if (S_ISLNK(sbuf.st_mode))
                v = "LNK";
        else if (S_ISSOCK(sbuf.st_mode))
                v = "SOCK";
        else {
		errno = EINVAL;
		uerror("lstat", path);
	}

	list = Val_int(0);

	list = add_field(list, "mode", (long long)(sbuf.st_mode & (~S_IFMT)));
	list = add_field(list, "dev", (long long)sbuf.st_dev);
	list = add_field(list, "ino", (long long)sbuf.st_ino);
	list = add_field(list, "nlink", (long long)sbuf.st_nlink);
	list = add_field(list, "uid", (long long)sbuf.st_uid);
	list = add_field(list, "gid", (long long)sbuf.st_gid);

	if (S_ISCHR(sbuf.st_mode) || S_ISBLK(sbuf.st_mode)) {
		list = add_field(list, "rdev", (long long)sbuf.st_rdev);
	}

	list = add_field(list, "size", (long long)sbuf.st_size);

	char tmp[40];
	sprintf(tmp, "%lld.%09lld", (long long)sbuf.st_mtime, (long long)sbuf.st_mtim.tv_nsec);
	list = add_property(list, "mtime", tmp);
	sprintf(tmp, "%lld.%09lld", (long long)sbuf.st_ctime, (long long)sbuf.st_ctim.tv_nsec);
	list = add_property(list, "ctime", tmp);

	pair = caml_alloc(2, 0);
	Store_field(pair, 0, copy_string(v));
	Store_field(pair, 1, list);

	CAMLreturn(pair);
}

/* Open for reading.  This is a bit special because of the lack of
 * O_NOATIME in the ocaml binding. */
CAMLprim value db_open_for_read(value path)
{
	CAMLparam1(path);
	/* Assume only called on real files, so the path doesn't need
	 * to be copied. */
	int fd = open(String_val(path), O_RDONLY | O_NOATIME);
	/* Non-root users aren't permitted to open without changing
	 * atime on files they don't own.  If this happens, try
	 * opening the normal way. */
	if (fd < 0 && errno == EPERM)
		fd = open(String_val(path), O_RDONLY);
	if (fd < 0)
		uerror("open_for_read", path);
	CAMLreturn (Val_int(fd));
}
