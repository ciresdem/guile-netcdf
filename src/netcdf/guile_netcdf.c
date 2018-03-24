/*

  nc_guile.c 

  Copyright (c) 2011, 2012, 2013, 2018 Matthew Love <matthew.love@colorado.edu>
  This file is part of guile-netcdf.
  GUILE-NETCDF is liscensed under the GPL v.3 or later and
  is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  <http://www.gnu.org/licenses/>

*/

#include <stdio.h> 
#include <stdlib.h> 
#include <string.h>
#include <ctype.h>
#include <libguile.h>
#include "netcdf.h"

//---BEGIN
static scm_t_bits ncid_tag;
static scm_t_bits nctype_tag;

/*----------
  NetCDF
----------*/
SCM
nc_inq_libvers_wrapper () 
{
  // RETURN NETCDF LIBVERSION
  return scm_from_locale_string (nc_inq_libvers ());
}

SCM
nc_guile_inq_libvers_wrapper () 
{
  // RETURN NETCDF LIBVERSION
  return scm_from_locale_string ("0.0.1");
}

/*----------
  NCID Smob Structs
----------*/

//---NCID
struct ncid {
  SCM fname;
  SCM idname;
  int status;
  int nc_id;
  SCM update_func;
};

static SCM
mark_ncid (SCM ncid_smob)
{
  /* Mark the image's name and update function.  */
  struct ncid *ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  
  scm_gc_mark (ncid->fname);
  scm_gc_mark (ncid->idname);

  return ncid->update_func;
}

static size_t
free_ncid (SCM ncid_smob)
{
  struct ncid *ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  
  scm_gc_free (ncid, sizeof (struct ncid), "ncid");
  
  return 0;
}

static int
print_ncid (SCM ncid_smob, SCM port, scm_print_state *pstate)
{

  struct ncid *ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);

  scm_puts ("#<NetCDF ", port);
  scm_display (ncid->fname, port);
  scm_puts (" ", port);
  scm_display (ncid->idname, port);
  scm_puts (">", port);
  
  /* non-zero means success */
  return 1;
}

//---NC_TYPE
struct nctype {
  SCM typename;
  SCM typeshortname;
  int status;
  nc_type c_nc_type;
  SCM update_func;
};

static SCM
mark_nctype (SCM nctype_smob)
{
  /* Mark the image's name and update function.  */
  struct nctype *nctype = (struct nctype *) SCM_SMOB_DATA (nctype_smob);
  
  scm_gc_mark (nctype->typename);
  scm_gc_mark (nctype->typeshortname);

  return nctype->update_func;
}

static size_t
free_nctype (SCM nctype_smob)
{
  struct nctype *nctype = (struct nctype *) SCM_SMOB_DATA (nctype_smob);
  
  scm_gc_free (nctype, sizeof (struct nctype), "nctype");
  
  return 0;
}

static int
print_nctype (SCM nctype_smob, SCM port, scm_print_state *pstate)
{

  struct nctype *nctype = (struct nctype *) SCM_SMOB_DATA (nctype_smob);

  scm_puts ("#<nctype ", port);
  scm_display (nctype->typename, port);
  scm_puts (">", port);
  
  /* non-zero means success */
  return 1;
}

SCM
nc_display_nctype (SCM nctype_smob)
{
  struct nctype *nctype = (struct nctype *) SCM_SMOB_DATA (nctype_smob);

  return nctype->typename;
}

SCM
nc_display_short_nctype (SCM nctype_smob)
{
  struct nctype *nctype = (struct nctype *) SCM_SMOB_DATA (nctype_smob);

  return nctype->typeshortname;
}

SCM
scm_make_nc_type (SCM scm_nctype) 
{
  int status = NC_NOERR;
  char *c_nctype_str;
  SCM smob;
  nc_type c_nc_type;
  struct nctype *nctype;

  // NC_TYPE
  nctype = (struct nctype *) scm_gc_malloc (sizeof (struct nctype), "nctype");

  c_nctype_str = scm_to_locale_string (scm_symbol_to_string (scm_nctype));
  if ((strcmp (c_nctype_str, "NC-BYTE") == 0) || 
      (strcmp (c_nctype_str, "byte") == 0))
    {
      c_nc_type = NC_BYTE;
    } 
  else if ((strcmp (c_nctype_str, "NC-CHAR") == 0) || 
	   (strcmp (c_nctype_str, "char") == 0))
    {
      c_nc_type = NC_CHAR;
    } 
  else if ((strcmp (c_nctype_str, "NC-SHORT") == 0) || 
	   (strcmp (c_nctype_str, "short") == 0))
    {
      c_nc_type = NC_SHORT;
    } 
  else if ((strcmp (c_nctype_str, "NC-USHORT") == 0) || 
	   (strcmp (c_nctype_str, "ushort") == 0))
    {
      c_nc_type = NC_USHORT;
    } 
  else if ((strcmp (c_nctype_str, "NC-INT") == 0) || 
	   (strcmp (c_nctype_str, "int") == 0))
    {
      c_nc_type = NC_INT;
    } 
  else if ((strcmp (c_nctype_str, "NC-UINT") == 0) || 
	   (strcmp (c_nctype_str, "uint") == 0))
    {
      c_nc_type = NC_UINT;
    } 
  else if ((strcmp (c_nctype_str, "NC-FLOAT") == 0) || 
	   (strcmp (c_nctype_str, "float") == 0))
    {
      c_nc_type = NC_FLOAT;
    } 
  else if ((strcmp (c_nctype_str, "NC-DOUBLE") == 0) || 
	   (strcmp (c_nctype_str, "double") == 0))
    {
      c_nc_type = NC_DOUBLE;
    } 
  else if ((strcmp (c_nctype_str, "NC-STRING") == 0) || 
	   (strcmp (c_nctype_str, "string") == 0))
    {
      c_nc_type = NC_STRING;
    } 
  else 
    {
      c_nc_type = -1;
    }

  // NC_TYPE SMOB
  nctype->typename = scm_from_locale_string("");;
  nctype->status = 0;
  nctype->c_nc_type = 0;
  nctype->update_func = SCM_BOOL_F;

  SCM_NEWSMOB (smob, nctype_tag, nctype);

  nctype->typename = scm_nctype;
  nctype->status = status;
  nctype->c_nc_type = c_nc_type;

  // RETURN NC_TYPE SMOB
  return smob;
}

/*----------
  Error
----------*/

SCM
nc_strerrer_wrapper (SCM ncid_smob) 
{
  int c_status;
  SCM status;

  // NCID
  struct ncid *ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);

  // STATUS
  c_status = ncid->status;
  status = scm_from_locale_string (nc_strerror (c_status));

  // RETURN STATUS
  return status;
}

/*----------
  Datasets
----------*/

SCM
nc_file_p_wrapper (SCM scm_fn) 
{
  char *nc_fn;
  int c_ncid, status;
  size_t length;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  if (scm_is_true (scm_string_p (scm_fn))) 
    {
      nc_fn = scm_to_locale_string (scm_fn);
    } 
  else 
    {
      return SCM_BOOL_F;
    }

  // NC_OPEN
  status = nc_open (nc_fn, 0, &c_ncid);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    } 
  else 
    {
      status = nc_close (c_ncid);
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr,"%s\n", nc_strerror (status));
	    }
	}
      return SCM_BOOL_T;
    }
}

SCM
nc_create_wrapper (SCM scm_fn, SCM scm_cmode) 
{
  char *nc_fn, *c_cmode_str;
  int c_ncid, status, c_cmode;
  size_t length;
  SCM smob;
  struct ncid *ncid;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // SCM_FN
  nc_fn = scm_to_locale_string (scm_fn);

  // NCID
  ncid = (struct ncid *) scm_gc_malloc (sizeof (struct ncid), "ncid");

  // OPEN MODE
  c_cmode_str = scm_to_locale_string (scm_symbol_to_string (scm_cmode));
  if (strcmp (c_cmode_str, "no-clobber") == 0) 
    {
      c_cmode = NC_NOCLOBBER;
    } 
  else if (strcmp (c_cmode_str, "64-bit-offset") == 0) 
    {
      c_cmode = NC_64BIT_OFFSET;
    } 
  else if (strcmp (c_cmode_str, "share") == 0) 
    {
      c_cmode = NC_SHARE;
    } 
  else if (strcmp (c_cmode_str, "netcdf4") == 0) 
    {
      c_cmode = NC_NETCDF4;
    } 
  else if (strcmp (c_cmode_str, "classic-model") == 0) 
    {
      c_cmode = NC_CLASSIC_MODEL;
    } 
  else 
    {
      c_cmode = -1;
    }

  // NC_CREATE
  status = nc_create (nc_fn, c_cmode, &c_ncid);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
    }

  // NCID SMOB
  ncid->fname = scm_fn;
  ncid->idname = scm_from_locale_string ("ncid");
  ncid->status = 0;
  ncid->nc_id = 0;
  ncid->update_func = SCM_BOOL_F;

  SCM_NEWSMOB (smob, ncid_tag, ncid);

  ncid->fname = scm_fn;
  ncid->idname = scm_from_locale_string ("ncid");
  ncid->status = status;
  ncid->nc_id = c_ncid;

  // RETURN NCID SMOB
  return smob;
}

SCM
nc_open_wrapper (SCM scm_fn, SCM scm_omode) 
{
  char *nc_fn, *c_omode_str;
  int c_ncid, status, c_omode;
  size_t length;
  SCM smob;
  struct ncid *ncid;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // SCM_FN
  nc_fn = scm_to_locale_string (scm_fn);

  // NCID
  ncid = (struct ncid *) scm_gc_malloc (sizeof (struct ncid), "ncid");

  // OPEN MODE
  c_omode_str = scm_to_locale_string (scm_symbol_to_string (scm_omode));
  if ( strcmp (c_omode_str, "read") == 0) 
    {
      c_omode = 0;
    } 
  else if ( strcmp (c_omode_str, "write") == 0) 
    {
      c_omode = NC_WRITE;
    } 
  else if ( strcmp (c_omode_str, "write-data") == 0) 
    {
      c_omode = NC_WRITE;
    } 
  else if ( strcmp (c_omode_str, "share") == 0) 
    {
      c_omode = NC_SHARE;
    } 
  else if ( strcmp (c_omode_str, "write/share") == 0) 
    {
      c_omode = NC_WRITE|NC_SHARE;
    } 
  else 
    {
      c_omode = 0;
    }

  // NC_OPEN
  status = nc_open (nc_fn, c_omode, &c_ncid);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
    }

  // NCID SMOB
  ncid->fname = scm_fn;
  ncid->idname = scm_from_locale_string ("ncid");
  ncid->status = 0;
  ncid->nc_id = 0;
  ncid->update_func = SCM_BOOL_F;

  SCM_NEWSMOB (smob, ncid_tag, ncid);

  ncid->fname = scm_fn;
  ncid->idname = scm_from_locale_string ("ncid");
  ncid->status = status;
  ncid->nc_id = c_ncid;

  // RETURN NCID SMOB
  return smob;
}

SCM
nc_redef_wrapper (SCM ncid_smob) 
{
  int c_ncid, status;
  struct ncid *nc_ncid;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // NC_REDEF
  status = nc_redef (c_ncid);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }
  // RETURN T
  return SCM_BOOL_T;
}

SCM
nc_enddef_wrapper (SCM ncid_smob) 
{
  int c_ncid, status;
  struct ncid *nc_ncid;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // NC_ENDDEF
  status = nc_enddef (c_ncid);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }
  // RETURN T
  return SCM_BOOL_T;
}

SCM
nc_close_wrapper (SCM ncid_smob) 
{
  int c_ncid, status;
  struct ncid *ncid;
  SCM smob;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = ncid->nc_id;

  // NC_CLOSE
  status = nc_close (c_ncid);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }
  return SCM_BOOL_T;
}

SCM
nc_inq_wrapper (SCM ncid_smob) 
{
  int c_ncid, status, ndimsp, nvarsp, ngattsp, unlimdimidp;
  struct ncid *nc_ncid;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // NC_INQ
  status = nc_inq (c_ncid, &ndimsp, &nvarsp, &ngattsp, &unlimdimidp);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }
  // RETURN '(number_dims number_vars number_atts unlimited_dim)
  return scm_list_4 (scm_from_int(ndimsp), scm_from_int(nvarsp), scm_from_int(ngattsp), scm_from_int(unlimdimidp));
}

SCM
nc_inq_ndims_wrapper (SCM ncid_smob) 
{
  int c_ncid, status, ndimsp;
  struct ncid *nc_ncid;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // NC_INQ_NDIMS
  status = nc_inq_ndims (c_ncid, &ndimsp);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }
  // RETURN number_dims
  return scm_from_int (ndimsp);
}

SCM
nc_inq_nvars_wrapper (SCM ncid_smob) 
{
  int c_ncid, status, nvarsp;
  struct ncid *nc_ncid;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // NC_INQ_NVARS
  status = nc_inq_nvars (c_ncid, &nvarsp);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }
  // RETURN number_vars
  return scm_from_int (nvarsp);
}

SCM
nc_inq_natts_wrapper (SCM ncid_smob) 
{
  int c_ncid, status, ngattsp;
  struct ncid *nc_ncid;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // NC_INQ_NATTS
  status = nc_inq_natts (c_ncid, &ngattsp);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }
  // RETURN number_atts
  return scm_from_int (ngattsp);
}

SCM
nc_inq_unlimdim_wrapper (SCM ncid_smob) 
{
  int c_ncid, status, unlimdimidp;
  struct ncid *nc_ncid;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // NC_INQ_UNLIMDIM
  status = nc_inq_unlimdim (c_ncid, &unlimdimidp);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }
  // RETURN unlimited_dims
  return scm_from_int (unlimdimidp);
}

SCM
nc_inq_format_wrapper (SCM ncid_smob) 
{
  int c_ncid, status, formatp;
  struct ncid *nc_ncid;
  SCM nc_format;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // NC_INQ_FORMAT
  status = nc_inq_format (c_ncid, &formatp);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  // NC_FORMAT
  if (formatp == NC_FORMAT_CLASSIC) 
    {
      nc_format = scm_from_locale_string("NC_FORMAT_CLASSIC");
    } 
  else if (formatp == NC_FORMAT_64BIT) 
    {
      nc_format = scm_from_locale_string("NC_FORMAT_64BIT");
    } 
  else if (formatp == NC_FORMAT_NETCDF4) 
    {
      nc_format = scm_from_locale_string("NC_FORMAT_NETCDF4");
    } 
  else if (formatp == NC_FORMAT_NETCDF4_CLASSIC) 
    {
      nc_format = scm_from_locale_string("NC_FORMAT_NETCDF4_CLASSIC");
    } 
  else 
    {
      nc_format = scm_from_locale_string("UNKNOWN");
    }
  // RETURN nc_format
  return nc_format;
}

SCM
nc_sync_wrapper (SCM ncid_smob) 
{
  int c_ncid, status;
  struct ncid *nc_ncid;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // NC_SYNC
  status = nc_sync (c_ncid);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }
  // RETURN T
  return SCM_BOOL_T;
}

SCM
nc_abort_wrapper (SCM ncid_smob) 
{
  int c_ncid, status;
  struct ncid *nc_ncid;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // NC_ABORT
  status = nc_abort (c_ncid);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }
  // RETURN T
  return SCM_BOOL_T;
}

/*----------
  Groups
----------*/

SCM
nc_inq_ncid_wrapper (SCM ncid_smob, SCM grpid_name) 
{
  int c_ncid, status, c_grpid;
  size_t length;
  char *grpid_c_name;
  struct ncid *nc_grpid, *nc_ncid;
  SCM grpid_smob;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // GROUP NAME
  grpid_c_name = scm_to_locale_string (grpid_name);

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // NC_GROUP
  nc_grpid = (struct ncid *) scm_gc_malloc (sizeof (struct ncid), "nc-grpid");

  // NC_INQ_NCID
  status = nc_inq_ncid (c_ncid, grpid_c_name, &c_grpid);
  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
    }

  // NC_GROUPID SMOB
  nc_grpid->fname = scm_from_locale_string ("nil");
  nc_grpid->idname = scm_from_locale_string ("grpid");
  nc_grpid->status = 0;
  nc_grpid->nc_id = 0;
  nc_grpid->update_func = SCM_BOOL_F;

  SCM_NEWSMOB (grpid_smob, ncid_tag, nc_grpid);

  nc_grpid->fname = nc_ncid->fname;
  nc_grpid->idname = grpid_name;
  nc_grpid->status = status;
  nc_grpid->nc_id = c_grpid;

  // RETURN NC_GROUPID SMOB
  return grpid_smob;
}

// TEST THIS! Don't have a netcdf file with grps :(
SCM
nc_inq_grps_wrapper (SCM ncid_smob) 
{
  int c_ncid, status, ngrpsp, i;
  struct ncid *nc_ncid;
  int *c_grpids;
  SCM grpid_smobs = SCM_EOL;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // NC_INQ_GRPS (get number of groups)
  status = nc_inq_grps (c_ncid, &ngrpsp, NULL);

  c_grpids = malloc (sizeof (int) * ngrpsp);

  // NC_INQ_GRPS (get group ncids)
  status = nc_inq_grps (c_ncid, NULL, c_grpids);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  for (i=0; i<ngrpsp; i++) {
    SCM grpid_smob_1;
    SCM grpid_smob;
    struct ncid *nc_grpids;

    nc_grpids = (struct ncid *) scm_gc_malloc (sizeof (struct ncid), "nc-grpid");
    nc_grpids->fname = scm_from_locale_string ("nil");
    nc_grpids->idname = scm_from_locale_string ("grpid");
    nc_grpids->status = 0;
    nc_grpids->nc_id = 0;
    nc_grpids->update_func = SCM_BOOL_F;  
    // DIMID SMOB    
    SCM_NEWSMOB (grpid_smob, ncid_tag, nc_grpids);
    
    nc_grpids->fname = nc_ncid->fname;
    nc_grpids->status = status;
    nc_grpids->nc_id = c_grpids[i];

    grpid_smob_1 = scm_list_1 (grpid_smob);
    
    grpid_smobs = scm_append (scm_list_2 (grpid_smobs, grpid_smob_1));    
  }
  // RETURN '(numgrps grpid_smobs)
  return scm_list_2 (scm_from_int (ngrpsp), grpid_smobs);
}

SCM
nc_inq_dimids_wrapper (SCM ncid_smob) 
{
  int c_ncid, status, ndimsp, i;
  struct ncid *nc_ncid;
  int c_dimids[NC_MAX_DIMS];
  SCM dimid_smobs = SCM_EOL;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // NC_INQ_DIMIDS
  status = nc_inq_dimids (c_ncid, &ndimsp, c_dimids, 0);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  for (i=0; i<ndimsp; i++) 
    {
      SCM dimid_smob_1;
      SCM dimid_smob;
      struct ncid *nc_dimids;
      
      nc_dimids = (struct ncid *) scm_gc_malloc (sizeof (struct ncid), "nc-dimid");
      nc_dimids->fname = scm_from_locale_string ("nil");
      nc_dimids->idname = scm_from_locale_string ("dimid");
      nc_dimids->status = 0;
      nc_dimids->nc_id = 0;
      nc_dimids->update_func = SCM_BOOL_F;  
      
      // DIMID SMOB    
      SCM_NEWSMOB (dimid_smob, ncid_tag, nc_dimids);
      
      nc_dimids->fname = nc_ncid->fname;
      nc_dimids->status = status;
      nc_dimids->nc_id = c_dimids[i];
      
      dimid_smob_1 = scm_list_1 (dimid_smob);
      
      dimid_smobs = scm_append (scm_list_2 (dimid_smobs, dimid_smob_1));    
    }
  // RETURN dimid_smobs
  return dimid_smobs;
}

SCM
nc_inq_varids_wrapper (SCM ncid_smob) 
{
  int c_ncid, status, nvarsp, i;
  struct ncid *nc_ncid;
  int c_varids[NC_MAX_VARS];
  SCM varid_smobs = SCM_EOL;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // NC_INQ_VARIDS
  status = nc_inq_varids (c_ncid, &nvarsp, c_varids);
  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}	
      return SCM_BOOL_F;
    }

  for (i=0; i<nvarsp; i++) 
    {
      SCM varid_smob_1;
      SCM varid_smob;
      struct ncid *nc_varids;

      nc_varids = (struct ncid *) scm_gc_malloc (sizeof (struct ncid), "nc-varid");
      nc_varids->fname = scm_from_locale_string ("nil");
      nc_varids->idname = scm_from_locale_string ("varid");
      nc_varids->status = 0;
      nc_varids->nc_id = 0;
      nc_varids->update_func = SCM_BOOL_F;  

      // DIMID SMOB    
      SCM_NEWSMOB (varid_smob, ncid_tag, nc_varids);
      
      nc_varids->fname = nc_ncid->fname;
      nc_varids->status = status;
      nc_varids->nc_id = c_varids[i];
      
      varid_smob_1 = scm_list_1 (varid_smob);
      
      varid_smobs = scm_append (scm_list_2 (varid_smobs, varid_smob_1));    
    }
  // RETURN varid_smobs
  return varid_smobs;
}

/*----------
   Dimensions
----------*/

SCM
nc_inq_dimid_wrapper (SCM ncid_smob, SCM dimid_name) 
{
  int c_ncid, status, c_dimid;
  size_t length;
  char *dimid_c_name;
  struct ncid *nc_dimid, *nc_ncid;
  SCM dimid_smob;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // DIMID NAME
  dimid_c_name = scm_to_locale_string (dimid_name);

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // NC_DIMID
  nc_dimid = (struct ncid *) scm_gc_malloc (sizeof (struct ncid), "nc-dimid");

  // NC_INQ_DIMID
  status = nc_inq_dimid (c_ncid, dimid_c_name, &c_dimid);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}	
    }

  // NC_DIMID SMOB
  nc_dimid->fname = scm_from_locale_string ("nil");
  nc_dimid->idname = scm_from_locale_string ("dimid");
  nc_dimid->status = 0;
  nc_dimid->nc_id = 0;
  nc_dimid->update_func = SCM_BOOL_F;

  SCM_NEWSMOB (dimid_smob, ncid_tag, nc_dimid);

  nc_dimid->fname = nc_ncid->fname;
  nc_dimid->idname = scm_string_append (scm_list_2 (dimid_name, scm_from_locale_string ("_dimid")));
  nc_dimid->status = status;
  nc_dimid->nc_id = c_dimid;

  // RETURN NC_DIMID SMOB
  return dimid_smob;
}

SCM
nc_inq_dim_wrapper (SCM ncid_smob, SCM dimid_smob) 
{
  int c_ncid, status, c_dimid;
  struct ncid *nc_ncid, *nc_dimid;
  char recname[NC_MAX_NAME+1];
  //SCM dimid_length, dimid_name;
  //size_t dim_length, recs;
  size_t recs;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // DIMID
  scm_assert_smob_type (ncid_tag, dimid_smob);
  nc_dimid = (struct ncid *) SCM_SMOB_DATA (dimid_smob);
  c_dimid = nc_dimid->nc_id;

  // NC_INQ_DIM
  status = nc_inq_dim (c_ncid, c_dimid, recname, &recs);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}	
      return SCM_BOOL_F;
    }

  nc_dimid->idname = scm_string_append (scm_list_2 (scm_from_locale_string (recname), scm_from_locale_string ("_dimid")));

  //RETURN '(recname num_recs)
  return scm_list_2 (scm_from_locale_string (recname), scm_from_size_t (recs));
}

SCM
nc_inq_dimlen_wrapper (SCM ncid_smob, SCM dimid_smob) 
{
  int c_ncid, status, c_dimid;
  struct ncid *nc_ncid, *nc_dimid;
  //SCM dimid_length, dimid_name;
  size_t dim_length;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // DIMID
  scm_assert_smob_type (ncid_tag, dimid_smob);
  nc_dimid = (struct ncid *) SCM_SMOB_DATA (dimid_smob);
  c_dimid = nc_dimid->nc_id;

  // NC_INQ_DIMLEN
  status = nc_inq_dimlen (c_ncid, c_dimid, &dim_length);

  if (status != NC_NOERR) 
    {
      fprintf (stderr, "%s\n", nc_strerror (status));
      return SCM_BOOL_F;
    }
  // RETURN dim_length
  return scm_from_size_t (dim_length);
}

SCM
nc_inq_dimname_wrapper (SCM ncid_smob, SCM dimid_smob) 
{
  int c_ncid, status, c_dimid;
  struct ncid *nc_ncid, *nc_dimid;
  char recname[NC_MAX_NAME+1];
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // DIMID
  scm_assert_smob_type (ncid_tag, dimid_smob);
  nc_dimid = (struct ncid *) SCM_SMOB_DATA (dimid_smob);
  c_dimid = nc_dimid->nc_id;

  // NC_INQ_DIMNAME
  status = nc_inq_dimname (c_ncid, c_dimid, recname);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  nc_dimid->idname = scm_string_append (scm_list_2 (scm_from_locale_string (recname), scm_from_locale_string ("_dimid")));
  nc_dimid->status = status;
  
  // RETURN recname
  return scm_from_locale_string (recname);
}

/*----------
  User Defined Data Types
----------*/

/*----------
  Compound Data Types
----------*/

/*----------
  Variables
----------*/

SCM
nc_inq_varid_wrapper (SCM ncid_smob, SCM varid_name) 
{
  int c_ncid, status, c_varid;
  size_t length;
  char *varid_c_name;
  struct ncid *nc_varid, *nc_ncid;
  SCM varid_smob;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // VARID NAME
  varid_c_name = scm_to_locale_string (varid_name);

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  nc_varid = (struct ncid *) scm_gc_malloc (sizeof (struct ncid), "nc-varid");

  // NC_INQ_VARID
  status = nc_inq_varid (c_ncid, varid_c_name, &c_varid);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
    }

  // VARID SMOB
  nc_varid->fname = scm_from_locale_string ("nil");
  nc_varid->idname = scm_from_locale_string ("varid");
  nc_varid->status = 0;
  nc_varid->nc_id = 0;
  nc_varid->update_func = SCM_BOOL_F;

  SCM_NEWSMOB (varid_smob, ncid_tag, nc_varid);

  nc_varid->fname = nc_ncid->fname;
  nc_varid->idname = scm_string_append (scm_list_2 (varid_name, scm_from_locale_string ("_varid")));
  nc_varid->status = status;
  nc_varid->nc_id = c_varid;

  // RETURN VARID SMOB
  return varid_smob;
}

SCM
nc_inq_var_wrapper (SCM ncid_smob, SCM varid_smob) 
{
  int c_ncid, status, c_varid, ndimsp, natts, i;
  nc_type vartype;
  struct ncid *nc_ncid, *nc_varid;
  struct nctype *scm_nctype;
  char recname[NC_MAX_NAME+1];
  int c_dimids[NC_MAX_VAR_DIMS];
  SCM scm_typename, scm_typeshortname, nc_scm_type;
  SCM dimid_smobs = SCM_EOL;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_INQ_VAR
  status = nc_inq_var (c_ncid, c_varid, recname, &vartype, &ndimsp, c_dimids, &natts);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  for (i=0; i<ndimsp; i++) 
    {
      SCM dimid_smob_1;
      SCM dimid_smob;
      struct ncid *nc_dimids;

      nc_dimids = (struct ncid *) scm_gc_malloc (sizeof (struct ncid), "nc-dimid");
      nc_dimids->fname = scm_from_locale_string ("nil");
      nc_dimids->idname = scm_from_locale_string ("dimid");
      nc_dimids->status = 0;
      nc_dimids->nc_id = 0;
      nc_dimids->update_func = SCM_BOOL_F;  
      
      // DIMID SMOB    
      SCM_NEWSMOB (dimid_smob, ncid_tag, nc_dimids);
      
      nc_dimids->fname = nc_ncid->fname;
      nc_dimids->status = status;
      nc_dimids->nc_id = c_dimids[i];
      
      dimid_smob_1 = scm_list_1 (dimid_smob);
      
      dimid_smobs = scm_append (scm_list_2 (dimid_smobs, dimid_smob_1));    
    }

  // NC_TYPE
  scm_nctype = (struct nctype *) scm_gc_malloc (sizeof (struct nctype), "nctype");

  scm_nctype->typename = scm_from_locale_string ("");
  scm_nctype->status = 0;
  scm_nctype->c_nc_type = -1;
  scm_nctype->update_func = SCM_BOOL_F;

  if (vartype == NC_BYTE) 
    {
      scm_typename = scm_from_locale_string ("NC_BYTE");
      scm_typeshortname = scm_from_locale_string ("byte");
    } 
  else if (vartype == NC_CHAR) 
    {
      scm_typename = scm_from_locale_string ("NC_CHAR");
      scm_typeshortname = scm_from_locale_string ("char");
    } 
  else if (vartype == NC_SHORT) 
    {
      scm_typename = scm_from_locale_string ("NC_SHORT");
      scm_typeshortname = scm_from_locale_string ("short");
    } 
  else if (vartype == NC_USHORT) 
    {
      scm_typename = scm_from_locale_string ("NC_USHORT");
      scm_typeshortname = scm_from_locale_string ("ushort");
    } 
  else if (vartype == NC_INT) 
    {
      scm_typename = scm_from_locale_string ("NC_INT");
      scm_typeshortname = scm_from_locale_string ("int");
    } 
  else if (vartype == NC_UINT) 
    {
      scm_typename = scm_from_locale_string ("NC_UINT");
      scm_typeshortname = scm_from_locale_string ("uint");
    } 
  else if (vartype == NC_FLOAT) 
    {
      scm_typename = scm_from_locale_string ("NC_FLOAT");
      scm_typeshortname = scm_from_locale_string ("float");
    } 
  else if (vartype == NC_DOUBLE) 
    {
      scm_typename = scm_from_locale_string ("NC_DOUBLE");
      scm_typeshortname = scm_from_locale_string ("double");
    } 
  else if (vartype == NC_STRING) 
    {
      scm_typename = scm_from_locale_string ("NC_STRING");
      scm_typeshortname = scm_from_locale_string ("string");
    } 
  else 
    {
      scm_typename = scm_from_locale_string ("UNKNOWN");
      scm_typeshortname = scm_from_locale_string ("unk");
    }
  
  SCM_NEWSMOB (nc_scm_type, nctype_tag, scm_nctype);

  scm_nctype->typename = scm_typename;
  scm_nctype->typeshortname = scm_typeshortname;
  scm_nctype->status = status;
  scm_nctype->c_nc_type = vartype;

  nc_varid->idname = scm_string_append (scm_list_2 (scm_from_locale_string (recname), scm_from_locale_string ("_varid")));

  // RETURN '(recname nc_type num_dims dimids num_atts)
  return scm_list_5 (scm_from_locale_string (recname), nc_scm_type, scm_from_int (ndimsp), dimid_smobs, scm_from_int (natts));
}

SCM
nc_inq_varname_wrapper (SCM ncid_smob, SCM varid_smob) 
{
  int c_ncid, status, c_varid;
  struct ncid *nc_ncid, *nc_varid;
  char recname[NC_MAX_NAME+1];
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_INQ_VARNAME
  status = nc_inq_varname (c_ncid, c_varid, recname);

  if (status != NC_NOERR) 
    {
      fprintf (stderr, "%s\n", nc_strerror (status));
      return SCM_BOOL_F;
    }

  nc_varid->idname = scm_string_append (scm_list_2 (scm_from_locale_string (recname), scm_from_locale_string ("_varid")));

  // RETURN recname
  return scm_from_locale_string (recname);
}

SCM
nc_inq_vartype_wrapper (SCM ncid_smob, SCM varid_smob) 
{
  int c_ncid, status, c_varid;
  nc_type vartype;
  struct ncid *nc_ncid, *nc_varid;
  struct nctype *nc_c_type;
  SCM nc_scm_type, scm_typename, scm_typeshortname;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NC_TYPE
  nc_c_type = (struct nctype *) scm_gc_malloc (sizeof (struct nctype), "nctype");

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_INQ_VARTYPE
  status = nc_inq_vartype (c_ncid, c_varid, &vartype);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }
  
  // NC_TYPE
  nc_c_type->typename = scm_from_locale_string ("");
  nc_c_type->status = 0;
  nc_c_type->c_nc_type = -1;
  nc_c_type->update_func = SCM_BOOL_F;

  if (vartype == NC_BYTE) 
    {
      scm_typename = scm_from_locale_string ("NC_BYTE");
      scm_typeshortname = scm_from_locale_string ("byte");
    } 
  else if (vartype == NC_CHAR) 
    {
      scm_typename = scm_from_locale_string ("NC_CHAR");
      scm_typeshortname = scm_from_locale_string ("char");
    } 
  else if (vartype == NC_SHORT) 
    {
      scm_typename = scm_from_locale_string ("NC_SHORT");
      scm_typeshortname = scm_from_locale_string ("short");
    }
  else if (vartype == NC_USHORT) 
    {
      scm_typename = scm_from_locale_string ("NC_USHORT");
      scm_typeshortname = scm_from_locale_string ("ushort");
    } 
  else if (vartype == NC_INT) 
    {
      scm_typename = scm_from_locale_string ("NC_INT");
      scm_typeshortname = scm_from_locale_string ("int");
    }
  else if (vartype == NC_UINT) 
    {
      scm_typename = scm_from_locale_string ("NC_UINT");
      scm_typeshortname = scm_from_locale_string ("uint");
    } 
  else if (vartype == NC_FLOAT) 
    {
      scm_typename = scm_from_locale_string ("NC_FLOAT");
      scm_typeshortname = scm_from_locale_string ("float");
    } 
  else if (vartype == NC_DOUBLE) 
    {
      scm_typename = scm_from_locale_string ("NC_DOUBLE");
      scm_typeshortname = scm_from_locale_string ("double");
    } 
  else 
    {
      scm_typename = scm_from_locale_string ("UNKNOWN");
      scm_typeshortname = scm_from_locale_string ("unk");
    }
  
  SCM_NEWSMOB (nc_scm_type, nctype_tag, nc_c_type);

  nc_c_type->typename = scm_typename;
  nc_c_type->typeshortname = scm_typeshortname;
  nc_c_type->status = status;
  nc_c_type->c_nc_type = vartype;

  // RETURN NC_TYPE
  return nc_scm_type;
}

SCM
nc_inq_varndims_wrapper (SCM ncid_smob, SCM varid_smob) 
{
  int c_ncid, status, c_varid, ndimsp;
  struct ncid *nc_ncid, *nc_varid;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_INQ_VARDIMS
  status = nc_inq_varndims (c_ncid, c_varid, &ndimsp);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }
  // RETURN number_dims
  return scm_from_int (ndimsp);
}

SCM
nc_inq_vardimid_wrapper (SCM ncid_smob, SCM varid_smob) 
{
  int c_ncid, status, c_varid, i;
  struct ncid *nc_ncid, *nc_varid;
  int c_dimids[NC_MAX_VAR_DIMS];
  SCM dimid_smobs = SCM_EOL;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;
  memset(c_dimids, -1, NC_MAX_VAR_DIMS * sizeof (int));

  // NC_INQ_VAR
  status = nc_inq_vardimid (c_ncid, c_varid, c_dimids);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  // DIMIDS

  for (i=0; i<(sizeof (c_dimids) / sizeof (int)); i++) {
    // DIMID SMOB  
    if (c_dimids[i] > -1 && c_dimids[i] < NC_MAX_VAR_DIMS) {
      //printf("%d\n",c_dimids[i]);
      SCM dimid_smob;  
      SCM dimid_smob_1;
      struct ncid *nc_dimids;

      nc_dimids = (struct ncid *) scm_gc_malloc (sizeof (struct ncid), "nc-dimid");
      nc_dimids->fname = scm_from_locale_string ("nil");
      nc_dimids->idname = scm_from_locale_string ("dimid");
      nc_dimids->status = 0;
      nc_dimids->nc_id = 0;
      nc_dimids->update_func = SCM_BOOL_F;  

      SCM_NEWSMOB (dimid_smob, ncid_tag, nc_dimids);
      
      nc_dimids->fname = nc_ncid->fname;
      nc_dimids->status = status;
      nc_dimids->nc_id = c_dimids[i];
      
      dimid_smob_1 = scm_list_1 (dimid_smob);
      
      dimid_smobs = scm_append (scm_list_2 (dimid_smobs, dimid_smob_1));
    }
  }
  return dimid_smobs;
}

SCM
nc_inq_varnatts_wrapper (SCM ncid_smob, SCM varid_smob) 
{
  int c_ncid, status, c_varid, nattsp;
  struct ncid *nc_ncid, *nc_varid;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_INQ_VARATTS
  status = nc_inq_varnatts (c_ncid, c_varid, &nattsp);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  // RETURN number_atts
  return scm_from_int (nattsp);
}

//---get

SCM
nc_get_var1_text_wrapper (SCM ncid_smob, SCM varid_smob, SCM scm_index) 
{
  int c_ncid, c_varid, status, i;
  struct ncid *nc_ncid, *nc_varid;
  char *c_text;
  SCM s_element;
  SCM si_len = scm_length (scm_index);
  int csi_len = scm_to_int (si_len);
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  size_t c_index[csi_len];

  for (i=0; i<csi_len; i++) 
    {
      s_element = scm_list_ref (scm_index, scm_from_int (i));
      c_index[i] = scm_to_double (s_element);
    }

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  //c_text = (char *) malloc(55);  /* + 1 for trailing null */

  // NC_GET_VAR1_TEXT
  status = nc_get_var1_text (c_ncid, c_varid, c_index, c_text);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  return scm_from_locale_string (c_text);
}

SCM
nc_get_var1_uchar_wrapper (SCM ncid_smob, SCM varid_smob, SCM scm_index) 
{
  int c_ncid, c_varid, status, i;
  struct ncid *nc_ncid, *nc_varid;
  unsigned char up;
  SCM s_element;
  SCM si_len = scm_length (scm_index);
  int csi_len = scm_to_int (si_len);
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  size_t c_index[csi_len];

  for (i=0; i<csi_len; i++) 
    {
      s_element = scm_list_ref (scm_index, scm_from_int (i));
      c_index[i] = scm_to_double (s_element);
    }

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_GET_VAR1_UCHAR
  status = nc_get_var1_uchar (c_ncid, c_varid, c_index, &up);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }  
  return scm_from_uchar (up);
}

SCM
nc_get_var1_schar_wrapper (SCM ncid_smob, SCM varid_smob, SCM scm_index) 
{
  int c_ncid, c_varid, status, i;
  struct ncid *nc_ncid, *nc_varid;
  signed char cp;
  SCM s_element;
  SCM si_len = scm_length (scm_index);
  int csi_len = scm_to_int (si_len);
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  size_t c_index[csi_len];

  for (i=0; i<csi_len; i++) 
    {
      s_element = scm_list_ref( scm_index, scm_from_int (i));
      c_index[i] = scm_to_double (s_element);
    }

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_GET_VAR1_SCHAR
  status = nc_get_var1_schar (c_ncid, c_varid, c_index, &cp);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }  
  return scm_from_schar (cp);
}

SCM
nc_get_var1_short_wrapper (SCM ncid_smob, SCM varid_smob, SCM scm_index) 
{
  int c_ncid, c_varid, status, i;
  struct ncid *nc_ncid, *nc_varid;
  short sp;
  SCM s_element;
  SCM si_len = scm_length (scm_index);
  int csi_len = scm_to_int (si_len);
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  size_t c_index[csi_len];

  for (i=0; i<csi_len; i++) 
    {
      s_element = scm_list_ref (scm_index, scm_from_int (i));
      c_index[i] = scm_to_double (s_element);
    }

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_GET_VAR1_SHORT
  status = nc_get_var1_short (c_ncid, c_varid, c_index, &sp);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }  
  return scm_from_short (sp);
}

SCM
nc_get_var1_ushort_wrapper (SCM ncid_smob, SCM varid_smob, SCM scm_index) 
{
  int c_ncid, c_varid, status, i;
  struct ncid *nc_ncid, *nc_varid;
  ushort sp;
  SCM s_element;
  SCM si_len = scm_length (scm_index);
  int csi_len = scm_to_int (si_len);
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  size_t c_index[csi_len];

  for (i=0; i<csi_len; i++) 
    {
      s_element = scm_list_ref (scm_index, scm_from_int (i));
      c_index[i] = scm_to_double (s_element);
    }

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_GET_VAR1_SHORT
  status = nc_get_var1_ushort (c_ncid, c_varid, c_index, &sp);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }  
  return scm_from_ushort (sp);
}

SCM
nc_get_var1_int_wrapper (SCM ncid_smob, SCM varid_smob, SCM scm_index) 
{
  int c_ncid, c_varid, status, i;
  struct ncid *nc_ncid, *nc_varid;
  int ip;
  SCM s_element;
  SCM si_len = scm_length (scm_index);
  int csi_len = scm_to_int (si_len);
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  size_t c_index[csi_len];

  for (i=0; i<csi_len; i++) 
    {
      s_element = scm_list_ref (scm_index, scm_from_int (i));
      c_index[i] = scm_to_double (s_element);
    }

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_GET_VAR1_INT
  status = nc_get_var1_int (c_ncid, c_varid, c_index, &ip);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }  
  return scm_from_int (ip);
}

SCM
nc_get_var1_uint_wrapper (SCM ncid_smob, SCM varid_smob, SCM scm_index) 
{
  int c_ncid, c_varid, status, i;
  struct ncid *nc_ncid, *nc_varid;
  uint ip;
  SCM s_element;
  SCM si_len = scm_length (scm_index);
  int csi_len = scm_to_int (si_len);
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  size_t c_index[csi_len];

  for (i=0; i<csi_len; i++) 
    {
      s_element = scm_list_ref (scm_index, scm_from_int (i));
      c_index[i] = scm_to_double (s_element);
    }

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_GET_VAR1_INT
  status = nc_get_var1_uint (c_ncid, c_varid, c_index, &ip);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }  
  return scm_from_uint (ip);
}

SCM
nc_get_var1_long_wrapper (SCM ncid_smob, SCM varid_smob, SCM scm_index) 
{
  int c_ncid, c_varid, status, i;
  long lp;
  struct ncid *nc_ncid, *nc_varid;
  SCM s_element;
  SCM si_len = scm_length (scm_index);
  int csi_len = scm_to_int (si_len);
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  size_t c_index[csi_len];

  for (i=0; i<csi_len; i++) 
    {
      s_element = scm_list_ref (scm_index, scm_from_int (i));
      c_index[i] = scm_to_double (s_element);
    }

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_GET_VAR1_LONG
  status = nc_get_var1_long (c_ncid, c_varid, c_index, &lp);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }  
  return scm_from_long (lp);
}

SCM
nc_get_var1_float_wrapper (SCM ncid_smob, SCM varid_smob, SCM scm_index) 
{
  int c_ncid, c_varid, status, i;
  struct ncid *nc_ncid, *nc_varid;
  float c_float;
  SCM s_element;
  SCM si_len = scm_length (scm_index);
  int csi_len = scm_to_int (si_len);
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  size_t c_index[csi_len];

  for (i=0; i<csi_len; i++) 
    {
      s_element = scm_list_ref (scm_index, scm_from_int (i));
      c_index[i] = scm_to_double (s_element);
    }

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_GET_VAR1_FLOAT
  status = nc_get_var1_float (c_ncid, c_varid, c_index, &c_float);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
  }

  return scm_from_double (c_float);
}

SCM
nc_get_var1_double_wrapper (SCM ncid_smob, SCM varid_smob, SCM scm_index) 
{
  int c_ncid, c_varid, status, i;
  struct ncid *nc_ncid, *nc_varid;
  double dp;
  SCM s_element;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  if (scm_is_false (scm_list_p (scm_index)))
    {
      return SCM_BOOL_F;
    }

  SCM si_len = scm_length (scm_index);
  int csi_len = scm_to_int (si_len);

  size_t c_index[csi_len];

  for (i=0; i<csi_len; i++) 
    {
      s_element = scm_list_ref (scm_index, scm_from_int (i));
      c_index[i] = scm_to_double (s_element);
    }

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_GET_VAR1_DOUBLE
  status = nc_get_var1_double (c_ncid, c_varid, c_index, &dp);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }  

  return scm_from_double (dp);
}

SCM
nc_get_var1_wrapper (SCM ncid_smob, SCM varid_smob, SCM scm_index) 
{
  int c_ncid, c_varid, status, i;
  struct ncid *nc_ncid, *nc_varid;
  nc_type v_type;
  SCM s_element;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  if (scm_is_false (scm_list_p (scm_index)))
    {
      return SCM_BOOL_F;
    }

  SCM si_len = scm_length (scm_index);
  int csi_len = scm_to_int (si_len);

  size_t c_index[csi_len];

  for (i=0; i<csi_len; i++) 
    {
      s_element = scm_list_ref (scm_index, scm_from_int (i));
      c_index[i] = scm_to_double (s_element);
    }

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_INQ_ATT_TYPE
  status = nc_inq_vartype (c_ncid, c_varid, &v_type);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr,"%s\n", nc_strerror (status));
	}
    }

  if (v_type == NC_BYTE) 
    {
      unsigned char vp;

      // NC_GET_VAR1_CHAR
      status = nc_get_var1_uchar (c_ncid, c_varid, c_index, &vp);
      
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr,"%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}  
      else
	{
	  return scm_from_uchar (vp);
	}
    }
  else if (v_type == NC_CHAR) 
    {
      char *vp;
      // NC_GET_VAR1_CHAR
      status = nc_get_var1_text (c_ncid, c_varid, c_index, vp);
      
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr,"%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}  
      else
	{
	  return scm_from_locale_string (vp);
	}
    } 
  else if (v_type == NC_SHORT) 
    {
      short vp;

      // NC_GET_VAR1_CHAR
      status = nc_get_var1_short (c_ncid, c_varid, c_index, &vp);
      
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr,"%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}  
      else
	{
	  return scm_from_short(vp);
	}
    } 
  else if (v_type == NC_USHORT) 
    {
      ushort vp;

      // NC_GET_VAR1_CHAR
      status = nc_get_var1_ushort (c_ncid, c_varid, c_index, &vp);
      
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr,"%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}  
      else
	{
	  return scm_from_ushort(vp);
	}
    } 
  else if (v_type == NC_LONG) 
    {
      long vp;

      // NC_GET_VAR1_CHAR
      status = nc_get_var1_long (c_ncid, c_varid, c_index, &vp);
      
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr,"%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}  
      else
	{
	  return scm_from_long(vp);
	}
    } 
  else if (v_type == NC_INT) 
    {
      int vp;

      // NC_GET_VAR1_INT
      status = nc_get_var1_int (c_ncid, c_varid, c_index, &vp);
      
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr,"%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}  
      else
	{
	  return scm_from_int(vp);
	}
    }
  else if (v_type == NC_UINT) 
    {
      uint vp;

      // NC_GET_VAR1_INT
      status = nc_get_var1_uint (c_ncid, c_varid, c_index, &vp);
      
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr,"%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}  
      else
	{
	  return scm_from_uint(vp);
	}
    } 
  else if (v_type == NC_FLOAT) 
    {
      float vp;

      // NC_GET_VAR1_CHAR
      status = nc_get_var1_float (c_ncid, c_varid, c_index, &vp);
      
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr,"%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}  
      else
	{
	  return scm_from_double (vp);
	}
    } 
  else if (v_type == NC_DOUBLE) 
    {
      double vp;

      // NC_GET_VAR1_CHAR
      status = nc_get_var1_double (c_ncid, c_varid, c_index, &vp);
      
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}  
      else
	{
	  return scm_from_double (vp);
	}
    } 
  else 
    {
      return SCM_BOOL_F;
    }
}

//---get_var_
SCM
nc_get_var_int_wrapper (SCM ncid_smob, SCM varid_smob)
{
  int c_ncid, c_varid, status, i;
  struct ncid *nc_ncid, *nc_varid;
  //struct ncparray *scm_parray;
  size_t dim_length;
  int dimlen = 1;
  int c_dimids[NC_MAX_VAR_DIMS];
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;
  memset(c_dimids, -1, NC_MAX_VAR_DIMS * sizeof (int));

  // NC_INQ_VAR
  status = nc_inq_vardimid (c_ncid, c_varid, c_dimids);

  if (status != NC_NOERR)
    {
      fprintf (stderr, "%s\n", nc_strerror (status));
      return SCM_BOOL_F;
    }

  // DIMIDS

  for (i=0; i<(sizeof (c_dimids) / sizeof (int)); i++)
    {
      // DIMID SMOB
      if (c_dimids[i] > -1 && c_dimids[i] < NC_MAX_VAR_DIMS)
	{
	  status = nc_inq_dimlen (c_ncid, c_dimids[i], &dim_length);
	  if (status != NC_NOERR)
	    {
	      if (nc_verbose == SCM_BOOL_T)
		{
		  fprintf (stderr, "%s\n", nc_strerror (status));
		}
	    }
	  else
	    {
	      dimlen = dimlen*dim_length;
	    }
	}
    }

  // Declare array to hold the data
  int *ptr = (int *) malloc (dimlen * sizeof (int));
  if (ptr == NULL)
    {
      fprintf (stderr, "nc_guile.c: Failed to allocate memory.");
    }
  
  // NC_GET_VAR_INT
  status = nc_get_var_int (c_ncid, c_varid, ptr);
  if (status != NC_NOERR)
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  SCM scm_ncarray;
  SCM scm_len = scm_from_int (dimlen);

  scm_ncarray = scm_make_array (SCM_BOOL_F, scm_list_1 (scm_len));

  for (i = 0; i < dimlen; i++)
    {
      scm_array_set_x (scm_ncarray, scm_from_int (ptr[i]), scm_list_1 (scm_from_int(i)));
    }
  free (ptr);
  return scm_ncarray;
}

SCM
nc_get_var_long_wrapper (SCM ncid_smob, SCM varid_smob)
{
  int c_ncid, c_varid, status, i;
  struct ncid *nc_ncid, *nc_varid;
  //struct ncparray *scm_parray;
  size_t dim_length;
  int dimlen = 1;
  int c_dimids[NC_MAX_VAR_DIMS];
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;
  memset(c_dimids, -1, NC_MAX_VAR_DIMS * sizeof (int));

  // NC_INQ_VAR
  status = nc_inq_vardimid (c_ncid, c_varid, c_dimids);

  if (status != NC_NOERR)
    {
      fprintf (stderr, "%s\n", nc_strerror (status));
      return SCM_BOOL_F;
    }

  // DIMIDS

  for (i=0; i<(sizeof (c_dimids) / sizeof (int)); i++)
    {
      // DIMID SMOB
      if (c_dimids[i] > -1 && c_dimids[i] < NC_MAX_VAR_DIMS)
	{
	  status = nc_inq_dimlen (c_ncid, c_dimids[i], &dim_length);
	  if (status != NC_NOERR)
	    {
	      if (nc_verbose == SCM_BOOL_T)
		{
		  fprintf (stderr, "%s\n", nc_strerror (status));
		}
	    }
	  else
	    {
	      dimlen = dimlen*dim_length;
	    }
	}
    }

  // Declare array to hold the data
  long *ptr = (long *) malloc (dimlen * sizeof (long));
  if (ptr == NULL)
    {
      fprintf (stderr, "nc_guile.c: Failed to allocate memory.");
    }
  
  // NC_GET_VAR_INT
  status = nc_get_var_long (c_ncid, c_varid, ptr);
  if (status != NC_NOERR)
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  SCM scm_ncarray;
  SCM scm_len = scm_from_int (dimlen);

  scm_ncarray = scm_make_array (SCM_BOOL_F, scm_list_1 (scm_len));

  for (i = 0; i < dimlen; i++)
    {
      scm_array_set_x (scm_ncarray, scm_from_long (ptr[i]), scm_list_1 (scm_from_int(i)));
    }
  free (ptr);
  return scm_ncarray;
}

SCM
nc_get_var_short_wrapper (SCM ncid_smob, SCM varid_smob)
{
  int c_ncid, c_varid, status, i;
  struct ncid *nc_ncid, *nc_varid;
  //struct ncparray *scm_parray;
  size_t dim_length;
  int dimlen = 1;
  int c_dimids[NC_MAX_VAR_DIMS];
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;
  memset(c_dimids, -1, NC_MAX_VAR_DIMS * sizeof (int));

  // NC_INQ_VAR
  status = nc_inq_vardimid (c_ncid, c_varid, c_dimids);

  if (status != NC_NOERR)
    {
      fprintf (stderr, "%s\n", nc_strerror (status));
      return SCM_BOOL_F;
    }

  // DIMIDS

  for (i=0; i<(sizeof (c_dimids) / sizeof (int)); i++)
    {
      // DIMID SMOB
      if (c_dimids[i] > -1 && c_dimids[i] < NC_MAX_VAR_DIMS)
	{
	  status = nc_inq_dimlen (c_ncid, c_dimids[i], &dim_length);
	  if (status != NC_NOERR)
	    {
	      if (nc_verbose == SCM_BOOL_T)
		{
		  fprintf (stderr, "%s\n", nc_strerror (status));
		}
	    }
	  else
	    {
	      dimlen = dimlen*dim_length;
	    }
	}
    }

  // Declare array to hold the data
  short *ptr = (short *) malloc (dimlen * sizeof (short));
  if (ptr == NULL)
    {
      fprintf (stderr, "nc_guile.c: Failed to allocate memory.");
    }
  
  // NC_GET_VAR_INT
  status = nc_get_var_short (c_ncid, c_varid, ptr);
  if (status != NC_NOERR)
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  SCM scm_ncarray;
  SCM scm_len = scm_from_int (dimlen);

  scm_ncarray = scm_make_array (SCM_BOOL_F, scm_list_1 (scm_len));

  for (i = 0; i < dimlen; i++)
    {
      scm_array_set_x (scm_ncarray, scm_from_short (ptr[i]), scm_list_1 (scm_from_int(i)));
    }
  free (ptr);
  return scm_ncarray;
}

SCM
nc_get_var_double_wrapper (SCM ncid_smob, SCM varid_smob)
{
  int c_ncid, c_varid, status, i;
  struct ncid *nc_ncid, *nc_varid;
  //struct ncparray *scm_parray;
  size_t dim_length;
  int dimlen = 1;
  int c_dimids[NC_MAX_VAR_DIMS];
  //  SCM dimid_smobs = SCM_EOL;
  //SCM parray_smob;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;
  memset(c_dimids, -1, NC_MAX_VAR_DIMS * sizeof (int));

  // NC_INQ_VAR
  status = nc_inq_vardimid (c_ncid, c_varid, c_dimids);

  if (status != NC_NOERR)
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  // DIMIDS

  for (i=0; i<(sizeof (c_dimids) / sizeof (int)); i++)
    {
      // DIMID SMOB
      if (c_dimids[i] > -1 && c_dimids[i] < NC_MAX_VAR_DIMS)
	{
	  status = nc_inq_dimlen (c_ncid, c_dimids[i], &dim_length);
	  if (status != NC_NOERR)
	    {
	      if (nc_verbose == SCM_BOOL_T)
		{
		  fprintf (stderr, "%s\n", nc_strerror (status));
		}
	    }
	  else
	    {
	      dimlen = dimlen * dim_length;
	    }
	}
    }

  // Declare array to hold the data

  double *dptr = (double *) malloc (dimlen * sizeof (double));
  if (dptr == NULL)
    {
      fprintf (stderr, "nc_guile.c: Failed to allocate memory.");
    }

  // NC_GET_VAR_DOUBLE
  status = nc_get_var_double (c_ncid, c_varid, dptr);

  if (status != NC_NOERR)
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  SCM scm_ncarray;
  SCM scm_len = scm_from_int (dimlen);

  scm_ncarray = scm_make_array (SCM_BOOL_F, scm_list_1 (scm_len));

  for (i = 0; i < dimlen; i++)
    {
      scm_array_set_x (scm_ncarray, scm_from_double (dptr[i]), scm_list_1 (scm_from_int(i)));
    }
  free (dptr);
  return scm_ncarray;
}

SCM
nc_get_var_float_wrapper (SCM ncid_smob, SCM varid_smob)
{
  int c_ncid, c_varid, status, i;
  struct ncid *nc_ncid, *nc_varid;
  //struct ncparray *scm_parray;
  size_t dim_length;
  int dimlen = 1;
  int c_dimids[NC_MAX_VAR_DIMS];
  //  SCM dimid_smobs = SCM_EOL;
  //SCM parray_smob;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;
  memset(c_dimids, -1, NC_MAX_VAR_DIMS * sizeof (int));

  // NC_INQ_VAR
  status = nc_inq_vardimid (c_ncid, c_varid, c_dimids);

  if (status != NC_NOERR)
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  // DIMIDS

  for (i=0; i<(sizeof (c_dimids) / sizeof (int)); i++)
    {
      // DIMID SMOB
      if (c_dimids[i] > -1 && c_dimids[i] < NC_MAX_VAR_DIMS)
	{
	  status = nc_inq_dimlen (c_ncid, c_dimids[i], &dim_length);
	  if (status != NC_NOERR)
	    {
	      if (nc_verbose == SCM_BOOL_T)
		{
		  fprintf (stderr, "%s\n", nc_strerror (status));
		}
	    }
	  else
	    {
	      dimlen = dimlen * dim_length;
	    }
	}
    }

  // Declare array to hold the data

  float *fptr = (float *) malloc (dimlen * sizeof (float));
  if (fptr == NULL)
    {
      fprintf (stderr, "nc_guile.c: Failed to allocate memory.");
    }

  // NC_GET_VAR_FLOAT
  status = nc_get_var_float (c_ncid, c_varid, fptr);

  if (status != NC_NOERR)
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  SCM scm_ncarray;
  SCM scm_len = scm_from_int (dimlen);

  scm_ncarray = scm_make_array (SCM_BOOL_F, scm_list_1 (scm_len));

  for (i = 0; i < dimlen; i++)
    {
      scm_array_set_x (scm_ncarray, scm_from_double (fptr[i]), scm_list_1 (scm_from_int(i)));
    }
  free(fptr);
  return scm_ncarray;
}

SCM
nc_get_var_wrapper (SCM ncid_smob, SCM varid_smob)
{
  int c_ncid, c_varid, status, i;
  struct ncid *nc_ncid, *nc_varid;
  //struct ncparray *scm_parray;
  size_t dim_length;
  int dimlen = 1;
  int c_dimids[NC_MAX_VAR_DIMS];
  nc_type v_type;
  //  SCM dimid_smobs = SCM_EOL;
  //SCM parray_smob;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;
  memset(c_dimids, -1, NC_MAX_VAR_DIMS * sizeof (int));

  // NC_INQ_VAR
  status = nc_inq_vardimid (c_ncid, c_varid, c_dimids);

  if (status != NC_NOERR)
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  // DIMIDS

  for (i=0; i<(sizeof (c_dimids) / sizeof (int)); i++)
    {
      // DIMID SMOB
      if (c_dimids[i] > -1 && c_dimids[i] < NC_MAX_VAR_DIMS)
	{
	  status = nc_inq_dimlen (c_ncid, c_dimids[i], &dim_length);
	  if (status != NC_NOERR)
	    {
	      if (nc_verbose == SCM_BOOL_T)
		{
		  fprintf (stderr, "%s\n", nc_strerror (status));
		}
	    }
	  else
	    {
	      dimlen = dimlen * dim_length;
	    }
	}
    }

  // NC_INQ_ATT_TYPE
  status = nc_inq_vartype (c_ncid, c_varid, &v_type);

  if (status != NC_NOERR)
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
    }

  if (v_type == NC_FLOAT) 
    {

      // Declare array to hold the data

      float *fptr = (float *) malloc (dimlen * sizeof (float));
      if (fptr == NULL)
	{
	  fprintf (stderr, "nc_guile.c: Failed to allocate memory.");
	}

      // NC_GET_VAR_FLOAT
      status = nc_get_var_float (c_ncid, c_varid, fptr);

      if (status != NC_NOERR)
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}

      SCM scm_ncarray;
      SCM scm_len = scm_from_int (dimlen);

      scm_ncarray = scm_make_array (SCM_BOOL_F, scm_list_1 (scm_len));

      for (i = 0; i < dimlen; i++)
	{
	  scm_array_set_x (scm_ncarray, scm_from_double (fptr[i]), scm_list_1 (scm_from_int(i)));
	}
      free(fptr);
      return scm_ncarray;
    }
  else if (v_type == NC_DOUBLE)
    {
      double *dptr = (double *) malloc (dimlen * sizeof (double));
      if (dptr == NULL)
	{
	  fprintf (stderr, "nc_guile.c: Failed to allocate memory.");
	}
      
      // NC_GET_VAR_DOUBLE
      status = nc_get_var_double (c_ncid, c_varid, dptr);
      
      if (status != NC_NOERR)
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}
      
      SCM scm_ncarray;
      SCM scm_len = scm_from_int (dimlen);
      
      scm_ncarray = scm_make_array (SCM_BOOL_F, scm_list_1 (scm_len));
      
      for (i = 0; i < dimlen; i++)
	{
	  scm_array_set_x (scm_ncarray, scm_from_double (dptr[i]), scm_list_1 (scm_from_int(i)));
	}
      free (dptr);
      return scm_ncarray;
    }
  else if (v_type == NC_INT)
    {
      int *ptr = (int *) malloc (dimlen * sizeof (int));
      if (ptr == NULL)
	{
	  fprintf (stderr, "nc_guile.c: Failed to allocate memory.");
	}
      
      // NC_GET_VAR_INT
      status = nc_get_var_int (c_ncid, c_varid, ptr);
      if (status != NC_NOERR)
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}

      SCM scm_ncarray;
      SCM scm_len = scm_from_int (dimlen);
      
      scm_ncarray = scm_make_array (SCM_BOOL_F, scm_list_1 (scm_len));
      
      for (i = 0; i < dimlen; i++)
	{
	  scm_array_set_x (scm_ncarray, scm_from_int (ptr[i]), scm_list_1 (scm_from_int(i)));
	}
      free (ptr);
      return scm_ncarray;
    }
  else if (v_type == NC_SHORT)
    {
      short *ptr = (short *) malloc (dimlen * sizeof (short));
      if (ptr == NULL)
	{
	  fprintf (stderr, "nc_guile.c: Failed to allocate memory.");
	}
      
      // NC_GET_VAR_INT
      status = nc_get_var_short (c_ncid, c_varid, ptr);
      if (status != NC_NOERR)
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}

      SCM scm_ncarray;
      SCM scm_len = scm_from_int (dimlen);
      
      scm_ncarray = scm_make_array (SCM_BOOL_F, scm_list_1 (scm_len));
      
      for (i = 0; i < dimlen; i++)
	{
	  scm_array_set_x (scm_ncarray, scm_from_short (ptr[i]), scm_list_1 (scm_from_int(i)));
	}
      free (ptr);
      return scm_ncarray;
    }
  else if (v_type == NC_LONG)
    {
      long *ptr = (long *) malloc (dimlen * sizeof (long));
      if (ptr == NULL)
	{
	  fprintf (stderr, "nc_guile.c: Failed to allocate memory.");
	}
      
      // NC_GET_VAR_LONG
      status = nc_get_var_long (c_ncid, c_varid, ptr);
      if (status != NC_NOERR)
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}

      SCM scm_ncarray;
      SCM scm_len = scm_from_int (dimlen);
      
      scm_ncarray = scm_make_array (SCM_BOOL_F, scm_list_1 (scm_len));
      
      for (i = 0; i < dimlen; i++)
	{
	  scm_array_set_x (scm_ncarray, scm_from_long (ptr[i]), scm_list_1 (scm_from_int(i)));
	}
      free (ptr);
      return scm_ncarray;
    }
  else if (v_type == NC_BYTE)
    {
      unsigned char *ptr = (unsigned char *) malloc (dimlen * sizeof (unsigned char));
      if (ptr == NULL)
	{
	  fprintf (stderr, "nc_guile.c: Failed to allocate memory.");
	}
      
      // NC_GET_VAR_LONG
      status = nc_get_var_uchar (c_ncid, c_varid, ptr);
      if (status != NC_NOERR)
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}

      SCM scm_ncarray;
      SCM scm_len = scm_from_int (dimlen);
      
      scm_ncarray = scm_make_array (SCM_BOOL_F, scm_list_1 (scm_len));
      
      for (i = 0; i < dimlen; i++)
	{
	  scm_array_set_x (scm_ncarray, scm_from_uchar (ptr[i]), scm_list_1 (scm_from_int(i)));
	}
      free (ptr);
      return scm_ncarray;
    }
  /* else if (v_type == NC_CHAR) */
  /*   { */
  /*     char *ptr = (char *) malloc (dimlen * sizeof (char)); */
  /*     if (ptr == NULL) */
  /* 	{ */
  /* 	  fprintf (stderr, "nc_guile.c: Failed to allocate memory."); */
  /* 	} */
      
  /*     // NC_GET_VAR_LONG */
  /*     status = nc_get_var_text (c_ncid, c_varid, ptr); */
  /*     if (status != NC_NOERR) */
  /* 	{ */
  /* 	  if (nc_verbose == SCM_BOOL_T) */
  /* 	    { */
  /* 	      fprintf (stderr, "%s\n", nc_strerror (status)); */
  /* 	    } */
  /* 	  return SCM_BOOL_F; */
  /* 	} */

  /*     SCM scm_ncarray; */
  /*     SCM scm_len = scm_from_int (dimlen); */
      
  /*     scm_ncarray = scm_make_array (SCM_BOOL_F, scm_list_1 (scm_len)); */
      
  /*     for (i = 0; i < dimlen; i++) */
  /* 	{ */
  /* 	  scm_array_set_x (scm_ncarray, scm_from_locale_string (ptr[i]), scm_list_1 (scm_from_int(i))); */
  /* 	} */
  /*     free (ptr); */
  /*     return scm_ncarray; */
  /*   } */
  else 
    {
      return SCM_BOOL_F;
    }
}

SCM
nc_scan_var_range_wrapper (SCM ncid_smob, SCM varid_smob)
{
  int c_ncid, c_varid, status, i;
  struct ncid *nc_ncid, *nc_varid;
  //struct ncparray *scm_parray;
  ssize_t dim_length;
  unsigned long dimlen = 1;
  int c_dimids[NC_MAX_VAR_DIMS];
  nc_type v_type;
  //  SCM dimid_smobs = SCM_EOL;
  //SCM parray_smob;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;
  memset(c_dimids, -1, NC_MAX_VAR_DIMS * sizeof (int));

  // NC_INQ_VAR
  status = nc_inq_vardimid (c_ncid, c_varid, c_dimids);

  if (status != NC_NOERR)
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  // DIMIDS

  for (i=0; i<(sizeof (c_dimids) / sizeof (int)); i++)
    {
      // DIMID SMOB
      if (c_dimids[i] > -1 && c_dimids[i] < NC_MAX_VAR_DIMS)
	{
	  status = nc_inq_dimlen (c_ncid, c_dimids[i], &dim_length);
	  if (status != NC_NOERR)
	    {
	      if (nc_verbose == SCM_BOOL_T)
		{
		  fprintf (stderr, "%s\n", nc_strerror (status));
		}
	    }
	  else
	    {
	      dimlen = dimlen * dim_length;
	    }
	}
    }

  // NC_INQ_ATT_TYPE
  status = nc_inq_vartype (c_ncid, c_varid, &v_type);

  if (status != NC_NOERR)
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
    }

  if (v_type == NC_FLOAT) 
    {

      float high_var;
      float low_var;

      // Declare array to hold the data

      float *fptr = (float *) malloc (dimlen * sizeof (float));
      if (fptr == NULL)
	{
	  fprintf (stderr, "nc_guile.c: Failed to allocate memory.");
	  exit(EXIT_FAILURE);
	}

      // NC_GET_VAR_FLOAT
      status = nc_get_var_float (c_ncid, c_varid, fptr);

      if (status != NC_NOERR)
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}

      /* scm_ncarray = scm_make_array (SCM_BOOL_F, scm_list_1 (scm_len)); */

      for (i = 0; i < dimlen; i++)
	{
	  if (i == 0)
	    {
	      high_var = fptr[i];
	      low_var = fptr[i];
	    }
	  else
	    {
	      if (high_var < fptr[i])
		{
		  high_var = fptr[i];
		}
	      if (low_var > fptr[i])
		{
		  low_var = fptr[i];
		}
	    }
	}

      free(fptr);
      return scm_list_2 (scm_from_double(low_var), scm_from_double(high_var));
    }
  else if (v_type == NC_DOUBLE)
    {

      double high_var;
      double low_var;

      double *dptr = (double *) malloc (dimlen * sizeof (double));
      if (dptr == NULL)
	{
	  fprintf (stderr, "nc_guile.c: Failed to allocate memory.");
	  exit(EXIT_FAILURE);
	}
      
      // NC_GET_VAR_DOUBLE
      status = nc_get_var_double (c_ncid, c_varid, dptr);
      
      if (status != NC_NOERR)
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}

      for (i = 0; i < dimlen; i++)
	{
	  if (i == 0)
	    {
	      high_var = dptr[i];
	      low_var = dptr[i];
	    }
	  else
	    {
	      if (high_var < dptr[i])
		{
		  high_var = dptr[i];
		}
	      if (low_var > dptr[i])
		{
		  low_var = dptr[i];
		}
	    }
	}

      free(dptr);
      return scm_list_2 (scm_from_double(low_var), scm_from_double(high_var));
    }
  else if (v_type == NC_INT)
    {

      int high_var;
      int low_var;

      int* ptr;
      ptr = (int*) malloc (dimlen * sizeof (int*));

      if (ptr == NULL)
	{
	  fprintf (stderr, "nc_guile.c: Failed to allocate memory.\n");
	  exit(EXIT_FAILURE);
	}
      
      // NC_GET_VAR_INT
      status = nc_get_var_int (c_ncid, c_varid, ptr);
      if (status != NC_NOERR)
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}

      for (i = 0; i < dimlen; i++)
	{
	  if (i == 0)
	    {
	      high_var = ptr[i];
	      low_var = ptr[i];
	    }
	  else
	    {
	      if (high_var < ptr[i])
		{
		  high_var = ptr[i];
		}
	      if (low_var > ptr[i])
		{
		  low_var = ptr[i];
		}
	    }
	}

      free(ptr);
      return scm_list_2 (scm_from_int(low_var), scm_from_int(high_var));
    }
  else if (v_type == NC_UINT)
    {

      uint high_var;
      uint low_var;

      uint* ptr;
      ptr = (uint*) malloc (dimlen * sizeof (uint*));

      if (ptr == NULL)
	{
	  fprintf (stderr, "nc_guile.c: Failed to allocate memory.\n");
	  exit(EXIT_FAILURE);
	}
      
      // NC_GET_VAR_INT
      status = nc_get_var_uint (c_ncid, c_varid, ptr);
      if (status != NC_NOERR)
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}

      for (i = 0; i < dimlen; i++)
	{
	  if (i == 0)
	    {
	      high_var = ptr[i];
	      low_var = ptr[i];
	    }
	  else
	    {
	      if (high_var < ptr[i])
		{
		  high_var = ptr[i];
		}
	      if (low_var > ptr[i])
		{
		  low_var = ptr[i];
		}
	    }
	}

      free(ptr);
      return scm_list_2 (scm_from_uint(low_var), scm_from_uint(high_var));
    }
  else if (v_type == NC_SHORT)
    {

      short high_var;
      short low_var;

      short *ptr = (short *) malloc (dimlen * sizeof (short));
      if (ptr == NULL)
	{
	  fprintf (stderr, "nc_guile.c: Failed to allocate memory.");
	  exit(EXIT_FAILURE);
	}
      
      // NC_GET_VAR_INT
      status = nc_get_var_short (c_ncid, c_varid, ptr);
      if (status != NC_NOERR)
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}

      for (i = 0; i < dimlen; i++)
	{
	  if (i == 0)
	    {
	      high_var = ptr[i];
	      low_var = ptr[i];
	    }
	  else
	    {
	      if (high_var < ptr[i])
		{
		  high_var = ptr[i];
		}
	      if (low_var > ptr[i])
		{
		  low_var = ptr[i];
		}
	    }
	}

      free(ptr);
      return scm_list_2 (scm_from_short(low_var), scm_from_short(high_var));
    }
  else if (v_type == NC_USHORT)
    {

      ushort high_var;
      ushort low_var;

      ushort *ptr = (ushort *) malloc (dimlen * sizeof (ushort));
      if (ptr == NULL)
	{
	  fprintf (stderr, "nc_guile.c: Failed to allocate memory.");
	  exit(EXIT_FAILURE);
	}
      
      // NC_GET_VAR_INT
      status = nc_get_var_ushort (c_ncid, c_varid, ptr);
      if (status != NC_NOERR)
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}

      for (i = 0; i < dimlen; i++)
	{
	  if (i == 0)
	    {
	      high_var = ptr[i];
	      low_var = ptr[i];
	    }
	  else
	    {
	      if (high_var < ptr[i])
		{
		  high_var = ptr[i];
		}
	      if (low_var > ptr[i])
		{
		  low_var = ptr[i];
		}
	    }
	}

      free(ptr);
      return scm_list_2 (scm_from_ushort(low_var), scm_from_ushort(high_var));
    }
  else if (v_type == NC_LONG)
    {

      long high_var;
      long low_var;

      long *ptr = (long *) malloc (dimlen * sizeof (long));
      if (ptr == NULL)
	{
	  fprintf (stderr, "nc_guile.c: Failed to allocate memory.");
	  exit(EXIT_FAILURE);
	}
      
      // NC_GET_VAR_LONG
      status = nc_get_var_long (c_ncid, c_varid, ptr);
      if (status != NC_NOERR)
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}

      for (i = 0; i < dimlen; i++)
	{
	  if (i == 0)
	    {
	      high_var = ptr[i];
	      low_var = ptr[i];
	    }
	  else
	    {
	      if (high_var < ptr[i])
		{
		  high_var = ptr[i];
		}
	      if (low_var > ptr[i])
		{
		  low_var = ptr[i];
		}
	    }
	}

      free(ptr);
      return scm_list_2 (scm_from_double(low_var), scm_from_double(high_var));
    }
  else if (v_type == NC_BYTE)
    {

      unsigned char high_var;
      unsigned char low_var;

      unsigned char *ptr = (unsigned char *) malloc (dimlen * sizeof (unsigned char));
      if (ptr == NULL)
	{
	  fprintf (stderr, "nc_guile.c: Failed to allocate memory.");
	  exit(EXIT_FAILURE);
	}
      
      // NC_GET_VAR_LONG
      status = nc_get_var_uchar (c_ncid, c_varid, ptr);
      if (status != NC_NOERR)
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}

      for (i = 0; i < dimlen; i++)
	{
	  if (i == 0)
	    {
	      high_var = ptr[i];
	      low_var = ptr[i];
	    }
	  else
	    {
	      if (high_var < ptr[i])
		{
		  high_var = ptr[i];
		}
	      if (low_var > ptr[i])
		{
		  low_var = ptr[i];
		}
	    }
	}

      free(ptr);
      return scm_list_2 (scm_from_double(low_var), scm_from_double(high_var));
    }
  else 
    {
      return SCM_BOOL_F;
    }
}

/* ------------*/
/* nc-put-var1 */

SCM
nc_put_var1_wrapper (SCM ncid_smob, SCM varid_smob, SCM scm_index,  SCM scm_val)
{
  int c_ncid, c_varid, status, i;  
  struct ncid *nc_ncid, *nc_varid;
  nc_type v_type;
  SCM s_element;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  if (scm_is_false (scm_list_p (scm_index)))
    {
      return SCM_BOOL_F;
    }

  SCM si_len = scm_length (scm_index);
  int csi_len = scm_to_int (si_len);

  size_t c_index[csi_len];

  for (i=0; i<csi_len; i++) 
    {
      s_element = scm_list_ref (scm_index, scm_from_int (i));
      c_index[i] = scm_to_double (s_element);
    }

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_INQ_VAR_TYPE
  status = nc_inq_vartype (c_ncid, c_varid, &v_type);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
    }

  if (v_type == NC_DOUBLE) 
    {
      static double vp;
      vp = scm_to_double(scm_val);
      status = nc_put_var1_double (c_ncid, c_varid, c_index, &vp);
      
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}  
      else
	{
	  return SCM_BOOL_T;
	}
    }
  else if (v_type == NC_FLOAT)
    {
      static float vp;
      vp = scm_to_double(scm_val);
      status = nc_put_var1_float (c_ncid, c_varid, c_index, &vp);
      
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}  
      else
	{
	  return SCM_BOOL_T;
	}
    }
  else if (v_type == NC_INT) 
    {
      static int vp;
      vp = scm_to_int(scm_val);
      status = nc_put_var1_int (c_ncid, c_varid, c_index, &vp);
      
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}  
      else
	{
	  return SCM_BOOL_T;
	}
    }
  else if (v_type == NC_LONG) 
    {
      static long vp;
      vp = scm_to_long(scm_val);
      status = nc_put_var1_long (c_ncid, c_varid, c_index, &vp);
      
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}  
      else
	{
	  return SCM_BOOL_T;
	}
    }
  else if (v_type == NC_SHORT) 
    {
      static short vp;
      vp = scm_to_short(scm_val);
      status = nc_put_var1_short (c_ncid, c_varid, c_index, &vp);
      
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}  
      else
	{
	  return SCM_BOOL_T;
	}
    }
  else if (v_type == NC_CHAR) 
    {
      static char *vp;
      size_t length;
      vp = scm_to_locale_string (scm_val);
      status = nc_put_var1_text (c_ncid, c_varid, c_index, vp);
      
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}  
      else
	{
	  return SCM_BOOL_T;
	}
    }
  else if (v_type == NC_BYTE) 
    {
      static unsigned char vp;
      vp = scm_to_char (scm_val);
      status = nc_put_var1_uchar (c_ncid, c_varid, c_index, &vp);
      
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	  return SCM_BOOL_F;
	}  
      else
	{
	  return SCM_BOOL_T;
	}
    }
  else 
    {
      return SCM_BOOL_F;
    }
}

SCM
nc_put_var1_double_wrapper (SCM ncid_smob, SCM varid_smob, SCM scm_index,  SCM scm_val)
{
  int c_ncid, c_varid, status, i;  
  struct ncid *nc_ncid, *nc_varid;
  SCM s_element;
  static double vp;
  vp = scm_to_double(scm_val);
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  if (scm_is_false (scm_list_p (scm_index)))
    {
      return SCM_BOOL_F;
    }

  SCM si_len = scm_length (scm_index);
  int csi_len = scm_to_int (si_len);

  size_t c_index[csi_len];

  for (i=0; i<csi_len; i++) 
    {
      s_element = scm_list_ref (scm_index, scm_from_int (i));
      c_index[i] = scm_to_double (s_element);
    }

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  status = nc_put_var1_double (c_ncid, c_varid, c_index, &vp);
  
  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }  
  else
    {
      return SCM_BOOL_T;
    } 
}

SCM
nc_put_var1_float_wrapper (SCM ncid_smob, SCM varid_smob, SCM scm_index,  SCM scm_val)
{
  int c_ncid, c_varid, status, i;  
  struct ncid *nc_ncid, *nc_varid;
  SCM s_element;
  static float vp;
  vp = scm_to_double(scm_val);
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  if (scm_is_false (scm_list_p (scm_index)))
    {
      return SCM_BOOL_F;
    }

  SCM si_len = scm_length (scm_index);
  int csi_len = scm_to_int (si_len);

  size_t c_index[csi_len];

  for (i=0; i<csi_len; i++) 
    {
      s_element = scm_list_ref (scm_index, scm_from_int (i));
      c_index[i] = scm_to_double (s_element);
    }

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  status = nc_put_var1_float (c_ncid, c_varid, c_index, &vp);
  
  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }  
  else
    {
      return SCM_BOOL_T;
    } 
}

SCM
nc_put_var1_int_wrapper (SCM ncid_smob, SCM varid_smob, SCM scm_index,  SCM scm_val)
{
  int c_ncid, c_varid, status, i;  
  struct ncid *nc_ncid, *nc_varid;
  SCM s_element;
  static int vp;
  vp = scm_to_int(scm_val);
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  if (scm_is_false (scm_list_p (scm_index)))
    {
      return SCM_BOOL_F;
    }

  SCM si_len = scm_length (scm_index);
  int csi_len = scm_to_int (si_len);

  size_t c_index[csi_len];

  for (i=0; i<csi_len; i++) 
    {
      s_element = scm_list_ref (scm_index, scm_from_int (i));
      c_index[i] = scm_to_double (s_element);
    }

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  status = nc_put_var1_int (c_ncid, c_varid, c_index, &vp);
  
  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }  
  else
    {
      return SCM_BOOL_T;
    } 
}

SCM
nc_put_var1_short_wrapper (SCM ncid_smob, SCM varid_smob, SCM scm_index,  SCM scm_val)
{
  int c_ncid, c_varid, status, i;  
  struct ncid *nc_ncid, *nc_varid;
  SCM s_element;
  static short vp;
  vp = scm_to_short (scm_val);
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  if (scm_is_false (scm_list_p (scm_index)))
    {
      return SCM_BOOL_F;
    }

  SCM si_len = scm_length (scm_index);
  int csi_len = scm_to_int (si_len);

  size_t c_index[csi_len];

  for (i=0; i<csi_len; i++) 
    {
      s_element = scm_list_ref (scm_index, scm_from_int (i));
      c_index[i] = scm_to_double (s_element);
    }

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  status = nc_put_var1_short (c_ncid, c_varid, c_index, &vp);
  
  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }  
  else
    {
      return SCM_BOOL_T;
    } 
}

SCM
nc_put_var1_long_wrapper (SCM ncid_smob, SCM varid_smob, SCM scm_index,  SCM scm_val)
{
  int c_ncid, c_varid, status, i;  
  struct ncid *nc_ncid, *nc_varid;
  SCM s_element;
  static long vp;
  vp = scm_to_long (scm_val);
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  if (scm_is_false (scm_list_p (scm_index)))
    {
      return SCM_BOOL_F;
    }

  SCM si_len = scm_length (scm_index);
  int csi_len = scm_to_int (si_len);

  size_t c_index[csi_len];

  for (i=0; i<csi_len; i++) 
    {
      s_element = scm_list_ref (scm_index, scm_from_int (i));
      c_index[i] = scm_to_double (s_element);
    }

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  status = nc_put_var1_long (c_ncid, c_varid, c_index, &vp);
  
  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }  
  else
    {
      return SCM_BOOL_T;
    } 
}

/* nc_rename_var */

SCM
nc_rename_var_wrapper (SCM ncid_smob, SCM varid_smob, SCM var_name)
{
  int c_ncid, c_varid, status, i;
  struct ncid *nc_ncid, *nc_varid;
  char *c_var_name;
  size_t length;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  c_var_name = scm_to_locale_string (var_name);

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // RENAME
  status = nc_rename_var (c_ncid, c_varid, c_var_name);
  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }  
  else
    {
      return SCM_BOOL_T;
    }
}

/*----------
  Attributes
----------*/

SCM
nc_inq_att_wrapper (SCM ncid_smob, SCM varid_smob, SCM att_name) 
{
  int c_varid, c_ncid, status;
  char *c_att_name;
  nc_type nc_att_type;
  size_t nc_att_len, length;
  struct nctype *scm_nctype;
  struct ncid *nc_ncid, *nc_varid;
  SCM scm_typename, scm_typeshortname, nc_scm_type;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // ATTRIBUTE NAME
  c_att_name = scm_to_locale_string (att_name);

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;
  
  // ATTID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;
  
  // NC_INQ_ATT
  status = nc_inq_att (c_ncid, c_varid, c_att_name, &nc_att_type, &nc_att_len);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  // NC_TYPE
  scm_nctype = (struct nctype *) scm_gc_malloc (sizeof (struct nctype), "nctype");

  scm_nctype->typename = scm_from_locale_string ("");
  scm_nctype->status = 0;
  scm_nctype->c_nc_type = -1;
  scm_nctype->update_func = SCM_BOOL_F;

  if (nc_att_type == NC_BYTE) 
    {
      scm_typename = scm_from_locale_string ("NC_BYTE");
      scm_typeshortname = scm_from_locale_string ("byte");
    } 
  else if (nc_att_type == NC_CHAR) 
    {
      scm_typename = scm_from_locale_string ("NC_CHAR");
      scm_typeshortname = scm_from_locale_string ("char");
    } 
  else if (nc_att_type == NC_SHORT) 
    {
      scm_typename = scm_from_locale_string ("NC_SHORT");
      scm_typeshortname = scm_from_locale_string ("short");
    } 
  else if (nc_att_type == NC_INT) 
    {
      scm_typename = scm_from_locale_string ("NC_INT");
      scm_typeshortname = scm_from_locale_string ("int");
    } 
  else if (nc_att_type == NC_FLOAT) 
    {
      scm_typename = scm_from_locale_string ("NC_FLOAT");
      scm_typeshortname = scm_from_locale_string ("float");
    } 
  else if (nc_att_type == NC_DOUBLE) 
    {
      scm_typename = scm_from_locale_string ("NC_DOUBLE");
      scm_typeshortname = scm_from_locale_string ("double");
    } 
  else if (nc_att_type == NC_STRING) 
    {
      scm_typename = scm_from_locale_string ("NC_STRING");
      scm_typeshortname = scm_from_locale_string ("string");
    } 
  else 
    {
      scm_typename = scm_from_locale_string ("UNKNOWN");
      scm_typeshortname = scm_from_locale_string ("unk");
    }

  SCM_NEWSMOB (nc_scm_type, nctype_tag, scm_nctype);

  scm_nctype->typename = scm_typename;
  scm_nctype->typeshortname = scm_typeshortname;
  scm_nctype->status = status;
  scm_nctype->c_nc_type = nc_att_type;

  //free_nctype (scm_nctype);

  //nc_attid->idname = scm_string_append(scm_list_2(att_name, scm_from_locale_string("_attid")));

  return scm_list_2 (nc_scm_type, scm_from_size_t (nc_att_len));
}

SCM
nc_inq_atttype_wrapper (SCM ncid_smob, SCM varid_smob, SCM att_name) 
{
  int c_varid, c_ncid, status;
  char *c_att_name;
  nc_type nc_att_type;
  size_t length;
  struct nctype *scm_nctype;
  struct ncid *nc_ncid, *nc_varid;
  SCM scm_typename, nc_scm_type, scm_typeshortname;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // ATTRIBUTE NAME
  c_att_name = scm_to_locale_string (att_name);

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;
  
  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_INQ_ATTTYPE
  status = nc_inq_atttype (c_ncid, c_varid, c_att_name, &nc_att_type);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  // NC_TYPE
  scm_nctype = (struct nctype *) scm_gc_malloc (sizeof (struct nctype), "nctype");

  scm_nctype->typename = scm_from_locale_string ("");
  scm_nctype->status = 0;
  scm_nctype->c_nc_type = -1;
  scm_nctype->update_func = SCM_BOOL_F;

  if (nc_att_type == NC_BYTE) 
    {
      scm_typename = scm_from_locale_string ("NC_BYTE");
      scm_typeshortname = scm_from_locale_string ("byte");
    } 
  else if (nc_att_type == NC_CHAR) 
    {
      scm_typename = scm_from_locale_string ("NC_CHAR");
      scm_typeshortname = scm_from_locale_string ("char");
    } 
  else if (nc_att_type == NC_SHORT) 
    {
      scm_typename = scm_from_locale_string ("NC_SHORT");
      scm_typeshortname = scm_from_locale_string ("short");
    } 
  else if (nc_att_type == NC_INT) 
    {
      scm_typename = scm_from_locale_string ("NC_INT");
      scm_typeshortname = scm_from_locale_string ("int");
    } 
  else if (nc_att_type == NC_FLOAT) 
    {
      scm_typename = scm_from_locale_string ("NC_FLOAT");
      scm_typeshortname = scm_from_locale_string ("float");
    } 
  else if (nc_att_type == NC_DOUBLE) 
    {
      scm_typename = scm_from_locale_string ("NC_DOUBLE");
      scm_typeshortname = scm_from_locale_string ("double");
    } 
  else if (nc_att_type == NC_STRING) 
    {
      scm_typename = scm_from_locale_string ("NC_STRING");
      scm_typeshortname = scm_from_locale_string ("string");
    } 
  else 
    {
      scm_typename = scm_from_locale_string ("UNKNOWN");
      scm_typeshortname = scm_from_locale_string ("unk");
    }

  SCM_NEWSMOB (nc_scm_type, nctype_tag, scm_nctype);

  scm_nctype->typename = scm_typename;
  scm_nctype->typeshortname = scm_typeshortname;
  scm_nctype->status = status;
  scm_nctype->c_nc_type = nc_att_type;

  return nc_scm_type;
}

SCM
nc_inq_attlen_wrapper (SCM ncid_smob, SCM varid_smob, SCM att_name) 
{
  int c_varid, c_ncid, status;
  char *c_att_name;
  size_t nc_att_len, length;
  struct ncid *nc_ncid, *nc_varid;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // ATTRIBUTE NAME
  c_att_name = scm_to_locale_string (att_name);

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_INQ_ATT
  status = nc_inq_attlen (c_ncid, c_varid, c_att_name, &nc_att_len);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }
  return scm_from_size_t(nc_att_len);
}

SCM
nc_inq_attname_wrapper (SCM ncid_smob, SCM varid_smob, SCM scm_attnum) 
{
  int c_varid, c_ncid, status, attnum;
  char *c_att_name;
  struct ncid *nc_ncid, *nc_varid;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // ATTRIBUTE NAME
  //c_att_name = scm_to_locale_string(satt_name);

  attnum = scm_to_int (scm_attnum);

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  c_att_name = (char *) malloc(NC_MAX_NAME + 1);  /* + 1 for trailing null */

  // NC_INQ_ATTNAME
  status = nc_inq_attname (c_ncid, c_varid, attnum, c_att_name);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }
  return scm_from_locale_string (c_att_name);
}

//---NC_GET_ATT_[TYPE]
SCM
nc_get_att_text_wrapper (SCM ncid_smob, SCM varid_smob, SCM att_name) 
{
  int c_varid, c_ncid, status;
  size_t length, t_len;
  char *c_att_name, *tp;
  struct ncid *nc_ncid, *nc_varid;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // ATTRIBUTE NAME
  c_att_name = scm_to_locale_string (att_name);

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_INQ_ATT_LEN
  status = nc_inq_attlen (c_ncid, c_varid, c_att_name, &t_len);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  tp = (char *) malloc(t_len + 1);  /* + 1 for trailing null */

  // NC_INQ_ATT
  status = nc_get_att_text (c_ncid, c_varid, c_att_name, tp);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  tp[t_len] = '\0';

  return scm_from_locale_string(tp);
}

SCM
nc_get_att_uchar_wrapper (SCM ncid_smob, SCM varid_smob, SCM att_name) 
{
  int c_varid, c_ncid, status, i;
  size_t length, u_len;
  char *c_att_name;
  unsigned char *up;
  struct ncid *nc_ncid, *nc_varid;
  SCM dbs = SCM_EOL;
  SCM dbs_1;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // ATTRIBUTE NAME
  c_att_name = scm_to_locale_string (att_name);

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_INQ_ATT_LEN
  status = nc_inq_attlen (c_ncid, c_varid, c_att_name, &u_len);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
    }

  up = (unsigned char *) malloc(u_len * sizeof(unsigned char));

  // NC_INQ_ATT
  status = nc_get_att_uchar (c_ncid, c_varid, c_att_name, up);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  if (u_len > 1) 
    {
    for (i=0; i<u_len; i++) 
      {
	dbs_1 = scm_list_1 (scm_from_schar (up[i]));
	dbs = scm_append (scm_list_2 (dbs, dbs_1));
      }
    return dbs;
    } 
  else 
    {
      return scm_from_uchar(*up);
    }
}

SCM
nc_get_att_schar_wrapper (SCM ncid_smob, SCM varid_smob, SCM att_name) 
{
  int c_varid, c_ncid, status, i;
  char *c_att_name;
  size_t length, c_len;
  signed char *cp;
  struct ncid *nc_ncid, *nc_varid;
  SCM dbs = SCM_EOL;
  SCM dbs_1;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // ATTRIBUTE NAME
  c_att_name = scm_to_locale_string (att_name);

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_INQ_ATT_LEN
  status = nc_inq_attlen (c_ncid, c_varid, c_att_name, &c_len);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
    }

  cp = (signed char *) malloc(c_len * sizeof(signed char));

  // NC_INQ_ATT
  status = nc_get_att_schar (c_ncid, c_varid, c_att_name, cp);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  // Possibly return a list
  if (c_len > 1) 
    {
      for (i=0; i<c_len; i++) 
	{
	  dbs_1 = scm_list_1 (scm_from_schar (cp[i]));
	  dbs = scm_append (scm_list_2 (dbs, dbs_1));
	}
      return dbs;
    } 
  else 
    {
      return scm_from_schar (*cp);
    }
}

SCM
nc_get_att_short_wrapper (SCM ncid_smob, SCM varid_smob, SCM att_name) 
{
  int c_varid, c_ncid, status, i;
  size_t length, s_len;
  char *c_att_name;
  short *sp;
  struct ncid *nc_ncid, *nc_varid;
  SCM dbs = SCM_EOL;
  SCM dbs_1;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // ATTRIBUTE NAME
  c_att_name = scm_to_locale_string (att_name);

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_INQ_ATT_LEN
  status = nc_inq_attlen (c_ncid, c_varid, c_att_name, &s_len);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
    }

  sp = (short *) malloc (s_len * sizeof (short));

  // NC_INQ_ATT
  status = nc_get_att_short (c_ncid, c_varid, c_att_name, sp);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  if (s_len > 1) 
    {
      for (i=0; i<s_len; i++) 
	{
	  dbs_1 = scm_list_1 (scm_from_short (sp[i]));
	  dbs = scm_append (scm_list_2 (dbs, dbs_1));
	}
      return dbs;
    } else 
    {
      return scm_from_short (*sp);
    }
}

SCM
nc_get_att_int_wrapper (SCM ncid_smob, SCM varid_smob, SCM att_name) 
{
  int c_varid, c_ncid, status, i;
  size_t length, i_len;
  char *c_att_name;
  int *ip;
  struct ncid *nc_ncid, *nc_varid;
  SCM dbs = SCM_EOL;
  SCM dbs_1;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // ATTRIBUTE NAME
  c_att_name = scm_to_locale_string (att_name);

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_INQ_ATT_LEN
  status = nc_inq_attlen (c_ncid, c_varid, c_att_name, &i_len);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
    }

  ip = (int *) malloc (i_len * sizeof (int));

  // NC_INQ_ATT
  status = nc_get_att_int (c_ncid, c_varid, c_att_name, ip);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  if (i_len > 1) 
    {
      for (i=0; i<i_len; i++) 
	{
	  dbs_1 = scm_list_1 (scm_from_int (ip[i]));
	  dbs = scm_append (scm_list_2 (dbs, dbs_1));
	}
    return dbs;
    } 
  else 
    {
      return scm_from_int (*ip);
    }
}

SCM
nc_get_att_long_wrapper (SCM ncid_smob, SCM varid_smob, SCM att_name) 
{
  int c_varid, c_ncid, status, i;
  size_t length, l_len;
  char *c_att_name;
  long *lp;
  struct ncid *nc_ncid, *nc_varid;
  SCM dbs = SCM_EOL;
  SCM dbs_1;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // ATTRIBUTE NAME
  c_att_name = scm_to_locale_string (att_name);

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_INQ_ATT_LEN
  status = nc_inq_attlen (c_ncid, c_varid, c_att_name, &l_len);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
    }

  lp = (long *) malloc (l_len * sizeof (long));

  // NC_INQ_ATT
  status = nc_get_att_long (c_ncid, c_varid, c_att_name, lp);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  if (l_len > 1) 
    {
      for (i=0; i<l_len; i++) 
	{
	  dbs_1 = scm_list_1 (scm_from_double (lp[i]));
	  dbs = scm_append (scm_list_2 (dbs, dbs_1));
	}
      return dbs;
    } 
  else 
    {
      return scm_from_long (*lp);
    }
}

SCM
nc_get_att_float_wrapper (SCM ncid_smob, SCM varid_smob, SCM att_name) 
{
  int c_varid, c_ncid, status, i;
  size_t length, f_len;
  char *c_att_name;
  float *fp;
  struct ncid *nc_ncid, *nc_varid;
  SCM dbs = SCM_EOL;
  SCM dbs_1;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // ATTRIBUTE NAME
  c_att_name = scm_to_locale_string (att_name);

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_INQ_ATT_LEN
  status = nc_inq_attlen (c_ncid, c_varid, c_att_name, &f_len);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
    }

  fp = (float *) malloc (f_len * sizeof (float));

  // NC_INQ_ATT
  status = nc_get_att_float (c_ncid, c_varid, c_att_name, fp);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  if (f_len > 1) 
    {
      for (i=0; i<f_len; i++) 
	{
	  dbs_1 = scm_list_1 (scm_from_double (fp[i]));
	  dbs = scm_append (scm_list_2 (dbs, dbs_1));
	}
      return dbs;
    } 
  else 
    {
      return scm_from_double (*fp);
    }
}

SCM
nc_get_att_double_wrapper (SCM ncid_smob, SCM varid_smob, SCM att_name) 
{
  int c_varid, c_ncid, status, i;
  size_t length, d_len;
  char *c_att_name;
  double *dp;
  struct ncid *nc_ncid, *nc_varid;
  SCM dbs = SCM_EOL;
  SCM dbs_1;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // ATTRIBUTE NAME
  c_att_name = scm_to_locale_string (att_name);

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_INQ_ATT_LEN
  status = nc_inq_attlen (c_ncid, c_varid, c_att_name, &d_len);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
    }

  dp = (double *) malloc (d_len * sizeof (double));

  // NC_INQ_ATT
  status = nc_get_att_double (c_ncid, c_varid, c_att_name, dp);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  if (d_len > 1) 
    {
      for (i=0; i<d_len; i++) 
	{
	  dbs_1 = scm_list_1 (scm_from_double (dp[i]));
	  dbs = scm_append (scm_list_2 (dbs, dbs_1));
	}
      return dbs;
    } 
  else 
    {
      return scm_from_double (*dp);
    }
}

SCM
nc_get_att_string_wrapper (SCM ncid_smob, SCM varid_smob, SCM att_name) 
{
  int c_varid, c_ncid, status, i;
  size_t length, t_len;
  char *c_att_name;
  struct ncid *nc_ncid, *nc_varid;
  SCM tbs = SCM_EOL;
  SCM tbs_1;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // ATTRIBUTE NAME
  c_att_name = scm_to_locale_string (att_name);

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_INQ_ATT_LEN
  status = nc_inq_attlen (c_ncid, c_varid, c_att_name, &t_len);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
    }

  char *tp[t_len];

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  //tp = (char *) malloc(t_len + 1);  /* + 1 for trailing null */

  // NC_GET_ATT_STRING
  status = nc_get_att_string (c_ncid, c_varid, c_att_name, tp);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }

  if (t_len > 1) 
    {
      for (i=0; i<t_len; i++) 
	{
	  tbs_1 = scm_list_1 (scm_from_locale_string (tp[i]));
	  tbs = scm_append (scm_list_2 (tbs, tbs_1));
	}
      return tbs;
    } 

  tp[t_len] = '\0';

  return scm_from_locale_string(*tp);
}

SCM
nc_get_att_wrapper (SCM ncid_smob, SCM varid_smob, SCM att_name) 
{
  int c_varid, c_ncid, status, i;
  size_t length, a_len;
  char *c_att_name;
  nc_type a_type;
  struct ncid *nc_ncid, *nc_varid;
  SCM dbs = SCM_EOL;
  SCM dbs_1;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // ATTRIBUTE NAME
  c_att_name = scm_to_locale_string (att_name);

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_INQ_ATT_LEN
  status = nc_inq_attlen (c_ncid, c_varid, c_att_name, &a_len);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
    }

  // NC_INQ_ATT_TYPE
  status = nc_inq_atttype (c_ncid, c_varid, c_att_name, &a_type);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
    }

  //fprintf (stderr, "%a\n", a_type);

  if (a_type == NC_BYTE) 
    {
      unsigned char *ap;

      ap = (unsigned char *) malloc (a_len * sizeof (unsigned char));
      status = nc_get_att_uchar (c_ncid, c_varid, c_att_name, ap);
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	}
      if (a_len > 1) 
	{
	  for (i=0; i<a_len; i++) 
	    {
	      dbs_1 = scm_list_1 (scm_from_uchar (ap[i]));
	      dbs = scm_append (scm_list_2 (dbs, dbs_1));
	    }
      return dbs;
	} 
      else 
	{
	  return scm_from_uchar(*ap);
	}
    } 
  else if (a_type == NC_CHAR) 
    {
      char *ap;

      ap = (char *) malloc (a_len + 1);  /* + 1 for trailing null */
      status = nc_get_att_text (c_ncid, c_varid, c_att_name, ap);
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	}
      ap[a_len] = '\0';
      
      return scm_from_locale_string (ap);
    } 
  else if (a_type == NC_SHORT) 
    {
      short *ap;

      ap = (short *) malloc (a_len * sizeof (short));
      status = nc_get_att_short (c_ncid, c_varid, c_att_name, ap);
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	}
      if (a_len > 1) 
	{
	  for (i=0; i<a_len; i++) 
	    {
	      dbs_1 = scm_list_1 (scm_from_short (ap[i]));
	      dbs = scm_append (scm_list_2 (dbs, dbs_1));
	    }
	  return dbs;
	} 
      else 
	{
	  return scm_from_short (*ap);
	}
    } 
  else if (a_type == NC_INT) 
    {
      int *ap;

      ap = (int *) malloc(a_len * sizeof(int));
      status = nc_get_att_int (c_ncid, c_varid, c_att_name, ap);
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	}
      if (a_len > 1) 
	{
	  for (i=0; i<a_len; i++) 
	    {
	      dbs_1 = scm_list_1 (scm_from_int (ap[i]));
	      dbs = scm_append (scm_list_2 (dbs, dbs_1));
	    }
	  return dbs;
	} 
      else 
	{
	  return scm_from_int (*ap);
	}
    } 
  else if 
    (a_type == NC_FLOAT) 
    {
      float *ap;

      ap = (float *) malloc (a_len * sizeof (float));
      status = nc_get_att_float (c_ncid, c_varid, c_att_name, ap);
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	}
      if (a_len > 1) 
	{
	  for (i=0; i<a_len; i++) 
	    {
	      dbs_1 = scm_list_1 (scm_from_double (ap[i]));
	      dbs = scm_append (scm_list_2 (dbs, dbs_1));
	    }
	  return dbs;
	} 
      else 
	{
	  return scm_from_double (*ap);
	}
    } 
  else if (a_type == NC_DOUBLE) 
    {
      double *dp;

      dp = (double *) malloc (a_len * sizeof (double));
      status = nc_get_att_double (c_ncid, c_varid, c_att_name, dp);
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	}
      if (a_len > 1) 
	{
	  for (i=0; i<a_len; i++) 
	    {
	      dbs_1 = scm_list_1 (scm_from_double (dp[i]));
	      dbs = scm_append (scm_list_2 (dbs, dbs_1));
	    }
	  return dbs;
	} 
      else 
	{
	  double test = *dp;
	  free(dp);
	  return scm_from_double (test);
	}
    } 
  else if (a_type == NC_STRING) 
    {
      char *ap[a_len];

      status = nc_get_att_string (c_ncid, c_varid, c_att_name, ap);
      if (status != NC_NOERR) 
	{
	  if (nc_verbose == SCM_BOOL_T)
	    {
	      fprintf (stderr, "%s\n", nc_strerror (status));
	    }
	}
      if (a_len > 1) 
	{
	  for (i=0; i<a_len; i++) 
	    {
	      dbs_1 = scm_list_1 (scm_from_locale_string (ap[i]));
	      dbs = scm_append (scm_list_2 (dbs, dbs_1));
	    }
	  return dbs;
	} 
      else 
	{
	  ap[a_len] = '\0';
	  return scm_from_locale_string (*ap);
	}
    } 
  else 
    {
      return SCM_BOOL_F;
    }
}

//---NC_PUT_ATT_[TYPE]
//TODO Allow lists as scm input for put functions
SCM
nc_put_att_text_wrapper (SCM ncid_smob, SCM varid_smob, SCM att_name, SCM att_val) 
{
  int c_varid, c_ncid, status;
  size_t length, t_len;
  char *c_att_name, *c_att_val;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));
  
  struct ncid *nc_ncid, *nc_varid;

  // ATTRIBUTE NAME
  c_att_name = scm_to_locale_string (att_name);
  // ATTRIBUTE VALUE
  c_att_val = scm_to_locale_string (att_val);

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_INQ_ATTLEN
  status = nc_inq_attlen (c_ncid, c_varid, c_att_name, &t_len);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
    }

  // NC_PUT_ATT_TEXT
  status = nc_put_att_text (c_ncid, c_varid, c_att_name, 
			    strlen(c_att_val), c_att_val);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }
  else 
    {
      return SCM_BOOL_T;
    }
}

SCM
nc_put_att_double_wrapper (SCM ncid_smob, SCM varid_smob, SCM att_name, SCM att_val) 
{
  int c_varid, c_ncid, status, i;
  size_t length; //, t_len;
  char *c_att_name;
  double *c_att_val;
  SCM s_symbol;
  SCM s_list;
  SCM s_value;
  unsigned long list_length;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  struct ncid *nc_ncid, *nc_varid;
  // ATTRIBUTE NAME
  c_att_name = scm_to_locale_string (att_name);

  if (SCM_NFALSEP(scm_list_p(att_val))) 
    {

      list_length = scm_to_ulong(scm_length(att_val));
      //fprintf (stderr, "list_length: %d\n", list_length);
      c_att_val = (double *)malloc(sizeof(double) * list_length);

      /* Copy the Guile list into the C array */ 
      for (i=0; i < list_length; i++) {
	//fprintf (stderr, "i: %d\n", i);	
	/* Get the i-th element of the list */ 
	s_value = scm_list_ref(att_val, scm_from_int(i));
	/* Convert it into a C double */
	c_att_val[i] = scm_to_double(s_value);
	//fprintf (stderr, "c_att_val(%d): %f\n", i, c_att_val[i]);
      }
    }
  else
    {
      // ATTRIBUTE VALUE
      c_att_val = (double *) malloc (sizeof (double));
      *c_att_val = scm_to_double (att_val);
      list_length = 1;
    }

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_INQ_ATTLEN
  //status = nc_inq_attlen (c_ncid, c_varid, c_att_name, &t_len);

  //fprintf (stderr, "%s\n", c_att_name);
  //fprintf (stderr, "%d\n", c_att_val);

  // NC_PUT_ATT_DOUBLE
  status = nc_put_att_double (c_ncid, c_varid, c_att_name, NC_DOUBLE, list_length, c_att_val);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    }
  else
    {
      return SCM_BOOL_T;
    }
}

//---editing

SCM
nc_rename_att_wrapper (SCM ncid_smob, SCM varid_smob, SCM att_name, SCM att_newname) 
{
  int c_varid, c_ncid, status;
  size_t length; //
  char *c_att_name, *c_att_newname;
  struct ncid *nc_ncid, *nc_varid;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // ATTRIBUTE NAMES
  c_att_name = scm_to_locale_string (att_name);
  c_att_newname = scm_to_locale_string (att_newname);

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_RENAME_ATT
  status = nc_rename_att (c_ncid, c_varid, c_att_name, c_att_newname);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    } 
  else 
    {
      return SCM_BOOL_T;
    }
}

SCM
nc_del_att_wrapper (SCM ncid_smob, SCM varid_smob, SCM att_name) 
{
  int c_varid, c_ncid, status;
  size_t length; //
  char *c_att_name;
  struct ncid *nc_ncid, *nc_varid;
  SCM nc_verbose = scm_variable_ref(scm_c_lookup("nc-verbose"));

  // ATTRIBUTE NAME
  c_att_name = scm_to_locale_string (att_name);

  // NCID
  scm_assert_smob_type (ncid_tag, ncid_smob);
  nc_ncid = (struct ncid *) SCM_SMOB_DATA (ncid_smob);
  c_ncid = nc_ncid->nc_id;

  // VARID
  scm_assert_smob_type (ncid_tag, varid_smob);
  nc_varid = (struct ncid *) SCM_SMOB_DATA (varid_smob);
  c_varid = nc_varid->nc_id;

  // NC_DEL_ATT
  status = nc_del_att (c_ncid, c_varid, c_att_name);

  if (status != NC_NOERR) 
    {
      if (nc_verbose == SCM_BOOL_T)
	{
	  fprintf (stderr, "%s\n", nc_strerror (status));
	}
      return SCM_BOOL_F;
    } 
  else 
    {
      return SCM_BOOL_T;
    }
}

/*----------
  Init
----------*/

void
scm_init_netcdf() 
{
  //---NC_VERBOSE
  SCM nc_verbose = SCM_BOOL_F;
  scm_c_define ("nc-verbose", nc_verbose);

  // NCID SMOB
  ncid_tag = scm_make_smob_type ("ncid", sizeof (struct ncid));
  scm_set_smob_mark (ncid_tag, mark_ncid);
  scm_set_smob_free (ncid_tag, free_ncid);
  scm_set_smob_print (ncid_tag, print_ncid);

  // NC_TYPE SMOB
  nctype_tag = scm_make_smob_type ("nctype", sizeof (struct nctype));
  scm_set_smob_mark (nctype_tag, mark_nctype);
  scm_set_smob_free (nctype_tag, free_nctype);
  scm_set_smob_print (nctype_tag, print_nctype);

  // SCM_Variables

  //---NC_GLOBAL SMOB
  struct ncid *nc_global;
  SCM global_smob;
  nc_global = (struct ncid *) scm_gc_malloc (sizeof (struct ncid), "nc-global");
  nc_global->fname = scm_from_locale_string ("nil");
  nc_global->idname = scm_from_locale_string ("global_varid");
  nc_global->status = 0;
  nc_global->nc_id = 0;
  nc_global->update_func = SCM_BOOL_F;  

  SCM_NEWSMOB (global_smob, ncid_tag, nc_global);
  
  nc_global->fname = scm_from_locale_string ("NC_GLOBAL");
  nc_global->nc_id = NC_GLOBAL;

  scm_c_define ("nc-global", global_smob);

  // NetCDF
  scm_c_define_gsubr ("nc-inq-libvers", 0, 0, 0, nc_inq_libvers_wrapper);
  scm_c_define_gsubr ("nc-guile-inq-libvers", 0, 0, 0, nc_guile_inq_libvers_wrapper);
  scm_c_define_gsubr ("nc-make-nctype", 1, 0, 0, scm_make_nc_type);
  scm_c_define_gsubr ("nc-display-nctype", 1, 0, 0, nc_display_nctype);
  scm_c_define_gsubr ("nc-display-short-nctype", 1, 0, 0, nc_display_short_nctype);
  // Error
  scm_c_define_gsubr ("nc-strerror", 1, 0, 0, nc_strerrer_wrapper);
  // Datasets
  scm_c_define_gsubr ("nc-file?", 1, 0, 0, nc_file_p_wrapper);
  scm_c_define_gsubr ("nc-create", 2, 0, 0, nc_create_wrapper);
  scm_c_define_gsubr ("nc-open", 2, 0, 0, nc_open_wrapper);
  scm_c_define_gsubr ("nc-redef", 1, 0, 0, nc_redef_wrapper);
  scm_c_define_gsubr ("nc-enddef", 1, 0, 0, nc_enddef_wrapper);
  scm_c_define_gsubr ("nc-close", 1, 0, 0, nc_close_wrapper);
  scm_c_define_gsubr ("nc-sync", 1, 0, 0, nc_sync_wrapper);
  scm_c_define_gsubr ("nc-abort", 1, 0, 0, nc_abort_wrapper);
  //---inq
  scm_c_define_gsubr ("nc-inq", 1, 0, 0, nc_inq_wrapper);
  scm_c_define_gsubr ("nc-inq-ndims", 1, 0, 0, nc_inq_ndims_wrapper);
  scm_c_define_gsubr ("nc-inq-nvars", 1, 0, 0, nc_inq_nvars_wrapper);
  scm_c_define_gsubr ("nc-inq-natts", 1, 0, 0, nc_inq_natts_wrapper);
  scm_c_define_gsubr ("nc-inq-unlimdim", 1, 0, 0, nc_inq_unlimdim_wrapper);
  scm_c_define_gsubr ("nc-inq-format", 1, 0, 0, nc_inq_format_wrapper);
  // Groups
  scm_c_define_gsubr ("nc-inq-ncid", 2, 0, 0, nc_inq_ncid_wrapper);
  scm_c_define_gsubr ("nc-inq-grps", 1, 0, 0, nc_inq_grps_wrapper);
  scm_c_define_gsubr ("nc-inq-dimids", 1, 0, 0, nc_inq_dimids_wrapper);
  scm_c_define_gsubr ("nc-inq-varids", 1, 0, 0, nc_inq_varids_wrapper);
  // Dimensions
  scm_c_define_gsubr ("nc-inq-dimid", 2, 0, 0, nc_inq_dimid_wrapper);
  scm_c_define_gsubr ("nc-inq-dim", 2, 0, 0, nc_inq_dim_wrapper);
  scm_c_define_gsubr ("nc-inq-dimname", 2, 0, 0, nc_inq_dimname_wrapper);
  scm_c_define_gsubr ("nc-inq-dimlen", 2, 0, 0, nc_inq_dimlen_wrapper);
  // Variables
  scm_c_define_gsubr ("nc-inq-varid", 2, 0, 0, nc_inq_varid_wrapper);
  scm_c_define_gsubr ("nc-inq-var", 2, 0, 0, nc_inq_var_wrapper);
  scm_c_define_gsubr ("nc-inq-varname", 2, 0, 0, nc_inq_varname_wrapper);
  scm_c_define_gsubr ("nc-inq-vartype", 2, 0, 0, nc_inq_vartype_wrapper);
  scm_c_define_gsubr ("nc-inq-varndims", 2, 0, 0, nc_inq_varndims_wrapper);
  scm_c_define_gsubr ("nc-inq-vardimid", 2, 0, 0, nc_inq_vardimid_wrapper);
  scm_c_define_gsubr ("nc-inq-varnatts", 2, 0, 0, nc_inq_varnatts_wrapper);
  //---get
  scm_c_define_gsubr ("nc-get-var1", 3, 0, 0, nc_get_var1_wrapper);
  scm_c_define_gsubr ("nc-get-var1-text", 3, 0, 0, nc_get_var1_text_wrapper);
  scm_c_define_gsubr ("nc-get-var1-uchar", 3, 0, 0, nc_get_var1_uchar_wrapper);
  scm_c_define_gsubr ("nc-get-var1-schar", 3, 0, 0, nc_get_var1_schar_wrapper);
  scm_c_define_gsubr ("nc-get-var1-short", 3, 0, 0, nc_get_var1_short_wrapper);
  scm_c_define_gsubr ("nc-get-var1-ushort", 3, 0, 0, nc_get_var1_ushort_wrapper);
  scm_c_define_gsubr ("nc-get-var1-int", 3, 0, 0, nc_get_var1_int_wrapper);
  scm_c_define_gsubr ("nc-get-var1-uint", 3, 0, 0, nc_get_var1_uint_wrapper);
  scm_c_define_gsubr ("nc-get-var1-long", 3, 0, 0, nc_get_var1_long_wrapper);
  scm_c_define_gsubr ("nc-get-var1-float", 3, 0, 0, nc_get_var1_float_wrapper);
  scm_c_define_gsubr ("nc-get-var1-double", 3, 0, 0, nc_get_var1_double_wrapper);
  scm_c_define_gsubr ("nc-get-var-int", 2, 0, 0, nc_get_var_int_wrapper);
  scm_c_define_gsubr ("nc-get-var-long", 2, 0, 0, nc_get_var_long_wrapper);
  scm_c_define_gsubr ("nc-get-var-short", 2, 0, 0, nc_get_var_short_wrapper);
  scm_c_define_gsubr ("nc-get-var-double", 2, 0, 0, nc_get_var_double_wrapper);
  scm_c_define_gsubr ("nc-get-var-float", 2, 0, 0, nc_get_var_float_wrapper);
  scm_c_define_gsubr ("nc-get-var", 2, 0, 0, nc_get_var_wrapper);
  scm_c_define_gsubr ("nc-scan-var-range", 2, 0, 0, nc_scan_var_range_wrapper);
  //---put
  scm_c_define_gsubr ("nc-put-var1", 4, 0, 0, nc_put_var1_wrapper);
  scm_c_define_gsubr ("nc-put-var1-double", 4, 0, 0, nc_put_var1_double_wrapper);
  scm_c_define_gsubr ("nc-put-var1-float", 4, 0, 0, nc_put_var1_float_wrapper);
  scm_c_define_gsubr ("nc-put-var1-int", 4, 0, 0, nc_put_var1_int_wrapper);
  scm_c_define_gsubr ("nc-put-var1-short", 4, 0, 0, nc_put_var1_short_wrapper);
  scm_c_define_gsubr ("nc-put-var1-long", 4, 0, 0, nc_put_var1_long_wrapper);
  //---other
  scm_c_define_gsubr ("nc-rename-var", 3, 0, 0, nc_rename_var_wrapper);
  // Attributes
  //---inq
  scm_c_define_gsubr ("nc-inq-att", 3, 0, 0, nc_inq_att_wrapper);
  scm_c_define_gsubr ("nc-inq-atttype", 3, 0, 0, nc_inq_atttype_wrapper);
  scm_c_define_gsubr ("nc-inq-attlen", 3, 0, 0, nc_inq_attlen_wrapper);
  scm_c_define_gsubr ("nc-inq-attname", 3, 0, 0, nc_inq_attname_wrapper);
  //---get
  scm_c_define_gsubr ("nc-get-att-text", 3, 0, 0, nc_get_att_text_wrapper);
  scm_c_define_gsubr ("nc-get-att-string", 3, 0, 0, nc_get_att_text_wrapper);
  scm_c_define_gsubr ("nc-get-att-uchar", 3, 0, 0, nc_get_att_uchar_wrapper);
  scm_c_define_gsubr ("nc-get-att-schar", 3, 0, 0, nc_get_att_schar_wrapper);
  scm_c_define_gsubr ("nc-get-att-short", 3, 0, 0, nc_get_att_short_wrapper);
  scm_c_define_gsubr ("nc-get-att-int", 3, 0, 0, nc_get_att_int_wrapper);
  scm_c_define_gsubr ("nc-get-att-long", 3, 0, 0, nc_get_att_long_wrapper);
  scm_c_define_gsubr ("nc-get-att-float", 3, 0, 0, nc_get_att_float_wrapper);
  scm_c_define_gsubr ("nc-get-att-double", 3, 0, 0, nc_get_att_double_wrapper);
  scm_c_define_gsubr ("nc-get-att", 3, 0, 0, nc_get_att_wrapper);
  //---put
  scm_c_define_gsubr ("nc-put-att-text", 4, 0, 0, nc_put_att_text_wrapper);
  scm_c_define_gsubr ("nc-put-att-double", 4, 0, 0, nc_put_att_double_wrapper);
  //---editing
  scm_c_define_gsubr ("nc-rename-att", 4, 0, 0, nc_rename_att_wrapper);
  scm_c_define_gsubr ("nc-del-att", 3, 0, 0, nc_del_att_wrapper);
}
//---END
