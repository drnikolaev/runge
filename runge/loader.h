// Routines loader
/* Copyright (C), Sergei Nikolaev, 2008-2011, http://cvmlib.com */

#ifndef _RUNGE_LOADER_
#define _RUNGE_LOADER_

// this is for MinGW
#define _GLIBCXX_USE_WCHAR_T 1

#include <cfun.h>

#if defined(WIN32) && defined(_DEBUG)
     #define DEBUG_NEW new( _NORMAL_BLOCK, __FILE__, __LINE__ )
     #define new DEBUG_NEW
#endif

#ifdef _WIN32
#   ifdef __MINGW32__
#       ifdef __stdcall
#           undef __stdcall
#       endif
#       define __stdcall __attribute__ ((__stdcall__))
#   endif

    typedef FARPROC DES_PROC_PTR;
    typedef HMODULE DES_SO_PTR;

#else
#   include <dlfcn.h>

    typedef void* DES_PROC_PTR;
    typedef void* DES_SO_PTR;
#endif


class DynamicLoader {
    mutable std::map<std::string, DES_SO_PTR> m_so_pool;
    std::string m_exe_dir;

    DynamicLoader();

    DES_SO_PTR _get_so (const std::string& so_name) const throw(std::exception);

public:
    ~DynamicLoader();

    DES_PROC_PTR get_solver (const std::string& so_name, const std::string& proc_name) const throw(std::exception);

/*
    solver_FTX_F get_solver_FTX_F (const std::string& so_name, const std::string& proc_name) const {
        return (solver_FTX_F)_get_proc (so_name, proc_name);
    }
    solver_FX_FFJ get_solver_FX_FFJ (const std::string& so_name, const std::string& proc_name) const {
        return (solver_FX_FFJ)_get_proc (so_name, proc_name);
    }
    solver_FTX_FFJFT get_solver_FTX_FFJFT (const std::string& so_name, const std::string& proc_name) const {
        return (solver_FTX_FFJFT)_get_proc (so_name, proc_name);
    }
    solver_FTX_FFJ get_solver_FTX_FFJ (const std::string& so_name, const std::string& proc_name) const {
        return (solver_FTX_FFJ)_get_proc (so_name, proc_name);
    }
    solver_BUTX_BFU get_solver_BUTX_BFU (const std::string& so_name, const std::string& proc_name) const {
        return (solver_BUTX_BFU)_get_proc (so_name, proc_name);
    }
    solver_AFI_FAFI get_solver_AFI_FAFI (const std::string& so_name, const std::string& proc_name) const {
        return (solver_AFI_FAFI)_get_proc (so_name, proc_name);
    }
*/

    friend const DynamicLoader& get_loader();
};


const DynamicLoader& get_loader();

std::string get_exe_dir() throw(std::exception);
std::string safe_get_exe_dir(const std::string& subst);

#endif  // _RUNGE_LOADER_
