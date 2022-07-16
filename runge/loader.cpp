#include "loader.h"
#include <stdexcept>

#ifdef __APPLE__
#   include <libproc.h>
#   include <unistd.h>
#else
#   ifndef _WIN32
#       include <sys/types.h>
#       include <unistd.h>
#   endif
#endif


#if !defined (PATH_MAX)
#   define PATH_MAX 256
#endif

#ifdef _WIN32
#   define DIR_SEPARATOR '\\'
#else
#   define DIR_SEPARATOR '/'
#endif


#ifdef _WIN32
#   define RUNGE_WIN32_MESSAGE_LENGTH 1024

void throw_win32_error(const char* arg = NULL) throw(std::exception)
{
    TCHAR lpMsgBuf[RUNGE_WIN32_MESSAGE_LENGTH];
    FormatMessage  (FORMAT_MESSAGE_FROM_SYSTEM,    
                    NULL,
                    GetLastError(),
                    MAKELANGID (LANG_NEUTRAL, SUBLANG_DEFAULT),
                    lpMsgBuf,    
                    RUNGE_WIN32_MESSAGE_LENGTH - 1,    
                    NULL);
    CHAR lpOemMsgBuf[RUNGE_WIN32_MESSAGE_LENGTH];
    CharToOem (lpMsgBuf, lpOemMsgBuf);
    std::ostringstream os;
    os << "Error: " << (strlen(lpOemMsgBuf) > 0 ? lpOemMsgBuf : "unknown\n");
    if (arg && strlen(arg) > 0) {
        os << "Argument: \'" << arg << "\'";
    }
    os << std::ends;
    throw std::runtime_error(os.str());
}

/*
std::wstring get_unicode_string (const std::string& ansi_str) throw(std::exception)
{
    const size_t buf_size = ansi_str.length() + 1;
    LPWSTR wstr = (LPWSTR) LocalAlloc (LMEM_FIXED | LMEM_ZEROINIT, 2 * buf_size);
    if (wstr == NULL) {
        throw std::bad_alloc();
    }
    if (!MultiByteToWideChar(CP_ACP, 0, ansi_str.c_str(), buf_size, wstr, buf_size)) {
        throw_win32_error();
    }

    std::wostringstream os;
    os << wstr << std::ends;
    LocalFree (wstr);
    return os.str();
}
*/
#endif


std::string safe_get_exe_dir(const std::string& subst)
{
    try {
        return get_exe_dir();
    }
    catch (std::exception&) {
	}
    return subst;
}


std::string get_exe_dir() throw(std::exception)
{
#ifdef _WIN32
    static char path[PATH_MAX]; 
    static TCHAR tpath[PATH_MAX];
    DWORD dwResult = GetModuleFileName(NULL, tpath, PATH_MAX);
    if (dwResult == 0) {
        throw_win32_error();
    }
    CharToOem (tpath, path);
#else
#	ifdef __APPLE__
    static char path[PROC_PIDPATHINFO_MAXSIZE];
    pid_t pid = getpid();
    ssize_t ret = proc_pidpath(pid, path, sizeof(path));
#	else
    static char path[PATH_MAX]; 
    char linkname[128];
    pid_t pid = getpid();
    snprintf(linkname, sizeof(linkname), "/proc/%i/exe", pid);
    ssize_t ret = readlink(linkname, path, sizeof(path));
#	endif
    if (ret <= 0 || ret > static_cast<ssize_t>(sizeof(path))) {
        std::ostringstream err_stream;
        err_stream << "Failed to retrieve executable path for process " << pid;
        throw std::runtime_error(err_stream.str());
    }
    path[ret] = '\0';
#endif
    char* p_last_separator = strrchr(path, DIR_SEPARATOR);
    if (p_last_separator == NULL) {
        std::ostringstream err_stream;
        err_stream << "Failed to retrieve executable directory for path \'" << path << "\'";
        throw std::runtime_error(err_stream.str());
    }
    p_last_separator[1] = '\0';
    return path;
}

DynamicLoader::DynamicLoader() :
    m_exe_dir(get_exe_dir())
{
}

const DynamicLoader& get_loader()
{
    static DynamicLoader loader;
    return loader;
}

DynamicLoader::~DynamicLoader()
{
    std::map<std::string, DES_SO_PTR>::const_iterator it = m_so_pool.begin();
    std::map<std::string, DES_SO_PTR>::const_iterator end = m_so_pool.end();
    while (it != end) {
#ifdef _WIN32
        if (!FreeLibrary(it->second)) throw_win32_error();
#else
        dlclose(it->second);
#endif
        ++it;
    }
}

DES_SO_PTR DynamicLoader::_get_so (const std::string& so_name) const throw(std::exception)
{
    std::map<std::string, DES_SO_PTR>::const_iterator it = m_so_pool.find(so_name);
    if (it == m_so_pool.end()) {
        std::string so_full_path = m_exe_dir + so_name;
#ifdef _WIN32
        static TCHAR tso_full_path[PATH_MAX];
        OemToChar(so_full_path.c_str(), tso_full_path);
        DES_SO_PTR p_so = LoadLibrary(tso_full_path);
        if (p_so == NULL) {
            throw_win32_error(so_full_path.c_str());
        }
#else
        DES_SO_PTR p_so = dlopen(so_full_path.c_str(), RTLD_LAZY | RTLD_LOCAL);
        if (p_so == NULL) {
            throw std::runtime_error(dlerror());
        }
#endif
        m_so_pool.insert(std::pair<std::string, DES_SO_PTR>(so_name, p_so));
        return p_so;
    }
    return it->second;
}

DES_PROC_PTR DynamicLoader::get_solver (const std::string& so_name, const std::string& proc_name) const throw(std::exception)
{
#ifdef _WIN32
//    std::wstring wproc_name = get_unicode_string (proc_name);
	DES_PROC_PTR p_proc = GetProcAddress(_get_so(so_name), proc_name.c_str());
    if (p_proc == NULL) {
        throw_win32_error(proc_name.c_str());
    }
#else
    void* p_proc = dlsym (_get_so(so_name), proc_name.c_str());
    if (p_proc == NULL) {
        throw std::runtime_error(dlerror());
    }
#endif
    return p_proc;
}
