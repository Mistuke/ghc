
/*
Need to concatenate this file with something that defines:
LPWSTR path_dirs[];
LPWSTR progDll;
LPWSTR rtsDll;
int rtsOpts;
*/

/* Define a minimum level of support needed for this file.  */
#ifndef WINVER
#define WINVER 0x06000100
#define UNICODE 1
#endif

#include <stdarg.h>
#include <stdio.h>
#include <Windows.h>
#include <Shlwapi.h>
#include <wchar.h>
#include <stdbool.h>

#include "Rts.h"

// MingW-w64 is missing these from the implementation. So we have to look them up
typedef DLL_DIRECTORY_COOKIE(WINAPI *LPAddDLLDirectory)(PCWSTR NewDirectory);
typedef WINBOOL(WINAPI *LPRemoveDLLDirectory)(DLL_DIRECTORY_COOKIE Cookie);

void die(char *fmt, ...) {
    va_list argp;

    fprintf(stderr, "error: ");
    va_start(argp, fmt);
    vfprintf(stderr, fmt, argp);
    va_end(argp);
    fprintf(stderr, "\n");

    exit(1);
}

static void *GetNonNullProcAddress(HINSTANCE h, char *sym) {
    void *p;

    p = GetProcAddress(h, sym);
    if (p == NULL) {
        die("Failed to find address for %s", sym);
    }
    return p;
}

DLL_DIRECTORY_COOKIE* setSearchPath (int *len) {
    HMODULE hDLL = (HMODULE)LoadLibraryW(L"Kernel32.DLL");
    LPAddDLLDirectory AddDllDirectory = (LPAddDLLDirectory)GetNonNullProcAddress(hDLL, "AddDllDirectory");

    LPWSTR *dir;
    LPWSTR exePath;

    exePath = malloc(sizeof(WCHAR) * MAX_PATH);

    /* Get the location of the exe to use as a base. */
    int lenPath = GetModuleFileName(NULL, exePath, MAX_PATH);

    int n;
    int entries = 0;

    DLL_DIRECTORY_COOKIE* cookies;

    /* Count the amount of entries we have;.  */
    for (dir = path_dirs; *dir != NULL; dir++) {
        entries++;
    }

    cookies = malloc(sizeof(DLL_DIRECTORY_COOKIE) * entries);

    const unsigned int init_buf_size = 4096;
    n = 0;

    for (dir = path_dirs; *dir != NULL; dir++) {
        /* Make sure the path is an absolute path.  */
        WCHAR* abs_path = malloc(sizeof(WCHAR) * init_buf_size);
        int len = wcsnlen_s(*dir, MAX_PATH) + lenPath + 5;
        LPWSTR path = malloc(sizeof(WCHAR) * len);
        wcscpy(path, exePath);
        wcscat(path, L"\\..\\"); /* exePath still contains filename, remove it.  */
        wcscat(path, *dir);

        DWORD wResult = GetFullPathNameW(path, init_buf_size, abs_path, NULL);
        if (!wResult){
            die("setSearchPath[GetFullPathNameW]: %" PATH_FMT " (Win32 error %lu)", path, GetLastError());
        }
        else if (wResult > init_buf_size) {
            abs_path = realloc(abs_path, sizeof(WCHAR) * wResult);
            if (!GetFullPathNameW(path, wResult, abs_path, NULL)) {
                die("setSearchPath[GetFullPathNameW]: %" PATH_FMT " (Win32 error %lu)", path, GetLastError());
            }
        }

        cookies[n++] = AddDllDirectory(abs_path);
        free(abs_path);
        free(path);
    }

    *len = entries;
    return cookies;
}

void cleanSearchPath (DLL_DIRECTORY_COOKIE* cookies, int len) {
    if (cookies){
        HMODULE hDLL = (HMODULE)LoadLibraryW(L"Kernel32.DLL");
        LPAddDLLDirectory RemoveDllDirectory = (LPAddDLLDirectory)GetNonNullProcAddress(hDLL, "RemoveDllDirectory");

        for (int x = 0; x < len; x++) {
            if (cookies[x]) {
                RemoveDllDirectory(cookies[x]);
                cookies[x] = NULL;
            }
        }

        free(cookies);
    }
}

HINSTANCE loadDll(LPWSTR dll) {
    HINSTANCE h;
    DWORD dw;
    LPVOID lpMsgBuf;

    const DWORD flags = LOAD_LIBRARY_SEARCH_USER_DIRS | LOAD_LIBRARY_SEARCH_DEFAULT_DIRS;

    h = LoadLibraryExW(dll, NULL, flags);

    if (h == NULL) {
        dw = GetLastError();
        FormatMessage(
            FORMAT_MESSAGE_ALLOCATE_BUFFER |
            FORMAT_MESSAGE_FROM_SYSTEM |
            FORMAT_MESSAGE_IGNORE_INSERTS,
            NULL,
            dw,
            MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
            (LPTSTR) &lpMsgBuf,
            0, NULL );
        die("loadDll %ls failed: %d: %ls\n", dll, dw, lpMsgBuf);
    }

    return h;
}

typedef int (*hs_main_t)(int , char **, StgClosure *, RtsConfig);
typedef void (*hs_init_t)(int *argc, char **argv[]);

int main(int argc, char *argv[]) {
    HINSTANCE hRtsDll, hProgDll;

    StgClosure *main_p;
    RtsConfig rts_config;
    hs_main_t hs_main_p;
    hs_init_t hs_init_p;

    int count = 0;
    DLL_DIRECTORY_COOKIE* cookies = setSearchPath(&count);

    /* RTS must be loaded first and initialized before any other haskell
       library, so the static constructors are properly initialized.  */
    hRtsDll = loadDll(rtsDll);
    hs_init_p = GetNonNullProcAddress(hRtsDll, "hs_init");
    hs_init_p(&argc, &argv);

    /* Now load the program Dll, this will trigger loading of all dependencies
       and initialize all static constructors.  */
    hProgDll = loadDll(progDll);

    /* Do some default initializations.
       These should mirror those done for mkExtraObjToLinkIntoBinary
       in compiler/main/DriverPipeline.hs or bad things will happen.  */
    hs_main_p    = GetNonNullProcAddress(hRtsDll , "hs_main");
    main_p       = GetNonNullProcAddress(hProgDll, "ZCMain_main_closure");
    rts_config   = *(RtsConfig*)GetNonNullProcAddress(hRtsDll, "defaultRtsConfig");
    rts_config.rts_opts_enabled     = rtsOpts;
    rts_config.rts_opts_suggestions = false;
    rts_config.rts_hs_main          = true;

    int hs_exit_code = hs_main_p(argc, argv, main_p, rts_config);

    cleanSearchPath(cookies, count);

    return hs_exit_code;
}

