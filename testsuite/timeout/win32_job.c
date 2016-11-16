#define UNICODE
#define _UNICODE
#define STRICT
#include <windows.h>
#include <stdio.h>
#include <stdbool.h>

extern int setJobParameters( HANDLE hJob )
{
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION jeli;
    ZeroMemory(&jeli, sizeof(JOBOBJECT_EXTENDED_LIMIT_INFORMATION));
    // Configure all child processes associated with the job to terminate when the
    // Last process in the job terminates. This prevent half dead processes and that
    // hanging ghc-iserv.exe process that happens when you interrupt the testsuite.
    jeli.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
    
    return SetInformationJobObject( hJob, JobObjectExtendedLimitInformation,
                                   &jeli, sizeof(JOBOBJECT_EXTENDED_LIMIT_INFORMATION) );
}

extern HANDLE createCompletionPort( HANDLE hJob )
{
    HANDLE ioPort = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 1);
    if (!ioPort)
    {
        printf("CreateIoCompletionPort error: %d\n", GetLastError());
        return NULL;
    }

    JOBOBJECT_ASSOCIATE_COMPLETION_PORT Port;
    Port.CompletionKey = hJob;
    Port.CompletionPort = ioPort;
    if (!SetInformationJobObject(hJob,
        JobObjectAssociateCompletionPortInformation,
        &Port, sizeof(Port))) {
        printf("SetInformation, error %d\n", GetLastError());
        return NULL;
    }

    return ioPort;
}

extern bool waitForJobCompletion ( HANDLE hJob, HANDLE ioPort, DWORD timeout )
{
    DWORD CompletionCode;
    ULONG_PTR CompletionKey;
    LPOVERLAPPED Overlapped;

#if DEBUG
    printf("[I/O] :: Wait Process: start.\n");
#endif
    while (GetQueuedCompletionStatus(ioPort, &CompletionCode,
             &CompletionKey, &Overlapped, timeout)) {
        // We're still waiting, let's sing a song! lalala lalalalaaa
        switch (CompletionCode)
        {
            case JOB_OBJECT_MSG_NEW_PROCESS:
#if DEBUG
                printf("[I/O] :: New Process: %p\n", Overlapped);
#endif
                break;
            case JOB_OBJECT_MSG_EXIT_PROCESS:
#if DEBUG
                printf("[I/O] :: Exit Process: %p\n", Overlapped);
#endif
                break;
            case JOB_OBJECT_MSG_ACTIVE_PROCESS_ZERO:
#if DEBUG
                printf("[I/O] :: All done: %p\n", Overlapped);
#endif
                return true;
            default:
                break;
        }
    }

#if DEBUG
    printf("[I/O] :: Wait Process: done.\n");
    JOBOBJECT_BASIC_PROCESS_ID_LIST idlist;
    ZeroMemory(&idlist, sizeof(JOBOBJECT_BASIC_PROCESS_ID_LIST));

    QueryInformationJobObject(hJob, JobObjectBasicProcessIdList, &idlist, sizeof(JOBOBJECT_BASIC_PROCESS_ID_LIST), NULL);
    printf("[I/O] Proc Count: %d>=%d\n", idlist.NumberOfProcessIdsInList, idlist.NumberOfAssignedProcesses);
    for (int x = 0; x < idlist.NumberOfProcessIdsInList; x++)
    {
        printf("[I/O] :: %p\n", idlist.ProcessIdList[x]);
    }
#endif

    if (Overlapped == NULL && (HANDLE)CompletionKey != hJob)
    {
#if DEBUG
        printf("[I/O] :: Timeout happened.\n");
#endif
        // Timeout occurred. *dark voice* YOU HAVE FAILED THIS TEST!.
        return false;
    }

    return true;
}
