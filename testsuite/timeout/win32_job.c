#define UNICODE
#define _UNICODE
#define STRICT
#include <windows.h>
#include <stdio.h>

extern int setJobParameters( HANDLE hJob )
{
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION jeli;
    ZeroMemory(&jeli, sizeof(JOBOBJECT_EXTENDED_LIMIT_INFORMATION));
    //Configure all child processes associated with the job to terminate when the
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

extern int waitForJobCompletion ( HANDLE hJob, HANDLE ioPort, DWORD timeout )
{
    DWORD CompletionCode;
    ULONG_PTR CompletionKey;
    LPOVERLAPPED Overlapped;

    while (GetQueuedCompletionStatus(ioPort, &CompletionCode,
             &CompletionKey, &Overlapped, timeout) &&
             !((HANDLE)CompletionKey == hJob &&
              CompletionCode == JOB_OBJECT_MSG_ACTIVE_PROCESS_ZERO)) {
        // We're still waiting, let's sing a song! lalala lalalalaaa
    }

    if (Overlapped == NULL && (HANDLE)CompletionKey != hJob)
    {
        printf("** IOCP ** Return value: %d, %d, %d\n", GetLastError(), CompletionCode, CompletionKey);
        // Timeout occurred. *dark voice* YOU HAVE FAILED THIS TEST!.
        return 0;
    }

    return 1;
}
