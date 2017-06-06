#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef _WIN32 /* for sleep function */

#include <windows.h>

void mysleep(int sec) {
    Sleep((DWORD)sec * 1000);
}

#else /* Unix/POSIX */

#include <time.h>
#include <errno.h>

void mysleep(int sec) {
    struct timespec ts;
    ts.tv_sec = sec;
    ts.tv_nsec = 0;
   
    for(;;) {
	struct timespec rem;
	if (nanosleep(&ts, &rem) < 0) {
	    if (errno != EINTR) {
		printf("UKNONW SLEEP ERROR!\n");
		exit(1);
	    }
	    ts.tv_sec = rem.tv_sec;
	    ts.tv_nsec = ts.tv_nsec;
	} else 
	    break;
    }
}
#endif

int main(int argc, char* argv[])
{
    int status = 0;
    char line[1000];

    printf("stdout 1\n"); fflush(stdout);
    fprintf(stderr, "stderr 1\n");
    fflush(stderr);

    if (argc > 1 && strcmp(argv[1], "1") == 0) {
	while(fgets(line, 1000, stdin)) printf("stdin: %s", line);
	fflush(stdout);
    }
    if (argc > 1 && strcmp(argv[1], "1")) {
	status = atoi(argv[1]);
    }
    if (argc > 1 && strcmp(argv[1], "infinite_loop") == 0) {
	printf("Going to infinite loop...\n");
	fflush(stdout);
	while(1); /* infinite loop */
    }
    if (argc > 2 && strcmp(argv[1], "sleep") == 0) {
	int sec = atoi(argv[2]);
	printf("Sleeping for %d seconds...\n", sec);
	fflush(stdout);
	mysleep(sec);
	printf("Done sleeping for %d seconds.\n", sec);
	fflush(stdout);
    }
    
    exit(status);
}

