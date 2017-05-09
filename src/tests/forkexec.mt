
/* fork into execlp */

pid = fork();

if (pid != 0)
{
	execlp("echo", "echo", "hi", 0);
}
