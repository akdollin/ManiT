pid = fork();
if (pid == 0)
{
	/* child process */
	print("forked");
}
