# Elfos-tee

The tee utility runs an Elf/OS command while copying it's output into a file. Usage is:

> tee [-a] logfile command ...

This will run the command given and copy the output into a file named logfile. If the -a option is provided, it will append the output to an existing file.

Tee requires kernel 0.4.0 and should work with most any program including shell and those run under shell.

